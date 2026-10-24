test_that("Okabe-Ito colours match the published hex codes", {
  # Okabe & Ito (2008): orange, sky blue, bluish green, blue, vermillion,
  # reddish purple, yellow
  expect_identical(
    toupper(bgg_palette("okabe_ito", 7)),
    c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  )
})

test_that("palettes never return missing or repeated colours", {
  for (nm in c("nejm", "okabe_ito", "viridis", "grey")) {
    cols <- bgg_palette(nm, 5)
    expect_false(anyNA(cols), label = nm)
    expect_identical(length(unique(cols)), 5L, label = nm)
  }
  expect_error(bgg_palette("okabe_ito", 8), "7 colours")
})

built_colours <- function(p, aes) {
  b <- ggplot2::ggplot_build(p)
  unique(unlist(lapply(b$data, function(d) d[[aes]])))
}

test_that("BetterGG plots carry the NEJM colours by default (no blanks)", {
  set.seed(1)
  bar <- suppressWarnings(ggbarstats(mtcars, am, cyl))
  fills <- built_colours(bar, "fill")
  expect_true(all(toupper(bgg_palette("nejm", 2)) %in% toupper(fills)))
  expect_false(anyNA(Filter(function(x) !identical(x, "white"), fills)))

  set.seed(1)
  bw <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  cols <- built_colours(bw, "colour")
  expect_true(all(bgg_palette("nejm", 3) %in% cols))
})

test_that("user palette and ggtheme are respected", {
  set.seed(1)
  p <- suppressWarnings(ggbarstats(mtcars, am, cyl, palette = "ggthemes::gdoc"))
  expect_false(any(bgg_palette("nejm", 2) %in% built_colours(p, "fill")))
  p2 <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, ggtheme = ggplot2::theme_dark(), results.subtitle = FALSE))
  expect_s3_class(ggplot2::calc_element("panel.background", p2$theme), "element_rect")
  expect_false(identical(ggplot2::calc_element("panel.background", p2$theme)$fill, "white"))
})

test_that("APA theme is applied by default and can be switched off", {
  set.seed(1)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE))
  expect_s3_class(ggplot2::calc_element("panel.grid.major.y", p$theme), "element_blank")
  expect_identical(ggplot2::calc_element("axis.title.x", p$theme)$face, "plain")
  old <- options(BetterGG.theme = FALSE, BetterGG.palette = NULL)
  on.exit(options(old))
  q <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE))
  expect_identical(ggplot2::calc_element("axis.title.x", q$theme)$face, "bold")
})

test_that("tidy-eval calling styles still work through the wrapper", {
  y <- "mpg"
  expect_identical(suppressWarnings(ggbetweenstats(mtcars, cyl, !!y, results.subtitle = FALSE))$labels$y, "mpg")
  f <- function(d, v) ggbetweenstats(d, cyl, {{ v }}, results.subtitle = FALSE)
  expect_identical(suppressWarnings(f(mtcars, wt))$labels$y, "wt")
  g <- function(...) ggbetweenstats(...)
  expect_identical(suppressWarnings(g(mtcars, cyl, disp, results.subtitle = FALSE))$labels$y, "disp")
})

test_that("grouped raincloud passes rain arguments to the rain, not to ggstatsplot", {
  set.seed(1)
  g <- suppressWarnings(grouped_ggbetweenrain(mtcars, cyl, mpg, grouping.var = am,
    box.args = list(width = 0.3)))
  box <- Filter(function(l) inherits(l$geom, "GeomBoxplot"), g$layers)[[1]]
  expect_equal(box$geom_params$width %||% box$aes_params$width %||% box$stat_params$width, 0.3)
})

test_that("save_figure writes vector PDF with embedded fonts and sized rasters", {
  set.seed(1)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE))
  pdf_file <- withr::local_tempfile(fileext = ".pdf")
  family <- test_font_family("sans")
  save_figure(p, pdf_file, width = "single", family = family)
  raw <- readBin(pdf_file, "raw", file.size(pdf_file))
  expect_true(length(grepRaw("/FontFile2", raw, fixed = TRUE)) > 0) # embedded TrueType font program
  expect_true(length(grepRaw(test_font_name(family), raw, fixed = TRUE)) > 0)

  png_file <- withr::local_tempfile(fileext = ".png")
  save_figure(p, png_file, width = "double", dpi = 300)
  sig <- readBin(png_file, "raw", 24)
  w <- sum(as.integer(sig[17:20]) * 256^(3:0))
  expect_equal(w, round(7.2 * 300))
  expect_error(save_figure(p, "x.bmp"), "unsupported")
})

test_that("NEJM palette matches the ggsci NEJM hex codes", {
  expect_identical(
    toupper(bgg_palette("nejm", 4)),
    c("#BC3C29", "#0072B5", "#E18727", "#20854E")
  )
})

test_that("results (subtitle, caption) are centred; font and size as approved", {
  th <- theme_apa()
  expect_equal(ggplot2::calc_element("plot.subtitle", th)$hjust, 0.5)
  expect_equal(ggplot2::calc_element("plot.caption", th)$hjust, 0.5)
  expect_equal(ggplot2::calc_element("text", th)$size, 12.5)
  if ("Trebuchet MS" %in% systemfonts::system_fonts()$family) {
    expect_identical(bgg_font(), "Trebuchet MS")
    expect_identical(ggplot2::calc_element("text", th)$family, "Trebuchet MS")
  } else {
    expect_identical(bgg_font(), "")
    expect_identical(ggplot2::calc_element("text", th)$family, "")
  }
  old <- options(BetterGG.font = "No Such Font 123")
  on.exit(options(old))
  expect_identical(bgg_font(), "") # falls back, never a missing font
})

test_that("in-panel labels use the BetterGG font", {
  family <- test_font_family("sans")
  withr::local_options(BetterGG.font = family)
  set.seed(1)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  lab <- Filter(function(l) inherits(l$geom, "GeomLabelRepel"), p$layers)[[1]]
  expect_identical(lab$aes_params$family, family)
})

test_that("greyscale option: only greys, darker outlines than fills, violins filled", {
  old <- options(BetterGG.palette = "grey")
  on.exit(options(old))
  is_grey <- function(h) { v <- grDevices::col2rgb(h); v[1, ] == v[2, ] & v[2, ] == v[3, ] }
  set.seed(1)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  b <- ggplot2::ggplot_build(p)
  vi <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomViolin"), TRUE))
  fills <- unique(b$data[[vi]]$fill)
  cols <- unique(b$data[[1]]$colour)
  expect_length(fills, 3L)
  expect_true(all(is_grey(fills)) && all(is_grey(cols)))
  lum <- function(h) colSums(grDevices::col2rgb(h))
  expect_true(all(lum(bgg_palette("grey", 3, "colour")) < lum(bgg_palette("grey", 3, "fill"))))
  bar <- suppressWarnings(ggbarstats(mtcars, am, cyl))
  expect_true(all(is_grey(Filter(Negate(is.na), built_colours(bar, "fill")))))
})

test_that("user drawing arguments win over the BetterGG look", {
  set.seed(1)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE,
    centrality.point.args = list(size = 6, colour = "darkred")))
  cp <- Filter(function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data), p$layers)[[1]]
  expect_identical(cp$aes_params$colour, "darkred")
})

test_that("BetterGG leaves the random-number stream untouched (same seed, same Bayes results)", {
  set.seed(7)
  a <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  set.seed(7)
  b <- suppressWarnings(ggstatsplot::ggbetweenstats(mtcars, cyl, mpg))
  expect_identical(ggstatsplot::extract_stats(a)$caption_data, ggstatsplot::extract_stats(b)$caption_data)
  set.seed(7)
  BetterGG:::.rng_neutral(BetterGG:::.style_args("ggbetweenstats"))
  x <- runif(1)
  set.seed(7)
  expect_identical(runif(1), x)
})

test_that("plots draw on R's base pdf() device (Rscript default) without a font error", {
  withr::local_options(BetterGG.font = test_font_family("sans"))
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(1)
  p <- suppressWarnings(ggbetweenrain(mtcars, cyl, mpg))
  expect_no_error(suppressWarnings(print(p)))
  expect_true(bgg_font() %in% names(grDevices::pdfFonts()))
})

test_that("the pdf() font alias never overwrites a mapping the user registered", {
  fam <- "BetterGG Test Family"
  withr::local_options(BetterGG.font = fam)
  user_map <- grDevices::pdfFonts()$Courier
  do.call(grDevices::pdfFonts, stats::setNames(list(user_map), fam))
  BetterGG:::.alias_for_base_pdf(fam)
  expect_identical(grDevices::pdfFonts(fam)[[1]], user_map)
})

test_that("the wrapper help page names the palette that is really the default", {
  rd <- testthat::test_path("..", "..", "man", "ggstatsplot-apa.Rd")
  testthat::skip_if_not(file.exists(rd), "man/ not available (installed-package test run)")
  txt <- paste(readLines(rd, warn = FALSE), collapse = " ")
  expect_identical(eval(formals(scale_colour_bgg)$palette), "nejm")
  expect_match(txt, "NEJM palette", fixed = TRUE)
  expect_no_match(txt, "colourblind-safe Okabe-Ito palette", fixed = TRUE)
})

test_that("R CMD check's package-use check: no unused Imports; only the deliberate ggstatsplot Depends note", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, "DESCRIPTION")), "source tree not available")
  used <- tools:::.check_packages_used(dir = root)
  expect_length(used$unused_imports, 0L) # beeswarm and patchwork are referenced (R/imports.R, R/reliability.R)
  expect_identical(used$depends_not_import, "ggstatsplot") # by design, see R/imports.R
})
