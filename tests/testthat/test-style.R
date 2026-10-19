test_that("Okabe-Ito colours match the published hex codes", {
  # Okabe & Ito (2008): orange, sky blue, bluish green, blue, vermillion,
  # reddish purple, yellow
  expect_identical(
    toupper(bgg_palette("okabe_ito", 7)),
    c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  )
})

test_that("palettes never return missing or repeated colours", {
  for (nm in c("okabe_ito", "viridis", "grey")) {
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

test_that("BetterGG plots carry the Okabe-Ito colours (no blanks)", {
  set.seed(1)
  bar <- suppressWarnings(ggbarstats(mtcars, am, cyl))
  fills <- built_colours(bar, "fill")
  expect_true(all(bgg_palette("okabe_ito", 2) %in% toupper(fills) | bgg_palette("okabe_ito", 2) %in% fills))
  expect_false(anyNA(Filter(function(x) !identical(x, "white"), fills)))

  set.seed(1)
  bw <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  cols <- built_colours(bw, "colour")
  expect_true(all(bgg_palette("okabe_ito", 3) %in% cols))
})

test_that("user palette and ggtheme are respected", {
  set.seed(1)
  p <- suppressWarnings(ggbarstats(mtcars, am, cyl, palette = "ggthemes::gdoc"))
  expect_false(any(bgg_palette("okabe_ito", 2) %in% built_colours(p, "fill")))
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
  save_figure(p, pdf_file, width = "single", family = "Arial")
  raw <- readBin(pdf_file, "raw", file.size(pdf_file))
  expect_true(length(grepRaw("/FontFile2", raw, fixed = TRUE)) > 0) # embedded TrueType font program
  expect_true(length(grepRaw("Arial", raw, fixed = TRUE)) > 0)

  png_file <- withr::local_tempfile(fileext = ".png")
  save_figure(p, png_file, width = "double", dpi = 300)
  sig <- readBin(png_file, "raw", 24)
  w <- sum(as.integer(sig[17:20]) * 256^(3:0))
  expect_equal(w, round(7.2 * 300))
  expect_error(save_figure(p, "x.bmp"), "unsupported")
})
