# Regressions for argument forwarding, palettes, grouped panels, and fonts.
cols <- function(p, a = "colour") unique(unlist(lapply(ggplot2::ggplot_build(p)$data, `[[`, a)))
oi3 <- bgg_palette("nejm", 3)

test_that("F1a: a palette forwarded through another function's ... is kept", {
  h <- function(d, ...) ggbetweenstats(d, cyl, mpg, results.subtitle = FALSE, ...)
  p <- suppressWarnings(h(mtcars, palette = "ggthemes::gdoc"))
  expect_false(any(oi3 %in% cols(p)))
})

test_that("F1b: an abbreviated palette argument is recognised", {
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE, pal = "ggthemes::gdoc"))
  expect_false(any(oi3 %in% cols(p)))
})

test_that("F1c: a colour scale the user adds is not overwritten", {
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg, results.subtitle = FALSE,
    ggplot.component = ggplot2::scale_colour_manual(values = c("red", "green", "blue"))))
  expect_true(all(c("red", "green", "blue") %in% cols(p)))
})

test_that("F2: ggtheme forwarded through ... works", {
  g <- function(...) ggbetweenstats(...)
  p <- suppressWarnings(g(mtcars, cyl, mpg, results.subtitle = FALSE, ggtheme = ggplot2::theme_dark()))
  expect_false(identical(ggplot2::calc_element("panel.background", p$theme)$fill, "white"))
})

test_that("F3: an abbreviated raincloud argument is an error, not ignored", {
  expect_error(suppressWarnings(ggbetweenrain(mtcars, cyl, mpg, orient = "horizontal")), "orientation")
  expect_error(suppressWarnings(grouped_ggbetweenrain(mtcars, cyl, mpg, grouping.var = am, orient = "horizontal")), "orientation")
})

test_that("F5: a group keeps one colour across grouped panels", {
  d <- dplyr::filter(mtcars, !(cyl == 4 & am == 0)) # cyl 4 missing from the am = 0 panel
  g <- suppressWarnings(grouped_ggbetweenrain(d, cyl, mpg, grouping.var = am, results.subtitle = FALSE))
  colour_of <- function(panel) {
    b <- ggplot2::ggplot_build(panel)
    dots <- b$data[[2]]
    lv <- sub("\n.*", "", b$layout$panel_params[[1]]$x$get_labels()) # drop "(n = ...)"
    stats::setNames(vapply(seq_along(lv), function(i) unique(dots$fill[dots$x == i])[1], ""), lv)
  }
  a <- colour_of(g$patches$plots[[1]])
  b <- colour_of(g)
  shared <- intersect(names(a), names(b))
  expect_true(length(shared) >= 2)
  expect_identical(a[shared], b[shared])
  expect_length(unique(c(a, b)), 3L)
  # same colours as an ungrouped plot: levels 4, 6, 8 get colours 1, 2, 3
  expect_identical(unname(b[c("4", "6", "8")]), bgg_palette("nejm", 3, "fill"))
})

test_that("panel level orders merge into the natural order", {
  expect_identical(BetterGG:::.merge_orders(list(c("6", "8"), c("4", "6", "8"))), c("4", "6", "8"))
  expect_identical(BetterGG:::.merge_orders(list(c("low", "high"), c("low", "mid", "high"))), c("low", "mid", "high"))
})

test_that("F6: save_figure(family =) uses one font for all text", {
  fonts <- function(f) {
    raw <- readBin(f, "raw", file.size(f))
    pos <- grepRaw("/BaseFont", raw, all = TRUE)
    unique(vapply(pos, function(i) {
      b <- raw[i:(i + 40)]
      sub("^/BaseFont */[A-Z]{6}[+]", "", rawToChar(b[b > 32 & b < 127]))
    }, ""))
  }
  pf <- withr::local_tempfile(fileext = ".pdf")
  family <- test_font_family("serif")
  save_figure(suppressWarnings(ggbetweenstats(mtcars, cyl, mpg)), pf, family = family)
  f <- fonts(pf)
  expect_false(any(grepl("Arial", f)))
  expect_true(any(grepl(test_font_name(family), f, fixed = TRUE)))
})

test_that("F1d: grouped wrappers recognise an abbreviated palette or theme (d-891b0b)", {
  all_cols <- function(g) unique(unlist(lapply(c(list(g), g$patches$plots), cols)))
  p <- suppressWarnings(grouped_ggbetweenstats(mtcars, cyl, mpg, grouping.var = am,
    results.subtitle = FALSE, pal = "ggthemes::gdoc"))
  expect_false(any(oi3 %in% all_cols(p)))
  # control: without pal= the BetterGG palette is drawn
  q <- suppressWarnings(grouped_ggbetweenstats(mtcars, cyl, mpg, grouping.var = am, results.subtitle = FALSE))
  expect_true(any(oi3 %in% all_cols(q)))
  # an abbreviated ggtheme reaches every panel instead of theme_apa()
  r <- suppressWarnings(grouped_ggbetweenstats(mtcars, cyl, mpg, grouping.var = am,
    results.subtitle = FALSE, ggth = ggplot2::theme_dark()))
  bg <- ggplot2::calc_element("panel.background", r$patches$plots[[1]]$theme)$fill
  expect_false(identical(bg, "white"))
})

test_that("save_figure: a font whose plotmath metrics break ragg falls back to Cairo (d-3752c9)", {
  ok_family <- test_font_family("sans")
  expect_true(BetterGG:::.ragg_plotmath_ok(ok_family, 110))
  skip_if_not("Calibri" %in% systemfonts::system_fonts()$family, "Calibri not installed")
  expect_false(BetterGG:::.ragg_plotmath_ok("Calibri", 110))
  set.seed(1)
  p <- suppressWarnings(ggwithinstats(data.frame(id = rep(1:10, 2), c = rep(c("a", "b"), each = 10),
    y = stats::rnorm(20)), c, y))
  for (ext in c(".png", ".tiff")) {
    f <- withr::local_tempfile(fileext = ext)
    expect_warning(save_figure(p, f, width = 11.8, height = 5.45, dpi = 110, family = "Calibri"), "Cairo")
    expect_gt(file.size(f), 1000)
  }
  withr::local_options(BetterGG.font = "Calibri")
  f <- withr::local_tempfile(fileext = ".png")
  q <- suppressWarnings(ggwithinstats(data.frame(id = rep(1:10, 2), c = rep(c("a", "b"), each = 10),
    y = stats::rnorm(20)), c, y))
  expect_warning(save_figure(q, f, width = 11.8, height = 5.45, dpi = 110), "Cairo")
})

test_that("ggstatsplot issue #312: y limits via ggplot.component reach every grouped panel (d-9f1901)", {
  g <- suppressMessages(suppressWarnings(grouped_ggbetweenstats(mtcars, cyl, mpg, grouping.var = am,
    results.subtitle = FALSE, ggplot.component = ggplot2::scale_y_continuous(limits = c(0, 40)))))
  ranges <- lapply(c(g$patches$plots, list(g)), function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range)
  expect_length(ranges, 2L)
  for (r in ranges) expect_equal(r, c(0, 40) + c(-2, 2)) # limits plus ggplot2's 5% expansion
})

test_that("ggstatsplot issue #299: a non-integer test.value is tested as given (d-9f1901)", {
  h <- suppressWarnings(gghistostats(mtcars, wt, test.value = 3.5))
  ref <- stats::t.test(mtcars$wt, mu = 3.5)
  txt <- paste(deparse(h$labels$subtitle), collapse = " ")
  expect_match(txt, sprintf("\"%.2f\"", unname(ref$statistic)), fixed = TRUE)
  expect_match(txt, sub("\"0.", "\".", sprintf("\"%.3f\"", ref$p.value), fixed = TRUE), fixed = TRUE)
})

test_that("var.equal = TRUE, which ggstatsplot ignores (always Welch), is named in a warning (d-ef7283)", {
  expect_warning(p <- ggbetweenstats(mtcars, am, mpg, var.equal = TRUE, results.subtitle = TRUE), "var.equal.*Welch")
  expect_match(paste(deparse(p$labels$subtitle), collapse = ""), "Welch", fixed = TRUE)
  expect_warning(grouped_ggbetweenstats(mtcars, am, mpg, grouping.var = vs, var.equal = TRUE), "var.equal.*Welch")
  expect_warning(ggbetweenrain(mtcars, am, mpg, var.equal = TRUE), "var.equal.*Welch")
  # no warning when it is not asked for, or FALSE (Welch is what is drawn), or the test is not parametric
  expect_no_warning(ggbetweenstats(mtcars, am, mpg, results.subtitle = FALSE))
  expect_no_warning(ggbetweenstats(mtcars, am, mpg, var.equal = FALSE, results.subtitle = FALSE))
  expect_no_warning(suppressMessages(ggbetweenstats(mtcars, am, mpg, type = "np", var.equal = TRUE)))
})

test_that("a statistics subtitle that ggstatsplot silently fails to draw is named in a warning (d-49f730)", {
  skip_if_not_installed("BayesFactor")
  set.seed(3)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), 20)), y = rnorm(60))
  # a grouping column named g breaks ggstatsplot's Bayesian subtitle (performance::r2_bayes)
  expect_warning(p <- ggbetweenstats(d, g, y, type = "bayes"), "subtitle")
  expect_null(p$labels$subtitle)
  # controls: another column name draws the subtitle without that warning; results.subtitle = FALSE asks for none
  names(d)[1] <- "grp"
  w <- character()
  q <- withCallingHandlers(ggbetweenstats(d, grp, y, type = "bayes"),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("subtitle", w)))
  expect_false(is.null(q$labels$subtitle))
  expect_no_warning(ggbetweenstats(d, grp, y, results.subtitle = FALSE))
})

test_that("grouped wrappers also warn when a panel's requested subtitle is missing (review 0.2.2)", {
  skip_if_not_installed("BayesFactor")
  set.seed(4)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), 40)), h = rep(c("u", "v"), each = 60), y = rnorm(120))
  expect_warning(grouped_ggbetweenstats(d, g, y, grouping.var = h, type = "bayes"), "subtitle")
  # control: renamed column, subtitles drawn, no such warning
  names(d)[1] <- "grp"
  w <- character()
  withCallingHandlers(grouped_ggbetweenstats(d, grp, y, grouping.var = h, type = "bayes"),
    warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("subtitle", w)))
})
