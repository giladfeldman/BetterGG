# Regressions for the 2026-10-19 second-model review (Sonnet, tier 2), round 3.
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
  save_figure(suppressWarnings(ggbetweenstats(mtcars, cyl, mpg)), pf, family = "Times New Roman")
  f <- fonts(pf)
  expect_false(any(grepl("Arial", f)))
  expect_true(any(grepl("TimesNewRoman", f)))
})
