geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")

same_stats <- function(rain, orig) {
  a <- ggstatsplot::extract_stats(rain)
  b <- ggstatsplot::extract_stats(orig)
  for (nm in c("subtitle_data", "caption_data", "pairwise_comparisons_data", "descriptive_data")) {
    expect_identical(a[[nm]], b[[nm]], label = nm)
  }
}

test_that("between-subjects raincloud: identical statistics, raincloud layers", {
  set.seed(42)
  rain <- suppressWarnings(ggbetweenrain(mtcars, cyl, mpg))
  set.seed(42)
  orig <- suppressWarnings(ggstatsplot::ggbetweenstats(mtcars, cyl, mpg))
  same_stats(rain, orig)

  g <- geoms(rain)
  expect_false("GeomViolin" %in% g)
  expect_true(all(c("GeomSlabinterval", "GeomDotsinterval", "GeomBoxplot", "GeomSignif", "GeomLabelRepel") %in% g) ||
    all(c("GeomSlab", "GeomDots", "GeomBoxplot", "GeomSignif", "GeomLabelRepel") %in% g))
  # centrality labels are ggstatsplot's own
  lab <- function(p) Filter(function(l) inherits(l$geom, "GeomLabelRepel"), p$layers)[[1]]$data$expression
  expect_identical(lab(rain), lab(orig))
  # APA p-values applied
  expect_false(any(grepl("e-", deparse(rain$labels$subtitle))))
  # the plot renders
  expect_no_error(ggplot2::ggplot_build(rain))
})

test_that("within-subjects raincloud (4 conditions): identical statistics", {
  set.seed(42)
  rain <- suppressWarnings(ggwithinrain(ggstatsplot::bugs_long, condition, desire, subject.id = subject))
  set.seed(42)
  orig <- suppressWarnings(ggstatsplot::ggwithinstats(ggstatsplot::bugs_long, condition, desire, subject.id = subject))
  same_stats(rain, orig)
  expect_false("GeomViolin" %in% geoms(rain))
  expect_no_error(ggplot2::ggplot_build(rain))
})

test_that("two conditions: mirrored, and every subject line ends on its own points", {
  d2 <- dplyr::filter(ggstatsplot::bugs_long, condition %in% c("HDHF", "LDLF"))
  set.seed(42)
  rain <- suppressWarnings(ggwithinrain(d2, condition, desire, subject.id = subject))
  set.seed(42)
  orig <- suppressWarnings(ggstatsplot::ggwithinstats(d2, condition, desire, subject.id = subject))
  same_stats(rain, orig)

  b <- ggplot2::ggplot_build(rain)
  g <- geoms(rain)
  pts <- b$data[[which(g == "GeomPoint")[1]]] # the rain (first point layer)
  path_idx <- which(g == "GeomPath" & !vapply(rain$layers, function(l) is.data.frame(l$data), TRUE))
  expect_length(path_idx, 1L)
  path <- b$data[[path_idx]]
  # rain sits inside: condition 1 to the right of x = 1, condition 2 to the left of x = 2
  expect_true(all(pts$x[pts$x < 1.5] > 1))
  expect_true(all(pts$x[pts$x > 1.5] < 2))
  # every path vertex coincides with a rain point at the same (x, y)
  key <- function(d) paste(round(d$x, 6), round(d$y, 6))
  expect_true(all(key(path) %in% key(pts)))
  # one cloud on each side: two slab layers
  expect_equal(sum(grepl("Slab", g)), 2L)
})

test_that("grouped raincloud: one panel per level, each with its own statistics", {
  set.seed(42)
  g <- suppressWarnings(grouped_ggbetweenrain(mtcars, cyl, mpg, grouping.var = am))
  expect_s3_class(g, "patchwork")
  panels <- c(g$patches$plots, list(g))
  expect_length(panels, 2L)
  stats <- ggstatsplot::extract_stats(g) # one entry per panel
  for (i in 1:2) {
    lv <- c(0, 1)[i]
    set.seed(42)
    orig <- suppressWarnings(ggstatsplot::ggbetweenstats(dplyr::filter(mtcars, am == lv), cyl, mpg))
    expect_identical(
      stats[[i]]$subtitle_data$p.value,
      ggstatsplot::extract_stats(orig)$subtitle_data$p.value
    )
    expect_identical(panels[[i]]$labels$title, as.character(lv))
  }
})

test_that("horizontal orientation flips coordinates", {
  set.seed(42)
  p <- suppressWarnings(ggbetweenrain(mtcars, cyl, mpg, orientation = "horizontal"))
  expect_s3_class(p$coordinates, "CoordFlip")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("user arguments reach the cloud, box and rain", {
  set.seed(42)
  p <- suppressWarnings(ggbetweenrain(mtcars, cyl, mpg, box.args = list(width = 0.3), cloud.args = list(alpha = 0.1)))
  box <- Filter(function(l) inherits(l$geom, "GeomBoxplot"), p$layers)[[1]]
  expect_equal(box$geom_params$width %||% box$aes_params$width %||% box$stat_params$width, 0.3)
  slab <- Filter(function(l) grepl("Slab", class(l$geom)[1]), p$layers)[[1]]
  expect_equal(slab$aes_params$alpha, 0.1)
})

test_that("an unexpected plot structure stops instead of drawing something wrong", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_point()
  expect_error(BetterGG:::.to_raincloud(p, FALSE, list(), list(), list(), "vertical"), "unexpected")
})

test_that("between-subjects rain dots sit at the exact data values (no binning)", {
  set.seed(42)
  p <- suppressWarnings(ggbetweenrain(mtcars, cyl, mpg))
  dots <- Filter(function(l) grepl("Dots", class(l$geom)[1]), p$layers)[[1]]
  layout <- dots$geom_params$layout %||% dots$stat_params$layout %||% "bin"
  for (cy in unique(mtcars$cyl)) {
    y <- mtcars$mpg[mtcars$cyl == cy]
    d <- ggdist::bin_dots(x = 0, y = y, binwidth = 0.8, layout = layout, side = "left", orientation = "vertical")
    expect_equal(sort(d$y), sort(y), tolerance = 0, label = paste("cyl", cy, "layout", layout))
  }
})

test_that("every observation appears once in the rain", {
  set.seed(42)
  b <- ggplot2::ggplot_build(suppressWarnings(ggbetweenrain(mtcars, cyl, mpg)))
  dots <- b$data[[2]]
  expect_equal(nrow(dots), nrow(mtcars))
  expect_equal(sort(dots$y), sort(mtcars$mpg))

  d2 <- dplyr::filter(ggstatsplot::bugs_long, condition %in% c("HDHF", "LDLF"))
  set.seed(42)
  w <- suppressWarnings(ggwithinrain(d2, condition, desire, subject.id = subject))
  pts <- ggplot2::ggplot_build(w)$data[[which(vapply(w$layers, function(l) class(l$geom)[1], "") == "GeomPoint")[1]]]
  expect_equal(nrow(pts), nrow(w$data))
  expect_equal(sort(pts$y), sort(w$data$desire))
})

test_that("mirror keeps its sides even when the user passes cloud.args", {
  d2 <- dplyr::filter(ggstatsplot::bugs_long, condition %in% c("HDHF", "LDLF"))
  set.seed(42)
  w <- suppressWarnings(ggwithinrain(d2, condition, desire, subject.id = subject,
    cloud.args = list(side = "right", justification = 0, alpha = 0.2)))
  slabs <- Filter(function(l) grepl("Slab", class(l$geom)[1]), w$layers)
  side <- function(l) l$geom_params$side %||% l$stat_params$side %||% l$aes_params$side
  expect_identical(side(slabs[[1]]), "left")
  expect_equal(slabs[[1]]$aes_params$alpha, 0.2)
})
