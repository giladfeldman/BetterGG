# All p-value text found anywhere on a plot (subtitle, caption, bracket
# annotations, label columns), recursing into grouped plots.
p_texts <- function(plot) {
  out <- character()
  grab <- function(x) {
    s <- if (is.character(x)) x else paste(deparse(x, width.cutoff = 500L), collapse = "")
    m <- regmatches(s, gregexpr("italic\\(p\\)[^,]*?(==|<|>) *\\\\?\"[^\"\\\\]*", s))[[1]]
    sub('.*"', "", m)
  }
  if (inherits(plot, "patchwork")) {
    for (sub_plot in plot$patches$plots) out <- c(out, p_texts(sub_plot))
  }
  for (slot in c("subtitle", "caption")) {
    if (!is.null(plot$labels[[slot]])) out <- c(out, grab(plot$labels[[slot]]))
  }
  for (layer in plot$layers) {
    for (a in layer$stat_params$annotations) out <- c(out, grab(a))
    if (is.data.frame(layer$data)) {
      for (col in layer$data) for (v in col) if (is.character(v) || is.language(v)) out <- c(out, grab(v))
    }
  }
  out
}

apa_ok <- function(x) grepl("^(\\.[0-9]{3}|NA)$", x)

test_that("apa_p_value follows APA rules", {
  f <- function(p) vapply(apa_p_value(p), function(z) paste(z$op, z$value), "")
  expect_identical(
    f(c(0.0456, 0.00049, 0.001, 0.0009999, 0.05, 0.9996, 0.5, 0, 0.00151)),
    c("== .046", "< .001", "== .001", "< .001", "== .050", "> .999", "== .500", "< .001", "== .002")
  )
})

test_that("a stale or mismatched exact value is never substituted", {
  e <- quote(list(italic(p) == "0.04"))
  expect_warning(out <- BetterGG:::.rewrite(e, 0.20, 3L, "test"), "unchanged")
  expect_identical(out, e)
  expect_warning(BetterGG:::.rewrite(e, NA_real_, 3L, "test"), "unchanged")
})

test_that("consistency check matches ggstatsplot's own rounding", {
  expect_true(BetterGG:::.consistent(0.0456, "0.05"))
  expect_true(BetterGG:::.consistent(1.234e-6, "1.23e-06"))
  expect_false(BetterGG:::.consistent(0.0456, "0.04"))
  expect_false(BetterGG:::.consistent(1.234e-6, "1.24e-06"))
})

plots <- function() {
  set.seed(123)
  movies <- dplyr::filter(ggstatsplot::movies_long, genre %in% c("Action", "Comedy", "Drama"))
  list(
    between = ggbetweenstats(mtcars, cyl, mpg),
    between_np = ggbetweenstats(mtcars, cyl, mpg, type = "nonparametric"),
    between_all = ggbetweenstats(mtcars, cyl, mpg, type = "nonparametric", pairwise.display = "all"),
    between_2 =ggbetweenstats(mtcars, am, mpg),
    within = ggwithinstats(ggstatsplot::bugs_long, condition, desire, subject.id = subject),
    scatter = ggscatterstats(mtcars, wt, mpg, marginal = FALSE),
    histo = gghistostats(mtcars, wt, test.value = 3),
    dot = ggdotplotstats(movies, y = genre, x = rating, test.value = 6),
    pie = ggpiestats(mtcars, am, cyl),
    bar = ggbarstats(mtcars, am, cyl),
    coef = ggcoefstats(stats::lm(mpg ~ wt + cyl, mtcars)),
    grouped = grouped_ggbetweenstats(mtcars, cyl, mpg, grouping.var = am),
    grouped_pie = grouped_ggpiestats(mtcars, am, cyl, grouping.var = vs)
  )
}

test_that("every plot type ends up with only APA p-values, with no warnings", {
  ps <- suppressMessages(withCallingHandlers(
    plots(),
    warning = function(w) {
      if (grepl("BetterGG", conditionMessage(w))) stop(conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ))
  for (nm in names(ps)) {
    txt <- p_texts(ps[[nm]])
    expect_gt(length(txt), 0L, label = paste(nm, "has p-values"))
    expect_true(all(apa_ok(txt)), label = paste(nm, ":", paste(txt, collapse = " ")))
  }
})

test_that("APA value equals the exact p-value rounded, label by label", {
  set.seed(123)
  p <- suppressWarnings(ggbetweenstats(mtcars, am, mpg, type = "nonparametric"))
  exact <- ggstatsplot::extract_stats(p)$subtitle_data$p.value
  expected <- apa_p_value(exact)[[1]]$value
  expect_identical(p_texts(p), expected)

  pie <- suppressWarnings(ggpiestats(mtcars, am, cyl))
  lab <- Filter(function(l) ".label" %in% names(l$data), pie$layers)[[1]]$data
  got <- vapply(lab$.label, function(s) p_texts(ggplot2::ggplot() + ggplot2::labs(subtitle = str2lang(s))), "")
  want <- vapply(apa_p_value(lab$p.value), `[[`, "", "value")
  expect_identical(unname(got), want)
})

test_that("the original ggstatsplot plot is not modified", {
  set.seed(123)
  orig <- suppressWarnings(ggstatsplot::ggbetweenstats(mtcars, cyl, mpg))
  before <- p_texts(orig)
  apa_p(orig)
  expect_identical(p_texts(orig), before)
  expect_true(any(grepl("e-", before)))
})

test_that("option turns the conversion off", {
  withr_opt <- options(BetterGG.apa = FALSE)
  on.exit(options(withr_opt))
  set.seed(123)
  p <- suppressWarnings(ggbetweenstats(mtcars, cyl, mpg))
  expect_true(any(grepl("e-", p_texts(p))))
})
