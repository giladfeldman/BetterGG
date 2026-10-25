# Figure defaults of 0.2.3: few-valued rain jitter, small-segment labels,
# wrapped long labels, ordered palettes, one-variable bars, 0/1 proportions.
# Synthetic data only.

rplot <- function(code, object = "r_plot", ...) {
  set.seed(1)
  run_recipe(code, ...)[[object]]
}
layer_data_of <- function(p, geom) {
  b <- ggplot2::ggplot_build(p)
  b$data[[which(vapply(p$layers, function(l) inherits(l$geom, geom), TRUE))[1]]]
}
likert <- local({
  set.seed(11)
  data.frame(score = sample(1:7, 240, TRUE), A = factor(rep(c("a1", "a2"), 120)),
    B = factor(rep(c("b1", "b2"), each = 120)))
})

test_that("factorial rain: few-valued outcomes get a small vertical jitter that never reaches the next value", {
  real <- ggfactorialrain(likert, A, score, colour = B)
  rec <- rplot(recipe_factorialrain("likert", "A", "score", colour = "B", name = "r"), likert = likert)
  expect_recipe_matches(real, rec)
  env <- run_recipe(recipe_factorialrain("likert", "A", "score", colour = "B", name = "r"), likert = likert)
  rain <- ggplot2::ggplot_build(env$r_plot)$data[[2]]
  off <- rain$y - env$r_data$score
  expect_true(all(abs(off) <= 0.15 + 1e-12))
  expect_gt(stats::sd(off), 0.05)
  # wider sideways spread than for continuous outcomes (0.06 of a slot)
  expect_gt(max(abs(rain$x - env$r_data$rain_position)), 0.06 * env$r_slot)
  expect_lte(max(abs(rain$x - env$r_data$rain_position)), 0.1 * env$r_slot + 1e-12)
  # the same jitter whatever the session's seed (fixed seed of the position)
  set.seed(99)
  again <- ggplot2::ggplot_build(env$r_plot)$data[[2]]
  expect_identical(again$y, rain$y)
  expect_identical(layer_data_of(real, "GeomPoint")$y, rain$y)
  # half-point scale: the vertical jitter is 0.15 of the 0.5 gap
  half <- transform(likert, score = score / 2)
  hr <- ggplot2::ggplot_build(ggfactorialrain(half, A, score))$data[[2]]
  expect_true(all(abs(hr$y - half$score) <= 0.075 + 1e-12))
  expect_gt(stats::sd(hr$y - half$score), 0.01)
})

test_that("factorial rain: continuous outcomes keep a sideways-only jitter", {
  tooth <- transform(ToothGrowth, dose = factor(dose))
  env <- run_recipe(recipe_factorialrain("tooth", "dose", "len", colour = "supp", name = "r"), tooth = tooth)
  rain <- ggplot2::ggplot_build(env$r_plot)$data[[2]]
  expect_equal(rain$y, env$r_data$len)
  expect_lte(max(abs(rain$x - env$r_data$rain_position)), 0.06 * env$r_slot + 1e-12)
  real <- ggplot2::ggplot_build(ggfactorialrain(tooth, dose, len, colour = supp))$data[[2]]
  expect_equal(real$y, tooth$len)
})
