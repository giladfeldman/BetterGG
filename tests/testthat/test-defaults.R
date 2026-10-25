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

small <- data.frame(
  a = factor(rep(rep(c("big", "mid", "tiny"), 2), c(90, 7, 3, 60, 30, 10))),
  g = factor(rep(c("p", "q"), each = 100))
)

test_that("stacked bars and pies drop percentage labels of segments below 5%", {
  labels_of <- function(p) layer_data_of(p, "GeomLabel")$label
  real <- ggbarstats(small, a, g, results.subtitle = FALSE)
  rec <- rplot(recipe_barstats("small", "a", "g", name = "r"), small = small)
  expect_recipe_matches(real, rec)
  expect_false("3%" %in% labels_of(rec))
  expect_false("3%" %in% labels_of(real))
  expect_true(all(c("7%", "10%", "90%") %in% labels_of(rec)))
  # the threshold is an argument; 0 keeps every non-empty segment
  real0 <- ggbarstats(small, a, g, results.subtitle = FALSE, min_label_percent = 0)
  rec0 <- rplot(recipe_barstats("small", "a", "g", name = "r", min_label_percent = 0), small = small)
  expect_recipe_matches(real0, rec0)
  expect_true("3%" %in% labels_of(rec0))
  pie <- data.frame(a = factor(rep(c("big", "mid", "tiny"), c(150, 42, 8))))
  realp <- ggpiestats(pie, a, results.subtitle = FALSE)
  recp <- rplot(recipe_piestats("pie", "a", name = "r"), pie = pie)
  expect_recipe_matches(realp, recp)
  expect_identical(sort(labels_of(recp)), c("21%", "75%"))
  # the pie draws no empty axis lines (as recipe_piestats() since 0.2.2)
  expect_s3_class(realp$theme$axis.line, "element_blank")
  expect_s3_class(realp$theme$axis.text, "element_blank")
  expect_error(ggpiestats(pie, a, min_label = 2), "write it in full")
})

long_levels <- c(
  "I strongly believe that the policy described in the scenario would help most people in my community",
  "I am not sure whether the policy described in the scenario would help or harm people in my community",
  "I strongly believe that the policy described in the scenario would harm most people in my community"
)
long <- local({
  set.seed(3)
  data.frame(opinion = factor(sample(long_levels, 150, TRUE), levels = long_levels),
    cond = factor(sample(c("control", "treatment"), 150, TRUE)))
})

fixed_width_in <- function(p) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = 7.2, height = 5.4, units = "in", res = 72)
  on.exit({ grDevices::dev.off(); unlink(file) })
  g <- ggplot2::ggplotGrob(p)
  c(width = grid::convertWidth(sum(g$widths), "in", valueOnly = TRUE),
    height = grid::convertHeight(sum(g$heights), "in", valueOnly = TRUE))
}

test_that("long category labels wrap at 40 characters and move the legend below the plot", {
  real <- ggpiestats(long, opinion, results.subtitle = FALSE)
  rec <- rplot(recipe_piestats("long", "opinion", name = "r"), long = long)
  expect_recipe_matches(real, rec)
  for (p in list(real, rec)) {
    b <- ggplot2::ggplot_build(p)
    shown <- b$plot$scales$get_scales("fill")$get_labels()
    expect_true(all(grepl("\n", shown, fixed = TRUE)))
    expect_true(all(nchar(unlist(strsplit(shown, "\n", fixed = TRUE))) <= 40))
    expect_identical(p$theme$legend.position, "bottom")
    # nothing but the panel needs more than the 7.2 x 5.4 in figure
    size <- fixed_width_in(p)
    expect_lt(size[["width"]], 7.2)
    expect_lt(size[["height"]], 5.4)
  }
  # bars: legend and the category axis both wrap
  realb <- ggbarstats(long, opinion, cond, results.subtitle = FALSE)
  recb <- rplot(recipe_barstats("long", "opinion", "cond", name = "r"), long = long)
  expect_recipe_matches(realb, recb)
  expect_identical(recb$theme$legend.position, "bottom")
  axis_long <- data.frame(group = factor(sample(long_levels, 150, TRUE)), resp = factor(sample(c("y", "n"), 150, TRUE)))
  realx <- ggbarstats(axis_long, resp, group, results.subtitle = FALSE)
  recx <- rplot(recipe_barstats("axis_long", "resp", "group", name = "r"), axis_long = axis_long)
  expect_recipe_matches(realx, recx)
  ticks <- ggplot2::ggplot_build(recx)$layout$panel_params[[1]]$x$get_labels()
  expect_true(all(grepl("\n", ticks, fixed = TRUE)))
  expect_lt(fixed_width_in(recx)[["height"]], 5.4)
  # short labels keep the legend on the right
  short <- rplot(recipe_piestats("mtcars", "cyl", name = "r"))
  expect_false(identical(short$theme$legend.position, "bottom"))
})

likert5 <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
opinions <- local({
  set.seed(8)
  data.frame(resp = factor(sample(likert5, 200, TRUE), levels = likert5, ordered = TRUE),
    resp4 = factor(sample(likert5[-3], 200, TRUE), levels = likert5[-3], ordered = TRUE),
    nominal = factor(sample(likert5, 200, TRUE), levels = likert5),
    cond = factor(sample(c("control", "treatment"), 200, TRUE)))
})

test_that("ordered fill variables get a diverging (odd) or sequential (even) palette in scale order", {
  fills_by_level <- function(p, column) {
    b <- ggplot2::ggplot_build(p)
    b$plot$scales$get_scales("fill")$map(levels(opinions[[column]]))
  }
  diverging <- rev(grDevices::hcl.colors(5, "Blue-Red 2"))
  sequential <- grDevices::hcl.colors(5, "Blues 3", rev = TRUE)[-1]
  real <- ggbarstats(opinions, resp, cond, results.subtitle = FALSE)
  rec <- rplot(recipe_barstats("opinions", "resp", "cond", name = "r"), opinions = opinions)
  expect_recipe_matches(real, rec)
  expect_identical(unname(fills_by_level(rec, "resp")), diverging)
  expect_identical(unname(fills_by_level(real, "resp")), diverging)
  realp <- ggpiestats(opinions, resp4, results.subtitle = FALSE)
  recp <- rplot(recipe_piestats("opinions", "resp4", name = "r"), opinions = opinions)
  expect_recipe_matches(realp, recp)
  expect_identical(unname(fills_by_level(recp, "resp4")), sequential)
  # unordered factors keep the categorical palette
  recn <- rplot(recipe_piestats("opinions", "nominal", name = "r"), opinions = opinions)
  expect_identical(unname(fills_by_level(recn, "nominal")), bgg_palette("nejm", 5, "fill"))
  # a palette the user chose wins
  own <- ggpiestats(opinions, resp, results.subtitle = FALSE, palette = "Set2", package = "RColorBrewer")
  expect_false(identical(unname(fills_by_level(own, "resp")), diverging))
  # greyscale: a grey ramp in scale order
  withr::local_options(BetterGG.palette = "grey")
  greys <- unname(fills_by_level(rplot(recipe_piestats("opinions", "resp", name = "r"), opinions = opinions), "resp"))
  expect_identical(greys, grDevices::gray.colors(5, start = 0.9, end = 0.3))
  expect_identical(unname(fills_by_level(ggpiestats(opinions, resp, results.subtitle = FALSE), "resp")), greys)
})

test_that("ggfreqbars: ordered scales as vertical bars in scale order, many categories as sorted horizontal bars", {
  real <- ggfreqbars(opinions, resp)
  rec <- rplot(recipe_freqbars("opinions", "resp", name = "r"), opinions = opinions)
  expect_recipe_matches(real, rec)
  expect_false(inherits(rec$coordinates, "CoordFlip"))
  b <- ggplot2::ggplot_build(rec)
  expect_identical(b$layout$panel_params[[1]]$x$get_labels(), likert5)
  bars <- b$data[[1]]
  expect_equal(sum(bars$y), 100)
  expect_identical(bars$fill, rev(grDevices::hcl.colors(5, "Blue-Red 2")))
  labels <- layer_data_of(rec, "GeomText")$label
  expect_identical(labels, paste0(round(as.vector(100 * prop.table(table(opinions$resp)))), "%"))
  set.seed(2)
  many <- data.frame(country = factor(sample(paste("Country", LETTERS[1:12]), 300, TRUE, prob = 12:1)))
  realm <- ggfreqbars(many, country)
  recm <- rplot(recipe_freqbars("many", "country", name = "r"), many = many)
  expect_recipe_matches(realm, recm)
  expect_true(inherits(recm$coordinates, "CoordFlip"))
  order_shown <- ggplot2::ggplot_build(recm)$layout$panel_params[[1]]$y$get_labels() # flipped: categories on y
  counts <- table(many$country)
  # bottom to top: smallest to largest, so the largest bar is on top
  expect_false(is.unsorted(as.vector(counts[order_shown])))
  expect_identical(order_shown[length(order_shown)], names(which.max(counts)))
  # few unordered categories: vertical, in level order, one colour
  few <- rplot(recipe_freqbars("opinions", "nominal", name = "r"), opinions = opinions)
  expect_false(inherits(few$coordinates, "CoordFlip"))
  expect_length(unique(ggplot2::ggplot_build(few)$data[[1]]$fill), 1L)
  # weights count each case by its weight; orientation can be forced
  w <- data.frame(a = c("x", "y", "y"), w = c(3, 0.5, 0.5))
  wb <- ggplot2::ggplot_build(rplot(recipe_freqbars("w", "a", name = "r", weights = "w"), w = w))$data[[1]]
  expect_equal(wb$y, c(75, 25))
  expect_equal(ggplot2::ggplot_build(ggfreqbars(w, a, weights = w))$data[[1]]$y, c(75, 25))
  expect_true(inherits(ggfreqbars(opinions, resp, orientation = "horizontal")$coordinates, "CoordFlip"))
  # long unordered names: horizontal; long ordered names: vertical, wrapped to share the axis
  expect_true(inherits(ggfreqbars(long, opinion)$coordinates, "CoordFlip"))
  ordered_long <- transform(long, opinion = factor(opinion, levels = long_levels, ordered = TRUE))
  ol <- ggfreqbars(ordered_long, opinion)
  expect_false(inherits(ol$coordinates, "CoordFlip"))
  ticks <- ggplot2::ggplot_build(ol)$layout$panel_params[[1]]$x$get_labels()
  expect_true(all(nchar(unlist(strsplit(ticks, "\n", fixed = TRUE))) <= floor(90 / 3)))
})

binary <- local({
  set.seed(21)
  data.frame(passed = rbinom(150, 1, 0.3), group = factor(rep(c("A", "B", "C"), each = 50)),
    w = rep(c(1, 2, 3), 50))
})

test_that("ggproportion: proportion of 1s with Wilson intervals, by group, with frequency weights", {
  real <- ggproportion(binary, passed)
  rec <- rplot(recipe_proportion("binary", "passed", name = "r"), binary = binary)
  expect_recipe_matches(real, rec)
  bar <- ggplot2::ggplot_build(rec)$data[[1]]
  ci <- layer_data_of(rec, "GeomErrorbar")
  ref <- stats::prop.test(sum(binary$passed), nrow(binary), correct = FALSE)
  expect_equal(bar$y, mean(binary$passed))
  expect_equal(c(ci$ymin, ci$ymax), unname(ref$conf.int[1:2]), tolerance = 1e-10)
  expect_match(rec$labels$caption, "Wilson")
  expect_match(rec$labels$caption, "95%")
  realg <- ggproportion(binary, passed, group = group, conf.level = 0.9)
  recg <- rplot(recipe_proportion("binary", "passed", group = "group", name = "r", conf.level = 0.9), binary = binary)
  expect_recipe_matches(realg, recg)
  cig <- layer_data_of(recg, "GeomErrorbar")
  for (i in 1:3) {
    part <- binary$passed[binary$group == levels(binary$group)[i]]
    ref <- stats::prop.test(sum(part), length(part), conf.level = 0.9, correct = FALSE)$conf.int
    expect_equal(c(cig$ymin[i], cig$ymax[i]), unname(ref[1:2]), tolerance = 1e-10)
  }
  # frequency weights: the same as repeating each row by its weight
  wide <- binary[rep(seq_len(nrow(binary)), binary$w), ]
  a <- layer_data_of(rplot(recipe_proportion("binary", "passed", "group", name = "r", weights = "w"), binary = binary), "GeomErrorbar")
  b <- layer_data_of(rplot(recipe_proportion("wide", "passed", "group", name = "r"), wide = wide), "GeomErrorbar")
  expect_equal(a[c("ymin", "ymax")], b[c("ymin", "ymax")])
  expect_equal(layer_data_of(ggproportion(binary, passed, group, weights = w), "GeomErrorbar")[c("ymin", "ymax")],
    b[c("ymin", "ymax")])
  # logical columns are 0/1; anything else is refused
  lg <- transform(binary, passed = passed == 1)
  expect_equal(ggplot2::ggplot_build(ggproportion(lg, passed))$data[[1]]$y, mean(binary$passed))
  expect_error(ggproportion(transform(binary, passed = passed + 1), passed), "only 0 and 1")
  expect_error(run_recipe(recipe_proportion("bad", "passed", name = "r"), bad = transform(binary, passed = passed * 3)),
    "only 0 and 1")
})

test_that("recipe_proportion keeps the groups in a column named after their variable", {
  rec <- rplot(recipe_proportion("binary", "passed", "group", name = "r"), binary = binary)
  expect_true("group" %in% names(rec$data))
  ticks <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_true(startsWith(ticks(rec)[1], paste0(levels(binary$group)[1], "\n(n = ")))
  # renaming the categories on the finished figure renames its ticks; n stays with its group
  renamed <- rec
  renamed$data$group <- factor(renamed$data$group, labels = paste("Group", seq_len(nlevels(binary$group))))
  expect_identical(sub("\n.*", "", ticks(renamed)), paste("Group", seq_len(nlevels(binary$group))))
  expect_identical(sub("^.*\n", "", ticks(renamed)), sub("^.*\n", "", ticks(rec)))
  # a group variable named like a summary column does not overwrite it
  clash <- transform(binary, n = group)
  recn <- rplot(recipe_proportion("clash", "passed", "n", name = "r"), clash = clash)
  expect_equal(sort(recn$data$n), sort(as.numeric(table(binary$group))))
})
