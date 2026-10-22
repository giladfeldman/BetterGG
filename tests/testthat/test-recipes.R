# Each recipe is run on data and compared with the real BetterGG plot it
# imitates (see helper-recipes.R): same layers, values, colours and labels.

tooth <- transform(ToothGrowth, dose = factor(dose))
tooth$age <- seq(20, 79, length.out = 60)
tooth$site <- rep(c("A", "B"), 30)
bugs <- ggstatsplot::bugs_long
bugs2 <- bugs[bugs$condition %in% c("HDLF", "HDHF"), ]
gaps <- mtcars[, c("mpg", "cyl", "disp", "hp", "qsec")]
gaps$mpg[c(1, 5)] <- NA
fit <- lm(mpg ~ wt + hp, mtcars)

recipe_plot <- function(code, object = "r_plot", ...) {
  set.seed(1)
  run_recipe(code, ...)[[object]]
}

test_that("recipes write self-contained code without reading data", {
  code <- recipe_betweenrain("no_such_data", "group", "score", name = "r")
  expect_type(code, "character")
  expect_length(code, 1L)
  expect_false(grepl("BetterGG::", code, fixed = TRUE))
  expect_true(grepl("theme_figure <- function", code, fixed = TRUE))
  expect_false(grepl("theme_figure <- function", recipe_betweenrain("d", "g", "s", setup = FALSE), fixed = TRUE))
  for (code in list(
    recipe_withinrain("d", "x", "y", "id"), recipe_factorialrain("d", "a", "y", colour = "b"),
    recipe_histostats("d", "x"), recipe_scatterstats("d", "x", "y"), recipe_corrmat("d", c("a", "b")),
    recipe_barstats("d", "a", "b"), recipe_piestats("d", "a"), recipe_coefstats("m"),
    recipe_reliability("d", c("a", "b"))
  )) {
    expect_false(grepl("BetterGG::", code, fixed = TRUE))
    expect_silent(parse(text = code))
  }
  expect_error(recipe_betweenrain("d", c("a", "b"), "y"), "must be one name")
  expect_error(recipe_betweenrain("d", "a", "y", name = "1bad"), "must start with a letter")
  expect_error(recipe_betweenrain("d", "a", "y", conf.level = 95), "between 0 and 1")
})

test_that("column names that are not syntactic are written safely", {
  odd <- data.frame(`my group` = rep(c("a", "b"), each = 10), `the score` = 1:20, check.names = FALSE)
  plot <- recipe_plot(recipe_betweenrain("odd", "my group", "the score", name = "r"), odd = odd)
  expect_s3_class(plot, "ggplot")
  expect_identical(plot$labels$x, "my group")
})

test_that("between raincloud recipe matches ggbetweenrain", {
  real <- ggbetweenrain(mtcars, cyl, mpg, results.subtitle = FALSE, pairwise.display = "none", bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_betweenrain("mtcars", "cyl", "mpg", name = "r")))
  for (type in c("nonparametric", "robust")) {
    real <- ggbetweenrain(mtcars, cyl, mpg, type = type, results.subtitle = FALSE, pairwise.display = "none")
    expect_recipe_matches(real, recipe_plot(recipe_betweenrain("mtcars", "cyl", "mpg", name = "r", type = type)))
  }
})

test_that("between raincloud recipe statistics match ggbetweenrain", {
  set.seed(1)
  real <- ggbetweenrain(mtcars, cyl, mpg)
  expect_recipe_matches(real, recipe_plot(recipe_betweenrain("mtcars", "cyl", "mpg", name = "r", stats = TRUE, bf.message = TRUE)))
  two <- mtcars[mtcars$cyl != 6, ]
  real <- ggbetweenrain(two, cyl, mpg, bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_betweenrain("two", "cyl", "mpg", name = "r", stats = TRUE), two = two))
  set.seed(1) # the rank effect size's confidence interval is bootstrapped
  real <- ggbetweenrain(mtcars, cyl, mpg, type = "np", bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_betweenrain("mtcars", "cyl", "mpg", name = "r", stats = TRUE, type = "np")))
})

test_that("Student's t is used when var.equal = TRUE (ggbetweenstats cannot)", {
  two <- mtcars[mtcars$cyl != 6, ]
  env <- run_recipe(recipe_betweenrain("two", "am", "mpg", name = "r", stats = TRUE, var.equal = TRUE), two = two)
  student <- stats::t.test(mpg ~ am, data = two, var.equal = TRUE)
  expect_equal(env$r_test$statistic, unname(student$statistic), tolerance = 1e-8)
  expect_equal(env$r_test$p.value, student$p.value, tolerance = 1e-10)
})

test_that("model means replace the observed means when supplied", {
  model <- lm(mpg ~ factor(cyl), mtcars)
  means <- as.data.frame(confint(emmeans::emmeans(model, "cyl")))
  plot <- recipe_plot(recipe_betweenrain("mtcars", "cyl", "mpg", name = "r", means = "means"), means = means)
  built <- ggplot2::ggplot_build(plot)
  expect_equal(built$data[[4]]$ymin, means$lower.CL)
  expect_equal(built$data[[5]]$y, means$emmean)
})

test_that("within raincloud recipe matches ggwithinrain", {
  real <- ggwithinrain(bugs2, condition, desire, subject.id = subject, results.subtitle = FALSE,
    pairwise.display = "none", bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_withinrain("bugs2", "condition", "desire", "subject", name = "r"), bugs2 = bugs2))
  real <- ggwithinrain(bugs, condition, desire, subject.id = subject, results.subtitle = FALSE,
    pairwise.display = "none", bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_withinrain("bugs", "condition", "desire", "subject", name = "r"), bugs = bugs))
  real <- ggwithinrain(bugs, condition, desire, subject.id = subject, bf.message = FALSE)
  expect_recipe_matches(real, recipe_plot(recipe_withinrain("bugs", "condition", "desire", "subject", name = "r", stats = TRUE), bugs = bugs))
})

test_that("factorial raincloud recipe matches ggfactorialrain", {
  expect_recipe_matches(ggfactorialrain(tooth, dose, len, colour = supp),
    recipe_plot(recipe_factorialrain("tooth", "dose", "len", colour = "supp", name = "r"), tooth = tooth))
  real <- ggfactorialrain(tooth, dose, len, colour = supp, facet = site, covariates = "age", stats = TRUE)
  expect_recipe_matches(real, recipe_plot(recipe_factorialrain("tooth", "dose", "len", colour = "supp", facet = "site",
    covariates = "age", name = "r", stats = TRUE), tooth = tooth))
  expect_recipe_matches(ggfactorialrain(tooth, supp, len, covariates = "age"),
    recipe_plot(recipe_factorialrain("tooth", "supp", "len", covariates = "age", name = "r"), tooth = tooth))
})

test_that("ggfactorialrain means and F tests come from the full factorial model", {
  model_data <- tooth
  contrasts(model_data$dose) <- contr.sum(3)
  model_data$supp <- factor(model_data$supp)
  contrasts(model_data$supp) <- contr.sum(2)
  model <- lm(len ~ dose * supp, model_data)
  ref <- as.data.frame(confint(emmeans::emmeans(model, c("dose", "supp"))))
  plot <- ggfactorialrain(tooth, dose, len, colour = supp, stats = TRUE)
  built <- ggplot2::ggplot_build(plot)
  expect_equal(sort(built$data[[5]]$y), sort(ref$emmean))
  # Type III F of the interaction, independently from drop1 on sum contrasts.
  anova3 <- drop1(model, scope = ~ dose:supp, test = "F")
  expect_match(plot$labels$subtitle, paste0("dose \u00d7 supp: F(2, 54) = ", formatC(anova3$`F value`[2], format = "f", digits = 2)), fixed = TRUE)
  expect_error(ggfactorialrain(tooth, dose, len, colour = supp, means = ref[1:2, ]), NA)
  expect_error(ggfactorialrain(tooth, dose, len, stats = TRUE, means = ref), "needs the model")
  expect_error(ggfactorialrain(transform(tooth, one = "x"), dose, len, colour = one), "two observed levels")
})

test_that("histogram recipe matches gghistostats", {
  expect_recipe_matches(gghistostats(mtcars, mpg, results.subtitle = FALSE, bf.message = FALSE),
    recipe_plot(recipe_histostats("mtcars", "mpg", name = "r")))
  set.seed(1)
  real <- gghistostats(mtcars, mpg, test.value = 20)
  expect_recipe_matches(real, recipe_plot(recipe_histostats("mtcars", "mpg", name = "r", stats = TRUE, test.value = 20, bf.message = TRUE)))
  expect_recipe_matches(gghistostats(mtcars, mpg, type = "np", results.subtitle = FALSE),
    recipe_plot(recipe_histostats("mtcars", "mpg", name = "r", type = "np")))
})

test_that("weighted and split histograms draw weighted counts and one centre per panel", {
  w <- data.frame(x = c(1, 2, 2, 3, 10), w = c(1, 1, 2, 1, 0), g = c("a", "a", "b", "b", "b"))
  env <- run_recipe(recipe_histostats("w", "x", name = "r", weights = "w", binwidth = 1), w = w)
  expect_equal(env$r_centre, weighted.mean(w$x, w$w))
  expect_equal(sum(ggplot2::ggplot_build(env$r_plot)$data[[1]]$count), sum(w$w))
  env <- run_recipe(recipe_histostats("w", "x", name = "r", facet = "g", binwidth = 1), w = w)
  expect_equal(env$r_centres$value, c(1.5, 5))
})

test_that("scatter recipe matches ggscatterstats", {
  expect_recipe_matches(ggscatterstats(mtcars, wt, mpg, results.subtitle = FALSE, bf.message = FALSE),
    recipe_plot(recipe_scatterstats("mtcars", "wt", "mpg", name = "r")))
  expect_recipe_matches(ggscatterstats(mtcars, wt, mpg, bf.message = FALSE),
    recipe_plot(recipe_scatterstats("mtcars", "wt", "mpg", name = "r", stats = TRUE)))
})

test_that("correlation matrix recipe matches ggcorrmat", {
  expect_recipe_matches(ggcorrmat(gaps), recipe_plot(recipe_corrmat("gaps", names(gaps), name = "r"), gaps = gaps))
  expect_recipe_matches(ggcorrmat(gaps, type = "np"),
    recipe_plot(recipe_corrmat("gaps", names(gaps), name = "r", type = "np"), gaps = gaps))
})

test_that("ggcorrmat's threshold caption no longer triggers an APA warning", {
  expect_no_warning(ggcorrmat(mtcars[, 1:4]))
  expect_warning(apa_p(ggplot2::ggplot() + ggplot2::labs(subtitle = quote(italic(p) ~ "is small"))), "left unchanged")
})

test_that("bar and pie recipes match ggbarstats and ggpiestats", {
  expect_recipe_matches(ggbarstats(mtcars, am, cyl, results.subtitle = FALSE),
    recipe_plot(recipe_barstats("mtcars", "am", "cyl", name = "r")))
  expect_recipe_matches(ggbarstats(mtcars, am, cyl, bf.message = FALSE),
    recipe_plot(recipe_barstats("mtcars", "am", "cyl", name = "r", stats = TRUE)))
  expect_recipe_matches(ggpiestats(mtcars, cyl, results.subtitle = FALSE),
    recipe_plot(recipe_piestats("mtcars", "cyl", name = "r")))
  expect_recipe_matches(ggpiestats(mtcars, cyl, bf.message = FALSE),
    recipe_plot(recipe_piestats("mtcars", "cyl", name = "r", stats = TRUE)))
  expect_recipe_matches(ggpiestats(mtcars, am, cyl, bf.message = FALSE),
    recipe_plot(recipe_piestats("mtcars", "am", "cyl", name = "r", stats = TRUE)))
})

test_that("frequency counts give the same bars as one row per case", {
  cases <- data.frame(a = c("x", "x", "y", "y", "y"), b = c("p", "q", "p", "p", "q"))
  counted <- as.data.frame(table(a = cases$a, b = cases$b), responseName = "n")
  one <- run_recipe(recipe_barstats("cases", "a", "b", name = "r"), cases = cases)
  two <- run_recipe(recipe_barstats("counted", "a", "b", name = "r", counts = "n"), counted = counted)
  expect_equal(one$r_cells$counts, two$r_cells$counts)
})

test_that("coefficient recipe matches ggcoefstats", {
  real <- ggcoefstats(fit, stats.labels = FALSE)
  real$labels$caption <- NULL # statistics (AIC/BIC) are off by default in the recipe
  expect_recipe_matches(real, recipe_plot(recipe_coefstats("fit", name = "r"), fit = fit))
  # with statistics: same labels and AIC/BIC; the recipe colours labels black
  expect_recipe_matches(ggcoefstats(fit), recipe_plot(recipe_coefstats("fit", name = "r", stats = TRUE), fit = fit),
    skip = list("4" = "colour"))
})

test_that("odds ratios use Wald or profile intervals on a log axis", {
  logit <- glm(am ~ wt, data = mtcars, family = binomial())
  env <- run_recipe(recipe_coefstats("logit", name = "r", ci = "wald", exponentiate = TRUE, intercept = FALSE), logit = logit)
  expect_equal(env$r_table$estimate, unname(exp(coef(logit))["wt"]))
  expect_equal(env$r_table$conf.low, unname(exp(confint.default(logit))["wt", 1]))
  built <- ggplot2::ggplot_build(env$r_plot)
  expect_equal(built$data[[3]]$xintercept, 0) # log10(1)
  env <- run_recipe(recipe_coefstats("logit", name = "r", ci = "profile"), logit = logit)
  expect_equal(env$r_table$conf.high, unname(suppressMessages(confint(logit))[, 2]))
})

test_that("reliability recipe matches ggreliability and the alpha formula", {
  set.seed(2)
  latent <- rnorm(120)
  item <- function(sign, sd) pmin(5, pmax(1, round(3 + sign * latent + rnorm(120, 0, sd))))
  survey <- data.frame(q1 = item(1, 0.8), q2 = item(1, 0.9), q3 = item(-1, 0.9), q4 = item(0.5, 1))
  survey$q2[3] <- NA
  real <- ggreliability(survey, c("q1", "q2", "q3", "q4"), reverse = "q3", stats = TRUE)
  env <- run_recipe(recipe_reliability("survey", c("q1", "q2", "q3", "q4"), reverse = "q3", stats = TRUE, name = "r"),
    survey = survey)
  expect_recipe_matches(real$patches$plots[[1]], env$r_plot$patches$plots[[1]])
  strip <- function(p) {
    p$patches <- NULL
    class(p) <- setdiff(class(p), "patchwork")
    p
  }
  expect_recipe_matches(strip(real), strip(env$r_plot))
  expect_identical(real$patches$annotation$subtitle, env$r_plot$patches$annotation$subtitle)
  # independent reference: psych::alpha on the reverse-keyed complete rows
  complete <- survey[stats::complete.cases(survey), ]
  complete$q3 <- min(complete$q3) + max(complete$q3) - complete$q3
  ref <- psych::alpha(complete, warnings = FALSE)
  expect_equal(env$r_alpha, ref$total$raw_alpha, tolerance = 1e-10)
  expect_equal(unname(env$r_rest), ref$item.stats$r.drop, tolerance = 1e-10)
})

test_that("continuous items get densities instead of response bars", {
  set.seed(3)
  wide <- data.frame(a = rnorm(40), b = rnorm(40), c = rnorm(40))
  real <- ggreliability(wide, c("a", "b", "c"))
  env <- run_recipe(recipe_reliability("wide", c("a", "b", "c"), name = "r"), wide = wide)
  expect_recipe_matches(real$patches$plots[[1]], env$r_plot$patches$plots[[1]])
  expect_s3_class(real$patches$plots[[1]]$layers[[1]]$geom, "GeomSlabinterval")
})

test_that("columns known only when the code runs can be given as code with I()", {
  model_data <- tooth
  contrasts(model_data$dose) <- contr.sum(3)
  model_data$supp <- factor(model_data$supp)
  contrasts(model_data$supp) <- contr.sum(2)
  model <- lm(len ~ dose * supp + age, model_data)
  means <- as.data.frame(confint(emmeans::emmeans(model, c("dose", "supp"))))
  fixed <- recipe_plot(recipe_factorialrain("tooth", "dose", "len", colour = "supp", covariates = "age",
    name = "r", means = "means"), tooth = tooth, means = means)
  code <- recipe_factorialrain("tooth", I("found[1]"), "len", colour = I("found[2]"), covariates = I("covs"),
    name = "r", means = "means", xlab = "dose", legend.title = "supp")
  expect_match(code, ".data[[found[2]]]", fixed = TRUE)
  runtime <- recipe_plot(code, tooth = tooth, means = means, found = c("dose", "supp"), covs = "age")
  expect_recipe_matches(fixed, runtime)
  expect_error(recipe_factorialrain("d", I("f[1]"), "y"), "need 'means'")
})

test_that("one- and two-sided tests follow 'alternative'", {
  two <- mtcars[mtcars$cyl != 6, ]
  env <- run_recipe(recipe_betweenrain("two", "am", "mpg", name = "r", stats = TRUE, var.equal = TRUE,
    alternative = "less"), two = two)
  expect_equal(env$r_test$p.value, stats::t.test(mpg ~ am, data = two, var.equal = TRUE, alternative = "less")$p.value)
})

test_that("the written-out NEJM palette is the ggsci palette", {
  expect_identical(bgg_palette("nejm", 8), substr(as.character(paletteer::paletteer_d("ggsci::default_nejm")), 1, 7))
})

test_that("weighted bars and pies count each case by its weight", {
  cases <- data.frame(a = c("x", "x", "y", "y", "y"), b = c("p", "q", "p", "p", "q"), w = c(2, 1, 0.5, 1, 3))
  env <- run_recipe(recipe_barstats("cases", "a", "b", name = "r", weights = "w"), cases = cases)
  cell <- env$r_cells[env$r_cells$b == "p" & env$r_cells$a == "y", ]
  expect_equal(cell$counts, 1.5)
  expect_equal(cell$perc, 100 * 1.5 / 3.5)
  env <- run_recipe(recipe_piestats("cases", "a", name = "r", weights = "w"), cases = cases)
  expect_equal(env$r_cells$counts, c(3, 4.5))
  expect_error(recipe_piestats("cases", "a", weights = "w", stats = TRUE), "not available with case weights")
})

test_that("bars can be split into panels", {
  cases <- data.frame(a = rep(c("x", "y"), 8), b = rep(c("p", "q"), each = 8), g = rep(c("u", "v"), each = 4, times = 2))
  env <- run_recipe(recipe_barstats("cases", "a", "b", name = "r", facet = "g"), cases = cases)
  expect_equal(nrow(env$r_cells), 8L)
  expect_true(all(abs(tapply(env$r_cells$perc, interaction(env$r_cells$g, env$r_cells$b), sum) - 100) < 1e-10))
  expect_equal(length(unique(ggplot2::ggplot_build(env$r_plot)$data[[1]]$PANEL)), 2L)
})

test_that("more categories than palette colours still draw, each with its own colour", {
  many <- data.frame(a = factor(sprintf("c%02d", rep(1:12, 2))))
  env <- run_recipe(recipe_piestats("many", "a", name = "r", max_labels = 12), many = many)
  fills <- ggplot2::ggplot_build(env$r_plot)$data[[1]]$fill
  expect_length(unique(fills), 12L)
})

test_that("Kendall and partial correlations follow their definitions", {
  set.seed(4)
  d <- data.frame(x = rnorm(60), z = rnorm(60))
  d$y <- d$x + d$z + rnorm(60)
  env <- run_recipe(recipe_corrmat("d", c("x", "y"), name = "r", method = "kendall"), d = d)
  ref <- suppressWarnings(stats::cor.test(d$x, d$y, method = "kendall", exact = FALSE))
  expect_equal(env$r_cells$r, unname(ref$estimate))
  env <- run_recipe(recipe_corrmat("d", c("x", "y"), name = "r", controls = "z", p.adjust.method = "none"), d = d)
  r <- stats::cor(d)
  partial <- (r["x", "y"] - r["x", "z"] * r["y", "z"]) / sqrt((1 - r["x", "z"]^2) * (1 - r["y", "z"]^2))
  t <- partial * sqrt((60 - 3) / (1 - partial^2))
  expect_equal(env$r_cells$r, partial, tolerance = 1e-12)
  expect_equal(env$r_cells$p, 2 * stats::pt(-abs(t), 57), tolerance = 1e-12)
  expect_error(recipe_corrmat("d", c("x", "y"), method = "kendall", controls = "z"), "partial Kendall")
})

test_that("pairs that cannot be correlated stay empty instead of stopping the figure", {
  d <- data.frame(x = 1:10, y = rep(NA_real_, 10), z = sin(1:10))
  env <- run_recipe(recipe_corrmat("d", c("x", "y", "z"), name = "r"), d = d)
  expect_true(all(is.na(env$r_cells$r[env$r_cells$Var1 == "x" & env$r_cells$Var2 == "y"])))
  expect_s3_class(ggplot2::ggplot_build(env$r_plot), "ggplot_built")
})

test_that("the BetterGG font draws on the Windows png() device without warnings", {
  if (.Platform$OS.type == "windows") {
    font <- bgg_font()
    if (nzchar(font)) expect_true(font %in% names(grDevices::windowsFonts()))
    file <- tempfile(fileext = ".png")
    expect_no_warning({
      grDevices::png(file, width = 400, height = 300, type = "windows")
      print(ggbetweenrain(mtcars, cyl, mpg, results.subtitle = FALSE))
      grDevices::dev.off()
    })
    env <- run_recipe(recipe_setup())
    if (nzchar(env$figure_font)) expect_true(env$figure_font %in% names(grDevices::windowsFonts()))
  } else {
    # off Windows there is no windowsFonts table to register in
    expect_false(exists("windowsFonts", envir = asNamespace("grDevices")))
  }
})

test_that("a one-factor factorial figure does not join the means of a nominal factor", {
  plot <- ggfactorialrain(tooth, dose, len, covariates = "age")
  expect_false(any(vapply(plot$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
})

test_that("factorial F tests are exact (car's type III), not rounded", {
  model_data <- tooth
  model_data$supp <- factor(model_data$supp)
  model <- lm(len ~ dose * supp + age, model_data, contrasts = list(dose = contr.sum, supp = contr.sum))
  tests <- BetterGG:::.type3_tests(model)
  ref <- car::Anova(model, type = 3)
  for (term in c("dose", "supp", "age", "dose:supp")) {
    expect_equal(tests$F.ratio[tests[["model term"]] == term], ref[term, "F value"], tolerance = 1e-10)
    expect_equal(tests$p.value[tests[["model term"]] == term], ref[term, "Pr(>F)"], tolerance = 1e-10)
  }
  env <- run_recipe(recipe_factorialrain("tooth", "dose", "len", colour = "supp", covariates = "age",
    name = "r", stats = TRUE), tooth = tooth)
  expect_equal(env$r_tests$F.ratio, tests$F.ratio, tolerance = 1e-12)
})

test_that("ggfactorialrain accepts column names computed at run time, NULL meaning none", {
  found <- c("dose", NA)
  plot <- ggfactorialrain(tooth, found[1], "len", colour = if (!is.na(found[2])) found[2])
  expect_identical(plot$labels$x, "dose")
  expect_false(any(vapply(plot$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
})

test_that("readable factor terms name the level and its reference", {
  model <- lm(mpg ~ wt + am, data = transform(mtcars, am = factor(am, labels = c("auto", "manual"))))
  env <- run_recipe(recipe_coefstats("model", name = "r", readable_terms = TRUE, intercept = FALSE), model = model)
  expect_setequal(as.character(env$r_table$term), c("wt", "am: manual vs auto"))
  env <- run_recipe(recipe_coefstats("model", name = "r", intercept = FALSE), model = model)
  expect_setequal(as.character(env$r_table$term), c("wt", "ammanual"))
})

test_that("long facet titles wrap instead of being clipped at the panel edge", {
  d <- data.frame(score = c(1, 2, 3, 4, 5, 6),
    panel = rep(c("A very long condition label that would be clipped", "Short"), 3))
  env <- run_recipe(recipe_histostats("d", "score", name = "h", facet = "panel"), d = d)
  labeller <- env$h_plot$facet$params$labeller
  labels <- labeller(data.frame(panel = unique(d$panel)))[[1]]
  expect_identical(labels[2], "Short")
  expect_match(labels[1], "\n", fixed = TRUE)
  expect_s3_class(ggplot2::ggplot_build(env$h_plot), "ggplot_built")
  for (code in list(recipe_histostats("d", "x", facet = "f"), recipe_barstats("d", "a", "b", facet = "f"),
    recipe_piestats("d", "a", "b"), recipe_factorialrain("d", "a", "y", facet = "f"))) {
    expect_match(code, "labeller = ggplot2::label_wrap_gen(width = 30)", fixed = TRUE)
  }
})
