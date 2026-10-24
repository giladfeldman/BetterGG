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
  skip_if_not_installed("psych")
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
  skip_if_not_installed("paletteer")
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
  skip_if_not_installed("car")
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

test_that("a statistic that cannot be computed is labelled, never drawn as NA", {
  env <- run_recipe(recipe_setup(stats = TRUE))
  fs <- env$figure_statistics
  ok <- "a: F(1, 96) = 0.72, p = .400\nb: F(1, 96) = 0.03, p = .866"
  expect_identical(fs(ok), ok)
  expect_warning(out <- fs("a: F(0, 98) =  NA, p NA\nb: F(1, 98) = 2.00, p = .160"), "not estimable from these data.*: a$")
  expect_identical(out, "a: not estimable from these data\nb: F(1, 98) = 2.00, p = .160")
  expect_warning(out <- fs("t(98) = NaN"), "The test")
  expect_identical(out, "The test: not estimable from these data")
  # statsExpressions labels are language objects.
  expect_identical(fs(quote(list(italic(t) == "1.20"))), quote(list(italic(t) == "1.20")))
  expect_warning(out <- fs(quote(list(italic(t) == "NA"))), "could not be computed")
  expect_match(out, "^Statistics not shown")
  # typed missing values deparse as NA_real_ / NA_integer_ / NA_character_ (d-e28067)
  for (na in list(NA_real_, NA_integer_, NA_character_)) {
    expect_warning(out <- fs(bquote(list(italic(t) == .(na)))), "could not be computed")
    expect_match(out, "^Statistics not shown")
  }
  expect_warning(fs("t(98) = NA_real_"), "The test")
  # A one-sided test's interval legitimately ends at Inf.
  one_sided <- quote(list(italic(t) == "-2.38", CI["95%"] ~ "[" * "-0.40", "Inf" * "]"))
  expect_identical(fs(one_sided), one_sided)
  # Names that merely contain the letters NA are not flagged.
  expect_identical(fs("NAME: F(1, 9) = 1.00, p = .343"), "NAME: F(1, 9) = 1.00, p = .343")
  # Every recipe with statistics routes its subtitle through the guard.
  code <- recipe_factorialrain(data = "d", x = "a", y = "y", colour = "b", name = "f", stats = TRUE)
  expect_match(code, "subtitle = figure_statistics(f_subtitle)", fixed = TRUE)
})

test_that("the uncorrected Cramer's V option prints V as jamovi computes it", {
  skip_if_not_installed("statsExpressions")
  set.seed(4)
  d <- data.frame(a = factor(sample(c("p", "q", "r"), 90, TRUE)), b = factor(sample(c("u", "v", "w", "z"), 90, TRUE)))
  chi <- unname(suppressWarnings(stats::chisq.test(table(d$a, d$b), correct = FALSE))$statistic)
  expected <- sqrt(chi / (90 * (3 - 1)))
  for (fn in list(recipe_barstats, recipe_piestats)) {
    e <- new.env(); assign("d", d, e)
    eval(parse(text = fn("d", "a", "b", stats = TRUE, cramers_v = "uncorrected")), e)
    expect_equal(unname(e$plot_v), expected)
    label <- gsub("[[:space:]]+", " ", paste(deparse(e$plot_subtitle), collapse = " "))
    expect_match(label, formatC(expected, format = "f", digits = 2), fixed = TRUE)
    expect_match(label, "italic(p) == \".144\"", fixed = TRUE)
    expect_false(grepl("widehat|CI", label))
  }
  # The default stays ggbarstats()'s bias-corrected V with its interval.
  e <- new.env(); assign("d", d, e)
  eval(parse(text = recipe_barstats("d", "a", "b", stats = TRUE)), e)
  expect_match(paste(deparse(e$plot_subtitle), collapse = " "), "widehat", fixed = TRUE)
  expect_error(recipe_barstats("d", "a", "b", cramers_v = "adjusted"))
})

test_that("a Bayesian raincloud has no p-value and draws without a warning", {
  skip_if_not_installed("statsExpressions"); skip_if_not_installed("BayesFactor")
  set.seed(5)
  d <- data.frame(g = factor(rep(c("a", "b"), 30)), y = rnorm(60) + rep(c(0, 0.8), 30))
  e <- new.env(); assign("d", d, e)
  expect_no_warning(eval(parse(text = recipe_betweenrain("d", "g", "y", stats = TRUE, type = "bayes")), e))
  expect_match(paste(deparse(e$plot_subtitle), collapse = " "), "BF", fixed = TRUE)
})

test_that("a bar with a single category is labelled not estimable, never p = NA", {
  skip_if_not_installed("statsExpressions")
  d <- data.frame(a = factor(rep(c("p", "q", "r"), each = 10)),
    b = factor(c(rep("u", 10), rep("v", 5), rep("w", 5), rep("u", 5), rep("v", 5))))
  e <- new.env(); assign("d", d, e)
  # The tiny cells make the chi-squared approximation warn; that is the test data, not the label.
  suppressWarnings(eval(parse(text = recipe_barstats("d", "a", "b", stats = TRUE)), e))
  expect_identical(e$plot_bar_tests$p_label[e$plot_bar_tests$b == "w"], "list(~'not estimable')")
  expect_false(any(grepl("NA", c(e$plot_bar_tests$p_label, e$plot_bar_tests$test_label), fixed = TRUE)))
})

test_that("Bayesian raincloud, 3+ groups: works for a column named g and draws every pair like ggbetweenstats (d-dbe80d)", {
  skip_if_not_installed("statsExpressions"); skip_if_not_installed("BayesFactor")
  set.seed(42)
  y <- rnorm(60) + rep(c(0, 0.8, 1.6), each = 20)
  lv <- factor(rep(c("a", "b", "c"), each = 20))
  run <- function(col) {
    d <- data.frame(lv, y); names(d)[1] <- col
    e <- new.env(); assign("d", d, e)
    set.seed(1)
    expect_no_error(suppressWarnings(eval(parse(text = recipe_betweenrain("d", col, "y", stats = TRUE, type = "bayes")), e)))
    e
  }
  g <- run("g"); grp <- run("grp")
  expect_match(paste(deparse(g$plot_subtitle), collapse = " "), "BF", fixed = TRUE)
  # renaming the column for the test changes nothing but the name
  expect_identical(deparse(g$plot_subtitle), deparse(grp$plot_subtitle))
  # ggbetweenstats(type = "bayes") draws a bar for every pair, labelled with its Bayes factor
  expect_s3_class(g$plot_brackets, "LayerInstance")
  expect_length(g$plot_brackets$stat_params$comparisons, 3L)
  expect_true(all(grepl("BF", g$plot_brackets$stat_params$annotations, fixed = TRUE)))
  expect_match(paste(deparse(g$plot_pairwise_note), collapse = " "), "all (Bayes factors)", fixed = TRUE)
  # the paired (within) design with a condition column named g also runs
  dw <- data.frame(id = rep(1:20, 3), g = lv, y = y)
  e <- new.env(); assign("dw", dw, e)
  expect_no_error(suppressWarnings(eval(parse(text = recipe_withinrain("dw", "g", "y", "id", stats = TRUE, type = "bayes")), e)))
  expect_match(paste(deparse(e$plot_subtitle), collapse = " "), "BF", fixed = TRUE)
})

test_that("reliability: a constant item or total is named, never shown as NA/NaN (d-4c8fe0)", {
  d <- data.frame(a = c(1, 2, 3, 4, 5, 4), b = c(2, 2, 3, 5, 5, 4), c = rep(3, 6))
  expect_warning(real <- ggreliability(d, c("a", "b", "c"), stats = TRUE), "not estimable for c")
  expect_warning(env <- run_recipe(recipe_reliability("d", c("a", "b", "c"), stats = TRUE, name = "r"), d = d),
    "not estimable for c")
  for (p in list(real, env$r_plot)) {
    lab <- p$data$label # the item-rest panel
    expect_false(any(grepl("NA|NaN", lab)))
    expect_true("not estimable" %in% lab)
  }
  # alpha is still estimable: the constant item adds no variance
  k <- 3; v <- vapply(d, stats::var, 0)
  expect_equal(env$r_alpha, k / (k - 1) * (1 - sum(v) / stats::var(rowSums(d))))
  expect_identical(real$patches$annotation$subtitle, env$r_plot$patches$annotation$subtitle)
  # every item constant: alpha is not estimable either
  z <- data.frame(a = rep(1, 5), b = rep(2, 5))
  expect_warning(zr <- ggreliability(z, c("a", "b"), stats = TRUE), "not estimable")
  expect_match(zr$patches$annotation$subtitle, "not estimable", fixed = TRUE)
  expect_no_match(zr$patches$annotation$subtitle, "NaN|NA|Inf")
  expect_warning(ze <- run_recipe(recipe_reliability("z", c("a", "b"), stats = TRUE, name = "r"), z = z), "not estimable")
  expect_identical(zr$patches$annotation$subtitle, ze$r_plot$patches$annotation$subtitle)
})

test_that("pie draws no empty axes and one legend guide with its title; corrmat crosses sit under the numbers (d-12e5d8, d-2110ef)", {
  d <- mtcars; d$cyl <- factor(d$cyl)
  code <- recipe_piestats("d", "cyl")
  expect_identical(lengths(regmatches(code, gregexpr("ggplot2::guides(", code, fixed = TRUE))), 1L)
  e <- run_recipe(code, d = d)
  p <- e$plot_plot
  expect_s3_class(p$theme$axis.line, "element_blank")
  expect_s3_class(p$theme$axis.text, "element_blank")
  expect_identical(p$guides$guides$fill$params$title %||% p$guides$guides$fill$title, "cyl")
  set.seed(1)
  cd <- data.frame(a = rnorm(50), b = rnorm(50), c = rnorm(50))
  cm <- run_recipe(recipe_corrmat("cd", c("a", "b", "c")), cd = cd)$plot_plot
  geoms <- vapply(cm$layers, function(l) class(l$geom)[1], "")
  expect_lt(which(geoms == "GeomPoint"), which(geoms == "GeomText"))
  expect_false(identical(cm$layers[[which(geoms == "GeomPoint")]]$aes_params$colour, "black"))
})

test_that("scatter: the marginal histograms draw no count tick labels (they collided) (d-0d3461)", {
  skip_if_not_installed("ggside")
  side_breaks <- function(p) lapply(c("xsidey", "ysidex"), function(a) {
    sc <- p$scales$get_scales(a)
    if (is.null(sc)) "no scale" else sc$breaks
  })
  rec <- recipe_plot(recipe_scatterstats("mtcars", "wt", "mpg", name = "r"))
  expect_identical(side_breaks(rec), list(NULL, NULL))
  real <- suppressWarnings(ggscatterstats(mtcars, wt, mpg, results.subtitle = FALSE, bf.message = FALSE))
  expect_identical(side_breaks(real), list(NULL, NULL))
  # the main axes keep their breaks
  expect_false(is.null(ggplot2::ggplot_build(rec)$layout$panel_params[[1]]$x$get_labels()))
})

test_that("faceted histogram: mean labels sit in headroom above the bars, beside their line, inside the panel (d-ca0c14)", {
  set.seed(4)
  d <- data.frame(x = c(rnorm(80, 10), rnorm(80, 14)), g = rep(c("a", "b"), each = 80))
  e <- run_recipe(recipe_histostats("d", "x", facet = "g"), d = d)
  b <- ggplot2::ggplot_build(e$plot_plot)
  lab <- b$data[[which(vapply(e$plot_plot$layers, function(l) inherits(l$geom, "GeomLabel"), TRUE))]]
  bars <- b$data[[1]]
  # headroom: the top of the y range is well above the tallest bar
  top <- b$layout$panel_params[[1]]$y.range[2]
  expect_gt(top, 1.15 * max(bars$count))
  # beside the line: left-justified in the left half of the x range, right-justified in the right half
  xr <- b$layout$panel_params[[1]]$x.range
  mid <- mean(xr)
  expect_true(all(ifelse(lab$x < mid, lab$hjust < 0.5, lab$hjust > 0.5)))
})
