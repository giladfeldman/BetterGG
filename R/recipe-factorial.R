#' Recipe: factorial raincloud with model means ([ggfactorialrain()])
#'
#' Returns 'ggplot2' code that draws the [ggfactorialrain()] figure: raw
#' observations and their distribution in every cell, with estimated marginal
#' means and confidence intervals, the second factor by colour and the third
#' by panels.
#'
#' @inheritParams recipe_betweenrain
#' @param x,y Names of the first factor and the outcome.
#' @param colour,facet Optional names of the second and third factor.
#' @param covariates Optional names of numeric covariates.
#' @param means Optional name of a data frame of model means (factor columns
#'   plus `emmean`, `lower.CL`, `upper.CL`). Without it the code fits the
#'   full factorial model with sum-to-zero contrasts and computes the means
#'   with 'emmeans'.
#' @param model With `means` and `stats = TRUE`: name of the fitted model
#'   whose F tests form the subtitle.
#' @param stats Show the type III F test of every model term ([stats::drop1()]
#'   on the sum-to-zero model) as the subtitle.
#' @param legend.title Title of the colour legend.
#' @return R code as one character string.
#' @examples
#' cat(recipe_factorialrain("tooth", "dose", "len", colour = "supp"))
#' @export
recipe_factorialrain <- function(data, x, y, colour = NULL, facet = NULL, covariates = NULL,
                                 name = "plot", means = NULL, model = NULL, stats = FALSE,
                                 conf.level = 0.95, xlab = x, ylab = y, title = NULL,
                                 legend.title = colour, setup = TRUE) {
  .check_name1(data = data, x = x, y = y, colour = colour, facet = facet, means = means, model = model)
  .check_names(covariates = covariates)
  .check_prefix(name)
  .check_conf(conf.level)
  factors <- c(list(x), if (!is.null(colour)) list(colour), if (!is.null(facet)) list(facet))
  cov_list <- if (is.null(covariates)) list() else if (.is_code(covariates)) list(covariates) else as.list(covariates)
  used <- c(list(y), factors, cov_list)
  plain <- unlist(Filter(function(u) !.is_code(u), used))
  if (anyDuplicated(plain)) stop("BetterGG recipe: a column is used twice.", call. = FALSE)
  runtime <- any(vapply(used, .is_code, logical(1)))
  if (runtime && is.null(means)) {
    stop("BetterGG recipe: columns given as code need 'means' from the analysis's own model.", call. = FALSE)
  }
  if (isTRUE(stats) && !is.null(means) && is.null(model)) {
    stop("BetterGG recipe: with 'means' and stats = TRUE, also give 'model'.", call. = FALSE)
  }
  group <- colour %||% x
  v <- list(name = name, data = data, used = .vec(used), factors = .vec(factors), xs = .lit(x),
    ys = .lit(y), gs = .lit(group), conf = .num(conf.level))

  prep <- do.call(.fill, c(list(paste(
    "# 1. Data: complete cases of the outcome, factors and covariates.",
    "{name}_data <- {data}[stats::complete.cases({data}[, {used}, drop = FALSE]), {used}, drop = FALSE]",
    "for (factor_name in {factors}) {",
    "  {name}_data[[factor_name]] <- droplevels(as.factor({name}_data[[factor_name]]))",
    "  if (nlevels({name}_data[[factor_name]]) < 2L) stop(\"Factor \", factor_name, \" needs two observed levels.\")",
    "}",
    sep = "\n")), v))
  if (is.null(means)) {
    rhs <- paste(c(paste(.sym(unlist(factors)), collapse = " * "), .sym(covariates)), collapse = " + ")
    model_name <- paste0(name, "_model")
    prep <- .code(prep, "", do.call(.fill, c(list(paste(
      "# 2. Model: full factorial linear model with sum-to-zero contrasts{cov_note}.",
      "{name}_model_data <- {name}_data",
      "for (factor_name in {factors}) {",
      "  stats::contrasts({name}_model_data[[factor_name]]) <- stats::contr.sum(nlevels({name}_model_data[[factor_name]]))",
      "}",
      "{name}_model <- stats::lm({formula}, data = {name}_model_data)",
      "# Estimated marginal means of every cell (equal cell weights{cov_mean}).",
      "{name}_means <- as.data.frame(stats::confint(",
      "  emmeans::emmeans({name}_model, specs = {factors}, weights = \"equal\"),",
      "  level = {conf}",
      "))",
      sep = "\n"), formula = paste(.sym(y), "~", rhs),
      cov_note = if (length(covariates)) " and covariates" else "",
      cov_mean = if (length(covariates)) "; covariates at their mean" else ""), v)))
  } else {
    model_name <- model
    prep <- .code(prep, "", .fill(paste(
      "# 2. Model means supplied by the analysis (from its own model).",
      "{name}_means <- as.data.frame({means})",
      "for (factor_name in {factors}) {",
      "  {name}_means[[factor_name]] <- factor(as.character({name}_means[[factor_name]]),",
      "    levels = levels({name}_data[[factor_name]]))",
      "}",
      "if (anyNA({name}_means[{factors}])) stop(\"The model means have levels not in the data.\")",
      sep = "\n"), name = name, means = means, factors = v$factors))
  }
  place <- if (is.null(colour)) {
    .fill("{name}_place <- function(df) as.integer(df[[{xs}]])", name = name, xs = v$xs)
  } else {
    .fill(paste0("{name}_place <- function(df) {\n",
      "  as.integer(df[[{xs}]]) + (as.integer(df[[{cs}]]) - ({name}_k + 1) / 2) * {name}_slot\n}"),
      name = name, xs = v$xs, cs = .lit(colour))
  }
  prep <- .code(prep, "", .fill(paste(
    "# 3. Positions: x levels at 1, 2, ...{side_note}",
    "{name}_k <- {k}",
    "{name}_slot <- 0.8 / {name}_k",
    "{place}",
    "{name}_data$x_position <- {name}_place({name}_data)",
    "{name}_data$rain_position <- {name}_data$x_position - 0.2 * {name}_slot",
    "{name}_data$cell <- interaction({name}_data[{factors}], drop = TRUE, lex.order = TRUE)",
    "{name}_means$x_position <- {name}_place({name}_means)",
    sep = "\n"), name = name, place = place, factors = v$factors,
    k = if (is.null(colour)) "1L" else .fill("nlevels({name}_data[[{cs}]])", name = name, cs = .lit(colour)),
    side_note = if (is.null(colour)) "" else "; colour groups side by side within each."))

  stats_code <- if (isTRUE(stats)) .fill(paste(
    "# Statistics: type III F test of every model term (each term dropped from the",
    "# full sum-to-zero model in turn), as in jamovi's ANOVA table.",
    "{name}_dropped <- stats::drop1({model}, scope = ~., test = \"F\")",
    "{name}_tests <- data.frame(",
    "  `model term` = rownames({name}_dropped)[-1], df1 = {name}_dropped$Df[-1],",
    "  df2 = stats::df.residual({model}), F.ratio = {name}_dropped$`F value`[-1],",
    "  p.value = {name}_dropped$`Pr(>F)`[-1], check.names = FALSE",
    ")",
    "{name}_eta <- {name}_tests$F.ratio * {name}_tests$df1 /",
    "  ({name}_tests$F.ratio * {name}_tests$df1 + {name}_tests$df2)",
    "{name}_p <- ifelse({name}_tests$p.value < 0.001, \"< .001\", ifelse({name}_tests$p.value > 0.999, \"> .999\",",
    "  paste(\"=\", sub(\"^0[.]\", \".\", formatC({name}_tests$p.value, format = \"f\", digits = 3)))))",
    "{name}_df2 <- ifelse(abs({name}_tests$df2 - round({name}_tests$df2)) < 1e-8, format(round({name}_tests$df2)),",
    "  formatC({name}_tests$df2, format = \"f\", digits = 2))",
    "{name}_subtitle <- paste0(gsub(\":\", \" \\u00d7 \", {name}_tests[[\"model term\"]], fixed = TRUE),",
    "  \": F(\", {name}_tests$df1, \", \", {name}_df2, \") = \", formatC({name}_tests$F.ratio, format = \"f\", digits = 2),",
    "  \", p \", {name}_p, \", \\u03b7p\\u00b2 = \", sub(\"^0[.]\", \".\", formatC({name}_eta, format = \"f\", digits = 2)),",
    "  collapse = \"\\n\")",
    sep = "\n"), name = name, model = model_name) else NULL

  G <- .fill(".data[[{gs}]]", gs = v$gs)
  layers <- c(
    .fill("ggplot2::ggplot({name}_data, ggplot2::aes(x = x_position, y = .data[[{ys}]]))", name = name, ys = v$ys),
    "  # cloud: the distribution of each cell (half density)",
    .fill(paste0("  ggdist::stat_slab(ggplot2::aes(fill = {G}, group = cell), orientation = \"vertical\",\n",
      "    side = \"right\", justification = 0, width = {name}_slot, scale = 0.45, colour = NA, alpha = 0.5,\n",
      "    normalize = \"groups\")"), G = G, name = name),
    "  # rain: every observation, slightly jittered sideways",
    .fill(paste0("  ggplot2::geom_point(ggplot2::aes(x = rain_position, colour = {G}),\n",
      "    position = ggplot2::position_jitter(width = 0.06 * {name}_slot, height = 0, seed = 1123),\n",
      "    size = 1.4, alpha = 0.5, stroke = 0)"), G = G, name = name),
    if (!is.null(colour)) c(
      "  # model means of each colour group joined across the x levels",
      .fill(paste0("  ggplot2::geom_line(data = {name}_means,\n",
        "    ggplot2::aes(x = x_position, y = emmean, colour = {G}, group = {G}), inherit.aes = FALSE, linewidth = 0.6)"),
        G = G, name = name)
    ),
    .fill("  # estimated marginal means with {pct}% confidence intervals", pct = .num(100 * conf.level)),
    .fill(paste0("  ggplot2::geom_errorbar(data = {name}_means, ggplot2::aes(x = x_position, ymin = lower.CL, ymax = upper.CL),\n",
      "    inherit.aes = FALSE, width = 0.12 * {name}_slot, linewidth = 0.6)"), name = name),
    .fill(paste0("  ggplot2::geom_point(data = {name}_means, ggplot2::aes(x = x_position, y = emmean, fill = {G}),\n",
      "    inherit.aes = FALSE, shape = 21, size = 3, colour = \"black\", stroke = 0.6)"), G = G, name = name),
    .fill("  ggplot2::scale_x_continuous(breaks = seq_len(nlevels({name}_data[[{xs}]])), labels = levels({name}_data[[{xs}]]))",
      name = name, xs = v$xs),
    "  ggplot2::scale_colour_manual(values = figure_colours)",
    "  ggplot2::scale_fill_manual(values = figure_fills)",
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, title = {title}, colour = {lt}, fill = {lt}, subtitle = {sub})",
      xlab = .lit(xlab), ylab = .lit(ylab), title = if (is.null(title)) "NULL" else .lit(title),
      lt = .lit(legend.title %||% group), sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL"),
    "  theme_figure()",
    .fill("  ggplot2::theme(legend.position = {pos}, panel.grid.major.x = ggplot2::element_blank())",
      pos = if (is.null(colour)) "\"none\"" else "\"bottom\""),
    if (!is.null(facet)) .fill("  ggplot2::facet_wrap(ggplot2::vars(.data[[{fs}]]), labeller = ggplot2::label_wrap_gen(width = 30))", fs = .lit(facet))
  )
  plot <- .join_layers(paste0(name, "_plot"), layers)
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 4. The figure", plot)
  .with_setup(code, setup, stats)
}
