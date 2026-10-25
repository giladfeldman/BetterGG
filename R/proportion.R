#' Proportion of 1s with a Wilson confidence interval
#'
#' For a 0/1 (or FALSE/TRUE) variable: the proportion of 1s as a bar with
#' its Wilson score confidence interval, one bar per level of `group` when
#' given. The caption names the interval. Use it instead of a histogram,
#' which shows a 0/1 variable as two spikes.
#'
#' The Wilson score interval for `k` ones out of `n` (with
#' `z = qnorm(1 - (1 - conf.level) / 2)`) is centred at
#' `(p + z^2 / (2n)) / (1 + z^2 / n)` with half-width
#' `z / (1 + z^2 / n) * sqrt(p (1 - p) / n + z^2 / (4 n^2))`, `p = k / n`;
#' it is the interval of [stats::prop.test()] without continuity correction.
#' Weights count as frequency weights: `k` and `n` are sums of weights (as
#' when each row is repeated by its weight).
#'
#' The figure is drawn by running the code [recipe_proportion()] writes, so
#' the function and the recipe always draw the same figure.
#'
#' @param data A data frame.
#' @param x The 0/1 column (bare or quoted); FALSE/TRUE is read as 0/1. Any
#'   other value is an error.
#' @param group Optional grouping column (bare or quoted): one bar per level.
#' @param weights Optional column of frequency weights.
#' @param conf.level Confidence level of the intervals.
#' @param xlab,ylab,title Axis titles and plot title; `ylab` defaults to
#'   "% with <x> = 1".
#' @param wrap_width Wrap group labels at this many characters.
#' @return A `ggplot` object.
#' @examples
#' ggproportion(mtcars, am, group = cyl)
#' @export
ggproportion <- function(data, x, group = NULL, weights = NULL, conf.level = 0.95,
                         xlab = NULL, ylab = NULL, title = NULL, wrap_width = 40) {
  x <- .column_name(rlang::enquo(x))
  group <- .column_name(rlang::enquo(group), optional = TRUE)
  weights <- .column_name(rlang::enquo(weights), optional = TRUE)
  .check_columns(data, c(x, group, weights))
  .run_recipe_code(recipe_proportion("source_data", x, group = group, name = "bgg", weights = weights,
    conf.level = conf.level, xlab = xlab %||% group, ylab = ylab, title = title,
    wrap_width = wrap_width, setup = TRUE), data)
}

#' Recipe: proportion of 1s with a Wilson confidence interval ([ggproportion()])
#'
#' Returns 'ggplot2' code that draws the [ggproportion()] figure.
#'
#' @inheritParams recipe_barstats
#' @inheritParams ggproportion
#' @param x Name of the 0/1 column.
#' @param group Optional name of a grouping column.
#' @param weights Optional name of a column of frequency weights.
#' @return R code as one character string.
#' @examples
#' cat(recipe_proportion("mtcars", "am", group = "cyl"))
#' @export
recipe_proportion <- function(data, x, group = NULL, name = "plot", weights = NULL, conf.level = 0.95,
                              xlab = group, ylab = NULL, title = NULL, wrap_width = 40, setup = TRUE) {
  .check_name1(data = data, x = x, group = group, weights = weights)
  .check_prefix(name)
  .check_conf(conf.level)
  .check_label_args(0, wrap_width)
  cols <- c(x, group, weights)
  pct <- .num(100 * conf.level)
  caption <- paste0("Bars: proportion of 1s; error bars: ", pct, "% Wilson score confidence intervals",
    if (is.null(weights)) "." else " (weights as frequencies).")
  v <- list(name = name, data = data, cols = .vec(cols), xs = .lit(x), conf = .num(conf.level),
    ww = .num(wrap_width), wrap = .wrap_code(wrap_width))
  prep <- do.call(.fill, c(list(paste(c(
    "# 1. Data: complete cases; {xs} must hold only 0 and 1 (FALSE/TRUE count as 0/1).",
    "{name}_data <- {data}[stats::complete.cases({data}[, {cols}, drop = FALSE]), {cols}, drop = FALSE]",
    "{name}_y <- as.numeric({name}_data[[{xs}]])",
    "if (!all({name}_y %in% c(0, 1))) stop(\"BetterGG: \", {xs}, \" must contain only 0 and 1 (or FALSE/TRUE).\")",
    if (is.null(weights)) "{name}_weight <- rep(1, nrow({name}_data))" else
      paste0("{name}_weight <- {name}_data[[", .lit(weights), "]] # frequency weights"),
    if (is.null(group)) "{name}_group <- factor(rep({xs}, nrow({name}_data)), levels = {xs})" else
      paste0("{name}_group <- droplevels(as.factor({name}_data[[", .lit(group), "]]))"),
    "",
    "# 2. Proportion of 1s in each group and its Wilson score interval",
    "# (the interval of prop.test() without continuity correction).",
    "{name}_z <- stats::qnorm(1 - (1 - {conf}) / 2)",
    "{name}_summary <- do.call(rbind, lapply(levels({name}_group), function(level) {",
    "  keep <- {name}_group == level",
    "  n <- sum({name}_weight[keep])",
    "  p <- sum({name}_weight[keep] * {name}_y[keep]) / n",
    "  z <- {name}_z",
    "  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)",
    "  half <- z / (1 + z^2 / n) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))",
    "  data.frame(level = level, n = n, proportion = p, lower = max(0, centre - half), upper = min(1, centre + half))",
    "}))",
    "{name}_summary$level <- factor({name}_summary$level, levels = levels({name}_group))",
    "{name}_summary$label <- paste0(round(100 * {name}_summary$proportion), \"%\")",
    "# Group labels (wrapped at {ww} characters) with the number of cases",
    "{name}_wrap <- {wrap}",
    "{name}_axis_labels <- stats::setNames(paste0({name}_wrap({name}_summary$level), \"\\n(n = \",",
    "  prettyNum(round({name}_summary$n, 2), big.mark = \",\"), \")\"), {name}_summary$level)"),
    collapse = "\n")), v))
  layers <- c(
    .fill("ggplot2::ggplot({name}_summary, ggplot2::aes(x = level, y = proportion))", name = name),
    .fill("  ggplot2::geom_col(fill = figure_fills[1], colour = \"black\", alpha = 0.8, width = {width})",
      width = if (is.null(group)) "0.4" else "0.6"),
    .fill("  # {pct}% Wilson score confidence intervals", pct = pct),
    "  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.12, linewidth = 0.6)",
    "  # percentage above each interval",
    "  ggplot2::geom_text(ggplot2::aes(y = upper, label = label), vjust = -0.7, size = 3.5, family = figure_font)",
    .fill("  ggplot2::scale_x_discrete(labels = {name}_axis_labels)", name = name),
    paste0("  ggplot2::scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2),\n",
      "    labels = function(p) paste0(round(100 * p), \"%\"), expand = ggplot2::expansion(mult = c(0, 0)))"),
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, title = {title}, caption = {caption})",
      xlab = if (is.null(xlab)) "NULL" else .lit(xlab),
      ylab = .lit(ylab %||% paste0("% with ", x, " = 1")),
      title = if (is.null(title)) "NULL" else .lit(title), caption = .lit(caption)),
    "  theme_figure()",
    "  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())"
  )
  code <- .code(prep, "", "# 3. The figure", .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, FALSE)
}
