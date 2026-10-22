#' Item responses and item-rest correlations of a scale
#'
#' A figure for a reliability analysis: on the left, the distribution of the
#' responses to each item (100% stacked bars when the items have at most 10
#' distinct values, as Likert items do; otherwise a density with the median
#' and quartiles); on the right, each item's correlation with the sum of the
#' other items (the item-rest correlation).
#'
#' Rows with a missing item are dropped (listwise), and reverse-scored items
#' are recoded as (minimum + maximum) - value using the observed minimum and
#' maximum of that item, as jamovi does. Cronbach's alpha is
#' `k / (k - 1) * (1 - sum(item variances) / variance(total))`.
#'
#' @param data A data frame.
#' @param items Character vector of the item columns (at least two).
#' @param reverse Optional character vector of items to reverse-score.
#' @param stats Show Cronbach's alpha as the subtitle (default `FALSE`).
#' @param title Plot title.
#' @return A 'patchwork' object.
#' @examples
#' survey <- data.frame(
#'   a = c(1, 2, 3, 4, 5, 4), b = c(2, 2, 3, 5, 5, 4), c = c(5, 3, 3, 2, 1, 2)
#' )
#' ggreliability(survey, c("a", "b", "c"), reverse = "c")
#' @export
ggreliability <- function(data, items, reverse = NULL, stats = FALSE, title = NULL) {
  if (!is.character(items) || length(items) < 2L || anyDuplicated(items)) {
    stop("BetterGG: give at least two different item columns.", call. = FALSE)
  }
  missing <- setdiff(c(items, reverse), names(data))
  if (length(missing)) stop("BetterGG: column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(setdiff(reverse, items))) stop("BetterGG: reverse-scored items must be among the items.", call. = FALSE)
  d <- as.data.frame(data)[items]
  for (item in items) {
    if (!is.numeric(d[[item]])) stop("BetterGG: item '", item, "' must be numeric.", call. = FALSE)
  }
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 2L) stop("BetterGG: fewer than two complete rows.", call. = FALSE)
  for (item in reverse) d[[item]] <- min(d[[item]]) + max(d[[item]]) - d[[item]]
  labels <- ifelse(items %in% reverse, paste0(items, " (R)"), items)

  rest <- vapply(items, function(item) {
    stats::cor(d[[item]], rowSums(d[setdiff(items, item)]))
  }, numeric(1))
  k <- length(items)
  alpha <- k / (k - 1) * (1 - sum(vapply(d, stats::var, numeric(1))) / stats::var(rowSums(d)))
  correlations <- data.frame(item = factor(labels, levels = rev(labels)), r = unname(rest))
  correlations$label <- sub("^(-?)0[.]", "\\1.", formatC(correlations$r, format = "f", digits = 2))

  values <- sort(unique(unlist(d, use.names = FALSE)))
  long <- data.frame(
    item = factor(rep(labels, each = nrow(d)), levels = rev(labels)),
    value = unlist(d, use.names = FALSE)
  )
  if (length(values) <= 10L) {
    long$response <- factor(long$value, levels = values)
    responses <- ggplot2::ggplot(long, ggplot2::aes(y = .data$item, fill = .data$response)) +
      ggplot2::geom_bar(position = ggplot2::position_fill(reverse = TRUE), colour = "white", width = 0.7) +
      ggplot2::scale_x_continuous(labels = function(p) paste0(round(100 * p), "%"), expand = c(0, 0)) +
      ggplot2::scale_fill_manual(values = grDevices::hcl.colors(length(values), "viridis")) +
      ggplot2::labs(x = "responses", y = NULL, fill = "response") +
      theme_apa() +
      ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::margin(5.5, 14, 5.5, 5.5))
  } else {
    responses <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$item)) +
      ggdist::stat_halfeye(fill = "grey70", point_colour = "black", interval_colour = "black",
        .width = 0.5, point_interval = "median_qi") +
      ggplot2::labs(x = "response", y = NULL) +
      theme_apa()
  }
  rest_plot <- ggplot2::ggplot(correlations, ggplot2::aes(x = .data$r, y = .data$item)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$r, yend = .data$item), colour = "grey40") +
    ggplot2::geom_point(size = 3, colour = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), nudge_y = 0.3, size = 3) +
    ggplot2::scale_x_continuous(limits = c(min(0, correlations$r), 1), breaks = c(0, 0.5, 1)) +
    ggplot2::labs(x = "item-rest correlation", y = NULL) +
    theme_apa() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
  subtitle <- if (isTRUE(stats)) {
    paste0("Cronbach's \u03b1 = ", sub("^(-?)0[.]", "\\1.", formatC(alpha, format = "f", digits = 2)),
      " (k = ", k, " items, n = ", nrow(d), ")")
  }
  out <- patchwork::wrap_plots(responses, rest_plot, widths = c(2, 1)) +
    patchwork::plot_annotation(title = title, subtitle = subtitle, theme = theme_apa())
  if (!identical(bgg_font(), "")) {
    out <- .set_family(out, bgg_font())
  }
  out
}

#' Recipe: reliability figure ([ggreliability()])
#'
#' Returns 'ggplot2' + 'patchwork' code that draws the [ggreliability()]
#' figure.
#'
#' @inheritParams recipe_betweenrain
#' @param items Names of the item columns.
#' @param reverse Optional names of reverse-scored items.
#' @param stats Show Cronbach's alpha as the subtitle.
#' @return R code as one character string.
#' @examples
#' cat(recipe_reliability("survey", c("q1", "q2", "q3")))
#' @export
recipe_reliability <- function(data, items, reverse = NULL, name = "plot", stats = FALSE,
                               title = NULL, setup = TRUE) {
  .check_names(items = items, reverse = reverse)
  .check_name1(data = data)
  .check_prefix(name)
  if (length(items) < 2L || anyDuplicated(items)) stop("BetterGG recipe: give at least two different items.", call. = FALSE)
  if (length(setdiff(reverse, items))) stop("BetterGG recipe: reverse-scored items must be among the items.", call. = FALSE)
  code <- .fill(paste(
    "# 1. Data: the items, complete rows only (listwise), as numbers.",
    "{name}_items <- {items}",
    "{name}_data <- {data}[, {name}_items, drop = FALSE]",
    "{name}_data <- {name}_data[stats::complete.cases({name}_data), , drop = FALSE]",
    "if (!all(vapply({name}_data, is.numeric, logical(1)))) stop(\"Reliability items must be numeric.\")",
    "{name}_reverse <- {reverse}",
    "# Reverse-scored items: (minimum + maximum) - value, as in jamovi.",
    "for (item in {name}_reverse) {",
    "  {name}_data[[item]] <- min({name}_data[[item]]) + max({name}_data[[item]]) - {name}_data[[item]]",
    "}",
    "{name}_labels <- ifelse({name}_items %in% {name}_reverse, paste0({name}_items, \" (R)\"), {name}_items)",
    "",
    "# 2. Item-rest correlations and Cronbach's alpha.",
    "{name}_rest <- vapply({name}_items, function(item) {",
    "  stats::cor({name}_data[[item]], rowSums({name}_data[setdiff({name}_items, item)]))",
    "}, numeric(1))",
    "{name}_k <- length({name}_items)",
    "{name}_alpha <- {name}_k / ({name}_k - 1) *",
    "  (1 - sum(vapply({name}_data, stats::var, numeric(1))) / stats::var(rowSums({name}_data)))",
    "{name}_correlations <- data.frame(item = factor({name}_labels, levels = rev({name}_labels)), r = unname({name}_rest))",
    "{name}_correlations$label <- sub(\"^(-?)0[.]\", \"\\\\1.\", formatC({name}_correlations$r, format = \"f\", digits = 2))",
    "",
    "# 3. Responses in long format (one row per person and item).",
    "{name}_values <- sort(unique(unlist({name}_data, use.names = FALSE)))",
    "{name}_long <- data.frame(",
    "  item = factor(rep({name}_labels, each = nrow({name}_data)), levels = rev({name}_labels)),",
    "  value = unlist({name}_data, use.names = FALSE)",
    ")",
    "",
    "# 4. The figure: responses (left) and item-rest correlations (right).",
    "if (length({name}_values) <= 10L) {",
    "  # Few distinct values (e.g. Likert items): 100% stacked bars per item.",
    "  {name}_long$response <- factor({name}_long$value, levels = {name}_values)",
    "  {name}_responses <- ggplot2::ggplot({name}_long, ggplot2::aes(y = item, fill = response)) +",
    "    ggplot2::geom_bar(position = ggplot2::position_fill(reverse = TRUE), colour = \"white\", width = 0.7) +",
    "    ggplot2::scale_x_continuous(labels = function(p) paste0(round(100 * p), \"%\"), expand = c(0, 0)) +",
    "    ggplot2::scale_fill_manual(values = grDevices::hcl.colors(length({name}_values), \"viridis\")) +",
    "    ggplot2::labs(x = \"responses\", y = NULL, fill = \"response\") +",
    "    theme_figure() +",
    "    ggplot2::theme(legend.position = \"bottom\", plot.margin = ggplot2::margin(5.5, 14, 5.5, 5.5))",
    "} else {",
    "  # Many distinct values: density with median and interquartile range per item.",
    "  {name}_responses <- ggplot2::ggplot({name}_long, ggplot2::aes(x = value, y = item)) +",
    "    ggdist::stat_halfeye(fill = \"grey70\", point_colour = \"black\", interval_colour = \"black\",",
    "      .width = 0.5, point_interval = \"median_qi\") +",
    "    ggplot2::labs(x = \"response\", y = NULL) +",
    "    theme_figure()",
    "}",
    "{name}_rest_plot <- ggplot2::ggplot({name}_correlations, ggplot2::aes(x = r, y = item)) +",
    "  ggplot2::geom_vline(xintercept = 0, colour = \"grey60\", linetype = \"dashed\") +",
    "  ggplot2::geom_segment(ggplot2::aes(x = 0, xend = r, yend = item), colour = \"grey40\") +",
    "  ggplot2::geom_point(size = 3, colour = \"black\") +",
    "  ggplot2::geom_text(ggplot2::aes(label = label), nudge_y = 0.3, size = 3, family = figure_font) +",
    "  ggplot2::scale_x_continuous(limits = c(min(0, {name}_correlations$r), 1), breaks = c(0, 0.5, 1)) +",
    "  ggplot2::labs(x = \"item-rest correlation\", y = NULL) +",
    "  theme_figure() +",
    "  ggplot2::theme(axis.text.y = ggplot2::element_blank())",
    "{name}_plot <- patchwork::wrap_plots({name}_responses, {name}_rest_plot, widths = c(2, 1)) +",
    "  patchwork::plot_annotation(title = {title}, subtitle = {subtitle}, theme = theme_figure())",
    sep = "\n"), name = name, data = data, items = .vec(items), reverse = .vec(reverse),
    title = if (is.null(title)) "NULL" else .lit(title),
    subtitle = if (isTRUE(stats)) .fill(paste0("paste0(\"Cronbach's \\u03b1 = \",\n",
      "    sub(\"^(-?)0[.]\", \"\\\\1.\", formatC({name}_alpha, format = \"f\", digits = 2)),\n",
      "    \" (k = \", {name}_k, \" items, n = \", nrow({name}_data), \")\")"), name = name) else "NULL")
  .with_setup(code, setup, FALSE)
}
