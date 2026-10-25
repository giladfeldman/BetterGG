#' Bars of one categorical variable
#'
#' The percentage of each category of `x` as bars labelled with their
#' percentages: an alternative to the pie of [ggpiestats()] for ordinal
#' variables and for variables with many categories.
#'
#' With `orientation = "auto"` (the default), an unordered factor with more
#' than 8 categories, or with a category name longer than `wrap_width`
#' characters, is drawn as horizontal bars sorted by count (the largest on
#' top); everything else as vertical bars in level order (scale order for an
#' ordered factor). Ordered factors are coloured in scale order as in
#' [ggbarstats()]; the bars of an unordered factor share one colour (the axis
#' names them). Category labels wrap at `wrap_width` characters (vertical
#' bars: about 90 characters shared by the bars, at least 10 each).
#'
#' The figure is drawn by running the code [recipe_freqbars()] writes, so the
#' function and the recipe always draw the same figure.
#'
#' @param data A data frame.
#' @param x The categorical column (bare or quoted).
#' @param counts Optional column of frequency counts (one row per category
#'   instead of one row per case).
#' @param weights Optional column of case weights: counts and percentages are
#'   weighted.
#' @param orientation `"auto"`, `"vertical"` or `"horizontal"`.
#' @param sort `"auto"` (by count for the horizontal bars of an unordered
#'   factor, otherwise level order), `"count"` (largest first) or
#'   `"levels"`.
#' @param xlab,ylab,title Category axis title (default: the column name),
#'   percentage axis title (the number of cases is added) and plot title.
#' @param wrap_width Wrap category labels at this many characters.
#' @return A `ggplot` object.
#' @examples
#' ggfreqbars(mtcars, cyl)
#' @export
ggfreqbars <- function(data, x, counts = NULL, weights = NULL,
                       orientation = c("auto", "vertical", "horizontal"),
                       sort = c("auto", "count", "levels"), xlab = NULL, ylab = "Percentage",
                       title = NULL, wrap_width = 40) {
  x <- .column_name(rlang::enquo(x))
  counts <- .column_name(rlang::enquo(counts), optional = TRUE)
  weights <- .column_name(rlang::enquo(weights), optional = TRUE)
  .check_columns(data, c(x, counts, weights))
  .run_recipe_code(recipe_freqbars("source_data", x, name = "bgg", counts = counts, weights = weights,
    orientation = match.arg(orientation), sort = match.arg(sort), xlab = xlab %||% x, ylab = ylab,
    title = title, wrap_width = wrap_width, setup = TRUE), data)
}

#' Recipe: bars of one categorical variable ([ggfreqbars()])
#'
#' Returns 'ggplot2' code that draws the [ggfreqbars()] figure: the
#' percentage of each category as a labelled bar; horizontal bars sorted by
#' count for an unordered variable with more than 8 categories, otherwise
#' vertical bars in level (scale) order.
#'
#' @inheritParams recipe_barstats
#' @inheritParams ggfreqbars
#' @param x Name of the categorical column.
#' @param counts Optional name of a column of frequency counts.
#' @param weights Optional name of a column of case weights.
#' @param xlab Category axis title.
#' @param ylab Percentage axis title (the number of cases is added).
#' @param title Plot title.
#' @return R code as one character string.
#' @examples
#' cat(recipe_freqbars("mtcars", "cyl"))
#' @export
recipe_freqbars <- function(data, x, name = "plot", counts = NULL, weights = NULL,
                            orientation = c("auto", "vertical", "horizontal"),
                            sort = c("auto", "count", "levels"), xlab = x, ylab = "Percentage",
                            title = NULL, wrap_width = 40, setup = TRUE) {
  .check_name1(data = data, x = x, counts = counts, weights = weights)
  .check_prefix(name)
  .check_label_args(0, wrap_width)
  orientation <- match.arg(orientation)
  sort <- match.arg(sort)
  cols <- c(x, counts, weights)
  v <- list(name = name, data = data, cols = .vec(cols), xs = .lit(x), X = .sym(x), ww = .num(wrap_width))
  weight <- paste(c("rep(1, nrow({name}_data))",
    if (!is.null(weights)) paste0("{name}_data[[", .lit(weights), "]]"),
    if (!is.null(counts)) paste0("{name}_data[[", .lit(counts), "]]")), collapse = " * ")
  horizontal <- switch(orientation, auto = "!{name}_ordered && (nlevels({name}_data[[{xs}]]) > 8L || {name}_long)",
    vertical = "FALSE", horizontal = "TRUE")
  by_count <- switch(sort, auto = "{name}_horizontal && !{name}_ordered", count = "TRUE", levels = "FALSE")
  prep <- do.call(.fill, c(list(paste(c(
    "# 1. Data: complete cases; the categories become a factor.",
    "{name}_data <- {data}[stats::complete.cases({data}[, {cols}, drop = FALSE]), {cols}, drop = FALSE]",
    "{name}_data[[{xs}]] <- as.factor({name}_data[[{xs}]])",
    paste0("{name}_weight <- ", weight),
    "",
    paste0("# 2. Count", if (is.null(weights) && is.null(counts)) "" else " (weighted)",
      " and percentage of each category."),
    "{name}_cells <- as.data.frame(stats::xtabs({name}_weight ~ {name}_data[[{xs}]]), responseName = \"counts\")",
    "names({name}_cells)[1] <- {xs}",
    "{name}_cells$perc <- 100 * {name}_cells$counts / sum({name}_cells$counts)",
    "{name}_cells$label <- paste0(round({name}_cells$perc), \"%\")",
    "{name}_n <- prettyNum(round(sum({name}_cells$counts), 2), big.mark = \",\")",
    "{name}_ordered <- is.ordered({name}_data[[{xs}]])",
    "{name}_long <- any(nchar(levels({name}_data[[{xs}]])) > {ww})",
    "# An ordered factor is coloured in scale order (diverging with a neutral middle for an odd",
    "# number of levels, sequential otherwise); the bars of an unordered factor share one colour.",
    "{name}_ordered_fills <- {ordered}",
    "{name}_fills <- if ({name}_ordered) {name}_ordered_fills(nrow({name}_cells)) else rep(figure_fills[1], nrow({name}_cells))",
    "names({name}_fills) <- levels({name}_cells[[{xs}]])",
    paste0("# Horizontal bars", if (orientation == "auto") " for an unordered variable with more than 8 categories" else "",
      if (orientation == "auto") " or a name longer than {ww} characters" else "",
      if (sort == "auto") ", sorted by count (largest on top)." else "."),
    paste0("{name}_horizontal <- ", horizontal),
    paste0("{name}_by_count <- ", by_count),
    "if ({name}_by_count) {",
    "  {name}_cells[[{xs}]] <- factor({name}_cells[[{xs}]],",
    "    levels = {name}_cells[[{xs}]][order({name}_cells$counts, decreasing = !{name}_horizontal)])",
    "}",
    "# Category names wrap at {ww} characters; vertical bars share about 90 characters (no legend; at least 10 each).",
    "{name}_wrap <- {wrap}",
    "{name}_axis_width <- if ({name}_horizontal) {ww} else max(10, min({ww}, floor(90 / nrow({name}_cells))))"),
    collapse = "\n")), c(v, list(ordered = .ordered_fills_code(getOption("BetterGG.palette", "nejm") %||% "nejm"),
    wrap = .wrap_code(wrap_width)))))
  layers <- c(
    .fill("ggplot2::ggplot({name}_cells, ggplot2::aes(x = {X}, y = perc, fill = {X}))", name = name, X = v$X),
    "  ggplot2::geom_col(colour = \"black\", width = 0.7)",
    "  # percentage labels at the end of each bar",
    .fill(paste0("  ggplot2::geom_text(ggplot2::aes(label = label), size = 3.5, family = figure_font,\n",
      "    vjust = if ({name}_horizontal) 0.5 else -0.5, hjust = if ({name}_horizontal) -0.15 else 0.5)"), name = name),
    .fill("  ggplot2::scale_x_discrete(labels = function(labels) {name}_wrap(labels, {name}_axis_width))", name = name),
    "  ggplot2::scale_y_continuous(labels = function(p) paste0(p, \"%\"), expand = ggplot2::expansion(mult = c(0, 0.12)))",
    .fill("  ggplot2::scale_fill_manual(values = {name}_fills, guide = \"none\")", name = name),
    .fill("  ggplot2::labs(x = {xlab}, y = paste0({ylab}, \" (n = \", {name}_n, \")\"), title = {title})",
      name = name, xlab = .lit(xlab), ylab = .lit(ylab), title = if (is.null(title)) "NULL" else .lit(title)),
    "  theme_figure()",
    "  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())",
    .fill("  (if ({name}_horizontal) ggplot2::coord_flip())", name = name)
  )
  code <- .code(prep, "", "# 3. The figure", .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, FALSE)
}

# Columns used by a figure function must exist.
.check_columns <- function(data, columns) {
  if (!is.data.frame(data)) stop("BetterGG: 'data' must be a data frame.", call. = FALSE)
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("BetterGG: column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Draw a figure by running recipe code (written for a data frame named
# `source_data` and the object prefix "bgg") in a private environment.
.run_recipe_code <- function(code, data) {
  env <- new.env(parent = globalenv()) # as when the recipe code runs in a session
  assign("source_data", as.data.frame(data), envir = env)
  eval(parse(text = code, keep.source = FALSE), envir = env)
  env$bgg_plot
}
