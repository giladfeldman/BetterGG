#' Raincloud of a factorial design with model-based means
#'
#' For designs a single [ggbetweenrain()] cannot show: two or three factors
#' and/or covariates. The first factor is on the x axis, the second is shown
#' by colour (side by side within each x level), and the third by panels.
#' Each cell shows the raw observations ("rain") and their distribution
#' ("cloud"). Black-outlined points with error bars are the estimated marginal
#' means of the full factorial linear model and their confidence intervals;
#' lines join the means of each colour group across the x levels.
#'
#' The model is `y ~ x * colour * facet + covariates` (sum-to-zero contrasts,
#' complete cases), and the means come from 'emmeans' with covariates held
#' at their mean and equal weights for the cells. Pass `means` to draw means
#' from another model instead (for example one with case weights or fewer
#' terms); the plot then draws them as given.
#'
#' @param data A data frame.
#' @param x,y Column names (bare or quoted): the first factor and the outcome.
#' @param colour,facet Optional second and third factor.
#' @param covariates Optional character vector of numeric covariates.
#' @param means Optional data frame of means with the factor columns plus
#'   `emmean`, `lower.CL` and `upper.CL`.
#' @param conf.level Confidence level of the intervals.
#' @param stats Show the model's type III F tests (each term dropped from
#'   the sum-to-zero model, [stats::drop1()]) as the subtitle (default `FALSE`).
#' @param xlab,ylab,title,legend.title Labels.
#' @return A `ggplot` object.
#' @examples
#' tooth <- transform(ToothGrowth, dose = factor(dose))
#' ggfactorialrain(tooth, dose, len, colour = supp)
#' @export
ggfactorialrain <- function(data, x, y, colour = NULL, facet = NULL, covariates = NULL,
                            means = NULL, conf.level = 0.95, stats = FALSE, xlab = NULL,
                            ylab = NULL, title = NULL, legend.title = NULL) {
  x <- .column_name(rlang::enquo(x))
  y <- .column_name(rlang::enquo(y))
  colour <- .column_name(rlang::enquo(colour), optional = TRUE)
  facet <- .column_name(rlang::enquo(facet), optional = TRUE)
  .check_conf(conf.level)
  factors <- c(x, colour, facet)
  used <- c(y, factors, covariates)
  missing <- setdiff(used, names(data))
  if (length(missing)) stop("BetterGG: column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(used)) stop("BetterGG: a column is used twice.", call. = FALSE)
  if (!is.numeric(data[[y]])) stop("BetterGG: the outcome must be numeric.", call. = FALSE)
  for (cv in covariates) if (!is.numeric(data[[cv]])) stop("BetterGG: covariate '", cv, "' must be numeric.", call. = FALSE)

  d <- as.data.frame(data)[stats::complete.cases(as.data.frame(data)[used]), used, drop = FALSE]
  for (f in factors) {
    d[[f]] <- droplevels(as.factor(d[[f]]))
    if (nlevels(d[[f]]) < 2L) stop("BetterGG: factor '", f, "' needs at least two observed levels.", call. = FALSE)
  }
  model <- NULL
  if (is.null(means)) {
    model_data <- d
    for (f in factors) stats::contrasts(model_data[[f]]) <- stats::contr.sum(nlevels(model_data[[f]]))
    rhs <- paste(c(paste(.sym(factors), collapse = " * "), .sym(covariates)), collapse = " + ")
    model <- stats::lm(stats::as.formula(paste(.sym(y), "~", rhs)), data = model_data)
    means <- as.data.frame(stats::confint(
      emmeans::emmeans(model, specs = factors, weights = "equal"),
      level = conf.level
    ))
  }
  means <- as.data.frame(means)
  need <- c(factors, "emmean", "lower.CL", "upper.CL")
  if (!all(need %in% names(means))) {
    stop("BetterGG: 'means' needs the columns ", paste(need, collapse = ", "), ".", call. = FALSE)
  }
  for (f in factors) means[[f]] <- factor(as.character(means[[f]]), levels = levels(d[[f]]))
  if (anyNA(means[factors])) stop("BetterGG: 'means' has levels not in the data.", call. = FALSE)

  group <- colour %||% x
  k <- if (is.null(colour)) 1L else nlevels(d[[colour]])
  slot <- 0.8 / k
  place <- function(df) {
    offset <- if (is.null(colour)) 0 else (as.integer(df[[colour]]) - (k + 1) / 2) * slot
    as.integer(df[[x]]) + offset
  }
  d$.bgg_x <- place(d)
  d$.bgg_rain_x <- d$.bgg_x - 0.2 * slot
  d$.bgg_cell <- interaction(d[factors], drop = TRUE, lex.order = TRUE)
  means$.bgg_x <- place(means)

  colours <- bgg_palette(getOption("BetterGG.palette", "nejm") %||% "nejm", nlevels(d[[group]]), "colour")
  fills <- bgg_palette(getOption("BetterGG.palette", "nejm") %||% "nejm", nlevels(d[[group]]), "fill")
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.bgg_x, y = .data[[y]])) +
    ggdist::stat_slab(
      ggplot2::aes(fill = .data[[group]], group = .data$.bgg_cell),
      orientation = "vertical", side = "right", justification = 0, width = slot, scale = 0.45,
      colour = NA, alpha = 0.5, normalize = "groups"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$.bgg_rain_x, colour = .data[[group]]),
      position = ggplot2::position_jitter(width = 0.06 * slot, height = 0, seed = 1123),
      size = 1.4, alpha = 0.5, stroke = 0
    )
  if (!is.null(colour)) {
    # lines join the means of each colour group across the x levels
    plot <- plot + ggplot2::geom_line(
      data = means,
      ggplot2::aes(x = .data$.bgg_x, y = .data$emmean, colour = .data[[group]], group = .data[[group]]),
      inherit.aes = FALSE, linewidth = 0.6
    )
  }
  plot <- plot +
    ggplot2::geom_errorbar(
      data = means,
      ggplot2::aes(x = .data$.bgg_x, ymin = .data$lower.CL, ymax = .data$upper.CL),
      inherit.aes = FALSE, width = 0.12 * slot, linewidth = 0.6
    ) +
    ggplot2::geom_point(
      data = means,
      ggplot2::aes(x = .data$.bgg_x, y = .data$emmean, fill = .data[[group]]),
      inherit.aes = FALSE, shape = 21, size = 3, colour = "black", stroke = 0.6
    ) +
    ggplot2::scale_x_continuous(breaks = seq_len(nlevels(d[[x]])), labels = levels(d[[x]])) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_fill_manual(values = fills) +
    ggplot2::labs(
      x = xlab %||% x, y = ylab %||% y, title = title,
      colour = legend.title %||% group, fill = legend.title %||% group,
      subtitle = if (isTRUE(stats)) .factorial_subtitle(model) else NULL
    ) +
    theme_apa() +
    ggplot2::theme(
      legend.position = if (is.null(colour)) "none" else "bottom",
      panel.grid.major.x = ggplot2::element_blank()
    )
  if (!is.null(facet)) plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data[[facet]]), labeller = ggplot2::label_wrap_gen(width = 30))
  if (!identical(bgg_font(), "")) plot <- .set_family(plot, bgg_font())
  plot
}

# F tests of every model term, one line each.
.factorial_subtitle <- function(model) {
  if (is.null(model)) {
    stop("BetterGG: stats = TRUE needs the model; it is not available when 'means' is given.", call. = FALSE)
  }
  .format_f_tests(.type3_tests(model))
}

# Type III F tests: each term dropped from the full model in turn. With
# sum-to-zero contrasts these are the tests jamovi's ANOVA reports. (Not
# emmeans::joint_tests(): it stores F rounded to three decimals.)
.type3_tests <- function(model) {
  dropped <- stats::drop1(model, scope = ~., test = "F")
  data.frame(
    `model term` = rownames(dropped)[-1], df1 = dropped$Df[-1], df2 = stats::df.residual(model),
    F.ratio = dropped$`F value`[-1], p.value = dropped$`Pr(>F)`[-1], check.names = FALSE
  )
}

.format_f_tests <- function(tests) {
  term <- gsub(":", " \u00d7 ", tests[["model term"]], fixed = TRUE)
  eta <- (tests$F.ratio * tests$df1) / (tests$F.ratio * tests$df1 + tests$df2)
  p <- vapply(apa_p_value(tests$p.value), function(f) {
    paste(if (f$op == "==") "=" else f$op, f$value)
  }, character(1))
  df2 <- ifelse(abs(tests$df2 - round(tests$df2)) < 1e-8, format(round(tests$df2)), formatC(tests$df2, format = "f", digits = 2))
  paste0(term, ": F(", tests$df1, ", ", df2, ") = ", formatC(tests$F.ratio, format = "f", digits = 2),
    ", p ", p, ", \u03b7p\u00b2 = ", sub("^0[.]", ".", formatC(eta, format = "f", digits = 2)),
    collapse = "\n")
}

# A column given bare (len) or quoted ("len").
.column_name <- function(quo, optional = FALSE) {
  if (rlang::quo_is_null(quo)) {
    if (optional) return(NULL)
    stop("BetterGG: a required column is missing.", call. = FALSE)
  }
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr) && length(expr) == 1L) return(expr)
  if (is.symbol(expr)) return(as.character(expr))
  value <- rlang::eval_tidy(quo)
  if (is.null(value) && optional) return(NULL)
  if (is.character(value) && length(value) == 1L) return(value)
  stop("BetterGG: give a column name.", call. = FALSE)
}
