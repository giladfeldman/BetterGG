#' Recipe: histogram ([gghistostats()])
#'
#' Returns 'ggplot2' code that draws the [gghistostats()] figure: a histogram
#' with counts (left axis) and proportions (right axis), and a dashed line at
#' the mean (or median / trimmed mean), labelled on the top axis.
#'
#' @inheritParams recipe_betweenrain
#' @param x Name of the numeric column.
#' @param test.value Value tested against when `stats = TRUE`.
#' @param alternative Direction of the one-sample test (`"two.sided"`,
#'   `"greater"`, `"less"`).
#' @param binwidth Optional bin width; by default the code uses the
#'   'ggstatsplot' rule (range / sqrt(n)).
#' @param weights Optional name of a column of case weights (counts and the
#'   centre are then weighted; BetterGG itself has no weights).
#' @param xlab,title Axis title and plot title.
#' @param facet Optional name of a column splitting the histogram into
#'   panels, each with its own centre line.
#' @return R code as one character string.
#' @examples
#' cat(recipe_histostats("mtcars", "mpg"))
#' @export
recipe_histostats <- function(data, x, name = "plot", type = "parametric", stats = FALSE,
                              test.value = 0, alternative = "two.sided", conf.level = 0.95,
                              bf.message = FALSE, tr = 0.2,
                              binwidth = NULL, weights = NULL, facet = NULL, xlab = x,
                              title = NULL, setup = TRUE) {
  .check_name1(data = data, x = x, weights = weights, facet = facet)
  .check_prefix(name)
  .check_conf(conf.level)
  type <- .stats_type(type)
  centre_type <- if (type == "bayes") "parametric" else type
  if (type == "bayes" && !isTRUE(stats)) type <- "parametric"
  if (!is.null(weights) && centre_type != "parametric") {
    stop("BetterGG recipe: weighted histograms support the mean only.", call. = FALSE)
  }
  if (!is.null(weights) && isTRUE(stats)) {
    stop("BetterGG recipe: statistics are not available for weighted histograms.", call. = FALSE)
  }
  centre <- .centrality_word(centre_type)
  keep <- c(x, weights, facet)
  centre_fun <- if (is.null(weights)) .centrality_code(centre_type, tr) else
    "stats::weighted.mean(values, weights)"
  v <- list(name = name, data = data, xs = .lit(x), X = .sym(x), keep = .vec(keep),
    centre = centre, centre_fun = centre_fun,
    ws = if (is.null(weights)) "" else .lit(weights))
  prep <- do.call(.fill, c(list(paste(
    "# 1. Data: rows with a value of {xs}.",
    "{name}_data <- {data}[stats::complete.cases({data}[, {keep}, drop = FALSE]), {keep}, drop = FALSE]",
    sep = "\n")), v))
  if (is.null(facet)) {
    prep <- .code(prep, do.call(.fill, c(list(paste(
      "# The {centre} drawn as a dashed line and labelled on the top axis.",
      "values <- {name}_data[[{xs}]]",
      if (!is.null(weights)) "weights <- {name}_data[[{ws}]]",
      "{name}_centre <- {centre_fun}",
      "{name}_centre_label <- paste0(\"list(widehat(mu)[{centre}]=='\", formatC({name}_centre, format = \"f\", digits = 2), \"')\")",
      sep = "\n")), v)))
  } else {
    prep <- .code(prep, .fill(paste(
      "{name}_data[[{fs}]] <- droplevels(as.factor({name}_data[[{fs}]]))",
      "# The {centre} of each panel, drawn as a dashed line and labelled.",
      "{name}_centres <- do.call(rbind, lapply(split({name}_data, {name}_data[[{fs}]]), function(part) {",
      "  values <- part[[{xs}]]",
      if (!is.null(weights)) "  weights <- part[[{ws}]]",
      "  data.frame(level = part[[{fs}]][1], value = {centre_fun})",
      "}))",
      "names({name}_centres)[1] <- {fs}",
      "{name}_centres$label <- paste0(\"list(widehat(mu)[{centre}]=='\", formatC({name}_centres$value, format = \"f\", digits = 2), \"')\")",
      "# each label beside its line, on the side with more room, so it stays inside the panel",
      "{name}_centres$hjust <- ifelse({name}_centres$value < mean(range({name}_data[[{xs}]])), -0.05, 1.05)",
      sep = "\n"), name = name, fs = .lit(facet), xs = v$xs, ws = v$ws, centre = centre, centre_fun = centre_fun))
  }
  bw <- if (is.null(binwidth)) {
    .fill("{name}_binwidth <- diff(range({name}_data[[{xs}]])) / sqrt(nrow({name}_data))", name = name, xs = v$xs)
  } else {
    paste0(name, "_binwidth <- ", .num(binwidth))
  }
  total <- if (is.null(weights)) .fill("nrow({name}_data)", name = name) else .fill("sum({name}_data[[{ws}]])", name = name, ws = v$ws)
  prep <- .code(prep, "# Bin width: the ggstatsplot rule, range / square root of n.", bw,
    .fill("{name}_total <- {total}", name = name, total = total))
  stats_code <- if (isTRUE(stats)) .fill(paste(
    "# Statistics (statsExpressions): one-sample test against {tv}; p-values in APA style.",
    "{name}_test <- statsExpressions::one_sample_test({name}_data, x = {X}, test.value = {tv}, type = {type},",
    "  alternative = {alt}, conf.level = {conf}, tr = {tr}, digits = 2L)",
    "{name}_subtitle <- figure_apa_p({name}_test$expression[[1]], {name}_test$p.value[[1]])",
    if (isTRUE(bf.message) && type == "parametric") paste(
      "{name}_caption <- statsExpressions::one_sample_test({name}_data, x = {X}, test.value = {tv},",
      "  type = \"bayes\", conf.level = {conf})$expression[[1]]", sep = "\n") else "{name}_caption <- NULL",
    sep = "\n"), name = name, X = v$X, tv = .num(test.value), type = .lit(type), conf = .num(conf.level),
    tr = .num(tr), alt = .lit(match.arg(alternative, c("two.sided", "greater", "less")))) else NULL
  weight_aes <- if (is.null(weights)) "" else paste0(", weight = ", .sym(weights))
  layers <- c(
    .fill("ggplot2::ggplot({name}_data, ggplot2::aes(x = {X}{w}))", name = name, X = v$X, w = weight_aes),
    "  # bars: counts per bin",
    .fill(paste0("  ggplot2::stat_bin(ggplot2::aes(y = ggplot2::after_stat(count), fill = ggplot2::after_stat(count)),\n",
      "    binwidth = {name}_binwidth, colour = \"black\", fill = \"grey50\", alpha = 0.7)"), name = name),
    "  # right axis: the same counts as proportions",
    if (!is.null(facet)) "  # headroom above the tallest bar for the labels; counts are whole numbers",
    .fill(paste0("  ggplot2::scale_y_continuous({breaks}sec.axis = ggplot2::sec_axis(transform = ~ . / {name}_total,\n",
      "    labels = function(p) paste0(round(100 * p), \"%\"), name = \"proportion\"){expand})"), name = name,
      expand = if (is.null(facet)) "" else ",\n    expand = ggplot2::expansion(mult = c(0.05, 0.3))",
      breaks = if (is.null(facet)) "" else "breaks = function(l) { b <- pretty(l); b[b == round(b)] },\n    "),
    "  ggplot2::guides(fill = \"none\")",
    .fill("  # the {centre}", centre = centre),
    if (is.null(facet)) c(
      .fill("  ggplot2::geom_vline(xintercept = {name}_centre, colour = \"blue\", linewidth = 1, linetype = \"dashed\")",
        name = name),
      .fill(paste0("  ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(transform = ~ ., name = NULL,\n",
        "    labels = parse(text = {name}_centre_label), breaks = {name}_centre))"), name = name)
    ) else c(
      .fill(paste0("  ggplot2::geom_vline(data = {name}_centres, ggplot2::aes(xintercept = value), colour = \"blue\",\n",
        "    linewidth = 1, linetype = \"dashed\")"), name = name),
      .fill(paste0("  ggplot2::geom_label(data = {name}_centres, ggplot2::aes(x = value, y = Inf, label = label, hjust = hjust),\n",
        "    inherit.aes = FALSE, parse = TRUE, vjust = 1.2, size = 3, family = figure_font)"), name = name),
      .fill("  ggplot2::facet_wrap(ggplot2::vars(.data[[{fs}]]), labeller = ggplot2::label_wrap_gen(width = 30))", fs = .lit(facet))
    ),
    .fill("  ggplot2::labs(x = {xlab}, y = \"count\", title = {title}, subtitle = {sub}, caption = {cap})",
      xlab = .lit(xlab), title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL",
      cap = if (isTRUE(stats)) paste0(name, "_caption") else "NULL"),
    "  theme_figure()"
  )
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 2. The figure",
    .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, stats)
}

#' Recipe: scatterplot ([ggscatterstats()])
#'
#' Returns 'ggplot2' code that draws the [ggscatterstats()] figure: the
#' observations, a linear fit with its confidence band, and histograms of
#' both variables on the margins (via 'ggside').
#'
#' @inheritParams recipe_betweenrain
#' @param x,y Names of the two numeric columns.
#' @param marginal Draw the marginal histograms.
#' @return R code as one character string.
#' @examples
#' cat(recipe_scatterstats("mtcars", "wt", "mpg"))
#' @export
recipe_scatterstats <- function(data, x, y, name = "plot", type = "parametric", stats = FALSE,
                                conf.level = 0.95, bf.message = FALSE, tr = 0.2, marginal = TRUE,
                                xlab = x, ylab = y, title = NULL, setup = TRUE) {
  .check_name1(data = data, x = x, y = y)
  .check_prefix(name)
  .check_conf(conf.level)
  type <- .stats_type(type)
  v <- list(name = name, data = data, xs = .lit(x), ys = .lit(y), X = .sym(x), Y = .sym(y),
    conf = .num(conf.level), type = .lit(type), tr = .num(tr))
  prep <- do.call(.fill, c(list(paste(
    "# 1. Data: rows with both {xs} and {ys}.",
    "{name}_data <- {data}[!is.na({data}[[{xs}]]) & !is.na({data}[[{ys}]]), , drop = FALSE]",
    sep = "\n")), v))
  stats_code <- if (isTRUE(stats)) do.call(.fill, c(list(paste(
    "# Statistics (statsExpressions): correlation test; p-values in APA style.",
    "{name}_test <- statsExpressions::corr_test({name}_data, x = {X}, y = {Y}, type = {type},",
    "  conf.level = {conf}, tr = {tr}, digits = 2L)",
    "{name}_subtitle <- figure_apa_p({name}_test$expression[[1]], {name}_test$p.value[[1]])",
    if (isTRUE(bf.message) && type == "\"parametric\"") paste(
      "{name}_caption <- statsExpressions::corr_test({name}_data, x = {X}, y = {Y}, type = \"bayes\",",
      "  conf.level = {conf})$expression[[1]]", sep = "\n") else "{name}_caption <- NULL",
    sep = "\n")), v)) else NULL
  layers <- c(
    .fill("ggplot2::ggplot({name}_data, ggplot2::aes(x = {X}, y = {Y}))", name = name, X = v$X, Y = v$Y),
    "  # observations",
    "  ggplot2::geom_point(position = ggplot2::position_jitter(width = 0, height = 0), size = 3, alpha = 0.4, stroke = 0)",
    .fill("  # linear fit with its {pct}% confidence band", pct = .num(100 * conf.level)),
    .fill(paste0("  ggplot2::geom_smooth(method = \"lm\", formula = y ~ x, level = {conf}, linewidth = 1.5,\n",
      "    colour = \"blue\", na.rm = TRUE)"), conf = v$conf),
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, title = {title}, subtitle = {sub}, caption = {cap})",
      xlab = .lit(xlab), ylab = .lit(ylab), title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL",
      cap = if (isTRUE(stats)) paste0(name, "_caption") else "NULL"),
    "  theme_figure()",
    if (isTRUE(marginal)) c(
      "  # marginal histograms",
      "  ggside::geom_xsidehistogram(ggplot2::aes(y = ggplot2::after_stat(count)), fill = \"#4285F4\", colour = \"black\", bins = 30, na.rm = TRUE)",
      "  ggside::geom_ysidehistogram(ggplot2::aes(x = ggplot2::after_stat(count)), fill = \"#EA4335\", colour = \"black\", bins = 30, na.rm = TRUE)",
      "  # no count tick labels on the narrow margins (they collide); the bars show the shape",
      "  ggside::scale_ysidex_continuous(breaks = NULL)",
      "  ggside::scale_xsidey_continuous(breaks = NULL)"
    )
  )
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 2. The figure",
    .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, stats)
}

#' Recipe: correlation matrix ([ggcorrmat()])
#'
#' Returns 'ggplot2' code that draws the [ggcorrmat()] figure: the lower
#' triangle of the correlation matrix as coloured tiles with the coefficient
#' in each cell, and a cross over each coefficient that is not significant
#' after the multiplicity correction. The code computes every correlation
#' with [stats::cor.test()] on the pairwise complete cases.
#'
#' @inheritParams recipe_betweenrain
#' @param vars Names of the numeric columns.
#' @param type `"parametric"` (Pearson) or `"nonparametric"` (Spearman).
#' @param sig.level Threshold for the crosses.
#' @param title Plot title.
#' @param method Overrides `type`: `"pearson"`, `"spearman"` or `"kendall"`.
#' @param controls Optional names of columns to partial out (partial
#'   correlations; not for Kendall).
#' @param alternative Direction of the tests (`"two.sided"`, `"greater"`,
#'   `"less"`).
#' @param exact For Spearman and Kendall: `FALSE` (normal approximation, as
#'   'ggcorrmat') or `NULL` (the default of [stats::cor.test()]: exact
#'   p-values in small samples without ties).
#' @return R code as one character string.
#' @examples
#' cat(recipe_corrmat("mtcars", c("mpg", "wt", "hp")))
#' @export
recipe_corrmat <- function(data, vars, name = "plot", type = "parametric", p.adjust.method = "holm",
                           sig.level = 0.05, conf.level = 0.95, method = NULL, controls = NULL,
                           alternative = "two.sided", exact = FALSE, title = NULL, setup = TRUE) {
  .check_names(vars = vars, controls = controls)
  .check_name1(data = data)
  .check_prefix(name)
  if (length(vars) < 2L || anyDuplicated(vars)) stop("BetterGG recipe: give at least two different columns.", call. = FALSE)
  if (length(intersect(vars, controls))) stop("BetterGG recipe: a control variable cannot also be correlated.", call. = FALSE)
  type <- .stats_type(type)
  method <- method %||% switch(type, parametric = "pearson", nonparametric = "spearman",
    stop("BetterGG recipe: corrmat supports parametric (Pearson) or nonparametric (Spearman).", call. = FALSE))
  method <- match.arg(method, c("pearson", "spearman", "kendall"))
  if (length(controls) && method == "kendall") {
    stop("BetterGG recipe: partial Kendall correlations are not available.", call. = FALSE)
  }
  p.adjust.method <- match.arg(p.adjust.method, stats::p.adjust.methods)
  adj_text <- switch(p.adjust.method, none = NULL, BH = , fdr = "FDR",
    paste0(toupper(substring(p.adjust.method, 1, 1)), substring(p.adjust.method, 2)))
  p_label <- if (is.null(adj_text)) "italic(p)[unadj.]" else paste0("italic(p)[", .lit(adj_text), " - adj.]")
  code <- .fill(paste(
    "# 1. Correlations of every pair of variables (pairwise complete cases),",
    "#    {method_word}{partial_note}, p-values corrected by the {adj} method.",
    "{name}_vars <- {vars}",
    "{name}_pairs <- utils::combn({name}_vars, 2L, simplify = FALSE)",
    "{name}_cells <- do.call(rbind, lapply({name}_pairs, function(pair) {",
    "{cell_body}",
    "}))",
    "{name}_cells$p_adjusted <- stats::p.adjust({name}_cells$p, method = {padj})",
    "{name}_cells$Var1 <- factor({name}_cells$Var1, levels = {name}_vars)",
    "{name}_cells$Var2 <- factor({name}_cells$Var2, levels = {name}_vars)",
    "{name}_cells <- {name}_cells[order({name}_cells$Var2, {name}_cells$Var1), ]",
    "{name}_cells$value <- round({name}_cells$r, 2)",
    "{name}_crossed <- {name}_cells[!is.na({name}_cells$p_adjusted) & {name}_cells$p_adjusted > {sig}, ]",
    "",
    "# Legend title: sample size and method.",
    "{name}_n_label <- if (length(unique({name}_cells$n)) == 1L) {",
    "  bquote(atop(scriptstyle(bold(\"sample sizes:\")), italic(n) ~ \"=\" ~ .(prettyNum({name}_cells$n[1], big.mark = \",\"))))",
    "} else {",
    "  {name}_mode <- as.numeric(names(which.max(table({name}_cells$n))))",
    "  bquote(atop(atop(scriptstyle(bold(\"sample sizes:\")), italic(n)[min] ~ \"=\" ~ .(prettyNum(min({name}_cells$n), big.mark = \",\"))),",
    "    atop(italic(n)[mode] ~ \"=\" ~ .(prettyNum({name}_mode, big.mark = \",\")), italic(n)[max] ~ \"=\" ~ .(prettyNum(max({name}_cells$n), big.mark = \",\")))))",
    "}",
    "{name}_legend <- bquote(atop(.({name}_n_label), atop(scriptstyle(bold(\"{corr_word}\")), {method_title})))",
    "",
    "# 2. The figure",
    "{name}_plot <- ggplot2::ggplot({name}_cells, ggplot2::aes(x = Var1, y = Var2, fill = value)) +",
    "  # tiles coloured by the correlation",
    "  ggplot2::geom_tile(colour = \"black\") +",
    "  # crosses: not significant after correction (grey, under the numbers so they stay readable)",
    "  ggplot2::geom_point(data = {name}_crossed, ggplot2::aes(x = Var1, y = Var2), inherit.aes = FALSE,",
    "    shape = \"cross\", size = 14, colour = \"grey60\") +",
    "  # the coefficient in each cell",
    "  ggplot2::geom_text(ggplot2::aes(x = Var1, y = Var2), label = ifelse(is.na({name}_cells$value), \"\", {name}_cells$value),",
    "    colour = \"black\",",
    "    size = 4, fontface = \"plain\", family = figure_font) +",
    "  ggplot2::scale_fill_gradient2(low = \"#EA4335\", mid = \"white\", high = \"#4285F4\", midpoint = 0,",
    "    limits = c(-1, 1), space = \"Lab\", na.value = \"grey85\", name = {name}_legend) +",
    "  ggplot2::labs(x = NULL, y = NULL, title = {title}, subtitle = NULL,",
    "    caption = quote(atop(displaystyle(NULL), expr = bold(\"X\") ~ \"= non-significant at\" ~ ({p_label} ~ \"< \" ~ {sig})))) +",
    "  theme_figure() +",
    "  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, size = 12, hjust = 1),",
    "    axis.text.y = ggplot2::element_text(size = 12), panel.grid.major = ggplot2::element_blank(),",
    "    panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_text(size = 15)) +",
    "  ggplot2::coord_fixed()",
    sep = "\n"), name = name, data = data, vars = .vec(vars),
    cell_body = .corr_cell_code(data, method, controls, match.arg(alternative, c("two.sided", "greater", "less")), exact),
    corr_word = if (length(controls)) "correlation (partial):" else "correlation:",
    partial_note = if (length(controls)) paste0(" controlling for ", paste(controls, collapse = ", ")) else "",
    method_word = switch(method, pearson = "Pearson's r", spearman = "Spearman's rho", kendall = "Kendall's tau-b"),
    method_title = switch(method, pearson = "Pearson", spearman = "Spearman", kendall = "Kendall"),
    adj = p.adjust.method, padj = .lit(p.adjust.method), sig = .num(sig.level),
    title = if (is.null(title)) "NULL" else .lit(title), p_label = p_label)
  .with_setup(code, setup, FALSE)
}

#' Recipe: stacked percentage bars ([ggbarstats()])
#'
#' Returns 'ggplot2' code that draws the [ggbarstats()] figure: for each
#' level of `y`, a 100% stacked bar of the levels of `x`, labelled with
#' percentages, and the group sizes under the bars.
#'
#' @inheritParams recipe_betweenrain
#' @param x Name of the categorical column shown by the stacks.
#' @param y Name of the column whose levels are the bars.
#' @param counts Optional name of a column of frequency counts (one row per
#'   cell instead of one row per case).
#' @param paired Paired (McNemar) test when `stats = TRUE`.
#' @param legend.title Legend title.
#' @param xlab,title Axis title and plot title.
#' @param weights Optional name of a column of case weights: counts and
#'   percentages are weighted (no statistics; BetterGG itself has no weights).
#' @param facet Optional name of a column splitting the bars into panels.
#' @param cramers_v With `stats = TRUE` and an unpaired parametric test of
#'   association: `"bias-corrected"` (the default, as [ggbarstats()] prints it,
#'   with its confidence interval) or `"uncorrected"`, Cramer's V as jamovi and
#'   SPSS report it, `sqrt(chi^2 / (n * (k - 1)))` from the same test, without
#'   an interval.
#' @return R code as one character string.
#' @examples
#' cat(recipe_barstats("mtcars", "am", "cyl"))
#' @export
recipe_barstats <- function(data, x, y, name = "plot", counts = NULL, stats = FALSE, type = "parametric",
                            paired = FALSE, conf.level = 0.95, xlab = y, legend.title = x,
                            title = NULL, weights = NULL, facet = NULL, setup = TRUE,
                            cramers_v = c("bias-corrected", "uncorrected")) {
  cramers_v <- match.arg(cramers_v)
  .check_name1(data = data, x = x, y = y, counts = counts, weights = weights, facet = facet)
  .check_prefix(name)
  type <- .stats_type(type)
  .check_categorical_stats(stats, weights, facet, counts)
  prep <- .categorical_prep(data, x, y, counts, name, weights = weights, facet = facet)
  stats_code <- if (isTRUE(stats)) .categorical_stats(name, x, y, type, paired, conf.level, two_way = TRUE,
    uncorrected_v = cramers_v == "uncorrected") else NULL
  layers <- c(
    .fill("ggplot2::ggplot({name}_cells, ggplot2::aes(x = {Y}, y = perc, fill = {X}))", name = name, X = .sym(x), Y = .sym(y)),
    "  # 100% stacked bars",
    "  ggplot2::geom_col(position = \"fill\", colour = \"black\")",
    paste0("  ggplot2::scale_y_continuous(labels = function(p) paste0(round(100 * p), \"%\"),\n",
      "    breaks = seq(0, 1, by = 0.1), minor_breaks = seq(0.05, 0.95, by = 0.1))"),
    "  # percentage labels",
    .fill(paste0("  ggplot2::geom_label(ggplot2::aes(label = label, group = {X}), position = ggplot2::position_fill(vjust = 0.5),\n",
      "    na.rm = TRUE, show.legend = FALSE, alpha = 1, fill = \"white\", family = figure_font)"), X = .sym(x)),
    "  theme_figure()",
    "  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())",
    .fill(paste0("  (if ({name}_many) ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
",
      "    plot.margin = ggplot2::margin(5.5, 5.5, 5.5, {name}_left_room)))"), name = name),
    .fill("  ggplot2::guides(fill = ggplot2::guide_legend(title = {lt}))", lt = .lit(legend.title)),
    .fill("  ggplot2::scale_fill_manual(values = {name}_fills, name = \"\", drop = FALSE)", name = name),
    if (!is.null(facet)) .fill("  ggplot2::facet_wrap(ggplot2::vars({F}), labeller = ggplot2::label_wrap_gen(width = 30))", F = .sym(facet)),
    if (isTRUE(stats)) c(
      "  # proportion test in each bar",
      .fill(paste0("  ggplot2::geom_text(data = {name}_bar_tests, ggplot2::aes(x = {Y}, y = 1.05, label = p_label, fill = NULL),\n",
        "    size = 2.8, parse = TRUE)"), name = name, Y = .sym(y))
    ),
    "  # group sizes under the bars",
    .fill(paste0("  ggplot2::geom_text(data = {name}_sizes, ggplot2::aes(x = {Y}, y = -0.05, label = N, fill = NULL),\n",
      "    size = {name}_size, family = figure_font)"), name = name, Y = .sym(y)),
    .fill("  ggplot2::labs(x = {xlab}, y = NULL, subtitle = {sub}, title = {title}, caption = NULL)",
      xlab = .lit(xlab), title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL")
  )
  many <- .fill(paste(
    "# Many or long bar labels are angled; with many bars the group sizes get smaller, so they do not overlap.",
    "{name}_ticks <- levels(factor({name}_cells[[{ys}]]))",
    "{name}_many <- length({name}_ticks) > 6L || sum(nchar({name}_ticks)) > 60L",
    "{name}_size <- max(2.5, min(4, 24 / length({name}_ticks)))",
    "# room on the left for the first angled label (about 4 pt per character)",
    "{name}_left_room <- max(5.5, 4 * nchar({name}_ticks[1]) - 30)",
    sep = "\n"), name = name, ys = .lit(y))
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 3. The figure", many,
    .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, stats)
}

#' Recipe: pie chart ([ggpiestats()])
#'
#' Returns 'ggplot2' code that draws the [ggpiestats()] figure: the
#' percentage of each level of `x`, one pie per level of `y` when `y` is
#' given.
#'
#' @inheritParams recipe_barstats
#' @param y Optional name of a column whose levels get one pie each.
#' @param max_labels Draw percentage labels only up to this many categories
#'   (all categories stay in the legend).
#' @param weights Optional name of a column of case weights (no statistics).
#' @param title Plot title.
#' @return R code as one character string.
#' @examples
#' cat(recipe_piestats("mtcars", "cyl"))
#' @export
recipe_piestats <- function(data, x, y = NULL, name = "plot", counts = NULL, stats = FALSE,
                            type = "parametric", paired = FALSE, conf.level = 0.95,
                            legend.title = x, title = NULL, max_labels = Inf, weights = NULL, setup = TRUE,
                            cramers_v = c("bias-corrected", "uncorrected")) {
  cramers_v <- match.arg(cramers_v)
  .check_name1(data = data, x = x, y = y, counts = counts, weights = weights)
  .check_prefix(name)
  type <- .stats_type(type)
  .check_categorical_stats(stats, weights, NULL, counts)
  prep <- .categorical_prep(data, x, y, counts, name, weights = weights)
  stats_code <- if (isTRUE(stats)) .categorical_stats(name, x, y, type, paired, conf.level, two_way = !is.null(y),
    uncorrected_v = cramers_v == "uncorrected") else NULL
  label_layer <- .fill(paste0("ggplot2::geom_label(ggplot2::aes(label = label, group = {X}), position = ggplot2::position_fill(vjust = 0.5),\n",
    "    fill = \"white\", alpha = 1, na.rm = TRUE, show.legend = FALSE, family = figure_font)"), X = .sym(x))
  layers <- c(
    .fill("ggplot2::ggplot({name}_cells, ggplot2::aes(x = \"\", y = perc))", name = name),
    "  # slices",
    .fill("  ggplot2::geom_col(ggplot2::aes(fill = {X}), position = \"fill\", colour = \"black\", width = 1)", X = .sym(x)),
    "  # percentage labels",
    if (is.finite(max_labels)) .fill("  (if (nlevels({name}_cells[[{xs}]]) <= {max}) {label})", name = name,
      xs = .lit(x), max = .num(max_labels), label = label_layer) else paste0("  ", label_layer),
    if (!is.null(y)) .fill("  ggplot2::facet_wrap(facets = ggplot2::vars({Y}), labeller = ggplot2::label_wrap_gen(width = 30))", Y = .sym(y)),
    "  ggplot2::coord_polar(theta = \"y\")",
    "  ggplot2::scale_y_continuous(breaks = NULL)",
    .fill("  ggplot2::scale_fill_manual(values = {name}_fills, name = \"\", drop = FALSE)", name = name),
    "  theme_figure()",
    "  # a pie has no axes: no lines, ticks or tick labels",
    paste0("  ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),\n",
      "    axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank())"),
    if (isTRUE(stats) && !is.null(y)) .fill(paste0("  ggplot2::geom_text(data = {name}_bar_tests, ggplot2::aes(label = test_label, x = 1.65, y = 0.5),\n",
      "    position = ggplot2::position_fill(vjust = 1), size = 2.8, parse = TRUE)"), name = name),
    .fill("  ggplot2::labs(x = NULL, y = NULL, subtitle = {sub}, title = {title}, caption = NULL)",
      title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL"),
    .fill("  ggplot2::guides(fill = ggplot2::guide_legend(title = {lt}, override.aes = list(colour = NA)))",
      lt = .lit(legend.title))
  )
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 3. The figure",
    .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, stats)
}

.check_categorical_stats <- function(stats, weights, facet, counts) {
  if (isTRUE(stats) && (!is.null(weights) || !is.null(facet))) {
    stop("BetterGG recipe: statistics are not available with case weights or panels.", call. = FALSE)
  }
  invisible(TRUE)
}

# Counts and percentages of x within each level of y (and facet), or overall.
# With weights, each case counts its weight.
.categorical_prep <- function(data, x, y, counts, name, weights = NULL, facet = NULL) {
  cols <- c(x, y, facet, counts, weights)
  by <- c(facet, y)
  v <- list(name = name, data = data, cols = .vec(cols), xs = .lit(x), ys = if (is.null(y)) "" else .lit(y),
    cs = if (is.null(counts)) "" else .lit(counts), ws = if (is.null(weights)) "" else .lit(weights))
  code <- do.call(.fill, c(list(paste(c(
    "# 1. Data: complete cases; the categories become factors.",
    "{name}_data <- {data}[stats::complete.cases({data}[, {cols}, drop = FALSE]), {cols}, drop = FALSE]",
    if (!is.null(counts)) c(
      "# One row per case: repeat each row by its count.",
      "{name}_data <- {name}_data[rep(seq_len(nrow({name}_data)), {name}_data[[{cs}]]), setdiff(names({name}_data), {cs}), drop = FALSE]"),
    paste0("for (column in ", .vec(c(x, y, facet)), ") {name}_data[[column]] <- as.factor({name}_data[[column]])"),
    if (!is.null(weights)) "{name}_weight <- {name}_data[[{ws}]]" else "{name}_weight <- rep(1, nrow({name}_data))",
    "",
    "# 2. Count{weighted} and percentage of each category{within}."),
    collapse = "\n")), c(v, within = if (is.null(y)) "" else " within each bar",
    weighted = if (is.null(weights)) "" else " (weighted)")))
  # xtabs() sums the weights of each cell (1 per case without weights)
  terms <- paste(c(by, x), collapse = " + ")
  cross <- function(vars) {
    paste0("as.data.frame(stats::xtabs(", name, "_weight ~ ", paste(paste0(name, "_data[[", .lit(vars), "]]"), collapse = " + "),
      "), responseName = \"counts\")")
  }
  code <- .code(code, paste0(name, "_cells <- ", cross(c(by, x))),
    paste0("names(", name, "_cells)[seq_len(", length(c(by, x)), ")] <- ", .vec(c(by, x))))
  if (is.null(y)) {
    code <- .code(code, .fill(paste(
      "{name}_cells$perc <- 100 * {name}_cells$counts / sum({name}_cells$counts)",
      "{name}_cells <- {name}_cells[order({name}_cells[[{xs}]]), ]",
      sep = "\n"), name = name, xs = v$xs))
  } else {
    group <- paste0("interaction(", paste(paste0(name, "_cells[[", .lit(by), "]]"), collapse = ", "), ")")
    code <- .code(code, .fill(paste(
      "{name}_cells$perc <- 100 * {name}_cells$counts / stats::ave({name}_cells$counts, {group}, FUN = sum)",
      "{name}_cells$perc[is.nan({name}_cells$perc)] <- 0",
      "{name}_cells <- {name}_cells[order({order}), ]",
      "# Group sizes, shown under the bars",
      "{name}_sizes <- {sizes}",
      "names({name}_sizes)[seq_len({nby})] <- {byv}",
      "{name}_sizes$N <- paste0(\"(n = \", prettyNum(round({name}_sizes$counts, 2), big.mark = \",\"), \")\")",
      "{name}_sizes <- {name}_sizes[order({name}_sizes[[{ys}]], decreasing = TRUE), ]",
      sep = "\n"), name = name, ys = v$ys, group = group, sizes = cross(by), nby = as.character(length(by)),
      byv = .vec(by), order = paste(paste0(name, "_cells[[", .lit(c(by, x)), "]]"), collapse = ", ")))
  }
  .code(code, .fill(paste(
    "{name}_cells$label <- ifelse({name}_cells$counts == 0, NA_character_, paste0(round({name}_cells$perc), \"%\"))",
    "# Fill colours: the palette, or a longer qualitative palette for many categories.",
    "{name}_fills <- if (nlevels({name}_data[[{xs}]]) <= length(figure_fills)) figure_fills else",
    "  grDevices::hcl.colors(nlevels({name}_data[[{xs}]]), \"Dark 3\")",
    sep = "\n"), name = name, xs = v$xs))
}

.categorical_stats <- function(name, x, y, type, paired, conf.level, two_way, uncorrected_v = FALSE) {
  v <- list(name = name, X = .sym(x), Y = if (is.null(y)) "NULL" else .sym(y), xs = .lit(x),
    ys = if (is.null(y)) "" else .lit(y), type = .lit(type), paired = .num(isTRUE(paired)),
    conf = .num(conf.level))
  code <- do.call(.fill, c(list(paste(
    "# Statistics (statsExpressions): {what}; p-values in APA style.",
    "{name}_test <- statsExpressions::contingency_table({name}_data, x = {X}{y_arg}, type = {type},",
    "  paired = {paired}, conf.level = {conf}, digits = 2L)",
    "{name}_subtitle <- figure_apa_p({name}_test$expression[[1]], {name}_test$p.value[[1]])",
    sep = "\n")), c(v, what = if (two_way) "test of association" else "goodness of fit",
    y_arg = if (two_way) paste0(", y = ", v$Y) else "")))
  if (two_way && !isTRUE(paired) && isTRUE(uncorrected_v) && identical(type, "parametric")) {
    code <- .code(code, do.call(.fill, c(list(paste(
      "# Cramer's V as jamovi reports it, without the bias correction statsExpressions applies:",
      "# sqrt(chi-squared / (n * (k - 1))) from the same test, k the smaller number of categories",
      "# (jamovi gives no interval for it).",
      "{name}_k <- min(dim(table(droplevels(factor({name}_data[[{xs}]])), droplevels(factor({name}_data[[{ys}]])))))",
      "{name}_v <- sqrt({name}_test$statistic[[1]] / ({name}_test$n.obs[[1]] * ({name}_k - 1)))",
      "{name}_subtitle <- figure_apa_p(substitute(",
      "  list(chi[\"Pearson\"]^2 * \"(\" * df * \")\" == chi2, italic(p) == p_value, italic(\"V\")[\"Cramer\"] == v, italic(\"n\")[\"obs\"] == n),",
      "  list(df = {name}_test$df[[1]], chi2 = formatC({name}_test$statistic[[1]], format = \"f\", digits = 2),",
      "    p_value = formatC({name}_test$p.value[[1]], format = \"f\", digits = 3),",
      "    v = formatC({name}_v, format = \"f\", digits = 2),",
      "    n = prettyNum({name}_test$n.obs[[1]], big.mark = \",\"))), {name}_test$p.value[[1]])",
      sep = "\n")), v)))
  }
  if (two_way) {
    code <- .code(code, do.call(.fill, c(list(paste(
      "# Goodness-of-fit test of the categories within each bar",
      "{name}_bar_tests <- do.call(rbind, lapply(levels({name}_data[[{ys}]]), function(level) {",
      "  part <- {name}_data[{name}_data[[{ys}]] == level, , drop = FALSE]",
      "  observed <- table(droplevels(part[[{xs}]]))",
      "  test <- if (length(observed) > 1L) suppressWarnings(stats::chisq.test(observed)) else NULL",
      "  p <- if (is.null(test)) NA_real_ else test$p.value",
      "  data.frame(level = level, n = sum(observed),",
      "    statistic = if (is.null(test)) NA_real_ else unname(test$statistic),",
      "    df = if (is.null(test)) NA_real_ else unname(test$parameter), p = p)",
      "}))",
      "names({name}_bar_tests)[1] <- {ys}",
      "{name}_bar_tests <- {name}_bar_tests[rev(seq_len(nrow({name}_bar_tests))), ]",
      "{name}_bar_tests$p_label <- vapply({name}_bar_tests$p, function(p) paste(deparse(",
      "  figure_apa_p(paste0(\"list(~italic(p)=='\", formatC(p, format = \"f\", digits = 2), \"')\"), p)), collapse = \"\"), character(1))",
      "{name}_bar_tests$test_label <- vapply(seq_len(nrow({name}_bar_tests)), function(i) paste(deparse(",
      "  figure_apa_p(paste0(\"list(~chi['gof']^2~(\", {name}_bar_tests$df[i], \")==\", formatC({name}_bar_tests$statistic[i], format = \"f\", digits = 2),",
      "    \", ~italic(p)=='\", formatC({name}_bar_tests$p[i], format = \"f\", digits = 2), \"', ~italic(n)=='\", {name}_bar_tests$n[i], \"')\"),",
      "    {name}_bar_tests$p[i])), collapse = \"\"), character(1))",
      "# A bar with a single category has no test: say so instead of printing p = NA",
      "{name}_bar_tests$p_label[is.na({name}_bar_tests$p)] <- \"list(~'not estimable')\"",
      "{name}_bar_tests$test_label[is.na({name}_bar_tests$p)] <- \"list(~'not estimable')\"",
      sep = "\n")), v)))
  }
  code
}

#' Recipe: regression coefficients ([ggcoefstats()])
#'
#' Returns 'ggplot2' code that draws the [ggcoefstats()] figure: each model
#' coefficient as a point with its confidence interval, and a dashed line at
#' zero (at one for odds ratios).
#'
#' @inheritParams recipe_betweenrain
#' @param model Name of a fitted `lm` or `glm` object.
#' @param table Optional name of a data frame with columns `term`,
#'   `estimate`, `conf.low`, `conf.high` (and, for `stats = TRUE`,
#'   `statistic`, `p.value` and optionally `df.error`) to plot instead of the
#'   table the code computes from `model`.
#' @param ci How the code computes intervals from `model`: `"t"`
#'   ([stats::confint()] for `lm`), `"wald"` (normal approximation,
#'   [stats::confint.default()]) or `"profile"` (profile likelihood,
#'   [stats::confint()] for `glm`).
#' @param exponentiate Show exponentiated coefficients (odds ratios for a
#'   logistic model) on a log axis.
#' @param intercept Include the intercept.
#' @param readable_terms Label factor coefficients as "factor: level vs
#'   reference" instead of the coefficient name (e.g. "framingloss").
#' @param stats Label each coefficient with its estimate, test statistic and
#'   p-value, and show AIC/BIC as the caption.
#' @return R code as one character string.
#' @examples
#' cat(recipe_coefstats("fit"))
#' @export
recipe_coefstats <- function(model, name = "plot", table = NULL, ci = c("t", "wald", "profile"),
                             conf.level = 0.95, exponentiate = FALSE, intercept = TRUE, stats = FALSE,
                             readable_terms = FALSE, xlab = NULL, ylab = "term", title = NULL, setup = TRUE) {
  .check_name1(model = model, table = table)
  .check_prefix(name)
  .check_conf(conf.level)
  ci <- match.arg(ci)
  stat_symbol <- "statistic"
  if (is.null(table)) {
    ci_code <- switch(ci,
      t = "stats::confint({model}, level = {conf})",
      wald = "stats::confint.default({model}, level = {conf})",
      profile = "suppressMessages(stats::confint({model}, level = {conf}))")
    prep <- .fill(paste(
      "# 1. Coefficient table: estimate, {ci_word} confidence interval, test statistic, p-value.",
      "{name}_summary <- summary({model})$coefficients",
      "{name}_intervals <- {ci_code}",
      "{name}_table <- data.frame(",
      "  term = rownames({name}_summary),",
      "  estimate = {name}_summary[, 1],",
      "  conf.low = {name}_intervals[rownames({name}_summary), 1],",
      "  conf.high = {name}_intervals[rownames({name}_summary), 2],",
      "  statistic = {name}_summary[, 3],",
      "  p.value = {name}_summary[, 4],",
      "  df.error = if (inherits({model}, \"glm\") && !(family({model})$family %in% c(\"gaussian\", \"Gamma\", \"inverse.gaussian\", \"quasi\"))) NA_real_ else stats::df.residual({model}),",
      "  row.names = NULL",
      ")",
      sep = "\n"), name = name, model = model, ci_code = .fill(ci_code, model = model, conf = .num(conf.level)),
      ci_word = switch(ci, t = "t-based", wald = "Wald (normal)", profile = "profile-likelihood"))
  } else {
    prep <- .fill(paste(
      "# 1. Coefficient table supplied by the analysis.",
      "{name}_table <- as.data.frame({table})",
      sep = "\n"), name = name, table = table)
  }
  if (!isTRUE(intercept)) {
    prep <- .code(prep, .fill("{name}_table <- {name}_table[!grepl(\"(Intercept)\", {name}_table$term, fixed = TRUE), , drop = FALSE]",
      name = name))
  }
  if (isTRUE(exponentiate)) {
    prep <- .code(prep, .fill(paste(
      "# Odds ratios: exponentiated estimates and interval limits.",
      "{name}_table[c(\"estimate\", \"conf.low\", \"conf.high\")] <- exp({name}_table[c(\"estimate\", \"conf.low\", \"conf.high\")])",
      sep = "\n"), name = name))
  }
  if (isTRUE(readable_terms) && is.null(table)) {
    prep <- .code(prep, .fill(paste(
      "# Readable names for factor coefficients, e.g. \"framing: loss vs gain\".",
      "for (factor_name in names({model}$xlevels)) {",
      "  levels <- {model}$xlevels[[factor_name]]",
      "  coded <- {name}_table$term %in% paste0(factor_name, levels[-1])",
      "  {name}_table$term[coded] <- paste0(factor_name, \": \", substring({name}_table$term[coded], nchar(factor_name) + 1L),",
      "    \" vs \", levels[1])",
      "}",
      sep = "\n"), name = name, model = model))
  }
  prep <- .code(prep, .fill(paste(
    "# Terms listed top to bottom in model order.",
    "{name}_table$term <- factor({name}_table$term, levels = rev(unique({name}_table$term)))",
    sep = "\n"), name = name))
  stats_code <- if (isTRUE(stats)) .fill(paste(
    "# Labels: estimate, test statistic and APA p-value of each coefficient.",
    "{name}_table$label <- vapply(seq_len(nrow({name}_table)), function(i) {",
    "  row <- {name}_table[i, ]",
    "  stat <- if (is.na(row$df.error)) {",
    "    paste0(\"italic(z)=='\", formatC(row$statistic, format = \"f\", digits = 2), \"'\")",
    "  } else {",
    "    paste0(\"italic(t)('\", row$df.error, \"')=='\", formatC(row$statistic, format = \"f\", digits = 2), \"'\")",
    "  }",
    "  label <- paste0(\"list(widehat(italic({symbol}))=='\", formatC(row$estimate, format = \"f\", digits = 2), \"', \",",
    "    stat, \", italic(p)=='\", formatC(row$p.value, format = \"f\", digits = 3), \"')\")",
    "  paste(deparse(figure_apa_p(label, row$p.value), width.cutoff = 500L), collapse = \"\")",
    "}, character(1))",
    if (is.null(table)) "{name}_caption <- bquote(list(AIC == .(formatC(stats::AIC({model}), format = \"f\", digits = 0)), BIC == .(formatC(stats::BIC({model}), format = \"f\", digits = 0))))" else "{name}_caption <- NULL",
    sep = "\n"), name = name, model = model, symbol = if (isTRUE(exponentiate)) "OR" else "beta") else NULL
  ref <- if (isTRUE(exponentiate)) "1" else "0"
  layers <- c(
    .fill("ggplot2::ggplot({name}_table, ggplot2::aes(x = estimate, y = term))", name = name),
    "  # estimate",
    "  ggplot2::geom_point(size = 3, colour = \"blue\", na.rm = TRUE)",
    .fill("  # {pct}% confidence interval", pct = .num(100 * conf.level)),
    "  ggplot2::geom_errorbar(ggplot2::aes(xmin = conf.low, xmax = conf.high), orientation = \"y\", width = 0, na.rm = TRUE)",
    .fill("  # no effect ({ref})", ref = ref),
    .fill("  ggplot2::geom_vline(xintercept = {ref}, linewidth = 1, linetype = \"dashed\")", ref = ref),
    if (isTRUE(stats)) .fill(paste0("  ggrepel::geom_label_repel(ggplot2::aes(label = label), parse = TRUE, colour = \"black\",\n",
      "    size = 3, direction = \"y\", min.segment.length = 0, na.rm = TRUE, family = figure_font)")),
    if (isTRUE(exponentiate)) "  ggplot2::scale_x_log10()",
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, caption = {cap}, subtitle = NULL, title = {title})",
      xlab = .lit(xlab %||% if (isTRUE(exponentiate)) "odds ratio" else "estimate"), ylab = .lit(ylab),
      cap = if (isTRUE(stats)) paste0(name, "_caption") else "NULL",
      title = if (is.null(title)) "NULL" else .lit(title)),
    "  theme_figure()",
    "  ggplot2::theme(plot.caption = ggplot2::element_text(size = 10))"
  )
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 2. The figure",
    .join_layers(paste0(name, "_plot"), layers))
  .with_setup(code, setup, stats)
}

# The code computing one cell of the correlation matrix (inside a function of
# `pair`). Without controls: stats::cor.test(), or NA when the pair cannot be
# correlated. With controls: the correlation of the residuals after
# regressing both variables on the controls (ranks first for Spearman),
# tested with n - 2 - k degrees of freedom, as for partial correlations.
.corr_cell_code <- function(data, method, controls, alternative = "two.sided", exact = FALSE) {
  if (!length(controls)) {
    return(.fill(paste(
      "  complete <- stats::complete.cases({data}[, pair])",
      "  test <- tryCatch(suppressWarnings(stats::cor.test({data}[[pair[1]]][complete], {data}[[pair[2]]][complete],",
      "    method = {method}{alt}{exact})), error = function(e) NULL)",
      "  if (is.null(test)) return(data.frame(Var1 = pair[1], Var2 = pair[2], r = NA_real_, p = NA_real_, n = sum(complete)))",
      "  data.frame(Var1 = pair[1], Var2 = pair[2], r = unname(test$estimate), p = test$p.value, n = sum(complete))",
      sep = "\n"), data = data, method = .lit(method),
      alt = if (alternative != "two.sided") paste0(", alternative = ", .lit(alternative)) else "",
      exact = if (method != "pearson" && isFALSE(exact)) ", exact = FALSE" else ""))
  }
  .fill(paste(
    c("  columns <- c(pair, {controls})",
      "  complete <- stats::complete.cases({data}[, columns])",
      "  part <- {data}[complete, columns, drop = FALSE]",
      if (method == "spearman") "  part[] <- lapply(part, rank)",
      "  design <- cbind(1, as.matrix(part[, {controls}, drop = FALSE]))",
      "  residual <- function(v) stats::lm.fit(design, part[[v]])$residuals",
      "  r <- stats::cor(residual(pair[1]), residual(pair[2]))",
      "  df <- nrow(part) - 2L - {k}",
      "  t <- r * sqrt(df / (1 - r^2))",
      paste0("  data.frame(Var1 = pair[1], Var2 = pair[2], r = r, p = ", switch(alternative,
        two.sided = "2 * stats::pt(-abs(t), df)", greater = "stats::pt(t, df, lower.tail = FALSE)",
        less = "stats::pt(t, df)"), ", n = nrow(part))")),
    collapse = "\n"), data = data, controls = .vec(controls), k = paste0(length(controls), "L"))
}
