#' Recipe: between-subjects raincloud ([ggbetweenrain()])
#'
#' Returns 'ggplot2' code that draws the [ggbetweenrain()] figure: a
#' half-density cloud, a narrow box and every observation as a dot for each
#' group, the group mean (or median / trimmed mean) as a labelled point, and
#' the group sizes under the axis.
#'
#' @param data Name of the data frame, as text.
#' @param x,y Names of the grouping column and the outcome column.
#' @param name Prefix for the objects the code creates (`<name>_data`,
#'   `<name>_plot`, ...).
#' @param type `"parametric"` (mean, Welch/Student tests), `"nonparametric"`
#'   (median), `"robust"` (20% trimmed mean). With `stats = TRUE`, also
#'   `"bayes"`.
#' @param stats Show the test result as the subtitle and, for three or more
#'   groups, the significant pairwise comparisons (default `FALSE`).
#' @param var.equal Assume equal variances (Student's t / Fisher's ANOVA)
#'   when `stats = TRUE`.
#' @param conf.level Confidence level of intervals and effect sizes.
#' @param p.adjust.method Multiplicity correction of pairwise comparisons.
#' @param alternative Direction of the two-group test (`"two.sided"`,
#'   `"greater"`, `"less"`) when `stats = TRUE`.
#' @param bf.message Add the Bayes factor caption when `stats = TRUE` and
#'   `type = "parametric"`.
#' @param tr Trimming proportion for `type = "robust"`.
#' @param xlab,ylab,title Axis titles and plot title.
#' @param means Optional name of a data frame of model-based means, with a
#'   column named like `x` plus `emmean`, `lower.CL` and `upper.CL` (as from
#'   `as.data.frame(confint(emmeans::emmeans(...)))`). When given, the labelled
#'   points are these means and their confidence intervals are drawn, instead
#'   of the observed group means.
#' @param means_label Subscript naming the model means in the labels.
#' @param orientation `"vertical"` or `"horizontal"`.
#' @param setup Include [recipe_setup()] code so the result runs on its own.
#' @return R code as one character string.
#' @examples
#' cat(recipe_betweenrain("mtcars", "cyl", "mpg"))
#' @export
recipe_betweenrain <- function(data, x, y, name = "plot", type = "parametric", stats = FALSE,
                               var.equal = FALSE, conf.level = 0.95, p.adjust.method = "holm",
                               alternative = "two.sided", bf.message = FALSE, tr = 0.2,
                               xlab = x, ylab = y, title = NULL,
                               means = NULL, means_label = "EMM",
                               orientation = c("vertical", "horizontal"), setup = TRUE) {
  .check_name1(data = data, x = x, y = y, means = means)
  .check_prefix(name)
  .check_conf(conf.level)
  type <- .stats_type(type)
  orientation <- match.arg(orientation)
  centre_type <- if (type == "bayes") "parametric" else type
  if (type == "bayes" && !isTRUE(stats)) type <- "parametric"
  centre <- .centrality_word(centre_type)

  v <- list(
    name = name, data = data, xs = .lit(x), ys = .lit(y), X = .sym(x), Y = .sym(y),
    centre = centre, centre_fun = .centrality_code(centre_type, tr)
  )
  prep <- do.call(.fill, c(list(paste(
    "# 1. Data: rows with both {xs} and {ys}; the groups become a factor.",
    "{name}_data <- {data}[!is.na({data}[[{xs}]]) & !is.na({data}[[{ys}]]), , drop = FALSE]",
    "{name}_data[[{xs}]] <- droplevels(as.factor({name}_data[[{xs}]]))",
    "",
    "# 2. Group sizes and the group {centre} shown as a labelled point.",
    "{name}_groups <- split({name}_data[[{ys}]], {name}_data[[{xs}]])",
    "{name}_summary <- data.frame(",
    "  level = factor(names({name}_groups), levels = levels({name}_data[[{xs}]])),",
    "  value = vapply({name}_groups, function(values) {centre_fun}, numeric(1)),",
    "  n = lengths({name}_groups)",
    ")",
    sep = "\n")), v))
  label_word <- centre
  if (!is.null(means)) {
    label_word <- paste0("'", gsub("['\"]", "", means_label), "'")
    prep <- .code(prep, .fill(paste(
      "# Model-based means and confidence intervals replace the observed {centre}s.",
      "{name}_match <- match(as.character({name}_summary$level), as.character({means}[[{xs}]]))",
      "if (anyNA({name}_match)) stop(\"The model means do not cover every group of {X}.\")",
      "{name}_summary$value <- {means}$emmean[{name}_match]",
      "{name}_summary$lower <- {means}$lower.CL[{name}_match]",
      "{name}_summary$upper <- {means}$upper.CL[{name}_match]",
      sep = "\n"), name = name, means = means, xs = .lit(x), X = gsub("\"", "", x), centre = centre))
  }
  prep <- .code(prep, .fill(paste(
    "{name}_summary$label <- paste0(\"list(widehat(mu)[{word}]=='\",",
    "  formatC({name}_summary$value, format = \"f\", digits = 2), \"')\")",
    "{name}_summary$axis_label <- paste0({name}_summary$level, \"\\n(n = \",",
    "  prettyNum({name}_summary$n, big.mark = \",\"), \")\")",
    sep = "\n"), name = name, word = label_word))

  stats_code <- if (isTRUE(stats)) .between_stats_code(v, type, var.equal, conf.level, tr,
    p.adjust.method, bf.message, paired = FALSE, alternative = alternative) else NULL

  layers <- c(
    .fill("ggplot2::ggplot({name}_data, ggplot2::aes(x = {X}, y = {Y}))", name = name, X = v$X, Y = v$Y),
    "  # cloud: the distribution of each group (half density)",
    .fill(paste0("  ggdist::stat_slab(ggplot2::aes(fill = {X}, group = {X}), colour = NA, adjust = 1,\n",
      "    width = 0.5, justification = -0.25, alpha = 0.5, normalize = \"groups\")"), X = v$X),
    "  # rain: every observation as a dot at its exact value",
    .fill(paste0("  ggdist::stat_dots(ggplot2::aes(fill = {X}, colour = {X}, group = {X}), side = \"left\",\n",
      "    justification = 1.2, scale = 0.45, alpha = 0.7, layout = \"swarm\", family = figure_font)"), X = v$X),
    "  # box: median and quartiles",
    .fill("  ggplot2::geom_boxplot(ggplot2::aes(group = {X}), width = 0.12, alpha = 0.6, outlier.shape = NA, na.rm = TRUE)",
      X = v$X),
    if (!is.null(means)) c(
      "  # model-based confidence intervals",
      .fill(paste0("  ggplot2::geom_errorbar(data = {name}_summary, ggplot2::aes(x = level, ymin = lower, ymax = upper),\n",
        "    inherit.aes = FALSE, width = 0.08, linewidth = 0.6)"), name = name)
    ),
    .fill("  # group {what}: point and label", what = if (is.null(means)) centre else "model means"),
    .fill(paste0("  ggplot2::geom_point(data = {name}_summary, ggplot2::aes(x = level, y = value),\n",
      "    inherit.aes = FALSE, size = 4, colour = \"black\")"), name = name),
    .fill(paste0("  ggrepel::geom_label_repel(data = {name}_summary, ggplot2::aes(x = level, y = value, label = label),\n",
      "    inherit.aes = FALSE, parse = TRUE, size = 3, nudge_x = 0.4, segment.linetype = 4,\n",
      "    min.segment.length = 0, family = figure_font)"), name = name)
  )
  if (isTRUE(stats)) layers <- c(layers, .fill("  {name}_brackets", name = name))
  layers <- c(layers,
    .fill("  ggplot2::scale_x_discrete(labels = {name}_summary$axis_label)", name = name),
    .fill("  ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = {sec}, breaks = NULL, labels = NULL))",
      sec = if (isTRUE(stats)) paste0(name, "_pairwise_note") else "NULL"),
    "  ggplot2::scale_colour_manual(values = figure_colours)",
    "  ggplot2::scale_fill_manual(values = figure_fills)",
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, colour = {xlab}, title = {title}, subtitle = {sub}, caption = {cap})",
      xlab = .lit(xlab), ylab = .lit(ylab), title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL",
      cap = if (isTRUE(stats)) paste0(name, "_caption") else "NULL"),
    "  theme_figure()",
    "  ggplot2::theme(legend.position = \"none\", panel.grid.major.x = ggplot2::element_blank())",
    if (orientation == "horizontal") "  ggplot2::coord_flip()"
  )
  plot <- .join_layers(paste0(name, "_plot"), layers)
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 3. The figure", plot)
  .with_setup(code, setup, stats)
}

# "a <- l1 +\n  l2 +\n ..." keeping comment lines (which must not end in +).
.join_layers <- function(object, layers) {
  layers <- layers[nzchar(layers)]
  is_comment <- grepl("^\\s*#", layers)
  real <- which(!is_comment)
  out <- layers
  for (i in real[-length(real)]) out[i] <- paste0(layers[i], " +")
  out[1] <- paste0(object, " <- ", out[1])
  paste(out, collapse = "\n")
}

# Subtitle, Bayes caption and pairwise brackets, computed with the
# 'statsExpressions' functions 'ggstatsplot' uses.
.between_stats_code <- function(v, type, var.equal, conf.level, tr, p.adjust.method,
                                bf.message, paired, subject = NULL, alternative = "two.sided") {
  alternative <- match.arg(alternative, c("two.sided", "greater", "less"))
  v$alt <- .lit(alternative)
  v$type <- .lit(type)
  v$veq <- .num(isTRUE(var.equal))
  v$conf <- .num(conf.level)
  v$tr <- .num(tr)
  v$padj <- .lit(p.adjust.method)
  v$paired <- .num(paired)
  v$subj <- if (paired) paste0(", subject.id = ", .lit(subject)) else ""
  bayes_caption <- isTRUE(bf.message) && type == "parametric"
  # performance::r2_bayes() (inside statsExpressions' Bayesian ANOVA) drops every
  # parameter whose name starts with "g_" as a g-prior, so a grouping column named
  # g or g_... makes it stop. The tests then use a copy with the column renamed.
  x_raw <- eval(str2lang(v$xs))
  rename <- (type == "bayes" || bayes_caption) && grepl("^g(_|$)", x_raw)
  td <- paste0(v$name, "_data")
  if (rename) {
    safe <- if (identical(eval(str2lang(v$ys)), "x_group")) "x_group_" else "x_group"
    rename_code <- .fill(paste(
      "# statsExpressions' Bayesian R-squared (package performance) fails for a grouping column",
      "# named g or g_..., so the tests use a copy in which {xs} is named {safe}.",
      "{name}_test_data <- {name}_data",
      "names({name}_test_data)[names({name}_test_data) == {xs}] <- {safe}",
      sep = "
"), name = v$name, xs = v$xs, safe = .lit(safe))
    td <- paste0(v$name, "_test_data")
    v$xs <- .lit(safe); v$X <- .sym(safe)
  }
  out <- .fill(paste(
    "# Statistics (statsExpressions, the engine of ggstatsplot); p-values in APA style.",
    "{name}_k <- nlevels({td}[[{xs}]])",
    "{name}_test_fun <- if ({name}_k < 3L) statsExpressions::two_sample_test else statsExpressions::oneway_anova",
    "{name}_test_args <- list({td}, x = {xs}, y = {ys}{subj}, paired = {paired}, type = {type},",
    "  var.equal = {veq}, conf.level = {conf}, tr = {tr}, digits = 2L)",
    "# the direction of the test applies to two groups only",
    "if ({name}_k < 3L) {name}_test_args$alternative <- {alt}",
    "{name}_test <- do.call({name}_test_fun, {name}_test_args)",
    "# A Bayesian test has no p-value; its label is shown as statsExpressions writes it.",
    "{name}_subtitle <- figure_apa_p({name}_test$expression[[1]], {name}_test[[\"p.value\"]][[1]])",
    if (bayes_caption) paste(
      "{name}_caption <- {name}_test_fun({td}, x = {X}, y = {Y}{subj}, paired = {paired},",
      "  type = \"bayes\", conf.level = {conf})$expression[[1]]", sep = "\n")
    else "{name}_caption <- NULL",
    "# Pairwise comparisons (three or more groups): brackets for p < .05.",
    "{name}_brackets <- NULL",
    "{name}_pairwise_note <- NULL",
    "if ({name}_k >= 3L) {",
    "  {name}_pairs <- statsExpressions::pairwise_comparisons({td}, x = {X}, y = {Y}{subj},",
    "    type = {type}, paired = {paired}, var.equal = {veq}, tr = {tr}, p.adjust.method = {padj}, digits = 2L)",
    "  {name}_pairwise_note <- parse(text = paste0(\"list('Pairwise test:'~bold('\",",
    "    sub(\"'\", \"\\\\'\", unique({name}_pairs$test), fixed = TRUE),",
    "    if (is.null({name}_pairs[[\"p.value\"]])) \"'), 'Bars shown:'~bold('all (Bayes factors)'))\" else",
    "      \"'), 'Bars shown:'~bold('significant'), alpha == 0.05)\"))",
    "  # as in ggbetweenstats(): Bayes factors (no p-values) get a bar for every pair",
    "  {name}_shown <- if (is.null({name}_pairs[[\"p.value\"]])) {name}_pairs else",
    "    {name}_pairs[which({name}_pairs[[\"p.value\"]] < 0.05), , drop = FALSE]",
    "  {name}_shown <- {name}_shown[order({name}_shown$group1, {name}_shown$group2, method = \"radix\"), , drop = FALSE]",
    "  if (nrow({name}_shown)) {",
    "    {name}_y_range <- diff(range({name}_data[[{ys}]]))",
    "    {name}_brackets <- ggsignif::geom_signif(",
    "      comparisons = Map(c, {name}_shown$group1, {name}_shown$group2),",
    "      annotations = vapply(seq_len(nrow({name}_shown)), function(i) {",
    "        paste(deparse(figure_apa_p({name}_shown$expression[[i]], {name}_shown$p.value[[i]]),",
    "          width.cutoff = 500L), collapse = \"\")",
    "      }, character(1)),",
    "      map_signif_level = TRUE, test = NULL, parse = TRUE, textsize = 3, tip_length = 0.01, na.rm = TRUE,",
    "      family = figure_font,",
    "      margin_top = if ({name}_y_range > 0) 0.05 + 0.025 * max(0, {name}_data[[{ys}]]) / {name}_y_range else 0.05,",
    "      step_increase = nrow({name}_pairs) / (20 * (nrow({name}_pairs) - 1L))",
    "    )",
    "  }",
    "}",
    sep = "\n"), name = v$name, xs = v$xs, ys = v$ys, X = v$X, Y = v$Y, subj = v$subj,
    paired = v$paired, type = v$type, veq = v$veq, conf = v$conf, tr = v$tr, padj = v$padj, alt = v$alt,
    td = td)
  if (rename) paste(rename_code, out, sep = "
") else out
}

#' Recipe: within-subjects raincloud ([ggwithinrain()])
#'
#' Returns 'ggplot2' code that draws the [ggwithinrain()] figure: for each
#' condition a cloud, a narrow box and the observations, the condition means
#' joined by a line, and (for two conditions) each subject's values joined by
#' a dashed line. Only subjects with a value in every condition are used.
#'
#' @inheritParams recipe_betweenrain
#' @param x,y Names of the condition column and the outcome column (long
#'   format: one row per subject and condition).
#' @param subject Name of the subject identifier column.
#' @return R code as one character string.
#' @examples
#' cat(recipe_withinrain("bugs_long", "condition", "desire", "subject"))
#' @export
recipe_withinrain <- function(data, x, y, subject, name = "plot", type = "parametric",
                              stats = FALSE, conf.level = 0.95, p.adjust.method = "holm",
                              alternative = "two.sided", bf.message = FALSE, tr = 0.2,
                              xlab = x, ylab = y, title = NULL,
                              orientation = c("vertical", "horizontal"), setup = TRUE) {
  .check_name1(data = data, x = x, y = y, subject = subject)
  .check_prefix(name)
  .check_conf(conf.level)
  type <- .stats_type(type)
  orientation <- match.arg(orientation)
  centre_type <- if (type == "bayes") "parametric" else type
  if (type == "bayes" && !isTRUE(stats)) type <- "parametric"
  centre <- .centrality_word(centre_type)
  v <- list(
    name = name, data = data, xs = .lit(x), ys = .lit(y), ss = .lit(subject),
    X = .sym(x), Y = .sym(y), S = .sym(subject), centre = centre,
    centre_fun = .centrality_code(centre_type, tr)
  )
  prep <- do.call(.fill, c(list(paste(
    "# 1. Data (long format): subjects with a value of {ys} in every condition.",
    "{name}_data <- {data}[!is.na({data}[[{xs}]]) & !is.na({data}[[{ss}]]), , drop = FALSE]",
    "{name}_data[[{xs}]] <- droplevels(as.factor({name}_data[[{xs}]]))",
    "if (anyDuplicated({name}_data[, c({xs}, {ss})])) {",
    "  stop(\"Each subject must appear at most once per condition; aggregate repeated rows first.\")",
    "}",
    "{name}_data <- {name}_data[!is.na({name}_data[[{ys}]]), , drop = FALSE]",
    "{name}_count <- table({name}_data[[{ss}]])",
    "{name}_complete <- names({name}_count)[{name}_count == nlevels({name}_data[[{xs}]])]",
    "{name}_data <- {name}_data[as.character({name}_data[[{ss}]]) %in% {name}_complete, , drop = FALSE]",
    "{name}_data <- {name}_data[order({name}_data[[{ss}]], {name}_data[[{xs}]], method = \"radix\"), , drop = FALSE]",
    "",
    "# The rain sits beside each box; with two conditions the first is mirrored so",
    "# each subject's line joins the two inner columns of points.",
    "{name}_levels <- levels({name}_data[[{xs}]])",
    "{name}_position <- match(as.character({name}_data[[{xs}]]), {name}_levels)",
    "{name}_mirror <- length({name}_levels) == 2L",
    "{name}_data$rain_x <- {name}_position + ifelse({name}_mirror & {name}_position == 1L, 0.22, -0.22)",
    "",
    "# 2. Condition sizes and the condition {centre} shown as a labelled point.",
    "{name}_groups <- split({name}_data[[{ys}]], {name}_data[[{xs}]])",
    "{name}_summary <- data.frame(",
    "  level = factor(names({name}_groups), levels = {name}_levels),",
    "  value = vapply({name}_groups, function(values) {centre_fun}, numeric(1)),",
    "  n = lengths({name}_groups)",
    ")",
    "{name}_summary$label <- paste0(\"list(widehat(mu)[{centre}]=='\",",
    "  formatC({name}_summary$value, format = \"f\", digits = 2), \"')\")",
    "{name}_summary$axis_label <- paste0({name}_summary$level, \"\n(n = \",",
    "  prettyNum({name}_summary$n, big.mark = \",\"), \")\")",
    sep = "\n")), v))
  slab <- function(data, extra) {
    paste0("ggdist::stat_slab(", data, "ggplot2::aes(fill = ", v$X, ", group = ", v$X, "),
",
      "    colour = NA, adjust = 1, width = 0.5, alpha = 0.5, normalize = \"groups\", ", extra, ")")
  }
  prep <- .code(prep, "",
    "# Clouds: with two conditions the first one opens to the left.",
    .fill("if ({name}_mirror) {", name = name),
    .fill("  {name}_clouds <- list(", name = name),
    paste0("    ", slab(.fill("data = {name}_data[{name}_position == 1L, , drop = FALSE],
    ", name = name),
      "justification = 1.25, side = \"left\""), ","),
    paste0("    ", slab(.fill("data = {name}_data[{name}_position != 1L, , drop = FALSE],
    ", name = name),
      "justification = -0.25")),
    "  )",
    "} else {",
    paste0(.fill("  {name}_clouds <- list(", name = name), slab("", "justification = -0.25"), ")"),
    "}",
    "# Subject lines are drawn only for two conditions.",
    .fill(paste0("{name}_subject_lines <- if ({name}_mirror) {
",
      "  ggplot2::geom_path(ggplot2::aes(x = rain_x), alpha = 0.5, linetype = \"dashed\")
",
      "}"), name = name))
  stats_code <- if (isTRUE(stats)) .between_stats_code(v, type, FALSE, conf.level, tr,
    p.adjust.method, bf.message, paired = TRUE, subject = subject, alternative = alternative) else NULL

  layers <- c(
    .fill("ggplot2::ggplot({name}_data, ggplot2::aes(x = {X}, y = {Y}, group = {S}))", name = name,
      X = v$X, Y = v$Y, S = v$S),
    "  # cloud: the distribution in each condition (half density)",
    .fill("  {name}_clouds", name = name),
    "  # rain: every observation",
    .fill("  ggplot2::geom_point(ggplot2::aes(x = rain_x, colour = {X}), size = 2, alpha = 0.5, na.rm = TRUE)", X = v$X),
    "  # box: median and quartiles",
    .fill("  ggplot2::geom_boxplot(ggplot2::aes(group = {X}), width = 0.12, alpha = 0.6, outlier.shape = NA, na.rm = TRUE)",
      X = v$X),
    "  # two conditions: each subject's values joined by a dashed line",
    .fill("  {name}_subject_lines", name = name),
    .fill("  # condition {centre}s: joined by a line, labelled", centre = centre),
    .fill(paste0("  ggplot2::geom_path(data = {name}_summary, ggplot2::aes(x = level, y = value, group = 1L),\n",
      "    inherit.aes = FALSE, linewidth = 1, colour = \"red\", alpha = 0.5)"), name = name),
    .fill(paste0("  ggplot2::geom_point(data = {name}_summary, ggplot2::aes(x = level, y = value),\n",
      "    inherit.aes = FALSE, size = 4, colour = \"black\")"), name = name),
    .fill(paste0("  ggrepel::geom_label_repel(data = {name}_summary, ggplot2::aes(x = level, y = value, label = label),\n",
      "    inherit.aes = FALSE, parse = TRUE, size = 3, nudge_x = 0.4, segment.linetype = 4,\n",
      "    min.segment.length = 0, family = figure_font)"), name = name)
  )
  if (isTRUE(stats)) layers <- c(layers, .fill("  {name}_brackets", name = name))
  layers <- c(layers,
    .fill("  ggplot2::scale_x_discrete(labels = {name}_summary$axis_label)", name = name),
    .fill("  ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(name = {sec}, breaks = NULL, labels = NULL))",
      sec = if (isTRUE(stats)) paste0(name, "_pairwise_note") else "NULL"),
    "  ggplot2::scale_colour_manual(values = figure_colours)",
    "  ggplot2::scale_fill_manual(values = figure_fills)",
    .fill("  ggplot2::labs(x = {xlab}, y = {ylab}, colour = {xlab}, title = {title}, subtitle = {sub}, caption = {cap})",
      xlab = .lit(xlab), ylab = .lit(ylab), title = if (is.null(title)) "NULL" else .lit(title),
      sub = if (isTRUE(stats)) paste0("figure_statistics(", name, "_subtitle)") else "NULL",
      cap = if (isTRUE(stats)) paste0(name, "_caption") else "NULL"),
    "  theme_figure()",
    "  ggplot2::theme(legend.position = \"none\", panel.grid.major.x = ggplot2::element_blank())",
    if (orientation == "horizontal") "  ggplot2::coord_flip()"
  )
  plot <- .join_layers(paste0(name, "_plot"), layers)
  code <- .code(prep, "", stats_code, if (!is.null(stats_code)) "", "# 3. The figure", plot)
  .with_setup(code, setup, stats)
}
