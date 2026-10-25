# Drawing defaults shared by the categorical figures (stacked bars, pies,
# one-variable bars). Each rule is kept as R code text: the wrappers evaluate
# it and the recipes write the same text into their code, so both draw the
# same labels and colours.

# Category names wrapped at `width` characters (existing line breaks kept as spaces).
.wrap_code <- function(width) {
  paste0("function(labels, width = ", .num(width), ") vapply(as.character(labels), function(label)\n",
    "  paste(strwrap(label, width = width), collapse = \"\\n\"), character(1), USE.NAMES = FALSE)")
}

# Fill colours of an ordered factor with k levels, in scale order: an odd number
# of levels (a symmetric scale with a middle point) gets a diverging red-to-blue
# palette with a neutral grey middle; an even number a sequential blue palette
# (light to dark, without the near-white end). Greyscale: a light-to-dark grey ramp.
# An ordered factor with at most 11 levels (a rating scale, up to 0-10); longer ordered
# factors (e.g. a region list stored as ordinal) are drawn as unordered.
.rating_scale <- function(x) is.ordered(x) && nlevels(x) <= 11L

.ordered_fills_code <- function(palette) {
  if (identical(palette, "grey")) return("function(k) grDevices::gray.colors(k, start = 0.9, end = 0.3)")
  paste0("function(k) if (k %% 2L == 1L) rev(grDevices::hcl.colors(k, \"Blue-Red 2\")) else\n",
    "  grDevices::hcl.colors(k + 1L, \"Blues 3\", rev = TRUE)[-1]")
}

.eval_code <- function(code) eval(str2lang(code), baseenv())

# Width at which the bar labels of the x axis wrap: up to six bars share about
# 70 characters of axis (at least 10 per bar); with more bars the labels are
# angled and wrap at `width`.
.axis_wrap_width <- function(n, width) if (n > 6L) width else max(10, min(width, floor(70 / n)))

.categorical_own_args <- c("min_label_percent", "wrap_width")

# ggbarstats(), ggpiestats() and their grouped_* versions: the 'ggstatsplot'
# figure, then the categorical drawing defaults.
.wrap_categorical <- function(name) {
  force(name)
  function(..., min_label_percent = 5, wrap_width = 40) {
    .check_own_args(names(rlang::enquos(...)), .categorical_own_args)
    .check_label_args(min_label_percent, wrap_width)
    call <- sys.call()
    env <- parent.frame()
    res <- .call_ggstatsplot(name, call, env, drop = .categorical_own_args)
    data <- .call_data(name, call, env)
    plot <- .finish(res$plot, res$given)
    palette <- getOption("BetterGG.palette", "nejm")
    recolour <- !is.null(palette) && !any(c("palette", "package") %in% res$given)
    .tidy_categorical(plot, data, min_label_percent, wrap_width, if (recolour) palette else NULL)
  }
}

.check_label_args <- function(min_label_percent, wrap_width) {
  if (!is.numeric(min_label_percent) || length(min_label_percent) != 1L || is.na(min_label_percent) ||
      min_label_percent < 0) {
    stop("BetterGG: 'min_label_percent' must be one number of 0 or more.", call. = FALSE)
  }
  if (!is.numeric(wrap_width) || length(wrap_width) != 1L || is.na(wrap_width) || wrap_width < 5) {
    stop("BetterGG: 'wrap_width' must be one number of 5 or more.", call. = FALSE)
  }
  invisible(TRUE)
}

# The data frame the user passed (to see whether a column is an ordered factor;
# 'ggstatsplot' turns ordered factors into plain ones).
.call_data <- function(name, call, env) {
  call[[1]] <- getExportedValue("ggstatsplot", name)
  for (d in intersect(.categorical_own_args, names(call))) call[[d]] <- NULL
  args <- match.call(getExportedValue("ggstatsplot", name), call, expand.dots = TRUE, envir = env)
  if (is.null(args$data)) return(NULL)
  eval(args$data, env)
}

# Percentage labels below `min_label_percent` are dropped (the legend still
# names the category); legend and bar labels wrap; a long category name moves
# the legend below the plot; an ordered fill variable gets ordered colours.
.tidy_categorical <- function(plot, data, min_label_percent, wrap_width, palette) {
  wrap <- .eval_code(.wrap_code(wrap_width))
  one <- function(p) {
    if (!inherits(p, "ggplot") || !all(c(".label", "perc") %in% names(p$data))) return(p)
    p$data$.label[!is.na(p$data$perc) & p$data$perc < min_label_percent] <- NA
    fill <- p$mapping$fill %||% p$layers[[1]]$mapping$fill
    column <- rlang::as_name(fill)
    lv <- levels(p$data[[column]])
    sc <- p$scales$get_scales("fill")
    if (!is.null(sc)) {
      new <- sc$clone()
      new$labels <- function(labels) wrap(labels)
      if (!is.null(palette) && is.data.frame(data) && .rating_scale(data[[column]])) {
        new$palette <- .eval_code(.ordered_fills_code(palette))
      } else if (!is.null(palette) && is.null(tryCatch(bgg_palette(palette, length(lv)), error = function(e) NULL))) {
        # more categories than the palette has colours: a longer qualitative palette (as the recipes)
        new$palette <- function(n) grDevices::hcl.colors(n, "Dark 3")
      }
      p <- suppressMessages(p + new)
    }
    if (any(nchar(lv) > wrap_width)) {
      p <- p + ggplot2::theme(legend.position = "bottom", legend.direction = "vertical",
        legend.key.spacing.y = ggplot2::unit(6, "pt"))
    }
    x <- p$mapping$x
    if (rlang::is_quosure(x) && rlang::quo_is_symbol(x)) {
      bars <- levels(p$data[[rlang::as_name(x)]])
      width <- .axis_wrap_width(length(bars), wrap_width)
      p <- suppressMessages(p + ggplot2::scale_x_discrete(labels = function(labels) wrap(labels, width)))
    } else if (inherits(p$coordinates, "CoordPolar")) {
      # a pie has no axes: no lines, ticks or tick labels (as recipe_piestats())
      p <- p + ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank())
    }
    p
  }
  if (inherits(plot, "patchwork")) plot$patches$plots <- lapply(plot$patches$plots, one)
  one(plot)
}
