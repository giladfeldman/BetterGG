# Each wrapper rebuilds the user's own call with ggstatsplot:: in front and
# evaluates it where the user wrote it, so tidy-eval arguments (bare names,
# !!, {{ }}) behave exactly as with ggstatsplot. The 'ggstatsplot' function is
# looked up when called, not when BetterGG is installed, so updating
# 'ggstatsplot' is enough to get its fixes.
.wrap <- function(name) {
  force(name)
  function(...) {
    res <- .call_ggstatsplot(name, sys.call(), parent.frame())
    .finish(res$plot, res$given)
  }
}

# Rebuild the call for ggstatsplot and evaluate it in the user's frame.
# `given` is the set of argument names the user supplied, resolved the way R
# resolves them (partial names, arguments forwarded through `...`), so a
# palette or theme the user chose is always recognised.
.call_ggstatsplot <- function(name, call, env, drop = character()) {
  fn <- getExportedValue("ggstatsplot", name)
  call[[1]] <- call("::", as.name("ggstatsplot"), as.name(name))
  for (d in intersect(drop, names(call))) call[[d]] <- NULL
  given <- names(match.call(fn, call, expand.dots = TRUE, envir = env))
  # grouped_* functions pass `...` on to the base function, which resolves
  # abbreviations (pal =, ggth =) against its own formals: do the same here.
  if (startsWith(name, "grouped_")) given <- .resolve_partial(given, sub("^grouped_", "", name))
  styled <- isTRUE(getOption("BetterGG.theme", TRUE)) && !"ggtheme" %in% given
  if (styled) {
    call$ggtheme <- theme_apa()
    style <- .rng_neutral(.style_args(sub("^grouped_", "", name)))
    for (nm in setdiff(names(style), given)) call[[nm]] <- style[[nm]]
  }
  if (sub("^grouped_", "", name) == "ggbetweenstats") .warn_var_equal(call, env)
  plot <- eval(call, env)
  if (styled && sub("^grouped_", "", name) == "ggbetweenstats" && !"point.args" %in% given) {
    plot <- .soften_points(plot)
  }
  list(plot = plot, given = c(given, if (styled) ".bgg_styled"))
}

# ggbetweenstats() takes var.equal only through `...` and never uses it: its
# parametric test is always Welch's (statsExpressions is called without it).
# Say so instead of letting var.equal = TRUE look as if it gave Student's t.
.warn_var_equal <- function(call, env) {
  if (!"var.equal" %in% names(call) || !isTRUE(eval(call$var.equal, env))) return(invisible())
  type <- if ("type" %in% names(call)) eval(call$type, env) else "parametric"
  # statsExpressions reads the type by its first letter: "p" is parametric
  if (!is.character(type) || !startsWith(type, "p")) return(invisible())
  warning("BetterGG: 'ggstatsplot' ignores var.equal = TRUE; this figure shows Welch's test ",
    "(and Games-Howell pairwise comparisons). recipe_betweenrain(var.equal = TRUE) draws ",
    "Student's t / Fisher's ANOVA.", call. = FALSE)
}

# The BetterGG points (softer, smaller) are set on ggstatsplot's own jittered
# point layer after the call, not passed as point.args: ggstatsplot's default
# point.args draws a random seed for the jitter, so supplying our own would
# shift the user's random-number stream (and the jitter) away from plain
# ggstatsplot's.
.soften_points <- function(plot) {
  one <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    p$layers <- lapply(p$layers, function(l) {
      if (!inherits(l$geom, "GeomPoint") || !inherits(l$position, "PositionJitterdodge")) return(l)
      new <- ggplot2::ggproto(NULL, l)
      new$aes_params <- utils::modifyList(l$aes_params, list(alpha = 0.6, size = 2.2))
      new
    })
    p
  }
  if (inherits(plot, "patchwork")) plot$patches$plots <- lapply(plot$patches$plots, one)
  one(plot)
}

# Expand unique partial argument names against the formals of ggstatsplot's
# `base` function, the way R matches them when the call reaches it.
.resolve_partial <- function(given, base) {
  formals_base <- setdiff(names(formals(getExportedValue("ggstatsplot", base))), "...")
  vapply(given, function(nm) {
    if (!nzchar(nm) || nm %in% formals_base) return(nm)
    hit <- formals_base[startsWith(formals_base, nm)]
    if (length(hit) == 1L) hit else nm
  }, "", USE.NAMES = FALSE)
}

# Evaluate `expr` without changing the random-number state, so a user's
# set.seed() gives exactly the same Bayesian/bootstrap results as calling
# 'ggstatsplot' directly.
.rng_neutral <- function(expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()))
  expr
}

# Drawing defaults of the BetterGG look (black mean marker, thin outlines).
# Each is used only when the user did not set that argument. The softer points
# are set afterwards by .soften_points().
.style_args <- function(base) {
  grey <- identical(getOption("BetterGG.palette", "nejm"), "grey")
  centre <- list(centrality.point.args = list(size = 4, colour = "black"))
  switch(base,
    ggbetweenstats = c(centre, list(
      violin.args = list(width = 0.55, alpha = 0.2, linewidth = 0.3,
        colour = if (grey) "black" else "grey40", na.rm = TRUE),
      boxplot.args = list(width = 0.22, alpha = 0, linewidth = 0.35, na.rm = TRUE)
    )),
    ggwithinstats = centre,
    list()
  )
}

# Palette (unless the user chose one), font of in-panel text, APA p-values.
.finish <- function(plot, given) {
  palette <- getOption("BetterGG.palette", "nejm")
  if (!any(c("palette", "package") %in% given)) {
    # greyscale: fill the violins in grey so groups differ by shade
    if (identical(palette, "grey")) plot <- .tint_violins(plot, alpha = 0.6)
    plot <- .apply_palette(plot, palette)
  }
  if (".bgg_styled" %in% given && !identical(bgg_font(), "")) plot <- .set_family(plot, bgg_font())
  if (isTRUE(getOption("BetterGG.apa", TRUE))) plot <- apa_p(plot)
  .side_counts_unlabelled(.crosses_under(plot))
}

# ggscatterstats()'s marginal histograms label their counts on axes about 1 cm
# long, where the tick labels collide. Drop those count breaks (as the recipe does).
.side_counts_unlabelled <- function(plot) {
  one <- function(p) {
    if (!inherits(p, c("ggside", "ggside::ggside")) || !requireNamespace("ggside", quietly = TRUE)) return(p)
    suppressMessages(p + ggside::scale_xsidey_continuous(breaks = NULL) +
      ggside::scale_ysidex_continuous(breaks = NULL))
  }
  if (inherits(plot, "patchwork")) plot$patches$plots <- lapply(plot$patches$plots, one)
  one(plot)
}

# ggcorrmat() draws its crosses (non-significant cells) in black over the
# coefficients, hiding them. Draw them in grey, under the numbers.
.crosses_under <- function(plot) {
  one <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    geom <- vapply(p$layers, function(l) class(l$geom)[1], "")
    text <- which(geom == "GeomText")
    cross <- which(geom == "GeomPoint" & vapply(p$layers, function(l) {
      isTRUE(l$aes_params$shape %in% c(4, "cross"))
    }, TRUE))
    if (!"GeomTile" %in% geom || length(text) != 1L || length(cross) != 1L || cross < text) return(p)
    old <- p$layers[[cross]] # a variable: ggproto() resolves its parent lazily
    l <- ggplot2::ggproto(NULL, old)
    l$aes_params$colour <- "grey60"
    p$layers <- append(p$layers[-cross], list(l), after = text - 1L)
    p
  }
  if (inherits(plot, "patchwork")) plot$patches$plots <- lapply(plot$patches$plots, one)
  one(plot)
}

#' 'ggstatsplot' functions with APA styling
#'
#' Same arguments and statistics as the 'ggstatsplot' function of the same
#' name (see its help page, e.g. [ggstatsplot::ggbetweenstats()]), with three
#' defaults changed, each of which you can switch off:
#'
#' * p-values in APA style ([apa_p()]); `options(BetterGG.apa = FALSE)`.
#' * the [theme_apa()] theme, unless you pass `ggtheme =`;
#'   `options(BetterGG.theme = FALSE)`.
#' * the NEJM palette ([bgg_palette()]), unless you pass `palette =`;
#'   `options(BetterGG.palette = "grey")`, `"okabe_ito"` or `"viridis"` to
#'   change it, `NULL` to keep 'ggstatsplot''s palette.
#'
#' 'ggstatsplot''s `ggbetweenstats()` always runs Welch's test and ignores
#' `var.equal = TRUE`; BetterGG warns when it is given. [recipe_betweenrain()]
#' honours it (Student's t / Fisher's ANOVA).
#'
#' @param ... Passed to the 'ggstatsplot' function of the same name.
#' @return A `ggplot` (or `patchwork`) object.
#' @name ggstatsplot-apa
#' @examples
#' library(BetterGG)
#' ggbetweenstats(mtcars, cyl, mpg)
NULL

#' @rdname ggstatsplot-apa
#' @export
ggbetweenstats <- .wrap("ggbetweenstats")
#' @rdname ggstatsplot-apa
#' @export
ggwithinstats <- .wrap("ggwithinstats")
#' @rdname ggstatsplot-apa
#' @export
ggscatterstats <- .wrap("ggscatterstats")
#' @rdname ggstatsplot-apa
#' @export
gghistostats <- .wrap("gghistostats")
#' @rdname ggstatsplot-apa
#' @export
ggdotplotstats <- .wrap("ggdotplotstats")
#' @rdname ggstatsplot-apa
#' @export
ggpiestats <- .wrap("ggpiestats")
#' @rdname ggstatsplot-apa
#' @export
ggbarstats <- .wrap("ggbarstats")
#' @rdname ggstatsplot-apa
#' @export
ggcoefstats <- .wrap("ggcoefstats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggbetweenstats <- .wrap("grouped_ggbetweenstats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggwithinstats <- .wrap("grouped_ggwithinstats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggscatterstats <- .wrap("grouped_ggscatterstats")
#' @rdname ggstatsplot-apa
#' @export
grouped_gghistostats <- .wrap("grouped_gghistostats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggdotplotstats <- .wrap("grouped_ggdotplotstats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggpiestats <- .wrap("grouped_ggpiestats")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggbarstats <- .wrap("grouped_ggbarstats")
#' @rdname ggstatsplot-apa
#' @export
ggcorrmat <- .wrap("ggcorrmat")
#' @rdname ggstatsplot-apa
#' @export
grouped_ggcorrmat <- .wrap("grouped_ggcorrmat")

# Fill ggstatsplot's (empty) violins in the group colour, and give the plot a
# fill scale cloned from its colour scale so the palette swap can recolour it.
.tint_violins <- function(plot, alpha) {
  one <- function(p) {
    if (!inherits(p, "ggplot") || !any(vapply(p$layers, function(l) inherits(l$geom, "GeomViolin"), TRUE))) {
      return(p)
    }
    x <- p$mapping$x
    p$layers <- lapply(p$layers, function(l) {
      if (!inherits(l$geom, "GeomViolin")) return(l)
      n <- ggplot2::ggproto(NULL, l)
      keep <- as.list(l$mapping)
      keep$fill <- x
      n$mapping <- ggplot2::aes(!!!keep)
      n$aes_params$alpha <- alpha
      n
    })
    .add_fill_scale(p)
  }
  if (inherits(plot, "patchwork")) plot$patches$plots <- lapply(plot$patches$plots, one)
  one(plot)
}

# A separate fill scale with the colour scale's settings (only if missing).
.add_fill_scale <- function(p) {
  sc <- p$scales$get_scales("colour")
  if (is.null(sc) || !is.null(p$scales$get_scales("fill"))) return(p)
  f <- sc$clone()
  f$aesthetics <- "fill"
  suppressMessages(p + f)
}
