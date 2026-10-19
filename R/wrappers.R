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
  if (isTRUE(getOption("BetterGG.theme", TRUE)) && !"ggtheme" %in% given) {
    call$ggtheme <- theme_apa()
  }
  list(plot = eval(call, env), given = given)
}

# Palette (unless the user chose one) and APA p-values.
.finish <- function(plot, given) {
  if (!any(c("palette", "package") %in% given)) {
    plot <- .apply_palette(plot, getOption("BetterGG.palette", "okabe_ito"))
  }
  if (isTRUE(getOption("BetterGG.apa", TRUE))) plot <- apa_p(plot)
  plot
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
#' * the colourblind-safe Okabe-Ito palette ([bgg_palette()]), unless you pass
#'   `palette =`; `options(BetterGG.palette = "viridis")` or `"grey"` to
#'   change it, `NULL` to keep 'ggstatsplot''s palette.
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
