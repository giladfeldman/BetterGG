# Each wrapper looks up the 'ggstatsplot' function when it is called, not when
# BetterGG is installed, so updating 'ggstatsplot' is enough to get its fixes.
.wrap <- function(name) {
  force(name)
  function(...) {
    plot <- getExportedValue("ggstatsplot", name)(...)
    if (isTRUE(getOption("BetterGG.apa", TRUE))) apa_p(plot) else plot
  }
}

#' 'ggstatsplot' functions with APA-style p-values
#'
#' Same arguments and output as the 'ggstatsplot' function of the same name
#' (see its help page, e.g. [ggstatsplot::ggbetweenstats()]), with the plot
#' passed through [apa_p()]. Set `options(BetterGG.apa = FALSE)` to get the
#' unchanged 'ggstatsplot' output.
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
