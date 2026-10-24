#' Raincloud plots with 'ggstatsplot' statistics
#'
#' `ggbetweenrain()` and `ggwithinrain()` take the same arguments as
#' [ggstatsplot::ggbetweenstats()] and [ggstatsplot::ggwithinstats()] and
#' return the same plot (subtitle, Bayesian caption, pairwise brackets,
#' centrality labels, theme) with the violin, box and points redrawn as a
#' raincloud: a half-density "cloud", a narrow box, and the raw data as "rain".
#'
#' The statistics are not recomputed here: 'ggstatsplot' builds the plot and
#' only its drawing layers are replaced, so every number on the plot is the
#' number 'ggstatsplot' reports for the same call.
#'
#' @param ... Passed to [ggstatsplot::ggbetweenstats()] /
#'   [ggstatsplot::ggwithinstats()].
#' @param cloud.args,box.args,rain.args Lists of arguments for the cloud
#'   ([ggdist::stat_slab()]), the box ([ggplot2::geom_boxplot()]) and the rain
#'   ([ggdist::stat_dots()] for between-subjects, in a beeswarm layout that
#'   keeps every dot at its exact value; [ggplot2::geom_point()] for
#'   within-subjects, where the points stay linked by their subject lines).
#' @param orientation `"vertical"` (default, like 'ggstatsplot') or
#'   `"horizontal"` (groups on the y axis, clouds above the rain).
#' @return A `ggplot` object.
#' @examples
#' ggbetweenrain(mtcars, cyl, mpg)
#' ggwithinrain(ggstatsplot::bugs_long, condition, desire, subject.id = subject)
#' @export
ggbetweenrain <- function(..., cloud.args = list(), box.args = list(),
                          rain.args = list(), orientation = c("vertical", "horizontal")) {
  .check_own_args(names(rlang::enquos(...)), .rain_own_args)
  res <- .call_ggstatsplot("ggbetweenstats", sys.call(), parent.frame(), drop = .rain_own_args)
  plot <- .to_raincloud(res$plot, paired = FALSE, cloud.args, box.args, rain.args, match.arg(orientation))
  .finish(plot, res$given)
}

#' @rdname ggbetweenrain
#' @export
ggwithinrain <- function(..., cloud.args = list(), box.args = list(),
                         rain.args = list(), orientation = c("vertical", "horizontal")) {
  .check_own_args(names(rlang::enquos(...)), .rain_own_args)
  res <- .call_ggstatsplot("ggwithinstats", sys.call(), parent.frame(), drop = .rain_own_args)
  plot <- .to_raincloud(res$plot, paired = TRUE, cloud.args, box.args, rain.args, match.arg(orientation))
  .finish(plot, res$given)
}

#' @rdname ggbetweenrain
#' @param data,grouping.var,plotgrid.args,annotation.args As in
#'   [ggstatsplot::grouped_ggbetweenstats()]: one raincloud per level of
#'   `grouping.var`, titled by the level, combined with
#'   [ggstatsplot::combine_plots()].
#' @export
grouped_ggbetweenrain <- function(data, ..., grouping.var, plotgrid.args = list(),
                                  annotation.args = list(), cloud.args = list(), box.args = list(),
                                  rain.args = list(), orientation = c("vertical", "horizontal")) {
  .grouped_rain("ggbetweenstats", FALSE, match.call(), parent.frame(), data,
    rlang::enquo(grouping.var), plotgrid.args, annotation.args,
    cloud.args, box.args, rain.args, match.arg(orientation))
}

#' @rdname ggbetweenrain
#' @export
grouped_ggwithinrain <- function(data, ..., grouping.var, plotgrid.args = list(),
                                 annotation.args = list(), cloud.args = list(), box.args = list(),
                                 rain.args = list(), orientation = c("vertical", "horizontal")) {
  .grouped_rain("ggwithinstats", TRUE, match.call(), parent.frame(), data,
    rlang::enquo(grouping.var), plotgrid.args, annotation.args,
    cloud.args, box.args, rain.args, match.arg(orientation))
}

# One panel per level, titled by the level, combined with
# ggstatsplot::combine_plots() -- the same panel logic as ggstatsplot's
# grouped_* functions. Each panel re-runs the user's own call on that level's
# rows, so tidy-eval arguments keep working.
.grouped_rain <- function(upstream, paired, call, env, data, gv, plotgrid.args,
                          annotation.args, cloud.args, box.args, rain.args, orientation) {
  data <- dplyr::filter(data, !is.na(!!gv))
  levels <- unique(dplyr::pull(data, !!gv))
  levels <- if (is.factor(levels)) levels(droplevels(levels)) else sort(levels)
  drop <- c("grouping.var", "plotgrid.args", "annotation.args", .rain_own_args)
  .check_own_args(names(call), drop)
  given <- NULL
  plots <- lapply(levels, function(lv) {
    cl <- call
    cl$data <- dplyr::filter(data, !!gv == lv)
    cl$title <- as.character(lv)
    res <- .call_ggstatsplot(upstream, cl, env, drop = drop)
    given <<- res$given
    .to_raincloud(res$plot, paired, cloud.args, box.args, rain.args, orientation)
  })
  combined <- ggstatsplot::combine_plots(plots, plotgrid.args = plotgrid.args, annotation.args = annotation.args)
  .finish(combined, given)
}

.default_rain_args <- list(
  cloud = list(adjust = 1, width = 0.5, justification = -0.25, alpha = 0.5, normalize = "groups"),
  box = list(width = 0.12, alpha = 0.6, outlier.shape = NA, na.rm = TRUE),
  rain_between = list(side = "left", justification = 1.2, scale = 0.45, alpha = 0.7, layout = "swarm"),
  rain_within = list(size = 2, alpha = 0.5, na.rm = TRUE)
)

.rain_nudge <- -0.22

.rain_own_args <- c("cloud.args", "box.args", "rain.args", "orientation")

# BetterGG's own arguments come after `...`, where R does not match partial
# names; an abbreviation would otherwise be silently ignored.
.check_own_args <- function(supplied, own) {
  supplied <- setdiff(supplied, c("", own))
  for (nm in supplied) {
    hit <- own[startsWith(own, nm)]
    if (length(hit)) {
      stop("BetterGG: argument '", nm, "' looks like an abbreviation of '", hit[1],
        "'; please write it in full.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' @importFrom rlang .data
NULL

.to_raincloud <- function(plot, paired, cloud.args, box.args, rain.args, orientation) {
  geoms <- vapply(plot$layers, function(l) class(l$geom)[1], "")
  own_data <- vapply(plot$layers, function(l) is.data.frame(l$data), logical(1))

  # ggstatsplot's own drawing layers inherit the plot data; its centrality
  # point/label/path layers carry their own data frame and are kept.
  raw_points <- geoms == "GeomPoint" & !own_data
  raw_paths <- geoms == "GeomPath" & !own_data
  drawing <- geoms %in% c("GeomViolin", "GeomBoxplot") | raw_points
  if (!any(geoms == "GeomViolin") || !any(raw_points)) {
    stop("BetterGG: unexpected ggstatsplot layer structure; cannot build the raincloud.", call. = FALSE)
  }

  x <- plot$mapping$x

  cloud <- list(do.call(
    ggdist::stat_slab,
    c(list(mapping = ggplot2::aes(fill = !!x, group = !!x), colour = NA),
      utils::modifyList(.default_rain_args$cloud, cloud.args))
  ))
  box <- do.call(
    ggplot2::geom_boxplot,
    c(list(mapping = ggplot2::aes(group = !!x)), utils::modifyList(.default_rain_args$box, box.args))
  )

  if (paired) {
    # With two conditions the first one is mirrored (cloud outside on the
    # left, rain on the inside) so each subject's line joins the two inner
    # rains instead of crossing a cloud.
    xname <- rlang::as_name(x)
    lv <- levels(droplevels(factor(plot$data[[xname]])))
    mirror <- length(lv) == 2L
    xpos <- match(as.character(plot$data[[xname]]), lv)
    plot$data$.bgg_rain_x <- xpos + ifelse(mirror & xpos == 1L, -.rain_nudge, .rain_nudge)

    rain <- do.call(
      ggplot2::geom_point,
      c(list(mapping = ggplot2::aes(x = .data$.bgg_rain_x, colour = !!x)),
        utils::modifyList(.default_rain_args$rain_within, rain.args))
    )
    # keep each subject's line attached to its (moved) points
    plot$layers[raw_paths] <- lapply(plot$layers[raw_paths], function(l) {
      new <- ggplot2::ggproto(NULL, l)
      new$mapping <- ggplot2::aes(x = .data$.bgg_rain_x)
      new
    })
    if (mirror) {
      first <- function(d) d[as.character(d[[xname]]) == lv[1], , drop = FALSE]
      rest <- function(d) d[as.character(d[[xname]]) != lv[1], , drop = FALSE]
      # the mirror's side and offset win over user cloud.args
      left_args <- utils::modifyList(
        utils::modifyList(.default_rain_args$cloud, cloud.args),
        list(side = "left", justification = 1.25)
      )
      cloud <- list(
        do.call(ggdist::stat_slab, c(list(mapping = ggplot2::aes(fill = !!x, group = !!x),
          data = first, colour = NA), left_args)),
        do.call(ggdist::stat_slab, c(list(mapping = ggplot2::aes(fill = !!x, group = !!x),
          data = rest, colour = NA), utils::modifyList(.default_rain_args$cloud, cloud.args)))
      )
    }
  } else {
    rain <- do.call(
      ggdist::stat_dots,
      # dots are filled with their outline colour: in greyscale the fill greys are
      # too light for small dots (identical colours for the other palettes)
      # (no fill legend key of its own: with a colour title different from the fill title the
      # key would be drawn without a colour and the staged fill would fail)
      c(list(mapping = ggplot2::aes(colour = !!x, group = !!x, fill = ggplot2::after_scale(.data$colour)),
        show.legend = c(fill = FALSE)),
        utils::modifyList(.default_rain_args$rain_between, rain.args))
    )
  }

  plot$layers <- c(cloud, list(rain, box), plot$layers[!drawing])
  plot <- .fill_like_colour(plot)
  if (orientation == "horizontal") plot <- plot + ggplot2::coord_flip()
  plot
}

# ggstatsplot sets the colour palette only; give the fills their own scale
# with the same settings, so cloud and rain share the group colours (and the
# greyscale option can use lighter fills than outlines).
.fill_like_colour <- function(plot) .add_fill_scale(plot)
