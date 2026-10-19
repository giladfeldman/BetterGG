#' APA-style ggplot2 theme
#'
#' White background, no grid lines, black axis lines and text, a sans-serif
#' font, and plain (not bold) axis titles, following the APA 7 figure
#' guidelines (sans-serif font, 8 to 14 pt, no background shading). It is
#' the default `ggtheme` of every BetterGG plot; pass `ggtheme =` yourself,
#' or set `options(BetterGG.theme = FALSE)`, to keep 'ggstatsplot''s theme.
#'
#' @param base_size Base font size in points (default 11).
#' @param base_family Font family. `""` (default) is the device's sans-serif
#'   font (Arial on Windows, Helvetica on macOS). Any installed font name
#'   works with [save_figure()].
#' @return A ggplot2 theme.
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() + theme_apa()
#' @export
theme_apa <- function(base_size = 11, base_family = "") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(face = "plain"),
      axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(0.75), colour = "grey30"),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.4),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.1), hjust = 0),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85), hjust = 0),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75), colour = "grey30"),
      plot.title.position = "plot",
      legend.title = ggplot2::element_text(face = "plain"),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

#' Colourblind-safe and print-safe palettes
#'
#' * `"okabe_ito"` (default): the Okabe-Ito palette, distinguishable with all
#'   common forms of colour-vision deficiency. Taken from base R's
#'   `palette.colors("Okabe-Ito")` without its black and grey; 7 colours.
#' * `"viridis"`: perceptually uniform, colourblind-safe, and ordered from dark
#'   to light, so it also survives greyscale printing; any number of colours.
#' * `"grey"`: greys for black-and-white print; any number of colours.
#'
#' Asking for more colours than a palette has is an error, never a silent
#' repeat (a repeated colour would make two groups look the same).
#'
#' @param name Palette name.
#' @param n Number of colours.
#' @return A character vector of hex colours.
#' @examples
#' bgg_palette("okabe_ito", 3)
#' @export
bgg_palette <- function(name = c("okabe_ito", "viridis", "grey"), n) {
  name <- match.arg(name)
  cols <- switch(name,
    okabe_ito = {
      # base R order: black, orange, sky blue, bluish green, yellow, blue,
      # vermillion, reddish purple, grey. Drop black and grey; yellow last
      # (lowest contrast on white).
      oi <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
      oi[c(2, 3, 4, 6, 7, 8, 5)]
    },
    viridis = viridisLite::viridis(n, end = 0.9),
    grey = grDevices::grey.colors(n, start = 0.15, end = 0.7)
  )
  if (anyNA(cols)) stop("BetterGG: palette '", name, "' returned missing colours.", call. = FALSE)
  if (n > length(cols)) {
    stop("BetterGG: palette '", name, "' has ", length(cols), " colours but ", n,
      " are needed; use \"viridis\" or \"grey\".", call. = FALSE)
  }
  cols[seq_len(n)]
}

#' Discrete colour and fill scales with a BetterGG palette
#'
#' @param palette Palette name, see [bgg_palette()].
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale applied to both `colour` and `fill`.
#' @export
scale_colour_bgg <- function(palette = "okabe_ito", ...) {
  ggplot2::discrete_scale(
    aesthetics = c("colour", "fill"),
    palette = function(n) bgg_palette(palette, n),
    ...
  )
}

# Replace the plot's discrete colour/fill scales with the BetterGG palette,
# keeping each scale's other settings (name, labels, guide).
.apply_palette <- function(plot, palette) {
  if (is.null(palette)) return(plot)
  if (inherits(plot, "patchwork") && length(plot$patches$plots)) {
    plot$patches$plots <- lapply(plot$patches$plots, .apply_palette, palette = palette)
  }
  if (!inherits(plot, "ggplot")) return(plot)
  for (aes in c("colour", "fill")) {
    sc <- plot$scales$get_scales(aes)
    if (is.null(sc) || !sc$is_discrete()) next
    new <- sc$clone()
    new$palette <- function(n) bgg_palette(palette, n)
    plot <- suppressMessages(plot + new)
  }
  plot
}
