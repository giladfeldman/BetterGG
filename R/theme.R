#' APA-style ggplot2 theme
#'
#' White background, no grid lines, thin black axes, plain (not bold) axis
#' titles, and the statistical results (subtitle and caption) centred over the
#' plot panel. Uses Trebuchet MS at 12.5 pt when it is installed (Windows and
#' macOS ship it), otherwise the device's sans-serif font. It is the default
#' `ggtheme` of every BetterGG plot; pass `ggtheme =` yourself, or set
#' `options(BetterGG.theme = FALSE)`, to keep 'ggstatsplot''s theme.
#'
#' @param base_size Base font size in points (default 12.5).
#' @param base_family Font family; default [bgg_font()].
#' @return A ggplot2 theme.
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point() + theme_apa()
#' @export
theme_apa <- function(base_size = 12.5, base_family = bgg_font()) {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(0.88)),
      axis.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(0.95)),
      axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(0.72), colour = "grey35"),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.35),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.35),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15), hjust = 0),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.92), colour = "black", hjust = 0.5),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.8), colour = "black", hjust = 0.5),
      plot.title.position = "panel",
      plot.caption.position = "panel",
      legend.title = ggplot2::element_text(face = "plain"),
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

#' The BetterGG font
#'
#' `"Trebuchet MS"` if it is installed, otherwise `""` (the device's
#' sans-serif font). Change it with `options(BetterGG.font = "Arial")`.
#'
#' Calibri is not recommended: at some sizes the 'ragg' PNG/TIFF devices
#' measure its subscripts as millions of inches high, so plots with
#' 'ggstatsplot''s formula-style subtitles fail ("Viewport has zero
#' dimension(s)"). [save_figure()] detects such a font and draws that file
#' with R's Cairo device instead, with a warning.
#'
#' R's base [grDevices::pdf()] and [grDevices::postscript()] devices cannot
#' use system fonts, so the first call registers the font there as an alias
#' of Helvetica (only if you have not registered it yourself). Plots then
#' draw on those devices in Helvetica instead of failing; every other device,
#' and [save_figure()], uses the real font.
#'
#' @return A font family name.
#' @export
bgg_font <- function() {
  want <- getOption("BetterGG.font", "Trebuchet MS")
  if (identical(want, "")) return("")
  if (!want %in% systemfonts::system_fonts()$family) return("")
  .alias_for_base_pdf(want)
  want
}

# R's base pdf() and postscript() devices only know their own Type 1 font
# table, so a plot naming a system font they lack stops with "failed to find
# or load PDF CID font". That device is the default for Rscript (Rplots.pdf)
# and for R CMD check examples. Map the family to Helvetica there, only if the
# user has not mapped it already; every other device (screen, png, ragg,
# cairo_pdf, save_figure) still draws the real font.
.alias_for_base_pdf <- function(family) {
  if (!family %in% names(grDevices::pdfFonts())) {
    spec <- list(grDevices::pdfFonts()$Helvetica)
    names(spec) <- family
    do.call(grDevices::pdfFonts, spec)
  }
  if (!family %in% names(grDevices::postscriptFonts())) {
    spec <- list(grDevices::postscriptFonts()$Helvetica)
    names(spec) <- family
    do.call(grDevices::postscriptFonts, spec)
  }
  # The Windows screen and png() devices (knitr's default on Windows) look
  # fonts up in their own table: register the family under its own name.
  if (.Platform$OS.type == "windows" && !family %in% names(grDevices::windowsFonts())) {
    spec <- list(grDevices::windowsFont(family))
    names(spec) <- family
    do.call(grDevices::windowsFonts, spec)
  }
  invisible(family)
}

#' BetterGG palettes
#'
#' * `"nejm"` (default): the New England Journal of Medicine palette (from
#'   the 'ggsci' collection via 'paletteer'); 8 colours.
#' * `"grey"`: greyscale for black-and-white print. Outlines and points use
#'   darker greys (`type = "colour"`) than the fills (`type = "fill"`), so
#'   points stay visible on filled violins and bars; 6 levels.
#' * `"okabe_ito"`: the Okabe-Ito colourblind-safe palette; 7 colours.
#' * `"viridis"`: perceptually uniform and colourblind-safe; any number.
#'
#' Asking for more colours than a palette has is an error, never a silent
#' repeat (a repeated colour would make two groups look the same).
#'
#' @param name Palette name.
#' @param n Number of colours.
#' @param type `"colour"` (points, lines, outlines) or `"fill"` (areas). Only
#'   `"grey"` differs between the two.
#' @return A character vector of hex colours.
#' @examples
#' bgg_palette("nejm", 3)
#' bgg_palette("grey", 3, type = "fill")
#' @export
bgg_palette <- function(name = c("nejm", "grey", "okabe_ito", "viridis"), n,
                        type = c("colour", "fill")) {
  name <- match.arg(name)
  type <- match.arg(type)
  cols <- switch(name,
    # the ggsci "default_nejm" palette (as paletteer::paletteer_d() returns it)
    nejm = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
    grey = if (type == "colour") {
      c("#000000", "#555555", "#8C8C8C", "#2B2B2B", "#707070", "#A8A8A8")
    } else {
      c("#5E5E5E", "#A6A6A6", "#DCDCDC", "#7F7F7F", "#C2C2C2", "#EFEFEF")
    },
    okabe_ito = {
      # base R order: black, orange, sky blue, bluish green, yellow, blue,
      # vermillion, reddish purple, grey. Drop black and grey; yellow last
      # (lowest contrast on white).
      oi <- unname(grDevices::palette.colors(palette = "Okabe-Ito"))
      oi[c(2, 3, 4, 6, 7, 8, 5)]
    },
    viridis = viridisLite::viridis(n, end = 0.9)
  )
  if (anyNA(cols)) stop("BetterGG: palette '", name, "' returned missing colours.", call. = FALSE)
  if (n > length(cols)) {
    stop("BetterGG: palette '", name, "' has ", length(cols), " colours but ", n,
      " are needed; use \"viridis\".", call. = FALSE)
  }
  cols[seq_len(n)]
}

#' Discrete colour and fill scales with a BetterGG palette
#'
#' @param palette Palette name, see [bgg_palette()].
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A list of two ggplot2 scales (colour and fill).
#' @export
scale_colour_bgg <- function(palette = "nejm", ...) {
  list(
    ggplot2::discrete_scale("colour", palette = function(n) bgg_palette(palette, n, "colour"), ...),
    ggplot2::discrete_scale("fill", palette = function(n) bgg_palette(palette, n, "fill"), ...)
  )
}

# Replace the discrete colour/fill scales that ggstatsplot itself created
# (via paletteer) with the BetterGG palette, keeping each scale's other
# settings. A scale the user added (e.g. through `ggplot.component`) is left
# alone. In grouped plots the colours are assigned by group NAME across all
# panels, so a group keeps one colour even when it is missing from a panel.
.apply_palette <- function(plot, palette) {
  if (is.null(palette)) return(plot)
  panels <- if (inherits(plot, "patchwork")) c(plot$patches$plots, list(plot)) else list(plot)
  named <- list()
  if (length(panels) > 1L) {
    for (aes in c("colour", "fill")) {
      lv <- .merge_orders(lapply(panels, .scale_limits, aes = aes))
      if (length(lv)) named[[aes]] <- stats::setNames(bgg_palette(palette, length(lv), aes), lv)
    }
  }
  recolour <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    for (aes in c("colour", "fill")) {
      sc <- p$scales$get_scales(aes)
      if (is.null(sc) || !sc$is_discrete() || !.from_ggstatsplot(sc)) next
      new <- sc$clone()
      pal <- named[[aes]]
      new$palette <- if (is.null(pal)) function(n) bgg_palette(palette, n, aes) else function(n) pal
      p <- suppressMessages(p + new)
    }
    p
  }
  if (inherits(plot, "patchwork") && length(plot$patches$plots)) {
    plot$patches$plots <- lapply(plot$patches$plots, recolour)
  }
  recolour(plot)
}

.from_ggstatsplot <- function(sc) {
  any(grepl("paletteer", deparse(sc$call), fixed = TRUE))
}

# Levels shown by a panel's discrete colour/fill scale (in display order).
.scale_limits <- function(p, aes) {
  if (!inherits(p, "ggplot")) return(NULL)
  sc <- p$scales$get_scales(aes)
  if (is.null(sc) || !sc$is_discrete() || !.from_ggstatsplot(sc)) return(NULL)
  p$patches <- NULL
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  b$plot$scales$get_scales(aes)$get_limits()
}

# Merge each panel's level order into one order: a level missing from the
# first panel goes right after the level that precedes it in a panel that has
# it (or first, if nothing precedes it). Panels 6,8 and 4,6,8 give 4,6,8.
.merge_orders <- function(orders) {
  out <- character()
  for (o in orders) {
    o <- as.character(o)
    for (i in seq_along(o)) {
      if (o[i] %in% out) next
      prev <- if (i > 1L) match(o[i - 1L], out) else 0L
      out <- append(out, o[i], after = if (is.na(prev)) length(out) else prev)
    }
  }
  out
}
