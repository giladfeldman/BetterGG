#' Save a figure at journal size with embedded fonts
#'
#' Widths follow the common journal column sizes: `"single"` 3.5 in (89 mm),
#' `"onehalf"` 5.5 in (140 mm), `"double"` 7.2 in (183 mm); or give a number
#' in inches. The file type comes from the extension:
#'
#' * `.pdf`: vector, fonts embedded (via `grDevices::cairo_pdf`).
#' * `.svg`: vector (via `grDevices::svg`, Cairo).
#' * `.png`, `.tiff`, `.jpg`: raster at `dpi` (default 600) via the 'ragg'
#'   package, which uses the installed system fonts exactly. A font whose
#'   subscripts 'ragg' mis-measures (Calibri at some sizes) is drawn with R's
#'   Cairo device instead, with a warning.
#'
#' @param plot A ggplot or patchwork object.
#' @param file Output path.
#' @param width `"single"`, `"onehalf"`, `"double"`, or a width in inches.
#' @param height Height in inches; default keeps a 4:3 ratio.
#' @param dpi Resolution for raster formats (default 600).
#' @param family Font family to use for all text, e.g. `"Arial"`; `NULL`
#'   keeps the plot's own font.
#' @return The file path, invisibly.
#' @export
save_figure <- function(plot, file, width = "double", height = NULL, dpi = 600, family = NULL) {
  sizes <- c(single = 3.5, onehalf = 5.5, double = 7.2)
  w <- if (is.character(width)) sizes[[match.arg(width, names(sizes))]] else width
  h <- height %||% (w * 3 / 4)
  if (!is.null(family)) plot <- .set_family(plot, family)
  ext <- tolower(tools::file_ext(file))
  device <- switch(ext,
    pdf = grDevices::cairo_pdf,
    svg = grDevices::svg,
    png = ragg::agg_png,
    tiff = , tif = ragg::agg_tiff,
    jpg = , jpeg = ragg::agg_jpeg,
    stop("BetterGG: unsupported file type '.", ext, "'.", call. = FALSE)
  )
  font <- family %||% .plot_family(plot)
  if (ext %in% c("png", "tiff", "tif", "jpg", "jpeg") && !.ragg_plotmath_ok(font, dpi)) {
    warning("BetterGG: 'ragg' measures subscripts in '", font, "' as millions of inches ",
      "(the plot area would collapse), so this ", ext, " is drawn with R's Cairo device instead.",
      call. = FALSE)
    device <- .cairo_raster(if (ext %in% c("tif", "tiff")) "tiff" else if (ext == "png") "png" else "jpeg")
  }
  ggplot2::ggsave(file, plot, device = device, width = w, height = h, units = "in", dpi = dpi)
  invisible(file)
}

# The font family a plot's theme text uses ("" = the device default).
.plot_family <- function(plot) {
  if (inherits(plot, "patchwork") && !inherits(plot, "ggplot")) plot <- plot$patches$plots[[1]]
  fam <- tryCatch(ggplot2::calc_element("text", ggplot2::theme_get() + plot$theme)$family,
    error = function(e) NULL)
  fam %||% ""
}

# Some fonts (Calibri on Windows) return absurd plotmath metrics in 'ragg' at
# some pixel sizes: a subscript such as CI["95%"] in 12 pt at 110 dpi measures
# about 15 million inches high, which collapses the panel of a ggstatsplot plot
# with a formula subtitle ("Viewport has zero dimension(s)"). Which sizes fail
# varies erratically, so measure every text size a figure uses (5-20 pt in
# quarter points) at the resolution being written. Cached per family and dpi.
.ragg_plotmath_ok <- function(family, dpi = 600) {
  if (is.null(family) || !nzchar(family)) return(TRUE)
  key <- paste(family, dpi)
  if (!is.null(.bgg_cache[[key]])) return(.bgg_cache[[key]])
  old <- grDevices::dev.cur()
  ragg::agg_png(tempfile(fileext = ".png"), 100, 100, res = dpi)
  on.exit({ grDevices::dev.off(); if (old > 1) grDevices::dev.set(old) })
  h <- vapply(seq(5, 20, by = 0.25), function(size) {
    g <- grid::textGrob(expression(CI["95%"]), gp = grid::gpar(fontfamily = family, fontsize = size))
    grid::convertHeight(grid::grobHeight(g), "in", valueOnly = TRUE)
  }, 0)
  ok <- all(is.finite(h) & h < 1)
  assign(key, ok, envir = .bgg_cache)
  ok
}
.bgg_cache <- new.env(parent = emptyenv())

# R's own Cairo raster devices, called the way ggsave() calls a device.
.cairo_raster <- function(kind) {
  dev <- switch(kind, png = grDevices::png, tiff = grDevices::tiff, jpeg = grDevices::jpeg)
  function(filename, width, height, res = 600, units = "in", bg = "white", ...) {
    dev(filename, width = width, height = height, units = units, res = res, bg = bg, type = "cairo")
  }
}

# One font everywhere: theme text (titles, axes, subtitle) and the text drawn
# inside the panel by geoms (centrality labels, bracket labels, pie labels).
.set_family <- function(plot, family) {
  one <- function(p) {
    if (!inherits(p, "ggplot")) return(p)
    p$layers <- lapply(p$layers, function(l) {
      takes_family <- "family" %in% c(names(l$geom$default_aes), l$geom$parameters(TRUE))
      if (!takes_family) return(l)
      new <- ggplot2::ggproto(NULL, l)
      new$aes_params <- utils::modifyList(l$aes_params, list(family = family))
      if ("family" %in% names(l$geom_params)) new$geom_params$family <- family
      new
    })
    p + ggplot2::theme(text = ggplot2::element_text(family = family))
  }
  if (inherits(plot, "patchwork")) {
    plot$patches$plots <- lapply(plot$patches$plots, one)
    plot <- plot & ggplot2::theme(text = ggplot2::element_text(family = family))
  }
  one(plot)
}
