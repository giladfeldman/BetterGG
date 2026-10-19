#' Save a figure at journal size with embedded fonts
#'
#' Widths follow the common journal column sizes: `"single"` 3.5 in (89 mm),
#' `"onehalf"` 5.5 in (140 mm), `"double"` 7.2 in (183 mm); or give a number
#' in inches. The file type comes from the extension:
#'
#' * `.pdf`: vector, fonts embedded (via `grDevices::cairo_pdf`).
#' * `.svg`: vector (via `grDevices::svg`, Cairo).
#' * `.png`, `.tiff`, `.jpg`: raster at `dpi` (default 600) via the 'ragg'
#'   package, which uses the installed system fonts exactly.
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
  ggplot2::ggsave(file, plot, device = device, width = w, height = h, units = "in", dpi = dpi)
  invisible(file)
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
