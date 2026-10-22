#' BetterGG code recipes
#'
#' Recipes write readable, self-contained 'ggplot2' code that draws the same
#' figure as a BetterGG plot. They never look at data: give them the name of a
#' data frame and the names of its columns, and they return R code as text.
#' The code runs without BetterGG installed (it needs 'ggplot2', plus 'ggdist',
#' 'ggrepel', 'ggside' or 'patchwork' for some plots, and 'statsExpressions'
#' only when `stats = TRUE`).
#'
#' * [recipe_setup()]: the figure style (font, palette, theme) the other
#'   recipes use. Run it once before them; each recipe includes it when
#'   `setup = TRUE`.
#' * [recipe_betweenrain()], [recipe_withinrain()], [recipe_factorialrain()]:
#'   rainclouds for group comparisons.
#' * [recipe_histostats()], [recipe_scatterstats()], [recipe_corrmat()],
#'   [recipe_barstats()], [recipe_piestats()], [recipe_coefstats()],
#'   [recipe_reliability()].
#'
#' Statistics on the figure are off by default (`stats = FALSE`). With
#' `stats = TRUE` the code recomputes them with 'statsExpressions', the
#' package 'ggstatsplot' uses, and prints p-values in APA style.
#'
#' Each recipe is tested against the matching BetterGG function: the code is
#' run on data and its layers, values, colours and labels are compared with
#' the real BetterGG plot.
#'
#' @name recipes
NULL

#' Figure style code for the recipes
#'
#' Writes the code that defines `figure_font`, `figure_colours`,
#' `figure_fills` and `theme_figure()`, the BetterGG look ([theme_apa()] and
#' [bgg_palette()]) written out in full. With `stats = TRUE` it also defines
#' `figure_apa_p()`, which rewrites the p-value in a 'statsExpressions' label
#' in APA style from the exact p-value.
#'
#' @param palette A [bgg_palette()] name; the colours are written into the
#'   code as hex values.
#' @param n_colours How many palette colours to write (default 8; viridis
#'   needs a number).
#' @param font Preferred font family; the code falls back to the device's
#'   sans-serif font when it is not installed.
#' @param base_size Base font size in points.
#' @param stats Also write the APA p-value helper.
#' @return R code as one character string.
#' @examples
#' cat(recipe_setup())
#' @export
recipe_setup <- function(palette = getOption("BetterGG.palette", "nejm"), n_colours = 8L,
                         font = getOption("BetterGG.font", "Trebuchet MS"),
                         base_size = 12.5, stats = FALSE) {
  palette <- palette %||% "nejm"
  if (!is.character(font) || length(font) != 1L) stop("BetterGG recipe: 'font' must be one name.", call. = FALSE)
  max_n <- switch(palette, nejm = 8L, grey = 6L, okabe_ito = 7L, n_colours)
  n <- min(n_colours, max_n)
  colours <- bgg_palette(palette, n, "colour")
  fills <- bgg_palette(palette, n, "fill")
  code <- .code(
    "# Figure style: the BetterGG look, written out so it runs without BetterGG.",
    paste0("figure_font <- ", .lit(font)),
    "if (!nzchar(figure_font) || !requireNamespace(\"systemfonts\", quietly = TRUE) ||",
    "    !figure_font %in% systemfonts::system_fonts()$family) {",
    "  figure_font <- \"\" # the device's sans-serif font",
    "}",
    "# R's base pdf() and postscript() devices cannot use system fonts: draw the",
    "# font as Helvetica there instead of failing.",
    "if (nzchar(figure_font) && !figure_font %in% names(grDevices::pdfFonts())) {",
    "  do.call(grDevices::pdfFonts, stats::setNames(list(grDevices::pdfFonts()$Helvetica), figure_font))",
    "}",
    "if (nzchar(figure_font) && !figure_font %in% names(grDevices::postscriptFonts())) {",
    "  do.call(grDevices::postscriptFonts, stats::setNames(list(grDevices::postscriptFonts()$Helvetica), figure_font))",
    "}",
    "# The Windows screen and png() devices (knitr's default there) need the font",
    "# registered by name.",
    "if (nzchar(figure_font) && .Platform$OS.type == \"windows\" && !figure_font %in% names(grDevices::windowsFonts())) {",
    "  do.call(grDevices::windowsFonts, stats::setNames(list(grDevices::windowsFont(figure_font)), figure_font))",
    "}",
    paste0("# Palette: ", palette, " (points, lines and outlines; then areas)."),
    paste0("figure_colours <- ", .vec(colours)),
    paste0("figure_fills <- ", .vec(fills)),
    "",
    "# APA-style theme: white background, no grid, thin black axes, plain axis",
    "# titles, statistics centred over the panel.",
    paste0("theme_figure <- function(base_size = ", .num(base_size), ") {"),
    "  ggplot2::theme_classic(base_size = base_size, base_family = figure_font) +",
    "    ggplot2::theme(",
    "      text = ggplot2::element_text(colour = \"black\"),",
    "      axis.text = ggplot2::element_text(colour = \"black\", size = ggplot2::rel(0.88)),",
    "      axis.title = ggplot2::element_text(face = \"plain\", size = ggplot2::rel(0.95)),",
    "      axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(0.72), colour = \"grey35\"),",
    "      axis.line = ggplot2::element_line(colour = \"black\", linewidth = 0.35),",
    "      axis.ticks = ggplot2::element_line(colour = \"black\", linewidth = 0.35),",
    "      plot.title = ggplot2::element_text(face = \"bold\", size = ggplot2::rel(1.15), hjust = 0),",
    "      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.92), colour = \"black\", hjust = 0.5),",
    "      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.8), colour = \"black\", hjust = 0.5),",
    "      plot.title.position = \"panel\",",
    "      plot.caption.position = \"panel\",",
    "      legend.title = ggplot2::element_text(face = \"plain\"),",
    "      legend.key = ggplot2::element_blank(),",
    "      strip.background = ggplot2::element_blank(),",
    "      strip.text = ggplot2::element_text(face = \"bold\", hjust = 0)",
    "    )",
    "}"
  )
  if (isTRUE(stats)) code <- .code(code, "", .apa_p_helper_code())
  code
}

.apa_p_helper_code <- function() {
  .code(
    "# APA p-values in a statsExpressions label: three decimals, no leading zero,",
    "# \"< .001\" below .001. `p` is the exact p-value of the same test.",
    "figure_apa_p <- function(label, p) {",
    "  if (is.character(label)) label <- str2lang(label)",
    "  if (is.null(p) || is.na(p)) return(label)",
    "  op <- if (p < 0.001) \"<\" else if (p > 0.999) \">\" else \"==\"",
    "  shown <- if (p < 0.001) \".001\" else if (p > 0.999) \".999\" else",
    "    sub(\"^0[.]\", \".\", formatC(p, format = \"f\", digits = 3))",
    "  is_p <- function(e) {",
    "    if (is.call(e) && identical(e[[1]], as.name(\"[\"))) e <- e[[2]]",
    "    identical(e, quote(italic(p)))",
    "  }",
    "  swap <- function(e) {",
    "    if (!is.call(e)) return(e)",
    "    if (identical(e[[1]], as.name(\"==\")) && length(e) == 3L && is_p(e[[2]])) {",
    "      return(as.call(list(as.name(op), e[[2]], shown)))",
    "    }",
    "    for (i in seq_along(e)[-1]) e[[i]] <- swap(e[[i]])",
    "    e",
    "  }",
    "  swap(label)",
    "}"
  )
}

# Setup code prepended when a recipe is asked to be self-contained.
.with_setup <- function(code, setup, stats) {
  if (isTRUE(setup)) .code(recipe_setup(stats = stats), "", code) else code
}
