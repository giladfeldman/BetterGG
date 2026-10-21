# Exercise real installed fonts instead of assuming Microsoft fonts on every OS.
test_font_family <- function(kind = c("sans", "serif")) {
  kind <- match.arg(kind)
  choices <- if (kind == "sans") c("Arial", "Liberation Sans", "DejaVu Sans", "Nimbus Sans")
    else c("Times New Roman", "Liberation Serif", "DejaVu Serif", "Nimbus Roman")
  installed <- unique(systemfonts::system_fonts()$family)
  available <- choices[choices %in% installed]
  if (!length(available)) stop("Font export tests need an installed sans/serif font.")
  available[[1L]]
}

test_font_name <- function(family) systemfonts::font_info(family = family)$name[[1L]]