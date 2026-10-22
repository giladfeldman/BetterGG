# Lines 'styler' (tidyverse style) would change in each piece of code. It
# runs in a separate R process: loading styler also loads R.utils, which
# breaks jamovi's option objects for the rest of a session.
styler_changes <- function(codes) {
  callr::r(function(codes) {
    lapply(codes, function(code) {
      lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
      styled <- as.character(suppressMessages(styler::style_text(lines)))
      if (identical(styled, lines)) return(character())
      if (length(styled) != length(lines)) {
        return(c(sprintf("line count %d -> %d", length(lines), length(styled)), utils::head(setdiff(styled, lines), 6)))
      }
      changed <- which(styled != lines)
      paste0(changed, ": ", lines[changed], "  =>  ", styled[changed])
    })
  }, args = list(codes = codes))
}
