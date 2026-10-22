# Run recipe code on data and compare its built layers with a real BetterGG
# plot: same geoms in the same order, and the same values in every column
# both builds share (positions, fills, colours, labels, ...).
run_recipe <- function(code, env = new.env(parent = globalenv()), ...) {
  extra <- list(...)
  for (nm in names(extra)) assign(nm, extra[[nm]], envir = env)
  eval(parse(text = code, keep.source = FALSE), envir = env)
  env
}

.normalise_column <- function(x) {
  if (is.list(x) || is.expression(x)) {
    return(vapply(x, function(e) {
      if (is.null(e)) return("NULL")
      if (is.character(e)) e <- tryCatch(str2lang(e), error = function(err) e)
      paste(deparse(e, width.cutoff = 500L), collapse = "")
    }, character(1), USE.NAMES = FALSE))
  }
  if (is.factor(x)) return(as.character(x))
  if (is.character(x)) {
    parsed <- vapply(x, function(e) {
      if (is.na(e)) return(NA_character_)
      out <- tryCatch(paste(deparse(str2lang(e), width.cutoff = 500L), collapse = ""), error = function(err) e)
      if (startsWith(e, "list(") || grepl("==", e, fixed = TRUE)) out else e
    }, character(1), USE.NAMES = FALSE)
    return(parsed)
  }
  as.vector(unclass(x))
}

# Returns a character vector describing every difference (empty = identical).
compare_builds <- function(real, recipe, skip_columns = character(), layers = NULL, skip = list()) {
  rb <- suppressWarnings(ggplot2::ggplot_build(real))
  mb <- suppressWarnings(ggplot2::ggplot_build(recipe))
  out <- character()
  rg <- unname(vapply(real$layers, function(l) class(l$geom)[1], ""))
  mg <- unname(vapply(recipe$layers, function(l) class(l$geom)[1], ""))
  if (!identical(rg, mg)) return(paste("layers differ:", paste(rg, collapse = ","), "vs", paste(mg, collapse = ",")))
  for (i in layers %||% seq_along(rb$data)) {
    a <- rb$data[[i]]
    b <- mb$data[[i]]
    if (nrow(a) != nrow(b)) {
      out <- c(out, sprintf("layer %d (%s): %d vs %d rows", i, rg[i], nrow(a), nrow(b)))
      next
    }
    missing <- setdiff(names(a), c(names(b), skip_columns))
    if (length(missing)) out <- c(out, sprintf("layer %d (%s): missing %s", i, rg[i], paste(missing, collapse = ",")))
    for (col in setdiff(intersect(names(a), names(b)), c(skip_columns, skip[[as.character(i)]]))) {
      x <- .normalise_column(a[[col]])
      y <- .normalise_column(b[[col]])
      if (!isTRUE(all.equal(x, y, check.attributes = FALSE, tolerance = 1e-10))) {
        out <- c(out, sprintf("layer %d (%s): column %s differs", i, rg[i], col))
      }
    }
  }
  for (aes in c("x", "y")) {
    ra <- tryCatch(rb$layout$panel_params[[1]][[aes]]$get_labels(), error = function(e) NULL)
    ma <- tryCatch(mb$layout$panel_params[[1]][[aes]]$get_labels(), error = function(e) NULL)
    if (!identical(.normalise_column(ra %||% character()), .normalise_column(ma %||% character()))) {
      out <- c(out, paste("axis labels differ:", aes))
    }
  }
  for (lab in c("x", "y", "title", "subtitle", "caption")) {
    ra <- real$labels[[lab]]
    ma <- recipe$labels[[lab]]
    if (is.expression(ra)) ra <- ra[[1]]
    if (is.expression(ma)) ma <- ma[[1]]
    if (!identical(.normalise_column(list(ra %||% "")), .normalise_column(list(ma %||% "")))) {
      out <- c(out, paste("label differs:", lab))
    }
  }
  out
}

expect_recipe_matches <- function(real, recipe, ...) {
  diffs <- compare_builds(real, recipe, ...)
  testthat::expect(length(diffs) == 0L, paste(c("recipe differs from BetterGG:", diffs), collapse = "\n  "))
  invisible(diffs)
}
