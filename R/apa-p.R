#' Format p-values in APA style
#'
#' Three decimals without a leading zero (`.046`), `< .001` below .001, and
#' `> .999` above .999.
#'
#' @param p Numeric vector of p-values.
#' @param digits Number of decimals (default 3).
#' @return A list with `op` (`"=="`, `"<"` or `">"`) and `value` (character),
#'   one element per p-value.
#' @examples
#' apa_p_value(c(0.0456, 0.0004, 0.9999))
#' @export
apa_p_value <- function(p, digits = 3L) {
  floor_p <- 10^-digits
  lapply(p, function(x) {
    if (is.na(x)) return(list(op = "==", value = "NA"))
    if (x < floor_p) {
      return(list(op = "<", value = .no_lead_zero(formatC(floor_p, format = "f", digits = digits))))
    }
    if (x > 1 - floor_p) {
      return(list(op = ">", value = .no_lead_zero(formatC(1 - floor_p, format = "f", digits = digits))))
    }
    list(op = "==", value = .no_lead_zero(formatC(x, format = "f", digits = digits)))
  })
}

.no_lead_zero <- function(s) sub("^0\\.", ".", s)

#' Convert the p-values on a 'ggstatsplot' plot to APA style
#'
#' Rewrites every `p = ...` shown on the plot: the subtitle, the pairwise
#' comparison brackets, and per-row labels (pie/bar proportion tests,
#' regression coefficient labels). Grouped plots are handled panel by panel.
#'
#' Each new value is computed from the exact p-value that 'ggstatsplot' stored
#' with the plot. Before replacing, the function checks that this exact value
#' rounds to the text currently shown; if it does not (or cannot be found),
#' that label is left unchanged and a warning names it.
#'
#' @param plot A plot returned by a 'ggstatsplot' function.
#' @param digits Number of decimals (default 3).
#' @return The same plot with APA-style p-values.
#' @export
apa_p <- function(plot, digits = 3L) {
  if (inherits(plot, "patchwork") && length(plot$patches$plots)) {
    plot$patches$plots <- lapply(plot$patches$plots, apa_p, digits = digits)
  }
  if (!inherits(plot, "ggplot")) return(plot)

  stats <- ggstatsplot::extract_stats(plot)
  if (inherits(plot, "patchwork")) stats <- .stats_of_single(plot)

  # 1. subtitle (and caption, in case a frequentist test sits there)
  for (slot in c("subtitle", "caption")) {
    lab <- plot$labels[[slot]]
    src <- stats[[paste0(slot, "_data")]]
    if (!is.null(lab) && .has_p(lab)) {
      raw <- if (!is.null(src) && "p.value" %in% names(src)) src$p.value[1] else NA_real_
      plot$labels[[slot]] <- .rewrite(lab, raw, digits, where = slot)
    }
  }

  # 2. layers: pairwise brackets and per-row labels
  mpc <- stats$pairwise_comparisons_data
  plot$layers <- lapply(plot$layers, function(layer) {
    ann <- layer$stat_params$annotations
    data <- layer$data
    touch_ann <- !is.null(ann) && any(vapply(ann, .has_p, logical(1)))
    touch_data <- is.data.frame(data) && "p.value" %in% names(data) &&
      any(vapply(data, function(col) any(vapply(col, .has_p, logical(1))), logical(1)))
    if (!touch_ann && !touch_data) return(layer)

    new <- ggplot2::ggproto(NULL, layer) # copy: layers are shared by reference
    if (touch_ann) {
      raws <- .bracket_p(layer$stat_params$comparisons, mpc, length(ann))
      params <- layer$stat_params
      params$annotations <- vapply(
        seq_along(ann),
        function(i) .rewrite(ann[[i]], raws[i], digits, where = "pairwise comparison"),
        character(1)
      )
      new$stat_params <- params
    }
    if (touch_data) {
      for (col in names(data)) {
        if (!any(vapply(data[[col]], .has_p, logical(1)))) next
        out <- lapply(seq_len(nrow(data)), function(i) {
          .rewrite(data[[col]][[i]], data$p.value[i], digits, where = paste0("label (", col, ")"))
        })
        data[[col]] <- if (is.character(data[[col]])) unlist(out) else out
      }
      new$data <- data
    }
    new
  })
  plot
}

# Exact p-value for each bracket. ggstatsplot draws brackets only for the
# comparisons it displays (by default the significant ones), so match each
# bracket to its row by the two group names, never by position.
.bracket_p <- function(comparisons, mpc, n) {
  if (is.null(mpc) || is.null(comparisons) || length(comparisons) != n) return(rep(NA_real_, n))
  g1 <- as.character(mpc$group1)
  g2 <- as.character(mpc$group2)
  vapply(comparisons, function(cmp) {
    cmp <- as.character(cmp)
    hit <- which((g1 == cmp[1] & g2 == cmp[2]) | (g1 == cmp[2] & g2 == cmp[1]))
    if (length(hit) == 1L) mpc$p.value[hit] else NA_real_
  }, numeric(1))
}

# extract_stats() on a patchwork maps over all panels; we want only the top panel
.stats_of_single <- function(plot) {
  env <- plot$plot_env
  pick <- function(nm) env[[nm]] %||% env[["stats_output"]][[nm]]
  list(
    subtitle_data = pick("subtitle_df"),
    caption_data = pick("caption_df"),
    pairwise_comparisons_data = pick("mpc_df")
  )
}

.has_p <- function(x) {
  if (is.null(x)) return(FALSE)
  s <- if (is.character(x)) x else paste(deparse(x), collapse = "")
  any(grepl("italic(p)", s, fixed = TRUE))
}

# Rewrite one label. `x` is a language object or a character string to parse.
.rewrite <- function(x, raw, digits, where) {
  was_char <- is.character(x)
  expr <- if (was_char) str2lang(x) else x
  state <- new.env()
  state$ok <- TRUE
  out <- .walk(expr, raw, digits, state)
  if (!state$ok) {
    warning(
      "BetterGG: left the p-value in the ", where, " unchanged (shown value ",
      state$shown, " does not match the stored exact p-value ", format(raw), ").",
      call. = FALSE
    )
    return(x)
  }
  if (was_char) paste(deparse(out, width.cutoff = 500L), collapse = "") else out
}

.is_p_symbol <- function(lhs) {
  # italic(p) or italic(p)[subscript]
  if (is.call(lhs) && identical(lhs[[1]], as.name("["))) lhs <- lhs[[2]]
  is.call(lhs) && identical(lhs[[1]], as.name("italic")) && length(lhs) == 2L &&
    identical(lhs[[2]], as.name("p"))
}

.walk <- function(e, raw, digits, state) {
  if (!is.call(e)) return(e)
  if (identical(e[[1]], as.name("==")) && length(e) == 3L && .is_p_symbol(e[[2]]) &&
    is.character(e[[3]])) {
    shown <- e[[3]]
    state$shown <- shown
    if (!.consistent(raw, shown)) {
      state$ok <- FALSE
      return(e)
    }
    f <- apa_p_value(raw, digits)[[1]]
    return(as.call(list(as.name(f$op), e[[2]], f$value)))
  }
  for (i in seq_along(e)[-1]) e[[i]] <- .walk(e[[i]], raw, digits, state)
  e
}

# Does the exact p-value round to the text that ggstatsplot displayed?
.consistent <- function(raw, shown) {
  if (is.null(raw) || length(raw) != 1L || is.na(raw)) return(FALSE)
  num <- suppressWarnings(as.numeric(shown))
  if (is.na(num)) return(FALSE)
  if (grepl("e", shown, ignore.case = TRUE)) {
    parts <- strsplit(tolower(shown), "e", fixed = TRUE)[[1]]
    dec <- nchar(sub("^[^.]*\\.?", "", parts[1]))
    tol <- 0.5 * 10^(as.numeric(parts[2]) - dec)
  } else {
    dec <- nchar(sub("^[^.]*\\.?", "", shown))
    tol <- 0.5 * 10^(-dec)
  }
  # ggstatsplot shows p = 0 as the smallest positive double
  if (raw == 0) raw <- .Machine$double.xmin
  abs(raw - num) <= tol * (1 + 1e-8)
}
