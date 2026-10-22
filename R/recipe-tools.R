# Small helpers that write R code as text. Recipes never look at data: they
# take column and object names and return code that runs on its own.

# A column name can also be given as code that gives the name when the code
# runs, wrapped in I(), e.g. I("factors[1]"): for designs whose columns are
# only known from the data (jamovi's ANCOVA formula rule).
.is_code <- function(x) inherits(x, "AsIs")

# A string literal, e.g. "my label" (or the code itself for I()).
.lit <- function(x) {
  if (.is_code(x)) return(as.character(unclass(x)))
  encodeString(as.character(x), quote = '"')
}

# A column or object name usable in code: bare when syntactic, otherwise
# backticked (`my column`); .data[[code]] for I().
.sym <- function(x) {
  if (.is_code(x)) return(paste0(".data[[", as.character(unclass(x)), "]]"))
  x <- as.character(x)
  ok <- make.names(x) == x & !x %in% c("TRUE", "FALSE", "NULL", "NA", "Inf", "NaN")
  ifelse(ok, x, paste0("`", gsub("`", "\\\\`", x), "`"))
}

# c("a", "b") as code. `x` may be a list mixing names and I() code.
.vec <- function(x) {
  if (!length(x)) return("character()")
  if (.is_code(x)) return(as.character(unclass(x)))
  pieces <- if (is.list(x)) vapply(x, .lit, character(1)) else .lit(x)
  if (length(pieces) == 1L && is.list(x) && .is_code(x[[1]])) return(pieces)
  paste0("c(", paste(pieces, collapse = ", "), ")")
}

# A number as code (finite, shortest exact form).
.num <- function(x) {
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop("BetterGG recipe: expected one finite number.", call. = FALSE)
  }
  format(x, digits = 15)
}

# Fill {key} placeholders in a code template.
.fill <- function(template, ...) {
  values <- list(...)
  for (key in names(values)) {
    template <- gsub(paste0("{", key, "}"), values[[key]], template, fixed = TRUE)
  }
  if (grepl("\\{[a-z_]+\\}", template)) {
    stop("BetterGG recipe: unfilled placeholder in ", template, call. = FALSE)
  }
  template
}

.code <- function(...) paste(c(...), collapse = "\n")

# Every recipe argument naming a column or object must be one plain string.
.check_names <- function(...) {
  args <- list(...)
  for (nm in names(args)) {
    v <- args[[nm]]
    if (is.null(v)) next
    if (!is.character(v) || anyNA(v) || !all(nzchar(v))) {
      stop("BetterGG recipe: '", nm, "' must be column or object name(s) as text.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.check_name1 <- function(...) {
  args <- list(...)
  .check_names(...)
  for (nm in names(args)) {
    if (!is.null(args[[nm]]) && length(args[[nm]]) != 1L) {
      stop("BetterGG recipe: '", nm, "' must be one name.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The object name prefix every recipe uses for its intermediate objects, e.g.
# "anova_1" gives anova_1_data, anova_1_plot.
.check_prefix <- function(name) {
  if (!is.character(name) || length(name) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    stop("BetterGG recipe: 'name' must start with a letter and use only letters, digits and _.",
      call. = FALSE)
  }
  invisible(TRUE)
}

.check_conf <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L || !(conf.level > 0 && conf.level < 1)) {
    stop("BetterGG recipe: 'conf.level' must be between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

.centrality_word <- function(type) {
  switch(type,
    parametric = "mean",
    nonparametric = "median",
    robust = "trimmed",
    stop("BetterGG recipe: centrality for type '", type, "' is not available; ",
      "use \"parametric\", \"nonparametric\" or \"robust\".", call. = FALSE)
  )
}

# The rule of statsExpressions::extract_stats_type(), which ggstatsplot uses:
# by the first letter, case-sensitive ("np" and "nonparametric" are
# nonparametric; anything not starting with n, r or b is parametric, so
# "Non-parametric" is parametric there too).
.stats_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop("BetterGG recipe: 'type' must be one string.", call. = FALSE)
  }
  if (grepl("^n", type)) "nonparametric" else if (grepl("^r", type)) "robust" else
    if (grepl("^b", type)) "bayes" else "parametric"
}

# Code computing one centrality value from a numeric vector named `values`.
.centrality_code <- function(type, tr) {
  switch(type,
    parametric = "mean(values)",
    nonparametric = "stats::median(values)",
    robust = paste0("mean(values, trim = ", .num(tr), ")")
  )
}
