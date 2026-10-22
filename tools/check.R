# Run from the package root in native R; retain every expected context.
if (getRversion() < "4.5.0") stop("BetterGG requires R >= 4.5.0.")
results <- testthat::test_local(".", reporter = "summary", stop_on_failure = FALSE)
checks <- as.data.frame(results)
expected <- c("apa-p", "raincloud", "recipes", "review3", "style")
missing <- setdiff(expected, unique(checks$context))
cat("Checks:", sum(checks$nb), "failures:", sum(checks$failed),
    "errors:", sum(checks$error), "skips:", sum(checks$skipped), "\n")
if (sum(checks$nb) < 148L || length(missing) || any(checks$failed > 0) ||
    any(checks$error) || any(checks$skipped)) {
  if (length(missing)) cat("Missing contexts:", paste(missing, collapse = ", "), "\n")
  stop("BetterGG validation failed; do not lower the test floor.")
}
