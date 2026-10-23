# BetterGG 0.2.1

- Recipe figures with `stats = TRUE` never print a statistic as NA. When a test cannot be computed
  from the data (an empty cell of the design, a term confounded with another, a variable with no
  variation), its line reads "not estimable from these data" and a warning names the terms; a
  statsExpressions label that contains NA is replaced by "Statistics not shown: the test could not
  be computed from these data." Inf is kept, since a one-sided confidence interval ends at Inf. The
  check is `figure_statistics()`, written by `recipe_setup(stats = TRUE)`; every recipe's subtitle
  goes through it, and `recipe_factorialrain()` now writes the statistics helpers in its setup too.
- `recipe_barstats()` and `recipe_piestats()` take `cramers_v = "uncorrected"` to print Cramer's V
  as jamovi and SPSS report it, `sqrt(chi^2 / (n * (k - 1)))` from the same test and without an
  interval. The default, `"bias-corrected"`, is unchanged and matches `ggbarstats()`, which prints
  the bias-corrected V (statsExpressions uses `effectsize::cramers_v(adjust = TRUE)`); on small
  tables the two differ noticeably (for example .00 corrected against .16 uncorrected).
- Bayesian rainclouds (`type = "bayes"`) no longer warn "Unknown or uninitialised column:
  `p.value`" (a Bayesian test has no p-value). With three or more groups the pairwise note says
  no bars are drawn for Bayes factors, instead of naming a significance level.

# BetterGG 0.2.0

- Code recipes: `recipe_betweenrain()`, `recipe_withinrain()`, `recipe_factorialrain()`,
  `recipe_histostats()`, `recipe_scatterstats()`, `recipe_corrmat()`, `recipe_barstats()`,
  `recipe_piestats()`, `recipe_coefstats()`, `recipe_reliability()` and `recipe_setup()` write
  readable, self-contained ggplot2 code that draws the same figure as the BetterGG plot. The code
  runs without BetterGG. Statistics on recipe figures are off by default; with `stats = TRUE` they are
  recomputed with 'statsExpressions' and printed with APA p-values. Round-trip tests compare each
  recipe's built layers, values, colours and labels with the real plot (with ggstatsplot 1.1.3; with
  1.1.2 the within-subject clouds and the bar charts are built slightly differently).
- New plots: `ggfactorialrain()` for factorial designs and covariates (model-based means and
  confidence intervals; exact type III F tests via `drop1()`, not `emmeans::joint_tests()`, which
  stores F rounded to three decimals) and `ggreliability()` (item responses, item-rest correlations,
  Cronbach's alpha, jamovi's reverse scoring).
- Long facet titles in faceted recipes and `ggfactorialrain()` wrap at 30 characters instead of
  being clipped at the panel edge.
- `apa_p()` no longer warns about the significance threshold in `ggcorrmat()` captions.
- The BetterGG font is registered for the Windows screen and `png()` devices (knitr's default on
  Windows), which removes "font family not found" warnings in knitted reports.
- The NEJM colours are written out (identical to the ggsci palette, tested), so recipes need no
  palette package.
- Requires ggstatsplot >= 1.1.2 (was 1.1.3) so BetterGG installs in WebR; its own tests pass with
  1.1.2.

# BetterGG 0.1.0

- Format p-values from exact ggstatsplot results with explicit mismatch warnings.
- Add between- and within-subject raincloud plots and grouped variants.
- Provide APA presentation defaults, palette controls, and publication exports.
- Avoid extra random draws when constructing styling defaults; test statistical
  results against ggstatsplot under matching seeds.
- Document grouped argument abbreviations, Windows font rendering, and random
  stream limitations.

- Make font export tests use installed font identities across operating systems.
  Test missing-default fallback explicitly, with no skipped font assertions.

Prepared release candidate; publication and remote installation checks are pending.
