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
