# BetterGG (development version)

- The wrappers warn when 'ggstatsplot' silently draws a figure without the statistics subtitle
  that was asked for (it catches errors in its statistics; with `type = "bayes"` a grouping
  column named `g` is one cause).
- `grouped_ggbetweenstats()` and the other `grouped_*` wrappers recognise an abbreviated
  `pal =` or `ggth =` (as R does when the call reaches the ggstatsplot function). Before, the
  user's palette or theme was drawn over by the BetterGG palette and `theme_apa()`.
- Bayesian rainclouds (`recipe_betweenrain()`/`recipe_withinrain()`, `type = "bayes"`) with three
  or more groups draw a bar for every pair labelled with its Bayes factor, as `ggbetweenstats()`
  does (0.2.1 drew none). A grouping column named `g` or `g_...` no longer stops the figure:
  `performance::r2_bayes()` inside statsExpressions drops such parameters as g-priors and fails,
  so the generated code runs the tests on a copy with the column renamed (statistics identical).
- `ggreliability()` and `recipe_reliability()`: an item with the same value in every row (or
  whose other items sum to a constant) gets an item-rest correlation labelled "not estimable"
  and a warning naming it, instead of "NA"; a constant total score gives "Cronbach's alpha = not
  estimable" instead of NaN.
- `figure_statistics()` also catches typed missing values (`NA_real_`, `NA_integer_`,
  `NA_character_`) in a statistics label.
- Greyscale (`options(BetterGG.palette = "grey")`): the 'ggstatsplot' wrappers now draw points
  and outlines in the darker outline greys. A closure over a loop variable gave the colour scale
  the light fill greys, so outlines were as pale as fills. Raincloud rain dots are filled with
  their outline colour, so the lightest group's dots are no longer faint (identical colours for
  the other palettes, whose fill and outline colours are the same).
  The dots draw no fill legend key of their own, so a raincloud with its own x title draws
  without an "Unable to apply staged modifications" warning.
- Correlation matrices (`ggcorrmat()` and `recipe_corrmat()`): the crosses over non-significant
  cells are grey and drawn under the coefficients, which stay readable.
- `recipe_piestats()` draws no empty axis lines or tick labels, and has one legend guide (title
  and borderless keys).
- Scatter plots (`ggscatterstats()` and `recipe_scatterstats()`): the marginal histograms no
  longer label their counts; on the ~1 cm margins those tick labels collided.
- Faceted `recipe_histostats()`: each panel's mean label sits in headroom above the bars, beside
  its line on the side with more room, instead of over the tallest bar or clipped at the edge.
- `recipe_barstats()` and `recipe_factorialrain()`: with more than six categories, or names
  longer than 60 characters in total, the x labels are angled (bar charts also get room on the
  left for the first label and smaller group-size labels), instead of overlapping.
- `ggbetweenstats()`/`grouped_ggbetweenstats()` leave the random-number stream, and the jittered
  point positions, exactly as plain ggstatsplot does. BetterGG's softer points are now set on
  ggstatsplot's own point layer after the call instead of being passed as `point.args` (whose
  default draws a seed for the jitter); before, the stream after a call was one draw behind.
- `ggbetweenstats()`, `grouped_ggbetweenstats()` and the between-subjects rainclouds warn when
  given `var.equal = TRUE` with a parametric test: 'ggstatsplot' ignores it and draws Welch's test
  (and Games-Howell pairwise comparisons). `recipe_betweenrain(var.equal = TRUE)` honours it.
- `save_figure()` no longer fails on PNG/TIFF/JPEG with fonts whose subscripts 'ragg'
  mis-measures (Calibri at some sizes measured ~15 million inches high, "Viewport has zero
  dimension(s)"). It measures the font at the requested resolution and, if needed, draws that
  file with R's Cairo device instead, with a warning.

# BetterGG 0.2.1

- Recipe figures with `stats = TRUE` never print a statistic as NA. When a test cannot be computed
  from the data (an empty cell of the design, a term confounded with another, a variable with no
  variation), its line reads "not estimable from these data" and a warning names the terms; a
  statsExpressions label that contains NA is replaced by "Statistics not shown: the test could not
  be computed from these data." Inf is kept, since a one-sided confidence interval ends at Inf. The
  check is `figure_statistics()`, written by `recipe_setup(stats = TRUE)`; every recipe's subtitle
  goes through it, and `recipe_factorialrain()` now writes the statistics helpers in its setup too.
- The per-bar goodness-of-fit labels of `recipe_barstats()` and `recipe_piestats()` read "not
  estimable" for a bar with a single category, instead of "p = NA".
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
