# BetterGG

An add-on for [ggstatsplot](https://github.com/IndrajeetPatil/ggstatsplot).
It loads on top of ggstatsplot and gives you the same plotting functions with
APA-style p-values. ggstatsplot itself stays untouched, so every update to it
reaches you as soon as you update it.

## What it changes

| ggstatsplot shows | BetterGG shows |
|---|---|
| *p* = 8.14e-09 | *p* < .001 |
| *p* = 0.05 (true value .0456) | *p* = .046 |
| *p*<sub>Holm-adj.</sub> = 1.10e-03 | *p*<sub>Holm-adj.</sub> = .001 |

Covered: the subtitle of every plot, the pairwise-comparison brackets, the
per-slice proportion tests in pie and bar charts, and the coefficient labels in
`ggcoefstats()`. Grouped plots (`grouped_*`) are handled panel by panel.

Each APA value is computed from the **exact** p-value ggstatsplot stored with
the plot, not from the rounded text on the plot. Before replacing a value,
BetterGG checks that the exact value rounds to what ggstatsplot displayed. If
it does not, that label is left as it was and you get a warning.

## Use

```r
library(ggstatsplot)
library(BetterGG)        # load after ggstatsplot

ggbetweenstats(mtcars, cyl, mpg)   # same arguments as always

apa_p(some_existing_plot)          # convert a plot you already made
options(BetterGG.apa = FALSE)      # back to the original ggstatsplot output
```

## Raincloud plots

```r
ggbetweenrain(mtcars, cyl, mpg)                       # same arguments as ggbetweenstats()
ggwithinrain(bugs_long, condition, desire, subject.id = subject)   # as ggwithinstats()
grouped_ggbetweenrain(mtcars, cyl, mpg, grouping.var = am)
ggbetweenrain(mtcars, cyl, mpg, orientation = "horizontal")
```

ggstatsplot builds the plot (subtitle, Bayesian caption, pairwise brackets,
means); BetterGG only replaces the violin, box and points with a half-density
cloud, a narrow box and the raw data as rain. Every number on the plot is the
one ggstatsplot reports for the same call. With two within-subject conditions
the first is mirrored so each subject's line joins the two inner rains.
Tune the parts with `cloud.args`, `box.args`, `rain.args`.

## Publication style

Every BetterGG plot also gets, by default:

- **`theme_apa()`**: white background, no grid lines, thin black axes, plain
  axis titles, the statistical results centred over the plot, Trebuchet MS
  at 12.5 pt (falls back to the system sans-serif font if missing; change it
  with `options(BetterGG.font = "Arial")`). Pass `ggtheme =` to use your own
  theme, or `options(BetterGG.theme = FALSE)`.
- **NEJM colours** (New England Journal of Medicine palette), a black mean
  marker, softer points and thin outlines.
- **Greyscale for print**: `options(BetterGG.palette = "grey")` gives dark
  grey points and outlines over lighter grey fills, so groups stay distinct
  in black-and-white. Other palettes: `"okabe_ito"`, `"viridis"`; `NULL`
  keeps ggstatsplot's colours. Pass `palette =` to use your own.

The tests compare statistical results against ggstatsplot under matching seeds,
and check that the random-number stream after a `ggbetweenstats()`,
`grouped_ggbetweenstats()` or `ggwithinstats()` call, and the jittered point
positions, are the same as with plain ggstatsplot. Record exact dependencies when
reproducing an analysis.

Save at journal size with embedded fonts:

```r
save_figure(plot, "figure1.pdf", width = "double")              # 7.2 in, vector, fonts embedded
save_figure(plot, "figure1.png", width = "single", dpi = 600)   # 3.5 in raster
save_figure(plot, "figure1.tiff", width = "onehalf", family = "Arial")
```

## Code recipes

Each BetterGG plot has a recipe that writes the same figure as plain, readable
ggplot2 code, which runs without BetterGG. Recipes never look at your data: give
them the names of the data frame and its columns.

```r
code <- recipe_betweenrain("mtcars", "cyl", "mpg", name = "mpg_by_cyl")
cat(code)          # paste into a script or an R Markdown chunk
```

Recipes exist for `ggbetweenrain()`, `ggwithinrain()`, `ggfactorialrain()`,
`gghistostats()`, `ggscatterstats()`, `ggcorrmat()`, `ggbarstats()`,
`ggpiestats()`, `ggcoefstats()` and `ggreliability()`; `recipe_setup()` writes the
shared style (font, palette, APA theme). Statistics are off by default; with
`stats = TRUE` they are recomputed with 'statsExpressions' in the code. The
tests run every recipe on data and compare its layers, values, colours and labels
with the real BetterGG plot.

## Factorial designs and reliability

`ggfactorialrain()` shows two or three factors (x axis, colour, panels) and
covariates: every observation, its distribution, and the estimated marginal
means with confidence intervals of the full factorial model (sum-to-zero
contrasts; `stats = TRUE` adds the type III F tests). `ggreliability()` shows the
responses to each item and each item's correlation with the other items, with
Cronbach's alpha and jamovi's reverse scoring.

## Install

Requires R >= 4.5 and ggstatsplot >= 1.1.2.

```r
install.packages("remotes")
remotes::install_github("giladfeldman/BetterGG@v0.2.2", upgrade = "never")
```

To install a downloaded source checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

## Included

1. APA p-values from exact computed values
2. Between- and within-subject raincloud plots
3. Publication colour palettes, fonts, and figure export
4. Code recipes for every plot (0.2.0)
5. Factorial-design and reliability plots (0.2.0)

## Reproducibility and validation

BetterGG delegates calculations to the installed ggstatsplot version. Record
`sessionInfo()` with your analysis and retain exact dependency versions. Future
ggstatsplot updates can change results or formatting; a BetterGG tag alone does
not freeze its dependencies. Bayesian/bootstrap comparisons require matching
random seeds.

The test suite checks exact-value APA formatting, statistical identity under
matching seeds, argument forwarding, raincloud geometry, palettes, fonts, and
exports. Run `Rscript tools/check.R` followed by `R CMD build .` and
`R CMD check --no-manual BetterGG_0.2.2.tar.gz` in native R. In WebR the
recipes and the plots work (they are used by the jmv2gg web app), except
ggstatsplot's parametric and nonparametric pairwise tests: their PMCMRplus
dependency needs Rmpfr, which WebR does not have.

## Known limitations

- Use full argument names such as `palette =` and `ggtheme =` in grouped
  wrappers. Abbreviated names forwarded through `...` can be replaced by the
  defaults.
- Font rendering depends on installed fonts and the graphics device. Check
  journal-size exports after changing `family` or the default font option.
- Very light greyscale rain dots may need manual contrast adjustment.
- This release formats p-values. It does not remove leading zeroes from every
  other bounded statistic.
