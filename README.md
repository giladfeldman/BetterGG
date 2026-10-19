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

## Install (from this folder)

```r
install.packages(".", repos = NULL, type = "source")
```

## Planned

1. APA p-values (done)
2. Raincloud plots with the ggstatsplot look and statistics subtitle
3. Publication-ready colour palettes and fonts
