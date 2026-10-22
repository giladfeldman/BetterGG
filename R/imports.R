# beeswarm is used at run time only through ggdist: dots layout = "swarm"
# (between-subjects rainclouds) needs it, and ggdist only suggests it. This
# reference declares the use without importing beeswarm into the namespace,
# so BetterGG's namespace still loads with rlang alone (jmv2gg's in-browser
# converter relies on that).
#
# ggstatsplot stays in Depends, not imported: library(BetterGG) attaches it,
# and the wrappers call it with ggstatsplot::. R CMD check's NOTE "Package in
# Depends field not imported from: 'ggstatsplot'" is expected for this design.
.swarm_layout_needs <- function() beeswarm::swarmx
