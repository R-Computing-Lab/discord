---
title: 'discord: Functions for Discordant Kinship Modeling'
tags:
  - kinship modeling
  - behavior genetics
  - causal inference
authors:
  - name: Jonathan D. Trattner
    orcid: 0000-0003-0872-7098
    affiliation: 1
  - name: S. Mason Garrison^[Correspondence should be addressed to <garrissm@wfu.edu>]
    affiliation: 1
affiliations:
 - name: Wake Forest University, Winston-Salem, NC
   index: 1
date: 21 October 2020
bibliography: paper.bib
editor_options: 
  markdown: 
    wrap: 72
---

# Summary

The study of behavior genetics involves examining the interaction
between genes and the environment on peoples' behavior. Although randomized
studies on identical twins are the gold standard to determine causality,
they are not always feasible due to small sample sizes and high costs.
As an alternative, our team has developed a quasi-experimental paradigm
(Garrison & Rodgers, 2016) that allows us to control for
gene-and-environmental variance among kinship pairs (siblings, cousins,
etc.) and shed light on causal relationships arising from the "nature
vs. nurture" debate. The crux of this paradigm relies on the
discordant-kinship model, which in turn requires specifically structured data. 
In addition, given the growing interest in limiting underpowered studies, 
we also include simulation functions so that researcher can create custome simulated data.

# Statement of need

`discord` is an R package that provides functions for discordant kinship
modeling and other sibling-based quasi-experimental designs. It has
highly customizable, efficient code for generating genetically-informed
simulations and provides user-friendly functions to perform
discordant-kinship regressions. It integrates seamlessly with the
NlsyLinks R package, which provides kinship links for the National
Longitudinal Survey of Youth -- a cross-generational, nationally
representative survey of over 30,000 participants for up to 35 years
[@beasley_nlsylinks_2016]. It has been used in previous publications
(cite, Mason, cite!) and supports the principles of tidy data
[@wickham2014] utilizing the broom package [@robinson2020] to report
model coefficients.

# Mathematics

The core of the discordant kinship model can be explained with a
simplistic case where a behavioral outcome $Y$ is predicted by one
variable $X$, the discord regression model relates the difference in
that outcome, $Y_{i\Delta}$, for a given kinship pair, indexed as $i$,
in the following model, where $X_{i\Delta}$ is the difference in the
predictor.

$\mathrm{Y_{i\Delta}} = \beta_{0} + \beta_{1}\mathrm{\bar{Y_{i}}} + \beta_{2}\mathrm{\bar{X_{i}}} + \beta_{3}\mathrm{X_{i\Delta}}$

where,

$\mathrm{Y_{i\Delta}} = \mathrm{Y_{i,1}} - \mathrm{Y_{i,2}}$

$\mathrm{X_{i\Delta}} = \mathrm{X_{i,1}} - \mathrm{X_{i,2}}$

and $1$ and $2$ identify the individuals within the kinship pair,
defined by

$\mathrm{Y_{i,1}} > \mathrm{Y_{i,2}}$

$\mathrm{X_{i,1}} > \mathrm{X_{i,2}}$

# Citations

# Acknowledgements

We acknowledge contributions from Cermet Ream and support from Lucy
D'Agostino McGowan on this project.

# References
