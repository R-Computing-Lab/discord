---
title: '{discord}: An R Package for Discordant-Kinship Regressions'
output:
  rticles::joss_article:
  rmarkdown::html_vignette:
    keep_md: TRUE
tags:
  - R
  - behavior genetics
  - discordant kinship modeling
  - difference scores
authors:
  - name: Jonathan D. Trattner
    orcid: 0000-0002-1097-7603
    affiliation: 1 # Maybe?  - name: Yoo Ri Hwang    orcid: 0000-0000-0000-0000    affiliation: 2
  - name: S. Mason Garrison
    orcid: 0000-0002-4804-6003
    affiliation: 2
affiliations:
 - name: Department of Neuroscience, Wake Forest School of Medicine
   index: 1
 - name: Department of Psychology, Wake Forest University
   index: 2
citation_author: Trattner and Garrison
date: "28 September, 2023"
bibliography: paper.bib
csl: apa.csl
journal: JOSS
---

# Summary

As a field, behavior genetics studies the genetic and environmental sources of individual differences in psychological traits and characteristics. More technically, the field focuses on decomposing the sources of phenotypic variation into genetic (Additive (A)+ Dominance (D)) and environmental (Shared Environment (C) + Non-Shared Environment (E)) variance components, by leveraging twin and family studies. These models can do more than merely describe sources of variance; they can be used to infer causation [@burt2019].
Here, we present software to facilitate genetically-informed quasi-experimental designs primarily for kinship modeling.  Specifically, it facilitates discordant-kinship regressions by comparing kin, such as siblings. These designs account for genetic-and-environmental variance when examining causal links in the realm of 'nature vs. nurture.'


<!-- alt 

Behavior genetics involves examining the genetic and environmental sources peoples' behavior. 
Specifically, it leverages genetic and environmental differences to better understand individual variation. Classic methods focus upon description, often by comparing different kinds of twins. 
However, behavior genetics can do so much more than merely compare twins. It can be used to evaluate causal claims using any kind of kinship groups, cousins, siblings, etc. In this package, we provide a series of tools to harness the power of behavior genetics, using publicly available data.

 Although randomized
studies on identical twins are the gold standard to determine causality,
they are not always feasible due to small sample sizes and high costs.
As an alternative, our team has developed a quasi-experimental paradigm
(Garrison & Rodgers, 2016) that allows us to control for
gene-and-environmental variance among kinship pairs (siblings, cousins,
etc.) and shed light on causal relationships arising from the "nature
vs. nurture" debate. The crux of this paradigm relies on the
discordant-kinship model, which in turn requires specifically structured data. 
In addition, given the growing interest in limiting underpowered studies, 
we also include simulation functions so that researcher can create custom simulated data.--> 

# Statement of Need

<!-- `discord` is an R package that provides functions for discordant kinship
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
model coefficients. -->

Kin-comparison designs distinguish "within-family variance" from "between-family variance" [@chamberlain1975]. Within-family variance indicates how individuals of a specific family differ from one another; the between-family variance reflects sources that make family members more similar to one another [@garrison2021 @garrison2016]. By partitioning these sources of variance, scholars may greatly reduce confounds when testing hypotheses [@lahey2010]. Our R package, {discord}, has customizable, efficient code for generating genetically-informed simulations and provides user-friendly functions to help researchers use kin-based quasi-experimental designs. 

{discord} augments the NlsyLinks R package, which provides kinship links for the National Longitudinal Surveys of Youth -- a series of cross-generational, nationally representative surveys of over 30,000 participants [@beasley2016; @rodgers2016]. It has been used in thousands of studies [CITE this database https://nlsinfo.org/bibliography-start]

# Mathematics

<!-- The core of the discordant kinship model can be explained with a
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

-->
To facilitate kinship comparisons, {discord} implements a modified reciprocal standard dyad model [@kenny2006] known as the discordant-kinship model (see @garrison2016 for an extension). Consider the simplified case where a behavioral outcome, $Y$, is predicted by variable, $X$. The discordant-kinship model relates the difference in the outcome, $Y_{i\Delta}$, for the $i\text{th}$ kinship pair, where $\bar{Y}_i$ is the mean level of the outcome, $\bar{X}_i$ is the mean level of the predictor, and $X_{i\Delta}$ is the between-kin difference in the predictor.

$$
Y_{i\Delta} = \beta_0 + \beta_1 \bar{Y}_i + \beta_2 \bar{X}_i + \beta_3 X_{i\Delta} + \epsilon_i
$$

This model partitions variance in line with the above discussion to support causal inference. Specifically, the within-family variance is described by $Y_{\Delta}$ and $X_{\Delta}$; between-family variance is captured by $\bar{Y}$ and $\bar{X}$ [@garrison2021].

A non-significant association between $Y_\Delta$ and $X_\Delta$ suggests that the relationship is not directly causal. Specifically, it means that kin differences in the outcome, after controlling for gene and shared-environmental factors, are not associated with kin differences in the outcome. In contrast, a significant association may provide support for a causal relationship between variables depending on the relatedness of each kin pair. That is, the discordant-kinship model is applicable for any set of kin: monozygotic twins who share 100% of their DNA; full-siblings who share 50%; half-siblings who share 25%; cousins who share 12.5%; etc. Thus, a significant relationship found with monozygotic twins would provide more compelling support for a causal claim than the same relationship between cousins.

Following [@garrison2021], we recommend interpreting significant associations as *not disproving a causal relationship*. Although this design controls for much (sibling) if not all (monozygotic twins) background heterogeneity, it is possible that a significant relationship between a phenotype and plausible covariates is possible due to non-shared environmental influences.

The next section illustrates one example of discordant-kinship regressions with the {discord} package.



# Acknowledgments

We acknowledge contributions from Cermet Ream, Joe Rodgers, and support from Lucy D'Agostino McGowan on this project.

# References
