
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discord

<!-- badges: start -->

<a href="https://r-computing-lab.github.io/discord/"><img src="man/figures/logo.png" alt="discord website" align="right" height="139"/></a>
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R package
version](https://www.r-pkg.org/badges/version/discord)](https://cran.r-project.org/package=discord)
[![Package
downloads](https://cranlogs.r-pkg.org/badges/grand-total/discord)](https://cran.r-project.org/package=discord)</br>
[![R-CMD-check](https://github.com/R-Computing-Lab/discord/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-Computing-Lab/discord/actions/workflows/R-CMD-check.yaml)
[![Dev Main
branch](https://github.com/R-Computing-Lab/discord/actions/workflows/R-CMD-dev_check.yaml/badge.svg)](https://github.com/R-Computing-Lab/discord/actions/workflows/R-CMD-dev_check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/R-Computing-Lab/discord/graph/badge.svg)](https://app.codecov.io/gh/R-Computing-Lab/discord)
![License](https://img.shields.io/badge/License-GPL_v3-blue.svg)

<!-- badges: end -->

`discord` is an R package that provides functions for discordant kinship
modeling and other sibling-based quasi-experimental designs. It includes
functions for data preparation, regression analysis, and simulation of
genetically-informed data. The package is designed to facilitate the
implementation of discordant sibling designs in research, allowing for
the control of shared familial confounding factors.

Visit the [discord website](https://r-computing-lab.github.io/discord/)
for more information and detailed documentation. Below is a brief
overview of the package, its features, and a roadmap to get you started.

## Quick Start Guide

### Step 1: Install the Package

``` r
# Install from CRAN
install.packages('discord')

# Or install development version from GitHub
# install.packages('devtools')
devtools::install_github('R-Computing-Lab/discord')
```

### Step 2: Choose Your Starting Point

Your workflow depends on your data structure and experience level:

**🚀 New to discordant-kinship regression?** - Start with [Full Data
Workflow](https://r-computing-lab.github.io/discord/articles/full_data_workflow.html) -
Shows all three models (OLS, Between-Family, Discordant) side-by-side -
Demonstrates data preparation from wide, long, or pedigree formats

**📊 Have NLSY data or existing kinship links?** - Use [NLSY Regression
Analysis](https://r-computing-lab.github.io/discord/articles/regression.html) -
Real-world example with flu vaccination and SES data - Complete workflow
from kinship linking to interpretation

**🔧 Need to build kinship links from scratch?** - See [Using discord
with Simple Family
Structures](https://r-computing-lab.github.io/discord/articles/links.html) -
Construct links from basic family IDs (mother, father) - Works without
pre-existing kinship databases

### Step 3: Explore Advanced Topics

Once you understand the basics, explore specialized topics:

- **Categorical predictors** (sex, race): [Categorical Predictors
  Vignette](https://r-computing-lab.github.io/discord/articles/categorical_predictors.html)
- **Visualizing results**: [Plotting
  Vignette](https://r-computing-lab.github.io/discord/articles/plots.html)
- **Sample size planning**: [Power Analysis
  Vignette](https://r-computing-lab.github.io/discord/articles/Power.html)

## Package Features

- **Data Preparation**: Functions to prepare and structure data for
  discordant sibling analysis, including handling of kinship pairs and
  demographic variables.
- **Regression Analysis**: Tools to perform discordant regression
  analyses, allowing for the examination of within-family effects while
  controlling for shared familial confounders.
- **Simulation**: Functions to simulate genetically-informed data,
  enabling researchers to test and validate their models.

## Complete Vignette Roadmap

The package includes several vignettes organized by user needs. All
vignettes can be accessed
[online](https://r-computing-lab.github.io/discord/articles/) or from
the RStudio “Vignettes” tab after package installation.

### 📚 Start Here: Core Workflows

These vignettes provide complete end-to-end examples and should be your
first stop:

- [Full data workflow for
  discord](https://r-computing-lab.github.io/discord/articles/full_data_workflow.html)
  - **What you’ll learn:**
  - How to transform data from wide, long, or pedigree formats
  - How to select siblings for standard OLS regression
  - How to order siblings for discordant-kinship analysis
  - How to run and compare all three model types (OLS, Between-Family,
    Discordant)
  - How to interpret difference scores and mean scores
  - Complete side-by-side model comparisons
- [NLSY regression analysis with
  discord](https://r-computing-lab.github.io/discord/articles/regression.html)
  - Use this vignette if you want an end-to-end applied example that
    links NLSY79 relatives, cleans variables for flu vaccination and
    SES, constructs dyads, and then fits within-family models. You will
    learn how to specify discord_regression correctly and interpret
    coefficients.

### 🔧 Data Preparation

- [No Database? No Problem: Using discord with simple family
  Structures](https://r-computing-lab.github.io/discord/articles/links.html)
  - This vignette is particularly useful for situations when you do not
    have existing kinship links and need to build relationships directly
    from simple family identifiers. It shows how to construct the links,
    optionally simulate phenotypes under specified structures, and fit
    discord_regression with alternative specifications for small or
    bespoke datasets.

### 📊 Advanced Topics

- [Creating plots for
  discord](https://r-computing-lab.github.io/discord/articles/plots.html)
  - This vignette takes fitted discord_regression outputs and produces
    publication-ready ggplot figures of effect estimates and
    within-family contrasts with minimal transformation of the model
    results. It includes complete plotting code paths you can reuse,
    from extracting estimates to saving figures that clearly communicate
    within-family findings.
- [Power Analysis with
  discord](https://r-computing-lab.github.io/discord/articles/power.html)
  - Use this vignette when you need to plan sample sizes or evaluate
    power by running simulation grids that vary effect sizes, kin types,
    and Ns using kinsim, then re-fitting discord_regression under each
    condition. It reports empirical power and writes tidy summaries.
- [Handling categorical predictors with
  discord](https://r-computing-lab.github.io/discord/articles/categorical_predictors.html)
  - This vignette formalizes categorical predictors in discord designs
    by separating categorical variables into within-dyad and
    between-dyad components. It makes the implied contrasts explicit. It
    discusses the pitfalls of interpreting coefficients naively when
    using categorical predictors, and reviews best practices for coding
    and interpretation.

## External Reproducible Examples

Beyond the vignettes, you can find additional examples that fully
reproduce analyses from our other publications (Garrison et al 2025,
etc). These examples can be accessed via the following links and are
presented in reverse chronological order:

- National Longitudinal Survey of Youth (NLSY) datasets
  - [NLSY AMPPS
    repo](https://github.com/R-Computing-Lab/target-causalclaims):
    Reproduces NLSY analyses from Garrison et al 2025, using `targets`
    for workflow management. Garrison, S. M., Trattner, J. D., Lyu, X.,
    Prillaman, H. R., McKinzie, L., Thompson, S. H. E., & Rodgers, J. L.
    (2025). Sibling Models Can Test Causal Claims without Experiments:
    Applications for Psychology.
    <https://doi.org/10.1101/2025.08.25.25334395>

  - [Frontiers
    repo](https://github.com/R-Computing-Lab/Sims-et-al-2024):
    Reproduces Sims, E. E., Trattner, J. D., & Garrison, S. M. (2024).
    Exploring the relationship between depression and delinquency: a
    sibling comparison design using the NLSY. Frontiers in psychology,
    15, 1430978. <https://doi.org/10.3389/fpsyg.2024.1430978>

  - [Intelligence
    repo](https://github.com/R-Computing-Lab/Project_AFI_Intelligence):
    Reproduces Garrison, S. M., & Rodgers, J. L. (2016). Casting doubt
    on the causal link between intelligence and age at first
    intercourse: A cross-generational sibling comparison design using
    the NLSY. Intelligence, 59, 139-156.
    <https://doi.org/10.1016/j.intell.2016.08.008>
- China Family Panel Studies (CFPS) dataset
  - [CFPS AMPPS repo](https://github.com/R-Computing-Lab/discord_CFPS):
    Reproduces analyses from the China Family Panel Studies (CFPS)
    dataset, focusing on the association between adolescent depression
    and math achievement. Garrison, S. M., Trattner, J. D., Lyu, X.,
    Prillaman, H. R., McKinzie, L., Thompson, S. H. E., & Rodgers, J. L.
    (2025). Sibling Models Can Test Causal Claims without Experiments:
    Applications for Psychology.
    <https://doi.org/10.1101/2025.08.25.25334395>

## Citation

If you use `discord` in your research or wish to refer to it, please
cite the following paper:

    To cite package 'discord' in publications use:

      Garrison S, Trattner J, Hwang Y (2025). _discord: Functions for
      Discordant Kinship Modeling_. doi:10.32614/CRAN.package.discord
      <https://doi.org/10.32614/CRAN.package.discord>, R package version
      1.3, <https://github.com/R-Computing-Lab/discord>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {discord: Functions for Discordant Kinship Modeling},
        author = {S. Mason Garrison and Jonathan Trattner and Yoo Ri Hwang},
        note = {R package version 1.3},
        url = {https://github.com/R-Computing-Lab/discord},
        year = {2025},
        doi = {10.32614/CRAN.package.discord},
      }

## Contributing

Contributions to the `discord` project are welcome. For guidelines on
how to contribute, please refer to the [Contributing
Guidelines](https://github.com/R-Computing-Lab/discord/blob/main/CONTRIBUTING.md).
Issues and pull requests should be submitted on the GitHub repository.
For support, please use the GitHub issues page.

## License

`discord` is licensed under the GNU General Public License v3.0. For
more details, see the
[LICENSE](https://github.com/R-Computing-Lab/discord/blob/main/LICENSE)
file.
