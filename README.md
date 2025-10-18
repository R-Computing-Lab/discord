
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

## What is Discordant-Kinship Regression?

Discordant-kinship regression is a quasi-experimental design that uses differences between siblings (or other kin) to control for unmeasured familial confounders. By comparing siblings who share family background, genes, and early environment, researchers can better assess whether associations reflect causal relationships or are due to shared familial factors.

**Key advantages:**

- Controls for shared genetic and environmental factors without randomization
- Provides stronger evidence for (or against) causal effects than standard regression
- Separates within-family effects from between-family effects

**The approach compares three models:**

1. **Standard OLS**: Individual-level associations (potentially confounded)
2. **Between-Family**: Associations across families using family means
3. **Discordant (Within-Family)**: Associations within families using sibling differences

When within-family effects differ from OLS estimates, it suggests familial confounding. When they persist, it provides stronger evidence for causal effects.

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

**🚀 New to discordant-kinship regression?**
- Start with [Full Data Workflow](https://r-computing-lab.github.io/discord/articles/full_data_workflow.html)
- Shows all three models (OLS, Between-Family, Discordant) side-by-side
- Demonstrates data preparation from wide, long, or pedigree formats

**📊 Have NLSY data or existing kinship links?**
- Use [NLSY Regression Analysis](https://r-computing-lab.github.io/discord/articles/regression.html)
- Real-world example with flu vaccination and SES data
- Complete workflow from kinship linking to interpretation

**🔧 Need to build kinship links from scratch?**
- See [Using discord with Simple Family Structures](https://r-computing-lab.github.io/discord/articles/links.html)
- Construct links from basic family IDs (mother, father)
- Works without pre-existing kinship databases

### Step 3: Explore Advanced Topics

Once you understand the basics, explore specialized topics:

- **Categorical predictors** (sex, race): [Categorical Predictors Vignette](https://r-computing-lab.github.io/discord/articles/categorical_predictors.html)
- **Visualizing results**: [Plotting Vignette](https://r-computing-lab.github.io/discord/articles/plots.html)
- **Sample size planning**: [Power Analysis Vignette](https://r-computing-lab.github.io/discord/articles/Power.html)

## Package Features

- **Data Preparation**: Functions to prepare and structure data for
  discordant sibling analysis, including handling of kinship pairs and
  demographic variables.
- **Regression Analysis**: Tools to perform discordant regression
  analyses, allowing for the examination of within-family effects while
  controlling for shared familial confounders.
- **Simulation**: Functions to simulate genetically-informed data,
  enabling researchers to test and validate their models.

## Core Functions

- `discord_data()`: Prepare sibling pair data with ordering and derived variables (means, differences)
- `discord_regression()`: Fit discordant-kinship regression models in one step
- `kinsim()`: Simulate genetically-informed data for testing and power analysis

## Typical Workflow Example

Here's a minimal example showing the typical workflow:

``` r
library(discord)

# 1. Prepare your data
# Start with wide-format data (one row per sibling pair)
# Columns: pair_id, var_s1, var_s2 (where _s1 and _s2 denote siblings)

# 2. Create discord data with ordering and derived variables
discord_data <- discord_data(
  data = my_data,
  outcome = "outcome_variable",
  predictors = c("predictor1", "predictor2"),
  pair_identifiers = c("_s1", "_s2"),
  id = "pair_id"
)

# This creates:
# - outcome_1, outcome_2 (ordered so outcome_1 >= outcome_2)
# - outcome_mean, outcome_diff (for between and within-family effects)
# - predictor_mean, predictor_diff (for each predictor)

# 3. Run discordant regression
model <- discord_regression(
  data = my_data,
  outcome = "outcome_variable",
  predictors = c("predictor1", "predictor2"),
  pair_identifiers = c("_s1", "_s2"),
  id = "pair_id"
)

# 4. Examine results
summary(model)

# Key coefficients:
# - predictor_mean: Between-family effect
# - predictor_diff: Within-family effect (controls for familial confounding)
```

**Interpreting the results:**

- **Significant `predictor_diff`**: Within-family effect persists after controlling for shared factors → stronger evidence for causal effect
- **Non-significant `predictor_diff` but significant OLS**: Association likely due to familial confounding
- **Similar `predictor_mean` and `predictor_diff`**: Effect operates similarly across and within families

## Complete Vignette Roadmap

The package includes comprehensive vignettes organized by user needs. All vignettes can be accessed [online](https://r-computing-lab.github.io/discord/articles/) or from the RStudio "Vignettes" tab after installation.

### 📚 Start Here: Core Workflows

These vignettes provide complete end-to-end examples and should be your first stop:

#### [Full Data Workflow](https://r-computing-lab.github.io/discord/articles/full_data_workflow.html) - **Start here if you're new!**

**What you'll learn:**
- How to transform data from wide, long, or pedigree formats
- How to select siblings for standard OLS regression
- How to order siblings for discordant-kinship analysis
- How to run and compare all three model types (OLS, Between-Family, Discordant)
- How to interpret difference scores and mean scores
- Complete side-by-side model comparisons

**Perfect for:** First-time users, understanding the methodology, comparing modeling approaches

**Key sections:**
- Visualization of sibling ordering process (before/after tables)
- Explanation of first-born selection vs. outcome-based ordering
- Three-model comparison with interpretation guide

---

#### [NLSY Regression Analysis](https://r-computing-lab.github.io/discord/articles/regression.html) - **Real-world application**

**What you'll learn:**
- Working with NLSY79 kinship links from {NlsyLinks}
- Complete data cleaning and preparation workflow
- Running standard OLS, between-family, and discordant models
- Interpreting results in context of flu vaccination and SES

**Perfect for:** NLSY users, applied researchers, understanding real data challenges

**Key sections:**
- Kinship linking with existing databases
- Data cleaning for consistency
- Three-model comparison with practical interpretation

---

### 🔧 Data Preparation

#### [Using discord with Simple Family Structures](https://r-computing-lab.github.io/discord/articles/links.html)

**What you'll learn:**
- Building kinship links from basic family IDs (mother, father, spouse)
- Working with pedigree data using {BGmisc}
- Simulating phenotypes for testing

**Perfect for:** Users without pre-existing kinship databases, pedigree data users

---

### 📊 Advanced Topics

#### [Handling Categorical Predictors](https://r-computing-lab.github.io/discord/articles/categorical_predictors.html)

**What you'll learn:**
- Binary-match vs. multi-match coding for categorical variables
- Between-dyad vs. mixed variables
- How coding choices affect interpretation

**Perfect for:** Analyzing sex, race, or other categorical variables

---

#### [Creating Plots](https://r-computing-lab.github.io/discord/articles/plots.html)

**What you'll learn:**
- Publication-ready ggplot figures
- Visualizing within-family contrasts
- Extracting and formatting model results

**Perfect for:** Preparing figures for papers or presentations

---

#### [Power Analysis](https://r-computing-lab.github.io/discord/articles/Power.html)

**What you'll learn:**
- Planning sample sizes for discordant designs
- Running simulation grids
- Evaluating power under different scenarios

**Perfect for:** Study design, grant proposals, sample size justification

---

### 📖 Vignette Decision Tree

**Not sure which vignette to start with?**

```
Are you new to discordant-kinship regression?
├─ YES → Start with "Full Data Workflow"
│         (comprehensive introduction with all three models)
│
└─ NO → What's your data source?
    ├─ NLSY dataset → "NLSY Regression Analysis"
    │                  (real-world example with kinship links)
    │
    ├─ Pedigree/family IDs → "Simple Family Structures"
    │                          (build links from scratch)
    │
    └─ Already have prepared data → What do you need?
        ├─ Categorical variables → "Categorical Predictors"
        ├─ Visualizations → "Creating Plots"
        └─ Sample size planning → "Power Analysis"
```
## External Reproducible Examples

Beyond the vignettes, you can find additional examples that fully
reproduce analyses from our other publications (Garrison et al 2025,
etc). These examples can be accessed via the following links:

- National Longitudinal Survey of Youth (NLSY) dataset
  - [Intelligence](https://github.com/R-Computing-Lab/Project_AFI_Intelligence):
    Reproduces Garrison, S. M., & Rodgers, J. L. (2016). Casting doubt
    on the causal link between intelligence and age at first
    intercourse: A cross-generational sibling comparison design using
    the NLSY. Intelligence, 59, 139-156.
    <https://doi.org/10.1016/j.intell.2016.08.008>

  - [Frontiers](https://github.com/R-Computing-Lab/Sims-et-al-2024):
    Reproduces Sims, E. E., Trattner, J. D., & Garrison, S. M. (2024).
    Exploring the relationship between depression and delinquency: a
    sibling comparison design using the NLSY. Frontiers in psychology,
    15, 1430978. <https://doi.org/10.3389/fpsyg.2024.1430978>

  - [AMPPS](https://github.com/R-Computing-Lab/target-causalclaims):
    Reproduces analyses from Garrison et al 2025, using `targets` for
    workflow management. Garrison, S. M., Trattner, J. D., Lyu, X.,
    Prillaman, H. R., McKinzie, L., Thompson, S. H. E., & Rodgers, J. L.
    (2025). Sibling Models Can Test Causal Claims without Experiments:
    Applications for Psychology.
    <https://doi.org/10.1101/2025.08.25.25334395>
- China Family Panel Studies (CFPS) dataset
  - [AMPPS](https://github.com/R-Computing-Lab/discord_CFPS): Reproduces
    analyses from the China Family Panel Studies (CFPS) dataset,
    focusing on the association between adolescent depression and math
    achievement.

## Installation

You can install the official version from CRAN

``` r
# Install/update discord with the release version from CRAN.
install.packages('discord')
```

You can also install/update discord with the development version of
discord from [GitHub](https://github.com/) with:

``` r
# If devtools is not installed, uncomment the line below.
# install.packages('devtools')
devtools::install_github('R-Computing-Lab/discord')
```

## Citation

If you use `discord` in your research or wish to refer to it, please
cite the following paper:

``` r
citation(package = "discord")
To cite package 'discord' in publications use:

  Garrison S, Trattner J, Hwang Y (2025). _discord: Functions for
  Discordant Kinship Modeling_. R package version 1.3,
  <https://github.com/R-Computing-Lab/discord>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {discord: Functions for Discordant Kinship Modeling},
    author = {S. Mason Garrison and Jonathan Trattner and Yoo Ri Hwang},
    note = {R package version 1.3},
    url = {https://github.com/R-Computing-Lab/discord},
  }
```

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
