# Handling categorical predictors with discord

## Introduction

### Purpose

This vignette demonstrates how to incorporate categorical predictors
into discordant-kinship regression analyses using the `discord` package.
Building on dyadic modeling strategies from Kenny et al. (Kenny, Kashy,
and Cook 2006) and extensions to kinship models in our prior work (Hwang
2022), we present several approaches for incorporating categorical
variables into these specialized regression models.

We focus on:

1.  How to code and interpret categorical variables at different levels
    (between-dyads vs. mixed)
2.  Methods for treating categorical predictors (binary match
    vs. multi-match)
3.  Practical implementation using the `discord` package

To illustrate these concepts, we examine whether sex and race predict
socioeconomic status (SES) at age 40, using different categorical coding
schemes applied to the 1979 National Longitudinal Survey of Youth
(NLSY79).

### Understanding Variable Types in Dyadic Analysis

In dyadic analysis, categorical predictors can operate at different
levels:

- **Between-dyad variables** are constant within dyads.
  - Example: For full siblings, race is often a between-dyad variable.
- **Within-dyad variables** vary within dyads but remain constant across
  dyads (rare for categorical variables).
  - Example: Division of chores between roommates (must sum to 100%).
- **Mixed variables** can vary both within and across dyads.
  - Example: Biological sex in non-MZ twin siblings; age; personality
    traits.

We summarize these distinctions below:

| Variable Type | Definition                                     | Examples                                                  | Analytic Implications                                         |
|---------------|------------------------------------------------|-----------------------------------------------------------|---------------------------------------------------------------|
| Between-dyads | Members of the same pair have identical values | Race in same-race siblings; Length of marriage in couples | Simplifies analysis; Functions like individual-level variable |
| Within-dyads  | Varies within pairs but constant across pairs  | Division of chores between roommates (must sum to 100%)   | Rare for categorical variables; Requires specialized handling |
| Mixed         | Varies both within and across pairs            | Sex in sibling pairs; Age; Personality traits             | Most complex; Requires transformation to dyad-level variables |

For continuous predictors, researchers typically compute mean and
difference scores. For categorical variables, these operations are not
meaningful (Kenny, Kashy, and Cook 2006). Use coding strategies designed
for categories instead.

### Coding Approaches for Categorical Variables

The `discord` package implements two main coding strategies for
categorical predictors:

1.  **Binary-match coding** collapses all category pairings into a
    single indicator showing whether two members of a dyad share the
    same category (1) or differ (0).

2.  **Multi-match coding** preserves information about which category
    the pair matches on. So rather than coding all same-category pairs
    identically, it distinguishes, for example, male–male from
    female–female pairings.

#### Binary-match Coding

Binary match coding creates a simple indicator of whether pairs match
(1) or differ (0) on a single categorical variable: - Same-sex pairs
(male-male or female-female) → 1 - Mixed-sex pairs (male-female) → 0

**Use case**: When the research question focuses on similarity versus
difference, rather than effects of specific categories.

#### Multi-match Coding

Multi-match coding preserves the identity of the specific category
shared within each pair, allowing distinctions among same-category
matches:

- Male-male pairs → “MALE”
- Female-female pairs → “FEMALE”
- Mixed pairs → “MIXED”

**Use case**: when the goal is to compare category-specific effects (for
example, male-male versus female-female pairs) rather than simply
testing for similarity or difference.

As noted by Hwang and Garrison (Hwang 2022), the appropriate coding
strategy should follow the conceptual level of your research question
and the theoretical justification for distinguishing categories.

## Data Preparation

To illustrate these coding approaches, we use sibling data from the 1979
National Longitudinal Survey of Youth (NLSY79). The example begins by
loading the required packages and preparing an analysis dataset that
links siblings, filters relevant variables, and recodes categorical
predictors.

### Package Loading and Data Setup

``` r
# Loading necessary packages and data
# For easy data manipulation
library(dplyr)
# For kinship linkages
library(NlsyLinks)
# For discordant-kinship regression
library(discord)
# pipe
library(magrittr)

# data
data(data_flu_ses)
```

Next, we filter and clean the NLSY79 data, construct kinship links, and
recode the categorical variables. In this example, we restrict to full
siblings (R = 0.5) from the Gen1 cohort.

``` r
# for reproducibility
set.seed(2023)

link_vars <- c("S00_H40", "RACE", "SEX")

# Specify NLSY database and kin relatedness

link_pairs <- Links79PairExpanded %>%
  filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5)
```

We use
[`CreatePairLinksSingleEntered()`](https://nlsy-links.github.io/NlsyLinks/reference/CreatePairLinks.html)
from the `NlsyLinks`package to merge the kinship linkage data with the
target variables. The resulting dataset is structured in wide format,
with suffixes identifying the two siblings within each pair.

``` r
df_link <- CreatePairLinksSingleEntered(
  outcomeDataset = data_flu_ses,
  linksPairDataset = link_pairs,
  outcomeNames = link_vars
)
```

To ensure that the dependent variable, SES at age 40, is available for
both siblings, we remove cases with missing values. Sex and race are
recoded as factors to prepare them for analysis.

``` r
# We removed the pair when the Dependent Variable is missing.
df_link <- df_link %>%
  filter(!is.na(S00_H40_S1) & !is.na(S00_H40_S2)) %>%
  mutate(
    SEX_S1 = case_when(
      SEX_S1 == 0 ~ "MALE",
      SEX_S1 == 1 ~ "FEMALE"
    ),
    SEX_S2 = case_when(
      SEX_S2 == 0 ~ "MALE",
      SEX_S2 == 1 ~ "FEMALE"
    ),
    RACE_S1 = case_when(
      RACE_S1 == 0 ~ "NONMINORITY",
      RACE_S1 == 1 ~ "MINORITY"
    ),
    RACE_S2 = case_when(
      RACE_S2 == 0 ~ "NONMINORITY",
      RACE_S2 == 1 ~ "MINORITY"
    )
  )
```

Because race is typically constant within families of full siblings, it
functions as a between-dyad variable. For illustration purposes, we
restrict the analytic sample to same-race pairs.

``` r
df_link <- df_link %>%
  dplyr::filter(RACE_S1 == RACE_S2)
```

To avoid violating assumptions of independence, we retain only one
sibling pair per household:

``` r
df_link <- df_link %>%
  group_by(ExtendedID) %>%
  slice_sample() %>%
  ungroup()
```

Here,
[`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
randomly selects one pair per household. In practice, researchers may
choose to select the oldest or youngest pair instead by using
[`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html) or
[`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html).

### Handling Categorical Predictors

#### Mixed Variables: Sex as an Example

Sex is a mixed variable in sibling studies because it can vary within
some pairs and also across pairs. Families may include same-sex or
mixed-sex sibling pairs, and that composition varies across families.

We use the
[`discord_data()`](https://r-computing-lab.github.io/discord/reference/discord_data.md)
function to prepare the data for analysis.

``` r
cat_sex <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "sex",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)
```

In the restructured data, the individual with the higher SES (the
dependent variable) is labeled “\_1” and the other is labeled “\_2”.
This gives us the following sex compositions:

| id  | S00_H40_1 | S00_H40_2 | S00_H40_diff | S00_H40_mean | SEX_1  | SEX_2  | SEX_binarymatch | SEX_multimatch |
|:---:|:---------:|:---------:|:------------:|:------------:|:------:|:------:|:---------------:|:--------------:|
| 359 | 80.83837  | 46.83232  |  34.006052   |   63.83535   | FEMALE | FEMALE |    same-sex     |     FEMALE     |
| 363 | 79.39371  | 74.43080  |   4.962906   |   76.91225   | FEMALE |  MALE  |    mixed-sex    |     mixed      |
| 492 | 70.61552  | 50.76235  |  19.853172   |   60.68894   | FEMALE |  MALE  |    mixed-sex    |     mixed      |
| 85  | 48.54822  | 35.09636  |  13.451863   |   41.82229   | FEMALE | FEMALE |    same-sex     |     FEMALE     |
| 131 | 49.05986  | 35.82118  |  13.238680   |   42.44052   |  MALE  | FEMALE |    mixed-sex    |     mixed      |
| 299 | 47.78226  | 31.52980  |  16.252455   |   39.65603   |  MALE  |  MALE  |    same-sex     |      MALE      |

The dataset contains the following sex pairings:

| SEX_1  | SEX_2  | sample_size |
|:------:|:------:|:-----------:|
| FEMALE | FEMALE |     416     |
| FEMALE |  MALE  |     358     |
|  MALE  | FEMALE |     429     |
|  MALE  |  MALE  |     396     |

By default, the `SEX_1` variable indicates the sex of the individual who
has the higher DV within the pair, and the `SEX_2` variable indicates
the sex of the other member of the dyad.

As shown, the
[`discord_data()`](https://r-computing-lab.github.io/discord/reference/discord_data.md)
function generates both `SEX_binarymatch` and `SEX_multimatch`
variables. This recodes the sex variable—which initially varied within
and between dyads—into between-dyad variables:

|  binary   | multi  | SEX_1  | SEX_2  | sample_size |
|:---------:|:------:|:------:|:------:|:-----------:|
| mixed-sex | mixed  | FEMALE |  MALE  |     358     |
| mixed-sex | mixed  |  MALE  | FEMALE |     429     |
| same-sex  | FEMALE | FEMALE | FEMALE |     416     |
| same-sex  |  MALE  |  MALE  |  MALE  |     396     |

Researchers can choose between these options depending on their research
question: - Use `SEX_binarymatch` when comparing same-sex and mixed-sex
pairs. - Use `SEX_multimatch` when comparing male-male, female-female,
and mixed-sex pairs.

#### Between-dyad Variable: Race as an Example

For demonstration purposes, we have already restricted the dataset to
include only same-race pairs, making race a between-dyad variable. We
now prepare the data specifically for testing whether there are racial
differences in SES discordance.

``` r
set.seed(2023) # for reproducibility

# Prepare data with race as demographic variable
cat_race <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  predictors = NULL,
  sex = "SEX",
  race = "RACE",
  demographics = "race",
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)
```

The table below reports the race composition of pairs.

| RACE_binarymatch | RACE_multimatch |   RACE_1    |   RACE_2    | sample_size |
|:----------------:|:---------------:|:-----------:|:-----------:|:-----------:|
|    same-race     |    MINORITY     |  MINORITY   |  MINORITY   |     778     |
|    same-race     |   NONMINORITY   | NONMINORITY | NONMINORITY |     821     |

Because we filtered for same-race pairs, all pairs have RACE_binarymatch
= “same-race.” When using NLSY data, the RACE_multimatch variable
distinguishes between the three categories used by the Bureau of Labor
Statistics (Black, Hispanic, and Non-Black, Non-Hispanic).

The `RACE_binarymatch` variable indicates whether the pair is same-race
or different-race. As all pairs are same-race in this sample, this
variable does not vary. The `RACE_multimatch` variable classifies pairs
into one of three categories: The RACE_binarymatch variable indicates
whether the pair is same-race or different-race. As all pairs are
same-race in this sample, this variable does not vary. The
RACE_multimatch variable classifies pairs into one of three categories:
Since we filtered for same-race pairs only, all pairs have
RACE_binarymatch = “same-race”. When using NLSY data, the
RACE_multimatch variable distinguishes between the three groupings that
the bureau of labor statistics uses (Black, Hispanic, and Non-Black,
Non-Hispanic).

The `RACE_binarymatch` variable indicates whether the pair is the
same-race pair or different-race pair. As all pairs are same-race in
this sample, this variable does not vary within dyad. The
`RACE_multimatch` The RACE_multimatch variable classifies pairs into one
of three categories: - Minority-minority, - Nonminority-nonminority, -
Discordant (unused in this restricted sample).

#### Combining Binary and Multi-match Variables

We can also prepare data that includes multiple demographic variables.

``` r
# for reproducibility

set.seed(2023)

cat_both <- discord_data(
  data = df_link,
  outcome = "S00_H40",
  predictors = NULL,
  sex = "SEX",
  race = "RACE",
  demographics = "both",
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "both"
)
```

The demographic variables can be used in the same regression model.

``` r
cat_both <- cat_both %>%
  dplyr::mutate(
    RACE_binarymatch = case_when(
      RACE_binarymatch == 0 ~ "diff-race",
      RACE_binarymatch == 1 ~ "same-race"
    ),
    SEX_binarymatch = case_when(
      SEX_binarymatch == 0 ~ "mixed-sex",
      SEX_binarymatch == 1 ~ "same-sex"
    )
  )
```

| RACE_multi  |   RACE_1    |   RACE_2    | SEX_binary | SEX_multi | SEX_1  | SEX_2  | sample_size |
|:-----------:|:-----------:|:-----------:|:----------:|:---------:|:------:|:------:|:-----------:|
|  MINORITY   |  MINORITY   |  MINORITY   | mixed-sex  |   mixed   | FEMALE |  MALE  |     179     |
|  MINORITY   |  MINORITY   |  MINORITY   | mixed-sex  |   mixed   |  MALE  | FEMALE |     201     |
|  MINORITY   |  MINORITY   |  MINORITY   |  same-sex  |  FEMALE   | FEMALE | FEMALE |     204     |
|  MINORITY   |  MINORITY   |  MINORITY   |  same-sex  |   MALE    |  MALE  |  MALE  |     194     |
| NONMINORITY | NONMINORITY | NONMINORITY | mixed-sex  |   mixed   | FEMALE |  MALE  |     179     |
| NONMINORITY | NONMINORITY | NONMINORITY | mixed-sex  |   mixed   |  MALE  | FEMALE |     228     |
| NONMINORITY | NONMINORITY | NONMINORITY |  same-sex  |  FEMALE   | FEMALE | FEMALE |     212     |
| NONMINORITY | NONMINORITY | NONMINORITY |  same-sex  |   MALE    |  MALE  |  MALE  |     202     |

In this table:

- `RACE_multimatch` shows whether pairs are minority-minority or
  nonminority-nonminority.

- `SEX_binarymatch` distinguishes same-sex and mixed-sex pairs.

- `SEX_multimatch` identifies male-male, female-female, and mixed-sex
  pairings.

## Results and Interpretation

### Regression Analysis: Sex Variables

#### Binary Match Coding for Sex

First, we test whether same-sex versus mixed-sex pairs differ in SES
discordance.

The regression model can be conducted as such:

``` r
discord_sex_binary <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "sex",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "binary"
)
```

|      Term       | Estimate | Standard Error | T Statistic | P Value  |
|:---------------:|:--------:|:--------------:|:-----------:|:--------:|
|   (Intercept)   |  23.952  |     1.166      |   20.534    | p\<0.001 |
|  S00_H40_mean   |  -0.090  |     0.020      |   -4.561    | p\<0.001 |
| SEX_binarymatch |  0.024   |     0.750      |    0.031    | p=0.975  |

##### Interpretation:

We predict sibling differences in SES (`S00_H40_diff`).

- The mean SES score (`S00_H40_mean`) is a significant control variable
  (p =0). `S00_H40_mean` is negatively associated with the difference in
  SES score between siblings at age 40, controlling for another variable
  (in this case, `SEX_binarymatch`). For one unit increase of
  `S00_H40_mean`, `S00_H40_diff`is expected to decrease approximately
  -0.09.

- The binary sex match variable `SEX_binarymatch` is not a significant
  predictor (p = 0.975), when controlling for `S00_H40_mean`. There is
  no significant differences between same-sex pairs and mixed-sex pairs
  in `S00_H40_diff`. This means that the difference between same-sex
  pairs and mixed-sex pairs does not significantly predict the
  `S00_H40_diff` in the pair when controlling for `S00_H40_mean`.

#### Multi-Match Coding for Sex

Next, we examine whether male-male, female-female, and mixed-sex pairs
differ in SES discordance.

The regression model can be conducted as such:

``` r
discord_sex_multi <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  24.523  |     1.248      |   19.647    | p\<0.001 |
|        S00_H40_mean        |  -0.075  |     0.020      |   -3.673    | p\<0.001 |
|     SEX_multimatchMALE     |  -0.380  |     1.053      |   -0.361    | p=0.718  |
|    SEX_multimatchmixed     |  -0.192  |     0.907      |   -0.212    | p=0.832  |
| RACE_multimatchNONMINORITY |  -2.277  |     0.772      |   -2.950    | p=0.003  |

##### Interpretation:

- The term `S00_H40_mean` was a significant control variable (p = 0).
  This means that the mean SES score for the sibling pairs
  (`S00_H40_mean`) is negatively associated with the difference in SES
  between siblings (`S00_H40_diff`), controlling for other variables (in
  this case, `SEX_multimatch`). It is estimated that for one unit
  increase of `S00_H40_mean`, `S00_H40_diff` is expected to decrease
  approximately 0.075.

- There was no significant difference between female-female pairs and
  male-male pairs (p=0.718) to predict `S00_H40_diff`. Similarly, there
  were no significant differences between mixed-sex pairs and
  female-female pairs (p = 0.832).

- The coefficient 0.38 is the difference between the expected
  `S00_H40_diff` for the reference group (in this case, the
  female-female pairs) and the male-male pairs.

- The coefficient 0.192 is the difference between the expected
  `S00_H40_diff` for the reference group (in this case, the
  female-female pairs) and the mixed-sex pairs. However, these
  coefficients are not significant, so it is not advisable to interpret
  the coefficients.

#### Mean SES Model with Sex

We can also examine whether sex composition predicts mean SES levels
(rather than SES differences):

``` r
discord_cat_mean <- lm(S00_H40_mean ~ SEX_binarymatch,
  data = cat_sex
)
```

|          Term           | Estimate | Standard Error | T Statistic | P Value  |
|:-----------------------:|:--------:|:--------------:|:-----------:|:--------:|
|       (Intercept)       |  52.399  |     0.675      |   77.576    | p\<0.001 |
| SEX_binarymatchsame-sex |  0.018   |     0.948      |    0.019    | p=0.985  |

##### Interpretation:

In this regression model, the mean SES score for the siblings
(`S00_H40_mean`) was regressed on the SEX-composition variable
(`SEX_binarymatch`).

There is no significant difference between same-sex pairs and mixed-sex
pairs in the mean SES score for the siblings (p=0.985)

It is estimated that compared to the mixed-sex pairs, the same-sex pairs
would have approximately 0.018 higher `S00_H40_mean`. However, this
coefficient is not significant, so it is not advisable to interpret the
coefficient.

#### Multi-Match Coding for Sex

``` r
discord_cat_mean2 <- lm(S00_H40_mean ~ SEX_multimatch,
  data = cat_sex
)
```

|        Term         | Estimate | Standard Error | T Statistic | P Value  |
|:-------------------:|:--------:|:--------------:|:-----------:|:--------:|
|     (Intercept)     |  50.529  |     0.927      |   54.516    | p\<0.001 |
| SEX_multimatchMALE  |  3.872   |     1.327      |    2.917    | p=0.004  |
| SEX_multimatchmixed |  1.870   |     1.146      |    1.632    | p=0.103  |

##### Interpretation:

There is a significant difference between female-female pairs and
male-male pairs (0.004) to predict the `S00_H40_mean`. However, there is
no significant difference between mixed-sex pairs and female-female
pairs (p = 0.103).

The coefficient 3.872 is the difference between the expected
`S00_H40_mean` (the mean SES score for the siblings) for the reference
group (in this case, the female-female pairs) and the male-male pairs.
It can be concluded that male-male pairs and female-female pair has
significant differences in `S00_H40_mean`.

The coefficient 1.87 is the difference between the expected
`S00_H40_mean` for the reference group (in this case, the female-female
pairs) and the mixed-sex pairs. However, these coefficients are not
significant, so it is not advisable to interpret the coefficients.

### Regression Analysis: Race Variables

For race variables, we use the multi-match coding to examine differences
between minority and non-minority pairs:

#### Multimatch

The regression model with a multi-match race variable as a predictor can
be conducted as such:

``` r
# perform kinship regressions
cat_race_reg <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "race",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  24.361  |     1.108      |   21.987    | p\<0.001 |
|        S00_H40_mean        |  -0.076  |     0.020      |   -3.712    | p\<0.001 |
| RACE_multimatchNONMINORITY |  -2.272  |     0.771      |   -2.945    | p=0.003  |

##### Interpretation:

The mean SES score for the siblings (`S00_H40_mean`) is a significant
control variable (p =0. The term `S00_H40_mean` is negatively associated
with the difference score of SES between siblings (`S00_H40_diff`),
controlling for another variable (in this case,
`RACE_multimatchNONMINORITY`). It is estimated that for one unit
increase of `S00_H40_mean`, the DV (`S00_H40_diff`) is expected to
decrease by approximately 0.076.

The term `RACE_multimatchNONMINORITY` was a significant predictor of
`S00_H40_diff` (p = 0.003) after controlling for `S00_H40_mean`. This
means that the difference between the “Minority-minority” sibling pairs
and “nonminority-non-minority” sibling pairs significantly predicts
`S00_H40_diff`. Specifically, compared to the reference group (the
“minority” pairs), “nonminority” pairs are expected to have
approximately 2.272 lower `S00_H40_diff`.

#### Mean SES Model with Race

We can also examine whether race composition predicts mean SES levels:

``` r
discord_cat_mean <- lm(S00_H40_mean ~ RACE_multimatch,
  data = cat_race
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  47.645  |     0.659      |   72.335    | p\<0.001 |
| RACE_multimatchNONMINORITY |  9.277   |     0.919      |   10.092    | p\<0.001 |

##### Interpretation:

There is significant difference between “minority” pairs and
“nonminority” pairs in `S00_H40_mean` (p =0). It is estimated that,
compared to the reference group (minority pairs), the nonminority pairs
would have approximately 9.277 lower `S00_H40_mean`.

### Regression Analysis: Combined Sex and Race Variables

We can include both sex and race as predictors in the same model:

Like before we restructure the data for the kinship-discordant
regression, but this time we using the
[`discord_regression()`](https://r-computing-lab.github.io/discord/reference/discord_regression.md)
function, which calls the
[`discord_data()`](https://r-computing-lab.github.io/discord/reference/discord_data.md)
function internally.

#### Multimatch

``` r
both_multi <- discord_regression(
  data = df_link,
  outcome = "S00_H40",
  sex = "SEX",
  race = "RACE",
  demographics = "both",
  predictors = NULL,
  pair_identifiers = c("_S1", "_S2"),
  coding_method = "multi"
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  24.523  |     1.248      |   19.647    | p\<0.001 |
|        S00_H40_mean        |  -0.075  |     0.020      |   -3.673    | p\<0.001 |
|     SEX_multimatchMALE     |  -0.380  |     1.053      |   -0.361    | p=0.718  |
|    SEX_multimatchmixed     |  -0.192  |     0.907      |   -0.212    | p=0.832  |
| RACE_multimatchNONMINORITY |  -2.277  |     0.772      |   -2.950    | p=0.003  |

##### Interpretation

The mean SES score for the siblings (`S00_H40_mean`) is a significant
control variable (p = 0 ). `S00_H40_mean` is negatively associated with
the difference score of SES between the siblings (`S00_H40_diff`),
controlling for other variables (in this case, the `SEX_multimatchMALE`,
`SEX_multimatchmixed` and `RACE_multimatchNONMINORITY`). It is estimated
that for one unit increase of the mean SES score for the sibling pairs(
`S00_H40_mean`), the difference score of SES between
siblings(`S00_H40_diff`) is expected to decrease approximately -0.075.

The `SEX_multimatchmixed` and `SEX_multimatchMALE` are not significant
predictors when controlling for other variables (i.e., `S00_H40_mean`
and `RACE_multimatchNONMINORITY`). The coefficient -0.38 is the
difference between the expected DV (`S00_H40_diff`) for the reference
group (in this case, the “female-female” pairs) and the “male-male”
pairs. The coefficient -0.192 is the difference between the expected DV
(`S00_H40_diff`) for the female-female pairs and the mixed-sex pairs.
However, these coefficients are not significant, so it is not advisable
to interpret the coefficients.

The term `RACE_multimatchNONMINORITY` is a significant predictor (p =
0.003) when controlling for other variables (i.e., `SEX_multimatchMALE`,
`SEX_multimatchmixed`, and `S00_H40_mean`). This means that there is a
significant difference between minority race pairs and nonminority race
pairs in the difference score of SES between siblings (`S00_H40_diff`)
when controlling for the model covariates (i.e., `SEX_multimatchMALE`,
`SEX_multimatchmixed`, and `S00_H40_mean`). Specifically, compared to
the minority race pairs, the nonminority race pairs were expected to
have approximately -2.277 higher difference score of SES between
siblings at age 40.

#### Alternative Model: Binary Sex Match and Multi-Match Race

To combine binary and multi-match coding approaches, we can use the
standard [`lm()`](https://rdrr.io/r/stats/lm.html) function:

We can perform regression using the binary-match sex variable and
multi-match race variable as such:

``` r
discord_cat_diff <- lm(
  S00_H40_diff ~ S00_H40_mean +
    RACE_multimatch + SEX_binarymatch,
  data = cat_both
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  24.357  |     1.172      |   20.787    | p\<0.001 |
|        S00_H40_mean        |  -0.076  |     0.020      |   -3.711    | p\<0.001 |
| RACE_multimatchNONMINORITY |  -2.272  |     0.771      |   -2.944    | p=0.003  |
|  SEX_binarymatchsame-sex   |  0.007   |     0.748      |    0.009    | p=0.993  |

##### Interpretation:

The mean SES score for the siblings at 40 (`S00_H40_mean`) is a
significant control variable (p = 0. The mean SES score for the siblings
(`S00_H40_mean`) is negatively associated with the difference score of
SES between the siblings (`S00_H40_diff`), controlling for other
variables (in this case, `SEX_binarymatchsame-sex` and
`RACE_multimatchNONMINORITY`). It is estimated that for one unit
increase of `S00_H40_mean`, the DV (`S00_H40_diff`) is expected to
decrease approximately 0.076.

The term `SEX_binarymatchsame-sex` is not a significant predictor (p =
0.993) when controlling for other variables (i.e., `S00_H40_mean` and
`RACE_multimatchNONMINORITY`). This means that the difference between
same-sex pairs and mixed-sex pairs does not significantly predict the
difference score of SES between siblings (`S00_H40_diff`) when
controlling for the mean SES score for the siblings (`S00_H40_mean`) and
race-composition of the pair (`RACE_multimatchNONMINORITY`). Compared to
the mixed-sex pairs, it is estimated that the same-sex pairs have
approximately 0.007higher difference score of SES between siblings
(`S00_H40_diff`) when controlling for the mean SES score for the sibling
pairs (`S00_H40_mean`) and race-composition of the pairs
(`RACE_multimatchNONMINORITY`). However, this coefficient is not
statistically significant and should not be interpreted.

The term `RACE_multimatchNONMINORITY`is a significant predictor (p =
0.003). This means that there is a significant difference between
minority race pairs and nonminority race pairs to predict the difference
score of SES between the siblings (`S00_H40_diff`) when controlling for
the model covariates (i.e., `SEX_binarymatchsame-sex` and
`S00_H40_mean`). Specifically, compared to the minority race pairs,
nonminority race pairs were expected to have approximately 2.272 lower
difference scores of SES between siblings (`S00_H40_diff`).

#### Mean SES Models with Both Variables

Finally, we examine how sex and race together predict mean SES levels:

``` r
discord_cat_mean <- lm(
  S00_H40_mean ~ RACE_multimatch +
    SEX_multimatch,
  data = cat_both
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  45.801  |     1.013      |   45.210    | p\<0.001 |
| RACE_multimatchNONMINORITY |  9.277   |     0.917      |   10.114    | p\<0.001 |
|     SEX_multimatchMALE     |  3.867   |     1.287      |    3.005    | p=0.003  |
|    SEX_multimatchmixed     |  1.800   |     1.111      |    1.620    | p=0.105  |

##### Interpretation:

The term `SEX_multimatchMALE` is a significant predictor (p = 0.003)
when controlling for other variables (i.e., `SEX_multimatchmixed`and
`RACEe_multimatchNONMINORITY`). This means that the difference between
female-female pairs and male-male pairs significantly predicted the mean
SES score for the siblings when controlling for race-composition of the
pairs. Compared to the female-female pairs, it is estimated that the
male-male pairs have approximately 3.867 higher mean SES score for the
siblings when controlling for and race-composition of the pairs.

The term `SEX_multimatchmixed` was not a significant predictor (p =
0.105) when controlling for other variables (i.e.,
`SEX_multimatchMALE`and `RACE_multimatchNONMINORITY`). This means that
the difference between female-female pairs and mixed-sex pairs does not
significantly predict the mean SES score for the siblings when
controlling for race-composition of the pairs. Compared to the
female-female pairs, it is estimated that the mixed-sex pairs have
approximately 1.8 higher mean SES score for the sibling pairs when
controlling for and race-composition of the pairs. However, this
variable is not significant, so it is not advisable to interpret the
coefficient.

The term `RACE_multimatchNONMINORITY` is a significant predictor (p =
0). This means that there is a significant difference between minority
race pairs and nonminority race pairs in the mean SES score for the
sibling pairs (`S00_H40_mean`) when controlling for the other variables
(i.e., `SEX_multimatchmixed` and `SEX_multimatchMALE`). Specifically,
compared to the minority race pairs, nonminority race pairs were
expected to have approximately 9.277 higher mean SES score for siblings

#### Mean SES Models with Combining multimatch and binarymatch

``` r
discord_cat_mean2 <- lm(S00_H40_mean ~ RACE_multimatch + SEX_binarymatch,
  data = cat_both
)
```

|            Term            | Estimate | Standard Error | T Statistic | P Value  |
|:--------------------------:|:--------:|:--------------:|:-----------:|:--------:|
|        (Intercept)         |  47.601  |     0.809      |   58.803    | p\<0.001 |
| RACE_multimatchNONMINORITY |  9.278   |     0.920      |   10.090    | p\<0.001 |
|  SEX_binarymatchsame-sex   |  0.086   |     0.919      |    0.093    | p=0.926  |

##### Interpretation:

The term `SEX_binarymatchsame-sex` is not a significant predictor (p =
0.926) when controlling for the race-composition variable (i.e.,
`RACE_multimatchNONMINORITY`). This means that the difference between
mixed-sex pairs and same sex pairs does not significantly predict the
mean SES score for the siblings when controlling for race-composition of
the pairs. Compared to the mixed-sex pairs, it is estimated that the
same-sex pairs have approximately 0.086 higher mean SES score for the
sibling pairs (`S00_H40_mean`) when controlling for and race-composition
of the pairs (`RACE_multimatchNONMINORITY`). However, this variable is
not significant, so it is not advisable to interpret the coefficient.

The term `RACE_multimatchNONMINORITY` is a significant predictor (p =
0). This means that there is a significant difference between minority
race pairs and nonminority race pairs in the the mean SES score for the
siblings when controlling for the sex-composition variable (i.e.,
`SEX_binarymatchsame-sex`). Specifically, compared to the minority race
pairs, nonminority race pairs were expected to have approximately 9.278
higher mean SES score for siblings

## Conclusion

This vignette has demonstrated how to incorporate categorical predictors
in discordant-kinship regression analyses using the `discord` package.

Key findings and recommendations include:

- Variable Type Matters: Categorical variables must be handled
  differently depending on whether they are between-dyads variables
  (like race in our filtered sample) or mixed variables (like sex in
  sibling pairs).
- Coding Approaches Offer Different Insights:
  - Binary match coding examines whether similarity/difference matters
  - Multi-match coding allows for more detailed examination of specific
    category effects

### Implementation Recommendations:

For implementation in your own research, we recommend: - Consider the
theoretical nature of your categorical predictors - Use
[`discord_data()`](https://r-computing-lab.github.io/discord/reference/discord_data.md)
to prepare categorical variables appropriately - Choose coding schemes
based on your specific research questions - Carefully interpret results
in light of variable coding decisions

## References

Hwang, Yoo Ri. 2022. “Dueling Dyads: Regression Versus MLM Analysis with
a Categorical Predictor.” Master's thesis, Wake Forest University.

Kenny, David A., Deborah A. Kashy, and William L. Cook. 2006. *Dyadic
Data Analysis*. Dyadic Data Analysis. New York, NY, US: Guilford Press.
