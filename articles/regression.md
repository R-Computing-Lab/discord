# NLSY: Regression demonstration with Flu Vaccination and SES data

## Introduction

This vignette shows how to run a **discordant-kinship regression** with
the [discord](https://github.com/R-Computing-Lab/discord) package and
the `{NLSYLinks}` package. The example uses data on flu vaccination and
socioeconomic status (SES) from the National Longitudinal Survey of
Youth 1979 (NLSY79). The goal of this analysis is to examine whether SES
at age 40 predicts the number of flu vaccinations received between 2006
and 2016, while controlling for genetic and shared environmental factors
by leveraging a discordant-kinship design.

### Data Description

We build on Trattner et al. (2020) (Trattner, Kennon, and Garrison
2020), originally motivated by reports of health disparities among
ethnic minority groups during the COVID-19 pandemic (Hooper, Nápoles,
and Pérez-Stable 2020). The data come from the 1979 National
Longitudinal Survey of Youth (NLSY79), a nationally-representative
household probability sample jointly sponsored by the U.S. Bureau of
Labor Statistics and the Department of Defense. Participants were
surveyed annually from 1979 until 1994 at which point surveys occurred
biennially. The data are publicly available at
<https://www.nlsinfo.org/> and include responses from a biennial flu
vaccine survey administered between 2006 and 2016.

The original analysis examined whether socioeconomic status (SES) at age
40 predicted flu vaccination rates, using a discordant kinship design.
For this vignette, the data were downloaded using the [NLS
Investigator](https://www.nlsinfo.org/investigator/pages/login) and are
available
[here](https://github.com/R-Computing-Lab/discord/blob/main/data-raw/flu_shot.dat).
SES at age 40 values can be found
[here](https://github.com/R-Computing-Lab/discord/blob/main/data-raw/nlsy-ses.csv).
For clarity and to emphasize the functionality of {discord}, the data
has been pre-processed using [this
script](https://github.com/R-Computing-Lab/discord/blob/main/data-raw/preprocess-discord-flu.R).
This analysis is enabled by recent work that inferred genetic
relatedness for approximately 95% of kin pairs in the NLSY79 cohort
(Rodgers et al. 2016). These kinship links are provided in the
[{NlsyLinks}](https://nlsy-links.github.io/NlsyLinks/index.html) R
package (Beasley et al. 2016), and can be easily utilized with the
{discord} package.

### Data Cleaning

#### Prepare person-level data

To perform a discordant-kinship regression, we first need to preprocess
the data. For this example, we will load the following packages:

``` r
# For easy data manipulation
library(dplyr)
# For kinship linkages
library(NlsyLinks)
# For discordant-kinship regression
library(discord)
# To clean data frame names
library(janitor)
# tidy up output
library(broom)
# pipe
library(magrittr)

data(data_flu_ses)
```

After preprocessing, we obtain a data frame that contain subject
identifiers, demographic information such as race and sex, and
behavioral variables such as flu vaccination totals and SES at age 40. A
random slice of this dataset is shown below.

| CASEID | RACE | SEX | FLU_total | S00_H40  |
|:------:|:----:|:---:|:---------:|:--------:|
|  222   |  1   |  1  |     4     | 47.87046 |
|  493   |  0   |  1  |     0     | 56.78763 |
|  561   |  0   |  0  |     5     | 75.05756 |
|   84   |  1   |  1  |     3     | 64.12765 |
|  590   |  0   |  0  |     3     | 32.22656 |
|  441   |  0   |  1  |     0     | 23.83602 |

Using kinship data from the NlsyLinks package, we restructure the
dataset to better lend itself to discordant kinship analysis. For each
kin pair, the function
[`CreatePairLinksSingleEntered()`](https://nlsy-links.github.io/NlsyLinks/reference/CreatePairLinks.html)
takes a data set like the one above, **\[a specification of the NLSY
database and the kin’s relatedness\]**, and the variables of interest.
It returns a data frame where each row represents a kin pair, and each
variable appears twice—once for each sibling—using suffixes to
distinguish individuals.

In this example, we examine the relationship between total flu
vaccinations (received between 2006 and 2016) and SES at age 40,
focusing on full siblings. The variable names used in this linkage are
drawn from the preprocessed data previewed above.

``` r
# Get kinship links for individuals with the following variables:
link_vars <- c(
  "FLU_total", "FLU_2008", "FLU_2010",
  "FLU_2012", "FLU_2014", "FLU_2016",
  "S00_H40", "RACE", "SEX"
)
```

#### Create kinship-linked data

We now link the subjects by the specified variables using
[`CreatePairLinksSingleEntered()`](https://nlsy-links.github.io/NlsyLinks/reference/CreatePairLinks.html),
from the {NlsyLinks} package. We filter full siblings (RFull == .5) and
use RelationshipPath == “Gen1Housemates”; see {NlsyLinks} docs for other
kin types.

``` r
# Specify NLSY database and kin relatedness
link_pairs <- Links79PairExpanded %>%
  filter(RelationshipPath == "Gen1Housemates" & RFull == 0.5)

df_link <- CreatePairLinksSingleEntered(
  outcomeDataset = data_flu_ses,
  linksPairDataset = link_pairs,
  outcomeNames = link_vars
)
```

We have saved this data frame as `df_link`. A random subset of this data
is shown below:[¹](#fn1)

| ExtendedID | SubjectTag_S1 | SubjectTag_S2 | FLU_total_S1 | FLU_total_S2 | S00_H40_S1 | S00_H40_S2 |
|:----------:|:-------------:|:-------------:|:------------:|:------------:|:----------:|:----------:|
|    1184    |    118400     |    118500     |      3       |      4       |  85.50907  |  92.97715  |
|    210     |     21000     |     21100     |      0       |      0       |  80.72205  |  73.67061  |
|    1366    |    136700     |    136800     |      5       |      0       |  85.67288  |  55.05292  |
|    1575    |    157500     |    157700     |      1       |      0       |  82.47228  |  50.07675  |
|    1675    |    167500     |    167700     |      3       |      1       |  88.99530  |  49.19417  |
|    666     |     66600     |     66700     |      3       |      1       |  27.04217  |  68.63659  |

This data is almost ready for analysis, but we want to ensure that the
data are representative of actual trends. The `FLU_total` column is
simply a sum of the biennial survey responses. So for a given
sibling-pair, one or both individuals may not have responded to the
survey indicating their vaccination status. If that’s the case, we want
to exclude those siblings to reduce **\[non-response bias\]**, by
examining the biennial responses and removing any rows with missingness.

``` r
# Take the linked data, group by the sibling pairs and
# count the number of responses for flu each year. If there is an NA,
# then data is missing for one of the years, and we omit it.
consistent_kin <- df_link %>%
  group_by(SubjectTag_S1, SubjectTag_S2) %>%
  count(
    FLU_2008_S1, FLU_2010_S1,
    FLU_2012_S1, FLU_2014_S1,
    FLU_2016_S1, FLU_2008_S2,
    FLU_2010_S2, FLU_2012_S2,
    FLU_2014_S2, FLU_2016_S2
  ) %>%
  na.omit()

# Create the flu_modeling_data object with only consistent responders.
# Clean the column names with the {janitor} package.
flu_modeling_data <- semi_join(df_link,
  consistent_kin,
  by = c(
    "SubjectTag_S1",
    "SubjectTag_S2"
  )
) %>%
  clean_names()
```

To avoid violating assumptions of independence, in our analysis we
specify that the kin-pairs should be from unique households (i.e. we
randomly select one sibling pair per household).

``` r
flu_modeling_data <- flu_modeling_data %>%
  group_by(extended_id) %>%
  slice_sample() %>%
  ungroup()
```

The data we will use for modeling now contains additional information
for each member of the kin pair, including sex and race of each
individual, flu vaccination status for the biennial survey between
2006-2016, and a total flu vaccination count for that period. The total
vaccination count ranges from 0 - 5, where 0 indicates that the
individual did not get a vaccine in any year between 2006-2016 and 5
indicates that an individual got at least 5 vaccines between 2006-2016.
Although our data set has individual years, we focused on the aggregate
as we felt that was a measure of general tendency. A subset of the data
to use in this regression looks like:

| extended_id | subject_tag_s1 | subject_tag_s2 | flu_total_s1 | flu_total_s2 | race_s1 | race_s2 | sex_s1 | sex_s2 | ses_age_40_s1 | ses_age_40_s2 |
|:-----------:|:--------------:|:--------------:|:------------:|:------------:|:-------:|:-------:|:------:|:------:|:-------------:|:-------------:|
|     17      |      1700      |      1800      |      0       |      0       |    0    |    0    |   1    |   1    |   49.26537    |   74.92440    |
|     29      |      2900      |      3000      |      2       |      0       |    0    |    0    |   0    |   0    |   56.80481    |   32.05423    |
|     37      |      3700      |      3800      |      1       |      5       |    0    |    0    |   0    |   0    |   58.55547    |   50.45408    |
|     40      |      4000      |      4100      |      2       |      0       |    0    |    0    |   1    |   1    |   78.19220    |   73.41860    |
|     58      |      5800      |      5900      |      5       |      0       |    0    |    0    |   0    |   1    |   80.56835    |   49.68414    |
|     61      |      6100      |      6200      |      3       |      4       |    0    |    0    |   0    |   0    |   74.43720    |   50.56920    |
|     67      |      6700      |      6800      |      4       |      4       |    0    |    0    |   1    |   0    |   89.67767    |   82.68649    |
|     74      |      7500      |      7600      |      0       |      0       |    0    |    0    |   0    |   1    |   88.15524    |   61.54234    |
|     83      |      8300      |      8400      |      0       |      3       |    1    |    1    |   1    |   1    |   46.41507    |   64.12765    |
|     85      |      8600      |      8700      |      0       |      4       |    1    |    1    |   0    |   1    |   45.06552    |   64.14045    |

### Modeling and Interpretation

To perform the regression using the {discord} package, we supply the
data frame and specify the outcome and predictors. It also requires a
kinship pair id, `extended_id` in our case, as well as pair identifiers
– the column name suffixes that identify to which kin a column’s values
correspond (“\_s1” and “\_s2” in our case).[²](#fn2) Optional, although
recommended, are columns containing sex and race information to control
for as additional covariates. In our case, these columns are prefixed
“race” and “sex”. Per the [pre-processing
script](https://github.com/R-Computing-Lab/discord/blob/main/data-raw/preprocess-discord-flu.R),
these columns contain dummy variables where the reference group for race
is “non-Black, non-Hispanic” and the reference group for sex is female.

By entering this information into the
[`discord_regression()`](https://r-computing-lab.github.io/discord/reference/discord_regression.md)
function, we can run the model as such:

``` r
# Setting a seed for reproducibility
set.seed(18)
flu_model_output <- discord_regression(
  data = flu_modeling_data,
  outcome = "flu_total",
  predictors = "s00_h40",
  id = "extended_id",
  sex = "sex",
  race = "race",
  pair_identifiers = c("_s1", "_s2")
)
```

The default output of
[`discord_regression()`](https://r-computing-lab.github.io/discord/reference/discord_regression.md)
is an `lm` object. The metrics for our regression can be summarized as
follows:

|      Term      | Estimate | Standard Error | T Statistic | P Value  |
|:--------------:|:--------:|:--------------:|:-----------:|:--------:|
|  (Intercept)   |  1.428   |     0.194      |    7.365    | p\<0.001 |
| flu_total_mean |  0.194   |     0.034      |    5.751    | p\<0.001 |
|  s00_h40_diff  |  0.006   |     0.002      |    2.981    | p=0.003  |
|  s00_h40_mean  |  0.002   |     0.003      |    0.873    | p=0.383  |
|     sex_1      |  -0.078  |     0.099      |   -0.789    | p=0.430  |
|     race_1     |  -0.061  |     0.104      |   -0.583    | p=0.560  |
|     sex_2      |  -0.017  |     0.099      |   -0.169    | p=0.866  |

Looking at this output, the intercept can be thought of as the average
difference in outcomes between siblings, controlling for all other
variables. That is, it estimates the expected difference for two sisters
of a non-minority ethnic background (the reference groups for sex and
race), when SES and all other predictors are equal – approximately 1.4.
The term `flu_total_mean` is essentially an extra component of the
intercept that adjusts for between-family differences in the outcome
variable. It allows the model to capture how the expected difference
between siblings changes as a function of their average level of flu
vaccinations, helping to avoid misattributing between-family differences
to within-family effects.

The next term, `s00_h40_mean`, represents the mean socioeconomic status
for the siblings. Like `flu_total_mean`, it adjusts the expected
difference to reflect between-family variation in SES. We also accounted
for sex and race, neither of which have a statistically significant
effect on the differences in flu vaccine shots between siblings
(different families) or within a sibling pair (same family).

The most important metric from the output, though, is the difference
score, `s00_h40_diff`. Here, it is statistically significant. An
interpretation of this result might be, “the difference in socioeconomic
status between siblings at age 40 is positively associated with the
difference in the number of flu vaccinations received between
2006-2016.” This finding means that a sibling with 10% higher SES is
expected to have 0.0612687 average in flu shots.

The goal of performing a discordant-kinship regression is to see whether
there is a significant difference in some behavioral measure while
controlling for as much gene-and-environmental variance as possible. In
this section, we walked through an analysis showing a statistically
significant difference in the number of flu shots a sibling received and
their socioeconomic status. From this finding, we *could not* claim the
relationship is causal. However, we cannot eliminate causality because
there are statistically significant within- and between-family
differences in our predictors and outcomes.

## Conclusion

In its current implementation, the {discord} package encourages best
practices for performing discordant-kinship regressions. For example,
the main function has the default expectation that demographic
covariates (sex and race) will be supplied. These measures are both
important covariates **when testing for causality between familial
background and psychological characteristics.**

This, and other design choices, are crucial to facilitating transparent
and reproducible results. Software ever-evolves, however, and to further
support reproducible research we plan to provide improved documentation
and allow for easier inspection of the underlying model implementation
and results.

## Acknowledgments

We acknowledge contributions from Cermet Ream, Joe Rodgers, and support
from Lucy D’Agostino McGowan on this project.

## Session Info

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31)
    #>  os       Ubuntu 24.04.3 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  C.UTF-8
    #>  ctype    C.UTF-8
    #>  tz       UTC
    #>  date     2025-12-08
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  package      * version date (UTC) lib source
    #>  backports      1.5.0   2024-05-23 [1] RSPM
    #>  broom        * 1.0.11  2025-12-04 [1] RSPM
    #>  bslib          0.9.0   2025-01-30 [1] RSPM
    #>  cachem         1.1.0   2024-05-16 [1] RSPM
    #>  cli            3.6.5   2025-04-23 [1] RSPM
    #>  desc           1.4.3   2023-12-10 [1] RSPM
    #>  digest         0.6.39  2025-11-19 [1] RSPM
    #>  discord      * 1.3     2025-12-08 [1] local
    #>  dplyr        * 1.1.4   2023-11-17 [1] RSPM
    #>  evaluate       1.0.5   2025-08-27 [1] RSPM
    #>  farver         2.1.2   2024-05-13 [1] RSPM
    #>  fastmap        1.2.0   2024-05-15 [1] RSPM
    #>  fs             1.6.6   2025-04-12 [1] RSPM
    #>  generics       0.1.4   2025-05-09 [1] RSPM
    #>  glue           1.8.0   2024-09-30 [1] RSPM
    #>  htmltools      0.5.9   2025-12-04 [1] RSPM
    #>  htmlwidgets    1.6.4   2023-12-06 [1] RSPM
    #>  janitor      * 2.2.1   2024-12-22 [1] RSPM
    #>  jquerylib      0.1.4   2021-04-26 [1] RSPM
    #>  jsonlite       2.0.0   2025-03-27 [1] RSPM
    #>  kableExtra     1.4.0   2024-01-24 [1] RSPM
    #>  knitr          1.50    2025-03-16 [1] RSPM
    #>  lifecycle      1.0.4   2023-11-07 [1] RSPM
    #>  lubridate      1.9.4   2024-12-08 [1] RSPM
    #>  magrittr     * 2.0.4   2025-09-12 [1] RSPM
    #>  NlsyLinks    * 2.2.3   2025-08-31 [1] RSPM
    #>  pillar         1.11.1  2025-09-17 [1] RSPM
    #>  pkgconfig      2.0.3   2019-09-22 [1] RSPM
    #>  pkgdown        2.2.0   2025-11-06 [1] any (@2.2.0)
    #>  purrr          1.2.0   2025-11-04 [1] RSPM
    #>  R6             2.6.1   2025-02-15 [1] RSPM
    #>  ragg           1.5.0   2025-09-02 [1] RSPM
    #>  RColorBrewer   1.1-3   2022-04-03 [1] RSPM
    #>  rlang          1.1.6   2025-04-11 [1] RSPM
    #>  rmarkdown      2.30    2025-09-28 [1] RSPM
    #>  rstudioapi     0.17.1  2024-10-22 [1] RSPM
    #>  sass           0.4.10  2025-04-11 [1] RSPM
    #>  scales         1.4.0   2025-04-24 [1] RSPM
    #>  sessioninfo    1.2.3   2025-02-05 [1] RSPM
    #>  snakecase      0.11.1  2023-08-27 [1] RSPM
    #>  stringi        1.8.7   2025-03-27 [1] RSPM
    #>  stringr        1.6.0   2025-11-04 [1] RSPM
    #>  svglite        2.2.2   2025-10-21 [1] RSPM
    #>  systemfonts    1.3.1   2025-10-01 [1] RSPM
    #>  textshaping    1.0.4   2025-10-10 [1] RSPM
    #>  tibble         3.3.0   2025-06-08 [1] RSPM
    #>  tidyr          1.3.1   2024-01-24 [1] RSPM
    #>  tidyselect     1.2.1   2024-03-11 [1] RSPM
    #>  timechange     0.3.0   2024-01-18 [1] RSPM
    #>  vctrs          0.6.5   2023-12-01 [1] RSPM
    #>  viridisLite    0.4.2   2023-05-02 [1] RSPM
    #>  withr          3.0.2   2024-10-28 [1] RSPM
    #>  xfun           0.54    2025-10-30 [1] RSPM
    #>  xml2           1.5.1   2025-12-01 [1] RSPM
    #>  yaml           2.3.11  2025-11-28 [1] RSPM
    #> 
    #>  [1] /home/runner/work/_temp/Library
    #>  [2] /opt/R/4.5.2/lib/R/site-library
    #>  [3] /opt/R/4.5.2/lib/R/library
    #>  * ── Packages attached to the search path.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────

## References

Beasley, Will, Joe Rodgers, David Bard, Michael Hunter, S. Mason
Garrison, and Kelly Meredith. 2016. *NlsyLinks: Utilities and Kinship
Information for Research with the NLSY*.
<https://CRAN.R-project.org/package=NlsyLinks>.

Hooper, Monica Webb, Anna María Nápoles, and Eliseo J. Pérez-Stable.
2020. “COVID-19 and Racial/Ethnic Disparities.” *JAMA*, May.
<https://doi.org/10.1001/jama.2020.8598>.

Rodgers, Joseph Lee, William H. Beasley, David E. Bard, Kelly M.
Meredith, Michael D. Hunter, Amber B. Johnson, Maury Buster, et al.
2016. “The NLSY Kinship Links: Using the NLSY79 and NLSY-Children Data
to Conduct Genetically-Informed and Family-Oriented Research.” *Behavior
Genetics* 46 (4): 538–51. <https://doi.org/10.1007/s10519-016-9785-3>.

Trattner, Jonathan, Later Kennon, and S. Mason Garrison. 2020. “Vaccine
Willingness and Socioeconomic Status: A Biometrically Controlled
Design.” *Behavior Genetics*, Behavior Genetics Association 50th Annual
Meeting Abstracts, 50 (6): 483–83.
<https://doi.org/10.1007/s10519-020-10018-8>.

------------------------------------------------------------------------

1.  Each variable name includes the suffix “\_S1” and “\_S2”,
    identifying the sibling to whom the value belongs. The only
    exception is the first column, which identifies the kin pair.

2.  Note these ids were previously “\_S1” and “\_S2”, however, we used
    the
    [`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
    function which coerced the column names to lowercase.
