
# diyar

[![CRAN
version](http://www.r-pkg.org/badges/version/diyar)](https://cran.r-project.org/package=diyar)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/diyar)](https://www.r-pkg.org/pkg/diyar)
[![Coverage
status](https://codecov.io/gh/OlisaNsonwu/diyar/branch/master/graph/badge.svg)](https://codecov.io/github/OlisaNsonwu/diyar?branch=master)
[![Travis build
status](https://travis-ci.org/OlisaNsonwu/diyar.svg?branch=master)](https://travis-ci.org/OlisaNsonwu/diyar)

## Installation

``` r
# Install the latest CRAN release 
install.packages("diyar")

# Or, install the development version from GitHub
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

## Overview

`diyar` is an `R` package for linking records with shared
characteristics. The linked records represent an entity, which depending
on the context of the analysis can be unique patients, infection
episodes, overlapping periods of care, clusters or other occurrences as
defined by a case definition. This makes it useful in ordinarily complex
analyses such as record linkage,  
contact or network analyses e.t.c.

The main functions are `links()`, `episodes()` and `partitions()`. They
are flexible in regards to how they compare records, as well as what are
considered matches. Their functionality can sometimes overlap however,
each is better suited to particular use cases:

-   `links()` - link records with no relevance to an index record. For
    example, deterministic record linkage
-   `episodes()` - link records in relation to an index record. For
    example, contact and network analysis.  
-   `partitions()` - link records in relation to a fixed interval.

## links()

**Key features;**

-   multi-stage record linkage. Here, multiple linkage criteria are
    assessed in a specified order of priority.

``` r
library(diyar)
data(missing_staff_id)
dfr_stages <- missing_staff_id[c("age", "hair_colour", "branch_office")]
priority_order_1 <- c("hair_colour", "branch_office")
priority_order_2 <- c("branch_office", "hair_colour")

dfr_stages$id.1 <- links(criteria = as.list(dfr_stages[priority_order_1]))
dfr_stages$id.2 <- links(criteria = as.list(dfr_stages[priority_order_2]))
```

-   create and use complex rules for record matching. This is done with
    a `sub_criteria()`.

``` r
sub.cri.1 <- sub_criteria(
  hair.color = dfr_stages$hair_colour,
  age = dfr_stages$age,
  match_funcs = c(
    "exact" = exact_match,
    "age.range" = range_match)
)
last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
not_equal <- function(x, y) x != y
sub.cri.2 <- sub_criteria(
  dfr_stages$branch_office, 
  dfr_stages$age,
  match_funcs = c(
    "last.word" = last_word_cmp,
    "not.equal" = not_equal)
)
sub.cri.3 <- sub_criteria(sub.cri.1, sub.cri.2, operator = "and")
sub.cri.1
#> {
#> exact(hair.color) OR age.range(age)
#> }
sub.cri.2
#> {
#> last.word(Republic of Ghana,France ...) OR not.equal(30,30 ...)
#> }
sub.cri.3
#> {
#>   {
#>   exact(hair.color) OR age.range(age)
#>   } AND 
#>   {
#>   last.word(Republic of Ghana,France ...) OR not.equal(30,30 ...)
#>   }
#> }
dfr_stages$id.3 <- links(
  criteria = "place_holder",
  sub_criteria = list("cr1" = sub.cri.3)
)
dfr_stages
#>   age hair_colour     branch_office          id.1          id.2          id.3
#> 1  30       Brown Republic of Ghana P.1 (CRI 001) P.1 (CRI 001) P.1 (CRI 001)
#> 2  30        Teal            France P.4 (CRI 002) P.2 (CRI 001) P.2 (CRI 001)
#> 3  30        <NA>              <NA> P.3 (No hits) P.3 (No hits) P.3 (No hits)
#> 4  30       Green              <NA> P.4 (CRI 001) P.2 (CRI 002) P.4 (No hits)
#> 5  30       Green            France P.4 (CRI 001) P.2 (CRI 001) P.2 (CRI 001)
#> 6  30  Dark brown             Ghana P.6 (No hits) P.6 (No hits) P.1 (CRI 001)
#> 7  30       Brown Republic of Ghana P.1 (CRI 001) P.1 (CRI 001) P.1 (CRI 001)
```

There are variations of `links()` like `links_wf_probabilistic()` and
`links_sv_probabilistic()` for specific use cases such as probabilistic
record linkage.

## episodes()

**Key features;**

-   link records within a specified period from an index record.

``` r
dfr_2 <- data.frame(date = as.Date("2020-01-01") + c(1:5, 10:15, 20:25))
dfr_2$id.1 <- episodes(
  date = dfr_2$date, case_length = 2,
  episodes_max = 1)
```

-   change the index record.

``` r
dfr_2$pref <- c(rep(2, 8), 1, rep(2, 8))
dfr_2$id.2 <- episodes(
  date = dfr_2$date, case_length = number_line(-2, 2),
  episodes_max = 1, 
  custom_sort = dfr_2$pref)
```

-   add a recurrence period

``` r
dfr_2$id.3 <- episodes(
  date = dfr_2$date, case_length = number_line(-2, 2), 
  episode_type = "rolling", recurrence_length = 1,
  episodes_max = 1, rolls_max = 1)
```

-   link overlapping periods

``` r
dfr_2$period <- number_line(dfr_2$date, dfr_2$date + 5)
dfr_2$id.4 <- episodes(
  date = dfr_2$period, case_length = index_window(dfr_2$period),
  episodes_max = 1)
dfr_2
#>          date     id.1 pref     id.2     id.3                   period     id.4
#> 1  2020-01-02 E.01 (C)    2 E.01 (S) E.01 (C) 2020-01-02 -> 2020-01-07 E.01 (C)
#> 2  2020-01-03 E.01 (D)    2 E.02 (S) E.01 (D) 2020-01-03 -> 2020-01-08 E.01 (D)
#> 3  2020-01-04 E.01 (D)    2 E.03 (S) E.01 (D) 2020-01-04 -> 2020-01-09 E.01 (D)
#> 4  2020-01-05 E.04 (S)    2 E.04 (S) E.01 (R) 2020-01-05 -> 2020-01-10 E.01 (D)
#> 5  2020-01-06 E.05 (S)    2 E.05 (S) E.05 (S) 2020-01-06 -> 2020-01-11 E.01 (D)
#> 6  2020-01-11 E.06 (S)    2 E.06 (S) E.06 (S) 2020-01-11 -> 2020-01-16 E.06 (S)
#> 7  2020-01-12 E.07 (S)    2 E.09 (D) E.07 (S) 2020-01-12 -> 2020-01-17 E.07 (S)
#> 8  2020-01-13 E.08 (S)    2 E.09 (D) E.08 (S) 2020-01-13 -> 2020-01-18 E.08 (S)
#> 9  2020-01-14 E.09 (S)    1 E.09 (C) E.09 (S) 2020-01-14 -> 2020-01-19 E.09 (S)
#> 10 2020-01-15 E.10 (S)    2 E.09 (D) E.10 (S) 2020-01-15 -> 2020-01-20 E.10 (S)
#> 11 2020-01-16 E.11 (S)    2 E.09 (D) E.11 (S) 2020-01-16 -> 2020-01-21 E.11 (S)
#> 12 2020-01-21 E.12 (S)    2 E.12 (S) E.12 (S) 2020-01-21 -> 2020-01-26 E.12 (S)
#> 13 2020-01-22 E.13 (S)    2 E.13 (S) E.13 (S) 2020-01-22 -> 2020-01-27 E.13 (S)
#> 14 2020-01-23 E.14 (S)    2 E.14 (S) E.14 (S) 2020-01-23 -> 2020-01-28 E.14 (S)
#> 15 2020-01-24 E.15 (S)    2 E.15 (S) E.15 (S) 2020-01-24 -> 2020-01-29 E.15 (S)
#> 16 2020-01-25 E.16 (S)    2 E.16 (S) E.16 (S) 2020-01-25 -> 2020-01-30 E.16 (S)
#> 17 2020-01-26 E.17 (S)    2 E.17 (S) E.17 (S) 2020-01-26 -> 2020-01-31 E.17 (S)
```

There are variations of `episodes()` like `episodes_wf_splits()` for
specific use cases such as more efficient handling of duplicate records.

## partitions()

**Key features;**

-   link all records within a specific periods in time

``` r
dfr_3 <- dfr_2["date"]
dfr_3$id.1 <- partitions(
  date = dfr_3$date, 
  window = number_line(as.Date(c("2020-01-10", "2020-01-17")), 
                       as.Date(c("2020-01-12", "2020-01-24")))
  )
```

-   link all records within a splits of an interval

``` r
dfr_3$id.2 <- partitions(date = dfr_3$date, by = 3, separate = TRUE) 
dfr_3$id.3 <- partitions(date = dfr_3$date, length.out = 3, separate = TRUE)
dfr_3
#>          date      id.1      id.2      id.3
#> 1  2020-01-02 PN.01 (S) PN.01 (I) PN.01 (I)
#> 2  2020-01-03 PN.02 (S) PN.01 (D) PN.01 (D)
#> 3  2020-01-04 PN.03 (S) PN.01 (D) PN.01 (D)
#> 4  2020-01-05 PN.04 (S) PN.04 (I) PN.01 (D)
#> 5  2020-01-06 PN.05 (S) PN.04 (D) PN.01 (D)
#> 6  2020-01-11 PN.06 (I) PN.06 (I) PN.06 (I)
#> 7  2020-01-12 PN.06 (D) PN.06 (D) PN.06 (D)
#> 8  2020-01-13 PN.08 (S) PN.06 (D) PN.06 (D)
#> 9  2020-01-14 PN.09 (S) PN.09 (I) PN.06 (D)
#> 10 2020-01-15 PN.10 (S) PN.09 (D) PN.06 (D)
#> 11 2020-01-16 PN.11 (S) PN.09 (D) PN.06 (D)
#> 12 2020-01-21 PN.06 (D) PN.12 (I) PN.12 (I)
#> 13 2020-01-22 PN.06 (D) PN.12 (D) PN.12 (D)
#> 14 2020-01-23 PN.06 (D) PN.14 (I) PN.12 (D)
#> 15 2020-01-24 PN.06 (D) PN.14 (D) PN.12 (D)
#> 16 2020-01-25 PN.16 (S) PN.14 (D) PN.12 (D)
#> 17 2020-01-26 PN.17 (S) PN.14 (D) PN.12 (D)
```

Other useful functions include in the `diyar` package are `combi()` and
`sets()`.

Find out more!

-   number_line and overlaps - `vignette("number_line")`
-   Introduction to epidemiological case definitions with diyar -
    `vignette("episodes")`
-   Introduction to record linkage with diyar - `vignette("links")`
-   Divvy up events with partitions - `vignette("panes")`

## Bugs and issues

Please report any bug or issues with using this package
[here](https://github.com/OlisaNsonwu/diyar/issues).
