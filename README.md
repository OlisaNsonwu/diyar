
# diyar

[![CRAN
version](http://www.r-pkg.org/badges/version/diyar)](https://cran.r-project.org/package=diyar)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/diyar)](https://www.r-pkg.org/pkg/diyar)
[![Coverage
status](https://codecov.io/gh/OlisaNsonwu/diyar/branch/master/graph/badge.svg)](https://codecov.io/github/OlisaNsonwu/diyar?branch=master)
<!-- [![Travis build status](https://travis-ci.org/OlisaNsonwu/diyar.svg?branch=master)](https://travis-ci.org/OlisaNsonwu/diyar) -->

## Overview

Record linkage and distinguishing between index, duplicate and recurrent
events are common tasks in epidemiological analyses and other fields of
research, particularly as part of a case definition. Implementing these
in `R` can be complex and challenging. The `diyar` package provides a
convenient and flexible way of doing these in `R`.

## Installation

``` r
# Install the latest CRAN release 
install.packages("diyar")

# Or, install the development version from GitHub
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

<!-- ## Usage -->
<!-- <img src = "fig_r1_light.png" width = "1000" height="580"> -->

### Number line

Use `number_line()` to create `number_line` objects - a range of numeric
values. These can be split or manipulated in several ways.

``` r
library(diyar)
nl <- number_line(1, 10); nl
#> [1] "1 -> 10"
invert_number_line(nl)
#> [1] "-1 <- -10"
seq(nl, length.out = 3)
#> [1] "1 -> 4"  "4 -> 7"  "7 -> 10"
```

`overlap()` and related functions test how `number_line` objects
overlap.

``` r
overlap_method(nl, nl); reverse(nl, nl)
#> [1] "exact"
#> [1] FALSE
nl2 <- reverse_number_line(nl); nl2
#> [1] "10 <- 1"
overlap_method(nl, nl2); reverse(nl, nl2)
#> [1] "reverse"
#> [1] TRUE
```

Set operations such as `union_number_lines()` are also possible for
pairs of `number_line` objects.

``` r
nl3 <- number_line(1, 20)
nl4 <- number_line(3, 6)
nl3; nl4
#> [1] "1 -> 20"
#> [1] "3 -> 6"
overlap_method(nl3, nl4)
#> [1] "y_inbetween_x"
intersect_number_lines(nl3, nl4)
#> [1] "3 -> 6"
subtract_number_lines(nl3, nl4)
#> $n1
#> [1] "1 -> 3"
#> 
#> $n2
#> [1] "6 -> 20"
```

### Record linkage

Use `links()` to create a unique identifier for matching records based
on a multistage deterministic approach to record linkage.

``` r
attr_1 <- c(1, 1, 1, NA, NA, NA, NA, NA)
attr_2 <- c(NA, NA, 2, 2, 2, NA, NA, NA)
links(list(attr_1, attr_2))
#> [1] "P.1 (CRI 001)" "P.1 (CRI 001)" "P.1 (CRI 001)" "P.1 (CRI 002)"
#> [5] "P.1 (CRI 002)" "P.6 (No hits)" "P.7 (No hits)" "P.8 (No hits)"
```

Use `link_records()` to implement both deterministic and probabilistic
record linkage by comparing every possible record-pair.

``` r
data(missing_staff_id)
dfr <- missing_staff_id[c("staff_id",  "initials", "hair_colour", "branch_office")]
p1 <- link_records(as.list(dfr), score_threshold = -4.2)
p1$pid
#> [1] "P.1 (CRI 001)" "P.2 (No hits)" "P.3 (No hits)" "P.4 (No hits)"
#> [5] "P.5 (No hits)" "P.1 (CRI 001)" "P.1 (CRI 001)"
subset(p1$pid_weights, record.match)
#>    sn_x sn_y cmp.staff_id cmp.initials cmp.hair_colour cmp.branch_office
#> 6     1    7            0            1               1                 1
#> 21    6    7            1            1               0                 0
#>    cmp.weight prb.staff_id prb.initials prb.hair_colour prb.branch_office
#> 6           3    -3.358454     1.074391        1.659354          1.659354
#> 21          2     1.659354     1.074391       -3.298333         -3.298333
#>    prb.weight record.match
#> 6    1.034645         TRUE
#> 21  -3.862921         TRUE
```

`links_wf_probabilistic()` is a wrapper function of `links()` and an
alternative to `link_records()`. It’s less memory intensive but can be
slower in comparison.

``` r
p2 <- links_wf_probabilistic(as.list(dfr), score_threshold = -4.2, recursive = TRUE)
p2$pid
#> [1] "P.1 (CRI 001)" "P.2 (No hits)" "P.3 (No hits)" "P.4 (No hits)"
#> [5] "P.5 (No hits)" "P.1 (CRI 001)" "P.1 (CRI 001)"
subset(p2$pid_weights, record.match)
#>   sn_x sn_y cmp.staff_id cmp.initials cmp.hair_colour cmp.branch_office
#> 1    1    1            0            1               1                 1
#> 6    6    7            1            1               0                 0
#> 7    7    1            0            1               1                 1
#>   cmp.weight prb.staff_id prb.initials prb.hair_colour prb.branch_office
#> 1          3    -4.321928     1.148392        1.733354          1.733354
#> 6          2     1.733354     1.148392       -3.298333         -3.298333
#> 7          3    -3.836501     1.148392        1.733354          1.733354
#>   prb.weight record.match
#> 1  0.2931724         TRUE
#> 6 -3.7149198         TRUE
#> 7  0.7785993         TRUE
```

### Case definitions

Use `episodes()` to create a unique identifier for related events based
on a case definition.

``` r
dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-07"), by = 1)
episodes(dates, case_length = 2, group_stats = TRUE)
#> [1] "E.1 2020-01-01 -> 2020-01-03 (C)" "E.1 2020-01-01 -> 2020-01-03 (D)"
#> [3] "E.1 2020-01-01 -> 2020-01-03 (D)" "E.4 2020-01-04 -> 2020-01-06 (C)"
#> [5] "E.4 2020-01-04 -> 2020-01-06 (D)" "E.4 2020-01-04 -> 2020-01-06 (D)"
#> [7] "E.7 2020-01-07 == 2020-01-07 (C)"
episodes(dates, case_length = 2, episode_type = "rolling", group_stats = TRUE)
#> [1] "E.1 2020-01-01 -> 2020-01-07 (C)" "E.1 2020-01-01 -> 2020-01-07 (D)"
#> [3] "E.1 2020-01-01 -> 2020-01-07 (D)" "E.1 2020-01-01 -> 2020-01-07 (R)"
#> [5] "E.1 2020-01-01 -> 2020-01-07 (D)" "E.1 2020-01-01 -> 2020-01-07 (R)"
#> [7] "E.1 2020-01-01 -> 2020-01-07 (D)"
```

Use `partitions()` to create a unique identifier for events within the
same period or numerical interval.

``` r
partitions(dates, by = 2, separate = TRUE, group_stats = TRUE)
#> [1] "PN.1 2020-01-01 -> 2020-01-02 (I)" "PN.1 2020-01-01 -> 2020-01-02 (D)"
#> [3] "PN.3 2020-01-03 -> 2020-01-04 (I)" "PN.3 2020-01-03 -> 2020-01-04 (D)"
#> [5] "PN.5 2020-01-05 -> 2020-01-07 (I)" "PN.5 2020-01-05 -> 2020-01-07 (D)"
#> [7] "PN.5 2020-01-05 -> 2020-01-07 (D)"
partitions(dates, length.out = 3, separate = TRUE, group_stats = TRUE)
#> [1] "PN.1 2020-01-01 -> 2020-01-02 (I)" "PN.1 2020-01-01 -> 2020-01-02 (D)"
#> [3] "PN.3 2020-01-03 -> 2020-01-04 (I)" "PN.3 2020-01-03 -> 2020-01-04 (D)"
#> [5] "PN.5 2020-01-05 -> 2020-01-07 (I)" "PN.5 2020-01-05 -> 2020-01-07 (D)"
#> [7] "PN.5 2020-01-05 -> 2020-01-07 (D)"
```

Find out more
[here!](https://olisansonwu.github.io/diyar/articles/overview.html)

## Bugs and issues

Please report any bug or issues with using this package
[here](https://github.com/OlisaNsonwu/diyar/issues).
