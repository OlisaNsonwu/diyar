
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

## Usage

<img src = "figures/fig_r1.png" width = "1000" height="580">

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
#> [1] "inbetween"
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

`links_wf_probabilistic()` is a wrapper function of `links()` to
implement probabilistic record linkage.

``` r
data(missing_staff_id)
dfr <- missing_staff_id[c("staff_id",  "initials", "hair_colour", "branch_office")]
links_wf_probabilistic(as.list(dfr), score_threshold = -4.2)
#> $pid
#> [1] "P.1 (CRI 001)" "P.2 (No hits)" "P.3 (No hits)" "P.4 (No hits)"
#> [5] "P.5 (No hits)" "P.6 (No hits)" "P.1 (CRI 001)"
#> 
#> $pid_weights
#>      sn_x sn_y cmp.staff_id cmp.initials cmp.hair_colour cmp.branch_office
#> [1,]    1    7            0            1               1                 1
#> [2,]    2    2           NA           NA              NA                NA
#> [3,]    3    3           NA           NA              NA                NA
#> [4,]    4    4           NA           NA              NA                NA
#> [5,]    5    5           NA           NA              NA                NA
#> [6,]    6    6           NA           NA              NA                NA
#> [7,]    7    7            1            1               1                 1
#>      cmp.weight cmp.threshold prb.staff_id prb.initials prb.hair_colour
#> [1,]          3            NA    -4.321928     1.148392        1.733354
#> [2,]         NA            NA           NA           NA              NA
#> [3,]         NA            NA           NA           NA              NA
#> [4,]         NA            NA           NA           NA              NA
#> [5,]         NA            NA           NA           NA              NA
#> [6,]         NA            NA           NA           NA              NA
#> [7,]          4            NA     1.733354     1.148392        1.733354
#>      prb.branch_office prb.weight prb.threshold
#> [1,]          1.733354  0.2931724             1
#> [2,]                NA         NA            NA
#> [3,]                NA         NA            NA
#> [4,]                NA         NA            NA
#> [5,]                NA         NA            NA
#> [6,]                NA         NA            NA
#> [7,]          1.733354  6.3484549             1
```

### Episode tracking

Use `episodes()` or `episodes_wf_splits()` to create a unique identifier
for related events based on a case definition.

``` r
episodes(1:7, case_length = 2)
#> [1] "E.1 (C)" "E.1 (D)" "E.1 (D)" "E.4 (C)" "E.4 (D)" "E.4 (D)" "E.7 (C)"
episodes(1:7, case_length = 2, episode_type = "rolling")
#> [1] "E.1 (C)" "E.1 (D)" "E.1 (D)" "E.1 (R)" "E.1 (D)" "E.1 (R)" "E.1 (D)"
```

Use `partitions()` to create a unique identifier for events within the
same time or numerical interval.

``` r
partitions(1:7, by = 2, separate = TRUE)
#> [1] "PN.1 (I)" "PN.1 (D)" "PN.3 (I)" "PN.3 (D)" "PN.5 (I)" "PN.5 (D)" "PN.5 (D)"
partitions(1:7, length.out = 3, separate = TRUE)
#> [1] "PN.1 (I)" "PN.1 (D)" "PN.3 (I)" "PN.3 (D)" "PN.5 (I)" "PN.5 (D)" "PN.5 (D)"
```

Find out more [here\!](https://olisansonwu.github.io/diyar/index.html)

## Bugs and issues

Please report any bug or issues with using this package
[here](https://github.com/OlisaNsonwu/diyar/issues).
