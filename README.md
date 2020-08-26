
# diyar

[![CRAN
version](http://www.r-pkg.org/badges/version/diyar)](https://cran.r-project.org/package=diyar)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/diyar)](http://www.r-pkg.org/pkg/diyar)
[![Coverage
status](https://codecov.io/gh/OlisaNsonwu/diyar/branch/master/graph/badge.svg)](https://codecov.io/github/OlisaNsonwu/diyar?branch=master)
[![Travis build
status](https://travis-ci.org/OlisaNsonwu/diyar.svg?branch=master)](https://travis-ci.org/OlisaNsonwu/diyar)

## Overview

Record linkage and deduplication of individual-level data, such as
repeated spells in hospital, or recurrent cases of infection is a common
task in epidemiological analysis and other fields of research.

The `diyar` package aims to provide a simple and flexible implementation
of multistage deterministic record linkage and episode grouping.

## Installation

``` r
# Install the latest CRAN release 
install.packages("diyar")

# Or, install the development version from GitHub
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

## Cheat sheet

<a href="https://github.com/OlisaNsonwu/diyar/tree/master/cheatsheet/diyar.pdf"><img src="https://github.com/OlisaNsonwu/diyar/blob/master/cheatsheet/thumbnail.png?raw=true"/></a>

## Usage

There are two main aspects of the `diyar` package; multistage data
linkage grouping (`links()`) and episode tracking (`episodes()`).
`number_line` objects are used in both.

  - `number_line()` - creates a range of real numbers on a number line.
      - They can be manipulated - `reverse_number_line`,
        `shift_number_line`, `expand_number_line`,
        `number_line_sequence`, `invert_number_line`,
        `compress_number_line`, `union_number_lines`,
        `subtract_number_lines`, `intersect_number_lines`.
      - Logical tests for overlap - `overlaps`, `overlap`, `exact`,
        `reverse`, `chain`, `across`, `aligns_start`, `aligns_end`,
        `inbetween`.

<!-- end list -->

``` r
library(diyar)

l <- as.Date("01/04/2019", "%d/%m/%Y"); r <- as.Date("30/04/2019", "%d/%m/%Y")
nl <- number_line(l, r)
nl
#> [1] "2019-04-01 -> 2019-04-30"
reverse_number_line(nl)
#> [1] "2019-04-30 <- 2019-04-01"
expand_number_line(nl, -2)
#> [1] "2019-04-03 -> 2019-04-28"
```

  - `episodes()` - Track temporal episodes.

<!-- end list -->

``` r
data(infections);
db <- infections[c("date")]
# Dates
db$date
#>  [1] "2018-04-01" "2018-04-07" "2018-04-13" "2018-04-19" "2018-04-25"
#>  [6] "2018-05-01" "2018-05-07" "2018-05-13" "2018-05-19" "2018-05-25"
#> [11] "2018-05-31"

# Fixed episodes
episodes(date = db$date, 
         case_length = 15, 
         display = "none", 
         group_stats = TRUE)
#> Episode tracking completed in < 0.01 secs!
#>  [1] "E.01 2018-04-01 -> 2018-04-13 (C)" "E.01 2018-04-01 -> 2018-04-13 (D)"
#>  [3] "E.01 2018-04-01 -> 2018-04-13 (D)" "E.04 2018-04-19 -> 2018-05-01 (C)"
#>  [5] "E.04 2018-04-19 -> 2018-05-01 (D)" "E.04 2018-04-19 -> 2018-05-01 (D)"
#>  [7] "E.07 2018-05-07 -> 2018-05-19 (C)" "E.07 2018-05-07 -> 2018-05-19 (D)"
#>  [9] "E.07 2018-05-07 -> 2018-05-19 (D)" "E.10 2018-05-25 -> 2018-05-31 (C)"
#> [11] "E.10 2018-05-25 -> 2018-05-31 (D)"

# Rolling episodes
episodes(date = db$date, 
         case_length = 15, 
         recurrence_length = 40, 
         display = "none",
         episode_type = "rolling", 
         group_stats = TRUE)
#> Episode tracking completed in 0.55 secs!
#>  [1] "E.1 2018-04-01 -> 2018-05-31 (C)" "E.1 2018-04-01 -> 2018-05-31 (D)"
#>  [3] "E.1 2018-04-01 -> 2018-05-31 (D)" "E.1 2018-04-01 -> 2018-05-31 (R)"
#>  [5] "E.1 2018-04-01 -> 2018-05-31 (D)" "E.1 2018-04-01 -> 2018-05-31 (D)"
#>  [7] "E.1 2018-04-01 -> 2018-05-31 (D)" "E.1 2018-04-01 -> 2018-05-31 (D)"
#>  [9] "E.1 2018-04-01 -> 2018-05-31 (D)" "E.1 2018-04-01 -> 2018-05-31 (R)"
#> [11] "E.1 2018-04-01 -> 2018-05-31 (D)"
```

  - `links()` - Multistage deterministic linkage with missing data

<!-- end list -->

``` r
# Two-stage exact matches
data(staff_records);

staff_records$pids_a <- links(criteria = list(staff_records$forename, 
                                              staff_records$surname),
                              data_source = staff_records$sex, 
                              display = "none")
#> Data linkage completed in < 0.01 secs!
staff_records
#>   r_id forename  surname sex    dataset        pids_a
#> 1    1    James    Green   M Staff list P.1 (CRI 002)
#> 2    2     <NA> Anderson   M Staff list P.2 (CRI 002)
#> 3    3    Jamey    Green   M  Pay slips P.1 (CRI 002)
#> 4    4              <NA>   F  Pay slips P.4 (No Hits)
#> 5    5  Derrick Anderson   M Staff list P.2 (CRI 002)
#> 6    6  Darrack Anderson   M  Pay slips P.2 (CRI 002)
#> 7    7 Christie    Green   F Staff list P.1 (CRI 002)

# Single-stage user-defined logical tests
# Matching `sex` and + 20-year age gaps
age <- c(30, 28, 40, 25, 25, 29, 27)
sex <- c("M", "M", "M", "F", "M", "M", "F")
f1 <- function(x, y) (y - x) %in% 0:20
links(criteria = sex,
      sub_criteria = list(cr1 = sub_criteria(age, funcs = f1)),
      display = "none")
#> Data linkage completed in < 0.01 secs!
#> [1] "P.3 (CRI 001)" "P.3 (CRI 001)" "P.3 (CRI 001)" "P.7 (CRI 001)"
#> [5] "P.3 (CRI 001)" "P.3 (CRI 001)" "P.7 (CRI 001)"

# + 20-year age gaps only
links(criteria = "place_holder",
      sub_criteria = list(cr1 = sub_criteria(age, funcs = f1)),
      display = "none")
#> Data linkage completed in < 0.01 secs!
#> [1] "P.3 (CRI 001)" "P.3 (CRI 001)" "P.3 (CRI 001)" "P.3 (CRI 001)"
#> [5] "P.3 (CRI 001)" "P.3 (CRI 001)" "P.3 (CRI 001)"
```

Find out more [here\!](https://olisansonwu.github.io/diyar/index.html)

## Bugs and issues

Please report any bug or issues with using this package
[here](https://github.com/OlisaNsonwu/diyar/issues).
