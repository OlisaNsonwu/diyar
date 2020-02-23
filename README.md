
diyar
=====

[![CRAN version](http://www.r-pkg.org/badges/version/diyar)](https://cran.r-project.org/package=diyar) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/diyar)](http://www.r-pkg.org/pkg/diyar) [![Coverage status](https://codecov.io/gh/OlisaNsonwu/diyar/branch/master/graph/badge.svg)](https://codecov.io/github/OlisaNsonwu/diyar?branch=master) [![Travis build status](https://travis-ci.org/OlisaNsonwu/diyar.svg?branch=master)](https://travis-ci.org/OlisaNsonwu/diyar)

Overview
--------

Record linkage and deduplication of individual-level data, such as repeated spells in hospital, or recurrent cases of infection is a common task in epidemiological analysis and other fields of research.

The `diyar` package aims to provide a simple and flexible implementation of deterministic record linkage and episode grouping for the application of case definitions in epidemiological analysis.

Installation
------------

``` r
# Install the latest CRAN release 
install.packages("diyar")

# Or, install the development version from GitHub
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

Cheat sheet
-----------

<a href="https://github.com/OlisaNsonwu/diyar/tree/master/cheatsheet/diyar2.pdf"><img src="https://github.com/OlisaNsonwu/diyar/blob/master/cheatsheet/thumbnail2.png?raw=true"/></a>

Usage
-----

There are two main aspects of the `diyar` package; multistage record grouping (`record_group()`) and episode grouping (`fixed_episodes()`, `rolling_episodes()` and `episode_group()`) for applying case definitions in epidemiological analysis. `number_line` objects are used for both.

-   `number_line` objects - series of real numbers on a number line. These can be manipulated and merged.

``` r
library(diyar)

l <- as.Date("01/04/2019", "%d/%m/%Y"); r <- as.Date("30/04/2019", "%d/%m/%Y")
nl <- number_line(l, r)
nl
#> [1] "2019-04-01 -> 2019-04-30"
reverse_number_line(nl)
#> [1] "2019-04-30 <- 2019-04-01"
shift_number_line(nl, -2)
#> [1] "2019-03-30 -> 2019-04-28"
expand_number_line(nl, 2)
#> [1] "2019-03-30 -> 2019-05-02"
number_line_sequence(nl, by =3)
#> [[1]]
#>  [1] "2019-04-01" "2019-04-04" "2019-04-07" "2019-04-10" "2019-04-13"
#>  [6] "2019-04-16" "2019-04-19" "2019-04-22" "2019-04-25" "2019-04-28"
#> [11] "2019-04-30"
```

-   `fixed_episodes()`, `rolling_episodes()` and `episode_group()` - Group records into chronological episodes. ***NOTE; `to_s4` and `to_s4()` changes their output from a data.frame (current default) to `epid` objects. `epid` objects will be the default output in the next release.***

``` r
data(infections);
db <- infections[c("date")]
db$date
#>  [1] "2018-04-01" "2018-04-07" "2018-04-13" "2018-04-19" "2018-04-25"
#>  [6] "2018-05-01" "2018-05-07" "2018-05-13" "2018-05-19" "2018-05-25"
#> [11] "2018-05-31"

# Fixed episodes
db$f_epid <- fixed_episodes(date = db$date, case_length = 15, 
                              display = FALSE, to_s4 = TRUE, group_stats = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.

# Rolling episodes
db$r_epid <- rolling_episodes(date = db$date, case_length = 15, 
                              recurrence_length = 40, display = FALSE, to_s4 = TRUE, 
                              group_stats = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.
db[c("f_epid","r_epid")]
#> # A tibble: 11 x 2
#>    f_epid                               r_epid                            
#>    <epid>                               <epid>                            
#>  1 E-01 2018-04-01 -> 2018-04-13 (C-01) E-1 2018-04-01 -> 2018-05-31 (C-1)
#>  2 E-01 2018-04-01 -> 2018-04-13 (D-01) E-1 2018-04-01 -> 2018-05-31 (D-1)
#>  3 E-01 2018-04-01 -> 2018-04-13 (D-01) E-1 2018-04-01 -> 2018-05-31 (D-1)
#>  4 E-04 2018-04-19 -> 2018-05-01 (C-04) E-1 2018-04-01 -> 2018-05-31 (R-1)
#>  5 E-04 2018-04-19 -> 2018-05-01 (D-04) E-1 2018-04-01 -> 2018-05-31 (D-1)
#>  6 E-04 2018-04-19 -> 2018-05-01 (D-04) E-1 2018-04-01 -> 2018-05-31 (D-1)
#>  7 E-07 2018-05-07 -> 2018-05-19 (C-07) E-1 2018-04-01 -> 2018-05-31 (D-7)
#>  8 E-07 2018-05-07 -> 2018-05-19 (D-07) E-1 2018-04-01 -> 2018-05-31 (R-7)
#>  9 E-07 2018-05-07 -> 2018-05-19 (D-07) E-1 2018-04-01 -> 2018-05-31 (D-7)
#> 10 E-10 2018-05-25 -> 2018-05-31 (C-10) E-1 2018-04-01 -> 2018-05-31 (D-7)
#> 11 E-10 2018-05-25 -> 2018-05-31 (D-10) E-1 2018-04-01 -> 2018-05-31 (D-7)
```

-   `record_group()` - Perform multistage deterministic linkages while addressing missing data using a specified list of alternative matching criteria or matching range of values. ***NOTE; `to_s4` and `to_s4()` changes the output from a data.frame (current default) to `pid` objects. `pid` objects will be the default output in the next release.***

``` r
# Two stages of record grouping
data(staff_records);

staff_records$pids_a <- record_group(staff_records, sn = r_id, criteria = c(forename, surname),
                     data_source = sex, display = FALSE, to_s4 = TRUE)
#> Record grouping complete - 1 record(s) assigned a group unique ID.
staff_records
#> # A tibble: 7 x 6
#>    r_id forename surname  sex   dataset    pids_a      
#>   <int> <chr>    <chr>    <chr> <chr>      <pid>       
#> 1     1 James    Green    M     Staff list P-1 (CRI 02)
#> 2     2 <NA>     Anderson M     Staff list P-2 (CRI 02)
#> 3     3 Jamey    Green    M     Pay slips  P-1 (CRI 02)
#> 4     4 ""       <NA>     F     Pay slips  P-4 (No Hit)
#> 5     5 Derrick  Anderson M     Staff list P-2 (CRI 02)
#> 6     6 Darrack  Anderson M     Pay slips  P-2 (CRI 02)
#> 7     7 Christie Green    F     Staff list P-1 (CRI 02)
```

Find out more [here](https://olisansonwu.github.io/diyar/index.html)!

Bugs and issues
---------------

Please report any bug or issues with using this package [here](https://github.com/OlisaNsonwu/diyar/issues).
