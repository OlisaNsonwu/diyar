
diyar
=====

[![CRAN version](http://www.r-pkg.org/badges/version/diyar)](https://cran.r-project.org/package=diyar) [![Coverage status](https://codecov.io/gh/OlisaNsonwu/diyar/branch/master/graph/badge.svg)](https://codecov.io/github/OlisaNsonwu/diyar?branch=master) [![Travis build status](https://travis-ci.org/OlisaNsonwu/diyar.svg?branch=master)](https://travis-ci.org/OlisaNsonwu/diyar)

Overview
--------

Record linkage and deduplication of individual-level data, such as repeated spells in hospital, or recurrent cases of infection is a common task in epidemiological analysis and other fields of research.

The `diyar` package aims to provide a simple and flexible implementation of deterministic record linkage, and episode grouping for the application of case definitions in epidemiological analysis.

Installation
------------

``` r
# Install the latest CRAN release 
install.packages("diyar")

# Or, install the development version from GitHub
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

Usage
-----

There are two main aspects of the `diyar` package; record and episode grouping. Additionally, `number_line` objects are used in both as representations of a range of values to match, and time periods respectively.

### Number line objects

Series of real numbers on a number line. `diyar` also includes functions used to manipulate these objects. Some useful ones are shown below.

``` r
library(diyar)
library(dplyr)

l <- as.Date("01/04/2019", "%d/%m/%Y")
r <- as.Date("30/04/2019", "%d/%m/%Y")
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
#>  [1] "2019-04-01" "2019-04-04" "2019-04-07" "2019-04-10" "2019-04-13"
#>  [6] "2019-04-16" "2019-04-19" "2019-04-22" "2019-04-25" "2019-04-28"
#> [11] "2019-04-30"
```

### Episode grouping

Group records into chronological episodes for the purpose of record deduplication and implementing case definitions in epidemiological analysis.

`fixed_episodes()`, `rolling_episodes()` and `episode_group()` - Currently, these returns a `data.frame` but will be changed to `epid` objects in the next major release. The `id` and `epid` columns/slots are the record and episode identifiers respectively.

***NOTE. `to_s4` and `to_s4()` changes their output from a data.frame (current default) to `epid` objects.***

``` r
data(infections);
db <- infections[c("date","epi_len")]
db$recur <- 40
db
#> # A tibble: 11 x 3
#>    date       epi_len recur
#>    <date>       <dbl> <dbl>
#>  1 2018-04-01      15    40
#>  2 2018-04-07      15    40
#>  3 2018-04-13      15    40
#>  4 2018-04-19      15    40
#>  5 2018-04-25      15    40
#>  6 2018-05-01      15    40
#>  7 2018-05-07      15    40
#>  8 2018-05-13      15    40
#>  9 2018-05-19      15    40
#> 10 2018-05-25      15    40
#> 11 2018-05-31      15    40

# Fixed episodes
db$f_epid <- fixed_episodes(date = db$date, case_length = db$epi_len, 
                              display = FALSE, to_s4 = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.

# Rolling episodes
db$r_epid <- rolling_episodes(date = db$date, case_length = 15, 
                                recurrence_length = 40, display = FALSE, to_s4 = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.
db
#> # A tibble: 11 x 5
#>    date       epi_len recur f_epid   r_epid 
#>    <date>       <dbl> <dbl> <epid>   <epid> 
#>  1 2018-04-01      15    40 E-01 (C) E-1 (C)
#>  2 2018-04-07      15    40 E-01 (D) E-1 (D)
#>  3 2018-04-13      15    40 E-01 (D) E-1 (D)
#>  4 2018-04-19      15    40 E-04 (C) E-1 (R)
#>  5 2018-04-25      15    40 E-04 (D) E-1 (D)
#>  6 2018-05-01      15    40 E-04 (D) E-1 (D)
#>  7 2018-05-07      15    40 E-07 (C) E-1 (D)
#>  8 2018-05-13      15    40 E-07 (D) E-1 (D)
#>  9 2018-05-19      15    40 E-07 (D) E-1 (D)
#> 10 2018-05-25      15    40 E-10 (C) E-1 (R)
#> 11 2018-05-31      15    40 E-10 (D) E-1 (D)

# episode_group() takes column names
db$f_epid.2 <- episode_group(db, date = date, case_length = epi_len, episode_type = "fixed", 
                              display = FALSE, to_s4 = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.

db$r_epid.2 <- episode_group(db, date = date, case_length = epi_len, episode_type = "rolling", 
                              recurrence_length = recur, display = FALSE, to_s4 = TRUE)
#> Episode grouping complete - 0 record(s) assinged a unique ID.
db
#> # A tibble: 11 x 7
#>    date       epi_len recur f_epid   r_epid  f_epid.2 r_epid.2
#>    <date>       <dbl> <dbl> <epid>   <epid>  <epid>   <epid>  
#>  1 2018-04-01      15    40 E-01 (C) E-1 (C) E-01 (C) E-1 (C) 
#>  2 2018-04-07      15    40 E-01 (D) E-1 (D) E-01 (D) E-1 (D) 
#>  3 2018-04-13      15    40 E-01 (D) E-1 (D) E-01 (D) E-1 (D) 
#>  4 2018-04-19      15    40 E-04 (C) E-1 (R) E-04 (C) E-1 (R) 
#>  5 2018-04-25      15    40 E-04 (D) E-1 (D) E-04 (D) E-1 (D) 
#>  6 2018-05-01      15    40 E-04 (D) E-1 (D) E-04 (D) E-1 (D) 
#>  7 2018-05-07      15    40 E-07 (C) E-1 (D) E-07 (C) E-1 (D) 
#>  8 2018-05-13      15    40 E-07 (D) E-1 (D) E-07 (D) E-1 (D) 
#>  9 2018-05-19      15    40 E-07 (D) E-1 (D) E-07 (D) E-1 (D) 
#> 10 2018-05-25      15    40 E-10 (C) E-1 (R) E-10 (C) E-1 (R) 
#> 11 2018-05-31      15    40 E-10 (D) E-1 (D) E-10 (D) E-1 (D)
```

### Record grouping

Multistage deterministic linkages that addresses missing values by using a specified list of alternative matching criteria.

`record_group()` - Currently, this returns a `data.frame` but will be changed to `pid` objects in the next major release. The `id` and `pid` columns/slots are the record and group identifiers respectively.

***NOTE. `to_s4` and `to_s4()` changes its output from a data.frame (current default) to `pid` objects.***

``` r
# Two or more stages of record grouping
data(staff_records); staff_records
#> # A tibble: 7 x 5
#>    r_id forename surname  sex   dataset   
#>   <int> <chr>    <chr>    <chr> <chr>     
#> 1     1 James    Green    M     Staff list
#> 2     2 <NA>     Anderson M     Staff list
#> 3     3 Jamey    Green    M     Pay slips 
#> 4     4 ""       <NA>     F     Pay slips 
#> 5     5 Derrick  Anderson M     Staff list
#> 6     6 Darrack  Anderson M     Pay slips 
#> 7     7 Christie Green    F     Staff list

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

# Range matching
dob <- select(staff_records, sex); dob
#> # A tibble: 7 x 1
#>   sex  
#>   <chr>
#> 1 M    
#> 2 M    
#> 3 M    
#> 4 F    
#> 5 M    
#> 6 M    
#> 7 F

dob$age <- c(10,8,20,5,5,9,7)

# age range - age + 20 years
dob$rng_b <- number_line(dob$age, dob$age+20, gid=dob$age)
# age range - age +- 20 years
dob$rng_c <- number_line(dob$age-20, dob$age+20, gid=dob$age)

dob$pids_b <- record_group(dob, criteria = sex, sub_criteria = list(s1a="rng_b"), display = FALSE, to_s4 = TRUE)
#> Record grouping complete - 1 record(s) assigned a group unique ID.
dob$pids_c <- record_group(dob, criteria = sex, sub_criteria = list(s1a="rng_c"), display = FALSE, to_s4 = TRUE)
#> Record grouping complete - 0 record(s) assigned a group unique ID.

dob
#> # A tibble: 7 x 6
#>   sex     age rng_b      rng_c      pids_b       pids_c      
#>   <chr> <dbl> <numbr_ln> <numbr_ln> <pid>        <pid>       
#> 1 M        10 10 -> 30   -10 -> 30  P-2 (CRI 01) P-1 (CRI 01)
#> 2 M         8 8 -> 28    -12 -> 28  P-2 (CRI 01) P-1 (CRI 01)
#> 3 M        20 20 -> 40   0 -> 40    P-2 (CRI 01) P-1 (CRI 01)
#> 4 F         5 5 -> 25    -15 -> 25  P-4 (CRI 01) P-4 (CRI 01)
#> 5 M         5 5 -> 25    -15 -> 25  P-5 (No Hit) P-1 (CRI 01)
#> 6 M         9 9 -> 29    -11 -> 29  P-2 (CRI 01) P-1 (CRI 01)
#> 7 F         7 7 -> 27    -13 -> 27  P-4 (CRI 01) P-4 (CRI 01)
```

Find out more [here](https://olisansonwu.github.io/diyar/index.html)!

Bugs and issues
---------------

Please report any bug or issues with using this package [here](https://github.com/OlisaNsonwu/diyar/issues).
