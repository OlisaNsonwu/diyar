
Overview
--------

Record linkage and deduplication of individual-level data, such as repeated spells in hospital, or recurrent cases of infection is a common task in epidemiological analysis and other fields of research.

The `diyar` package aims to provide a simple and flexible implementation of deterministic record linkage, and episode grouping for the application of case definitions in epidemiological analysis.

Installation
------------

``` r
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

`fixed_episode()` and `rolling_episode()` are the simplest implementation this process in `diyar`.

``` r
data(infections); infections
#> # A tibble: 11 x 4
#>    rd_id date       infection epi_len
#>    <int> <date>     <chr>       <dbl>
#>  1     1 2018-04-01 BSI            15
#>  2     2 2018-04-07 UTI            15
#>  3     3 2018-04-13 UTI            15
#>  4     4 2018-04-19 UTI            15
#>  5     5 2018-04-25 BSI            15
#>  6     6 2018-05-01 UTI            15
#>  7     7 2018-05-07 BSI            15
#>  8     8 2018-05-13 BSI            15
#>  9     9 2018-05-19 RTI            15
#> 10    10 2018-05-25 RTI            15
#> 11    11 2018-05-31 BSI            15
db_a <- infections
# Fixed episodes
fixed_episodes(x = db_a$date, case_length = db_a$epi_len, display = FALSE)
#>  [1] "2018-04-01 -> 2018-04-13" "2018-04-01 -> 2018-04-13"
#>  [3] "2018-04-01 -> 2018-04-13" "2018-04-19 -> 2018-05-01"
#>  [5] "2018-04-19 -> 2018-05-01" "2018-04-19 -> 2018-05-01"
#>  [7] "2018-05-07 -> 2018-05-19" "2018-05-07 -> 2018-05-19"
#>  [9] "2018-05-07 -> 2018-05-19" "2018-05-25 -> 2018-05-31"
#> [11] "2018-05-25 -> 2018-05-31"
# Rolling episodes
rolling_episodes(x = db_a$date, case_length = db_a$epi_len, recurrence_length = 40, display = FALSE)
#>  [1] "2018-04-01 -> 2018-05-31" "2018-04-01 -> 2018-05-31"
#>  [3] "2018-04-01 -> 2018-05-31" "2018-04-01 -> 2018-05-31"
#>  [5] "2018-04-01 -> 2018-05-31" "2018-04-01 -> 2018-05-31"
#>  [7] "2018-04-01 -> 2018-05-31" "2018-04-01 -> 2018-05-31"
#>  [9] "2018-04-01 -> 2018-05-31" "2018-04-01 -> 2018-05-31"
#> [11] "2018-04-01 -> 2018-05-31"

db_b <- mutate(db_a, epid_interval= fixed_episodes(x = date, case_length = epi_len, strata = infection, display = FALSE))
db_b$epid <- db_b$epid_interval@gid
db_b
#> # A tibble: 11 x 6
#>    rd_id date       infection epi_len epid_interval             epid
#>    <int> <date>     <chr>       <dbl> <S4: number_line>        <int>
#>  1     1 2018-04-01 BSI            15 2018-04-01 == 2018-04-01     1
#>  2     2 2018-04-07 UTI            15 2018-04-07 -> 2018-04-19     2
#>  3     3 2018-04-13 UTI            15 2018-04-07 -> 2018-04-19     2
#>  4     4 2018-04-19 UTI            15 2018-04-07 -> 2018-04-19     2
#>  5     5 2018-04-25 BSI            15 2018-04-25 -> 2018-05-07     5
#>  6     6 2018-05-01 UTI            15 2018-05-01 == 2018-05-01     6
#>  7     7 2018-05-07 BSI            15 2018-04-25 -> 2018-05-07     5
#>  8     8 2018-05-13 BSI            15 2018-05-13 == 2018-05-13     8
#>  9     9 2018-05-19 RTI            15 2018-05-19 -> 2018-05-25     9
#> 10    10 2018-05-25 RTI            15 2018-05-19 -> 2018-05-25     9
#> 11    11 2018-05-31 BSI            15 2018-05-31 == 2018-05-31    11
```

`episode_group()` is a more comprehensive options and provides useful information on each episode.

``` r
db_c <- episode_group(db_a, sn=rd_id, date = date, strata = infection, case_length = epi_len, display = FALSE, group_stats = TRUE)
#> Episode grouping complete - 4 record(s) assinged a unique ID.
db_c
#> # A tibble: 11 x 6
#>       sn  epid case_nm   epid_length epid_total epid_interval           
#>    <int> <dbl> <chr>     <time>           <int> <S4: number_line>       
#>  1     1     1 Case       0 days              1 2018-04-01 == 2018-04-01
#>  2     2     2 Case      12 days              3 2018-04-07 -> 2018-04-19
#>  3     3     2 Duplicate 12 days              3 2018-04-07 -> 2018-04-19
#>  4     4     2 Duplicate 12 days              3 2018-04-07 -> 2018-04-19
#>  5     5     5 Case      12 days              2 2018-04-25 -> 2018-05-07
#>  6     6     6 Case       0 days              1 2018-05-01 == 2018-05-01
#>  7     7     5 Duplicate 12 days              2 2018-04-25 -> 2018-05-07
#>  8     8     8 Case       0 days              1 2018-05-13 == 2018-05-13
#>  9     9     9 Case       6 days              2 2018-05-19 -> 2018-05-25
#> 10    10     9 Duplicate  6 days              2 2018-05-19 -> 2018-05-25
#> 11    11    11 Case       0 days              1 2018-05-31 == 2018-05-31
```

### Record grouping

Multi-stage deterministic linkages that addresses missing values by using a specfied list of alternative matching criteria.

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
pids <- record_group(staff_records, sn = r_id, criteria = c(forename, surname),
                     data_source = sex, display = FALSE)
#> Record grouping complete - 1 record(s) assigned a group unique ID.
left_join(staff_records, pids, by=c("r_id"="sn"))
#> # A tibble: 7 x 8
#>    r_id forename surname  sex   dataset      pid pid_cri    pid_dataset
#>   <int> <chr>    <chr>    <chr> <chr>      <dbl> <chr>      <chr>      
#> 1     1 James    Green    M     Staff list     1 Criteria 2 F,M        
#> 2     2 <NA>     Anderson M     Staff list     2 Criteria 2 M          
#> 3     3 Jamey    Green    M     Pay slips      1 Criteria 2 F,M        
#> 4     4 ""       <NA>     F     Pay slips      4 None       F          
#> 5     5 Derrick  Anderson M     Staff list     2 Criteria 2 M          
#> 6     6 Darrack  Anderson M     Pay slips      2 Criteria 2 M          
#> 7     7 Christie Green    F     Staff list     1 Criteria 2 F,M
staff_records
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

# Range matching
dob <- select(staff_records, sex)
dob$age <- c(10,8,20,5,5,9,7)

# age range - age + 20 years
dob$range <- number_line(dob$age, dob$age+20, gid=dob$age)
bind_cols(dob, record_group(dob, criteria = sex, sub_criteria = list(s1a="range"), display = FALSE))
#> Record grouping complete - 1 record(s) assigned a group unique ID.
#> # A tibble: 7 x 6
#>   sex     age range                sn   pid pid_cri   
#>   <chr> <dbl> <S4: number_line> <int> <dbl> <chr>     
#> 1 M        10 10 -> 30              1     2 Criteria 1
#> 2 M         8 8 -> 28               2     2 Criteria 1
#> 3 M        20 20 -> 40              3     2 Criteria 1
#> 4 F         5 5 -> 25               4     4 Criteria 1
#> 5 M         5 5 -> 25               5     5 None      
#> 6 M         9 9 -> 29               6     2 Criteria 1
#> 7 F         7 7 -> 27               7     4 Criteria 1

# age range - age +- 20 years
dob$range <- number_line(dob$age-20, dob$age+20, gid=dob$age)
bind_cols(dob, record_group(dob, criteria = sex, sub_criteria = list(s1a="range"), display = FALSE))
#> Record grouping complete - 0 record(s) assigned a group unique ID.
#> # A tibble: 7 x 6
#>   sex     age range                sn   pid pid_cri   
#>   <chr> <dbl> <S4: number_line> <int> <int> <chr>     
#> 1 M        10 -10 -> 30             1     1 Criteria 1
#> 2 M         8 -12 -> 28             2     1 Criteria 1
#> 3 M        20 0 -> 40               3     1 Criteria 1
#> 4 F         5 -15 -> 25             4     4 Criteria 1
#> 5 M         5 -15 -> 25             5     1 Criteria 1
#> 6 M         9 -11 -> 29             6     1 Criteria 1
#> 7 F         7 -13 -> 27             7     4 Criteria 1
```

Find out more [here](https://olisansonwu.github.io/diyar/index.html)!.
