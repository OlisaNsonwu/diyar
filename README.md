# Overview

Record linkage and deduplication of individual-level data, such as repeated spells in hospital, or recurrent cases of infection is a common task in epidemiological analysis and other fields of research. 

The \code{diyar} package aims to provide a simple and flexible way implementation of deterministic record linkage, episode grouping for the application of epidemiological case definitions to datasets.

# Installation

```r
install.packages("devtools")
devtools::install_github("OlisaNsonwu/diyar")
```

## Usage
There are two main aspects of \code{diyar}; record and episode grouping. Additionally, \code{number_line} objects are used in both aspects representations of range of values and time periods respectively. 

## Number line objects
Series of real numbers on a number line. \code{diyar} also includes functions used to manipulate these objects. Some useful ones are shown below.
```r
l <- as.Date("01/04/2019", "%d/%m/%Y")
r <- as.Date("30/04/2019", "%d/%m/%Y")
nl <- number_line(l, r)
reverse_number_line(nl)
shift_number_line(nl, -2)
expand_number_line(nl, 2)
series(nl)
```

## Episode grouping
The simplest implementation of episode grouping in \code{diyar} is the use of \code{compress_number_line()}.

```r
dates <- seq.Date(l, r, by = "3 days")
episode_length <- 7
nl_2 <- number_line(dates, dates + episode_length)
compress_number_line(nl_2, deduplicate = TRUE)
```
This package has two functions; `record_group()` and `episode_group()`. 
Please visit [here](https://olisansonwu.github.io/diyar/index.html) for an introduction to both.
