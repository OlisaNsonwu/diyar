context("testing number line object and functions")

library(testthat)
library(diyar)
library(dplyr)
library(lubridate)

t1 <- diyar::number_line(1, 10)
t2 <- diyar::number_line(10, 1)
t3 <- diyar::number_line(5, NA)
t4 <- diyar::number_line(NA, 5)
t5 <- diyar::number_line(20, 20)

test_that("test direction of number line", {

  expect_equal(t1@.Data, 9)
  expect_equal(t2@.Data, -9)
  expect_equal(t3@.Data, NA_real_)
  expect_equal(t4@.Data, NA_real_)
  expect_equal(t5@.Data, 0)

  expect_equal(t1@start, 1)
  expect_equal(t2@start, 10)
  expect_equal(t3@start, 5)
  expect_equal(t4@start, NA)
  expect_equal(t5@start, 20)

  expect_equal(show(t1), "1 -> 10")
  expect_equal(show(t2), "10 <- 1")
  expect_equal(show(t3), "5 ?? NA")
  expect_equal(show(t4), "NA ?? NA")
  expect_equal(show(t5), "20 == 20")

   s1<- t1$start
   s2<- t2$start
   s3<- t3$start
   s4<- t4$start
   s5<- t5$start

  expect_equal(s1, 1)
  expect_equal(s2, 10)
  expect_equal(s3, 5)
  expect_equal(s4, NA)
  expect_equal(s5, 20)

})


t1$.Data <- -t1$.Data
t2$start <- t2$start + 10
t3$.Data <- 0

t4$start <- t4$.Data <- 5
t5$.Data <- -10

test_that("test changing the number line", {

  expect_equal(t1@.Data, -9)
  expect_equal(t2@.Data, -9)
  expect_equal(t3@.Data, 0)
  expect_equal(t4@.Data, 5)
  expect_equal(t5@.Data, -10)

  expect_equal(t1@start, 1)
  expect_equal(t2@start, 20)
  expect_equal(t3@start, 5)
  expect_equal(t4@start, 5)
  expect_equal(t5@start, 20)

  expect_equal(show(t1), "1 <- -8")
  expect_equal(show(t2), "20 <- 11")
  expect_equal(show(t3), "5 == 5")
  expect_equal(show(t4), "5 -> 10")
  expect_equal(show(t5), "20 <- 10")

  expect_equal(show(rep(t1, 3))[1:2], rep("1 <- -8", 2))
  expect_equal(show(rep(t2, 3))[1:2], rep("20 <- 11", 2))
  expect_equal(show(rep(t3, 3))[1:2], rep("5 == 5", 2))
  expect_equal(show(rep(t4, 3))[1:2], rep("5 -> 10", 2))
  expect_equal(show(rep(t5, 3))[1:2], rep("20 <- 10", 2))

  expect_equal(show(t5[[1:2]]), c("20 <- 10", "NA ?? NA" ))

  expect_equal(diyar::swap(diyar::number_line(1000,-123)), diyar::number_line(-123, 1000))
})

