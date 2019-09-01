context("testing overlap functions")

library(testthat)
library(diyar)
library(dplyr)
library(lubridate)

test_that("test overlap functions", {

  expect_equal(diyar::across(number_line(-100, 100), number_line(50, 200)), TRUE)
  expect_equal(diyar::across(number_line(-100, 50), number_line(50, 200)), FALSE)
  expect_equal(diyar::across(number_line(-100, 50), number_line(-100, 200)), FALSE)
  expect_equal(diyar::chain(number_line(-100, 50), number_line(50, 200)), TRUE)
  expect_equal(diyar::chain(number_line(-100, 50), number_line(200, 50)), FALSE)
  expect_equal(diyar::aligns_start(number_line(-100, 50), number_line(-100, 200)), TRUE)
  expect_equal(diyar::aligns_start(number_line(-100, 50), number_line(200, -100)), FALSE)
  expect_equal(diyar::aligns_start(number_line(-100, -100), number_line(200, -100)), FALSE)

  expect_equal(diyar::chain(number_line(-100, -100), number_line(200, -100)), TRUE)
  expect_equal(diyar::aligns_end(number_line(-1121, -100), number_line(200, -100)), TRUE)
  expect_equal(diyar::within(number_line(-1121, 1100), number_line(-100, 100)), TRUE)
  expect_equal(diyar::within(number_line(-1121, 1100), number_line(-100, 1100)), FALSE)

  expect_equal(diyar::overlap(number_line(-100, -100), number_line(200, -100)), TRUE)
  expect_equal(diyar::overlap(number_line(-100, 50), number_line(200, -100)), TRUE)

})


test_that("test overlap method function", {
  expect_equal(diyar::overlap_method(number_line(-100, 100), number_line(50, 200)), "across")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(50, 200)), "chain")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(-100, 200)), "aligns_start")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(200, 50)), "aligns_end")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(200, -100)), "chain,within")
  expect_equal(diyar::overlap_method(number_line(-100, -100), number_line(200, -100)), "chain,aligns_end")
  expect_equal(diyar::overlap_method(number_line(-100, -100), number_line(200, -100)), "chain,aligns_end")
  expect_equal(diyar::overlap_method(number_line(-1121, -100), number_line(200, -100)), "aligns_end")
  expect_equal(diyar::overlap_method(number_line(-1121, 1100), number_line(-100, 100)), "within")
  expect_equal(diyar::overlap_method(number_line(-1121, 1100), number_line(-100, 1100)), "aligns_end")
})

overlap_method(number_line(-100, -100), number_line(200, -100))
