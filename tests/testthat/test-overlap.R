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
  expect_equal(diyar::inbetween(number_line(-1121, 1100), number_line(-100, 100)), TRUE)
  expect_equal(diyar::inbetween(number_line(-1121, 1100), number_line(-100, 1100)), FALSE)

  expect_equal(diyar::overlap(number_line(-100, -100), number_line(200, -100)), TRUE)
  expect_equal(diyar::overlap(number_line(-100, 50), number_line(200, -100)), TRUE)

})


test_that("test overlap method function", {
  expect_equal(diyar::overlap_method(number_line(-100, 100), number_line(50, 200)), "across")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(50, 200)), "chain")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(-100, 200)), "aligns_start")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(200, 50)), "aligns_end")
  expect_equal(diyar::overlap_method(number_line(-100, 50), number_line(200, -100)), "chain,inbetween")
  expect_equal(diyar::overlap_method(number_line(-100, -100), number_line(200, -100)), "chain,aligns_end")
  expect_equal(diyar::overlap_method(number_line(-100, -100), number_line(200, -100)), "chain,aligns_end")
  expect_equal(diyar::overlap_method(number_line(-1121, -100), number_line(200, -100)), "aligns_end")
  expect_equal(diyar::overlap_method(number_line(-1121, 1100), number_line(-100, 100)), "inbetween")
  expect_equal(diyar::overlap_method(number_line(-1121, 1100), number_line(-100, 1100)), "aligns_end")
})

test_that("test that error and warning messages are returned correctly", {
  expect_error(diyar::overlap(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::overlap(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::across(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::across(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::inbetween(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::inbetween(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::aligns_start(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::aligns_start(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::aligns_end(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::aligns_end(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::chain(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::chain(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::overlap_method(1, number_line(50, 200)), "'x' is not a number_line object")
  expect_error(diyar::overlap_method(number_line(50, 200), 1), "'y' is not a number_line object")

  expect_error(diyar::overlap(number_line(-100, 100), number_line(50, 200), "overlaping"), "`method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'")
  expect_error(diyar::overlap(number_line(-100, 100), number_line(50, 200), 2), "'method' must be a character object")
})


test_that("test compress function", {
  expect_equal(diyar::compress_number_line(diyar::number_line(1, 20), diyar::number_line(5, 7)), diyar::number_line(1,20))
  expect_equal(diyar::compress_number_line(diyar::number_line(20, 1), diyar::number_line(5, 7)), diyar::number_line(1,20))
  expect_equal(diyar::compress_number_line(diyar::number_line(2, 5), diyar::number_line(3, 3), diyar::number_line(6, 7)), c(diyar::number_line(2,5, id=1), diyar::number_line(6,7, id=3)))

  #warnings and errors
  expect_error(diyar::compress_number_line(diyar::number_line(1, 20), diyar::number_line(5, 7), diyar::number_line(1,20), method = 2), "'method' must be a character object")
  expect_error(diyar::compress_number_line(diyar::number_line(1, 20), diyar::number_line(5, 7), diyar::number_line(1,20), method = "crossing"), "`method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'")
  expect_error(diyar::compress_number_line(mtcars, diyar::number_line(6, 7), diyar::number_line(3, 3)), "'...' is not a number_line object")


})
