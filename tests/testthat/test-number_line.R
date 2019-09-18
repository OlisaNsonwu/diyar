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

  expect_equal(diyar::reverse_number_line(diyar::number_line(1000,-123)), diyar::number_line(-123, 1000))
  expect_equal(diyar::reverse_number_line(diyar::number_line(1000,-123), "increasing"), diyar::number_line(1000, -123))
  expect_equal(diyar::reverse_number_line(diyar::number_line(1000,-123), "decreasing"), diyar::number_line(-123, 1000))

  nl_2 <- nl_1 <- c(diyar::number_line(1, 10), diyar::number_line(5, 10))
  nl_1[2]@.Data <- 0
  nl_2[[1]]@start <- 10

  expect_equal(nl_1, c(diyar::number_line(1, 10), diyar::number_line(5, 5)))
  expect_equal(nl_2, c(diyar::number_line(10, 19), diyar::number_line(5, 10)))

  expect_equal(unique(c(rep(diyar::number_line(50, 200),3), diyar::number_line(5, 10))), c(diyar::number_line(50, 200, id =1, gid =1), diyar::number_line(5, 10, id =4, gid=4)))
  expect_equal(diyar::as.number_line(2), diyar::number_line(2,2,1,1))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2), c(number_line(1,8,1,1), number_line(8,1,2,2)))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2, "start"), c(number_line(1,6,1,1), number_line(8,3,2,2)))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2, "end"), c(number_line(3,8,1,1), number_line(6,1,2,2)))
  expect_equal(left_point(diyar::number_line(5, 1)), 5)
  expect_equal(right_point(diyar::number_line(5, 1)), 1)
  expect_equal(start_point(diyar::number_line(5, 1)), 1)
  expect_equal(end_point(diyar::number_line(5, 1)), 5)

  expect_equal(shift_number_line(number_line(5,6), 2), number_line(7,8))
})

test_that("test that error and warning messages are returned correctly", {
  expect_warning(diyar::number_line(50, "200"), "'l' and 'r' have different classes. It may need to be reconciled")
  expect_error(diyar::reverse_number_line(1), "'x' is not a number_line object")
  expect_error(diyar::shift_number_line(1), "'x' is not a number_line object")
  expect_error(diyar::shift_number_line(number_line(5,6), "A"), "'by' must be a numeric based object of length 1")
  expect_error(diyar::expand_number_line(1), "'x' is not a number_line object")
  expect_error(diyar::expand_number_line(number_line(5,6), "A"), "'by' must be a numeric based object of length 1")
  expect_error(diyar::expand_number_line(number_line(5,6), 1, 1), "'point' must be a character object of length 1")
  expect_error(diyar::expand_number_line(number_line(5,6), 1, "AA"), "`point` must be either 'start','end' or 'both'")
  expect_error(diyar::left_point(1), "'x' is not a number_line object")
  expect_error(diyar::right_point(1), "'x' is not a number_line object")
  expect_error(diyar::start_point(1), "'x' is not a number_line object")
  expect_error(diyar::end_point(1), "'x' is not a number_line object")
  expect_error(diyar::reverse_number_line(number_line(10,100), c("both","increasing")), "'direction' must be a character of length 1")
  expect_error(diyar::reverse_number_line(number_line(10,100), "increased"), "`direction` must be either 'increasing', 'decreasing', or 'both'")
  expect_warning(diyar::number_line(50, "20A"), "'l' and 'r' have different classes. It may need to be reconciled")
  expect_warning(diyar::number_line(50, "20A"), "NAs introduced by coercion")

  expect_warning(diyar::number_line("10A", 20), "'l' and 'r' have different classes. It may need to be reconciled")
  expect_warning(diyar::number_line("10A", 20), "NAs introduced by coercion")
})

test_that("test that error and warning messages are returned correctly", {
  expect_error(diyar::number_line(mtcars, mtcars), "'l' or 'r' aren't compatible for a number_line object")
  expect_error(diyar::number_line(1.2, 3.1, id = NA), "'id' must be numeric")
  expect_error(diyar::as.number_line(mtcars), "'x' can't be coerced to a number_line object")

})

test_that("test series function", {
  expect_equal(diyar::number_line_sequence(number_line(1, 5)), c(1:5))
  expect_equal(diyar::number_line_sequence(number_line(5, 1), .5), seq(5,1,-.5))
  expect_equal(diyar::number_line_sequence(number_line(1, NA), .5), c(1, NA))
  expect_equal(diyar::number_line_sequence(number_line(NA, 1), .5), c(NA_real_, NA_real_))
  expect_equal(diyar::number_line_sequence(number_line(dmy("01/04/2019"), dmy("10/04/2019")), 1), seq(dmy("01/04/2019"), dmy("10/04/2019"), 1))
  expect_error(diyar::number_line_sequence(1, .5), "'x' is not a number_line object")
  expect_error(diyar::number_line_sequence(diyar::number_line(1,2), NA), "'by' must be a numeric object of length 1")
})
