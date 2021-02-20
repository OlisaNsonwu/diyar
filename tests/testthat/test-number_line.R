context("testing number line object and functions")

library(testthat)
library(diyar)

dmy <- function(x) as.Date(x, "%d/%m/%Y")

t1 <- number_line(1, 10)
t2 <- number_line(10, 1)
t3 <- number_line(5, NA)
t4 <- number_line(NA, 5)
t5 <- number_line(20, 20)

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

  expect_equal(format(number_line()), "number_line(0)")

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

  expect_equal(reverse_number_line(number_line(1000,-123)), number_line(-123, 1000))
  expect_equal(reverse_number_line(number_line(1000,-123), "increasing"), number_line(1000, -123))
  expect_equal(reverse_number_line(number_line(1000,-123), "decreasing"), number_line(-123, 1000))

  nl_2 <- nl_1 <- c(number_line(1, 10), number_line(5, 10))
  nl_1[2]@.Data <- 0
  nl_2[[1]]@start <- 10

  expect_equal(nl_1, c(number_line(1, 10), number_line(5, 5)))
  expect_equal(nl_2, c(number_line(10, 19), number_line(5, 10)))

  expect_equal(unique(c(rep(number_line(50, 200),3), number_line(5, 10))), number_line(c(50,5), c(200,10), id =c(1L, 4L), gid =c(1L, 4L)))
  expect_equal(as.number_line(2), number_line(2, 2, 1L, 1L))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2), c(number_line(1,8), number_line(8,1)))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2, "start"), c(number_line(1,6), number_line(6,5)))
  expect_equal(expand_number_line(c(number_line(3,6), number_line(6,3)), 2, "end"), c(number_line(3,8), number_line(8,3)))
  expect_equal(left_point(number_line(5, 1)), 5)
  expect_equal(right_point(number_line(5, 1)), 1)
  expect_equal(start_point(number_line(5, 1)), 1)
  expect_equal(end_point(number_line(5, 1)), 5)
  expect_equal(sort(c(number_line(5,7), number_line(2,10))), number_line(c(2,5),c(10,7), id =c(2L, 1L), gid =c(2L, 1L)))
  expect_equal(sort(c(number_line(5,7), number_line(2,10)), decreasing = TRUE), rev(number_line(c(2,5),c(10,7), id =c(2L, 1L), gid =c(2L, 1L))))
  expect_equal(number_line_width(number_line(5,10)), 5)
  expect_equal(number_line_width(number_line(25,10)), -15)
  expect_equal(shift_number_line(number_line(5,6), 2), number_line(7,8))
  expect_equal(number_line(), new("number_line"))
})

test_that("test that error and warning messages are returned correctly", {
  expect_warning(number_line(50, "200"), "`l` and `r` have different classes. They may need to be reconciled")
  expect_error(reverse_number_line(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(shift_number_line(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(shift_number_line(number_line(5,6), "A"), "Invalid object type for `by`.\ni - Valid object types are `numeric` or `integer`.\nX - You've supplied a `character` object.")
  expect_error(expand_number_line(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(expand_number_line(number_line(5,6), "A"), "Invalid object type for `by`.\ni - Valid object types are `numeric` or `integer`.\nX - You've supplied a `character` object.")
  expect_error(expand_number_line(number_line(5,6), 1, 1), "Invalid object type for `point`.\ni - Valid object types are `character`.\nX - You've supplied a `numeric` object.")
  #expect_error(expand_number_line(number_line(5,6), 1, "AA"), "Invalid values for `point`:\ni - Vaild values are \"both\", \"start\", \"end\", \"left\" or \"right\".\nX - You've supplied \"AA\" at [1].")
  expect_error(left_point(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(right_point(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(start_point(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(end_point(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(number_line_width(1), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(reverse_number_line(number_line(10,100), c("both","increasing")), "Invalid length for `direction`:\ni - Length must be 1 or the same as `x`.\ni - Expecting a length of 1.\nX - Length is 2.")
  #expect_error(reverse_number_line(number_line(10,100), "increased"), "Invalid values for `direction`:\ni - Vaild values are \"increasing\", \"decreasing\" or \"both\".\nX - You've supplied \"increased\" at [1].")
  expect_warning(number_line(50, "20A"), "`l` and `r` have different classes. They may need to be reconciled.")
  # expect_warning(number_line(50, "20A"), "NAs introduced by coercion")
  #
  expect_warning(number_line("10A", 20), "`l` and `r` have different classes. They may need to be reconciled.")
  # expect_warning(number_line("10A", 20), "NAs introduced by coercion")
})

test_that("test that error and warning messages are returned correctly", {
  expect_error(number_line(mtcars, mtcars), "'l' or 'r' aren't compatible for a `number_line` object")
  expect_error(as.number_line(mtcars), "`x` can't be coerced to a `number_line` object")
})

test_that("test series function", {
  expect_equal(number_line_sequence(number_line(1, 5), by = 1, simplify = TRUE), c(1:5))
  expect_equal(number_line_sequence(number_line(5, 1), -.5, simplify = TRUE), seq(5,1,-.5))
  expect_equal(number_line_sequence(number_line(dmy("01/04/2019"), dmy("10/04/2019")), 1, simplify = TRUE), seq(dmy("01/04/2019"), dmy("10/04/2019"), 1))
  expect_error(number_line_sequence(1, .5, simplify = TRUE), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
})

a <- c(number_line(1,3), number_line(3,3), number_line(5,3))
test_that("Convert `number_line to data.frame and vice versa`", {
  expect_equal(a, to_s4(to_df(a)))
  expect_error(to_s4(), "argument 'df' is missing, with no default")
  expect_error(to_df(), "argument 's4' is missing, with no default")
})
