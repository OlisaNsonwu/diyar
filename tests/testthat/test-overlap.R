context("testing overlap functions")

library(testthat)
library(diyar)

test_that("test overlap functions", {

  expect_equal(across(number_line(-100, 100), number_line(50, 200)), TRUE)
  expect_equal(across(number_line(-100, 50), number_line(50, 200)), FALSE)
  expect_equal(across(number_line(-100, 50), number_line(-100, 200)), FALSE)
  expect_equal(chain(number_line(-100, 50), number_line(50, 200)), TRUE)
  expect_equal(chain(number_line(-100, 50), number_line(200, 50)), FALSE)
  expect_equal(aligns_start(number_line(-100, 50), number_line(-100, 200)), TRUE)
  expect_equal(aligns_start(number_line(-100, 50), number_line(200, -100)), FALSE)
  expect_equal(aligns_start(number_line(-100, -100), number_line(200, -100)), FALSE)

  expect_equal(chain(number_line(-100, -100), number_line(200, -100)), FALSE)
  expect_equal(aligns_end(number_line(-1121, -100), number_line(200, -100)), TRUE)
  expect_equal(inbetween(number_line(-1121, 1100), number_line(-100, 100)), TRUE)
  expect_equal(inbetween(number_line(-1121, 1100), number_line(-100, 1100)), FALSE)

  expect_equal(overlaps(number_line(-100, -100), number_line(200, -100)), TRUE)
  expect_equal(overlaps(number_line(-100, 50), number_line(200, -100)), TRUE)

})


test_that("test overlap method function", {
  expect_equal(overlap_method(number_line(-100, 100), number_line(50, 200)), "across")
  expect_equal(overlap_method(number_line(-100, 50), number_line(50, 200)), "chain")
  expect_equal(overlap_method(number_line(-100, 50), number_line(-100, 200)), "aligns_start")
  expect_equal(overlap_method(number_line(-100, 50), number_line(200, 50)), "aligns_end")
  expect_equal(overlap_method(number_line(-100, 50), number_line(200, -100)), "none")
  expect_equal(overlap_method(number_line(-100, -100), number_line(200, -100)), "aligns_end")
  expect_equal(overlap_method(number_line(-1121, -100), number_line(200, -100)), "aligns_end")
  expect_equal(overlap_method(number_line(-1121, 1100), number_line(-100, 100)), "inbetween")
  expect_equal(overlap_method(number_line(-1121, 1100), number_line(-100, 1100)), "aligns_end")
})

test_that("test that error and warning messages are returned correctly", {
  expect_error(overlaps(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(overlaps(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(across(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(across(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(inbetween(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(inbetween(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(aligns_start(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(aligns_start(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(aligns_end(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(aligns_end(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(chain(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(chain(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(overlap_method(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(overlap_method(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object types are `number_line`.\nX - You've supplied a `numeric` object.")

  # expect_error(overlaps(number_line(-100, 100), number_line(50, 200), "overlaping"), "`method` must be either 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end', 'inbetween' or 'none'")
  # expect_error(overlaps(number_line(-100, 100), number_line(50, 200), 2), "'method' must be a character object")
})


test_that("test compress function", {
  expect_equal(compress_number_line(c(number_line(1, 20), number_line(5, 7))), number_line(1,20))
  expect_equal(compress_number_line(c(number_line(20, 1), number_line(5, 7))), number_line(20,1))
  expect_equal(compress_number_line(c(number_line(2, 5), number_line(3, 3), number_line(6, 7))), number_line(c(2,6), c(5,7), id=c(1,3), gid =c(1,3)))

  #warnings and errors
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), methods = 2), "Invalid object type for `methods`.\ni - Valid object types are `character`.\nX - You've supplied a `numeric` object.")
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), collapse = 2), "Invalid object type for `collapse`.\ni - Valid object types are `logical`.\nX - You've supplied a `numeric` object.")
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), collapse = rep(TRUE,4)), "Invalid length for `collapse`:\ni - Expecting a length of 1.\nX - Length is 4.")
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), deduplicate ="2"), "Invalid object type for `deduplicate`.\ni - Valid object types are `logical`.\nX - You've supplied a `character` object.")
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), methods = "crossing"), "Invalid option for `methods`\ni - Valid options are \"overlap\", \"exact\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\ni - Syntax 1 ~ \"aligns_end|exact...\".\ni - Syntax 2 ~ include_overlap_method(c(\"aligns_end\", \"exact\")).\ni - Syntax 3 ~ exclude_overlap_method(c(\"across\", \"chain\", \"aligns_start\", \"inbetween\")).\nX - `overlap_methods` 1`: You've supplied \"crossing\" at [1].")
  expect_error(compress_number_line(mtcars, number_line(6, 7), number_line(3, 3)), "Invalid object type for `x`.\ni - Valid object types are `number_line`, `numeric` or `integer`.\nX - You've supplied a `data.frame` object.")

})

test_that("test set overlap functions", {
  expect_equal(exclude_overlap_method(""),"exact|across|chain|aligns_start|aligns_end|inbetween")
  expect_equal(exclude_overlap_method("chain"),"exact|across|aligns_start|aligns_end|inbetween")
  expect_equal(exclude_overlap_method(c("chain","aligns_end")),"exact|across|aligns_start|inbetween")
  expect_equal(include_overlap_method(""),"")
  expect_equal(include_overlap_method("chain"),"chain")
  expect_equal(include_overlap_method(c("chain","aligns_end")),"chain|aligns_end")

})
