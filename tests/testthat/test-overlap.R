context("testing overlap functions")

library(testthat)
library(diyar)

test_that("test overlap functions", {

  expect_equal(across(number_line(-100, 100), number_line(50, 200)), TRUE)
  expect_equal(across(number_line(-100, 50), number_line(50, 200)), FALSE)
  expect_equal(across(number_line(-100, 50), number_line(-100, 200)), FALSE)
  expect_equal(chain(number_line(-100, 50), number_line(50, 200)), TRUE)
  expect_equal(chain(number_line(-100, 50), number_line(200, 50)), TRUE)
  expect_equal(aligns_start(number_line(-100, 50), number_line(-100, 200)), TRUE)
  expect_equal(aligns_start(number_line(-100, 50), number_line(200, -100)), TRUE)
  expect_equal(aligns_start(number_line(-100, -100), number_line(200, -100)), TRUE)

  expect_equal(chain(number_line(-100, -100), number_line(200, -100)), FALSE)
  expect_equal(aligns_end(number_line(-1121, -100), number_line(200, -100)), FALSE)
  expect_equal(inbetween(number_line(-1121, 1100), number_line(-100, 100)), TRUE)
  expect_equal(inbetween(number_line(-1121, 1100), number_line(-100, 1100)), FALSE)

  expect_equal(overlaps(number_line(-100, -100), number_line(200, -100)), TRUE)
  expect_equal(overlaps(number_line(-100, 50), number_line(200, -100)), TRUE)

})


test_that("test overlap method function", {
  expect_equal(overlap_method(number_line(-100, 100), number_line(50, 200)), "x_across_y")
  expect_equal(overlap_method(number_line(-100, 50), number_line(50, 200)), "x_chain_y")
  expect_equal(overlap_method(number_line(-100, 50), number_line(-100, 200)), "x_aligns_start_y")
  expect_equal(overlap_method(number_line(-100, 50), number_line(200, 50)), "x_chain_y")
  expect_equal(overlap_method(number_line(-100, 50), number_line(200, -100)), "x_aligns_start_y")
  expect_equal(overlap_method(number_line(-100, -100), number_line(200, -100)), "x_aligns_start_y")
  expect_equal(overlap_method(number_line(-1121, -100), number_line(200, -100)), "x_chain_y")
  expect_equal(overlap_method(number_line(-1121, 1100), number_line(-100, 100)), "y_inbetween_x")
  expect_equal(overlap_method(number_line(-1121, 1100), number_line(-100, 1100)), "x_aligns_end_y")
})

test_that("test that error and warning messages are returned correctly", {
  expect_error(overlaps(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(overlaps(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(across(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(across(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(inbetween(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(inbetween(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(aligns_start(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(aligns_start(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(aligns_end(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(aligns_end(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(chain(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(chain(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  expect_error(overlap_method(1, number_line(50, 200)), "Invalid object type for `x`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")
  expect_error(overlap_method(number_line(50, 200), 1), "Invalid object type for `y`.\ni - Valid object type is `number_line`.\nX - You've supplied a `numeric` object.")

  # expect_error(overlaps(number_line(-100, 100), number_line(50, 200), "overlaping"), "`method` must be either 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end', 'inbetween' or 'none'")
  # expect_error(overlaps(number_line(-100, 100), number_line(50, 200), 2), "'method' must be a character object")
})


test_that("test compress function", {
  expect_equal(compress_number_line(c(number_line(1, 20), number_line(5, 7))), number_line(1,20))
  expect_equal(compress_number_line(c(number_line(20, 1), number_line(5, 7))), number_line(20,1))
  expect_equal(compress_number_line(c(number_line(2, 5), number_line(3, 3), number_line(6, 7))), number_line(c(2,6), c(5,7), id=c(1L,3L), gid =c(1L,3L)))

  #warnings and errors
  # expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), methods = 2), "Invalid object type for `methods`.\ni - Valid object types are `character`.\nX - You've supplied a `numeric` object.")
  #expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), collapse = 2), "Invalid object type for `collapse`.\ni - Valid object types are `logical (`TRUE` or `FALSE`)`.\nX - You've supplied a `numeric` object.")
  expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), collapse = rep(TRUE,4)), "Invalid length for `collapse`:\ni - Expecting a length of 1.\nX - Length is 4.")
  #expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), deduplicate ="2"), "Invalid object type for `deduplicate`.\ni - Valid object types are `logical (`TRUE` or `FALSE`)`.\nX - You've supplied a `character` object.")
  # expect_error(compress_number_line(c(number_line(1, 20), number_line(5, 7), number_line(1,20)), methods = "crossing"), "Invalid option for `methods`\ni - Valid options are \"overlap\", \"exact\", \"reverse\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\ni - Syntax 1 ~ \"aligns_end|exact...\".\ni - Syntax 2 ~ include_overlap_method(c(\"aligns_end\", \"exact\")).\ni - Syntax 3 ~ exclude_overlap_method(c(\"across\", \"chain\", \"aligns_start\", \"inbetween\")).\nX - `methods 1`: You've supplied \"crossing\" at [1].")
  expect_error(compress_number_line(mtcars, number_line(6, 7), number_line(3, 3)), "Invalid object type for `x`.\ni - Valid object types are `number_line`, `numeric` or `integer`.\nX - You've supplied a `data.frame` object.")

})

test_that("test set overlap functions", {
  expect_equal(exclude_overlap_method(""), overlap_method_codes("x_across_y|y_across_x|aligns_end|aligns_start|x_chain_y|y_chain_x|exact|x_inbetween_y|y_inbetween_x|y_aligns_start_x"))
  expect_equal(exclude_overlap_method("chain"), overlap_method_codes("across|aligns_end|aligns_start|exact|inbetween"))
  expect_equal(exclude_overlap_method(c("chain","aligns_end")), overlap_method_codes("across|aligns_start|exact|inbetween"))
  expect_equal(include_overlap_method(""), overlap_method_codes("none"))
  expect_equal(include_overlap_method("chain"), overlap_method_codes("chain"))
  expect_equal(include_overlap_method(c("chain","aligns_end")), overlap_method_codes("aligns_end|chain"))
})

nl <- diyar::number_line

test_that("test overlap method function", {
  expect_equal(overlap_method(x = nl(1, 2), y = nl(1, 5)), "x_aligns_start_y")
  expect_equal(overlap_method(x = nl(1, 2), y = nl(5, 1)), "x_aligns_start_y")
  expect_equal(overlap_method(x = nl(2, 1), y = nl(5, 1)), "x_aligns_start_y")

  expect_equal(overlap_method(y = nl(1, 2), x = nl(1, 5)), "y_aligns_start_x")
  expect_equal(overlap_method(y = nl(1, 2), x = nl(5, 1)), "y_aligns_start_x")
  expect_equal(overlap_method(y = nl(2, 1), x = nl(5, 1)), "y_aligns_start_x")

  expect_equal(overlap_method(x = nl(1, 2), y = nl(2, 5)), "x_chain_y")
  expect_equal(overlap_method(x = nl(1, 2), y = nl(5, 2)), "x_chain_y")
  expect_equal(overlap_method(x = nl(2, 1), y = nl(5, 2)), "x_chain_y")

  expect_equal(overlap_method(y = nl(1, 2), x = nl(2, 5)), "y_chain_x")
  expect_equal(overlap_method(y = nl(1, 2), x = nl(5, 2)), "y_chain_x")
  expect_equal(overlap_method(y = nl(2, 1), x = nl(5, 2)), "y_chain_x")

  expect_equal(overlap_method(x = nl(1, 2), y = nl(1, 2)), "exact")
  expect_equal(overlap_method(x = nl(2, 1), y = nl(1, 2)), "exact")

  expect_equal(overlap_method(x = nl(1, 10), y = nl(3, 5)), "y_inbetween_x")
  expect_equal(overlap_method(x = nl(10, 1), y = nl(5, 3)), "y_inbetween_x")
  expect_equal(overlap_method(x = nl(10, 1), y = nl(3, 5)), "y_inbetween_x")

  expect_equal(overlap_method(y = nl(1, 10), x = nl(3, 5)), "x_inbetween_y")
  expect_equal(overlap_method(y = nl(10, 1), x = nl(5, 3)), "x_inbetween_y")
  expect_equal(overlap_method(y = nl(10, 1), x = nl(3, 5)), "x_inbetween_y")

  expect_equal(overlap_method(x = nl(1, 5), y = nl(3, 8)), "x_across_y")
  expect_equal(overlap_method(x = nl(1, 5), y = nl(8, 3)), "x_across_y")
  expect_equal(overlap_method(x = nl(5, 1), y = nl(8, 3)), "x_across_y")

  expect_equal(overlap_method(y = nl(1, 5), x = nl(3, 8)), "y_across_x")
  expect_equal(overlap_method(y = nl(1, 5), x = nl(8, 3)), "y_across_x")
  expect_equal(overlap_method(y = nl(5, 1), x = nl(8, 3)), "y_across_x")
})
