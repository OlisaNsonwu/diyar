context("testing make_ids function")

library(testthat)
library(diyar)

tst_1a <- make_ids(x_pos = c(2,4), y_pos = c(4,5))
tst_1b <- make_ids(y_pos = c(2,4), x_pos = c(4,5))

tst_2a <- make_ids(x_pos = c(2,7), y_pos = c(7,5))
tst_2b <- make_ids(y_pos = c(2,7), x_pos = c(7,5))

tst_3a <- make_ids(x_pos = c(1, 1, 1, 4, 4, 4, 5, 5, 5),
                   y_pos = c(2, 3, 4, 5, 6, 7, 10, 11, 7))
tst_3b <- make_ids(x_pos = c(1, 1, 1, 4, 4, 4, 5, 5, 5),
                   y_pos = c(2, 3, 4, 5, 6, 7, 10, 11, 7))

tst_4a <- make_ids(x_pos = 10, y_pos = 10)
tst_4b <- make_ids(y_pos = 10, x_pos = 10)


tst_5a <- make_ids(x_pos = c(1, 1, 2, 3, 4, 6),
                   y_pos = c(6, 7, 5, 4, 5, 7),
                   id_length = 7)
tst_5b <- make_ids(y_pos = c(1, 1, 2, 3, 4, 6),
                   x_pos = c(6, 7, 5, 4, 5, 7),
                   id_length = 7)

test_that("", {
  expect_equal(tst_1a$sn, 1:5)
  expect_equal(tst_1a$linked, c(FALSE, TRUE, FALSE, TRUE, TRUE))
  # expect_equal(tst_1a$link_id, c(1, 2, 3, 2, 2))
  expect_equal(tst_1a$group_id, c(1, 2, 3, 2, 2))

  expect_equal(tst_1a$sn, tst_1a$sn)
  expect_equal(tst_1a$linked, tst_1b$linked)
  # expect_equal(tst_1a$link_id, tst_1b$link_id)
  expect_equal(tst_1a$group_id, tst_1b$group_id)
})

test_that("", {
  expect_equal(tst_2a$sn, 1:7)
  expect_equal(tst_2a$linked, c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))
  # expect_equal(tst_2a$link_id, c(1, 2, 3, 4, 2, 6, 2))
  expect_equal(tst_2a$group_id, c(1, 2, 3, 4, 2, 6, 2))

  expect_equal(tst_2a$sn, tst_2a$sn)
  expect_equal(tst_2a$linked, tst_2b$linked)
  # expect_equal(tst_2a$link_id, tst_2b$link_id)
  expect_equal(tst_2a$group_id, tst_2b$group_id)
})

test_that("", {
  expect_equal(tst_3a$sn, 1:11)
  expect_equal(tst_3a$linked, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,TRUE))
  # Needs more work
  # expect_equal(tst_3a$link_id, c(1, 1, 1, 1, 4, 4, 4, 8, 9, 5, 5))
  expect_equal(tst_3a$group_id, c(1, 1, 1, 1, 1, 1, 1, 8, 9, 1, 1))

  expect_equal(tst_3a$sn, tst_3a$sn)
  expect_equal(tst_3a$linked, tst_3b$linked)
  # expect_equal(tst_3a$link_id, tst_3b$link_id)
  expect_equal(tst_3a$group_id, tst_3b$group_id)
})

test_that("", {
  expect_equal(tst_4a$sn, 1:10)
  expect_equal(tst_4a$linked, rep(FALSE, 10))
  # expect_equal(tst_4a$link_id, 1:10)
  expect_equal(tst_4a$group_id, 1:10)

  expect_equal(tst_4a$sn, tst_4a$sn)
  expect_equal(tst_4a$linked, tst_4b$linked)
  # expect_equal(tst_4a$link_id, tst_4b$link_id)
  expect_equal(tst_4a$group_id, tst_4b$group_id)
})

test_that("", {
  expect_equal(tst_5a$sn, 1:7)
  expect_equal(tst_5a$linked, rep(TRUE, 7))
  #expect_equal(tst_5a$link_id, c(1, 2, 3, 3, 2, 1, 1))
  expect_equal(tst_5a$group_id, c(1, rep(2, 4), 1, 1))

  expect_equal(tst_5a$sn, tst_5a$sn)
  expect_equal(tst_5a$linked, tst_5b$linked)
  # expect_equal(tst_5a$link_id, tst_5b$link_id)
  expect_equal(tst_5a$group_id, tst_5b$group_id)
})
