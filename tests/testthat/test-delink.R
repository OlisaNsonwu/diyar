context("testing record_group function")

library(testthat)
library(diyar)

ep <- episodes(1:8)
unlinked_ep <- delink(ep, ep@sn %in% c(3, 8))

pd <- links(list(c(1, 1, 1, NA, NA),
                 c(NA, NA, 2, 2, 2)))
unlinked_pd <- delink(pd, pd@pid_cri == 1)

test_that("", {
  expect_equal(ep@.Data, c(1, 1, 1, 1, 1, 1, 1, 1))
  expect_equal(unlinked_ep@.Data, c(1, 1, 3, 1, 1, 1, 1, 8))

  expect_equal(pd@.Data, c(1, 1, 1, 1, 1))
  expect_equal(unlinked_pd@.Data, c(1, 2, 3, 1, 1))
})
