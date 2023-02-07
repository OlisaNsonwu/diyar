context("testing record_group function")

library(testthat)
library(diyar)

# Test 1 - Consistent row position for input and output
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = c(1:5)
)

links <- function(..., group_stats = T){
  diyar::links(..., group_stats = group_stats, display = "none", recursive = TRUE)
}
sub_criteria <- diyar::sub_criteria
decode <- function(x) as.vector(diyar::decode(x))

#
test_1 <- df
test_1$pids <- links(
  criteria = df$cri_1,
  group_stats = TRUE,
  recursive = TRUE,
  display = "none")

test_that("basic tests", {
  expect_equal(test_1$pids@sn, test_1$r_id)
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
  expect_equal(test_1$pids@.Data, c(1,2,3,2,1))
  expect_equal(test_1$pids@pid_cri, c(1,1,0,1,1))
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
})

#
test_2a <- df
test_2a$cri_1a <- ifelse(test_2a$cri_1=="A", NA, test_2a$cri_1)
test_2a$cri_1b <- ifelse(test_2a$cri_1=="A", "", test_2a$cri_1)
test_2a$pids_a <- links(criteria = test_2a$cri_1a)
test_2a$pids_b <- links(criteria = test_2a$cri_1b)

test_that("handling missing data", {
  expect_equal(test_2a$pids_a@.Data, c(1,2,3,2,5))
  expect_equal(test_2a$pids_b@.Data, c(1,2,3,2,1))
})

#
test_3a <- data.frame(
  cri_1 = c("A","C","Z","V","F","G","G"),
  cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
  r_id = c(1L:7L),
  stringsAsFactors = FALSE
)

test_3a <- test_3a
test_3a$cri_1b <-test_3a$cri_1
test_3a$cri_1b[test_3a$r_id==7] <- NA
test_3a$pids_a <- links(
  criteria = list(test_3a$cri_1, test_3a$cri_2),
  recursive = TRUE)
test_3a$pids_b <- links(
  criteria = list(test_3a$cri_1b, test_3a$cri_2),
  recursive = TRUE)

test_4 <- data.frame(
  cri_1 = c("A","A","A",NA,NA,NA, NA),
  cri_2 = c(NA,NA,"B","B","B",NA, NA),
  cri_3 = c(NA,NA,NA,NA, "C","C","C")
)
test_4$pids_a <- links(criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
                       repeats_allowed = FALSE,
                       permutations_allowed = FALSE,
                       recursive = TRUE)
test_4$pids_b <- links(criteria = as.list(test_4[c("cri_3", "cri_2", "cri_1")]),
                       repeats_allowed = FALSE,
                       permutations_allowed = FALSE,
                       recursive = TRUE)

test_that("handling match priority", {
  expect_equal(test_3a$pids_a@.Data, c(6,2,6,4,2,6,6))
  expect_equal(test_3a$pids_a@pid_cri, c(2,2,2,0,2,1,1))
  expect_equal(test_3a$pids_a@pid_total, c(4,2,4,1,2,4,4))
  expect_equal(test_3a$pids_b@.Data, c(1,2,1,4,2,6,1))
  expect_equal(test_3a$pids_b@pid_cri, c(2,2,2, 0,2,0, 2))
  expect_equal(test_3a$pids_b@pid_total, c(3,2,3,1,2,1,3))

  expect_equal(test_4$pids_a@.Data, rep(1, 7))
  expect_equal(test_4$pids_a@pid_cri, c(1,1,1,2,2,3,3))
  expect_equal(test_4$pids_b@.Data, rep(5, 7))
  expect_equal(test_4$pids_b@pid_cri, c(3,3,2,2,1,1,1))
})

test_4$t_sort_a <- c(2,1,2,2,2,2,2)
test_4$t_sort_b <- c(2,2,1,2,2,2,2)
test_4$pids_c <- links(
  criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
  tie_sort = test_4$t_sort_a,
  recursive = TRUE
  )
test_4$pids_d <- links(
  criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
  tie_sort = test_4$t_sort_b,
  recursive = TRUE
)
test_that("handling tie-sort without a sub_criteria", {
  expect_equal(test_4$pids_c@.Data, rep(2, 7))
  expect_equal(test_4$pids_d@.Data, rep(3, 7))
  expect_equal(test_4$pids_c@pid_cri, test_4$pids_c@pid_cri)
})

test_5 <- data.frame(
  cri_1 = c("A","A","A","A","A","A", NA, NA),
  cri_2 = c(1,1,1,2,2,3,3,3)
)
test_5$pids_a <- links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  shrink = TRUE,
  expand = TRUE,
  repeats_allowed = FALSE,
  permutations_allowed = FALSE,
  recursive = TRUE)
test_5$pids_b <- links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  shrink = TRUE,
  expand = FALSE,
  repeats_allowed = FALSE,
  permutations_allowed = FALSE,
  recursive = TRUE)
test_5$pids_c <- links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  shrink = FALSE,
  expand = TRUE,
  repeats_allowed = FALSE,
  permutations_allowed = FALSE,
  recursive = TRUE)
test_5$pids_d <- links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  shrink = FALSE,
  expand = FALSE,
  repeats_allowed = FALSE,
  permutations_allowed = FALSE,
  recursive = TRUE)

test_that("handling group expansion vs group shrinking", {
  expect_equal(test_5$pids_a@.Data, c(1,1,1,4,4,6,7,8))
  expect_equal(test_5$pids_a@pid_cri, c(2,2,2,2,2,0,0,0))
  # `shrink = TRUE & expand = TRUE` is not possible based on their functionality and
  # should leads to the same outcome as `shrink = TRUE & expand = FALSE`.
  expect_equal(test_5$pids_a@.Data, test_5$pids_b@.Data)
  expect_equal(test_5$pids_a@pid_cri, test_5$pids_b@pid_cri)
  expect_equal(test_5$pids_c@.Data, c(1,1,1,1,1,1,1,1))
  expect_equal(test_5$pids_c@pid_cri, c(1,1,1,1,1,1,2,2))
  expect_equal(test_5$pids_d@.Data, c(1,1,1,1,1,1,7,7))
  expect_equal(test_5$pids_d@pid_cri, c(1,1,1,1,1,1,2,2))
})

attr1 <- c(1,1,1,1,1)
attr2 <- c(1,1,1,2,2)
attr3 <- c(1,1,1,4,5)

x1 <- links(list(attr1, attr3),
            shrink = TRUE, batched = "yes",
            recursive = TRUE)
x2 <- links(list(attr1, attr3),
            shrink = TRUE, batched = "no",
            recursive = TRUE)
x3 <- links(list(attr1, attr3, attr1),
            shrink = TRUE,batched = "no",
            repeats_allowed = FALSE,
            permutations_allowed = FALSE,
            recursive = TRUE)
x4 <- links(list(attr1, attr3, attr1, attr1),
            shrink = TRUE,
            batched = "no", repeats_allowed = FALSE,
            permutations_allowed = FALSE,
            recursive = TRUE)
x5 <- links(list(attr1, attr3), shrink = TRUE,
            batched = "no", tie_sort = c(1,1,1,1,0),
            repeats_allowed = FALSE,
            permutations_allowed = FALSE,
            recursive = TRUE)

tmp.func.1 <- function(x, y) x == 1
tmp.func.2 <- function(x, y) x == y
x6 <- links(list("p1", "p1"),
                sub_criteria = list(
                  cr1 = sub_criteria(attr1, match_funcs = tmp.func.1),
                  cr2 = sub_criteria(attr2, match_funcs = tmp.func.1)),
                shrink = TRUE, batched = "no",
                repeats_allowed = FALSE,
                permutations_allowed = FALSE,
            recursive = TRUE)

x7 <- links(list("p1", "p1"),
                sub_criteria = list(
                  cr1 = sub_criteria(attr1, match_funcs = tmp.func.2),
                  cr2 = sub_criteria(attr2, match_funcs = tmp.func.2)),
                shrink = TRUE, batched = "no",
                repeats_allowed = FALSE,
                permutations_allowed = FALSE,
            recursive = TRUE)

test_that("handling when `shrink` affects `batched`", {
  #`shrink` breaks group-1.
  # when `batched` is `no`, not links are available and so it isn't possible to
  # know if record 4 & 5 remain in the same group, so those records are reset
  expect_equal(x1@.Data, c(1,1,1,4,5))
  expect_equal(x1@pid_cri, c(2,2,2,0,0))
  # when `batched` is `yes`, all links are available and it's possible to
  # know if record 4 & 5 can and do remain in their own group.
  # the group IDs for 4 and 5 are 'corrected' and given a unique and new ID
  expect_equal(x2@.Data, c(1,1,1,6,6))
  expect_equal(x2@pid_cri, c(2,2,2,1,1))
  # record 4 & 5 can and do remain a group but will not be included in all subsequent iterations.
  # They remain linked at criteria 2 until the end
  expect_equal(x3@.Data, x2@.Data)
  expect_equal(x4@.Data, x3@.Data)
  expect_equal(x3@pid_cri, c(3,3,3,1,1))
  expect_equal(x4@pid_cri, c(4,4,4,1,1))
  # `shrink` breaks breaks group-1
  # record 4 & 5 can and do remain in their own group
  # however, the reference for 1 and 5 (which is 5) is not broken
  # so a correction is not required and a new ID is not created
  expect_equal(x5@.Data, c(1,1,1,5,5))
  expect_equal(x5@pid_cri, x2@pid_cri)

  # `shrink` breaks breaks group-1 via a sub_criteria
  # match_func is in a way that the second half can't remain as a group
  expect_equal(x6@.Data, x1@.Data)
  expect_equal(x6@pid_cri, x1@pid_cri)
  # exact scenario but
  # match_func is in a way that the second half can still remain as a group
  expect_equal(x7@.Data, c(1,1,1,4,4))
  expect_equal(x7@pid_cri, c(2,2,2,2,2))
})

btwn <- function(x, l, r){
  x >= l & x <= r
}
f2 <- function(x, y){
  btwn(abs(x$num - y$num), 0, 40)
}
sb.cri <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10),
  match_funcs = f2,
  equal_funcs = diyar::false
)
d1 <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri),
  recursive = FALSE)

d2 <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri),
  recursive = TRUE)

test_that("test `recursive` linkage", {
  expect_equal(d1@.Data, c(1,1,1,1,1,6,6,6,6,6))
  expect_equal(d1@pid_cri, rep(1, 10))
  expect_equal(d2@.Data, rep(1, 10))
  expect_equal(d2@pid_cri, d1@pid_cri)
})

f2.v2 <- function(x, y){
  btwn(abs(x$num - y$num), 0, 50)
}
sb.cri.v2 <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10),
  match_funcs = f2.v2,
  equal_funcs = diyar::false
)

pd1a <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  batched = "yes")

pd1b <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  batched = "semi")

pd1c <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  batched = "no")

test_that("test `batched` linkage", {
  expect_equal(pd2a@.Data, rep(1, 10))
  expect_equal(pd1a@.Data, pd1b@.Data)
  expect_equal(pd1b@.Data, pd1c@.Data)
  expect_equal(max(pd1a@iteration) > max(pd1b@iteration), TRUE)
  expect_equal(max(pd1b@iteration) > max(pd1c@iteration), TRUE)
  expect_equal(max(pd1a@iteration), 10)
  expect_equal(max(pd1b@iteration), 4)
  expect_equal(max(pd1c@iteration), 1)
})

f2.v3 <- function(x, y){
  btwn(x$num - y$num, 0, 50)
}
sb.cri.v3 <- sub_criteria(
  attrs(id = 1:10, num = 1:10 * 10),
  match_funcs = f2.v3,
  equal_funcs = diyar::false
)
t_sort_v3 <- c(1,1,1,1,1,0,0,0,0,0)
pd2a <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  tie_sort = t_sort_v3,
  batched = "yes",
  recursive = TRUE)

pd2b <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  tie_sort = t_sort_v3,
  batched = "semi",
  recursive = TRUE)

pd2c <- links(
  criteria = "p1",
  sub_criteria = list("cr1" = sb.cri.v2),
  recursive = TRUE,
  tie_sort = t_sort_v3,
  batched = "no",
  recursive = TRUE)

test_that("test `batched` with `tie_sort`", {
  # should be the same as with no `tie_sort`
  expect_equal(pd2a@.Data, rep(6, 10))
  expect_equal(pd2a@.Data, pd2b@.Data)
  expect_equal(pd2b@.Data, pd2c@.Data)
  expect_equal(max(pd2a@iteration) > max(pd2b@iteration), TRUE)
  expect_equal(max(pd2b@iteration) > max(pd2c@iteration), TRUE)
  expect_equal(max(pd2a@iteration), 10)
  expect_equal(max(pd2b@iteration), 3)
  expect_equal(max(pd2c@iteration), 1)
})
