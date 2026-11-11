sub_criteria <- diyar::sub_criteria
decode <- function(x) as.vector(diyar::decode(x))

# Test 1 - Consistent row position for input and output
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = c(1:5)
)
#
test_1 <- df
test_1$pids <- uat.links(criteria = df$cri_1)

test_that("basic tests", {
  expect_equal(test_1$pids@sn, test_1$r_id)
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
  expect_equal(all(same_group(test_1$pids@.Data, c(1,2,3,2,1))), TRUE)
  expect_equal(test_1$pids@pid_cri, c(1,1,0,1,1))
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
})

#
test_2a <- df
test_2a$cri_1a <- ifelse(test_2a$cri_1=="A", NA, test_2a$cri_1)
test_2a$cri_1b <- ifelse(test_2a$cri_1=="A", "", test_2a$cri_1)
test_2a$pids_a <- uat.links(criteria = test_2a$cri_1a)
test_2a$pids_b <- uat.links(criteria = test_2a$cri_1b)

test_that("handling missing data", {
  expect_equal(all(same_group(test_2a$pids_a@.Data, c(1,2,3,2,5))), TRUE)
  expect_equal(all(same_group(test_2a$pids_b@.Data, c(1,2,3,2,1))), TRUE)
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
test_3a$pids_a <- uat.links(
  criteria = list(test_3a$cri_1, test_3a$cri_2), recursive = TRUE)
test_3a$pids_b <- uat.links(
  criteria = list(test_3a$cri_1b, test_3a$cri_2), recursive = TRUE)

test_4 <- data.frame(
  cri_1 = c("A","A","A",NA,NA,NA, NA),
  cri_2 = c(NA,NA,"B","B","B",NA, NA),
  cri_3 = c(NA,NA,NA,NA, "C","C","C")
)
test_4$pids_a <- uat.links(
  criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
  repeats_allowed = FALSE, permutations_allowed = FALSE, recursive = TRUE)
test_4$pids_b <- uat.links(
  criteria = as.list(test_4[c("cri_3", "cri_2", "cri_1")]),
  repeats_allowed = FALSE, permutations_allowed = FALSE, recursive = TRUE)

test_that("handling match priority", {
  expect_equal(all(same_group(test_3a$pids_a@.Data, c(6,2,6,4,2,6,6))), TRUE)
  expect_equal(test_3a$pids_a@pid_cri, c(2,2,2,0,2,1,1))
  expect_equal(test_3a$pids_a@pid_total, c(4,2,4,1,2,4,4))
  expect_equal(all(same_group(test_3a$pids_b@.Data, c(1,2,1,4,2,6,1))), TRUE)
  expect_equal(test_3a$pids_b@pid_cri, c(2,2,2, 0,2,0, 2))
  expect_equal(test_3a$pids_b@pid_total, c(3,2,3,1,2,1,3))

  expect_equal(all(same_group(test_4$pids_a@.Data, rep(1, 7))), TRUE)
  expect_equal(test_4$pids_a@pid_cri, c(1,1,1,2,2,3,3))
  expect_equal(all(same_group(test_4$pids_b@.Data, rep(5, 7))), TRUE)
  expect_equal(test_4$pids_b@pid_cri, c(3,3,2,2,1,1,1))
})

test_4$t_sort_a <- c(2,1,2,2,2,2,2)
test_4$t_sort_b <- c(2,2,1,2,2,2,2)
test_4$pids_c <- uat.links(
  criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
  tie_sort = test_4$t_sort_a, recursive = TRUE)
test_4$pids_d <- uat.links(
  criteria = as.list(test_4[c("cri_1", "cri_2", "cri_3")]),
  tie_sort = test_4$t_sort_b, recursive = TRUE)
test_that("handling tie-sort without a sub_criteria", {
  expect_equal(all(same_group(test_4$pids_c@.Data, rep(2, 7))), TRUE)
  expect_equal(all(same_group(test_4$pids_d@.Data, rep(3, 7))), TRUE)
  expect_equal(test_4$pids_c@pid_cri, test_4$pids_c@pid_cri)
})

test_5 <- data.frame(
  cri_1 = c("A","A","A","A","A","A", NA, NA),
  cri_2 = c(1,1,1,2,2,3,3,3)
)
test_5$pids_a <- uat.links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  stepwise_method = 'shrink_to_last_match', repeats_allowed = FALSE,
  permutations_allowed = FALSE, recursive = TRUE)
test_5$pids_c <- uat.links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  stepwise_method = 'expand_with_priority', repeats_allowed = FALSE,
  permutations_allowed = FALSE, recursive = TRUE)
test_5$pids_d <- uat.links(
  criteria = as.list(test_5[c("cri_1", "cri_2")]),
  stepwise_method = 'exclude_previous_matches', repeats_allowed = FALSE,
  permutations_allowed = FALSE, recursive = TRUE)

test_that("handling group expansion vs group shrinking", {
  expect_equal(all(same_group(test_5$pids_a@.Data, c(1,1,1,4,4,6,7,8))), TRUE)
  expect_equal(test_5$pids_a@pid_cri, c(2,2,2,2,2,0,0,0))
  expect_equal(all(same_group(test_5$pids_c@.Data, c(1,1,1,1,1,1,1,1))), TRUE)
  expect_equal(test_5$pids_c@pid_cri, c(1,1,1,1,1,1,2,2))
  expect_equal(all(same_group(test_5$pids_d@.Data, c(1,1,1,1,1,1,7,7))), TRUE)
  expect_equal(test_5$pids_d@pid_cri, c(1,1,1,1,1,1,2,2))
})

attr1 <- c(1,1,1,1,1)
attr2 <- c(1,1,1,2,2)
attr3 <- c(1,1,1,4,5)

x1 <- uat.links(
  list(attr1, attr3), stepwise_method = 'shrink_to_last_match', batched = "yes", recursive = TRUE)
x2 <- uat.links(
  list(attr1, attr3), stepwise_method = 'shrink_to_last_match', batched = "no", recursive = TRUE)
x3 <- uat.links(
  list(attr1, attr3, attr1), stepwise_method = 'shrink_to_last_match', batched = "no",
  repeats_allowed = FALSE, permutations_allowed = FALSE, recursive = TRUE)
x4 <- uat.links(
  list(attr1, attr3, attr1, attr1), stepwise_method = 'shrink_to_last_match', batched = "no",
  repeats_allowed = FALSE, permutations_allowed = FALSE, recursive = TRUE)
x5 <- uat.links(
  list(attr1, attr3), stepwise_method = 'shrink_to_last_match', batched = "no", tie_sort = c(1,1,1,1,0),
  repeats_allowed = FALSE, permutations_allowed = FALSE, recursive = TRUE)

test_that("handling when `shrink` affects `batched`", {
  #`shrink` breaks group-1.
  # when `batched` is `no`, not links are available and so it isn't possible to
  # know if record 4 & 5 remain in the same group, so those records are reset
  # Corrected
  expect_equal(all(same_group(x1@.Data, c(1,1,1,4,4))), TRUE)
  expect_equal(x1@pid_cri, c(2,2,2,1,1))
  # when `batched` is `yes`, all links are available and it's possible to
  # know if record 4 & 5 can and do remain in their own group.
  # the group IDs for 4 and 5 are 'corrected' and given a unique and new ID
  expect_equal(all(same_group(x2@.Data, c(1,1,1,4,4))), TRUE)
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
  expect_equal(all(same_group(x5@.Data, c(1,1,1,4,4))), TRUE)
  expect_equal(x5@pid_cri, x2@pid_cri)
})
