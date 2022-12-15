context("testing record_group function")

library(testthat)
library(diyar)

# Test 1 - Consistent row position for input and output
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = c(1:5)
  )

test_1 <- df
test_1$pids <- links_wf_probabilistic(attribute = df$cri_1, probabilistic = FALSE)

test_that("Matching input and ouput rows", {
  expect_equal(test_1$pids@sn, test_1$r_id)
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
})

test_that("Record identifier is as expected for one criteria", {
  expect_equal(test_1$pids@.Data, c(1,2,3,2,1))
  expect_equal(test_1$pids@pid_cri, c(1,1,0,1,1))
  expect_equal(test_1$pids@pid_total, c(2,2,1,2,2))
})

# Test 2 - Missing values
test_2b <- test_2a <- df
test_2a$cri_1 <- ifelse(test_2a$cri_1=="A", NA, test_2a$cri_1)
test_2b$cri_1 <- ifelse(test_2b$cri_1=="A", "", test_2b$cri_1)

test_2a$pids <- links_wf_probabilistic(attribute = test_2a$cri_1, probabilistic = FALSE)
test_2b$pids <- links_wf_probabilistic(attribute = test_2b$cri_1, probabilistic = FALSE)

test_that("test that test `NAs` are treated as unique record groups", {
  expect_equal(test_2a$pids@.Data, c(1,2,3,2,5))
  #expect_equal(test_2b$pids@.Data, c(1,2,3,2,5))
  expect_equal(test_2a$pids@pid_total, c(1,2,1,2,1))
  #expect_equal(test_2b$pids@pid_total, c(1,2,1,2,1))
})

# Test 3 - Decreasing order of certainty
test_3a <- data.frame(
  cri_1 = c("A","C","Z","V","F","G","G"),
  cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
  r_id = c(1L:7L),

  stringsAsFactors = FALSE
)

test_3b <- test_3a

test_3b$cri_1 <- ifelse(test_3b$r_id==7, NA, test_3b$cri_1)

test_3a$pids_1 <- links_wf_probabilistic(attribute = test_3a$cri_1, probabilistic = FALSE)
test_3a$pids_2 <- links_wf_probabilistic(attribute = test_3a$cri_2, probabilistic = FALSE)
test_3a$pids <- merge_ids(test_3a$pids_1, test_3a$pids_2)

test_3b$pids_1 <- links_wf_probabilistic(attribute = test_3b$cri_1, probabilistic = FALSE)
test_3b$pids_2 <- links_wf_probabilistic(attribute = test_3b$cri_2, probabilistic = FALSE)
test_3b$pids <- merge_ids(test_3b$pids_1, test_3b$pids_2)

test_that("test that record grouping with >1 criteria follows an order of decreasing certaintity", {
  expect_equal(test_3a$pids@.Data, c(6,2,6,4,2,6,6))
  expect_equal(test_3a$pids@pid_cri, c(2,2,2,0,2,1,1))
  expect_equal(test_3a$pids@pid_total, c(4,2,4,1,2,4,4))
  expect_equal(test_3b$pids@.Data, c(1,2,1,4,2,6,1))
  expect_equal(test_3b$pids@pid_cri, c(2,2,2, 0,2,0, 2))
  expect_equal(test_3b$pids@pid_total, c(3,2,3,1,2,1,3))
})

# Test 4 - Matching with 1 sub_criteria
df_4a <- data.frame(
  cri_1 = c(NA,NA,NA,NA,NA,2,2),
  cri_2 = rep("A1",7),
  cri_2a = c("A","B","D","X","F","G","G"),
  cri_2b = c("B","B","D","X","F","G","I"),
  cri_2c = c("A","C","A","A","F","G","I"),
  r_id = c(1L:7L),

  stringsAsFactors = FALSE
)

test_4a <- df_4a
test_4a$pids_1 <- links_wf_probabilistic(attribute = test_4a$cri_1, probabilistic = FALSE)
test_4a$pids_2 <- links_wf_probabilistic(blocking_attribute = test_4a$cri_2, attribute = list(test_4a$cri_2a, test_4a$cri_2b, test_4a$cri_2c), probabilistic = FALSE)
test_4a$pids <- merge_ids(test_4a$pids_1, test_4a$pids_2)

test_that("test record grouping with 1 set of sub-criteria per criteria", {
  expect_equal(test_4a$pids@.Data, c(1,1,1,1,5,6,6))
  expect_equal(test_4a$pids@pid_cri, c(2,2,2,2, 0,1,1))
  expect_equal(test_4a$pids@pid_total, c(4,4,4,4,1,2,2))
})

#Test 7 - Range matching
df_7 <- data.frame(
  r_id = as.integer(1:15),
  cri_1 = c(rep("P1",5), rep("P2",10)),
  age = as.integer(c(10,15,9,40,42,50,70,57,55,34,12, 35,39,20,11)),
  stringsAsFactors = TRUE
)

df_7$corrupt_range <- df_7$age_range <- number_line(df_7$age-5L, df_7$age+5L, gid = df_7$age)
df_7$corrupt_range@gid[3] <- 205L

test_7b <- test_7 <- df_7
test_7$pids <- links_wf_probabilistic(attribute = list(test_7$age_range),
                            cmp_func = range_match_legacy,
                            probabilistic = FALSE,
                            recursive = TRUE,
                            check_duplicates = TRUE)

test_7b$pids <- links_wf_probabilistic(attribute = list(test_7$age_range),
                             cmp_func = range_match_legacy,
                             probabilistic = FALSE,
                             recursive = TRUE,
                             check_duplicates = TRUE)

test_that("test record grouping using range matching in criteria", {
  # expect_equal(test_7$pids@.Data, c(1,1,1,4,4,6,7,6,6,10,11,10,10,14,11))
  expect_equal(test_7$pids@.Data, c(1, 1, 1, 4, 4, 6, 7, 6, 6, 4, 1, 4, 4, 1, 1))
  # expect_equal(test_7$pids@pid_cri, c(rep(1,6),0, rep(1, 6),0,1))
  expect_equal(test_7$pids@pid_cri, c(rep(1,6),0, rep(1, 8)))
  # expect_equal(test_7$pids@pid_total, c(3,3,3,2,2,3,1,3,3,3,2,3,3,1,2))
  expect_equal(test_7$pids@pid_total, c(6, 6, 6, 5, 5, 3, 1, 3, 3, 5, 6, 5, 5, 6, 6))
})

test_that("test record grouping using range matching in sub_criteria", {
  expect_equal(test_7b$pids@.Data, c(1,1,1,4,4,6,7,6,6,4,1,4,4,1,1))
  expect_equal(test_7b$pids@pid_cri, c(rep(1,6),0, rep(1, 8)))
  expect_equal(test_7b$pids@pid_total, c(6,6,6,5,5,3,1,3,3,5,6,5,5,6,6))
})
