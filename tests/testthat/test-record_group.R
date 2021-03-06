context("testing record_group function")

library(testthat)
library(diyar)

record_group <- function(df, ...){
  diyar::record_group(df, display = "none", ...)
}
decode <- function(x) as.vector(diyar::decode(x))
# Test 1 - Consistent row position for input and output
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = as.integer(c(1:5))
)

test_1 <- df
test_1$pids <- record_group(df, criteria=cri_1, group_stats=TRUE)

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
test_2a$cri_1 <- ifelse(test_2a$cri_1=="A", NA, test_2a$cri_1 )
test_2b$cri_1 <- ifelse(test_2b$cri_1=="A", "", test_2b$cri_1 )

test_2a$pids <- record_group(test_2a, criteria = cri_1, group_stats = TRUE)
test_2b$pids <- record_group(test_2b, criteria = cri_1, group_stats = TRUE)

test_that("test that test blank space or NAs criteria are treated as unique record groups", {
  expect_equal(test_2a$pids@.Data, c(1,2,3,2,5))
 # expect_equal(test_2b$pids@.Data, c(1,2,3,2,5))
  expect_equal(test_2a$pids@pid_total, c(1,2,1,2,1))
  #expect_equal(test_2b$pids@pid_total, c(1,2,1,2,1))
})

# Test 3 - Decreasing order of certainty
test_3a <- data.frame(
  cri_1 = c("A","C","Z","V","F","G","G"),
  cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
  r_id = as.integer(c(1:7)),

  stringsAsFactors = FALSE
)

test_3b <- test_3a

test_3b$cri_1 <- ifelse(test_3b$r_id==7, NA, test_3b$cri_1 )

test_3a$pids <- record_group(test_3a, sn = r_id, criteria = c(cri_1, cri_2), group_stats = TRUE)
test_3b$pids <- record_group(test_3b, sn = r_id, criteria = c(cri_1, cri_2), group_stats = TRUE)

test_that("test that record grouping with >1 criteria follows an order of decreasing certaintity", {
  expect_equal(test_3a$pids@.Data, c(6,2,6,4,2,6,6))
  expect_equal(test_3a$pids@pid_cri, c(2,2,2, 0,2,1, 1))
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
  r_id = as.integer(c(1:7)),

  stringsAsFactors = FALSE
)

test_4a <- df_4a
test_4a$pids <- record_group(df_4a, sn = r_id, criteria = c(cri_1, cri_2), sub_criteria = list(s2a=c("cri_2a","cri_2b","cri_2c")) , group_stats = TRUE)

test_that("test record grouping with 1 set of sub-criteria per criteria", {
  expect_equal(test_4a$pids@.Data, c(1,1,1,1,5,6,6))
  expect_equal(test_4a$pids@pid_cri, c(2,2,2,2, 0,1,1))
  expect_equal(test_4a$pids@pid_total, c(4,4,4,4,1,2,2))
})

# Test 5 - Matching with >1 subcriteria
df_5a <- data.frame(
  cri_1 = c(NA,NA,NA,NA,NA,2,2),
  cri_2 = rep("A1",7),
  cri_2a = c("A","B","D","X","F","G","G"),
  cri_2b = c("B","B","D","X","F","G","I"),
  cri_2c = c("A","B","D","X","F","G","G"),
  cri_2d = c("T","T","Z","V","F","H","J"),
  r_id = as.integer(c(1:7)),

  stringsAsFactors = FALSE
)

test_5a <- df_5a
test_5a$pids <- record_group(df_5a, sn = r_id, criteria = c(cri_1, cri_2), sub_criteria = list(s2a=c("cri_2a","cri_2b"), s2b=c("cri_2c","cri_2d")), group_stats = TRUE)

test_that("test record grouping with >1 set of sub-criteria per criteria", {
  expect_equal(test_5a$pids@.Data, c(1,1,3,4,5,6,6))
  expect_equal(test_5a$pids@pid_cri, c(2,2,0,0,0,1,1))
  expect_equal(test_5a$pids@pid_total, c(2,2,1,1,1,2,2))
})

# Test 6 - Deterministic linkage
df_6a <- df_4a
df_6a$dataset <- c("DS1","DS2","DS1","DS4","DS4","DS1","DS3")
test_6a <- df_6a
test_6a$pids <- record_group(df_6a, sn = r_id, criteria = c(cri_1, cri_2), sub_criteria = list(s2a=c("cri_2a","cri_2b","cri_2c")), data_source = dataset, group_stats = TRUE)

test_that("test record grouping for deterministic linkage", {
  expect_equal(test_6a$pids@.Data, c(1,1,1,1,5,6,6))
  expect_equal(test_6a$pids@pid_cri, c(2,2,2,2,0,1,1))
  expect_equal(test_6a$pids@pid_total, c(4,4,4,4,1,2,2))
  expect_equal(decode(test_6a$pids@pid_dataset), c(rep("DS1,DS2,DS4",4), "DS4",rep("DS1,DS3",2)) )
})

#Test 7 - Range matching
# df_7 <- data.frame(
#   r_id = 1:15,
#   cri_1 = c(rep("P1",5), rep("P2",10)),
#   age = c(10,15,9,40,42,50,70,57,55,34,12, 35,39,20,11),
#   stringsAsFactors = TRUE
# )
#
# df_7$corrupt_range <- df_7$age_range <- number_line(df_7$age-5, df_7$age+5, gid = df_7$age)
# df_7$corrupt_range@gid[3] <- 205
#
# test_7 <- df_7
# test_7$pids_a <- record_group(df_7, sn = r_id, criteria = cri_1, sub_criteria = list(s1a="age_range"), group_stats = TRUE)
# test_7$pids_b <- record_group(df_7, sn = r_id, criteria = age_range, group_stats = TRUE)
#
# test_that("test record grouping using range matching in criteria", {
#   expect_equal(test_7$pids_a@.Data, c(1,1,1,4,4,8,7,8,8,10,11,10,10,14,11))
#   expect_equal(test_7$pids_a@pid_cri, c(rep(1,6),0, rep(1, 6),0,1))
#   expect_equal(test_7$pids_a@pid_total, c(3,3,3,2,2,3,1,3,3,3,2,3,3,1,2))
# })
#
# test_that("test record grouping using range matching in sub_criteria", {
#   expect_equal(test_7$pids_b@.Data, c(14,14,14,10,10,8,7,8,8,10,14,10,10,14,14))
#   expect_equal(test_7$pids_b@pid_cri, c(rep(1,6),0, rep(1, 8)))
#   expect_equal(test_7$pids_b@pid_total, c(6,6,6,5,5,3,1,3,3,5,6,5,5,6,6))
# })
