context("testing record_group function")

library(testthat)
library(diyar)
library(dplyr)
library(lubridate)

# Test 1 - Consistent row position for input and output
df <- data.frame(
  cri_1 = c("A","C","B","C","A"),
  r_id = c(1:5)
)

test_1 <- cbind(df, record_group(df,r_id, cri_1))

test_that("test that row positions of the resulting dataframe are the same as supplied", {
  expect_equal(test_1$sn, test_1$r_id)
})

test_that("test that test record identifier is as expected for one criteria", {
  expect_equal(test_1$pid, c(1,2,3,2,1))
  expect_equal(test_1$pid_cri, c("Criteria 1","Criteria 1","None","Criteria 1","Criteria 1"))
})

# Test 2 - Missing values
df_2b <- df_2a <- df
df_2a$cri_1 <- ifelse(df_2a$cri_1=="A", NA, df_2a$cri_1 )
df_2b$cri_1 <- ifelse(df_2b$cri_1=="A", "", df_2b$cri_1 )

test_2a <- cbind(df_2a, record_group(df_2a,r_id, cri_1))
test_2b <- cbind(df_2b, record_group(df_2b,r_id, cri_1))

test_that("test that test blank space or NAs criteria are treated as unique record groups", {
  expect_equal(test_2a$pid, c(1,2,3,2,5))
  expect_equal(test_2b$pid, c(1,2,3,2,5))

})

# Test 3 - Decreasing order of certainty
df_3a <- data.frame(
  cri_1 = c("A","C","Z","V","F","G","G"),
  cri_2 = c("CC","AA","CC","VV","AA","CB","CC"),
  r_id = c(1:7),

  stringsAsFactors = FALSE
)

df_3b <- df_3a

df_3b$cri_1 <- ifelse(df_3b$r_id==7, NA, df_3b$cri_1 )

test_3a <- cbind(df_3a, record_group(df_3a,r_id, c(cri_1, cri_2) ))
test_3b <- cbind(df_3b, record_group(df_3b,r_id, c(cri_1, cri_2) ))

test_that("test that record grouping with >1 criteria follows an order of decreasing certaintity", {
  expect_equal(test_3a$pid, c(6,2,6,4,2,6,6))
  expect_equal(test_3a$pid_cri, c("Criteria 2","Criteria 2","Criteria 2", "None","Criteria 2","Criteria 1", "Criteria 1"))
  expect_equal(test_3b$pid, c(1,2,1,4,2,6,1))
  expect_equal(test_3b$pid_cri, c("Criteria 2","Criteria 2","Criteria 2", "None","Criteria 2","None", "Criteria 2"))

})

# Test 4 - Matching with 1 criteria
df_4a <- data.frame(
  cri_1 = c(NA,NA,NA,NA,NA,2,2),
  cri_2 = rep("A1",7),
  cri_2a = c("A","B","D","X","F","G","G"),
  cri_2b = c("B","B","D","X","F","G","I"),
  cri_2c = c("A","C","A","A","F","G","I"),
  r_id = c(1:7),

  stringsAsFactors = FALSE
)

test_4a <- cbind(df_4a, record_group(df_4a,r_id, c(cri_1, cri_2), list(s2a=c("cri_2a","cri_2b","cri_2c")) ))

test_that("test record grouping with 1 set of sub-criteria per criteria", {
  expect_equal(test_4a$pid, c(1,1,1,1,5,6,6))
  expect_equal(test_4a$pid_cri, c("Criteria 2","Criteria 2","Criteria 2","Criteria 2", "None","Criteria 1","Criteria 1"))
})

# Test 5 - Matching with >1 subcriteria
df_5a <- data.frame(
  cri_1 = c(NA,NA,NA,NA,NA,2,2),
  cri_2 = rep("A1",7),
  cri_2a = c("A","B","D","X","F","G","G"),
  cri_2b = c("B","B","D","X","F","G","I"),
  cri_2c = c("A","B","D","X","F","G","G"),
  cri_2d = c("T","T","Z","V","F","H","J"),
  r_id = c(1:7),

  stringsAsFactors = FALSE
)

test_5a <- cbind(df_5a, record_group(df_5a,r_id, c(cri_1, cri_2), list(s2a=c("cri_2a","cri_2b"), s2b=c("cri_2c","cri_2d")) ))

test_that("test record grouping with >1 set of sub-criteria per criteria", {
  expect_equal(test_5a$pid, c(1,1,3,4,5,6,6))
  expect_equal(test_5a$pid_cri, c("Criteria 2","Criteria 2","None","None","None","Criteria 1","Criteria 1"))
})

# Test 6 - Deterministic linkage
df_6a <- df_4a

df_6a$dataset <- c("DS1","DS2","DS1","DS4","DS4","DS1","DS3")
test_6a <- cbind(df_6a, record_group(df_6a,r_id, c(cri_1, cri_2), list(s2a=c("cri_2a","cri_2b","cri_2c")), dataset ))

test_that("test record grouping for deterministic linkage", {
  expect_equal(test_6a$pid_dataset, c(rep("DS1,DS2,DS4",4), "DS4",rep("DS1,DS3",2)) )
})

#Test 7 - Range matching and group stats
df_7 <- data.frame(
  r_id = 1:15,
  cri_1 = c(rep("P1",5), rep("P2",10)),
  age = c(10,15,9,40,42,50,70,57,55,34,12, 35,39,20,11),
  stringsAsFactors = TRUE
)

df_7$age_range <- diyar::number_line(df_7$age-5, df_7$age+5)
test_7 <- cbind(df_7, record_group(df_7, r_id, cri_1, list(s1a="age_range"), group_stats = TRUE))

test_that("test record grouping with range matching", {
  expect_equal(test_7$pid, c(1,1,1,4,4,6,7,6,6,10,11,10,10,11,11))
  expect_equal(test_7$pid_cri, c(rep("Criteria 1",6),"None", rep("Criteria 1", 8)))
  expect_equal(test_7$pid_total, c(3,3,3,2,2,3,1,3,3,3,3,3,3,3,3))
})

