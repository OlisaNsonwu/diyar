library(testthat)
library(diyar)

data("infections")
fixed_episodes <- diyar::fixed_episodes
record_group <- diyar::record_group

df <- fixed_episodes(date = infections$date, case_length = 30, to_s4=F, display = "none")
s4 <- fixed_episodes(date = infections$date, case_length = 30, to_s4=T, display = "none")

df_2 <- record_group(df = infections, criteria = infection, to_s4 = F, display = "none")
s4_2 <- record_group(df = infections, criteria = infection, to_s4 = T, display = "none")

test_that("test to_s4", {
  expect_equal(to_s4(df), s4)
  expect_equal(to_df(s4)$epid, df$epid)
  expect_equal(to_df(s4)$case_nm, df$case_nm)
  expect_equal(to_df(s4)$sn, df$sn)

  expect_equal(to_s4(df_2), s4_2)
  expect_equal(to_df(s4_2)$pid, df_2$pid)
  expect_equal(to_df(s4_2)$pid_cri, df_2$pid_cri)
  expect_equal(to_df(s4_2)$sn, df_2$sn)
  expect_error(to_df(mtcars),"'s4' must be an `epid`, `pid`, `pane` or `number_line` object")
  expect_error(to_s4(letters),"'df' must be a data.frame")
})
