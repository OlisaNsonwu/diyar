library(testthat)
library(diyar)

data("infections")
fixed_episodes <- diyar::fixed_episodes
record_group <- diyar::record_group

df <- to_df(episodes(date = infections$date, case_length = 30))
s4 <- episodes(date = infections$date, case_length = 30)

s4@options <- list()
s4@wind_id <- list()

df_2 <- to_df(links(criteria = infections$infection))
s4_2 <- links(criteria = infections$infection)

test_that("test to_s4", {
  # expect_equal(to_s4(df), s4)
  # expect_equal(to_df(s4)$epid, df$epid)
  # expect_equal(to_df(s4)$case_nm, df$case_nm)
  # expect_equal(to_df(s4)$sn, df$sn)
  #
  # expect_equal(to_s4(df_2), s4_2)
  # expect_equal(to_df(s4_2)$pid, df_2$pid)
  # expect_equal(to_df(s4_2)$pid_cri, df_2$pid_cri)
  # expect_equal(to_df(s4_2)$sn, df_2$sn)
  # expect_error(to_df(mtcars),"'s4' must be an `epid`, `pid`, `pane` or `number_line` object")
  # expect_error(to_s4(letters),"'df' must be a data.frame")
})
