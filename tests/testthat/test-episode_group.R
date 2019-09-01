context("testing episode_group function")

library(testthat)
library(diyar)
library(dplyr)
library(lubridate)

# Test 1 - Fixed episodes
data <- data.frame(date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="3 days"))
data$pid <- "Patient 1"
data$episode_len <- 6
data <- mutate(data, rd_id = row_number())

test_1 <- episode_group(head(data,10), strata = pid, date = date, case_length = episode_len, group_stats = TRUE)

test_that("test that row positions of the resulting dataframe are the same as supplied", {
  expect_equal(test_1$sn, head(data,10)$rd_id)
})

e_int <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("07/04/2018")), 3),
  rep(lubridate::interval(lubridate::dmy("10/04/2018"), lubridate::dmy("16/04/2018")), 3),
  rep(lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("25/04/2018")), 3),
  rep(lubridate::interval(lubridate::dmy("28/04/2018"), lubridate::dmy("28/04/2018")), 1)
  )

test_that("test that test episode identifier is as expected for fixed episode type", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10))
  expect_equal(test_1$case_nm, rep(c("Case",rep("Duplicate",2)),4)[1:10] )

  expect_equal(test_1$epid_interval, e_int)
  expect_equal(test_1$epid_total, c(rep(3,9),1))
  expect_equal(test_1$epid_length, lubridate::as.difftime(c(rep(6,9),0), units = "days" ))
})


# Test 2 - Case assignment - Reverse chronological order
data_2 <- mutate(head(data,10), episode_len_s=13)
test_2 <-
cbind(data_2,
      rename_all(episode_group(data_2, strata = pid, date = date, case_length = episode_len_s, display = FALSE, from_last = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
      rename_all(episode_group(data_2, strata = pid, date = date, case_length = episode_len_s, display = FALSE, from_last = TRUE, group_stats = TRUE), funs(paste(.,2,sep=".")))
)

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("13/04/2018")), 5),
  rep(lubridate::interval(lubridate::dmy("16/04/2018"), lubridate::dmy("28/04/2018")), 5)
)

e_int.2 <- c(
  rep(lubridate::int_flip(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("13/04/2018"))), 5),
  rep(lubridate::int_flip(lubridate::interval(lubridate::dmy("16/04/2018"), lubridate::dmy("28/04/2018"))), 5)
)

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid.1, c(rep(1,5),rep(6,5)))
  expect_equal(test_2$epid.2, c(rep(5,5),rep(10,5)))
  expect_equal(test_2$case_nm.1, rep(c("Case",rep("Duplicate",4)),2))
  expect_equal(test_2$case_nm.2, rep(c(rep("Duplicate",4),"Case"),2))

  expect_equal(test_2$epid_interval.1, e_int.1)
  expect_equal(test_2$epid_total.1, rep(5,10))
  expect_equal(test_2$epid_length.1, lubridate::as.difftime(rep(12,10), units = "days" ))

  expect_equal(test_2$epid_interval.2, e_int.2)
  expect_equal(test_2$epid_total.2, rep(5,10))
  expect_equal(test_2$epid_length.2, lubridate::as.difftime(rep(-12,10), units = "days" ))
  })

# Test 3 - Rolling episodes
test_3 <- cbind(data_2,
      rename_all(episode_group(data_2, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
      rename_all(episode_group(data_2, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", display = FALSE, from_last = TRUE, group_stats = TRUE), funs(paste(.,2,sep=".")))
      )

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("28/04/2018")), 10)
)

e_int.2 <- c(
  rep(lubridate::int_flip(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("28/04/2018"))), 10)
)

test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid.1, rep(1,10))
  expect_equal(test_3$epid.2, rep(10,10))
  expect_equal(test_3$case_nm.1, c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent"))
  expect_equal(test_3$case_nm.2, rev(c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent")))

  expect_equal(test_3$epid_interval.1, e_int.1)
  expect_equal(test_3$epid_total.1, rep(10,10))
  expect_equal(test_3$epid_length.1, lubridate::as.difftime(rep(27,10), units = "days" ))

  expect_equal(test_3$epid_interval.2, e_int.2)
  expect_equal(test_3$epid_total.2, rep(10,10))
  expect_equal(test_3$epid_length.2, lubridate::as.difftime(rep(-27,10), units = "days" ))

})

# Test 3 - Rolls max
data_4 <- mutate(data_2, recurrence=3)
test_4 <- cbind(data_4,
      rename_all(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, display = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
      rename_all(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, rolls_max = 1,  display = FALSE, group_stats = TRUE), funs(paste(.,2,sep=".")))
      )

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("28/04/2018")), 10)
)

e_int.2 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("16/04/2018")), 6),
  rep(lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("28/04/2018")), 4)
)

test_that("test user defined recurrence length and roll_max", {
  expect_equal(test_4$epid.1, rep(1,10))
  expect_equal(test_4$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_4$case_nm.1, c("Case",rep("Duplicate",4),rep("Recurrent",5)))
  expect_equal(test_4$case_nm.2, c("Case",rep("Duplicate",4),"Recurrent","Case",rep("Duplicate",3)))

  expect_equal(test_4$epid_interval.1, e_int.1)
  expect_equal(test_4$epid_total.1, rep(10,10))
  expect_equal(test_4$epid_length.1, lubridate::as.difftime(rep(27,10), units = "days" ))

  expect_equal(test_4$epid_interval.2, e_int.2)
  expect_equal(test_4$epid_total.2, c(rep(6,6), rep(4,4)))
  expect_equal(test_4$epid_length.2, lubridate::as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))
})

# Test 5 - Episodes max
test_5 <- cbind(data_4,
      rename_all(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="fixed", recurrence_length = recurrence, episodes_max = 1, display = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
      rename_all(episode_group(data_4, sn=rd_id, strat = pid, date = date, case_length = episode_len_s, episode_type ="fixed", recurrence_length = recurrence, episodes_max = 2,  display = FALSE, group_stats = TRUE), funs(paste(.,2,sep=".")))
      )

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("13/04/2018")), 5),
  lubridate::interval(lubridate::dmy("16/04/2018"), lubridate::dmy("16/04/2018")),
  lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("19/04/2018")),
  lubridate::interval(lubridate::dmy("22/04/2018"), lubridate::dmy("22/04/2018")),
  lubridate::interval(lubridate::dmy("25/04/2018"), lubridate::dmy("25/04/2018")),
  lubridate::interval(lubridate::dmy("28/04/2018"), lubridate::dmy("28/04/2018"))
)

e_int.2 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("13/04/2018")), 5),
  rep(lubridate::interval(lubridate::dmy("16/04/2018"), lubridate::dmy("28/04/2018")), 5)
)

test_that("testing user defined episodes_max", {
  expect_equal(test_5$epid.1, c(rep(1,5),6:10))
  expect_equal(test_5$epid.2, c(rep(1,5), rep(6,5)))
  expect_equal(test_5$case_nm.1, c("Case",rep("Duplicate",4),rep("Case",5)))
  expect_equal(test_5$case_nm.2, rep(c("Case",rep("Duplicate",4)),2))

  expect_equal(test_5$epid_interval.1, e_int.1)
  expect_equal(test_5$epid_total.1, rep(c(rep(5,5), rep(1,5))))
  expect_equal(test_5$epid_length.1, lubridate::as.difftime(c(rep(12,5), rep(0,5)), units = "days" ))

  expect_equal(test_5$epid_interval.2, e_int.2)
  expect_equal(test_5$epid_total.2, rep(5,10))
  expect_equal(test_5$epid_length.2, lubridate::as.difftime(rep(12,10), units = "days" ))
})


# Test 6 - Combining rolls_max and episodes_max
test_6 <- cbind(data_4,
                rename_all(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 1, rolls_max = 1, display = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
                rename_all(episode_group(data_4, sn=rd_id, strat = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 1, display = FALSE, group_stats = TRUE), funs(paste(.,2,sep="."))),
                rename_all(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 1, display = FALSE, group_stats = TRUE), funs(paste(.,3,sep="."))),
                rename_all(episode_group(data_4, sn=rd_id, strat = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 3, display = FALSE, group_stats = TRUE), funs(paste(.,4,sep=".")))

)

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("16/04/2018")), 6),
  lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("19/04/2018")),
  lubridate::interval(lubridate::dmy("22/04/2018"), lubridate::dmy("22/04/2018")),
  lubridate::interval(lubridate::dmy("25/04/2018"), lubridate::dmy("25/04/2018")),
  lubridate::interval(lubridate::dmy("28/04/2018"), lubridate::dmy("28/04/2018"))
)

e_int.2 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("16/04/2018")), 6),
  rep(lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("28/04/2018")), 4)
)

e_int.3 <- e_int.2

e_int.4 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("22/04/2018")), 8),
  rep(lubridate::interval(lubridate::dmy("25/04/2018"), lubridate::dmy("28/04/2018")), 2)
)

test_that("testing episodes_max and rolls_max combinations", {
  expect_equal(test_6$epid.1, c(rep(1,6),7:10))
  expect_equal(test_6$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_6$case_nm.1, c("Case",rep("Duplicate",4),"Recurrent",rep("Case",4)))
  expect_equal(test_6$case_nm.2, rep(c("Case",rep("Duplicate",4),"Recurrent"),2)[1:10])

  expect_equal(test_6$epid.3, c(rep(1,6),rep(7,4)))
  expect_equal(test_6$epid.4, c(rep(1,8), rep(9,2)))
  expect_equal(test_6$case_nm.3, c("Case",rep("Duplicate",4),"Recurrent", "Case", rep("Duplicate",3)))
  expect_equal(test_6$case_nm.4, rep(c("Case",rep("Duplicate",4),rep("Recurrent",3)),2)[1:10])

  expect_equal(test_6$epid_interval.1, e_int.1)
  expect_equal(test_6$epid_total.1, rep(c(rep(6,6), rep(1,4))))
  expect_equal(test_6$epid_length.1, lubridate::as.difftime(c(rep(15,6), rep(0,4)), units = "days" ))

  expect_equal(test_6$epid_interval.2, e_int.2)
  expect_equal(test_6$epid_total.2, rep(c(rep(6,6), rep(4,4))))
  expect_equal(test_6$epid_length.2, lubridate::as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))

  expect_equal(test_6$epid_interval.3, e_int.3)
  expect_equal(test_6$epid_total.3, rep(c(rep(6,6), rep(4,4))))
  expect_equal(test_6$epid_length.3, lubridate::as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))

  expect_equal(test_6$epid_interval.4, e_int.4)
  expect_equal(test_6$epid_total.4, rep(c(rep(8,8), rep(2,2))))
  expect_equal(test_6$epid_length.4, lubridate::as.difftime(rep(c(rep(21,8), rep(3,2))), units = "days" ))
})

# Test 7 - Deterministic linkage
data_7 <-  mutate(data_4, recurrence=2)
data_7$dataset <- paste("DS",c(1:3, rep(c(1:2),2), rep(3,3)), sep="")

test_7 <- cbind(data_7,
                rename_all(episode_group(data_7, sn=rd_id, strata = pid, date = date, case_length = episode_len, episode_type ="rolling", recurrence_length = recurrence, data_source = dataset, display = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),
                rename_all(episode_group(data_7, sn=rd_id, strata = pid, date = date, case_length = episode_len, episode_type ="rolling", recurrence_length = recurrence, data_source = c(dataset, episode_len_s), display = FALSE, group_stats = TRUE), funs(paste(.,2,sep=".")))
      )

e_int.1 <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("07/04/2018")), 3),
  rep(lubridate::interval(lubridate::dmy("10/04/2018"), lubridate::dmy("16/04/2018")), 3),
  rep(lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("25/04/2018")), 3),
  lubridate::interval(lubridate::dmy("28/04/2018"), lubridate::dmy("28/04/2018"))
)

test_that("testing epid_dataset", {
  expect_equal(test_7$epid.1, c(rep(1,3),rep(4,3),rep(7,3),10))
  expect_equal(test_7$case_nm.1, rep(c("Case",rep("Duplicate",2)),4)[1:10])
  expect_equal(test_7$epid_dataset.1, c(rep("DS1,DS2,DS3",3),rep("DS1,DS2",3),rep("DS2,DS3",3),"DS3" ))

  expect_equal(test_7$epid_interval.1, e_int.1)
  expect_equal(test_7$epid_total.1, c(rep(3,9),1))
  expect_equal(test_7$epid_length.1, lubridate::as.difftime(c(rep(6,9),0), units = "days" ))

  expect_equal(test_7$epid.1, test_7$epid.2)
  expect_equal(test_7$case_nm.1, test_7$case_nm.2)
  expect_equal(test_7$epid_interval.1, test_7$epid_interval.2)
  expect_equal(test_7$epid_total.1, test_7$epid_total.2)
  expect_equal(test_7$epid_length.1, test_7$epid_length.2)

  expect_equal(test_7$epid_dataset.2, c(rep("DS1-13,DS2-13,DS3-13",3),rep("DS1-13,DS2-13",3),rep("DS2-13,DS3-13",3),"DS3-13" ))
})

hospital_infections <- tibble(
  rd_id = c(1:11),
  date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="6 days"),
  infection = c("BSI", rep("UTI",2), "UTI", "BSI",  "UTI", rep("BSI",2), "RTI","RTI","BSI"),
  epi_len = 15
)

# Test 8 - Episode unit
# 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
test_8a <- bind_cols(hospital_infections,
          episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                        from_last = TRUE, episode_unit = "hours", display = FALSE, group_stats = TRUE)) %>%
  select(-sn)

e_int <- lubridate::interval(test_8a$date, test_8a$date)

test_that("testing; episode grouping by the hour", {
  expect_equal(test_8a$epid, 1:11)
  expect_equal(test_8a$case_nm, rep("Case",11))
  expect_equal(test_8a$epid_interval, e_int)
  expect_equal(test_8a$epid_total, rep(1,11))
  expect_equal(test_8a$epid_length, lubridate::as.difftime(rep(0,11), units = "hours" ))
})

# 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
test_8b <- bind_cols(hospital_infections,
          episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                        from_last = TRUE, episode_unit = "weeks", display = FALSE, group_stats = TRUE)) %>%
  select(-sn)

e_int <- rep(lubridate::interval(lubridate::dmy("31/05/2018"), lubridate::dmy("01/04/2018")), 11)

test_that("testing; episode grouping by weeks", {
  expect_equal(test_8b$epid, rep(11,11))
  expect_equal(test_8b$case_nm, c(rep("Duplicate",10),"Case"))
  expect_equal(test_8b$epid_interval, e_int)
  expect_equal(test_8b$epid_total, rep(11,11))
  expect_equal(test_8b$epid_length, lubridate::as.difftime(rep(-60,11), units = "days" ))
})

# Test 9 - User defined case assignment
# preference for case assignment - UTI > BSI > RTI
hospital_infections$infection <- factor(hospital_infections$infection, levels = c("UTI","BSI","RTI"))

# Different case and recurrence lengths for different source of infection
hospital_infections <- mutate(
  hospital_infections,
  epi_len = case_when(
    infection == "BSI" ~ 14,
    infection == "UTI" ~ 30,
    infection == "RTI" ~ 60
  )
)

# n-day episodes beginning with the earliest record with the specified preference; UTI > BSI > RTI
test_9a <- bind_cols(hospital_infections,
          episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
                        custom_sort = infection,  display = FALSE, group_stats = TRUE)) %>% select(-sn)

e_int <- c(
  lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("01/04/2018")),
  rep(lubridate::interval(lubridate::dmy("07/04/2018"), lubridate::dmy("07/05/2018")), 6),
  rep(lubridate::interval(lubridate::dmy("13/05/2018"), lubridate::dmy("25/05/2018")), 3),
  lubridate::interval(lubridate::dmy("31/05/2018"), lubridate::dmy("31/05/2018"))
)

test_that("testing episode; custom sort", {
  expect_equal(test_9a$epid, c(1,rep(2,6), rep(8,3), 11))
  expect_equal(test_9a$case_nm, c("Case","Case",rep("Duplicate",5),"Case", rep("Duplicate",2), "Case"))
  expect_equal(test_9a$epid_interval, e_int)
  expect_equal(test_9a$epid_total, c(1,rep(6,6), rep(3,3), 1))
  expect_equal(test_9a$epid_length, lubridate::as.difftime(c(0,rep(30,6), rep(12,3), 0), units = "days" ))
})

# preference for case assignment - RTI > UTI, or  RTI > BSI, or earliest record
hospital_infections$infection_ord <- ifelse(hospital_infections$infection =="RTI",0,1)

# n-day episodes with duplicates before and after the most recent "RTI" record, otherwise begin at the most recent record
test_9b <- bind_cols(hospital_infections,
          rename_all(episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
                        custom_sort = infection_ord, from_last = TRUE, bi_direction = TRUE, display = FALSE, group_stats = TRUE), funs(paste(.,1,sep="."))),

          rename_all(episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
                        custom_sort = infection_ord, from_last = TRUE, bi_direction = FALSE, display = FALSE, group_stats = TRUE), funs(paste(.,2,sep=".")))
          ) %>%
  select(-starts_with("sn"))

e_int.1 <- rep(lubridate::interval(lubridate::dmy("31/05/2018"), lubridate::dmy("01/04/2018")), 11)

e_int.2 <- c(
  rep(lubridate::interval(lubridate::dmy("25/05/2018"), lubridate::dmy("01/04/2018")), 10),
  lubridate::interval(lubridate::dmy("31/05/2018"), lubridate::dmy("31/05/2018"))
  )

test_that("testing; episode grouping with custom sort and bi_direction", {
  expect_equal(test_9b$epid.1, rep(10,11))
  expect_equal(test_9b$case_nm.1, c(rep("Duplicate",9),"Case","Duplicate"))
  expect_equal(test_9b$epid_interval.1, e_int.1)
  expect_equal(test_9b$epid_total.1, rep(11,11))
  expect_equal(test_9b$epid_length.1, lubridate::as.difftime(rep(-60,11), units = "days" ))

  expect_equal(test_9b$epid.2, c(rep(10,10), 11))
  expect_equal(test_9b$case_nm.2, c(rep("Duplicate",9),"Case","Case"))
  expect_equal(test_9b$epid_interval.2, e_int.2)
  expect_equal(test_9b$epid_total.2, c(rep(10,10),1))
  expect_equal(test_9b$epid_length.2, lubridate::as.difftime(c(rep(-54,10), 0), units = "days" ))
})


# Test 10 - Stratified episode grouping
hospital_infections$patient_id <- c(rep("PID 1",8), rep("PID 2",3))

# Only one n-day episode per patient_id
test_10a <- bind_cols(hospital_infections,
          episode_group(hospital_infections, rd_id, date=date, strata = patient_id, case_length = epi_len,
                        episodes_max = 1, from_last = FALSE, display = FALSE, data_source = infection, group_stats = TRUE)) %>%
  select(-sn)

e_int <- c(
  rep(lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("13/04/2018")), 3),
  lubridate::interval(lubridate::dmy("19/04/2018"), lubridate::dmy("19/04/2018")),
  lubridate::interval(lubridate::dmy("25/04/2018"), lubridate::dmy("25/04/2018")),
  lubridate::interval(lubridate::dmy("01/05/2018"), lubridate::dmy("01/05/2018")),
  lubridate::interval(lubridate::dmy("07/05/2018"), lubridate::dmy("07/05/2018")),
  lubridate::interval(lubridate::dmy("13/05/2018"), lubridate::dmy("13/05/2018")),
  rep(lubridate::interval(lubridate::dmy("19/05/2018"), lubridate::dmy("31/05/2018")), 3)
)

test_that("testing; stratified grouping", {
  expect_equal(test_10a$epid, c(rep(1,3), 4:8, rep(9,3)))
  expect_equal(test_10a$case_nm, c("Case",rep("Duplicate",2), rep("Case",6), rep("Duplicate",2)))
  expect_equal(test_10a$epid_interval, e_int)
  expect_equal(test_10a$epid_total, c(rep(3,3), rep(1,5), rep(3,3)))
  expect_equal(test_10a$epid_length, lubridate::as.difftime(c(rep(12,3), rep(0,5), rep(12,3)), units = "days" ))
  expect_equal(test_10a$epid_dataset, c(rep("BSI,UTI",3), "UTI","BSI","UTI","BSI","BSI",rep("BSI,RTI",3)))
})

# Only three 9-day (difference of 8 days) rolling episode per patient and infection.
hospital_infections$epi_len <- 8
hospital_infections$recur <- 30
test_10b <- bind_cols(hospital_infections,
          episode_group(hospital_infections, rd_id, date=date, strata = c(patient_id, infection), case_length = epi_len,
                        episode_type = "rolling", recurrence_length = recur, episodes_max = 3, data_source = c(patient_id, infection),
                        display = FALSE, group_stats = TRUE)) %>%
  select(-sn)

e_int <- c(
  lubridate::interval(lubridate::dmy("01/04/2018"), lubridate::dmy("01/04/2018")),
  rep(lubridate::interval(lubridate::dmy("07/04/2018"), lubridate::dmy("01/05/2018")), 3),
  lubridate::interval(lubridate::dmy("25/04/2018"), lubridate::dmy("25/04/2018")),
  lubridate::interval(lubridate::dmy("07/04/2018"), lubridate::dmy("01/05/2018")),
  lubridate::interval(lubridate::dmy("07/05/2018"), lubridate::dmy("13/05/2018")),
  lubridate::interval(lubridate::dmy("07/05/2018"), lubridate::dmy("13/05/2018")),
  rep(lubridate::interval(lubridate::dmy("19/05/2018"), lubridate::dmy("25/05/2018")), 2),
  lubridate::interval(lubridate::dmy("31/05/2018"), lubridate::dmy("31/05/2018"))
)

test_that("testing; stratified grouping 2", {
  expect_equal(test_10b$epid, c(1, rep(2,3), 5,2, 7,7,9,9, 11))
  expect_equal(test_10b$case_nm, c("Case","Case","Duplicate","Recurrent",
                                   "Case", "Duplicate", "Case",
                                   "Duplicate","Case","Duplicate","Case"
                                   ))
  expect_equal(test_10b$epid_interval, e_int)
  expect_equal(test_10b$epid_total, c(1, rep(4,3), 1,4, rep(2,4),1))
  expect_equal(test_10b$epid_length, lubridate::as.difftime(c(0, rep(24,3), 0, 24, rep(6,4), 0), units = "days" ))
})

#Test 11 - Interval grouping
hospital_admissions <- tibble(
  rd_id = 1:9,
  admin_dt = c(dmy("01/01/2019"), dmy("01/01/2019"), dmy("10/01/2019"), dmy("05/01/2019"),
               dmy("05/01/2019"), dmy("07/01/2019"), dmy("04/01/2019"),
               dmy("20/01/2019"), dmy("26/01/2019")),
  discharge_dt = c(dmy("01/01/2019"), dmy("10/01/2019"), dmy("13/01/2019"), dmy("06/01/2019"),
                   dmy("15/01/2019"), dmy("15/01/2019"), dmy("13/01/2019"),
                   dmy("30/01/2019"), dmy("31/01/2019"))
)

hospital_admissions$epi_len <- 0
hospital_admissions$admin_period <- interval(hospital_admissions$admin_dt, hospital_admissions$discharge_dt)
hospital_admissions

# episodes of overlaping intervals of admission
test_11a <-bind_cols(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(lubridate::interval(lubridate::dmy("01/01/2019"), lubridate::dmy("15/01/2019")), 7),
  rep(lubridate::interval(lubridate::dmy("20/01/2019"), lubridate::dmy("31/01/2019")), 2)
)

test_that("testing; intervals grouping", {
  expect_equal(test_11a$epid, c(rep(2,7), rep(8,2)))
  expect_equal(test_11a$case_nm, c("Duplicate","Case", rep("Duplicate",5),
                                   "Case", "Duplicate"))
  expect_equal(test_11a$epid_interval, e_int)
  expect_equal(test_11a$epid_total, c(rep(7,7), rep(2,2)))
  expect_equal(test_11a$epid_length, lubridate::as.difftime(c(rep(14,7), rep(11,2)), units = "days" ))
})

# rolling episodes of overlaping intervals of admission, and those within 10 days of the last interval
hospital_admissions$epi_len <- 0
hospital_admissions$recur <- 1

test_11b <- bind_cols(
  hospital_admissions,
  episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len,
                episode_type = "rolling", recurrence_length = recur, episode_unit = "months", group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(lubridate::interval(lubridate::dmy("01/01/2019"), lubridate::dmy("31/01/2019")), 9)
)

test_that("testing; intervals grouping for rolling intervals", {
  expect_equal(test_11b$epid, rep(2,9))
  expect_equal(test_11b$case_nm, c("Duplicate","Case",rep("Duplicate",5),
                                   "Recurrent", "Duplicate"))
  expect_equal(test_11b$epid_interval, e_int)
  expect_equal(test_11b$epid_total, rep(9,9))
  expect_equal(test_11b$epid_length, lubridate::as.difftime(rep(30,9), units = "days" ))
})

# fixed episodes of overlaping intervals of admission seperated by 1 month
hospital_admissions$epi_len <- 1

test_11c <- bind_cols(hospital_admissions,
          episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, episode_unit = "months", group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(lubridate::interval(lubridate::dmy("01/01/2019"), lubridate::dmy("31/01/2019")), 9)
)

test_that("testing; intervals grouping with a case length", {
  expect_equal(test_11c$epid, rep(2,9))
  expect_equal(test_11c$case_nm, c("Duplicate","Case",rep("Duplicate",7)))
  expect_equal(test_11c$epid_interval, e_int)
  expect_equal(test_11c$epid_total, rep(9,9))
  expect_equal(test_11c$epid_length, lubridate::as.difftime(rep(30,9), units = "days" ))
})
