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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("07/04/2018  00:00:00")), 3),
  rep(diyar::number_line(lubridate::dmy_hms("10/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018  00:00:00")), 3),
  rep(diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018  00:00:00")), 3),
  rep(diyar::number_line(lubridate::dmy_hms("28/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018  00:00:00")), 1)
  )

test_that("test that test episode identifier is as expected for fixed episode type", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10))
  expect_equal(test_1$case_nm, rep(c("Case",rep("Duplicate",2)),4)[1:10] )

  e_int@id <- 1:10
  e_int@gid <- c(1,1,1,4,4,4,7,7,7,10)
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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("13/04/2018 00:00:00")), 5),
  rep(diyar::number_line(lubridate::dmy_hms("16/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 5)
)

e_int.2 <- c(
  rep(diyar::reverse_number_line(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("13/04/2018 00:00:00"))), 5),
  rep(diyar::reverse_number_line(diyar::number_line(lubridate::dmy_hms("16/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00"))), 5)
)

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid.1, c(rep(1,5),rep(6,5)))
  expect_equal(test_2$epid.2, c(rep(5,5),rep(10,5)))
  expect_equal(test_2$case_nm.1, rep(c("Case",rep("Duplicate",4)),2))
  expect_equal(test_2$case_nm.2, rep(c(rep("Duplicate",4),"Case"),2))

  e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- c(rep(1,5),rep(6,5))
  e_int.2@gid <- c(rep(5,5),rep(10,5))

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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 10)
)

e_int.2 <- c(
  rep(diyar::reverse_number_line(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00"))), 10)
)

test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid.1, rep(1,10))
  expect_equal(test_3$epid.2, rep(10,10))
  expect_equal(test_3$case_nm.1, c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent"))
  expect_equal(test_3$case_nm.2, rev(c("Case",rep("Duplicate",4),"Recurrent",rep("Duplicate",3),"Recurrent")))

  e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- rep(1,10)
  e_int.2@gid <- rep(10,10)

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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 10)
)

e_int.2 <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018 00:00:00")), 6),
  rep(diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 4)
)

test_that("test user defined recurrence length and roll_max", {
  expect_equal(test_4$epid.1, rep(1,10))
  expect_equal(test_4$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_4$case_nm.1, c("Case",rep("Duplicate",4),rep("Recurrent",5)))
  expect_equal(test_4$case_nm.2, c("Case",rep("Duplicate",4),"Recurrent","Case",rep("Duplicate",3)))

  e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- rep(1,10)
  e_int.2@gid <- c(rep(1,6), rep(7,4))

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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("13/04/2018 00:00:00")), 5),
  diyar::number_line(lubridate::dmy_hms("16/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("19/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("22/04/2018 00:00:00"), lubridate::dmy_hms("22/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("25/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("28/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00"))
)

e_int.2 <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("13/04/2018 00:00:00")), 5),
  rep(diyar::number_line(lubridate::dmy_hms("16/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 5)
)

test_that("testing user defined episodes_max", {
  expect_equal(test_5$epid.1, c(rep(1,5),6:10))
  expect_equal(test_5$epid.2, c(rep(1,5), rep(6,5)))
  expect_equal(test_5$case_nm.1, c("Case",rep("Duplicate",4),rep("Case",5)))
  expect_equal(test_5$case_nm.2, rep(c("Case",rep("Duplicate",4)),2))

  e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- c(rep(1,5),6:10)
  e_int.2@gid <- c(rep(1,5), rep(6,5))

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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018: 00:00:00")), 6),
  diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("19/04/2018: 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("22/04/2018 00:00:00"), lubridate::dmy_hms("22/04/2018: 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("25/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018: 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("28/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018: 00:00:00"))
)

e_int.2 <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018 00:00:00")), 6),
  rep(diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 4)
)

e_int.3 <- e_int.2

e_int.4 <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("22/04/2018 00:00:00")), 8),
  rep(diyar::number_line(lubridate::dmy_hms("25/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00")), 2)
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

  e_int.4@id <- e_int.3@id <- e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- c(rep(1,6),7:10)
  e_int.3@gid  <- e_int.2@gid <- c(rep(1,6), rep(7,4))
  e_int.4@gid <- c(rep(1,8), rep(9,2))
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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("07/04/2018 00:00:00")), 3),
  rep(diyar::number_line(lubridate::dmy_hms("10/04/2018 00:00:00"), lubridate::dmy_hms("16/04/2018 00:00:00")), 3),
  rep(diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018 00:00:00")), 3),
  diyar::number_line(lubridate::dmy_hms("28/04/2018 00:00:00"), lubridate::dmy_hms("28/04/2018 00:00:00"))
)

test_that("testing epid_dataset", {
  expect_equal(test_7$epid.1, c(rep(1,3),rep(4,3),rep(7,3),10))
  expect_equal(test_7$case_nm.1, rep(c("Case",rep("Duplicate",2)),4)[1:10])
  expect_equal(test_7$epid_dataset.1, c(rep("DS1,DS2,DS3",3),rep("DS1,DS2",3),rep("DS2,DS3",3),"DS3" ))

  e_int.1@id <- 1:10
  e_int.1@gid <- c(rep(1,3),rep(4,3),rep(7,3),10)

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

data(infections)
hospital_infections <- infections
# Test 8 - Episode unit
# 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
test_8a <- bind_cols(hospital_infections,
          episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                        from_last = TRUE, episode_unit = "hours", display = FALSE, group_stats = TRUE)) %>%
  select(-sn)

e_int <- diyar::number_line(test_8a$date, test_8a$date)

test_that("testing; episode grouping by the hour", {
  expect_equal(test_8a$epid, 1:11)
  expect_equal(test_8a$case_nm, rep("Case",11))

  e_int@gid <- e_int@id <- 1:11

  expect_equal(test_8a$epid_interval, e_int)
  expect_equal(test_8a$epid_total, rep(1,11))
  expect_equal(test_8a$epid_length, lubridate::as.difftime(rep(0,11), units = "hours" ))
})

# 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
test_8b <- bind_cols(hospital_infections,
          episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                        from_last = TRUE, episode_unit = "weeks", display = FALSE, group_stats = TRUE)) %>%
  select(-sn)

e_int <- rep(diyar::number_line(lubridate::dmy_hms("31/05/2018 00:00:00"), lubridate::dmy_hms("01/04/2018 00:00:00")), 11)

test_that("testing; episode grouping by weeks", {
  expect_equal(test_8b$epid, rep(11,11))

  e_int@id <- 1:11
  e_int@gid <- rep(11,11)

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
  diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("01/04/2018 00:00:00")),
  rep(diyar::number_line(lubridate::dmy_hms("07/04/2018 00:00:00"), lubridate::dmy_hms("07/05/2018 00:00:00")), 6),
  rep(diyar::number_line(lubridate::dmy_hms("13/05/2018 00:00:00"), lubridate::dmy_hms("25/05/2018 00:00:00")), 3),
  diyar::number_line(lubridate::dmy_hms("31/05/2018 00:00:00"), lubridate::dmy_hms("31/05/2018 00:00:00"))
)

test_that("testing episode; custom sort", {
  expect_equal(test_9a$epid, c(1,rep(2,6), rep(8,3), 11))
  expect_equal(test_9a$case_nm, c("Case","Case",rep("Duplicate",5),"Case", rep("Duplicate",2), "Case"))

  e_int@id <- 1:11
  e_int@gid <- c(1,rep(2,6), rep(8,3), 11)

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

e_int.1 <- rep(diyar::number_line(lubridate::dmy_hms("31/05/2018 00:00:00"), lubridate::dmy_hms("01/04/2018 00:00:00")), 11)

e_int.2 <- c(
  rep(diyar::number_line(lubridate::dmy_hms("25/05/2018 00:00:00"), lubridate::dmy_hms("01/04/2018 00:00:00")), 10),
  diyar::number_line(lubridate::dmy_hms("31/05/2018 00:00:00"), lubridate::dmy_hms("31/05/2018 00:00:00"))
  )

test_that("testing; episode grouping with custom sort and bi_direction", {
  expect_equal(test_9b$epid.1, rep(10,11))
  expect_equal(test_9b$case_nm.1, c(rep("Duplicate",9),"Case","Duplicate"))

  e_int.2@id <- e_int.1@id <- 1:11
  e_int.1@gid <- rep(10,11)

  expect_equal(test_9b$epid_interval.1, e_int.1)
  expect_equal(test_9b$epid_total.1, rep(11,11))
  expect_equal(test_9b$epid_length.1, lubridate::as.difftime(rep(-60,11), units = "days" ))

  e_int.2@gid <- c(rep(10,10), 11)

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
  rep(diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("13/04/2018 00:00:00")), 3),
  diyar::number_line(lubridate::dmy_hms("19/04/2018 00:00:00"), lubridate::dmy_hms("19/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("25/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("01/05/2018 00:00:00"), lubridate::dmy_hms("01/05/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("07/05/2018 00:00:00"), lubridate::dmy_hms("07/05/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("13/05/2018 00:00:00"), lubridate::dmy_hms("13/05/2018 00:00:00")),
  rep(diyar::number_line(lubridate::dmy_hms("19/05/2018 00:00:00"), lubridate::dmy_hms("31/05/2018 00:00:00")), 3)
)

test_that("testing; stratified grouping", {
  expect_equal(test_10a$epid, c(rep(1,3), 4:8, rep(9,3)))
  expect_equal(test_10a$case_nm, c("Case",rep("Duplicate",2), rep("Case",6), rep("Duplicate",2)))

  e_int@id <- 1:11
  e_int@gid <- c(rep(1,3), 4:8, rep(9,3))

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
  diyar::number_line(lubridate::dmy_hms("01/04/2018 00:00:00"), lubridate::dmy_hms("01/04/2018 00:00:00")),
  rep(diyar::number_line(lubridate::dmy_hms("07/04/2018 00:00:00"), lubridate::dmy_hms("01/05/2018 00:00:00")), 3),
  diyar::number_line(lubridate::dmy_hms("25/04/2018 00:00:00"), lubridate::dmy_hms("25/04/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("07/04/2018 00:00:00"), lubridate::dmy_hms("01/05/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("07/05/2018 00:00:00"), lubridate::dmy_hms("13/05/2018 00:00:00")),
  diyar::number_line(lubridate::dmy_hms("07/05/2018 00:00:00"), lubridate::dmy_hms("13/05/2018 00:00:00")),
  rep(diyar::number_line(lubridate::dmy_hms("19/05/2018 00:00:00"), lubridate::dmy_hms("25/05/2018 00:00:00")), 2),
  diyar::number_line(lubridate::dmy_hms("31/05/2018 00:00:00"), lubridate::dmy_hms("31/05/2018 00:00:00"))
)

test_that("testing; stratified grouping 2", {
  expect_equal(test_10b$epid, c(1, rep(2,3), 5,2, 7,7,9,9, 11))
  expect_equal(test_10b$case_nm, c("Case","Case","Duplicate","Recurrent",
                                   "Case", "Duplicate", "Case",
                                   "Duplicate","Case","Duplicate","Case"
                                   ))
  e_int@id <- 1:11
  e_int@gid <- c(1, rep(2,3), 5,2, 7,7,9,9, 11)

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

data(hospital_admissions)
admissions <- hospital_admissions
admissions$epi_len <- 0
admissions$admin_period <- diyar::number_line(admissions$admin_dt, admissions$discharge_dt)
admissions

# episodes of overlaping intervals of admission
test_11a <-bind_cols(
  admissions,
  episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len, group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/01/2019 00:00:00"), lubridate::dmy_hms("15/01/2019 00:00:00")), 7),
  rep(diyar::number_line(lubridate::dmy_hms("20/01/2019 00:00:00"), lubridate::dmy_hms("31/01/2019 00:00:00")), 2)
)

test_that("testing; intervals grouping", {
  expect_equal(test_11a$epid, c(rep(2,7), rep(8,2)))
  expect_equal(test_11a$case_nm, c("Duplicate","Case", rep("Duplicate",5),
                                   "Case", "Duplicate"))
  e_int@id <- 1:9
  e_int@gid <- c(rep(2,7), rep(8,2))

  expect_equal(test_11a$epid_interval, e_int)
  expect_equal(test_11a$epid_total, c(rep(7,7), rep(2,2)))
  expect_equal(test_11a$epid_length, lubridate::as.difftime(c(rep(14,7), rep(11,2)), units = "days" ))
})

# rolling episodes of overlaping intervals of admission, and those within 10 days of the last interval
admissions$epi_len <- 0
admissions$recur <- 1

test_11b <- bind_cols(
  admissions,
  episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len,
                episode_type = "rolling", recurrence_length = recur, episode_unit = "months", group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/01/2019 00:00:00"), lubridate::dmy_hms("31/01/2019 00:00:00")), 9)
)

test_that("testing; intervals grouping for rolling intervals", {
  expect_equal(test_11b$epid, rep(2,9))
  expect_equal(test_11b$case_nm, c("Duplicate","Case",rep("Duplicate",5),
                                   "Recurrent", "Duplicate"))
  e_int@id <- 1:9
  e_int@gid <- rep(2,9)

  expect_equal(test_11b$epid_interval, e_int)
  expect_equal(test_11b$epid_total, rep(9,9))
  expect_equal(test_11b$epid_length, lubridate::as.difftime(rep(30,9), units = "days" ))
})

# fixed episodes of overlaping intervals of admission seperated by 1 month
admissions$epi_len <- 1

test_11c <- bind_cols(admissions,
          episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len, episode_unit = "months", group_stats = TRUE)) %>%
  select(-c(admin_dt, discharge_dt, sn))

e_int <- c(
  rep(diyar::number_line(lubridate::dmy_hms("01/01/2019 00:00:00"), lubridate::dmy_hms("31/01/2019 00:00:00")), 9)
)

test_that("testing; intervals grouping with a case length", {
  expect_equal(test_11c$epid, rep(2,9))
  expect_equal(test_11c$case_nm, c("Duplicate","Case",rep("Duplicate",7)))
  e_int@id <- 1:9
  e_int@gid <- rep(2,9)
  expect_equal(test_11c$epid_interval, e_int)
  expect_equal(test_11c$epid_total, rep(9,9))
  expect_equal(test_11c$epid_length, lubridate::as.difftime(rep(30,9), units = "days" ))
})

dft_11 <- dft_10 <- dft_9 <- dft_8 <- admissions
dft_8$rd_id <- -dft_8$rd_id
dft_9$rd_id <- c(1,1,3:9)

dft_10$epi_len <- -3
dft_11$recur <- "A"

test_that("test that error and warning messages are returned correctly", {
  expect_error(episode_group(as.list(dft_8), date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = TRUE), "A dataframe is required")
  expect_error(episode_group(dft_8, date=admin_periods, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = TRUE), "object 'admin_periods' not found")
  expect_error(episode_group(dft_8, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = TRUE), "'rd_id' as 'sn' must be > 0")
  expect_error(episode_group(dft_9, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = TRUE), "'rd_id' as 'sn' must not have duplicate values")
  expect_error(episode_group(admissions, date=epi_len, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = TRUE), "'epi_len' as 'date' must be a date, datetime or lubridate interval, and not have missing values")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", group_stats = "TRUE"), "'group_stats', 'from_last' and 'display' must be TRUE or FALSE")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = "months", from_last = "TRUE"), "'group_stats', 'from_last' and 'display' must be TRUE or FALSE")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_unit = 1), "'episode_unit' must be a character of length 1")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_type = 1), "'episode_type' must be a character of length 1")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_type = c("rolling","fixed")), "'episode_type' must be a character of length 1")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_type = "moving"), "`episode_type` must be either 'rolling' or 'fixed'")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, overlap_method = c("aligning")), "`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, overlap_method = 10), "'overlap_method' must be a character object")
  expect_error(episode_group(admissions, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_type = "rolling", rolls_max = NA, episodes_max = NA), "'episodes_max' and 'rolls_max' must be, or can be coerced to an integer between 0 and Inf")
  expect_error(episode_group(dft_10, date=admin_period, sn=rd_id,
                             case_length = epi_len, episode_type = "rolling"), "'epi_len' as 'case_length' must be -1 or a positive integer, numeric or double data type")
  expect_error(episode_group(dft_11, date=admin_period, sn=rd_id,
                             case_length = epi_len, recurrence_length = recur, episode_type = "rolling"), "'recur' as 'recurrence_length' must be -1 or a positive integer, numeric or double data type")
})
