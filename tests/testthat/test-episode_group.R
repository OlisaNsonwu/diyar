context("testing episode_group function")

library(testthat)
library(diyar)

date <- function(x) as.Date(x, "%d/%m/%Y")
dttm <- function(x) as.POSIXct(x, "GMT",format="%d/%m/%Y %H:%M:%S")
suffix <- function(df, x){
  names(df) <- paste0(names(df), ".", x)
  df
}

# Test 1 - Fixed episodes
data <- data.frame(date = seq.POSIXt(dttm("01/04/2018 00:00:00"), dttm("31/05/2018 00:00:00"), by="3 days"))
data$pid <- "Patient 1"
data$episode_len <- 6
data$d <- data$episode_len * diyar::episode_unit$days

data$rd_id <- 1:nrow(data)
data$date_int <- as.number_line(data$date)
#data$date_int@id <- 1

episode_group <- function(...){
  x <- diyar::episode_group(..., display = "none")
  if(class(x) == "epid") x@options <- list()
  x
}

fixed_episodes <- function(...){
  x <- diyar::fixed_episodes(..., display = "none")
  if(class(x) == "epid") x@options <- list()
  x
}

rolling_episodes <- function(...){
  x <- diyar::rolling_episodes(..., display = "none")
  if(class(x) == "epid") x@options <- list()
  x
}

# episode grouping with episode_group()
test_1 <- cbind(head(data,10),
                episode_group(head(data,10), strata = pid, date = date, case_length = episode_len, group_stats = T, to_s4 = F))

test_that("test that row positions of the resulting dataframe are the same as supplied", {
  expect_equal(test_1$sn, head(data,10)$rd_id)
})

l <- c(
  rep("01/04/2018 00:00:00", 3),
  rep("10/04/2018 00:00:00", 3),
  rep("19/04/2018 00:00:00", 3),
  rep("28/04/2018 00:00:00", 1)
)

r <- c(
  rep("07/04/2018  00:00:00", 3),
  rep("16/04/2018  00:00:00", 3),
  rep("25/04/2018  00:00:00", 3),
  rep("28/04/2018  00:00:00", 1)
)

e_int <- number_line(dttm(l), dttm(r))

test_that("test that test episode identifier is as expected for fixed episodes", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10))
  expect_equal(test_1$case_nm, rep(c("Case",rep("Duplicate_C",2)),4)[1:10] )

  e_int@id <- 1L:10L
  e_int@gid <- as.integer(c(1,1,1,4,4,4,7,7,7,10))
  expect_equal(test_1$epid_start, left_point(e_int))
  expect_equal(test_1$epid_end, right_point(e_int))
  expect_equal(test_1$epid_total, c(rep(3,9),1))
  expect_equal(test_1$epid_length, as.difftime(c(rep(6,9),0), units = "days" ))
})

# Test 2 - Case assignment - Reverse chronological order
data_2 <- head(data, 10)
data_2$episode_len_s <- 13
data_2$d <- 13 * diyar::episode_unit$days

test_2 <-
  cbind(data_2,
        suffix(episode_group(data_2, strata = pid, date = date, case_length = episode_len_s, from_last = F, group_stats = T, to_s4 = F), 1),
        suffix(episode_group(data_2, strata = pid, date = date, case_length = episode_len_s, from_last = T, group_stats = T, to_s4 = F), 2)
  )

l <- c(rep("01/04/2018 00:00:00", 5), rep("16/04/2018 00:00:00", 5))
r <- c(rep("13/04/2018 00:00:00", 5), rep("28/04/2018 00:00:00", 5))
e_int.1 <- number_line(dttm(l), dttm(r))
e_int.2 <- reverse_number_line(e_int.1)

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid.1, c(rep(1,5),rep(6,5)))
  expect_equal(test_2$epid.2, c(rep(5,5),rep(10,5)))
  expect_equal(test_2$case_nm.1, rep(c("Case",rep("Duplicate_C",4)),2))
  expect_equal(test_2$case_nm.2, rep(c(rep("Duplicate_C",4),"Case"),2))

  e_int.2@id <- e_int.1@id <- 1:10
  e_int.1@gid <- as.integer(c(rep(1,5),rep(6,5)))
  e_int.2@gid <- as.integer(c(rep(5,5),rep(10,5)))

  expect_equal(test_2$epid_start.1, left_point(e_int.1))
  expect_equal(test_2$epid_end.1, right_point(e_int.1))
  expect_equal(test_2$epid_total.1, rep(5,10))
  expect_equal(test_2$epid_length.1, as.difftime(rep(12,10), units = "days" ))

  expect_equal(test_2$epid_start.2, left_point(e_int.2))
  expect_equal(test_2$epid_end.2, right_point(e_int.2))
  expect_equal(test_2$epid_total.2, rep(5,10))
  expect_equal(test_2$epid_length.2, as.difftime(rep(-12,10), units = "days" ))

})

# Test 3 - Rolling episodes
test_3 <- cbind(data_2,
                suffix(episode_group(data_2, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", from_last = F, group_stats = T, to_s4 = F), 1),
                suffix(episode_group(data_2, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", from_last = T, group_stats = T, to_s4 = F), 2))

l <- rep("01/04/2018 00:00:00", 10)
r <- rep("28/04/2018 00:00:00", 10)
e_int.1 <- number_line(dttm(l), dttm(r))
e_int.2 <- reverse_number_line(e_int.1)

test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid.1, rep(1,10))
  expect_equal(test_3$epid.2, rep(10,10))
  expect_equal(test_3$case_nm.1, c("Case",rep("Duplicate_C",4),"Recurrent",rep("Duplicate_R",3),"Recurrent"))
  expect_equal(test_3$case_nm.2, rev(c("Case",rep("Duplicate_C",4),"Recurrent",rep("Duplicate_R",3), "Recurrent")))

  e_int.2@id <- e_int.1@id <- 1L:10L
  e_int.1@gid <- rep(1L, 10)
  e_int.2@gid <- rep(10L, 10)

  expect_equal(test_3$epid_start.1, left_point(e_int.1))
  expect_equal(test_3$epid_end.1, right_point(e_int.1))
  expect_equal(test_3$epid_total.1, rep(10L, 10))
  expect_equal(test_3$epid_length.1, as.difftime(rep(27,10), units = "days" ))

  expect_equal(test_3$epid_start.2, left_point(e_int.2))
  expect_equal(test_3$epid_end.2, right_point(e_int.2))
  expect_equal(test_3$epid_total.2, rep(10L, 10))
  expect_equal(test_3$epid_length.2, as.difftime(rep(-27,10), units = "days" ))

})

# Test 3 - Rolls max
data_4 <-data_2
data_4$recurrence <- 3
data_4$r <- 3 * diyar::episode_unit$days

test_4 <- cbind(data_4,
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, group_stats = T, to_s4 = F), 1),
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, rolls_max = 1, group_stats = T, to_s4 = F), 2))

l <- rep("01/04/2018 00:00:00", 10)
r <- rep("28/04/2018 00:00:00", 10)
e_int.1 <- number_line(dttm(l), dttm(r))

l <- c(rep("01/04/2018 00:00:00", 6), rep("19/04/2018 00:00:00", 4))
r <- c(rep("16/04/2018 00:00:00", 6), rep("28/04/2018 00:00:00", 4))
e_int.2 <- number_line(dttm(l), dttm(r))

test_that("test user defined recurrence length and roll_max", {
  expect_equal(test_4$epid.1, rep(1,10))
  expect_equal(test_4$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_4$case_nm.1, c("Case",rep("Duplicate_C",4), rep("Recurrent",5) ))
  expect_equal(test_4$case_nm.2, c("Case",rep("Duplicate_C",4),"Recurrent","Case",rep("Duplicate_C",3)))

  e_int.2@id <- e_int.1@id <- 1L:10L
  e_int.1@gid <- rep(1L, 10)
  e_int.2@gid <- c(rep(1L, 6), rep(7L, 4))

  expect_equal(test_4$epid_start.1, left_point(e_int.1))
  expect_equal(test_4$epid_end.1, right_point(e_int.1))
  expect_equal(test_4$epid_total.1, rep(10,10))
  expect_equal(test_4$epid_length.1, as.difftime(rep(27,10), units = "days" ))

  expect_equal(test_4$epid_start.2, left_point(e_int.2))
  expect_equal(test_4$epid_end.2, right_point(e_int.2))
  expect_equal(test_4$epid_total.2, c(rep(6,6), rep(4,4)))
  expect_equal(test_4$epid_length.2, as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))
})

# Test 5 - Episodes max
test_5 <- cbind(data_4,
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="fixed", recurrence_length = recurrence, episodes_max = 1, group_stats = T, to_s4 = F), 1),
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="fixed", recurrence_length = recurrence, episodes_max = 2, group_stats = T, to_s4 = F), 2))

l <- c(rep("01/04/2018 00:00:00", 5),"16/04/2018 00:00:00","19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
r <- c(rep("13/04/2018 00:00:00", 5),"16/04/2018 00:00:00","19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
e_int.1 <- number_line(dttm(l), dttm(r))

l <- c(rep("01/04/2018 00:00:00", 5), rep("16/04/2018 00:00:00", 5))
r <- c(rep("13/04/2018 00:00:00", 5), rep("28/04/2018 00:00:00", 5))
e_int.2 <- number_line(dttm(l), dttm(r))

test_that("testing user defined episodes_max", {
  expect_equal(test_5$epid.1, c(rep(1,5),6:10))
  expect_equal(test_5$epid.2, c(rep(1,5), rep(6,5)))
  expect_equal(test_5$case_nm.1, c("Case",rep("Duplicate_C",4),rep("Skipped",5)))
  expect_equal(test_5$case_nm.2, rep(c("Case",rep("Duplicate_C",4)),2))

  e_int.2@id <- e_int.1@id <- 1L:10L
  e_int.1@gid <- c(rep(1L, 5), 6L:10L)
  e_int.2@gid <- c(rep(1L, 5), rep(6L, 5))

  expect_equal(test_5$epid_start.1, left_point(e_int.1))
  expect_equal(test_5$epid_end.1, right_point(e_int.1))
  expect_equal(test_5$epid_total.1, rep(c(rep(5L, 5), rep(1L, 5))))
  expect_equal(test_5$epid_length.1, as.difftime(c(rep(12,5), rep(0,5)), units = "days" ))

  expect_equal(test_5$epid_start.2, left_point(e_int.2))
  expect_equal(test_5$epid_end.2, right_point(e_int.2))
  expect_equal(test_5$epid_total.2, rep(5L, 10))
  expect_equal(test_5$epid_length.2, as.difftime(rep(12,10), units = "days" ))
})


# Test 6 - Combining rolls_max and episodes_max
test_6 <- cbind(data_4,
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 1, rolls_max = 1, group_stats = T, to_s4 = F), 1),
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 1, group_stats = T, to_s4 = F), 2),
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 1, group_stats = T, to_s4 = F), 3),
                suffix(episode_group(data_4, sn=rd_id, strata = pid, date = date, case_length = episode_len_s, episode_type ="rolling", recurrence_length = recurrence, episodes_max = 2, rolls_max = 3, group_stats = T, to_s4 = F), 4))

l <- c(rep("01/04/2018 00:00:00", 6),"19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
r <- c(rep("16/04/2018 00:00:00", 6),"19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
e_int.1 <- number_line(dttm(l), dttm(r))

l <- c(rep("01/04/2018 00:00:00", 6), rep("19/04/2018 00:00:00", 4))
r <- c(rep("16/04/2018 00:00:00", 6), rep("28/04/2018 00:00:00", 4))
e_int.2 <- number_line(dttm(l), dttm(r))

e_int.3 <- e_int.2

l <- c(rep("01/04/2018 00:00:00", 8), rep("25/04/2018 00:00:00", 2))
r <- c(rep("22/04/2018 00:00:00", 8), rep("28/04/2018 00:00:00", 2))
e_int.4 <- number_line(dttm(l), dttm(r))

test_that("testing episodes_max and rolls_max combinations", {
  expect_equal(test_6$epid.1, c(rep(1,6),7:10))
  expect_equal(test_6$epid.2, c(rep(1,6), rep(7,4)))
  expect_equal(test_6$case_nm.1, c("Case",rep("Duplicate_C",4),"Recurrent",rep("Skipped",4)))
  expect_equal(test_6$case_nm.2, rep(c("Case",rep("Duplicate_C",4),"Recurrent"),2)[1:10])

  expect_equal(test_6$epid.3, c(rep(1,6),rep(7,4)))
  expect_equal(test_6$epid.4, c(rep(1,8), rep(9,2)))
  expect_equal(test_6$case_nm.3, c("Case",rep("Duplicate_C",4),"Recurrent", "Case", rep("Duplicate_C",3)))
  expect_equal(test_6$case_nm.4, rep(c("Case",rep("Duplicate_C",4),rep("Recurrent",3)),2)[1:10])

  e_int.4@id <- e_int.3@id <- e_int.2@id <- e_int.1@id <- 1L:10L
  e_int.1@gid <- c(rep(1L, 6), 7L:10L)
  e_int.3@gid  <- e_int.2@gid <- c(rep(1L, 6), rep(7L, 4))
  e_int.4@gid <- c(rep(1L, 8), rep(9L, 2))
  expect_equal(test_6$epid_start.1, left_point(e_int.1))
  expect_equal(test_6$epid_end.1, right_point(e_int.1))

  expect_equal(test_6$epid_total.1, rep(c(rep(6L, 6), rep(1L, 4))))
  expect_equal(test_6$epid_length.1, as.difftime(c(rep(15,6), rep(0,4)), units = "days" ))

  expect_equal(test_6$epid_start.2, left_point(e_int.2))
  expect_equal(test_6$epid_end.2, right_point(e_int.2))
  expect_equal(test_6$epid_total.2, rep(c(rep(6L, 6), rep(4L, 4))))
  expect_equal(test_6$epid_length.2, as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))

  expect_equal(test_6$epid_start.3, left_point(e_int.3))
  expect_equal(test_6$epid_end.3, right_point(e_int.3))
  expect_equal(test_6$epid_total.3, rep(c(rep(6L, 6), rep(4L, 4))))
  expect_equal(test_6$epid_length.3, as.difftime(c(rep(15,6), rep(9,4)), units = "days" ))

  expect_equal(test_6$epid_start.4, left_point(e_int.4))
  expect_equal(test_6$epid_end.4, right_point(e_int.4))
  expect_equal(test_6$epid_total.4, rep(c(rep(8L, 8), rep(2L, 2))))
  expect_equal(test_6$epid_length.4, as.difftime(rep(c(rep(21,8), rep(3,2))), units = "days" ))
})

# Test 7 - Deterministic linkage
data_7 <- data_4
data_7$recurrence <- 2
data_7$dataset <- paste("DS",c(1:3, rep(c(1:2),2), rep(3,3)), sep="")

test_7 <- cbind(data_7,
                suffix(episode_group(data_7, sn=rd_id, strata = pid, date = date, case_length = episode_len, episode_type ="rolling", recurrence_length = recurrence, data_source = dataset, group_stats = T, to_s4 = F), 1),
                suffix(episode_group(data_7, sn=rd_id, strata = pid, date = date, case_length = episode_len, episode_type ="rolling", recurrence_length = recurrence, data_source = c(dataset, episode_len_s), group_stats = T, to_s4 = F), 2))

l <- c(rep("01/04/2018 00:00:00", 3), rep("10/04/2018 00:00:00", 3), rep("19/04/2018 00:00:00", 3), "28/04/2018 00:00:00")
r <- c(rep("07/04/2018 00:00:00", 3), rep("16/04/2018 00:00:00", 3), rep("25/04/2018 00:00:00", 3), "28/04/2018 00:00:00")
e_int.1 <- number_line(dttm(l), dttm(r))

test_that("testing epid_dataset", {
  expect_equal(test_7$epid.1, c(rep(1,3),rep(4,3),rep(7,3),10))
  expect_equal(test_7$case_nm.1, rep(c("Case",rep("Duplicate_C",2)),4)[1:10])
  expect_equal(test_7$epid_dataset.1, c(rep("DS1,DS2,DS3",3),rep("DS1,DS2",3),rep("DS2,DS3",3),"DS3" ))

  e_int.1@id <- 1L:10L
  e_int.1@gid <- as.integer(c(rep(1L, 3), rep(4L, 3),rep(7L,3),10))

  expect_equal(test_7$epid_start.1, left_point(e_int.1))
  expect_equal(test_7$epid_end.1, right_point(e_int.1))
  expect_equal(test_7$epid_total.1, c(rep(3,9),1))
  expect_equal(test_7$epid_length.1, as.difftime(c(rep(6,9),0), units = "days" ))

  expect_equal(test_7$epid.1, test_7$epid.2)
  expect_equal(test_7$case_nm.1, test_7$case_nm.2)
  expect_equal(test_7$epid_start.1, test_7$epid_start.2)
  expect_equal(test_7$epid_end.1, test_7$epid_end.2)
  expect_equal(test_7$epid_total.1, test_7$epid_total.2)
  expect_equal(test_7$epid_length.1, test_7$epid_length.2)

  expect_equal(test_7$epid_dataset.2, c(rep("DS1-13,DS2-13,DS3-13",3),rep("DS1-13,DS2-13",3),rep("DS2-13,DS3-13",3),"DS3-13" ))
})

hospital_infections <- diyar::infections
# Test 8 - Episode unit
# 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
test_8a <- cbind(hospital_infections,
                 episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                               from_last = T, episode_unit = "hours", group_stats = T, to_s4 = F))

e_int <- number_line(dttm(format(test_8a$date, "%d/%m/%Y 00:00:00")), dttm(format(test_8a$date, "%d/%m/%Y 00:00:00")))

test_that("testing; episode grouping by the hour", {
  expect_equal(test_8a$epid, 1L:11L)
  expect_equal(test_8a$case_nm, rep("Case",11))

  e_int@gid <- e_int@id <- 1L:11L

  expect_equal(test_8a$epid_start, left_point(e_int))
  expect_equal(test_8a$epid_end, right_point(e_int))
  expect_equal(test_8a$epid_total, rep(1,11))
  expect_equal(test_8a$epid_length, as.difftime(rep(0,11), units = "hours" ))
})

# 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
test_8b <- cbind(hospital_infections,
                 episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
                               from_last = T, episode_unit = "weeks", group_stats = T, to_s4 = F))

l <- rep("31/05/2018 00:00:00", 11)
r <- rep("01/04/2018 00:00:00", 11)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; episode grouping by weeks", {
  expect_equal(test_8b$epid, rep(11L,11))

  e_int@id <- 1L:11L
  e_int@gid <- as.integer(rep(11L,11))

  expect_equal(test_8b$case_nm, c(rep("Duplicate_C",10),"Case"))
  expect_equal(test_8b$epid_start, left_point(e_int))
  expect_equal(test_8b$epid_end, right_point(e_int))
  expect_equal(test_8b$epid_total, rep(11L,11))
  expect_equal(round(test_8b$epid_length,6), as.difftime(rep(-8.571429,11), units = "weeks"))
})

# Test 9 - User defined case assignment
# preference for case assignment - UTI > BSI > RTI
hospital_infections$infection <- factor(hospital_infections$infection, levels = c("UTI","BSI","RTI"))

# Different case and recurrence lengths for different source of infection
hospital_infections$epi_len <- ifelse(hospital_infections$infection =="BSI", 14, 0)
hospital_infections$epi_len <- ifelse(hospital_infections$infection =="UTI", 30, hospital_infections$epi_len)
hospital_infections$epi_len <- ifelse(hospital_infections$infection =="RTI", 60, hospital_infections$epi_len)


# n-day episodes beginning with the earliest record with the specified preference; UTI > BSI > RTI
test_9a <- cbind(hospital_infections,
                 episode_group(hospital_infections, sn = rd_id, date=date, case_length = epi_len,
                               custom_sort = infection, group_stats = T, to_s4 = F))

l <- c("01/04/2018 00:00:00", rep("07/04/2018 00:00:00", 6), rep("13/05/2018 00:00:00", 3), "31/05/2018 00:00:00")
r <- c("01/04/2018 00:00:00", rep("07/05/2018 00:00:00", 6), rep("25/05/2018 00:00:00", 3), "31/05/2018 00:00:00")
e_int <- number_line(dttm(l), dttm(r))

test_that("testing episode; custom sort", {
  expect_equal(test_9a$epid, c(1,rep(2,6), rep(8,3), 11))
  expect_equal(test_9a$case_nm, c("Case","Case",rep("Duplicate_C",5),"Case", rep("Duplicate_C",2), "Case"))

  e_int@id <- 1L:11L
  e_int@gid <- as.integer(c(1,rep(2,6), rep(8,3), 11))

  expect_equal(test_9a$epid_start, left_point(e_int))
  expect_equal(test_9a$epid_end, right_point(e_int))
  expect_equal(test_9a$epid_total, c(1,rep(6,6), rep(3,3), 1))
  expect_equal(test_9a$epid_length, as.difftime(c(0,rep(30,6), rep(12,3), 0), units = "days" ))
})

# preference for case assignment - RTI > UTI, or  RTI > BSI, or earliest record
hospital_infections$infection_ord <- ifelse(hospital_infections$infection =="RTI",0,1)

# n-day episodes with duplicates before and after the most recent "RTI" record, otherwise begin at the most recent record
test_9b <- cbind(hospital_infections,
                 suffix(episode_group(hospital_infections, sn = rd_id, date=date, case_length = epi_len,
                                      custom_sort = infection_ord, from_last = T, bi_direction = T, group_stats = T, to_s4 = F), 1),

                 suffix(episode_group(hospital_infections, sn = rd_id, date=date, case_length = epi_len,
                                      custom_sort = infection_ord, from_last = T, bi_direction = F, group_stats = T, to_s4 = F), 2))

l <- rep("31/05/2018 00:00:00", 11)
r <- rep("01/04/2018 00:00:00", 11)
e_int.1 <- number_line(dttm(l), dttm(r))

l <- c(rep("25/05/2018 00:00:00", 10), "31/05/2018 00:00:00")
r <- c(rep("01/04/2018 00:00:00", 10), "31/05/2018 00:00:00")
e_int.2 <- number_line(dttm(l), dttm(r))


test_that("testing; episode grouping with custom sort and bi_direction", {
  expect_equal(test_9b$epid.1, rep(10,11))
  expect_equal(test_9b$case_nm.1, c(rep("Duplicate_C",9),"Case","Duplicate_C"))

  e_int.2@id <- e_int.1@id <- 1L:11L
  e_int.1@gid <- as.integer(rep(10,11))

  expect_equal(test_9b$epid_start.1, left_point(e_int.1))
  expect_equal(test_9b$epid_end.1, right_point(e_int.1))
  expect_equal(test_9b$epid_total.1, rep(11L,11))
  expect_equal(test_9b$epid_length.1, as.difftime(rep(-60,11), units = "days" ))

  e_int.2@gid <- as.integer(c(rep(10,10), 11))

  expect_equal(test_9b$epid.2, c(rep(10,10), 11))
  expect_equal(test_9b$case_nm.2, c(rep("Duplicate_C",9),"Case","Case"))
  expect_equal(test_9b$epid_start.2, left_point(e_int.2))
  expect_equal(test_9b$epid_end.2, right_point(e_int.2))
  expect_equal(test_9b$epid_total.2, c(rep(10,10),1))
  expect_equal(test_9b$epid_length.2, as.difftime(c(rep(-54,10), 0), units = "days" ))
})

# Test 10 - Stratified episode grouping
hospital_infections$patient_id <- c(rep("PID 1",8), rep("PID 2",3))

# Only one n-day episode per patient_id
hospital_infections$infection <- as.character(hospital_infections$infection)
test_10a <- cbind(hospital_infections,
                  episode_group(hospital_infections, sn = rd_id, date=date, strata = patient_id, case_length = epi_len,
                                episodes_max = 1, from_last = F, data_source = infection, group_stats = T, to_s4 = F))

l <- c(rep("01/04/2018 00:00:00", 3), "19/04/2018 00:00:00", "25/04/2018 00:00:00", "01/05/2018 00:00:00", "07/05/2018 00:00:00", "13/05/2018 00:00:00", rep("19/05/2018 00:00:00", 3))
r <- c(rep("13/04/2018 00:00:00", 3), "19/04/2018 00:00:00", "25/04/2018 00:00:00", "01/05/2018 00:00:00", "07/05/2018 00:00:00", "13/05/2018 00:00:00", rep("31/05/2018 00:00:00", 3))
e_int <- number_line(dttm(l), dttm(r))


test_that("testing; stratified grouping", {
  expect_equal(test_10a$epid, c(rep(1,3), 4:8, rep(9,3)))
  expect_equal(test_10a$case_nm, c("Case",rep("Duplicate_C",2), rep("Skipped",5), "Case", rep("Duplicate_C",2)))

  e_int@id <- 1L:11L
  e_int@gid <- as.integer(c(rep(1,3), 4:8, rep(9,3)))

  expect_equal(test_10a$epid_start, left_point(e_int))
  expect_equal(test_10a$epid_end, right_point(e_int))
  expect_equal(test_10a$epid_total, c(rep(3,3), rep(1,5), rep(3,3)))
  expect_equal(test_10a$epid_length, as.difftime(c(rep(12,3), rep(0,5), rep(12,3)), units = "days" ))
  expect_equal(test_10a$epid_dataset, c(rep("BSI,UTI",3), "UTI","BSI","UTI","BSI","BSI",rep("BSI,RTI",3)))
})

test_10a.1 <- cbind(hospital_infections,
                    episode_group(hospital_infections, sn = rd_id, date=date, strata = patient_id, case_length = epi_len,
                                  episode_type="rolling", data_source = infection, group_stats = T, to_s4 = F))

# Only three 9-day (difference of 8 days) rolling episode per patient and infection.
hospital_infections$epi_len <- 8
hospital_infections$recur <- 30
test_10b <- cbind(hospital_infections,
                  episode_group(hospital_infections, sn = rd_id, date=date, strata = c(patient_id, infection), case_length = epi_len,
                                episode_type = "rolling", recurrence_length = recur, episodes_max = 3, data_source = c(patient_id, infection), group_stats = T, to_s4 = F))

l <- c("01/04/2018 00:00:00", rep("07/04/2018 00:00:00", 3), "01/04/2018 00:00:00", "07/04/2018 00:00:00", "01/04/2018 00:00:00", "01/04/2018 00:00:00", rep("19/05/2018 00:00:00", 2), "31/05/2018 00:00:00")
r <- c("13/05/2018 00:00:00", rep("01/05/2018 00:00:00", 3), "13/05/2018 00:00:00", "01/05/2018 00:00:00", "13/05/2018 00:00:00", "13/05/2018 00:00:00", rep("25/05/2018 00:00:00", 2), "31/05/2018 00:00:00")
e_int <- number_line(dttm(l), dttm(r))


test_that("testing; stratified grouping 2", {
  expect_equal(test_10b$epid, c(1,2,2,2,1,2,1,1,9,9, 11))
  expect_equal(test_10b$case_nm, c("Case","Case","Duplicate_C","Recurrent",
                                   "Recurrent", "Duplicate_R", "Recurrent",
                                   "Duplicate_R","Case","Duplicate_C","Case"))
  e_int@id <- 1L:11L
  e_int@gid <- as.integer(c(1,2,2,2,1,2,1,1,9,9, 11))

  expect_equal(test_10b$epid_start, left_point(e_int))
  expect_equal(test_10b$epid_end, right_point(e_int))
  expect_equal(test_10b$epid_total, c(rep(4,8),2,2,1))
  expect_equal(test_10b$epid_length, as.difftime(c(42, rep(24,3), 42, 24, 42, 42, rep(6,2), 0), units = "days" ))
})

#Test 11 - Interval grouping
admissions <- diyar::hospital_admissions
admissions$epi_len <- 0
admissions$admin_period <- number_line(admissions$admin_dt, admissions$discharge_dt)

admissions <- admissions[1L:9L,]
admissions

# episodes of overlaping intervals of admission
test_11a <- cbind(
  admissions,
  episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len, group_stats = T, to_s4 = F))

l <- c(rep("01/01/2019 00:00:00", 7), rep("20/01/2019 00:00:00", 2))
r <- c(rep("15/01/2019 00:00:00", 7), rep("31/01/2019 00:00:00", 2))
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping", {
  expect_equal(test_11a$epid, c(rep(2,7), rep(8,2)))
  expect_equal(test_11a$case_nm, c("Duplicate_C","Case", rep("Duplicate_C",5),
                                   "Case", "Duplicate_C"))
  e_int@id <- as.integer(1L:9L)
  e_int@gid <- as.integer(c(rep(2,7), rep(8,2)))

  expect_equal(test_11a$epid_start, left_point(e_int))
  expect_equal(test_11a$epid_end, right_point(e_int))
  expect_equal(test_11a$epid_total, c(rep(7,7), rep(2,2)))
  expect_equal(test_11a$epid_length, as.difftime(c(rep(14,7), rep(11,2)), units = "days" ))
})

# rolling episodes of overlapping intervals of admission, and those within 10 days of the last interval
admissions$epi_len <- 0
admissions$recur <- 1

test_11b <- cbind(
  admissions,
  episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len,
                episode_type = "rolling", recurrence_length = recur, episode_unit = "months", group_stats = T, to_s4 = F))

l <- rep("01/01/2019 00:00:00", 9)
r <- rep("31/01/2019 00:00:00", 9)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping for rolling intervals", {
  expect_equal(test_11b$epid, rep(2,9))
  expect_equal(test_11b$case_nm, c("Duplicate_C","Case",rep("Duplicate_C",5),
                                   "Recurrent", "Duplicate_R"))
  e_int@id <- 1L:9L
  e_int@gid <- as.integer(rep(2,9))

  expect_equal(test_11b$epid_start, left_point(e_int))
  expect_equal(test_11b$epid_end, right_point(e_int))
  expect_equal(test_11b$epid_total, rep(9,9))
  expect_equal(test_11b$epid_length, as.difftime(rep(30,9), units = "days" ))
})

# fixed episodes of overlapping intervals of admission separated by 1 month
admissions$epi_len <- 1

test_11c <- cbind(admissions,
                  episode_group(admissions, date=admin_period, sn=rd_id, case_length = epi_len, episode_unit = "months", group_stats = T, to_s4 = F))

l <- rep("01/01/2019 00:00:00", 9)
r <- rep("31/01/2019 00:00:00", 9)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping with a case length", {
  expect_equal(test_11c$epid, rep(2,9))
  expect_equal(test_11c$case_nm, c("Duplicate_C","Case",rep("Duplicate_C",7)))
  e_int@id <- 1L:9L
  e_int@gid <- as.integer(rep(2,9))
  expect_equal(test_11b$epid_start, left_point(e_int))
  expect_equal(test_11b$epid_end, right_point(e_int))
  expect_equal(test_11c$epid_total, rep(9,9))
  expect_equal(test_11c$epid_length, as.difftime(rep(30,9), units = "days" ))
})


infections <- diyar::infections

fixed_epids_1 <- episode_group(infections, case_length = epi_len, date=date, to_s4=T)
fixed_epids_2 <- fixed_episodes(date = infections$date, case_length = 15, to_s4=T)
fixed_epids_2b <- fixed_episodes(sn= 1:length(infections$date), date = infections$date, case_length = 15, to_s4=T)

fixed_epids_3 <- episode_group(infections, case_length = epi_len, date=date, data_source = infection, to_s4=T)
fixed_epids_4 <- fixed_episodes(date = infections$date, case_length = 15, data_source = infections$infection, to_s4=T)

fixed_epids_5 <- episode_group(infections, case_length = epi_len, date=date, strata = infection, to_s4=T)
fixed_epids_6 <- fixed_episodes(date = infections$date, case_length = 15, strata = infections$infection, to_s4=T)

fixed_epids_7 <- episode_group(infections, case_length = epi_len, date=date, custom_sort = infection, to_s4=T)
fixed_epids_8 <- fixed_episodes(date = infections$date, case_length = 15, custom_sort = infections$infection, to_s4=T)


rolling_epids_1 <- episode_group(infections, case_length = epi_len, date=date, episode_type = "rolling", to_s4=T)
rolling_epids_2 <- rolling_episodes(date = infections$date, case_length = 15, to_s4=T)
rolling_epids_2b <- rolling_episodes(sn= 1:length(infections$date), date = infections$date, case_length = 15, to_s4=T)

rolling_epids_3 <- episode_group(infections, case_length = epi_len, date=date, data_source = infection, episode_type = "rolling", to_s4=T)
rolling_epids_4 <- rolling_episodes(date = infections$date, case_length = 15, data_source = infections$infection, to_s4=T)

rolling_epids_5 <- episode_group(infections, case_length = epi_len, date=date, strata = infection, episode_type = "rolling", to_s4=T)
rolling_epids_6 <- rolling_episodes(date = infections$date, case_length = 15, strata = infections$infection, to_s4=T)

rolling_epids_7 <- episode_group(infections, case_length = epi_len, date=date, custom_sort = infection, episode_type = "rolling", to_s4=T)
rolling_epids_8 <- rolling_episodes(date = infections$date, case_length = 15, custom_sort = infections$infection, to_s4=T)
rolling_epids_8b <- rolling_episodes(date = infections$date, case_length = 15, recurrence_length = 15, custom_sort = infections$infection, to_s4=T)

dup_f_epid <- episode_group(infections, case_length = epi_len, date=date, to_s4=T, deduplicate = T)

test_that("test fixed and rolling episode values", {
  expect_equal(fixed_epids_1, fixed_epids_2)
  expect_equal(fixed_epids_2, fixed_epids_2b)
  expect_equal(fixed_epids_3, fixed_epids_4)
  expect_equal(fixed_epids_5, fixed_epids_6)
  expect_equal(fixed_epids_7, fixed_epids_8)
  expect_equal(rolling_epids_1, rolling_epids_2)
  expect_equal(rolling_epids_2, rolling_epids_2b)
  expect_equal(rolling_epids_3, rolling_epids_4)
  expect_equal(rolling_epids_5, rolling_epids_6)
  expect_equal(rolling_epids_7, rolling_epids_8)
  expect_equal(rolling_epids_8, rolling_epids_8b)
  expect_equal(dup_f_epid@.Data, unique(fixed_epids_1)@.Data)
})

test_that("test that fixed_episodes() with numeric 'date' works the same as compress_number_line()", {
  a <- fixed_episodes(date = c(1,1,4,4,1,4,3,2), case_length = 0, to_s4 = T, group_stats = T, deduplicate = F)
  b <- compress_number_line(x = as.number_line(c(1,1,4,4,1,4,3,2)), collapse =T, deduplicate = F)
  expect_equal(a@epid_interval, b)
})

test_that("test some generic functions", {
  expect_equal(show(new("epid")), "epid(0)")
  b <- rep(as.epid(5L), 2)
  b@epid_interval@gid <- b@epid_interval@id <- 1:length(b)
  # temp
  # expect_equal(c(as.epid(5L), as.epid(5L)), b)
})

x <- c("01/04/2019", "04/04/2019", "14/04/2019", "16/04/2019", "19/04/2019")
x <- date(x)
df <- data.frame(x=x, c=5, r=10)
epids_a <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, case_for_recurrence = T, rolls_max = 1, episode_type = "rolling", group_stats = T)
epids_b <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, case_for_recurrence = F, rolls_max = 1, episode_type = "rolling", group_stats = T)

x <- c("01/04/2019", "04/04/2019", "8/04/2019", "14/04/2019", "16/04/2019", "19/04/2019")
x <- date(x)
df <- data.frame(x=x, c=5, r=10)
epids2_a <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, case_for_recurrence = T, rolls_max = 1, episode_type = "rolling")
epids2_b <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, case_for_recurrence = F, rolls_max = 1, episode_type = "rolling")

x <- c("01/04/2019", "04/04/2019", "12/04/2019", "14/04/2019", "16/04/2019", "19/04/2019")
x <- date(x)
df <- data.frame(x=x, c=5, r=10)
epids3_a <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, reference_event = "last_record", rolls_max = 2, episode_type = "rolling")
epids3_b <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, reference_event = "first_record", rolls_max = 2, episode_type = "rolling")

x <- c(date("01/01/2007"), date("07/01/2007"), date("09/01/2007"), date("19/01/2007"))
df <- data.frame(x=x, c=5, r=10)
epids6_a <- episode_group(df, date = x, case_length = c, recurrence_length = r, to_s4 = T, episode_type = "rolling", reference_event = "last_record", rolls_max = 2)
epids6_b <- episode_group(df, date = x, case_length = c, recurrence_length = r, to_s4 = T, episode_type = "rolling", reference_event = "first_record", rolls_max = 2)

x <- c("01/04/2019", "07/04/2019", "12/04/2019","21/04/2019","26/04/2019","29/04/2019")
x <- date(x)
df <- data.frame(x=x, c=5, r=20)

epids4_a <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, reference_event = "last_record", rolls_max = 2, episode_type = "rolling")
epids4_b <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, reference_event = "first_record", rolls_max = 2, episode_type = "rolling")
epids4_c <- episode_group(df, date =x, case_length = c, recurrence_length = r, to_s4=T, reference_event = "first_record", case_for_recurrence =T, rolls_max = 2, episode_type = "rolling")


test_that("test 'case_for_recurrence' in rolling_episodes", {
  expect_equal(epids_a@.Data, rep(1,5))
  expect_equal(decode(epids_a@case_nm), c("Case","Duplicate_C","Recurrent","Duplicate_R","Duplicate_R"))
  expect_equal(epids_b@.Data, c(1,1,1,4,4))
  expect_equal(decode(epids_b@case_nm), c("Case","Duplicate_C","Recurrent","Case","Duplicate_C"))

  expect_equal(epids2_a@.Data, rep(1,6))
  expect_equal(decode(epids2_a@case_nm), c("Case","Duplicate_C","Recurrent","Duplicate_R","Duplicate_R","Duplicate_R"))
  expect_equal(epids2_b@.Data, c(1,1,1,1,5,5))
  expect_equal(decode(epids2_b@case_nm), c("Case","Duplicate_C","Recurrent","Duplicate_R","Case","Duplicate_C"))

  expect_equal(epids3_a@.Data, rep(1,6))
  expect_equal(decode(epids3_a@case_nm), c("Case","Duplicate_C","Recurrent","Duplicate_R","Recurrent","Duplicate_R"))
  expect_equal(epids3_b@.Data, c(1,1,3,3,3,3))
  expect_equal(decode(epids3_b@case_nm), c("Case","Duplicate_C","Case","Duplicate_C","Duplicate_C","Recurrent"))

  expect_equal(epids6_a@.Data, rep(1,4))
  expect_equal(decode(epids6_a@case_nm), c("Case","Recurrent","Duplicate_R","Recurrent"))
  expect_equal(epids6_b@.Data, c(1,1,1,4))
  expect_equal(decode(epids6_b@case_nm), c("Case","Recurrent","Duplicate_R","Case"))

  expect_equal(epids4_a@.Data, rep(1,6))
  expect_equal(decode(epids4_a@case_nm), c("Case","Recurrent","Duplicate_R","Duplicate_R","Recurrent","Duplicate_R"))
  expect_equal(epids4_b@.Data, c(rep(1,5),6))
  expect_equal(decode(epids4_b@case_nm), c("Case","Recurrent","Duplicate_R","Duplicate_R","Recurrent","Case"))
  expect_equal(epids4_c@.Data, c(rep(1,4),5,5))
  expect_equal(decode(epids4_c@case_nm), c("Case","Recurrent","Duplicate_R","Duplicate_R","Case","Duplicate_C"))
})


x <- date(c("01/01/2007","04/01/2007","12/01/2007","15/01/2007","22/01/2007"))
df <- data.frame(x=x, c=5, r=10)
epids_r <- episode_group(df, date = x, case_length = c, recurrence_length = r, to_s4 = T, episode_type = "rolling")

x <- c(date("01/01/2007"), date("10/01/2007"), date("12/01/2007"), date("15/01/2007"), date("22/01/2007"))
df <- data.frame(x=x, c=5, r=10)
epids2_r <- episode_group(df, date = x, case_length = c, recurrence_length = r, to_s4 = T, episode_type = "rolling")

test_that("test rolling_episodes", {
  expect_equal(epids_r@.Data, rep(1,5))
  expect_equal(decode(epids_r@case_nm), c("Case","Duplicate_C","Recurrent","Recurrent","Duplicate_R"))
  expect_equal(epids2_r@.Data, rep(1,5))
  expect_equal(decode(epids2_r@case_nm), c("Case","Recurrent","Recurrent","Duplicate_R","Recurrent"))
})

d <- seq.Date(date("01/04/2019"), date("18/04/2019"), "3 day" )
df<- data.frame(date=d)
df$r1 <- rolling_episodes(date = d, case_length = 3, case_for_recurrence = F, to_s4 = T)
df$r2 <- rolling_episodes(date = d, case_length = 3, case_for_recurrence = T, to_s4 = T)

test_that("test case_for_recurrence", {
  expect_equal(df$r1@.Data, rep(1,6))
  expect_equal(decode(df$r1@case_nm), c("Case","Duplicate_C","Recurrent","Recurrent","Recurrent", "Recurrent"))
  expect_equal(df$r2@.Data, rep(1,6))
  expect_equal(decode(df$r2@case_nm), c("Case","Duplicate_C","Recurrent","Duplicate_R","Recurrent","Duplicate_R"))
})

# Lengths range
dts <- 1:10
c1 <- rep(number_line(2, 3), 10)
c2 <- rep(number_line(0, 3), 10)
c3 <- rep(number_line(-3, 0), 10)
c5 <- rep(number_line(-3, -2), 10)
c4 <- rep(number_line(-3, 3), 10)
c7 <- 1
cs <- c(rep(3,5), rep(1,5))
df <- data.frame(dt= dts, c_sort = cs, stringsAsFactors = F)

df$c1 = c1
df$c2 = c2
df$c3 = c3
df$c4 = c4
df$c5 = c5
df$c7 = c7

df$ep1 <- episode_group(df, date=dt, case_length = c1)
df$ep2 <- episode_group(df, date=dt, case_length = c1, bi_direction = T)
# Test - Expect TRUE
all(df$ep1==df$ep2)

df$ep3 <- episode_group(df, date=dt, case_length = c2)
df$ep4 <- episode_group(df, date=dt, case_length = c2, bi_direction = T)
# Test - Expect TRUE
all(df$ep3==df$ep4)

df$ep5 <- episode_group(df, date=dt, case_length = c3)
df$ep6 <- episode_group(df, date=dt, case_length = c3, bi_direction = T)
# Test - Expect TRUE
all(df$ep3==df$ep6)

df$ep7 <- episode_group(df, date=dt, case_length = c3, custom_sort = c_sort)

df$ep8 <- episode_group(df, date=dt, case_length = c1, custom_sort = c_sort, bi_direction = T)
df$ep9 <- episode_group(df, date=dt, case_length = c5, custom_sort = c_sort, bi_direction = T)
# Test - Expect TRUE
all(df$ep8 == df$ep9)

#test that these two should give the same result
df$ep10 <- episode_group(df, date=dt, case_length = c1, custom_sort = c_sort, bi_direction = T, from_last = T)
df$ep11 <- episode_group(df, date=dt, case_length = c5, custom_sort = c_sort, bi_direction = T, from_last = T)
# Test - Expect TRUE
all(df$ep10 == df$ep11)
test_that("test case_for_recurrence", {
  expect_equal(df$ep1, df$ep2)
  expect_equal(df$ep3, df$ep4)
  expect_equal(df$ep3, df$ep6)
  expect_equal(df$ep4, df$ep6)
  expect_equal(df$ep8, df$ep9)
  expect_equal(df$ep11, df$ep10)
})


df <- data.frame(x=c(1,6,7,8,10), rc=8, ep =4)
df$ep1 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = T, episode_type = "rolling", rolls_max = 1)
df$ep2 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = F, episode_type = "rolling", rolls_max = 1)

df$ep3 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = T, reference_event = "first_record", episode_type = "rolling", rolls_max = 1)
df$ep4 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = F, reference_event = "first_record", episode_type = "rolling", rolls_max = 1)

df$ep5 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = T, episode_type = "rolling", rolls_max = 2)
df$ep6 <- episode_group(df, date =x, case_length = ep, recurrence_length = rc, case_for_recurrence = F, episode_type = "rolling", rolls_max = 2)

test_that("test wind_id and wind_nm", {
  expect_equal(df$ep1@.Data, rep(1, 5))
  expect_equal(decode(df$ep1@case_nm), c("Case","Recurrent", rep("Duplicate_R", 3)))
  expect_equal(decode(df$ep1@wind_nm), c("Recurrence", rep("Recurrence", 3), "Case"))
  expect_equal(df$ep1@wind_id[[1]], c(rep(1,4),4))
  expect_equal(df$ep2, df$ep4)
  expect_equal(decode(df$ep1@case_nm), decode(df$ep3@case_nm))
  expect_equal(decode(df$ep1@wind_nm), decode(df$ep3@wind_nm))
  expect_equal(df$ep1@.Data, df$ep3@.Data)
  expect_equal(df$ep3@wind_id[[1]], c(rep(1,4),2))
  expect_equal(df$ep1, df$ep5)
  expect_equal(df$ep1@.Data, df$ep6@.Data)
  expect_equal(decode(df$ep6@case_nm), c("Case","Recurrent", rep("Duplicate_R", 2), "Recurrent"))
  expect_equal(decode(df$ep6@wind_nm), c("Recurrence", rep("Recurrence", 4)))
})

x <- c(1,6,7,8,10)
df1 <- data.frame(x=x, rc=0)
df1$cl <- number_line(9,10)
df1$ep1 <- episode_group(df1, date =x, case_length = cl, recurrence_length = rc, episode_type = "rolling", skip_if_b4_lengths = T)
df1$ep2 <- episode_group(df1, date =x, case_length = cl, recurrence_length = rc, episode_type = "rolling", skip_if_b4_lengths = F)

x <- seq(1,20,3)
df2 <- data.frame(x=x, rc=0)
df2$ep <- number_line(6,6)
df2$ep3 <- episode_group(df2, date =x, case_length = ep, recurrence_length = rc, episode_type = "rolling", skip_if_b4_lengths = T)
df2$ep4 <- episode_group(df2, date =x, case_length = ep, recurrence_length = rc, episode_type = "rolling", skip_if_b4_lengths = F)

test_that("test cut-off ranges", {
  expect_equal(df1$ep1@.Data, c(1,2,3,4,1))
  expect_equal(df1$ep1@wind_id[[1]], df1$ep1@.Data)
  expect_equal(decode(df1$ep1@case_nm), c("Case","Skipped","Skipped","Skipped","Duplicate_C"))
  expect_equal(decode(df1$ep1@wind_nm), c("Case","Skipped","Skipped","Skipped","Case"))

  expect_equal(df1$ep1@.Data, df1$ep2@.Data)
  expect_equal(df1$ep1@wind_id[[1]], df1$ep1@wind_id[[1]])
  expect_equal(decode(df1$ep2@case_nm), c(rep("Case",4),"Duplicate_C"))
  expect_equal(decode(df1$ep2@wind_nm), c(rep("Case",5)))

  expect_equal(df2$ep3@.Data, c(1,2,1,4,5,4,7))
  expect_equal(df2$ep3@.Data, df2$ep3@wind_id[[1]])
  expect_equal(decode(df2$ep3@wind_nm), c("Case","Skipped", "Case", "Case", "Skipped", "Case", "Case"))
  expect_equal(decode(df2$ep3@case_nm), c("Case","Skipped", "Duplicate_C", "Case", "Skipped", "Duplicate_C", "Case"))

  expect_equal(df2$ep4@.Data, c(1,2,1,2,5,6,5))
  expect_equal(df2$ep4@.Data, df2$ep4@wind_id[[1]])
  expect_equal(decode(df2$ep4@wind_nm), rep("Case",7))
  expect_equal(decode(df2$ep4@case_nm), c("Case","Case", "Duplicate_C","Duplicate_C", "Case", "Case", "Duplicate_C"))

})

ds <- data.frame(
  x =  seq(1,25,5),
  c1 =10,
  s = c(2,2,1,1,1)
)

ds$rx <- (max(ds$x)-ds$x)+1

ds$c2 <- number_line(7,10)
ds$c3 <- number_line(0,10)
ds$c4 <- 0
ds$ep_1 <- episode_group(ds, date =x, case_length = c1)
ds$ep_2 <- episode_group(ds, date =x, case_length = c1, custom_sort = s)
ds$ep_3 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, skip_if_b4_lengths = T)
ds$ep_4 <- episode_group(ds, date =x, case_length = c2, skip_if_b4_lengths = T)
ds$ep_5 <- episode_group(ds, date =x, case_length = c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_6 <- episode_group(ds, date =x, case_length = c4, recurrence_length = c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_7 <- episode_group(ds, date =x, case_length = c4, recurrence_length = c3, episode_type = "rolling")
ds$ep_8 <- episode_group(ds, date =x, case_length = c2, recurrence_length = c3, episode_type = "rolling", skip_if_b4_lengths = T)


ds$c5 <- number_line(-7,-10)
ds$c6 <- number_line(-10,-0)
ds$c7 <- number_line(-10, 10)
ds$ep_9 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, from_last = T)
ds$ep_10 <- episode_group(ds, date =rx, case_length = c2, custom_sort = s)
ds$ep_11 <- episode_group(ds, date =x, case_length = c5, custom_sort = s, from_last = T)
ds$ep_11.2 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, skip_if_b4_lengths = T)

ds$ep_12 <- episode_group(ds, date =x, case_length = c5, custom_sort = s, skip_if_b4_lengths = T)
ds$ep_13 <- episode_group(ds, date =x, case_length = c6, custom_sort = s)

ds$ep_14 <- episode_group(ds, date =x, case_length = c7, custom_sort = s)
ds$ep_15 <- episode_group(ds, date =x, case_length = c7, custom_sort = s, bi_direction = T)
ds$ep_16 <- episode_group(ds, date =x, case_length = c7, custom_sort = s, from_last = T)

test_that("test concepts in event groupping", {
  # Simple fixed episode grouping
  expect_equal(ds$ep_1@.Data, c(1,1,1,4,4))
  expect_equal(decode(ds$ep_1@case_nm), c("Case", "Duplicate_C","Duplicate_C", "Case","Duplicate_C"))
  # Fixed episode grouping with custom sort
  expect_equal(ds$ep_2@.Data, c(1,1,3,3,3))
  expect_equal(decode(ds$ep_2@case_nm), c("Case", "Duplicate_C","Case", "Duplicate_C","Duplicate_C"))
  # Fixed episode grouping ranges for length
  expect_equal(ds$ep_3@.Data, c(1,2,3,4,3))
  expect_equal(decode(ds$ep_3@case_nm), c("Case", "Case","Case", "Skipped","Duplicate_C"))
  expect_equal(ds$ep_4@.Data, c(1,2,1,4,5))
  expect_equal(decode(ds$ep_4@case_nm), c("Case", "Skipped","Duplicate_C","Case", "Case"))
  # Rolling episode grouping ranges for length
  expect_equal(ds$ep_5@.Data, c(1,2,1,4,1))
  expect_equal(decode(ds$ep_5@case_nm), c("Case", "Skipped","Duplicate_C","Skipped", "Recurrent"))
  expect_equal(ds$ep_6@.Data, c(1,2,1,4,1))
  expect_equal(decode(ds$ep_6@case_nm), c("Case", "Skipped","Recurrent","Skipped", "Recurrent"))
  expect_equal(ds$ep_7@.Data, c(1,1,1,1,1))
  expect_equal(decode(ds$ep_7@case_nm), c("Case", "Recurrent","Duplicate_R", "Recurrent", "Duplicate_R"))
  expect_equal(ds$ep_8@.Data, c(1,2,1,1,1))
  expect_equal(decode(ds$ep_8@case_nm), c("Case", "Skipped","Duplicate_C", "Recurrent", "Duplicate_R"))
  # from_last - use ep_10 approach later
  expect_equal(ds$ep_9@.Data, ds$ep_10@.Data)
  expect_equal(decode(ds$ep_9@case_nm), decode(ds$ep_10@case_nm))
  expect_equal(ds$ep_3, ds$ep_11.2)
  expect_equal(ds$ep_11@.Data, c(1,2,3,4,5))
  expect_equal(decode(ds$ep_11@case_nm), c("Case", "Case","Case","Case", "Case"))

  # Neg lengths
  expect_equal(ds$ep_12@.Data, c(3,2,3,4,5))
  expect_equal(decode(ds$ep_12@case_nm), c("Duplicate_C", "Skipped","Case", "Case", "Case"))
  expect_equal(ds$ep_13@.Data, c(3,3,3,4,5))
  expect_equal(decode(ds$ep_13@case_nm), c("Duplicate_C", "Duplicate_C","Case", "Case", "Case"))

  # Case level bi_direction
  expect_equal(ds$ep_15, ds$ep_14)
  expect_equal(ds$ep_16@.Data, c(2,2,5,5,5))
  expect_equal(decode(ds$ep_16@case_nm), c("Duplicate_C", "Case", "Duplicate_C","Duplicate_C", "Case"))
})


x =  seq(1,25,5)
y =  seq(2,22, 5)
nl = number_line(l=c(x,y),
                 r=c(x+3, y+1))

ds <- data.frame(
  c1 = 10,
  s = rep(c(2,2,1,1,1),2)
)
ds$x <- nl

ds <- ds[order(ds$x@start),]

ds$c2 <- number_line(7,10)
ds$c3 <- number_line(0,10)
ds$c4 <- 0
ds$ep_1 <- episode_group(ds, date =x, case_length = c1)
ds$ep_2 <- episode_group(ds, date =x, case_length = c1, custom_sort = s)
ds$ep_3 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, skip_if_b4_lengths = T)
ds$ep_3.5 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, include_index_period = F, skip_if_b4_lengths = T)

ds$ep_4 <- episode_group(ds, date =x, case_length = c2, skip_if_b4_lengths = T)
ds$ep_5 <- episode_group(ds, date =x, case_length = c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_5.5 <- episode_group(ds, date =x, case_length = c2, episode_type = "rolling", include_index_period = F, skip_if_b4_lengths = T)
ds$ep_6 <- episode_group(ds, date =x, case_length = c4, recurrence_length = c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_7 <- episode_group(ds, date =x, case_length = c4, recurrence_length = c3, episode_type = "rolling")
ds$ep_8 <- episode_group(ds, date =x, case_length = c2, recurrence_length = c3, episode_type = "rolling", skip_if_b4_lengths = T)

ds$c5 <- number_line(-7,-10)
ds$c6 <- number_line(-10,-0)
ds$c7 <- number_line(-10, 10)

ds$ep_9 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, from_last = T, skip_if_b4_lengths = T)
ds$ep_9.2 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, from_last = T, include_index_period = F, skip_if_b4_lengths = T)

ds$ep_11 <- episode_group(ds, date =x, case_length = c5, custom_sort = s, from_last = T)
ds$ep_11.2 <- episode_group(ds, date =x, case_length = c2, custom_sort = s, skip_if_b4_lengths = T)

ds$ep_12 <- episode_group(ds, date =x, case_length = c5, custom_sort = s)
ds$ep_13 <- episode_group(ds, date =x, case_length = c6, custom_sort = s)

ds$ep_14 <- episode_group(ds, date =x, case_length = c7, custom_sort = s)
ds$ep_15 <- episode_group(ds, date =x, case_length = c7, custom_sort = s, bi_direction = T)
ds$ep_16 <- episode_group(ds, date =x, case_length = c7, custom_sort = s, from_last = T)

test_that("test concepts in interval groupping", {
  # Simple fixed episode grouping
  expect_equal(ds$ep_1@.Data, c(rep(1,6), rep(7,4)))
  expect_equal(decode(ds$ep_1@case_nm), c("Case", rep("Duplicate_C",5), "Case", rep("Duplicate_C",3)))
  # Fixed episode grouping with custom sort
  expect_equal(ds$ep_2@.Data, c(rep(1,4), rep(5,6)))
  expect_equal(decode(ds$ep_2@case_nm), c("Case", rep("Duplicate_C",3), "Case", rep("Duplicate_C",5)))
  # Fixed episode grouping ranges for length
  # ep3 and ep3.5 include_inital_period
  expect_equal(ds$ep_3@.Data, c(1,1,3,3,5,5,7,8,5,5))
  expect_equal(decode(ds$ep_3@case_nm), c("Case","Duplicate_C","Case","Duplicate_C","Case","Duplicate_C", "Skipped","Skipped", "Duplicate_C", "Duplicate_C"))
  expect_equal(ds$ep_3.5@.Data, c(1,2,3,4,5,6,7,8,5,5))
  expect_equal(decode(ds$ep_3.5@case_nm), c("Case","Case","Case","Case","Case","Skipped", "Skipped","Skipped", "Duplicate_C", "Duplicate_C"))
  expect_equal(ds$ep_4@.Data, c(1,1,3,4,1,1,7,7,9,9))
  expect_equal(decode(ds$ep_4@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Duplicate_C","Duplicate_C", "Case","Duplicate_C", "Case", "Duplicate_C"))

  # Rolling episode grouping ranges for length
  expect_equal(ds$ep_5@.Data, c(1,1,3,4,1,1,7,8,1,1))
  expect_equal(decode(ds$ep_5@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Duplicate_C","Duplicate_C", "Skipped","Skipped", "Recurrent", "Duplicate_R"))
  expect_equal(ds$ep_5.5@.Data, c(1,2,3,4,1,1,7,8,1,1))
  expect_equal(decode(ds$ep_5.5@case_nm), c("Case","Skipped","Skipped","Skipped","Duplicate_C","Duplicate_C", "Skipped","Skipped", "Recurrent", "Duplicate_R"))
  expect_equal(ds$ep_6@.Data, c(1,1,3,4,1,1,7,8,1,1))
  expect_equal(decode(ds$ep_6@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Recurrent","Duplicate_R", "Skipped","Skipped", "Recurrent", "Duplicate_R"))
  expect_equal(ds$ep_7@.Data, rep(1,10))
  expect_equal(decode(ds$ep_7@case_nm), c("Case","Duplicate_C","Recurrent",rep("Duplicate_R", 3),"Recurrent",rep("Duplicate_R", 3)))
  expect_equal(ds$ep_8@.Data, c(1,1,3,4,rep(1,6)))
  expect_equal(decode(ds$ep_8@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Duplicate_C","Duplicate_C", "Recurrent","Duplicate_R", "Duplicate_R", "Duplicate_R"))

  # from_last
  expect_equal(ds$ep_9@.Data, c(4,2,4,4,10,10,10,8,10,10))
  expect_equal(decode(ds$ep_9@case_nm), c("Duplicate_C","Skipped","Duplicate_C","Case","Duplicate_C","Duplicate_C", "Duplicate_C","Skipped", "Duplicate_C", "Case"))
  expect_equal(ds$ep_9.2@.Data, c(4,2,3,4,10,10,10,8,9,10))
  expect_equal(decode(ds$ep_9.2@case_nm), c("Duplicate_C","Skipped","Skipped","Case","Duplicate_C","Duplicate_C", "Duplicate_C","Skipped", "Skipped", "Case"))

  expect_equal(ds$ep_11@.Data, c(2,2,4,4,6,6,8,8,10,10))
  expect_equal(decode(ds$ep_11@case_nm), c("Duplicate_C","Case","Duplicate_C","Case","Duplicate_C","Case", "Duplicate_C","Case", "Duplicate_C", "Case"))
  expect_equal(ds$ep_11.2@.Data, c(1,1,3,3,5,5,7,8,5,5))
  expect_equal(decode(ds$ep_11.2@case_nm), c("Case","Duplicate_C","Case","Duplicate_C", "Case", "Duplicate_C", "Skipped", "Skipped", "Duplicate_C", "Duplicate_C"))
  expect_equal(ds$ep_3, ds$ep_11.2)

  # Neg lengths
  expect_equal(ds$ep_12@.Data, c(5,2,5,5,5,5,7,7,9,9))
  expect_equal(decode(ds$ep_12@case_nm), c("Duplicate_C","Case","Duplicate_C","Duplicate_C", "Case", "Duplicate_C", "Case", "Duplicate_C", "Case", "Duplicate_C"))
  expect_equal(ds$ep_12@.Data, c(5,2,5,5,5,5,7,7,9,9))
  expect_equal(decode(ds$ep_12@case_nm), c("Duplicate_C","Case","Duplicate_C","Duplicate_C", "Case", "Duplicate_C", "Case", "Duplicate_C", "Case", "Duplicate_C"))
  expect_equal(ds$ep_12, ds$ep_13)

  # Case level bi_direction
  expect_equal(ds$ep_14@.Data, c(5,2,rep(5, 8)))
  expect_equal(decode(ds$ep_14@case_nm), c("Duplicate_C","Case","Duplicate_C","Duplicate_C", "Case", rep("Duplicate_C",5)))
  expect_equal(ds$ep_15, ds$ep_14)

  expect_equal(ds$ep_16@.Data, c(rep(rep(4,4)),rep(10, 6)))
  expect_equal(decode(ds$ep_16@case_nm), c(rep("Duplicate_C",3), "Case",rep("Duplicate_C",5), "Case"))
})
