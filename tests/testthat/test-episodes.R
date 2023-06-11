context("Unit tests - episodes()")

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
#data$date_int@id <- 1L

episodes <- function(..., to_s4 = T){
  if(to_s4 == F){
    as.data.frame(diyar::episodes(..., display = "none"), stringsAsFactors = FALSE)
  }else{
    x <- diyar::episodes(..., display = "none")
    x@options <- list()
    return(x)
  }
}

decode <- function(x) as.vector(diyar::decode(x))
# episode grouping with episode_group()
T1 <- rbind(head(data,10), head(data,10))
test_1 <- cbind(T1,
                episodes(strata = paste0(T1$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                         date = T1$date,
                         case_length = T1$episode_len,
                         episode_type = c(rep("fixed", 10), rep("rolling", 10)),
                         group_stats = T,
                         to_s4 = F))

l <- c(
  rep("01/04/2018 00:00:00", 3),
  rep("10/04/2018 00:00:00", 3),
  rep("19/04/2018 00:00:00", 3),
  rep("28/04/2018 00:00:00", 1),
  rep("01/04/2018 00:00:00", 10)
)

r <- c(
  rep("07/04/2018 00:00:00", 3),
  rep("16/04/2018 00:00:00", 3),
  rep("25/04/2018 00:00:00", 3),
  rep("28/04/2018 00:00:00", 1),
  rep("28/04/2018 00:00:00", 10)
)

e_int <- number_line(dttm(l), dttm(r))
e_case_nm <- c("Case", "Duplicate_C", "Duplicate_C", "Case", "Duplicate_C",
               "Duplicate_C", "Case", "Duplicate_C", "Duplicate_C", "Case",
               "Case", "Duplicate_C", "Duplicate_C", "Recurrent",  "Duplicate_R",
               "Recurrent", "Duplicate_R", "Recurrent", "Duplicate_R", "Recurrent")

test_that("test that test episode identifier is as expected for fixed episodes", {
  expect_equal(test_1$epid, c(1,1,1,4,4,4,7,7,7,10, rep(11, 10)))
  expect_equal(test_1$case_nm, e_case_nm)
  expect_equal(test_1$epid_start, left_point(e_int))
  expect_equal(test_1$epid_end, right_point(e_int))
  expect_equal(test_1$epid_total, c(rep(3,9), 1, rep(10,10)))
  expect_equal(test_1$epid_length, as.difftime(c(rep(6,9),0, rep(27,10)), units = "days"))
})

# Test 2 - Case assignment - Reverse chronological order
data_2 <- head(data, 10)
data_2$episode_len_s <- 13
data_2$d <- 13 * diyar::episode_unit$days

data_2 <- rbind(data_2, data_2)
test_2 <- cbind(data_2,
                episodes(strata = paste0(data_2$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                         date = data_2$date,
                         case_length = data_2$episode_len_s,
                         from_last = c(rep(F, 10), rep(T, 10)),
                         group_stats = T,
                         to_s4 = F))

l <- c(rep("01/04/2018 00:00:00", 5), rep("16/04/2018 00:00:00", 5))
r <- c(rep("13/04/2018 00:00:00", 5), rep("28/04/2018 00:00:00", 5))
e_int <- number_line(dttm(c(l, r)), dttm(c(r, l)))

test_that("test reverse episode grouping", {
  expect_equal(test_2$epid, c(rep(1,5),rep(6,5), rep(15,5),rep(20,5)))
  expect_equal(test_2$case_nm, c(rep(c("Case",rep("Duplicate_C",4)),2), rep(c(rep("Duplicate_C",4),"Case"),2)))
  expect_equal(test_2$epid_start, left_point(e_int))
  expect_equal(test_2$epid_end, right_point(e_int))
  expect_equal(test_2$epid_total, rep(5,20))
  expect_equal(test_2$epid_length, as.difftime(c(rep(12,10), rep(-12,10)), units = "days" ))
})

# Test 3 - Rolling episodes
test_3 <- cbind(data_2,
                episodes(
                  strata = paste0(data_2$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                  date = data_2$date,
                  case_length = data_2$episode_len_s,
                  episode_type ="rolling",
                  from_last = c(rep(F, 10), rep(T, 10)),
                  group_stats = T,
                  to_s4 = F))

l <- rep("01/04/2018 00:00:00", 10)
r <- rep("28/04/2018 00:00:00", 10)
e_int <- number_line(dttm(c(l,r)), dttm(c(r,l)))

e_case_nm <- c("Case",rep("Duplicate_C",4),"Recurrent",rep("Duplicate_R",3),"Recurrent")
e_case_nm <- c(e_case_nm, rev(e_case_nm))
test_that("test rolling/recurring episodes", {
  expect_equal(test_3$epid, c(rep(1,10), rep(20,10)))
  expect_equal(test_3$case_nm, e_case_nm)

  expect_equal(test_3$epid_start, left_point(e_int))
  expect_equal(test_3$epid_end, right_point(e_int))
  expect_equal(test_3$epid_total, rep(10,20))
  expect_equal(test_3$epid_length, as.difftime(c(rep(27,10), rep(-27,10)), units = "days" ))
})

# Test 3 - Rolls max
data_4 <- data_2
data_4$recurrence <- 3
data_4$r <- 3 * diyar::episode_unit$days

test_4 <- cbind(data_4,
                episodes(
                  strata = paste0(data_4$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                  date = data_4$date,
                  case_length = data_4$episode_len_s,
                  episode_type ="rolling",
                  rolls_max = c(rep(Inf, 10), rep(1, 10)),
                  to_s4 = F,
                  recurrence_length = data_4$recurrence,
                  group_stats = T))

l <- rep("01/04/2018 00:00:00", 10)
r <- rep("28/04/2018 00:00:00", 10)

l2 <- c(rep("01/04/2018 00:00:00", 6), rep("19/04/2018 00:00:00", 4))
r2 <- c(rep("16/04/2018 00:00:00", 6), rep("28/04/2018 00:00:00", 4))
e_int <- number_line(dttm(c(l, l2)), dttm(c(r,r2)))
e_case_nm <- c("Case",rep("Duplicate_C",4), rep("Recurrent",5),"Case",rep("Duplicate_C",4),"Recurrent","Case",rep("Duplicate_C",3))

test_that("test user defined recurrence length and roll_max", {
  expect_equal(test_4$epid, c(rep(1,10), rep(11,6), rep(17,4)))
  expect_equal(test_4$case_nm, e_case_nm)
  expect_equal(test_4$epid_start, left_point(e_int))
  expect_equal(test_4$epid_end, right_point(e_int))
  expect_equal(test_4$epid_total, c(rep(10,10), rep(6,6), rep(4,4)))
  expect_equal(test_4$epid_length, as.difftime(c(rep(27,10), rep(15,6), rep(9,4)), units = "days" ))
})

# Test 5 - Episodes max
test_5 <- cbind(data_4,
                episodes(
                  strata = paste0(data_4$pid, " ", c(rep("DS-A", 10), rep("DS-B", 10))),
                  date = data_4$date,
                  case_length = data_4$episode_len_s,
                  episode_type ="fixed",
                  recurrence_length = data_4$recurrence,
                  episodes_max = c(rep(1, 10), rep(2, 10)),
                  group_stats = T,
                  to_s4 = F))


l <- c(rep("01/04/2018 00:00:00", 5),"16/04/2018 00:00:00","19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
r <- c(rep("13/04/2018 00:00:00", 5),"16/04/2018 00:00:00","19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")

l2 <- c(rep("01/04/2018 00:00:00", 5), rep("16/04/2018 00:00:00", 5))
r2 <- c(rep("13/04/2018 00:00:00", 5), rep("28/04/2018 00:00:00", 5))
e_int <- number_line(dttm(c(l,l2)), dttm(c(r,r2)))
e_case_nm <- c("Case",rep("Duplicate_C",4),rep("Skipped",5), rep(c("Case",rep("Duplicate_C",4)),2))
test_that("testing user defined episodes_max", {
  expect_equal(test_5$epid, c(rep(1,5), 6:10, rep(11,5), rep(16,5)))
  expect_equal(test_5$case_nm, e_case_nm)
  expect_equal(test_5$epid_start, left_point(e_int))
  expect_equal(test_5$epid_end, right_point(e_int))
  expect_equal(test_5$epid_total, c(rep(c(rep(5,5), rep(1,5))), rep(5,10)))
  expect_equal(test_5$epid_length, as.difftime(c(rep(12,5), rep(0,5), rep(12,10)), units = "days" ))
})

# Test 6 - Combining rolls_max and episodes_max
test_6 <- rbind(data_4, head(data_4, 10))
test_6$strata <- paste0(test_6$pid, " DS-", sort(rep(c("A","B","C"), 10)))
test_6 <- cbind(test_6,
                episodes(strata = test_6$strata,
                         date = test_6$date,
                         case_length = test_6$episode_len_s,
                         episode_type ="rolling",
                         recurrence_length = test_6$recurrence,
                         episodes_max = c(rep(1, 10), rep(2, 20)),
                         rolls_max = c(rep(1, 20), rep(3, 10)),
                         group_stats = T,
                         to_s4 = F))

l <- c(rep("01/04/2018 00:00:00", 6),"19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")
r <- c(rep("16/04/2018 00:00:00", 6),"19/04/2018 00:00:00","22/04/2018 00:00:00","25/04/2018 00:00:00","28/04/2018 00:00:00")

l2 <- c(rep("01/04/2018 00:00:00", 6), rep("19/04/2018 00:00:00", 4))
r2 <- c(rep("16/04/2018 00:00:00", 6), rep("28/04/2018 00:00:00", 4))

l3 <- c(rep("01/04/2018 00:00:00", 8), rep("25/04/2018 00:00:00", 2))
r3 <- c(rep("22/04/2018 00:00:00", 8), rep("28/04/2018 00:00:00", 2))
e_int <- number_line(dttm(c(l,l2,l3)), dttm(c(r,r2,r3)))
e_case_nm <- c("Case",rep("Duplicate_C",4),"Recurrent",rep("Skipped",4), "Case",
               "Duplicate_C", "Duplicate_C", "Duplicate_C", "Duplicate_C",
               "Recurrent", "Case", "Duplicate_C", "Duplicate_C", "Duplicate_C",
               "Case", "Duplicate_C", "Duplicate_C", "Duplicate_C", "Duplicate_C",
               "Recurrent", "Recurrent", "Recurrent", "Case", "Duplicate_C")
test_that("testing episodes_max and rolls_max combinations", {
  expect_equal(test_6$epid, c(rep(1,6), 7:10, rep(11,6), rep(17,4), rep(21,8), rep(29,2)))
  expect_equal(test_6$case_nm, e_case_nm)
  expect_equal(test_6$epid_start, left_point(e_int))
  expect_equal(test_6$epid_end, right_point(e_int))
  expect_equal(test_6$epid_total, c(rep(c(rep(6,6), rep(1,4))), rep(c(rep(6,6), rep(4,4))), rep(c(rep(8,8), rep(2,2)))))
  expect_equal(test_6$epid_length, as.difftime(c(rep(15,6), rep(0,4), rep(15,6), rep(9,4), rep(21,8), rep(3,2)), units = "days" ))
})

# Test 7 - Deterministic linkage
data_7 <- data_4
data_7$recurrence <- 2
ds <- paste("DS",c(1:3, rep(c(1:2),2), rep(3,3)), sep="")
data_7$dataset <- c(ds, paste(ds, 13, sep = "-"))

test_7 <- cbind(data_7,
                episodes(
                  strata = paste0(data_7$pid, " DS-", sort(rep(c("A","B"), 10))),
                  date = data_7$date,
                  case_length = data_7$episode_len,
                  to_s4 = F,
                  episode_type ="rolling",
                  recurrence_length = data_7$recurrence,
                  data_source = data_7$dataset, group_stats = T))

l <- c(rep("01/04/2018 00:00:00", 3), rep("10/04/2018 00:00:00", 3), rep("19/04/2018 00:00:00", 3), "28/04/2018 00:00:00")
r <- c(rep("07/04/2018 00:00:00", 3), rep("16/04/2018 00:00:00", 3), rep("25/04/2018 00:00:00", 3), "28/04/2018 00:00:00")
e_int <- number_line(dttm(rep(l,2)), dttm(rep(r,2)))
e_case_nm <- c("Case", "Duplicate_C", "Duplicate_C","Case","Duplicate_C",
               "Duplicate_C", "Case", "Duplicate_C", "Duplicate_C", "Case")
e_ds <- c(rep("DS1,DS2,DS3",3),rep("DS1,DS2",3),rep("DS2,DS3",3),"DS3",rep("DS1-13,DS2-13,DS3-13",3),rep("DS1-13,DS2-13",3),rep("DS2-13,DS3-13",3),"DS3-13")
test_that("testing epid_dataset", {
  expect_equal(test_7$epid, c(rep(1,3),rep(4,3),rep(7,3), 10, rep(11,3),rep(14,3),rep(17,3), 20))
  expect_equal(test_7$case_nm, rep(e_case_nm, 2))
  expect_equal(test_7$epid_dataset, e_ds)
  expect_equal(test_7$epid_start, left_point(e_int))
  expect_equal(test_7$epid_end, right_point(e_int))
  expect_equal(test_7$epid_total, c(rep(3,9),1,rep(3,9),1))
  expect_equal(test_7$epid_length, as.difftime(c(rep(6,9),0,rep(6,9),0), units = "days" ))
})

hospital_infections <- diyar::infections
# Test 8 - Episode unit
# 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
test_8a <- cbind(hospital_infections,
                 episodes(sn= hospital_infections$rd_id, date = hospital_infections$date, case_length = hospital_infections$epi_len,
                          from_last = T, episode_unit = "hours", group_stats = T, to_s4 = F))

e_int <- number_line(dttm(format(test_8a$date, "%d/%m/%Y 00:00:00")), dttm(format(test_8a$date, "%d/%m/%Y 00:00:00")))

test_that("testing; episode grouping by the hour", {
  expect_equal(test_8a$epid, 1L:11L)
  expect_equal(test_8a$case_nm, c( "Case", rep("Case",10)))

  e_int@gid <- e_int@id <- 1L:11L

  expect_equal(test_8a$epid_start, left_point(e_int))
  expect_equal(test_8a$epid_end, right_point(e_int))
  expect_equal(test_8a$epid_total, rep(1,11))
  expect_equal(test_8a$epid_length, as.difftime(rep(0,11), units = "hours" ))
})

# 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
test_8b <- cbind(hospital_infections,
                 episodes(sn= hospital_infections$rd_id, date = hospital_infections$date, case_length = hospital_infections$epi_len,
                          from_last = T, episode_unit = "weeks",  group_stats = T, to_s4 = F))

l <- rep("31/05/2018 00:00:00", 11)
r <- rep("01/04/2018 00:00:00", 11)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; episode grouping by weeks", {
  expect_equal(test_8b$epid, rep(11L, 11))

  e_int@id <- 1L:11L
  e_int@gid <- rep(11L, 11)

  expect_equal(test_8b$case_nm, c(rep("Duplicate_C",10),"Case"))
  expect_equal(test_8b$epid_start, left_point(e_int))
  expect_equal(test_8b$epid_end, right_point(e_int))
  expect_equal(test_8b$epid_total, rep(11L, 11))
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
                 episodes(sn = hospital_infections$rd_id, date = hospital_infections$date, case_length = hospital_infections$epi_len,
                          custom_sort = hospital_infections$infection, group_stats = T, to_s4 = F))

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
                 suffix(episodes(sn = hospital_infections$rd_id, date = hospital_infections$date,
                                 case_length = list(hospital_infections$epi_len, -hospital_infections$epi_len),
                                 custom_sort = hospital_infections$infection_ord, from_last = T, group_stats = T, to_s4 = F), 1),

                 suffix(episodes(sn = hospital_infections$rd_id, date = hospital_infections$date,
                                 case_length = hospital_infections$epi_len,
                                 custom_sort = hospital_infections$infection_ord, from_last = T, group_stats = T, to_s4 = F), 2))

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
  expect_equal(test_9b$epid_total.1, rep(11L, 11))
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
test_10a <- cbind(hospital_infections,
                  episodes(sn = hospital_infections$rd_id, date = hospital_infections$date, strata = hospital_infections$patient_id,
                           case_length = hospital_infections$epi_len, episodes_max = 1, from_last = F,
                           data_source = as.character(hospital_infections$infection), group_stats = T, to_s4 = F))

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
                    episodes(sn = hospital_infections$rd_id, date = hospital_infections$date, strata = hospital_infections$patient_id,
                             case_length = hospital_infections$epi_len, episode_type="rolling", to_s4 = F,
                             data_source = as.character(hospital_infections$infection), group_stats = T))

# Only three 9-day (difference of 8 days) rolling episode per patient and infection.
hospital_infections$epi_len <- 8
hospital_infections$recur <- 30
hospital_infections$strata <- paste(hospital_infections$patient_id, hospital_infections$infection, sep="-")
hospital_infections$data_source <- paste(hospital_infections$patient_id, hospital_infections$infection, sep = "-")

#hospital_infections <- filter(hospital_infections, strata == "PID 1-UTI")

test_10b <- cbind(hospital_infections,
                  episodes(sn = hospital_infections$rd_id, date = hospital_infections$date,
                           strata = hospital_infections$strata,
                           case_length = hospital_infections$epi_len, episode_type = "rolling",
                           recurrence_length = hospital_infections$recur, episodes_max = 3,
                           data_source = hospital_infections$data_source,
                           group_stats = T, to_s4 = F))

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

# episodes of overlapping intervals of admission
test_11a <- cbind(
  admissions,
  episodes(date = admissions$admin_period, sn=admissions$rd_id,
           case_length = list(number_line(-admissions$admin_period@.Data, 0), admissions$epi_len),
           group_stats = T, to_s4 = F))

l <- c(rep("01/01/2019 00:00:00", 7), rep("20/01/2019 00:00:00", 2))
r <- c(rep("15/01/2019 00:00:00", 7), rep("31/01/2019 00:00:00", 2))
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping", {
  expect_equal(test_11a$epid, c(rep(2,7), rep(8,2)))
  expect_equal(test_11a$case_nm, c("Duplicate_C","Case", rep("Duplicate_C",5),
                                   "Case", "Duplicate_C"))
  e_int@id <- 1L:9L
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
  episodes(date = admissions$admin_period, sn = admissions$rd_id,
           case_length = list(number_line(-admissions$admin_period@.Data, 0), admissions$epi_len), to_s4 = F,
           episode_type = "rolling",
           recurrence_length = list(number_line(-admissions$admin_period@.Data, 0), admissions$recur), episode_unit = "months", group_stats = T))

l <- rep("01/01/2019 00:00:00", 9)
r <- rep("31/01/2019 00:00:00", 9)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping for rolling intervals", {
  expect_equal(test_11b$epid, rep(2L, 9))
  expect_equal(test_11b$case_nm, c("Duplicate_C","Case",rep("Duplicate_C",5),
                                   "Recurrent", "Duplicate_R"))
  e_int@id <- 1L:9L
  e_int@gid <- rep(2L, 9)

  expect_equal(test_11b$epid_start, left_point(e_int))
  expect_equal(test_11b$epid_end, right_point(e_int))
  expect_equal(test_11b$epid_total, rep(9,9))
  expect_equal(test_11b$epid_length, as.difftime(rep(30,9), units = "days" ))
})

# fixed episodes of overlapping intervals of admission seperated by 1 month
admissions$epi_len <- 1
test_11c <- cbind(admissions,
                  episodes(date = admissions$admin_period, sn = admissions$rd_id,
                           case_length = list(number_line(-admissions$admin_period@.Data, 0), admissions$epi_len),
                           episode_unit = "months", group_stats = T, to_s4 = F))

l <- rep("01/01/2019 00:00:00", 9)
r <- rep("31/01/2019 00:00:00", 9)
e_int <- number_line(dttm(l), dttm(r))

test_that("testing; intervals grouping with a case length", {
  expect_equal(test_11c$epid, rep(2L, 9))
  expect_equal(test_11c$case_nm, c("Duplicate_C","Case",rep("Duplicate_C",7)))
  e_int@id <- 1L:9L
  e_int@gid <- rep(2L, 9)
  expect_equal(test_11b$epid_start, left_point(e_int))
  expect_equal(test_11b$epid_end, right_point(e_int))
  expect_equal(test_11c$epid_total, rep(9,9))
  expect_equal(test_11c$epid_length, as.difftime(rep(30,9), units = "days" ))
})

infections <- diyar::infections
test_that("test that fixed_episodes() with numeric 'date' works the same as compress_number_line()", {
  a <- episodes(date = c(1,1,4,4,1,4,3,2), case_length = 0, to_s4 = T, group_stats = T)
  b <- compress_number_line(x = as.number_line(c(1,1,4,4,1,4,3,2)), collapse =T, deduplicate = F)
  expect_equal(a@epid_interval, b)
})

test_that("test some generic functions", {
  expect_equal(show(new("epid")), "epid(0)")
  b <- rep(as.epid(5L), 2)
  b@epid_interval@gid <- b@epid_interval@id <- 1:length(b)
  # temp
  #expect_equal(c(as.epid(5L), as.epid(5L)), b)
})

x1 <- c("01/04/2019", "04/04/2019", "14/04/2019", "16/04/2019", "19/04/2019")
x2 <- c("01/04/2019", "04/04/2019", "8/04/2019", "14/04/2019", "16/04/2019", "19/04/2019")
x3 <- c("01/04/2019", "04/04/2019", "12/04/2019",  "14/04/2019", "16/04/2019", "19/04/2019")
x4 <- c("01/01/2007", "07/01/2007", "09/01/2007", "19/01/2007")
x5 <- c("01/04/2019", "07/04/2019", "12/04/2019","21/04/2019","26/04/2019","29/04/2019")

x1 <- rep(x1, 2); x2 <- rep(x2, 2); x3 <- rep(x3, 2); x4 <- rep(x4, 2); x5 <- rep(x5, 3)
c <- c(rep(5, length(x1) + length(x2) + length(x3) + length(x4) + length(x5)) )
r <- c(rep(10, length(x1) + length(x2) + length(x3) + length(x4)), rep(20, length(x5)) )

cr <- c(rep(T, length(x1)/2),
        rep(F, length(x1)/2),
        rep(T, length(x2)/2),
        rep(F, length(x2)/2),
        rep(F, length(x3) + length(x4) + (length(x5)/3*2) ),
        rep(T, length(x5)/3))

rl <- c(rep("last_record", length(x1) + length(x2)),
        rep("last_record", length(x3)/2),
        rep("first_record", length(x3)/2),
        rep("last_record", length(x4)/2),
        rep("first_record", length(x4)/2),
        rep("last_record", length(x5)/3),
        rep("first_record", length(x5)/3*2))

rm <- c(rep(1, length(x1) + length(x2)),
        rep(2, length(x3) + length(x4) + length(x5)))

strt <- c(rep("DS-A", length(x1)/2),
          rep("DS-B", length(x1)/2),
          rep("DS-C", length(x2)/2),
          rep("DS-D", length(x2)/2),
          rep("DS-E", length(x3)/2),
          rep("DS-F", length(x3)/2),
          rep("DS-G", length(x4)/2),
          rep("DS-H", length(x4)/2),
          rep("DS-I", length(x5)/3),
          rep("DS-J", length(x5)/3),
          rep("DS-K", length(x5)/3)
)

epids <- episodes(date = date(c(x1, x2, x3, x4, x5)),
                  strata = strt,
                  case_length = c,
                  recurrence_length = r,
                  to_s4=T,
                  reference_event = rl,
                  case_for_recurrence = cr,
                  rolls_max = rm,
                  episode_type = "rolling")

e_case_nm <- c("Case", "Duplicate_C", "Recurrent", "Recurrent", "Duplicate_C",
               "Case", "Duplicate_C", "Recurrent", "Case", "Duplicate_C", "Case",
               "Duplicate_C", "Recurrent", "Duplicate_R", "Recurrent", "Duplicate_C",
               "Case", "Duplicate_C", "Recurrent", "Duplicate_R", "Case", "Duplicate_C",
               "Case", "Duplicate_C", "Recurrent", "Duplicate_R", "Recurrent",
               "Duplicate_R", "Case", "Duplicate_C", "Case", "Duplicate_C",
               "Duplicate_C", "Recurrent", "Case", "Recurrent", "Duplicate_R",
               "Recurrent", "Case", "Recurrent", "Duplicate_R", "Case", "Case",
               "Recurrent", "Duplicate_R", "Duplicate_R", "Recurrent", "Duplicate_R",
               "Case", "Recurrent", "Duplicate_R", "Duplicate_R", "Recurrent",
               "Case", "Case", "Recurrent", "Duplicate_R", "Duplicate_R", "Case",
               "Duplicate_C")

e_eid <- c(1, 1, 1, 1, 1, 6, 6, 6, 9, 9,
           11, 11, 11, 11, 11, 11, 17, 17, 17, 17,
           21, 21, 23, 23, 23, 23, 23, 23,
           29, 29, 31, 31, 31, 31, 35, 35, 35, 35,
           39, 39, 39, 42, 43, 43, 43, 43, 43, 43,
           49, 49, 49, 49, 49, 54, 55, 55, 55, 55, 59, 59)

test_that("test 'case_for_recurrence' in rolling_episodes", {
  expect_equal(epids@.Data, e_eid)
  expect_equal(decode(epids@case_nm), e_case_nm)
})


x <- date(c("01/01/2007","04/01/2007","12/01/2007","15/01/2007","22/01/2007"))
df <- data.frame(x=x, c=5, r=10)
epids_r <- episodes(date = df$x, case_length = df$c,  recurrence_length = df$r, to_s4 = T, episode_type = "rolling")

x <- c(date("01/01/2007"), date("10/01/2007"), date("12/01/2007"), date("15/01/2007"), date("22/01/2007"))
df <- data.frame(x=x, c=5, r=10)
epids2_r <- episodes(date = df$x, case_length = df$c,  recurrence_length = df$r, to_s4 = T, episode_type = "rolling")

test_that("test rolling_episodes", {
  expect_equal(epids_r@.Data, rep(1,5))
  expect_equal(decode(epids_r@case_nm), c("Case","Duplicate_C","Recurrent","Recurrent","Duplicate_R"))
  expect_equal(epids2_r@.Data, rep(1,5))
  expect_equal(decode(epids2_r@case_nm), c("Case","Recurrent","Recurrent","Duplicate_R","Recurrent"))
})


d <- seq.Date(date("01/04/2019"), date("18/04/2019"), "3 day" )
df<- data.frame(date=d)
df$r1 <- episodes(d, case_length = 3, case_for_recurrence = F, to_s4 = T, episode_type = "rolling")
df$r2 <- episodes(d, case_length = 3, case_for_recurrence = T, to_s4 = T, episode_type = "rolling")

test_that("test case_for_recurrence", {
  expect_equal(df$r1@.Data, rep(1,6))
  expect_equal(decode(df$r1@case_nm), c("Case", "Duplicate_C", rep("Recurrent", 4)))
  r1.list <- lapply(as.list(df$r1), decode)
  r1.list <- r1.list[!grepl("wind_nm", names(r1.list))]
  r2.list <- lapply(as.list(df$r2), decode)
  r2.list <- r2.list[!grepl("wind_nm", names(r2.list))]
  expect_equal(r2.list, r2.list)
  expect_equal(decode(df$r1@wind_nm$wind_nm1), c(rep("Case", 2), rep("Recurrence", 4)))
  expect_equal(decode(df$r2@wind_nm$wind_nm1), c(rep("Case", 2), "Recurrence", "Case_for_recurrence", "Recurrence", "Case_for_recurrence"))
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
df$ep1 <- episodes(date = df$dt, case_length = df$c1)
df$ep2 <- episodes(date = df$dt, case_length = list(df$c1, invert_number_line(df$c1)))
# Test - Expect TRUE
all(df$ep1==df$ep2)

df$ep3 <- episodes(date = df$dt, case_length = list(df$c2, index_window(df$dt)))
df$ep4 <- episodes(date = df$dt, case_length = list(df$c2, invert_number_line(df$c2), index_window(df$dt)))
# Test - Expect TRUE
all(df$ep3==df$ep4)

df$ep5 <- episodes(date = df$dt, case_length = df$c3)
df$ep6 <- episodes(date = df$dt, case_length = list(df$c3, invert_number_line(df$c3), index_window(df$dt)))
# Test - Expect TRUE
all(df$ep3==df$ep6)

df$ep7 <- episodes(date = df$dt, case_length = df$c3, custom_sort = df$c_sort)

df$ep8 <- episodes(date = df$dt, case_length = list(df$c1, invert_number_line(df$c1)), custom_sort = df$c_sort)
df$ep9 <- episodes(date = df$dt, case_length = list(df$c5, invert_number_line(df$c5)), custom_sort = df$c_sort)
# Test - Expect TRUE
all(df$ep8 == df$ep9)

#test that these two should give the same result
df$ep10 <- episodes(date = df$dt, case_length = list(df$c1, invert_number_line(df$c1)), custom_sort = df$c_sort, from_last = T)
df$ep11 <- episodes(date = df$dt, case_length = list(df$c5, invert_number_line(df$c5)), custom_sort = df$c_sort, from_last = T)

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
df$ep1 <- episodes(date = df$x, case_length = df$ep, recurrence_length = df$rc, case_for_recurrence = T, episode_type = "rolling", rolls_max = 1)
df$ep2 <- episodes(date = df$x, case_length = df$ep, recurrence_length = df$rc, case_for_recurrence = F, episode_type = "rolling", rolls_max = 1)

df$ep3 <- episodes(date = df$x, case_length = list(df$ep, 0), recurrence_length = list(df$rc, 0), case_for_recurrence = T, reference_event = "first_record", episode_type = "rolling", rolls_max = 1)
df$ep4 <- episodes(date = df$x, case_length = df$ep, recurrence_length = df$rc, case_for_recurrence = F, reference_event = "first_record", episode_type = "rolling", rolls_max = 1)

df$ep5 <- episodes(date = df$x, case_length = list(df$ep, 0), recurrence_length = list(df$rc, 0), case_for_recurrence = T, episode_type = "rolling", rolls_max = 2)
df$ep6 <- episodes(date = df$x, case_length = df$ep, recurrence_length = df$rc, case_for_recurrence = F, episode_type = "rolling", rolls_max = 2)

test_that("test wind_id and wind_nm", {
  expect_equal(df$ep1@.Data, rep(1, 5))
  expect_equal(decode(df$ep1@case_nm), c("Case","Recurrent", rep("Duplicate_R", 2), "Recurrent"))
  expect_equal(decode(df$ep1@wind_nm$wind_nm1), c("Case", rep("Recurrence", 3), "Case_for_recurrence"))
  expect_equal(df$ep1@wind_id[[1]], c(rep(1,4),4))
  expect_equal(df$ep2, df$ep4)
  expect_equal(decode(df$ep1@case_nm), decode(df$ep3@case_nm))
  expect_equal(decode(df$ep1@wind_nm$wind_nm1), decode(df$ep3@wind_nm$wind_nm1))
  expect_equal(df$ep1@.Data, df$ep3@.Data)
  expect_equal(df$ep3@wind_id[[1]], c(rep(1,4),2))
  expect_equal(df$ep1, df$ep5)
  expect_equal(df$ep1@.Data, df$ep6@.Data)
  expect_equal(decode(df$ep6@case_nm), c("Case","Recurrent", rep("Duplicate_R", 2), "Recurrent"))
  expect_equal(decode(df$ep6@wind_nm$wind_nm1), c("Case", rep("Recurrence", 4)))
})

x <- c(1,6,7,8,10)
df1 <- data.frame(x=x, rc=0)
df1$cl <- number_line(9,10)
df1$ep1 <- episodes(date = df1$x, case_length = df1$cl, recurrence_length = df1$rc, episode_type = "rolling", skip_if_b4_lengths = T)
df1$ep2 <- episodes(date = df1$x, case_length = df1$cl, recurrence_length = df1$rc, episode_type = "rolling", skip_if_b4_lengths = F)

x <- seq(1,20,3)
df2 <- data.frame(x=x, rc=0)
df2$ep <- number_line(6,6)
df2$ep3 <- episodes(date = df2$x, case_length = df2$ep, recurrence_length = df2$rc, episode_type = "rolling", skip_if_b4_lengths = T)
df2$ep4 <- episodes(date = df2$x, case_length = df2$ep, recurrence_length = df2$rc, episode_type = "rolling", skip_if_b4_lengths = F)

test_that("test cut-off ranges", {
  expect_equal(df1$ep1@.Data, c(1,2,3,4,1))
  expect_equal(df1$ep1@wind_id[[1]], df1$ep1@.Data)
  expect_equal(decode(df1$ep1@case_nm), c("Case","Skipped","Skipped","Skipped","Duplicate_C"))
  expect_equal(decode(df1$ep1@wind_nm$wind_nm1), c("Case","Skipped","Skipped","Skipped","Case"))

  expect_equal(df1$ep1@.Data, df1$ep2@.Data)
  expect_equal(df1$ep1@wind_id[[1]], df1$ep1@wind_id[[1]])
  expect_equal(decode(df1$ep2@case_nm), c(rep("Case",3), "Case","Duplicate_C"))
  expect_equal(decode(df1$ep2@wind_nm$wind_nm1), c(rep("Case",3), "Case", "Case"))

  expect_equal(df2$ep3@.Data, c(1,2,1,4,5,4,7))
  expect_equal(df2$ep3@.Data, c(1,2,1,4,5,4,7))
  expect_equal(decode(df2$ep3@wind_nm$wind_nm1), c("Case","Skipped", "Case", "Case", "Skipped", "Case", "Case"))
  expect_equal(decode(df2$ep3@case_nm), c("Case","Skipped", "Duplicate_C", "Case", "Skipped", "Duplicate_C", "Case"))

  expect_equal(df2$ep4@.Data, c(1,2,1,2,5,6,5))
  expect_equal(df2$ep4@.Data, df2$ep4@wind_id[[1]])
  expect_equal(decode(df2$ep4@wind_nm$wind_nm1), c(rep("Case",5), "Case", "Case"))
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
ds$ep_1 <- episodes(date = ds$x, case_length = ds$c1, skip_if_b4_lengths = T)
ds$ep_2 <- episodes(date = ds$x, case_length = ds$c1, custom_sort = ds$s)
ds$ep_3 <- episodes(date = ds$x, case_length = ds$c2, custom_sort = ds$s, skip_if_b4_lengths = T)
ds$ep_4 <- episodes(date = ds$x, case_length = ds$c2, skip_if_b4_lengths = T)
ds$ep_5 <- episodes(date = ds$x, case_length = ds$c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_6 <- episodes(date = ds$x, case_length = ds$c4, recurrence_length = ds$c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_7 <- episodes(date = ds$x, case_length = ds$c4, recurrence_length = ds$c3, episode_type = "rolling")
ds$ep_8 <- episodes(date = ds$x, case_length = ds$c2, recurrence_length = ds$c3, episode_type = "rolling", skip_if_b4_lengths = T)


ds$c5 <- number_line(-7,-10)
ds$c6 <- number_line(-10,-0)
ds$c7 <- number_line(-10, 10)
ds$ep_9 <- episodes(date = ds$x, case_length = ds$c2, custom_sort = ds$s, from_last = T)
ds$ep_10 <- episodes(date = ds$rx, case_length = ds$c2, custom_sort = ds$s)
ds$ep_11 <- episodes(date = ds$x, case_length = ds$c5, custom_sort = ds$s, from_last = T)
ds$ep_11.2 <- episodes(date = ds$x, case_length = ds$c2, custom_sort = ds$s, skip_if_b4_lengths = T)

ds$ep_12 <- episodes(date = ds$x, case_length = list(ds$c5, index_window(ds$x)), custom_sort = ds$s, skip_if_b4_lengths = T)
ds$ep_13 <- episodes(date = ds$x, case_length = ds$c6, custom_sort = ds$s)

ds$ep_14 <- episodes(date = ds$x, case_length = ds$c7, custom_sort = ds$s)
ds$ep_15 <- episodes(date = ds$x, case_length = list(ds$c7, invert_number_line(ds$c7)), custom_sort = ds$s)
ds$ep_16 <- episodes(date = ds$x, case_length = ds$c7, custom_sort = ds$s, from_last = T)

test_that("test concepts in event grouping", {
  # Simple fixed episode grouping
  expect_equal(ds$ep_1@.Data, c(1,1,1,4,4))
  expect_equal(decode(ds$ep_1@case_nm), c("Case", "Duplicate_C","Duplicate_C", "Case","Duplicate_C"))
  # Fixed episode grouping with custom sort
  expect_equal(ds$ep_2@.Data, c(1,1,3,3,3))
  expect_equal(decode(ds$ep_2@case_nm), c("Case", "Duplicate_C","Case", "Duplicate_C","Duplicate_C"))
  # Fixed episode grouping ranges for length
  expect_equal(ds$ep_3@.Data, c(1,2,3,4,3))
  expect_equal(decode(ds$ep_3@case_nm), c("Case", "Skipped","Case", "Skipped","Duplicate_C"))
  expect_equal(ds$ep_4@.Data, c(1,2,1,4,5))
  expect_equal(decode(ds$ep_4@case_nm), c("Case", "Skipped","Duplicate_C","Case", "Skipped"))
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
ds$ep_1 <- episodes(date = ds$x, case_length = list(ds$c1, index_window(ds$x)))
ds$ep_2 <- episodes(date = ds$x, case_length = list(ds$c1, index_window(ds$x)), custom_sort = ds$s)
ds$ep_3 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x)), custom_sort = ds$s, skip_if_b4_lengths = T)
ds$ep_3.5 <- episodes(date = ds$x, case_length = ds$c2, custom_sort = ds$s, skip_if_b4_lengths = T)

ds$ep_4 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x)), skip_if_b4_lengths = T)
ds$ep_5 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x)), episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_5.5 <- episodes(date = ds$x, case_length = ds$c2, episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_6 <- episodes(date =ds$x, case_length = list(ds$c4, index_window(ds$x)), recurrence_length = list(ds$c2, index_window(ds$x)), episode_type = "rolling", skip_if_b4_lengths = T)
ds$ep_7 <- episodes(date =ds$x, case_length = list(ds$c4, index_window(ds$x)), recurrence_length = list(ds$c3, index_window(ds$x)), episode_type = "rolling")
ds$ep_8 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x)), recurrence_length = list(ds$c3, index_window(ds$x)), episode_type = "rolling", skip_if_b4_lengths = T)

ds$c5 <- number_line(-7,-10)
ds$c6 <- number_line(-10,-0)
ds$c7 <- number_line(-10, 10)

ds$ep_9 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x, from_last = T)), custom_sort = ds$s, from_last = T, skip_if_b4_lengths = T)
ds$ep_9.2 <- episodes(date = ds$x, case_length = ds$c2, custom_sort = ds$s, from_last = T, skip_if_b4_lengths = T)

ds$ep_11 <- episodes(date = ds$x, case_length = list(ds$c5, index_window(ds$x)), custom_sort = ds$s, from_last = T)
ds$ep_11.2 <- episodes(date = ds$x, case_length = list(ds$c2, index_window(ds$x)), custom_sort = ds$s, skip_if_b4_lengths = T)

ds$ep_12 <- episodes(date = ds$x, case_length = list(ds$c5, index_window(ds$x)), custom_sort = ds$s)
ds$ep_13 <- episodes(date = ds$x, case_length = list(ds$c6, index_window(ds$x)), custom_sort = ds$s)

ds$ep_14 <- episodes(date = ds$x, case_length = ds$c7, custom_sort = ds$s)
ds$ep_15 <- episodes(date = ds$x, case_length = list(ds$c7, invert_number_line(ds$c7)) , custom_sort = ds$s)
ds$ep_16 <- episodes(date = ds$x, case_length = ds$c7, custom_sort = ds$s, from_last = T)

test_that("test concepts in interval grouping", {
  # Simple fixed episode grouping
  expect_equal(ds$ep_1@.Data, c(rep(1,6), rep(7,4)))
  expect_equal(decode(ds$ep_1@case_nm), c("Case", rep("Duplicate_C",5), "Case", rep("Duplicate_C",3)))
  # Fixed episode grouping with custom sort
  expect_equal(ds$ep_2@.Data, c(rep(1,4), rep(5,6)))
  expect_equal(decode(ds$ep_2@case_nm), c("Case", rep("Duplicate_C",3), "Case", rep("Duplicate_C",5)))
  # Fixed episode grouping ranges for length
  # ep3 and ep3.5 include_inital_period
  expect_equal(ds$ep_3@.Data, c(1,1,3,4,5,5,7,8,5,5))
  expect_equal(decode(ds$ep_3@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Case","Duplicate_C", "Skipped","Skipped", "Duplicate_C", "Duplicate_C"))
  expect_equal(ds$ep_3.5@.Data, c(1,2,3,4,5,6,7,8,5,5))
  expect_equal(decode(ds$ep_3.5@case_nm), c("Case","Case","Skipped","Skipped","Case","Case", "Skipped","Skipped", "Duplicate_C", "Duplicate_C"))
  expect_equal(ds$ep_4@.Data, c(1,1,3,4,1,1,7,7,9,10))
  expect_equal(decode(ds$ep_4@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Duplicate_C","Duplicate_C", "Case","Duplicate_C", "Skipped", "Skipped"))

  # Rolling episode grouping ranges for length
  expect_equal(ds$ep_5@.Data, c(1,1,3,4,1,1,7,8,1,1))
  expect_equal(decode(ds$ep_5@case_nm), c("Case","Duplicate_C","Skipped","Skipped","Duplicate_C","Duplicate_C", "Skipped","Skipped", "Recurrent", "Duplicate_R"))
  expect_equal(ds$ep_5.5@.Data, c(1,2,3,4,1,1,7,8,1,1))
  expect_equal(decode(ds$ep_5.5@case_nm), c("Case","Case","Skipped","Skipped","Duplicate_C","Duplicate_C", "Skipped","Skipped", "Recurrent", "Duplicate_R"))
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
  expect_equal(ds$ep_11.2@.Data, c(1,1,3,4,5,5,7,8,5,5))
  expect_equal(decode(ds$ep_11.2@case_nm), c("Case","Duplicate_C","Skipped","Skipped", "Case", "Duplicate_C", "Skipped", "Skipped", "Duplicate_C", "Duplicate_C"))
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

# Dates
dates <- seq(as.Date("01/04/2019", "%d/%M/%Y"), as.Date("20/04/2019", "%d/%M/%Y"), "4 days")

# Periods
periods <- number_line(dates, dates + 4)

# Track fixed episodes from events with a 5 day cut-off
mth1 <- episodes(date = dates, case_length = 4)

# Track fixed episodes from periods that are 5 days long
mth2 <- episodes(date = periods, case_length = 0)

# Track rolling episodes from events using a 5 day cut-off
mth3 <- episodes(date = dates, case_length = 4, episode_type = "rolling")

# Track rolling episode from periods that are 5 days long
mth4 <- episodes(date = periods, case_length = 0, episode_type = "rolling")

test_that("test interchangeable use of interval grouping and event grouping ", {
  expect_equal(mth1@.Data, c(1,1,3,3,5))
  expect_equal(mth1@wind_id[[1]], c(1,1,3,3,5))
  expect_equal(decode(mth1@case_nm), c("Case", "Duplicate_C", "Case", "Duplicate_C", "Case"))
  expect_equal(decode(mth1@wind_nm$wind_nm1), c("Case", "Case", "Case", "Case", "Case"))
  expect_equal(mth3@.Data, c(1,1,1,1,1))
  expect_equal(mth3@wind_id[[1]], c(1,1,2,3,4))
  expect_equal(decode(mth3@case_nm), c("Case", "Duplicate_C", "Recurrent", "Recurrent", "Recurrent"))
  expect_equal(decode(mth3@wind_nm$wind_nm1), c("Case", "Case", "Recurrence", "Recurrence", "Recurrence"))
  expect_equal(mth1, mth2)
  expect_equal(mth3, mth4)
})

nl <- number_line
dt <- c(nl(1,1), nl(1,4), nl(4,5))
ep <- episodes(
  date = dt,
  custom_sort = c(1:3),
  episodes_max = 1,
  case_length = 2,
  reference_event = "last_event",
  batched = "yes")

test_that("xxx", {
  expect_equal(ep@.Data, c(1,1,1))
  expect_equal(decode(ep@case_nm), c("Case", "Case", "Duplicate_C"))
})

dt <- c(nl(1,1), nl(1,4), nl(4,5), nl(4,5), nl(14,25))
ep_l <- c(nl(10, 15), nl(10, 15), nl(-1, 0), nl(-1, 0), nl(-1, 0))
ep <-  episodes(
  date = dt,
  custom_sort = c(1:5),
  case_length = ep_l,
  reference_event = "last_event",
  skip_if_b4_lengths = TRUE,
  batched = "yes")

ep2 <-  episodes(
  date = dt,
  custom_sort = c(1:5),
  case_length = ep_l,
  reference_event = "last_event",
  skip_if_b4_lengths = FALSE,
  batched = "yes")

ep3 <-  episodes(
  date = dt,
  custom_sort = c(1:5),
  episodes_max = 1,
  case_length = ep_l,
  reference_event = "last_event",
  skip_if_b4_lengths = TRUE,
  batched = "yes")

test_that("batched + skip_if_b4_lengths", {
  expect_equal(ep@.Data, c(1,1,3,4,1))
  expect_equal(decode(ep@case_nm), c("Case", "Case", "Skipped", "Skipped", "Duplicate_C"))
  expect_equal(ep2@.Data, c(1,1,3,3,1))
  expect_equal(decode(ep2@case_nm), c("Case", "Case", "Case", "Case", "Duplicate_C"))
  expect_equal(ep, ep3)
})



