library(diyar)
load(file = "testData/epids_htest_1.RData")
htest_ep <- episodes(
  date = number_line(epids_htest_1$period_start, epids_htest_1$period_end),
  strata = epids_htest_1$pid,
  case_length = number_line(epids_htest_1$len_start, epids_htest_1$len_end),
  group_stats = TRUE,
  case_overlap_methods = epids_htest_1$overlap_methods_nm,
  skip_if_b4_lengths = FALSE,
  display = "progress",
  sn = epids_htest_1$sn)

epids_htest_1$htest_ep <- htest_ep
all(epids_htest_1$epid == htest_ep@.Data)
all(epids_htest_1$wind_nm == decode(htest_ep@wind_nm$wind_nm1))
all(epids_htest_1$case_nm == decode(htest_ep@case_nm))
all(epids_htest_1$wind_id1 == htest_ep@wind_id$wind_id1)
all(epids_htest_1$dist_wind_index == htest_ep@dist_wind_index)
all(epids_htest_1$dist_epid_index == htest_ep@dist_epid_index)
all(epids_htest_1$epid_start  == htest_ep@epid_interval@start)
all(epids_htest_1$epid_end  == right_point(htest_ep@epid_interval))
all(epids_htest_1$epid_length  == htest_ep@epid_length)

load(file = "testData/epids_mtest_1.RData")
mtest_ep <- episodes(
  sn = epids_mtest_1$sn,
  strata = epids_mtest_1$strata,
  date = epids_mtest_1$date,
  case_length = list(
    epids_mtest_1$case_length,
    -epids_mtest_1$case_length),
    episode_type = "fixed",
    episodes_max = 1,
    custom_sort =  epids_mtest_1$custom_sort,
    from_last = TRUE)

all(epids_mtest_1$epid == mtest_ep@.Data)
all(epids_mtest_1$wind_nm == decode(mtest_ep@wind_nm$wind_nm1))
all(epids_mtest_1$case_nm == decode(mtest_ep@case_nm))
all(epids_mtest_1$wind_id1 == mtest_ep@wind_id$wind_id1)
all(epids_mtest_1$dist_wind_index == mtest_ep@dist_wind_index)
all(epids_mtest_1$dist_epid_index == mtest_ep@dist_epid_index)
all(epids_mtest_1$epid_length  == mtest_ep@epid_length)
