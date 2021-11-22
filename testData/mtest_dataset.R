# Create epids_mtest_1

# Data for arguments
sn <- diyar::combi(master_dedup$id)
strata <- diyar::combi(master_dedup$strata)
strata[is.na(master_dedup$strata)] <- NA
date <- as.integer(master_dedup$date - min(master_dedup$date)) + 1
epi_len <- master_dedup$epi_len
c_sort <- custom_sort(master_dedup$neg_diff, master_dedup$mod_dod_days, master_dedup$fix_a)

# Episode grouping; Results have been verified
ep <- episodes(sn = sn,
               strata = strata,
               date = date,
               case_length = list(epi_len, -epi_len),
               episode_type = "fixed", episodes_max = 1,
               custom_sort =  c_sort,
               from_last = TRUE,
               display = "progress")

epids_mtest_1 <- tibble(sn, strata, date, epi_len, c_sort)
epids_mtest_1 <- bind_cols(epids_mtest_1, select(as_tibble(ep), -sn))
## Exclude the non-critical use cases
# Exclude `NA` strata
epids_mtest_1 <- filter(epids_mtest_1, !is.na(strata))
# Exclude strata with only one record
epids_mtest_1 <- epids_mtest_1 %>%
  group_by(strata) %>%
  filter(n() > 1) %>%
  ungroup()

epids_mtest_1 <- as.data.frame(epids_mtest_1)
save(list = "epids_mtest_1", file = "data/epids_mtest_1.RData")
