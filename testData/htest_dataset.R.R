# Create epids_htest_1
spell_period_2 <- spell_period
spell_period_2@start <- as.integer((spell_period_2@start - min(spell_period_2@start)) + 1)

# Episode grouping; Results have been verified
ep <- diyar::episodes(date = spell_period_2,
                strata = pid,
                case_length = ep_len,
                group_stats = T,
                case_overlap_methods = ov_m,
                skip_if_b4_lengths = F,
                display = "progress",
                sn = sn)

epids_htest_1 <- tibble(pid, ov_m, sn)
epids_htest_1 <- bind_cols(epids_htest_1, select(as_tibble(ep), -sn, -epid_total, -iteration, -epid_dataset),
                    rename_all(select(as_tibble(spell_period_2), -id, -gid), ~paste0("period_", .)),
                    rename_all(select(as_tibble(ep_len), -id, -gid), ~paste0("len_", .)))

## Exclude the non-critical use cases
# Exclude `NA` strata
epids_htest_1 <- filter(epids_htest_1, !is.na(pid))
# Exclude same period duplicates
epids_htest_1 <- epids_htest_1 %>%
  arrange(pid, epid, case_nm) %>%
  distinct(pid, period_start, period_end, .keep_all = TRUE)
# Exclude strata with only one record
epids_htest_1$N <- bys_tot(epids_htest_1$pid)
epids_htest_1 <- filter(epids_htest_1, N >1) %>%
  select(-N)

epids_htest_1 <- as.data.frame(epids_htest_1)
save(list = "epids_htest_1", file = "data/epids_htest_1.RData")
