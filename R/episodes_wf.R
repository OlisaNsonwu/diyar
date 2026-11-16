#' @name episodes_wf
#' @title Link events to chronological episodes.
#'
#' @description \code{episodes_wf_*} functions are wrapper functions of \code{\link{episodes}}.
#' They are designed to be more efficient with large datasets i.e. reduce peak memory usage and/or run-time.
#' Duplicate records that do not affect the case definition are excluded prior to episode tracking.
#' The resulting episode identifiers are then recycled for the duplicate records.
#'
#' @param ... Arguments passed to \code{\link{episodes}}.
#' @param duplicates_recovered \code{[character]}. Determines which duplicate records are recycled.
#' Options are \code{"ANY"} (default), \code{"without_sub_criteria"}, \code{"with_sub_criteria"} or \code{"ALL"}. See \code{Details}.
#' @param sub_criteria_reframer \code{[function]}. A function to reframe the attributes of a \code{\link{sub_criteria}}. See \code{Details}.
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}.
#'
#' @seealso
#' \code{\link{episodes}}; \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{episodes_wf_repeats()}} reduces the number of records analysed in one go,
#' while \bold{\code{episodes_wf_splits()}} reduces the number strata analysed in one go.
#'
#' \bold{\code{episodes_wf_repeats()}} - Reframes a dataset to the minimum
#' number of records required to implement a case definition.
#' Episode tracking is performed on the reframed dataset and the
#' resulting episode identifiers are then recycled to the original records.
#'
#' The \code{duplicates_recovered} argument determines which
#' group identifiers from the reframed dataset are recycled. Options are:
#' \itemize{
#' \item \code{"with_sub_criteria"} - Only matches with a \code{\link{sub_criteria}} (\code{"Case_CR"} and \code{"Recurrent_CR"}) are recycled.
#' \item \code{"without_sub_criteria"} - Only matches without \code{\link{sub_criteria}} (\code{"Case"} and \code{"Recurrent"}) are recycled.
#' \item \code{"ANY"}/\code{"ALL"} - Convenience values to match any or all options.
#' }
#'
#' Important: When \code{case_sub_criteria} or \code{recurrence_sub_criteria}
#' are used, removing repeat records can change the behaviour of
#' \code{match_funcs} and equal_funcs. The sub_criteria_reframer argument
#' can be used to reframe \code{sub_criteria} attributes appropriately.
#' See \code{\link{reframe_repeats}}
#'
#' \bold{\code{episodes_wf_splits()}} - Split the datasets (by \code{strata}) into N-parts
#' and analyse each part separately. This can reduce peak memory usage by processing subsets of the data
#' sequentially rather than simultaneously.
#'
#'
#' The \code{reframe} argument will either \code{\link{reframe}} or subset a \code{\link{sub_criteria}}.
#' Both will require slightly different functions for \code{match_funcs} or \code{equal_funcs}.
#'
#' @rdname episodes_wf
#' @examples
#' # `episodes_wf_repeats()`
#' # With 2,000 duplicate records of 20 events,
#' # `episodes_wf_repeats()` will take less time than `episodes()`
#' dates <- seq(from = as.Date("2019-04-01"), to = as.Date("2019-04-20"), by = 1)
#' dates <- rep(dates, 200000)
#'
#' system.time(ep1 <- episodes(dates, 1, display = 'progress'))
#' # Recycle episode identifiers for duplicate records
#' system.time(ep2 <- episodes_wf_repeats(dates, 1, display = 'progress'))
#'
#' # Both leads to the same outcome.
#' all(ep1 == ep2)
#' all(ep1@case_nm == ep2@case_nm)
#'
#' @export
episodes_wf_repeats <- function(..., duplicates_recovered = "ANY", sub_criteria_reframer = subset_repeats){
  # Validations
  errs <- err_episodes_checks_0(...)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  # Extract options
  opts <- extract.opts(..., ref.func = episodes)

  tgt.opts.nms <-
    c("date", "case_length", "episode_type", "recurrence_length",
    "episode_unit", "strata", "sn", "episodes_max", "rolls_max",
    "case_overlap_methods", "recurrence_overlap_methods", "skip_if_b4_lengths",
    "data_source", "custom_sort", "skip_order", "reference_event",
    "case_for_recurrence", "from_last", "case_sub_criteria",
    "recurrence_sub_criteria", "case_length_total", "recurrence_length_total")
  tgt.opts <- opts[tgt.opts.nms]

  duplicates_recovered <- tolower(duplicates_recovered)

  # Unpack sub_criteria attributes
  for (opt in tgt.opts.nms) {
    if(inherits(tgt.opts[[opt]], "sub_criteria")){
      tgt.opts[[opt]] <- unpack_sub_criteria(tgt.opts[[opt]], part = 'attribute')
    }else if(inherits(tgt.opts[[opt]], "number_line")){
      tgt.opts[[opt]] <- list(tgt.opts[[opt]]@start, tgt.opts[[opt]]@.Data)
    }
  }

  # Id repeat records
  repeat_id <- flatten_list(tgt.opts)
  arg.lens <- lapply(repeat_id, length)
  repeat_id <- repeat_id[arg.lens > 1]
  repeat_id <- combi(repeat_id)

  tmp.nm <- c(
    'case_sub_criteria','recurrence_sub_criteria','case_length',
    'recurrence_length','case_length_total', 'recurrence_length_total')
  tgt.opts[tmp.nm] <- opts[tmp.nm]

  tmp.subset_func <- function(x, id = repeat_id) subset_repeats(x = x , repeat_id = id)
  tmp.reframer <- function(x, id = repeat_id) sub_criteria_reframer(x = x , repeat_id = id)

  for(arg in tgt.opts.nms){
    if(inherits(opts[[arg]], 'sub_criteria')){
      tgt.opts[[arg]] <- reframe(opts[[arg]], func = tmp.reframer)
    }else if(inherits(opts[[arg]], 'list')){
      tgt.opts[[arg]] <- reframe(opts[[arg]], func = tmp.subset_func)
    }else{
      tgt.opts[[arg]] <- tmp.subset_func(opts[[arg]])
    }
  }

  tgt.opts <- c(tgt.opts, opts[!names(opts) %in% tgt.opts.nms])

  dup_lgk <- duplicated(repeat_id)
  epids <- do.call(episodes, tgt.opts)

  rep_indx <- match(repeat_id, repeat_id[!dup_lgk])
  wf_epid <- epids[rep_indx]

  if(is.null(opts$sn)){
    wf_epid@sn <- seq_len(length(wf_epid))
  }else{
    wf_epid@sn <- opts$sn
  }

  wf_epid@case_nm[
    ((wf_epid@case_nm %in% c(0, 4) & duplicates_recovered == "any") |
    (wf_epid@case_nm == 0 & duplicates_recovered == "without_sub_criteria") |
    (wf_epid@case_nm == 4 & duplicates_recovered == "with_sub_criteria")) & dup_lgk] <- 2
  wf_epid@case_nm[
    ((wf_epid@case_nm %in% c(1, 5) & duplicates_recovered == "all") |
    (wf_epid@case_nm == 1 & duplicates_recovered == "without_sub_criteria") |
    (wf_epid@case_nm == 5 & duplicates_recovered == "with_sub_criteria")) & dup_lgk] <- 3

  lgk <- which(
    wf_epid@case_nm == -1 | (wf_epid@case_nm %in% c(0, 1, 4, 5) & dup_lgk))
  if(length(lgk) > 0){
    wf_epid@.Data[lgk] <- wf_epid@sn[lgk]
    for(i in seq_len(length(wf_epid@wind_id))){
      wf_epid@wind_id[lgk] <- wf_epid@sn[lgk]
    }
  }
  tot <- rle(sort(wf_epid@.Data[!seq_len(length(wf_epid)) %in% lgk]))
  wf_epid@epid_total <- tot$lengths[match(wf_epid@.Data, tot$values)]
  wf_epid@epid_total[is.na(wf_epid@epid_total)] <- 1L

  return(wf_epid)
}

#' @rdname episodes_wf
#' @examples
#' # `episodes_wf_splits()`
#' # 20,000 records distributed across 100 `strata`
#' dfr <- data.frame(
#'   date = seq_len(20000), strata = sample(1:100, 20000, replace = TRUE))
#' # Standard episode tracking
#' dfr$ep1 <- episodes(
#'   date = dfr$date, case_length = 100, strata = dfr$strata,
#'   display = 'progress')
#' # Process the same data in 5 strata batches with `episodes_wf_splits()`
#' dfr$ep2 <- episodes_wf_splits(
#'   date = dfr$date, case_length = 100, strata = dfr$strata,
#'   display = 'progress', splits_by_strata = 5)
#' # Equivalent split processing implemented directly in `episodes()`
#' dfr$ep3 <- episodes(
#'   date = dfr$date, case_length = 100, strata = dfr$strata,
#'   display = 'progress', splits_by_strata = 5)
#'
#' # All approaches produce the same result.
#' # Processing strata in 5 batches approximately halves peak memory
#' # usage, with only a modest increase in run-time.
#' all(dfr$ep1 == dfr$ep2)
#' all(dfr$ep2 == dfr$ep3)
#'
#' @export
episodes_wf_splits <- function(..., splits_by_strata = 1L){
  # Validations
  errs <- err_episodes_checks_0(...)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  # Extract options
  opts <- extract.opts(..., ref.func = episodes)

  if(is.null(opts$sn)){
    opts$sn <- seq_len(length(opts$date))
  }

  tgt.opts.nms <-
    c("date", "case_length", "episode_type", "recurrence_length",
      "episode_unit", "strata", "sn", "episodes_max", "rolls_max",
      "case_overlap_methods", "recurrence_overlap_methods", "skip_if_b4_lengths",
      "data_source", "custom_sort", "skip_order", "reference_event",
      "case_for_recurrence", "from_last", "case_sub_criteria",
      "recurrence_sub_criteria", "case_length_total", "recurrence_length_total")
  tgt.opts <- opts[tgt.opts.nms]

  # splits
  if(length(opts$strata) == 0){
    splits <- rep(1, length(opts$date))
  }else{
    splits <- (combi(opts$strata, ordered = TRUE) %% splits_by_strata) + 1L
  }
  sn_splits <- split(opts$sn, splits)

  epids <- list()
  for(i in sort(unique(splits))){
    i.opts <- list()
    iSplit <- splits == i
    tmp.func <- function(x) if(length(x) > 1) x[iSplit] else x
    for(arg in tgt.opts.nms){
      if(inherits(opts[[arg]], 'sub_criteria')){
        i.opts[[arg]] <- reframe(opts[[arg]], func = tmp.func)
      }else if(inherits(opts[[arg]], 'list')){
        i.opts[[arg]] <- reframe(opts[[arg]], func = tmp.func)
      }else{
        i.opts[[arg]] <- tmp.func(opts[[arg]])
      }
    }

    i.opts <- c(i.opts, opts[!names(opts) %in% tgt.opts.nms])
    epids[[i]] <- do.call(episodes, i.opts)
  }

  epids <- do.call('c', epids)
  sn_splits <- unlist(sn_splits, recursive = FALSE, use.names = FALSE)
  epids <- epids[order(sn_splits)]
  return(epids)
}
