#' @name episodes
#' @title Group dated events into episodes.
#'
#' @description Dated events (records) within a certain duration of an index event are assigned to a unique group.
#' Each group has unique ID and are described as \code{"episodes"}.
#' \code{"episodes"} can be \code{"fixed"} or \code{"rolling"} (\code{"recurring"}).
#' Each episodes has a \code{"Case"} and/or \code{"Recurrent"} record
#' while all other records within the group are either \code{"Duplicates"} of
#' the \code{"Case"} or \code{"Recurrent"} event.
#'
#' @param sn \code{[integer]}. Unique record ID.
#' @param strata \code{[atomic]}. Subsets of the dataset. Episodes are created separately by each \code{strata}.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Record date or period.
#' @param case_length \code{[integer|\link{number_line}]}. Duration from an index event distinguishing one \code{"Case"} from another.
#' @param episodes_max \code{[integer]}. Maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{[character]}. Options are \code{"fixed"} (default) or \code{"rolling"}. See \code{Details}.
#' @param recurrence_length \code{[integer|\link{number_line}]}. Duration from an index event distinguishing a \code{"Recurrent"} event from its \code{"Case"} or prior \code{"Recurrent"} event.
#' @param episode_unit \code{[character]}. Unit of time for \code{case_length} and \code{recurrence_length}. Options are "seconds", "minutes", "hours", "days" (default), "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max \code{[integer]}. Maximum number of times an index event can recur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source \code{[character]}. Source ID for each record. If provided, a list of all sources in each episode is returned. See \code{\link[=epid-class]{epid_dataset slot}}.
#' @param from_last \code{[logical]}. Track episodes beginning from the earliest to the most recent record (\code{FALSE}) or vice versa (\code{TRUE}).
#' @param case_overlap_methods \code{[character|integer]}. Specific ways a period (record) most overlap with a \code{"Case"} event. See (\code{\link{overlaps}}).
#' @param recurrence_overlap_methods \code{[character|integer]}. Specific ways a period (record) most overlap with a \code{"Recurrent"} event. See (\code{\link{overlaps}}).
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index events. See \code{\link{custom_sort}}.
#' @param group_stats \code{[character]}. A selection of group metrics to return for each episode. Most are added to slots of the \code{\link[=epid-class]{epid}} object.
#' Options are \code{NULL} or any combination of \code{"case_nm"}, \code{"wind"} and \code{"epid_interval"}.
#' @param display \code{[character]}. Display progress update and/or generate a linkage report for the analysis. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param reference_event \code{[character]}. Specifies which of the records are used as index events. Options are \code{"last_record"} (default), \code{"last_event"}, \code{"first_record"} or \code{"first_event"}.
#' @param case_for_recurrence \code{[logical]}. If \code{TRUE}, a \code{case_length} is applied to both \code{"Case"} and \code{"Recurrent"} events.
#' If \code{FALSE} (default), a \code{case_length} is applied to only \code{"Case"} events.
#' @param skip_order \code{[integer]}. End episode tracking in a \code{strata} when the an index event's \code{custom_sort} order is greater than the supplied \code{skip_order}.
#' @param data_links \code{[list|character]}. \code{data_source} required in each \code{\link[=epid-class]{epid}}. An episode without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param skip_if_b4_lengths \code{[logical]}. If \code{TRUE} (default), events before a lagged \code{case_length} or \code{recurrence_length} are skipped.
#' @param skip_unique_strata \code{[logical]}. If \code{TRUE}, a strata with a single event is skipped.
#' @param case_sub_criteria \code{[\link{sub_criteria}]}. Additional nested match criteria for events in a \code{case_length}.
#' @param recurrence_sub_criteria \code{[\link{sub_criteria}]}. Additional nested match criteria for events in a \code{recurrence_length}.
#' @param case_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{case_lengths} required for an episode.
#' @param recurrence_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{recurrence_lengths} required for an episode.
#' @param batched \code{[character]}. At each iteration, compare records against one index record per \code{strata} (\code{"no"}) or all records of an index event (\code{"semi"}).
#' typically, the (\code{"semi"}) option will have a higher max memory and shorter run-time while (\code{"no"}) will have a lower max memory but longer run-time
#' @param splits_by_strata \code{[integer]}. Split the analysis into \code{n} parts. This typically lowers max memory used but increases the run time.
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{episodes_wf_splits}}; \code{\link{custom_sort}};
#' \code{\link{sub_criteria}}; \code{\link[=windows]{epid_length}};
#' \code{\link[=windows]{epid_window}}; \code{\link{partitions}};
#' \code{\link{links}}; \code{\link{overlaps}};
#'
#' @details
#' \bold{\code{episodes()}} links dated records (events) that
#' are within a set duration of each other in iterations.
#' Every record is linked to a unique group (episode; \code{\link[=epid-class]{epid}} object).
#' These episodes represent occurrences of interest as specified by function's arguments and defined by a case definition.
#'
#' In general, three type of episodes are possible;
#' \itemize{
#' \item \code{"fixed"} - An episode where all events are within a fixed duration of an index event.
#' \item \code{"rolling"} - An episode where all events are within a recurring duration of an index event.
#' }
#'
#' Every record in each episode is categorised as one of the following;
#' \itemize{
#' \item \code{"Case"} - Index event of the episode (without a nested match criteria).
#' \item \code{"Case_CR"} - Index event of the episode (with a nested match criteria).
#' \item \code{"Duplicate_C"} - Duplicate of the index event.
#' \item \code{"Recurrent"} - Recurrence of the index event (without a nested match criteria).
#' \item \code{"Recurrent_CR"} - Recurrence of the index event (with a nested match criteria).
#' \item \code{"Duplicate_R"} - Duplicate of the recurrent event.
#' \item \code{"Skipped"} - Skipped records.
#' }
#'
#' If \code{data_links} is supplied, every element of the list must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements are assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, groups without records from every listed \code{data_source} will be unlinked.
#' \item If named \code{"g"}, groups without records from any listed \code{data_source} will be unlinked.
#' }
#'
#' All records with a missing (\code{NA}) \code{strata} or \code{date} are skipped.
#'
#' Wrapper functions or alternative implementations of \bold{\code{episodes()}} for specific use cases or benefits:
#' \itemize{
#' \item \bold{\code{episodes_wf_splits()}} - Identical records are excluded from the main analysis.
#' \item \bold{\code{episodes_af_shift()}} - A mostly vectorised approach.
#' \item \bold{\code{links_wf_episodes()}} - The same functionality achieved with \code{\link{links}}.
#' }
#'
#' See \code{vignette("episodes")} for further details.
#'
#' @examples
#' data(infections)
#' data(hospital_admissions)
#'
#' # One 16-day (15-day difference) fixed episode per type of infection
#' episodes(date = infections$date,
#'          strata = infections$infection,
#'          case_length = 15,
#'          episodes_max = 1,
#'          episode_type = "fixed")
#'
#' # Multiple 16-day episodes with an 11-day recurrence period
#' episodes(date = infections$date,
#'          strata = NULL,
#'          case_length = 15,
#'          episodes_max = Inf,
#'          episode_type = "rolling",
#'          recurrence_length = 10,
#'          episode_type = "rolling")
#'
#' # Overlapping periods of hospital stays
#' dfr <- hospital_admissions[2:3]
#'
#' dfr$admin_period <-
#'   number_line(dfr$admin_dt,dfr$discharge_dt)
#'
#' dfr$ep <-
#'   episodes(date = dfr$admin_period,
#'            strata = NULL,
#'            case_length = index_window(dfr$admin_period),
#'            case_overlap_methods = "inbetween")
#'
#' dfr
#' as.data.frame(dfr$ep)
#'
#' @aliases episodes
#' @export
episodes <- function(
    date, case_length = Inf, episode_type = "fixed", recurrence_length = case_length,
    episode_unit = "days", strata = NULL, sn = NULL, episodes_max = Inf, rolls_max = Inf,
    case_overlap_methods = 8, recurrence_overlap_methods = case_overlap_methods,
    skip_if_b4_lengths = FALSE, data_source = NULL,
    data_links = "ANY", custom_sort = NULL, skip_order = Inf, reference_event = "last_record",
    case_for_recurrence = FALSE, from_last = FALSE, group_stats = c("case_nm", "wind", "epid_interval"),
    display = "none", case_sub_criteria = NULL, recurrence_sub_criteria = case_sub_criteria,
    case_length_total = 1, recurrence_length_total = case_length_total,
    skip_unique_strata = TRUE, splits_by_strata = 1, batched = "semi") {
  web <- list(tm_a = Sys.time())
  # Validations
  errs <- err_episodes_checks_0(
    sn = sn, date = date, case_length = case_length, strata = strata,
    display = display, episodes_max = episodes_max, from_last = from_last,
    episode_unit = episode_unit, case_overlap_methods = case_overlap_methods,
    recurrence_overlap_methods = recurrence_overlap_methods,
    skip_order = skip_order, custom_sort = custom_sort, group_stats = group_stats,
    data_source=data_source, data_links = data_links,
    skip_if_b4_lengths = skip_if_b4_lengths,
    rolls_max = rolls_max, case_for_recurrence = case_for_recurrence,
    reference_event = reference_event,
    episode_type = episode_type, recurrence_length = recurrence_length,
    case_sub_criteria = case_sub_criteria,
    recurrence_sub_criteria = recurrence_sub_criteria,
    case_length_total = case_length_total,
    recurrence_length_total = recurrence_length_total,
    skip_unique_strata = skip_unique_strata)
  if(!isFALSE(errs)){
    stop(errs, call. = FALSE)
  }

  web$counts$dataset.n <- length(date)

  # `episode_unit`
  episode_unit <- tolower(episode_unit)
  episode_unit <- match(episode_unit, names(diyar::episode_unit))
  class(episode_unit) <- "d_label"
  attr(episode_unit, "value") <- as.vector(sort(episode_unit[!duplicated(episode_unit)]))
  attr(episode_unit, "label") <- names(diyar::episode_unit)[attr(episode_unit, "value")]
  attr(episode_unit, "state") <- "encoded"
  # `strata`
  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1L, web$counts$dataset.n)
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }

  options_lst = list(date = date,
                     strata = (
                       if(is.null(strata)){
                         NULL
                       }else{
                           encode(strata)
                         }
                     ),
                     case_length = (
                       if(!inherits(case_length, "list")){
                         list(case_length)
                         }else{
                           case_length
                           }
                       ),
                     recurrence_length = (
                       if(!inherits(recurrence_length, "list")){
                         list(recurrence_length)
                         }else{
                           recurrence_length
                           }
                       ),
                     episode_unit = episode_unit,
                     from_last = from_last)
  episode_unit <- as.vector(episode_unit)

  # Standardise inputs
  # `display`
  display <- tolower(display)
  # `data_links`
  dl_lst <- unlist(data_links, use.names = FALSE)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))

  # `episode_type`
  episode_type <- tolower(episode_type)
  episode_type <- match(episode_type, c("fixed", "rolling", "recursive"))

  web$controls$use_recurrence <- any(episode_type %in% c(2, 3))

  # `date`
  date <- as.number_line(date)
  is_dt <- inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  episode_unit[!is_dt] <- 1L
  tmp.func <- function(x){
    if(inherits(x, "number_line")){
      x <- reverse_number_line(x, "decreasing")
    }else{
      x <- number_line(0, x)
    }
    return(x)
  }

  tmp.func.2 <- function(x){
    if(inherits(x, "list")){
      x <- lapply(x, function(y) any(start_point(y) < 0))
    }else{
      x <- any(start_point(x) < 0)
    }
    x <- any(as.logical(x))
    x <- ifelse(x, "assign_ord", "look_back_ord")
    return(x)
  }

  # `case_length`
  web$mm_opts$case_length <- length_to_range(
    lengths = case_length,
    date = date,
    from_last = from_last,
    episode_unit = episode_unit,
    skip_if_b4_lengths = skip_if_b4_lengths)
  if(inherits(case_length, "list")){
    web$mm_opts$case_length$length <- lapply(case_length, tmp.func)
  }else{
    web$mm_opts$case_length$length <- tmp.func(case_length)
  }
  if(FALSE){
    web$mm_opts$case_length$ord_var <- tmp.func.2(web$mm_opts$case_length$length)
  }else{
    web$mm_opts$case_length$ord_var <- "assign_ord"
  }

  web$counts$max_indexes <- 1L
  web$counts$w.list.num <- length(web$mm_opts$case_length$range)

  if(is.null(names(web$mm_opts$case_length$range))){
    names(web$mm_opts$case_length$range) <- rep(
      "w", web$counts$w.list.num)
  }else{
    names(web$mm_opts$case_length$range) <- ifelse(
      names(web$mm_opts$case_length$range) %in% c("", NA),
      "w", names(web$mm_opts$case_length$range))
  }

  web$opt_levels$case_length <- names(web$mm_opts$case_length$range)
  names(web$mm_opts$case_length$range) <- NULL
  web$mm_opts$case_length$range <- do.call("c", web$mm_opts$case_length$range)
  web$mm_opts$case_length$range_dt_a <- start_point(web$mm_opts$case_length$range)
  web$mm_opts$case_length$range_dt_z <- end_point(web$mm_opts$case_length$range)

  # `case_overlap_methods`
  # Use `overlap_methods` as a record-level input by default
  if(!all(class(case_overlap_methods) == "list")){
    case_overlap_methods <- list(r = case_overlap_methods)
  }
  if(is.null(names(case_overlap_methods))){
    names(case_overlap_methods) <-
      rep("r", length(case_overlap_methods))
  }else{
    names(case_overlap_methods) <-
      ifelse(names(case_overlap_methods)
             %in% c("", NA), "r",
             names(case_overlap_methods))
  }

  case_overlap_methods <- lapply(case_overlap_methods, function(x){
    if(length(x) == 1){
      x <- rep(x, web$counts$dataset.n)
    }
    return(x)
  })
  if(length(case_overlap_methods) == 1 & web$counts$w.list.num != 1){
    case_overlap_methods <- rep(case_overlap_methods, web$counts$w.list.num)
  }
  web$opt_levels$case_overlap_methods <- names(case_overlap_methods)
  names(case_overlap_methods) <- NULL
  web$mm_opts$case_overlap_methods <- do.call("c", case_overlap_methods)

  # `case_length_total`
  if(is.number_line(case_length_total)){
    case_length_total[case_length_total@.Data < 0] <-
      reverse_number_line(case_length_total[case_length_total@.Data < 0],
                          "decreasing")
  }else{
    case_length_total <- number_line(case_length_total, Inf)
  }
  web$controls$use_case_length_total <-
    any(!(case_length_total@start == 1 & case_length_total@.Data == Inf))

  web$controls$case$use_operators <-
    all(web$mm_opts$case_overlap_methods == 8) &
    all(date@.Data == 0) &
    all(web$mm_opts$case_length$range_dt_a == 0)

  if(web$controls$case$use_operators){
    web$controls$case$check.use_operators <-
      any(web$mm_opts$case_overlap_methods == 8) &
      any(date@.Data == 0) &
      any(web$mm_opts$case_length$range_dt_a == date@start)
  }else{
    web$controls$case$check.use_operators <-
      web$controls$case$use_operators <- FALSE
  }

  if(web$controls$use_recurrence){
    # `recurrence_length`
    web$mm_opts$recurrence_length <- length_to_range(
      lengths = recurrence_length,
      date = date,
      from_last = from_last,
      episode_unit = episode_unit,
      skip_if_b4_lengths = skip_if_b4_lengths)
    if(inherits(recurrence_length, "list")){
      web$mm_opts$recurrence_length$length <- lapply(recurrence_length, tmp.func)
    }else{
      web$mm_opts$recurrence_length$length <- tmp.func(recurrence_length)
    }
    if(FALSE){
      web$mm_opts$recurrence_length$ord_var <- tmp.func.2(web$mm_opts$recurrence_length$length)
    }else{
      web$mm_opts$recurrence_length$ord_var <- "assign_ord"
    }
    web$tmp$tmp.list.num <- length(web$mm_opts$recurrence_length$range)
    web$counts$w.list.num <- ifelse(
      web$counts$w.list.num > web$tmp$tmp.list.num,
      web$counts$w.list.num, web$tmp$tmp.list.num)

    if(is.null(names(web$mm_opts$recurrence_length$range))){
      names(web$mm_opts$recurrence_length$range) <- rep(
        "w", web$counts$w.list.num)
    }else{
      names(web$mm_opts$recurrence_length$range) <- ifelse(
        names(web$mm_opts$recurrence_length$range) %in% c("", NA),
        "w", names(web$mm_opts$recurrence_length$range))
    }

    web$opt_levels$recurrence_length <-
      names(web$mm_opts$recurrence_length$range)
    names(web$mm_opts$recurrence_length$range) <- NULL
    web$mm_opts$recurrence_length$range <-
      do.call("c", web$mm_opts$recurrence_length$range)
    web$mm_opts$recurrence_length$range_dt_a <-
      web$mm_opts$recurrence_length$range@start
    web$mm_opts$recurrence_length$range_dt_z <-
      right_point(web$mm_opts$recurrence_length$range)

    # `recurrence_overlap_methods`
    # Use `overlap_methods` as a record-level input by default
    if(!all(class(recurrence_overlap_methods) == "list")){
      recurrence_overlap_methods <- list(r = recurrence_overlap_methods)
    }
    if(is.null(names(recurrence_overlap_methods))){
      names(recurrence_overlap_methods) <-
        rep("r", length(recurrence_overlap_methods))
    }else{
      names(recurrence_overlap_methods) <-
        ifelse(names(recurrence_overlap_methods) %in% c("", NA),
               "r", names(recurrence_overlap_methods))
    }
    recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){
      if(length(x) == 1){
        x <- rep(x, web$counts$dataset.n)
      }
      return(x)
    })
    if(length(recurrence_overlap_methods) == 1 & web$counts$w.list.num != 1){
      recurrence_overlap_methods <-
        rep(recurrence_overlap_methods, web$counts$w.list.num)
    }
    web$opt_levels$recurrence_overlap_methods <-
      names(recurrence_overlap_methods)
    names(recurrence_overlap_methods) <- NULL
    web$mm_opts$recurrence_overlap_methods <-
      do.call("c", recurrence_overlap_methods)

    # `recurrence_length_total`
    if(is.number_line(recurrence_length_total)){
      recurrence_length_total[recurrence_length_total@.Data < 0] <-
        reverse_number_line(
          recurrence_length_total[recurrence_length_total@.Data < 0],
          "decreasing")
    }else{
      recurrence_length_total <- number_line(recurrence_length_total, Inf)
    }
    web$controls$use_recurrence_length_total <-
      any(!(recurrence_length_total@start == 1 &
              recurrence_length_total@.Data == Inf))

    attr(recurrence_length_total, "opts") <- attr(recurrence_length, "opts") <-
      class(case_for_recurrence) <-
      "d_lazy_opts"

    web$controls$recurrence$use_operators <-
      all(web$mm_opts$recurrence_overlap_methods == 8) &
      all(date@.Data == 0) &
      all(web$mm_opts$recurrence_length$range_dt_a == 0)

    if(web$controls$recurrence$use_operators){
      web$controls$recurrence$check.use_operators <-
        any(web$mm_opts$recurrence_overlap_methods == 8) &
        any(date@.Data == 0) &
        any(web$mm_opts$case_length$range_dt_a == date@start)
    }else{
      web$controls$recurrence$check.use_operators <-
        web$controls$recurrence$use_operators <- FALSE
    }
  }

  # `reference_event`
  if(inherits(reference_event, "logical")){
    reference_event[reference_event] <- "last_record"
    reference_event[reference_event != "last_record"] <- "first_record"
  }

  web$controls$use_events_as_refs <-
    any(reference_event %in% c("first_event", "last_event"))
  # `skip_if_b4_lengths`
  web$controls$use_skip_b4_len <- any(skip_if_b4_lengths == TRUE)

  # `d_lazy_opts`
  class(episodes_max) <- class(rolls_max) <- class(skip_order) <-
    class(from_last) <- class(episode_type) <- class(reference_event) <-
    attr(case_length_total, "opts") <-
    class(episode_unit) <- class(skip_if_b4_lengths) <- "d_lazy_opts"

  pr_sn <- seq_len(web$counts$dataset.n)

  # System preference for case-assignment
  ord_a <- abs(
    max(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start))
  ord_z <- abs(
    max(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date)))
  ord_a[!from_last] <- abs(
    min(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start[!from_last]))
  ord_z[!from_last] <- abs(
    min(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date[!from_last])))

  temporal_ord <- order(order(ord_a, -ord_z, pr_sn))
  rev_temporal_ord <- order(order(-ord_a, -ord_z, -pr_sn))

  # User-defined preference for case-assignment
  if(!is.null(custom_sort)) {
    if(any(!class(custom_sort) %in% c("numeric", "integer", "double"))){
      custom_sort <- as.integer(as.factor(custom_sort))
    }
    if(length(custom_sort) == 1){
      custom_sort <- rep(custom_sort, web$counts$dataset.n)
    }
  }else{
    custom_sort <- rep(0L, web$counts$dataset.n)
  }

  if(!is.null(data_source)) {
    if(length(data_source) == 1){
      data_source <- rep(data_source, web$counts$dataset.n)
    }
  }
  web$tm_ia <- Sys.time()
  web$repo$sn <- sn
  web$repo$pr_sn <-
    web$repo$epid <- pr_sn

  web$repo$c_hits <- web$repo$ld_pos <- rep(NA_real_, web$counts$dataset.n)

  web$repo$group_stats <- group_stats
  if(inherits(web$repo$group_stats, "logical")){
    if(web$repo$group_stats){
      web$repo$group_stats <- c("case_nm", "wind", "epid_interval")
    }else{
      web$repo$group_stats <- c("case_nm", "wind")
    }
  }

  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm <- rep(NA_real_, web$counts$dataset.n)
  }else{
    web$repo$case_nm <- NULL
  }

  if("wind" %in% web$repo$group_stats){
    web$repo$wind_id <- web$repo$wind_nm <-
      rep(NA_real_, web$counts$dataset.n)
  }else{
    web$repo$wind_id <- web$repo$wind_nm <- NULL
  }

  web$repo$cri <- cri
  web$repo$strata <- strata
  web$repo$temporal_ord <- temporal_ord
  web$repo$rev_temporal_ord <- rev_temporal_ord

  web$repo$cur_refs <- web$repo$max_refs <- web$repo$epid_n <-
    web$repo$iteration <- rep(0L, web$counts$dataset.n)

  web$repo$episode_unit <- episode_unit

  web$repo$tag <- rep(20L, web$counts$dataset.n)
  web$repo$reference_event <- reference_event
  web$repo$episode_type <- episode_type

  web$repo$last.batched <- rep(TRUE, web$counts$dataset.n)
  web$repo$nwEpi <- rep(FALSE, web$counts$dataset.n)

  web$repo$date <- date
  web$repo$case_sub_criteria <- case_sub_criteria
  web$repo$case_length_total <- case_length_total
  web$repo$skip_if_b4_lengths <- skip_if_b4_lengths
  web$repo$episodes_max <- episodes_max
  web$repo$episodes_max[web$repo$episodes_max < 0] <- 0
  web$repo$from_last <- from_last

  web$repo$custom_sort <- custom_sort
  web$repo$skip_order <- skip_order

  web$repo$data_links <- data_links
  web$repo$data_source <- data_source

  web$controls$use_skip_order <- any(skip_order < Inf) & !is.null(custom_sort)
  web$controls$use_episodes_max <- any(episodes_max < Inf)
  web$controls$use_rolls_max <- any(rolls_max < Inf)

  web$controls$is_dt <- is_dt
  web$controls$skip_unique_strata <- skip_unique_strata
  web$controls$display <- display
  web$export <- web$report <- list()

  web$controls$dl_lst <- dl_lst

  if(web$controls$use_recurrence){
    web$repo$roll_n <-
      # web$repo$rxt_n <-
      rep(0L, web$counts$dataset.n)
    web$repo$recurrence_sub_criteria <- recurrence_sub_criteria
    web$repo$recurrence_length_total <- recurrence_length_total
    web$repo$rolls_max <- rolls_max
    web$repo$case_for_recurrence <- case_for_recurrence
  }

  # User-specified records to skip
  if(!is.null(web$repo$strata)){
    web$tmp$lgk <- is.na(web$repo$strata)
    web$repo$tag[web$tmp$lgk] <- 10L
    if("case_nm" %in% web$repo$group_stats){
      web$repo$case_nm[web$tmp$lgk] <- -1L
    }
    web$repo$iteration[web$tmp$lgk] <- 0L
  }

  # Skip events with non-finite `dates`
  web$tmp$lgk <- is.na(web$repo$date@start) | is.na(web$repo$date@.Data)
  web$repo$tag[web$tmp$lgk] <- 10L
  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm[web$tmp$lgk] <- -1L
  }
  web$repo$iteration[web$tmp$lgk] <- 0L

  # Skip events from certain `data_source`
  if(!is.null(web$repo$data_source) & !all(toupper(web$controls$dl_lst) == "ANY")){
    web$tmp$lgk <- check_links(
      web$repo$cri,
      web$repo$data_source,
      web$repo$data_links)$rq
    web$repo$tag[!web$tmp$lgk] <- 10L
    if("case_nm" %in% web$repo$group_stats){
      web$repo$case_nm[!web$tmp$lgk] <- -1L
    }
    web$repo$iteration[!web$tmp$lgk] <- 0L
  }

  # Skip events without the required `skip_order`
  if(web$controls$use_skip_order){
    web$tmp$lgk <- order(web$repo$cri, web$repo$custom_sort)
    web$tmp$t_cri <- web$repo$cri[web$tmp$lgk]
    web$tmp$t_csort <- web$repo$custom_sort[web$tmp$lgk]
    web$tmp$t_csort <- web$tmp$t_csort[!duplicated(web$tmp$t_cri)]
    web$tmp$t_cri <- web$tmp$t_cri[!duplicated(web$tmp$t_cri)]

    web$tmp$min_custom_sort <-
      web$tmp$t_csort[match(web$repo$cri, web$tmp$t_cri)]
    web$tmp$lgk <- web$tmp$min_custom_sort <= web$repo$skip_order
    web$tmp$lgk <- !web$repo$cri %in% web$repo$cri[web$tmp$lgk]
    web$repo$tag[web$tmp$lgk] <- 10L
    if("case_nm" %in% web$repo$group_stats){
      web$repo$case_nm[web$tmp$lgk] <- -1L
    }
    web$repo$iteration[web$tmp$lgk] <- 0L
    web$tmp$min_custom_sort <-  web$tmp$t_cri <- web$tmp$t_csort <- NULL
  }

  # Close strata with only one record.
  web$tmp$lgk <- !duplicated(web$repo$cri, fromLast = TRUE) &
    !duplicated(web$repo$cri, fromLast = FALSE) & web$controls$skip_unique_strata
  web$repo$tag[web$tmp$lgk] <- 10L
  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm[web$tmp$lgk & is.na(web$repo$case_nm)] <- 0L
  }
  if("wind" %in% web$repo$group_stats){
    web$repo$wind_nm[web$tmp$lgk & is.na(web$repo$wind_nm)] <- 0L
    web$repo$wind_id[web$tmp$lgk] <- web$repo$pr_sn[web$tmp$lgk]
  }

  web$repo$iteration[web$tmp$lgk] <- 0L
  web$tmp$lgk <- web$tmp$lgk & is.na(web$repo$wind_id)

  web$tmp <- NULL

  web$counts$max_indexes <- ite <- 1L

  web$repo$look_back_ord <- order(order(
    web$repo$cri, -web$repo$custom_sort, web$repo$temporal_ord))
  web$repo$assign_ord <- order(order(
    web$repo$cri, web$repo$custom_sort, web$repo$temporal_ord))
  web$repo$rev_assign_ord <- order(order(
    web$repo$cri, web$repo$custom_sort, web$repo$rev_temporal_ord))

  # browser()
  rm(#assign_ord,
     case_for_recurrence, case_length, case_length_total,
     case_overlap_methods, case_sub_criteria, cri, custom_sort, data_links,
     data_source, date, display, dl_lst, episode_type,
     episode_unit, episodes_max, errs, from_last, group_stats,
     is_dt, ord_a, ord_z, pr_sn, recurrence_length, recurrence_length_total,
     recurrence_overlap_methods, recurrence_sub_criteria, reference_event,
     rolls_max, skip_if_b4_lengths, skip_order, skip_unique_strata,
     sn, strata)

  if(grepl("report$", web$controls$display)){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_a,
      "Data Prep."
      ,current_tot = web$counts$dataset.n
      ,memory_used =
        utils::object.size(web[names(web)[names(web) != "report"]]))
    web$report[length(web$report) + 1] <- list(web$rp_data)
  }

  web$sys.tmp$all_pos <- web$repo$pr_sn[
    order(web$repo$assign_ord)
  ]
  web$sys.tmp$rev_all_pos <- web$repo$pr_sn[
    order(web$repo$rev_assign_ord)
  ]

  web$counts$split <- 1L
  web$repo$prev.ite.window <-
    web$repo$ite.window <- rep(NA_real_, web$counts$dataset.n)
  web$repo$sys.batched <- web$repo$batched <- rep(FALSE, web$counts$dataset.n)

  web$tm_ia <- Sys.time()
  if(splits_by_strata > 1){
    web$splits$cri <- web$repo$cri[!duplicated(web$repo$cri)]
    web$splits$order <- as.integer(cut(web$splits$cri, breaks = splits_by_strata))
    web$splits <- split(web$splits$cri, web$splits$order)

    web$sys.tmp$ite_pos <-
      web$sys.tmp$all_pos[
        web$sys.tmp$all_pos %in% web$repo$pr_sn[
          web$repo$cri %in% web$splits[[web$counts$split]]
        ]
      ]
  }else{
    web$sys.tmp$ite_pos <- web$sys.tmp$all_pos
  }

  while (max(web$repo$tag) == 20) {
    web$sys.tmp$ite_pos <-
      web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] != 10]
    web$sys.tmp$ite_pos <- web$sys.tmp$all_pos[web$sys.tmp$all_pos %in% web$sys.tmp$ite_pos]
    web$sys.tmp$rev_ite_pos <- web$sys.tmp$rev_all_pos[web$sys.tmp$rev_all_pos %in% web$sys.tmp$ite_pos]

    if(splits_by_strata > 1 & length(web$sys.tmp$ite_pos) == 0){
      web$counts$split <- web$counts$split + 1L
      web$sys.tmp$ite_pos <-
        web$sys.tmp$all_pos[
          web$sys.tmp$all_pos %in% web$repo$pr_sn[
            web$repo$cri %in% web$splits[[web$counts$split]]
          ]
        ]
    }

    # Priority for reference events
    # Non-Batched tracked reference events
    web$tmp$tgt_pos_rev <- web$sys.tmp$rev_ite_pos[
      web$repo$tag[web$sys.tmp$rev_ite_pos] == 1 &
        !web$repo$batched[web$sys.tmp$rev_ite_pos]
      ]

    web$tmp$tgt_pos <- web$sys.tmp$ite_pos[
      web$repo$tag[web$sys.tmp$ite_pos] == 1 &
        !web$repo$batched[web$sys.tmp$ite_pos]
    ]

      # Batched tracked reference events
    web$tmp$lgk1 <-
      web$repo$reference_event[web$repo$ld_pos[web$tmp$tgt_pos_rev]] %in%
      c("last_event", "last_record") &
      web$repo$episode_type[web$repo$ld_pos[web$tmp$tgt_pos_rev]] != 3

    web$tmp$lgk2 <-
      web$repo$reference_event[web$repo$ld_pos[web$tmp$tgt_pos]] %in%
      c("first_event", "first_record") &
      web$repo$episode_type[web$repo$ld_pos[web$tmp$tgt_pos]] != 3

    web$sys.tmp$ite_pos <- c(
      # 0.
      web$sys.tmp$ite_pos[web$repo$batched[web$sys.tmp$ite_pos]],
      # 0.
      # web$tmp$tgt_pos,
      # 1. Windows with last record/event
      web$tmp$tgt_pos_rev[web$tmp$lgk1],
      # 2. Windows with first record/event
      web$tmp$tgt_pos[web$tmp$lgk2],
      # 3. Un-linked records
      web$sys.tmp$ite_pos[
        web$repo$tag[web$sys.tmp$ite_pos] == 20 &
          !web$repo$batched[web$sys.tmp$ite_pos]
        ]
    )

    # Window's reference record
    web$tmp$tr_lgk <- !duplicated(web$repo$cri[web$sys.tmp$ite_pos],
                                  fromLast = FALSE)
    # Excluded from this iteration
    web$tmp$exc_lgk <- rep(FALSE, length(web$sys.tmp$ite_pos))
    #
    if(web$controls$use_episodes_max){
      # Strata with `episode_max`
      web$tmp$lgk <-
        web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] ==
        web$repo$episodes_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]]
      web$tmp$lgk <-
        web$repo$cri[web$sys.tmp$ite_pos] %in%
        web$repo$cri[web$sys.tmp$ite_pos[web$tmp$tr_lgk]][web$tmp$lgk]
      web$tmp$exc_lgk[web$tmp$lgk] <- TRUE
    }

    if(web$controls$use_skip_order){
      # Strata that's reached `skip_order`
      web$tmp$lgk <- web$repo$custom_sort[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] >
        web$repo$skip_order[web$sys.tmp$ite_pos[web$tmp$tr_lgk]]
      web$tmp$lgk <- web$repo$cri[web$sys.tmp$ite_pos] %in%
        web$repo$cri[web$sys.tmp$ite_pos[web$tmp$tr_lgk]][web$tmp$lgk]
      web$tmp$exc_lgk[web$tmp$lgk] <- TRUE
    }

    # Closed excluded records
    web$repo$tag[web$sys.tmp$ite_pos[web$tmp$exc_lgk]] <- 10L
    # Exclude from iteration
    web$sys.tmp$ite_pos <- web$sys.tmp$ite_pos[!web$tmp$exc_lgk]

    if(length(web$sys.tmp$ite_pos) < 1){
      ite <- ite  + 1L

      if(web$counts$split < length(web$splits)){
        web$sys.tmp$ite_pos <- integer()
        next
      }else{
        break
      }
    }
    # Window's reference record based on new selection
    web$tmp2$s_cri <- web$repo$cri[web$sys.tmp$ite_pos]

    web$tmp2$s_ord <- order(
      web$tmp2$s_cri
    )
      # Window's reference record based on new selection
    web$tmp2$tr_lgk <- !duplicated(
      web$tmp2$s_cri[web$tmp2$s_ord], fromLast = FALSE)
    web$tmp2$ite_pos <- web$sys.tmp$ite_pos[web$tmp2$s_ord]
    web$tmp2$rl <- rle(web$tmp2$s_cri[web$tmp2$s_ord])
    # Window's reference record's position based on new selection
    web$tmp2$tr_pos <- rep(
      web$tmp2$ite_pos[web$tmp2$tr_lgk],
      web$tmp2$rl$lengths
    )

    web$sys.tmp$ite_tr_lgk <- web$tmp2$tr_lgk[order(web$tmp2$s_ord)]
    web$sys.tmp$ite_tr_pos <- web$tmp2$tr_pos[order(web$tmp2$s_ord)]

    # web$repo$tag[
    #   web$sys.tmp$ite_pos[!web$repo$batched[web$sys.tmp$ite_tr_pos]] &
    #     web$repo$tag[web$sys.tmp$ite_pos] == 1
    # ] <- 10L

    # Flag `strata` forming a new episode
    web$repo$nwEpi[web$sys.tmp$ite_pos] <-
      web$repo$cri[web$sys.tmp$ite_pos] %in% web$repo$cri[
        web$sys.tmp$ite_pos[
          web$sys.tmp$ite_tr_lgk &
            web$repo$tag[web$sys.tmp$ite_pos] == 20 &
            # !web$repo$batched[web$sys.tmp$ite_tr_pos]
            web$repo$last.batched[web$sys.tmp$ite_tr_pos]
        ]
      ]

    # Episode's reference record
    web$tmp$lgk <- web$repo$nwEpi[web$sys.tmp$ite_pos]

    web$repo$ld_pos[web$sys.tmp$ite_pos[web$tmp$lgk]] <-
      web$sys.tmp$ite_tr_pos[web$tmp$lgk]

    if(batched == "yes"){
      web$repo$last.batched[web$sys.tmp$ite_pos[web$tmp$lgk]] <-
        TRUE
    }


    if(grepl("^stats", web$controls$display)){
      msg <- paste0("Iteration ", fmt(ite) ,".\n")
      cat(msg)
    }
    nms_a <- names(web)
    # Record-pairs
      # Index for rec-pairs
    web$sys.tmp$ite_index <- web$sys.tmp$ite_tr_lgk
    if(ite == 1){
      # browser()
    }

    # Records for recursive checks (semi batched)
    web$sys.tmp$ite_rIndex <-
      rep(FALSE, length(web$sys.tmp$ite_pos))
    if(any(web$repo$reference_event[web$repo$ld_pos[web$sys.tmp$ite_pos]]
           %in% c("first_event", "last_event") |
           web$repo$episode_type[web$repo$ld_pos[web$sys.tmp$ite_pos]] == 3)
    ){
      # Reference events
      web$sys.tmp$ite_rIndex[
        web$repo$reference_event[web$repo$ld_pos[web$sys.tmp$ite_pos]] %in%
          c("first_event", "last_event") &
          # !web$sys.tmp$ite_tr_lgk &
          # !web$repo$batched[web$sys.tmp$ite_tr_pos]
          web$repo$last.batched[web$sys.tmp$ite_tr_pos]
      ]  <- TRUE

      web$sys.tmp$ite_rIndex[web$sys.tmp$ite_rIndex] <-
        overlap(
          web$repo$date[web$sys.tmp$ite_pos[web$sys.tmp$ite_rIndex]],
          web$repo$date[web$sys.tmp$ite_tr_pos[web$sys.tmp$ite_rIndex]])
      # Linked records of recursive window
      web$sys.tmp$ite_rIndex[
        web$repo$episode_type[web$repo$ld_pos[web$sys.tmp$ite_pos]] == 3 &
          web$repo$tag[web$sys.tmp$ite_pos] == 1 &
          # !web$sys.tmp$ite_tr_lgk &
          # !web$repo$batched[web$sys.tmp$ite_tr_pos]
          web$repo$last.batched[web$sys.tmp$ite_tr_pos]
      ] <- TRUE
      #
      web$sys.tmp$ite_index[
        web$sys.tmp$ite_rIndex &
          batched == "semi"
      ] <- TRUE

      if(batched == "yes"){
        web$manual_recursive_index.pos <- web$sys.tmp$ite_pos[web$sys.tmp$ite_rIndex]
        web$repo$batched[web$manual_recursive_index.pos] <- TRUE

        web$manual_recursive_index.tr_pos <-
          web$sys.tmp$ite_tr_pos[web$sys.tmp$ite_rIndex]
        web$manual_recursive_index.tr_pos <-
          web$manual_recursive_index.tr_pos[!duplicated(web$manual_recursive_index.tr_pos)]

        web$repo$sys.batched[
          c(web$manual_recursive_index.pos,
            web$manual_recursive_index.tr_pos)] <- TRUE

      }else{
        web$manual_recursive_index.pos <- integer()
      }

    }

    web$repo$tmp.assign_ord <- web$repo$assign_ord[web$sys.tmp$ite_pos]
    if(batched == "yes"){
      web$repo$tmp.assign_ord[
        web$repo$tag[web$sys.tmp$ite_pos] == 1 &
          web$repo$sys.batched[web$sys.tmp$ite_pos] &
          !web$sys.tmp$ite_index
      ] <- Inf
    }

    if(ite == 4){
      # browser()
    }

    web$rec.pairs <- make_pairs_batched(
      strata = web$repo$cri[web$sys.tmp$ite_pos],
      x = web$sys.tmp$ite_pos,
      index_record = web$sys.tmp$ite_index,
      assign_ord = web$repo$tmp.assign_ord,
      include_repeat = TRUE,
      look_back = FALSE
    )

    # print(as.data.frame(web$rec.pairs))

    web$rec.pairs$x_pos <-
      web$rec.pairs$y_pos <- NULL
    names(web$rec.pairs)[which(names(web$rec.pairs) == "x_val")] <-
      "cu_pos"
    names(web$rec.pairs)[which(names(web$rec.pairs) == "y_val")] <-
      "tr_pos"
    web$rec.pairs$cri <- web$repo$cri[web$rec.pairs$cu_pos]
    web$rec.pairs$nwEpi <- web$repo$nwEpi[web$rec.pairs$cu_pos]
    # Episode's reference record
    web$rec.pairs$ld_pos <- web$repo$ld_pos[web$rec.pairs$cu_pos]
    # Window's reference record
    web$rec.pairs$ref_rd <-
      web$rec.pairs$cu_pos == web$rec.pairs$tr_pos

    if(web$controls$use_recurrence){
      # Record-pairs split by type of windows
      # Reset to case window if it's a new episode
      web$repo$ite.window[
        web$repo$cri %in%
          web$rec.pairs$cri[
            web$rec.pairs$nwEpi &
              web$repo$tag[web$rec.pairs$tr_pos] == 20]
      ] <- 1L

      web$repo$prev.ite.window <- web$repo$ite.window
      web$repo$ite.window[
        web$repo$cri %in%
          web$rec.pairs$cri[
            web$repo$case_for_recurrence[web$rec.pairs$ld_pos] &
              !web$rec.pairs$nwEpi &
              web$repo$prev.ite.window[web$rec.pairs$tr_pos] == 2 &
              # !web$repo$batched[web$rec.pairs$tr_pos]]
              web$repo$last.batched[web$rec.pairs$tr_pos]]
      ] <- 3L

      web$repo$ite.window[
        web$repo$cri %in%
          web$rec.pairs$cri[
            !web$rec.pairs$nwEpi &
              web$repo$prev.ite.window[web$rec.pairs$tr_pos] %in% c(1, 3) &
              # !web$repo$batched[web$rec.pairs$tr_pos]]
              web$repo$last.batched[web$rec.pairs$tr_pos]]
      ] <- 2L

      web$rec.pairs$is_case_window <-
        web$repo$ite.window[web$rec.pairs$cu_pos]

      web$window_opts <-
        web$rec.pairs$is_case_window[!duplicated(web$rec.pairs$is_case_window)]
      web$window_opts <- sort(web$window_opts, decreasing = TRUE)

      web$rec.pairs <- web$rec.pairs[
        c("cri", "index_ord", "cu_pos", "tr_pos",
          "nwEpi", "ld_pos", "is_case_window", "ref_rd")]
      web$rec.pairs <- lapply(web$window_opts, function(i){
        lgk <- web$rec.pairs$is_case_window %in% i
        lapply(web$rec.pairs, function(x){
          x[lgk]
        })
      })
      web$window_opts_nm <-
        c("case", "recurrence", "case_for_recurrence")[web$window_opts]
      names(web$rec.pairs) <- web$window_opts_nm
    }else{
      web$rec.pairs$is_case_window <- rep(1L, length(web$rec.pairs$cu_pos))
      web$window_opts <- 1L
      web$window_opts_nm <- "case"
      web$rec.pairs <- list(case = web$rec.pairs[
        c("cri", "index_ord", "cu_pos", "tr_pos",
          "nwEpi", "ld_pos", "is_case_window", "ref_rd")
      ])
    }

    if(batched == "yes"){
      web$tmp$tgt_pos <- web$sys.tmp$ite_pos[web$repo$batched[web$sys.tmp$ite_pos]]
      web$tmp$tgt_pos_2 <- web$sys.tmp$ite_pos[web$repo$sys.batched[web$sys.tmp$ite_pos]]
      web$repo$last.batched[web$tmp$tgt_pos_2] <-
        web$repo$cri[web$tmp$tgt_pos_2] %in%
        web$repo$cri[web$tmp$tgt_pos][
          !duplicated(web$repo$cri[web$tmp$tgt_pos], fromLast = TRUE) &
            !duplicated(web$repo$cri[web$tmp$tgt_pos], fromLast = FALSE)
        ]
    }

    # Separate overlap and sub_criteria check for each type of window
    for (w.code in seq_len(length(web$window_opts))) {
      #
      w.name <- web$window_opts_nm[w.code]
      w.type <- ifelse(w.name %in% c("case", "case_for_recurrence"),
                       "case", "recurrence")
      w.code <- web$window_opts[w.code]
      web$tmp$range.n <- paste0(w.type, "_range.n")
      web$tmp$overlap_method <- paste0(w.type, "_overlap_methods")
      web$tmp$sub_criteria_nm <- paste0(w.type, "_sub_criteria")
      web$tmp$length_nm <- paste0(w.type, "_length")
      web$controls$use_sub_cri <- inherits(web$repo[[web$tmp$sub_criteria_nm]], "sub_criteria")

      #
      web$counts$rec.pairs.wind <- length(web$rec.pairs[[w.name]]$cu_pos)
      #
      if(web$counts$w.list.num > 1){
        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$cu_pos,
          multiples = web$counts$dataset.n,
          repeats = web$counts$w.list.num)
        names(web$tmp$tgt_pos) <- paste0("cu_pos.", names(web$tmp$tgt_pos))
        web$rec.pairs[[w.name]] <- c(
          web$rec.pairs[[w.name]], web$tmp$tgt_pos)

        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$tr_pos,
          multiples = web$counts$dataset.n,
          repeats = web$counts$w.list.num)
        names(web$tmp$tgt_pos) <- paste0("tr_pos.", names(web$tmp$tgt_pos))
        web$rec.pairs[[w.name]] <- c(
          web$rec.pairs[[w.name]], web$tmp$tgt_pos)

        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$ld_pos,
          multiples = web$counts$dataset.n,
          repeats = web$counts$w.list.num)
        names(web$tmp$tgt_pos) <- paste0("ld_pos.", names(web$tmp$tgt_pos))
        web$rec.pairs[[w.name]] <- c(
          web$rec.pairs[[w.name]], web$tmp$tgt_pos)
        web$tmp$tgt_pos <- NULL
      }else{
        web$rec.pairs[[w.name]]$cu_pos.mi <-
          web$rec.pairs[[w.name]]$cu_pos.mm <-
          web$rec.pairs[[w.name]]$cu_pos
        web$rec.pairs[[w.name]]$tr_pos.mi <-
          web$rec.pairs[[w.name]]$tr_pos.mm <-
          web$rec.pairs[[w.name]]$tr_pos
        web$rec.pairs[[w.name]]$ld_pos.mi <-
          web$rec.pairs[[w.name]]$ld_pos.mm <-
          web$rec.pairs[[w.name]]$ld_pos
        web$rec.pairs[[w.name]]$cu_pos.ord <- rep(
          1L, web$counts$rec.pairs.wind)
      }

      #
      web$rec.pairs[[w.name]]$len_pos <- mix_pos(
        cu_pos.mi = web$rec.pairs[[w.name]]$cu_pos.mm,
        tr_pos.mi = web$rec.pairs[[w.name]]$tr_pos.mm,
        ld_pos.mi = web$rec.pairs[[w.name]]$ld_pos.mi,
        cu_pos.ord = web$rec.pairs[[w.name]]$cu_pos.ord,
        opt_levels = web$opt_levels[[web$tmp$length_nm]]
      )
      #
      web$rec.pairs[[w.name]]$ovr_pos <- mix_pos(
        cu_pos.mi = web$rec.pairs[[w.name]]$cu_pos.mm,
        tr_pos.mi = web$rec.pairs[[w.name]]$tr_pos.mm,
        ld_pos.mi = web$rec.pairs[[w.name]]$ld_pos.mi,
        cu_pos.ord = web$rec.pairs[[w.name]]$cu_pos.ord,
        opt_levels = web$opt_levels[[web$tmp$overlap_method]]
      )
      # Check overlapping periods
      web$rec.pairs[[w.name]]$ep_checks <-
        rep(FALSE, length(web$rec.pairs[[w.name]]$cu_pos.mi))
      if(web$controls[[w.type]]$use_operators){
        web$tmp$lgk <- TRUE
        web$tmp$lgk <- mk_lazy_opt(web$tmp$lgk)
      }else{
        if(web$controls[[w.type]]$check.use_operators){
          web$tmp$lgk <-
            web$mm_opts[[web$tmp$overlap_method]][web$rec.pairs[[w.name]]$ovr_pos] == 8 &
            web$repo$date@.Data[web$rec.pairs[[w.name]]$cu_pos.mi] == 0 &
            web$mm_opts[[web$tmp$length_nm]]$range_dt_a[web$rec.pairs[[w.name]]$cu_pos.mi] ==
            web$repo$date@start[web$rec.pairs[[w.name]]$cu_pos.mi]
        }else{
          web$tmp$lgk <- FALSE
          web$tmp$lgk <- mk_lazy_opt(web$tmp$lgk)
        }
      }

      web$tmp$lgk2 <- web$tmp$lgk &
        !web$repo$from_last[web$rec.pairs[[w.name]]$cu_pos.mi]
      web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk2] <-
        web$repo$date@start[
          web$rec.pairs[[w.name]]$cu_pos.mi[web$tmp$lgk2]] <=
        web$mm_opts[[web$tmp$length_nm]]$range_dt_z[
          web$rec.pairs[[w.name]]$len_pos[web$tmp$lgk2]]

      web$tmp$lgk2 <- web$tmp$lgk &
        web$repo$from_last[web$rec.pairs[[w.name]]$cu_pos.mi]
      web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk2] <-
        web$repo$date@start[
          web$rec.pairs[[w.name]]$cu_pos.mi[web$tmp$lgk2]] >=
        web$mm_opts[[web$tmp$length_nm]]$range_dt_a[
          web$rec.pairs[[w.name]]$len_pos[web$tmp$lgk2]]

      if(any(!web$tmp$lgk)){
        web$rec.pairs[[w.name]]$ep_checks[!web$tmp$lgk] <-
          overlaps(
            x = web$repo$date[
              web$rec.pairs[[w.name]]$cu_pos.mi[!web$tmp$lgk]],
            y = web$mm_opts[[web$tmp$length_nm]]$range[
              web$rec.pairs[[w.name]]$len_pos[!web$tmp$lgk]],
            m = web$mm_opts[[web$tmp$overlap_method]][
              web$rec.pairs[[w.name]]$ovr_pos[!web$tmp$lgk]]
          )
      }

      # Do not match previously linked records.
      # Is possible if case_length is less than recurrence length and case_for_recurrence is `TRUE`
      web$rec.pairs[[w.name]]$ep_checks[
        web$rec.pairs[[w.name]]$ep_checks &
          web$repo$tag[web$rec.pairs[[w.name]]$cu_pos.mi] != 20
      ] <- FALSE

      if(web$controls[[paste0("use_", w.type, "_length_total")]]){
        # Number of records matched (Excludes self-matches)
        web$tmp$c_hits <- make_refs_V2(
          x = web$rec.pairs[[w.name]]$tr_pos.mi[web$rec.pairs[[w.name]]$ep_checks],
          y = !duplicated(web$rec.pairs[[w.name]]$tr_pos.ord[web$rec.pairs[[w.name]]$ep_checks]),
          useAsPos = FALSE,
          na = 0
        )

        web$tmp$nms <- colnames(web$tmp$c_hits)
        web$tmp$nms.y <- web$tmp$nms[grepl("^y", colnames(web$tmp$c_hits))]
        if(length(web$tmp$nms.y) > 1){
          if(nrow(web$tmp$c_hits) > 1){
            web$tmp$c_hits <- cbind(
              web$tmp$c_hits[, "x"],
              rowSums(web$tmp$c_hits[, web$tmp$nms.y]))
          }else{
            web$tmp$c_hits <- cbind(
              web$tmp$c_hits[, "x"],
              sum(web$tmp$c_hits[, web$tmp$nms.y]))
          }
        }else{
          web$tmp$c_hits <- cbind(
            web$tmp$c_hits[, "x"],
            web$tmp$c_hits[, web$tmp$nms.y])
        }

        colnames(web$tmp$c_hits) <- c("x", "sum")
        web$repo$c_hits[web$tmp$c_hits[, "x"]] <- web$tmp$c_hits[, "sum"]
        web$rec.pairs[[w.name]]$c_hits <-
          web$repo$c_hits[web$rec.pairs[[w.name]]$tr_pos]

        # Number of matches requested
        web$tmp$required_len_tot <-
          web$repo[[paste0(w.type, "_length_total")]][
            web$rec.pairs[[w.name]]$ld_pos]
      }

      # Implement `sub_criteria`
      if(web$controls$use_sub_cri){
        web$rec.pairs[[w.name]]$s.match <-
          rep(FALSE, web$counts$rec.pairs.wind)
        web$tmp$lgk <-
          web$rec.pairs[[w.name]]$tr_pos.mm[
            web$rec.pairs[[w.name]]$ep_checks
          ]
        web$tmp$lgk <-
          !duplicated(web$tmp$lgk, fromLast = TRUE) &
          !duplicated(web$tmp$lgk, fromLast = FALSE) &
          web$controls$skip_unique_strata
        web$rec.pairs[[w.name]]$s.match[
          web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk] <-
          eval_sub_criteria(
            x = web$repo[[web$tmp$sub_criteria_nm]],
            x_pos = web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk],
            y_pos = web$rec.pairs[[w.name]]$tr_pos[
              web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk])$logical_test

        # Check of >0 `sub_criteria`-matches across all case_length or recurrence_length
        web$rec.pairs[[w.name]]$s.match <-
          rowSums(matrix(web$rec.pairs[[w.name]]$s.match,
                         ncol = web$counts$w.list.num))
      }else{
        web$rec.pairs[[w.name]]$s.match <- FALSE
        web$rec.pairs[[w.name]]$s.match <-
          mk_lazy_opt(web$rec.pairs[[w.name]]$s.match)
      }

      if(web$counts$w.list.num > 1){
        # Check of >0 overlap-matches across all case_length or recurrence_length
        web$rec.pairs[[w.name]]$ep_checks <-
          rowSums(matrix(web$rec.pairs[[w.name]]$ep_checks,
                         ncol = web$counts$w.list.num))
      }

      # Check user-defined conditions
      web$rec.pairs[[w.name]]$w.match <- as.logical(web$rec.pairs[[w.name]]$ep_checks)

      if(web$controls$use_sub_cri){
        web$rec.pairs[[w.name]]$w.match <-
          web$rec.pairs[[w.name]]$w.match &
          web$rec.pairs[[w.name]]$s.match
      }

      if(web$controls[[paste0("use_", w.type, "_length_total")]]){
        web$rec.pairs[[w.name]]$w.match <-
          web$rec.pairs[[w.name]]$w.match &
          !is.na(web$rec.pairs[[w.name]]$c_hits) &
          web$rec.pairs[[w.name]]$c_hits >= web$tmp$required_len_tot@start &
          web$rec.pairs[[w.name]]$c_hits <= web$tmp$required_len_tot@start + web$tmp$required_len_tot@.Data
      }

      web$rec.pairs[[w.name]]$w.match[
        web$rec.pairs[[w.name]]$ref_rd
        ] <- TRUE

      if("wind" %in% web$repo$group_stats){
        web$tmp$tgt_lgk <- any(duplicated(web$rec.pairs[[w.name]]$cu_pos[
          web$rec.pairs[[w.name]]$w.match]))

        if(web$tmp$tgt_lgk){
          web$tmp$s_ord <- order(web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$w.match])
          web$tmp$nw_index_ord <- rle(web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$w.match][web$tmp$s_ord])
          web$tmp$nw_index_ord <- sequence(web$tmp$nw_index_ord$lengths)
          web$tmp$nw_index_ord <- web$tmp$nw_index_ord[order(web$tmp$s_ord)]

          web$tmp$lgk <- !duplicated(web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$w.match], fromLast = TRUE)
          web$repo$max_refs[web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$w.match][web$tmp$lgk]] <-
            web$repo$cur_refs[web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$w.match][web$tmp$lgk]] + web$tmp$nw_index_ord[web$tmp$lgk]

          # Maximum number of index records per episode (`max_indexes`).
          web$tmp$max_indexes <- suppressWarnings(max(web$repo$max_refs))
          # Increase the number of `wind_id` by multiples of `max_indexes`
          if(web$tmp$max_indexes > web$counts$max_indexes){
            web$tmp$indx <- rep(
              rep(NA_real_, web$counts$dataset.n),
              (web$tmp$max_indexes - web$counts$max_indexes))

            web$repo$wind_id <- c(web$repo$wind_id, web$tmp$indx)
            web$repo$wind_nm <- c(web$repo$wind_nm, web$tmp$indx)
            web$counts$max_indexes <- web$tmp$max_indexes
          }
          web$tmp$pos <- ((web$tmp$nw_index_ord + web$repo$cur_refs[web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]] - 1L) * web$counts$dataset.n) + web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]
          web$repo$cur_refs <- web$repo$max_refs
        }else{
          web$tmp$pos <- web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]
        }
        # Index for the multiple window ids
        # Update window ids for matches
        web$repo$wind_nm[web$tmp$pos[is.na(web$repo$wind_id[web$tmp$pos])]] <-
          (w.code - 1)
        web$repo$wind_id[web$tmp$pos[is.na(web$repo$wind_id[web$tmp$pos])]] <-
          web$rec.pairs[[w.name]]$tr_pos[web$rec.pairs[[w.name]]$w.match][
            is.na(web$repo$wind_id[web$tmp$pos])
          ]
      }

      web$rec.pairs[[w.name]]$index_rd <-
        web$rec.pairs[[w.name]]$ref_rd
      if(web$counts$w.list.num > 1){
        # Consolidate tests for the same record across all reference windows
        web$rec.pairs[[w.name]]$w.match <- web$rec.pairs[[w.name]]$cu_pos %in%
          web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$w.match
          ]
        web$rec.pairs[[w.name]]$ep_checks <- web$rec.pairs[[w.name]]$cu_pos %in%
          web$rec.pairs[[w.name]]$cu_pos[
            as.logical(web$rec.pairs[[w.name]]$ep_checks)
          ]
        if(web$controls$use_sub_cri){
          web$rec.pairs[[w.name]]$s.match <- web$rec.pairs[[w.name]]$cu_pos %in%
            web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$s.match
            ]
        }
        if(web$controls$use_skip_b4_len){
          web$rec.pairs[[w.name]]$sk_checks <- web$rec.pairs[[w.name]]$cu_pos %in%
            web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$sk_checks
            ]
        }
        web$rec.pairs[[w.name]]$index_rd <- web$rec.pairs[[w.name]]$cu_pos %in%
          web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$index_rd
          ]
      }

      if(web$controls$use_skip_b4_len){
        web$tmp$indx <- which(!web$rec.pairs[[w.name]]$w.match &
                                web$repo$skip_if_b4_lengths[
                                  web$rec.pairs[[w.name]]$ld_pos] &
                                !web$repo$batched[
                                  web$rec.pairs[[w.name]]$cu_pos]
                                )
        web$rec.pairs[[w.name]]$sk_checks <-
          rep(FALSE, web$counts$rec.pairs.wind)
        web$rec.pairs[[w.name]]$sk_checks[web$tmp$indx] <- overlap(
          x = web$repo$date[
            web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]],
          y = web$mm_opts[[web$tmp$length_nm]]$coverage[
            web$rec.pairs[[w.name]]$len_pos[web$tmp$indx]]
        )
      }
      web$tmp$vrs <- c("cri", "cu_pos", "tr_pos", "ld_pos", "index_ord",
                       "is_case_window", "nwEpi", "s.match",
                       "w.match", "ep_checks", "ref_rd", "index_rd",
                       "sk_checks")
      web$tmp$vrs <- names(web$rec.pairs[[w.name]])[
        names(web$rec.pairs[[w.name]]) %in% web$tmp$vrs]
      web$rec.pairs[[w.name]] <-
        web$rec.pairs[[w.name]][web$tmp$vrs]

      # Remove record-pairs uses for recursive checks
      if(web$counts$w.list.num > 1){
        web$tmp$lgk <- web$rec.pairs[[w.name]]$index_ord == 1
        web$rec.pairs[[w.name]]$cri <- web$rec.pairs[[w.name]]$cri[web$tmp$lgk]
        web$rec.pairs[[w.name]]$cu_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$lgk]
        web$rec.pairs[[w.name]]$tr_pos <- web$rec.pairs[[w.name]]$tr_pos[web$tmp$lgk]
        web$rec.pairs[[w.name]]$ld_pos <- web$rec.pairs[[w.name]]$ld_pos[web$tmp$lgk]
        web$rec.pairs[[w.name]]$index_ord <- web$rec.pairs[[w.name]]$index_ord[web$tmp$lgk]
        web$rec.pairs[[w.name]]$is_case_window <- web$rec.pairs[[w.name]]$is_case_window[web$tmp$lgk]
        web$rec.pairs[[w.name]]$nwEpi <- web$rec.pairs[[w.name]]$nwEpi[web$tmp$lgk]
        web$rec.pairs[[w.name]]$w.match <- web$rec.pairs[[w.name]]$w.match[web$tmp$lgk]
        web$rec.pairs[[w.name]]$ep_checks <- web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk]
        web$rec.pairs[[w.name]]$ref_rd <- web$rec.pairs[[w.name]]$ref_rd[web$tmp$lgk]
        web$rec.pairs[[w.name]]$index_rd <- web$rec.pairs[[w.name]]$index_rd[web$tmp$lgk]
        if(web$controls$use_sub_cri){
          web$rec.pairs[[w.name]]$s.match <- web$rec.pairs[[w.name]]$s.match[web$tmp$lgk]
        }
        if(web$controls$use_skip_b4_len){
          web$rec.pairs[[w.name]]$sk_checks <- web$rec.pairs[[w.name]]$sk_checks[web$tmp$lgk]
        }
      }

      web$repo$epid[
        web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]] <-
        -web$rec.pairs[[w.name]]$ld_pos[web$rec.pairs[[w.name]]$w.match]

      # Update episode counter
      if(web$controls$use_episodes_max){
        web$tmp$upd_lgk <-
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] == 1 &
          web$repo$tag[web$rec.pairs[[w.name]]$tr_pos] == 20L &
          web$repo$last.batched[web$rec.pairs[[w.name]]$tr_pos]

        web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] <-
          web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] + 1L

        web$tmp$upd_lgk <-
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] %in% c(2, 3) &
          web$repo$tag[web$rec.pairs[[w.name]]$tr_pos] == 20L &
          web$repo$last.batched[web$rec.pairs[[w.name]]$tr_pos]

        web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] <-
          web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] + .5
      }

      if(w.name %in% c("recurrence", "case_for_recurrence")){
        # Update rolling counter
        web$tmp$vr <- web$rec.pairs[[w.name]]$cri %in%
          web$rec.pairs[[w.name]]$cri[
            web$rec.pairs[[w.name]]$w.match &
              # !web$repo$batched[web$rec.pairs[[w.name]]$tr_pos]
              web$repo$last.batched[web$rec.pairs[[w.name]]$tr_pos]
            ]
        web$tmp$vr2 <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$vr]
        web$tmp$vr <- web$rec.pairs[[w.name]]$tr_pos[web$tmp$vr]
        web$tmp$vr2 <- c(web$tmp$vr[!duplicated(web$tmp$vr)], web$tmp$vr2)

        web$repo$roll_n[web$tmp$vr2] <-
          web$repo$roll_n[web$tmp$vr2] + 1L
      }

      # Update `case_nm` - "Case" records
      if(w.name == "case" & "case_nm" %in% web$repo$group_stats){
        web$tmp$indx <- which(
          web$rec.pairs[[w.name]]$ref_rd |
            web$rec.pairs[[w.name]]$cu_pos %in% web$manual_recursive_index.pos
                              )
        web$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx][
          !web$rec.pairs[[w.name]]$s.match[web$tmp$indx] &
            web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] == 20
        ]
        web$repo$case_nm[web$tgt_pos] <- 0L

        # web$tmp$indx <- which(web$rec.pairs[[w.name]]$ref_rd)
        web$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx][
          web$rec.pairs[[w.name]]$s.match[web$tmp$indx] &
            web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] == 20
        ]
        web$repo$case_nm[web$tgt_pos] <- 4L
      }

      if("case_nm" %in% web$repo$group_stats){
        # Update `case_nm` - Duplicate" records
        web$repo$case_nm[web$rec.pairs[[w.name]]$cu_pos[
          web$rec.pairs[[w.name]]$w.match &
            !web$rec.pairs[[w.name]]$ref_rd &
            is.na(web$repo$case_nm[web$rec.pairs[[w.name]]$cu_pos])
        ]] <- ifelse(w.type == "recurrence", 3L, 2L)
      }

      if(batched == "yes"){
        web$tmp$lgk <-
          web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$ref_rd &
              web$repo$batched[web$rec.pairs[[w.name]]$tr_pos]
          ]
        # web$repo$tag[web$tmp$lgk] <- 1L
        web$repo$batched[web$tmp$lgk] <- FALSE
      }

      # Close if episode type is "fixed"
      if(w.name == "case"){

        web$tmp$indx <- web$rec.pairs[[w.name]]$w.match &
          web$repo$episode_type[web$rec.pairs[[w.name]]$cu_pos] == 1

        if(batched == "yes"){
          # use web$sys.tmp$ite_pos like the rec window?
          web$tmp$indx <- web$tmp$indx &
            (
              (!web$repo$batched[web$rec.pairs[[w.name]]$cu_pos] &
               web$repo$sys.batched[web$rec.pairs[[w.name]]$cu_pos])
             |
              !web$repo$sys.batched[web$rec.pairs[[w.name]]$cu_pos]
             )

        }
        web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[
          web$tmp$indx]] <- 10L
      }

      if(w.name %in% c("recurrence", "case_for_recurrence")){
        if("case_nm" %in% web$repo$group_stats){
          # Update `case_nm` - "Recurrent" index records
          web$rec.pairs[[w.name]]$new_hits <-
            web$rec.pairs[[w.name]]$is.rec_rd <-
            !web$rec.pairs[[w.name]]$cu_pos %in% web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$ref_rd] &
            web$rec.pairs[[w.name]]$w.match

          web$rec.pairs[[w.name]]$is.rec_rd[
            web$rec.pairs[[w.name]]$new_hits
          ] <- !duplicated(web$rec.pairs[[w.name]]$tr_pos[web$rec.pairs[[w.name]]$new_hits])

          if(web$controls$use_events_as_refs){
            web$repo$rec_rd <- rep(NA_real_, web$counts$dataset.n)
            web$repo$rec_rd[
              web$rec.pairs[[w.name]]$tr_pos[web$rec.pairs[[w.name]]$is.rec_rd]
            ] <- web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$is.rec_rd]
            web$rec.pairs[[w.name]]$rec_rd <- web$repo$rec_rd[
              web$rec.pairs[[w.name]]$tr_pos
            ]
            web$rec.pairs[[w.name]]$is.rec_rd[web$rec.pairs[[w.name]]$new_hits] <-
              overlap(
                x = web$repo$date[web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$new_hits]],
                y = web$repo$date[web$rec.pairs[[w.name]]$rec_rd[web$rec.pairs[[w.name]]$new_hits]]
              )
          }
          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$is.rec_rd &
              !web$rec.pairs[[w.name]]$s.match
              ]

          web$repo$case_nm[web$tmp$tgt_pos] <- 1L

          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$is.rec_rd &
              web$rec.pairs[[w.name]]$s.match
          ]
          web$repo$case_nm[web$tmp$tgt_pos] <- 5L
        }


        # Close records previously flagged for recurrence check.
        if(batched == "semi"){
          web$repo$tag[
            web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] == 1]
          ] <- 10L
        }else{
          web$repo$tag[
            web$sys.tmp$ite_pos[
              web$repo$tag[web$sys.tmp$ite_pos] == 1 &
                web$sys.tmp$ite_pos %in% web$rec.pairs[[w.name]]$cu_pos &
                (
                  (!web$repo$batched[web$sys.tmp$ite_pos] &
                     web$repo$sys.batched[web$sys.tmp$ite_pos])
                  |
                  # !web$repo$sys.batched[web$sys.tmp$ite_pos]
                  !web$repo$sys.batched[web$sys.tmp$ite_tr_pos]
                )
            ]] <- 10L
        }

      }

      if(web$controls$use_skip_b4_len){
        # close if `skip_if_b4_lengths`
        web$repo$tag[
          web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$sk_checks
          ]
        ] <- 10L

        if("case_nm" %in% web$repo$group_stats){
          web$repo$case_nm[
            web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$sk_checks
            ]
          ] <- -1L
        }

      }


      if(web$controls$use_recurrence){
        if(web$controls$use_rolls_max){

          # End recurrence when rolls_max is reached
          web$tmp$tr_pos <- web$rec.pairs[[w.name]]$tr_pos[web$rec.pairs[[w.name]]$w.match]
          web$tmp$ld_pos <- web$rec.pairs[[w.name]]$ld_pos[web$rec.pairs[[w.name]]$w.match]
          web$tmp$cu_pos <- web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]

          web$tmp$lgk <- (web$repo$roll_n[web$tmp$tr_pos] == web$repo$rolls_max[web$tmp$tr_pos] &
                            !web$repo$case_for_recurrence[web$tmp$ld_pos]
          ) |
            (web$repo$roll_n[web$tmp$tr_pos] == web$repo$rolls_max[web$tmp$tr_pos] * 2 &
               web$repo$case_for_recurrence[web$tmp$ld_pos]
            ) |
            web$repo$rolls_max[web$tmp$tr_pos] == 0

          web$tmp$lgk <- web$tmp$lgk & web$repo$last.batched[web$tmp$tr_pos]

          # Flag strata with no more recurrence
          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cri %in%
            web$rec.pairs[[w.name]]$cri[web$rec.pairs[[w.name]]$w.match][web$tmp$lgk]
          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$tgt_pos]
          web$tmp$tgt_pos <- web$tmp$tgt_pos[!duplicated(web$tmp$tgt_pos)]

          if(web$controls$use_episodes_max){
            # Update episode counter
            web$repo$epid_n[web$tmp$tgt_pos] <- web$repo$epid_n[web$tmp$tgt_pos] + .5
          }
          # Restart recurrence counter
          # web$repo$rxt_n[web$tmp$tgt_pos] <-
            web$repo$roll_n[web$tmp$tgt_pos] <-
            0L
          # Close matched records to stop recurrence
          # web$repo$tag[web$tmp$cu_pos[web$tmp$lgk]] <- 10L
            web$repo$tag[web$tmp$tgt_pos][
              web$tmp$tgt_pos %in% web$tmp$cu_pos[web$tmp$lgk] |
              web$repo$tag[web$tmp$tgt_pos] == 1
            ] <- 10L
        }

        # Flag next batched to be checked for recurrence
        web$tmp$indx <-
          web$rec.pairs[[w.name]]$w.match &
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] %in% 2:3 &
          web$repo$tag[web$rec.pairs[[w.name]]$cu_pos] == 20 &
          !web$repo$batched[web$rec.pairs[[w.name]]$cu_pos]

        web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] <- 1L
      }

      # print(as.data.frame(web$rec.pairs[[w.name]][c("cu_pos", "tr_pos")]))
    }

    if(TRUE){
      web$tmp$lgk <- web$repo$iteration[web$sys.tmp$ite_pos] == 0 &
        web$repo$tag[web$sys.tmp$ite_pos] != 20
    }else{
      web$tmp$lgk <- web$repo$iteration[web$sys.tmp$ite_pos] == 0 &
        web$repo$tag[web$sys.tmp$ite_pos] == 10
    }
    web$repo$iteration[web$sys.tmp$ite_pos[web$tmp$lgk]] <- ite
    web$tmp$ite.linked.n <- length(which(web$tmp$lgk))
    web$tmp$ite.tot.n <- length(web$sys.tmp$ite_pos)

    if(grepl("^progress", web$controls$display)){
      web$msg <- progress_bar(
        n = length(web$repo$tag[web$repo$tag == 10]),
        d = web$count$dataset.n,
        max_width = 100,
        msg = paste0("Iteration ",
                     fmt(ite), " (",
                     fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                     ")"),
        prefix_msg = "")
      cat(web$msg, "\r", sep = "")
    }else if (grepl("^stats", web$controls$display)){
      web$msg <- update_text(
        tot_records = fmt(web$count$dataset.n),
        current_tot = fmt(web$tmp$ite.tot.n),
        current_tagged = fmt(web$tmp$ite.linked.n),
        time = fmt(Sys.time() - web$tm_ia, "difftime"),
        #iteration = ite,
        indent_txt = ""
      )
      cat(web$msg, "\n", sep = "")
    }
    if(grepl("report$", web$controls$display)){
      web$rp_data <- di_report(
        cumm_time = Sys.time() - web$tm_a,
        duration = Sys.time() - web$tm_ia,
        iteration = ite,
        current_tagged = web$tmp$ite.linked.n,
        current_tot = web$tmp$ite.tot.n,
        memory_used =  utils::object.size(web[names(web)[names(web) != "report"]])
      )
      web$report[length(web$report) + 1] <- list(web$rp_data)
    }

    # print(paste0("Ite. ", ite, " and tr ",  unique(web$sys.tmp$ite_tr_pos), " - ", w.name))
    # print(as.data.frame(web$repo[c("pr_sn", "date", "tag", "case_nm",
    # "iteration", "ite.window", "last.batched", "sys.batched",
    # "batched", "epid", "roll_n", "epid_n")]) %>% arrange(date.start, date.end))

    web$tm_ia <- Sys.time()
    ite <- ite + 1L
    nms_z <- names(web)
    web$tmp <- web$rec.pairs <- NULL
  }

  web$tmp <- web$rec.pairs <- NULL
  web$tmp$tgt_pos <- web$repo$pr_sn[web$repo$epid > 0]

  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm[web$tmp$tgt_pos] <- -1L
  }

  web$repo$iteration[web$tmp$tgt_pos] <- ite - 1L
  # web$tmp$tgt_pos <- web$repo$pr_sn[web$repo$epid < 0]

  if(all(length(web$tmp$tgt_pos) > 0 & "wind" %in% web$repo$group_stats)){
    web$tmp$tgt_pos <- index_multiples(
      web$tmp$tgt_pos,
      multiples = web$counts$dataset.n,
      repeats = web$counts$max_indexes
    )

    web$repo$wind_nm[web$tmp$tgt_pos$mm] <- -1L
    web$repo$wind_id[web$tmp$tgt_pos$mm] <- web$repo$pr_sn[web$tmp$tgt_pos$mi]
  }

  if(!grepl("^none", web$controls$display)){
    cat("\n")
  }

  web$repo$epid <- abs(web$repo$epid)
  web$epids <- make_episodes(
    y_pos = web$repo$epid,
    date = (
      if("epid_interval" %in% web$repo$group_stats){
        web$repo$date
      }else{
        NULL
      }),
    x_pos = web$repo$pr_sn,
    x_val = web$repo$sn,
    iteration = web$repo$iteration,
    wind_id = web$repo$wind_id,
    options = options_lst,
    case_nm = web$repo$case_nm,
    wind_nm = web$repo$wind_nm,
    episode_unit = names(diyar::episode_unit)[web$repo$episode_unit],
    data_source = web$repo$data_source,
    data_links = web$repo$data_links,
    from_last = web$repo$from_last)

  if(grepl("report$", web$controls$display)){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_ia,
      iteration = "End",
      current_tot = web$counts$dataset.n,
      memory_used =  utils::object.size(web[names(web)[names(web) != "report"]])
    )
    web$report[length(web$report) + 1] <- list(web$rp_data)
  }
  if(grepl("report$", web$controls$display)){
    web$epids <- list(epid = web$epids,
                      report = as.list(do.call("rbind", lapply(web$report, as.data.frame))))
    class(web$epids$report) <- "d_report"
  }
  tms <- difftime(Sys.time(), web$tm_a)
  if(!grepl("^none", web$controls$display)){
    cat("Episodes tracked in ", fmt(tms, "difftime"), "!\n", sep = "")
  }
  if(length(web$export) > 0){
    if(inherits(web$epids, "list")){
      web$epids <- c(web$epids, web["export"])
    }else{
      web$epids <- list(epid = web$epids, export = web$export)
    }
  }
  web <- web$epids
  return(web)
}

#' @rdname episodes
#' @export
links_wf_episodes <- function(date,
                              case_length = Inf,
                              episode_type = "fixed",
                              strata = NULL,
                              sn = NULL,
                              display = "none"){
  is_recurisve <-  episode_type != "fixed"
  recurisve <- ifelse(is_recurisve, "unlinked",  "none")
  check_duplicates <- !isTRUE(is_recurisve)
  if(length(strata) == 0){
    strata <- "p1"
  }

  f1 <- function(x, y, l = case_length){
    (x - y) <= l
  }

  if(isTRUE(is_recurisve)){
    f2 <- function(x, y, l = case_length){
      lgk <- f1(x = x, y = y, l = l)
      lgk[lgk] <- duplicated(y[lgk], fromLast = TRUE)
      lgk
    }
  }else{
    f2 <- exact_match
  }

  episodes.scri <- sub_criteria(
    date = date,
    match_funcs = c("f1" = f1),
    equal_funcs = c("f2" = f2)
  )

  links(
    sn = sn,
    criteria = strata,
    recursive = recurisve,
    check_duplicates = check_duplicates,
    sub_criteria = list(cr1 = episodes.scri),
    tie_sort = date,
    batched = "yes",
    permutations_allowed = FALSE,
    repeats_allowed = FALSE,
    ignore_same_source = FALSE,
    display = display)
}

#' @rdname episodes
#' @export
episodes_af_shift <- function(date, case_length = Inf, sn = NULL,
                              strata = NULL, group_stats = FALSE,
                              episode_type = "fixed", data_source = NULL,
                              episode_unit = "days",
                              data_links = "ANY",
                              display = "none"){
  web <- list()
  web$tm_a <- Sys.time()
  dataset.n <- length(date)
  roll_first <- TRUE
  date <- as.number_line(date)
  web$controls$is_dt <- ifelse(
    inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt")),
    TRUE, FALSE)
  if(isTRUE(web$controls$is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  if(!web$controls$is_dt){
    episode_unit <- "seconds"
  }

  web$controls$display <- display
  web$repo$pr_sn <- seq_len(length(date@start))
  web$repo$date <- date
  web$controls$case_length <- diyar::episode_unit[[episode_unit]] * case_length

  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))
  data_links <- data_links
  web$repo$rw.strata <- strata


  unq.pr_sn.0 <- integer()
  lgk <- is.na(web$repo$date@start) |
    is.na(web$repo$date@.Data) |
    !is.finite(web$repo$date@start) |
    !is.finite(web$repo$date@.Data)

  if(any(lgk)){
    unq.pr_sn.0 <- web$repo$pr_sn[lgk]
    web$repo <- lapply(web$repo, function(x) x[!lgk])
  }

  unq.pr_sn.1 <- integer()
  if(length(web$repo$rw.strata) > 1){
    lgk <- (!duplicated(web$repo$rw.strata, fromLast = TRUE) &
      !duplicated(web$repo$rw.strata, fromLast = FALSE)) |
      is.na(web$repo$rw.strata)

    if(any(lgk)){
      unq.pr_sn.1 <- web$repo$pr_sn[lgk]
      web$repo <- lapply(web$repo, function(x) x[!lgk])
    }
  }

  web$repo$tmp.dt_a <- web$repo$dt_a <- as.numeric(web$repo$date@start)
  web$repo$tmp.dt_z <- web$repo$dt_z <- as.numeric(right_point(web$repo$date))
  web$repo$tmp.wind_z <- web$repo$wind_z <- web$repo$dt_z + web$controls$case_length

  # Sort records by strata and ascending order of event periods
  if(length(web$repo$rw.strata) > 1){
    web$repo$strata_cd <- match(
      web$repo$rw.strata,
      web$repo$rw.strata[!duplicated(web$repo$rw.strata)])
  }else{
    web$repo$strata_cd <- rep(1L, length(web$repo$pr_sn))
  }
  web$repo$s_ord <- order(web$repo$strata_cd, web$repo$dt_a, -web$repo$dt_z)
  web$repo <- lapply(web$repo, function(x) x[web$repo$s_ord])

  if((episode_type == "fixed" & roll_first) | episode_type == "rolling"){
    # Lead and lag records indexes
    lead.pos <- 2:length(web$repo$dt_a)
    lag.pos <- 1:(length(web$repo$dt_a)-1)

    if(length(web$repo$rw.strata) > 1){
      # Make the identical dates in subsequent strata larger,
      # but still relatively the same compared to other dates in the same strata
      RNG <- range(c(web$repo$dt_a, web$repo$wind_z[is.finite(web$repo$wind_z)]))
      faC <- as.integer(log10(RNG[[2]] - RNG[[1]])) + 1
      faC <- 10 ^ faC

      web$repo$tmp.strata <- web$repo$strata_cd * faC

      web$repo$tmp.dt_a <- web$repo$dt_a - RNG[[1]]
      web$repo$tmp.dt_a <- web$repo$tmp.strata + web$repo$dt_a

      web$repo$tmp.wind_z <- web$repo$wind_z - RNG[[1]]
      web$repo$tmp.wind_z <- web$repo$tmp.strata + web$repo$wind_z

      web$repo$Nxt.strata <-
        web$repo$strata[c(lead.pos, NA)]
      web$repo$Prv.strata <-
        web$repo$strata_cd[c(NA, lag.pos)]

      end_strata_lgk <- web$repo$strata_cd != web$repo$Nxt.strata
    }else{
      end_strata_lgk <- FALSE
    }

    web$repo$Nxt.wind_a <-
      web$repo$tmp.dt_a[c(lead.pos, NA)]

    web$repo$cumEnd <- cummax(as.numeric(web$repo$tmp.wind_z))

    lgk <- end_strata_lgk | is.na(web$repo$Nxt.wind_a)
    web$repo$Nxt.wind_a[lgk] <-
      web$repo$tmp.dt_a[lgk]

    web$repo$change_lgk <- (web$repo$Nxt.wind_a > web$repo$cumEnd)
    web$repo$change_lgk <- web$repo$change_lgk[c(NA, lag.pos)]

    if(length(web$repo$rw.strata) > 1){
      # Start of new strata
      web$repo$new_strata_lgk <-
        web$repo$strata != web$repo$Prv.strata
      web$repo$new_strata_lgk <-
        which(web$repo$new_strata_lgk | is.na(web$repo$new_strata_lgk))
    }else{
      web$repo$new_strata_lgk <- 1
    }
    web$repo$change_lgk[web$repo$new_strata_lgk] <- TRUE
    web$repo$Cummchange_lgk <- cumsum(web$repo$change_lgk)

    web$repo$epid <- web$repo$Cummchange_lgk + 1
  }else{
    web$repo$epid <- web$repo$strata_cd
  }

  unq.pr_sn.2 <- integer()
  if(episode_type == "fixed"){
    web$repo <- web$repo[c("pr_sn", "date", "dt_a", "dt_z", "wind_z", "strata_cd", "epid")]

    web$repo$iteration <- rep(ifelse(roll_first, 1L, 0L) , length(web$repo$pr_sn))

    if(roll_first){
      if(length(web$repo$epid[!duplicated(web$repo$epid)]) > 1){
        lgk <- !duplicated(web$repo$epid, fromLast = TRUE) &
          !duplicated(web$repo$epid, fromLast = FALSE)

        if(any(lgk)){
          unq.pr_sn.2 <- web$repo$pr_sn[lgk]
          web$repo <- lapply(web$repo, function(x) x[!lgk])
        }
      }

      web$repo$tmp.strata <- match(
        web$repo$epid,
        web$repo$epid[!duplicated(web$repo$epid)])
    }else{
      web$repo$tmp.strata <- web$repo$strata_cd
    }

    web$repo$rec_pos <- seq_len(length(web$repo$tmp.strata))
    faC <- as.integer(log10(max(web$repo$rec_pos, na.rm = FALSE))) + 1
    faC <- 10 ^ faC

    web$repo$tmp.strata <- ((max(web$repo$tmp.strata, na.rm = FALSE) + 1)) - web$repo$tmp.strata
    web$repo$tmp.strata <- web$repo$tmp.strata * faC

    web$repo$tmp.rec_pos <- web$repo$tmp.strata + web$repo$rec_pos

    i <- 1L
    tm_ia <- Sys.time()
    while(any(is.finite(web$repo$tmp.rec_pos))){
      ite_lgk <- web$repo$tmp.rec_pos != Inf
      web$repo$ld.rec_pos[ite_lgk] <-
        abs(cummax(-web$repo$tmp.rec_pos[ite_lgk])) - web$repo$tmp.strata[ite_lgk]

      web$repo$lgk[ite_lgk] <- (web$repo$wind_z[web$repo$ld.rec_pos[ite_lgk]] >= web$repo$dt_a[ite_lgk])
      web$repo$lgk[ite_lgk] <- web$repo$lgk[ite_lgk] & !is.na(web$repo$lgk[ite_lgk])

      web$repo$epid[ite_lgk][web$repo$lgk[ite_lgk]] <- web$repo$ld.rec_pos[ite_lgk][web$repo$lgk[ite_lgk]]
      web$repo$tmp.rec_pos[ite_lgk][web$repo$lgk[ite_lgk]] <- Inf
      web$repo$iteration[ite_lgk][web$repo$lgk[ite_lgk]] <- i

      if(!grepl("^none", web$controls$display)){
        ite.linked.n <- length(which(web$repo$lgk[ite_lgk]))
        ite.tot.n <- length(which(ite_lgk)) + ite.linked.n
        cri.linked.n <- length(which(!ite_lgk))

        if(grepl("^progress", web$controls$display)){
          msg <- progress_bar(
            n = dataset.n - (ite.tot.n - ite.linked.n),
            d = dataset.n,
            max_width = 100,
            msg = paste0("Iteration ",
                         fmt(i + 1L), " (",
                         fmt(difftime(Sys.time(), tm_ia), "difftime"),
                         ")"),
            prefix_msg = "")
          cat(msg, "\r", sep = "")
        }else if (grepl("^stats", web$controls$display)){
          web$msg <- update_text(
            tot_records = fmt(dataset.n),
            current_tot = fmt(ite.tot.n),
            current_tagged = fmt(ite.linked.n),
            time = fmt(Sys.time() - tm_ia, "difftime"),
            # iteration = ite,
            indent_txt = ""
          )
          cat(msg, "\n", sep = "")
        }
        tm_ia <- Sys.time()
      }

      i <- i + 1L
    }

    web$repo$epid <- combi(web$repo$tmp.strata, web$repo$epid)
    web$repo$iteration <- web$repo$iteration + 1L

  }else{
    web$repo$iteration <- rep(1L, length(date))
  }

  if(FALSE){
    # Match the default preference for case records used in `episodes()`
    case_s_ord <- order(web$repo$dt_a, -as.numeric(web$repo$dt_z), web$repo$pr_sn)
    web$repo <- lapply(web$repo[c("pr_sn", "epid", "iteration")],
                       function(x) x[case_s_ord])
  }

  web$repo <- list(
    pr_sn = c(web$repo$pr_sn, unq.pr_sn.0, unq.pr_sn.1, unq.pr_sn.2),
    group_id = c(-web$repo$epid, unq.pr_sn.0, unq.pr_sn.1, unq.pr_sn.2),
    iteration = c(web$repo$iteration,
                  rep(0L, length(unq.pr_sn.0)),
                  rep(0L, length(unq.pr_sn.1)),
                  rep(ifelse(roll_first, 1L, 0L), length(unq.pr_sn.2)))
  )

  # Familiar IDs
  indx <- which(!duplicated(web$repo$group_id))
  web$repo$group_id <- web$repo$pr_sn[indx][
    match(web$repo$group_id, web$repo$group_id[indx])
    ]

  # Re-order as entered
  web$s_ord <- order(web$repo$pr_sn)
  web$repo <- lapply(web$repo, function(x){
    x[web$s_ord]
  })

  date <- as.number_line(date)
  web$repo$case_nm <- !(web$repo$pr_sn %in% web$repo$group_id)
  web$repo$case_nm[web$repo$case_nm] <-
    ifelse(episode_type == "rolling", 3L, 2L)

  web$repo$wind_nm <- rep(
    ifelse(episode_type == "rolling", 1L, 0L),
    length(web$repo$group_id)
  )

  web$epids <- make_episodes(
    y_pos = web$repo$group_id,
    date = (
      if(group_stats){
        date
      }else{
        NULL
      }),
    x_pos = web$repo$pr_sn,
    x_val = sn,
    iteration = web$repo$iteration,
    wind_id = web$repo$group_id,
    # options = options_lst,
    case_nm = web$repo$case_nm,
    wind_nm = web$repo$wind_nm,
    episode_unit = episode_unit,
    data_source = data_source,
    data_links = data_links)

  tms <- difftime(Sys.time(), web$tm_a)
  if(!grepl("^none", web$controls$display)){
    cat("\nEpisodes tracked in ", fmt(tms, "difftime"), "!\n", sep = "")
  }
  web <- web$epids
  return(web)
}

#'
#' @name episodes_wf_splits
#' @title Link events to chronological episodes.
#'
#' @description \code{episodes_wf_splits} is a wrapper function of \code{\link{episodes}}.
#' It's designed to be more efficient with larger datasets.
#' Duplicate records which do not affect the case definition are excluded prior to episode tracking.
#' The resulting episode identifiers are then recycled for the duplicate records.
#'
#' @param ... Arguments passed to \code{\link{episodes}}.
#' @param duplicates_recovered \code{[character]}. Determines which duplicate records are recycled.
#' Options are \code{"ANY"} (default), \code{"without_sub_criteria"}, \code{"with_sub_criteria"} or \code{"ALL"}. See \code{Details}.
#' @param reframe \code{[logical]}. Determines if the duplicate records in a \code{\link{sub_criteria}} are reframed (\code{TRUE}) or excluded (\code{FALSE}).
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{episodes}}; \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{episodes_wf_splits()}} reduces or re-frames a dataset to
#' the minimum datasets required to implement a case definition.
#' This leads to the same outcome but with the benefit of a shorter processing time.
#'
#' The \code{duplicates_recovered} argument determines which identifiers are recycled.
#' Selecting the \code{"with_sub_criteria"} option will force only identifiers created resulting from a matched \code{\link{sub_criteria}} (\code{"Case_CR"} and \code{"Recurrent_CR"}) are recycled.
#' However, if \code{"without_sub_criteria"} is selected then only identifiers created that do not result from a matched \code{\link{sub_criteria}} (\code{"Case"} and \code{"Recurrent"}) are recycled
#' Excluded duplicates of \code{"Duplicate_C"} and \code{"Duplicate_R"} are always recycled.
#'
#' The \code{reframe} argument will either \code{\link{reframe}} or subset a \code{\link{sub_criteria}}.
#' Both will require slightly different functions for \code{match_funcs} or \code{equal_funcs}.
#'
#' @examples
#' # With 2,000 duplicate records of 20 events,
#' # `episodes_wf_splits()` will take less time than `episodes()`
#' dates <- seq(from = as.Date("2019-04-01"), to = as.Date("2019-04-20"), by = 1)
#' dates <- rep(dates, 2000)
#'
#' system.time(
#'   ep1 <- episodes(dates, 1)
#' )
#' system.time(
#'   ep2 <- episodes_wf_splits(dates, 1)
#' )
#'
#' # Both leads to the same outcome.
#' all(ep1 == ep2)

#' @export
episodes_wf_splits <- function(..., duplicates_recovered = "ANY", reframe = FALSE){
  tm_a <- Sys.time()

  # Validations
  errs <- err_episodes_checks_0(...)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  duplicates_recovered <- tolower(duplicates_recovered)

  opt_lst <- list(...)
  def_list <- formals(episodes)
  named_pos <- which(names(opt_lst) %in% names(def_list))
  unamed_pos <- seq_len(length(opt_lst))[!seq_len(length(opt_lst)) %in% named_pos]
  unnamed_args <- names(def_list)[!names(def_list) %in% names(opt_lst)]
  unnamed_args <- unnamed_args[unamed_pos]
  names(opt_lst)[unamed_pos] <- unnamed_args

  opt_lst <- c(
    opt_lst,
    def_list[names(def_list)[!names(def_list) %in% names(opt_lst)]]
  )
  rm(def_list, unnamed_args, named_pos, unamed_pos)

  if(is.null(opt_lst$sn)) {
    opt_lst$sn <- seq_len(length(opt_lst$date))
  }
  sn <- opt_lst$sn
  display <- opt_lst$display

  combi_opt_lst <- opt_lst[names(opt_lst) != "sn"]
  check_lens <- function(x){
    if(inherits(x, "sub_criteria")){
      max(attr_eval(x))
    }else if(inherits(x, "list")){
      max(as.numeric(lapply(x, length)))
    }else {
      length(x)
    }
  }
  args_len <- unlist(lapply(combi_opt_lst, check_lens), use.names = FALSE)
  combi_opt_lst <- combi_opt_lst[args_len > 1]

  if(length(combi_opt_lst) != 0){
    combi_opt_lst <- lapply(combi_opt_lst, function(x){
      if(inherits(x, "sub_criteria")){
        attr_eval(x, func = identity, simplify = FALSE)
      }else if(inherits(x, "list")){
        x
      }else{
        list(x)
      }
    })

    combi_opt_lst <- unlist(combi_opt_lst, recursive = FALSE, use.names = FALSE)
    combi_opt_lst <- lapply(combi_opt_lst, function(x){
      if(inherits(x, "number_line")){
        list(x@start, x@.Data)
      }else{
        x
      }
    })
    # sub_criteria with d_attributes/number_line objects
    lgk <- unlist(lapply(combi_opt_lst, function(x) inherits(x, "list")), use.names = FALSE)
    combi_opt_lst <- c(combi_opt_lst[!lgk],
                       unlist(combi_opt_lst[lgk], recursive = FALSE, use.names = FALSE))

    cmbi_cd <- combi(combi_opt_lst)
    rf_lgk <- duplicated(cmbi_cd)
  }else{
    rf_lgk <- cmbi_cd <- rep(TRUE, length(date))
    rf_lgk[1] <- FALSE
  }

  opt_lst_nms <- names(opt_lst)
  opt_lst <- lapply(seq_len(length(opt_lst)), function(i){
    if(inherits(opt_lst[[i]], "name")) {
      return(
        opt_lst[[as.character(opt_lst[[i]])]]
      )
    }else{
      return(
        opt_lst[[i]]
      )
    }
  })
  opt_lst <- lapply(seq_len(length(opt_lst)), function(i){
    if(inherits(opt_lst[[i]], "sub_criteria")) {
      if(reframe){
        return(reframe(opt_lst[[i]], func = function(x) split(x, cmbi_cd)))
      }else{
        return(reframe(opt_lst[[i]], func = function(x) x[!rf_lgk]))
      }
    }else if(inherits(opt_lst[[i]], "list")){
      return(lapply(opt_lst[[i]], function(x){
        if(length(x) == 1){
          x
        }else{
          x[!rf_lgk]
        }
      }))
    }else if(length(opt_lst[[i]]) %in% 0:1 | opt_lst_nms[i] %in% "data_links"){
      return(opt_lst[[i]])
    }else{
      return((opt_lst[[i]])[!rf_lgk])
    }
  })
  names(opt_lst) <- opt_lst_nms
  if(!display %in% c("none")){
    rp_data <- di_report(duration = Sys.time() - tm_a,
                         iteration = "Remove duplicates",
                         current_tot = length(rf_lgk),
                         current_skipped = length(rf_lgk[rf_lgk]))
    report_a <- rp_data
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0("Remove duplicates\n",
                 "Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                 "Skipped: ", fmt(rp_data$records_skipped), " record(s)","\n",
                 "Time: ", fmt(rp_data$duration, "difftime"),
                 "\n\n"))
    }
  }

  epids <- episodes(sn = opt_lst$sn, date = opt_lst$date,
                    case_length = opt_lst$case_length,
                    strata = opt_lst$strata,
                    display = opt_lst$display, episodes_max = opt_lst$episodes_max,
                    from_last = opt_lst$from_last, episode_unit = opt_lst$episode_unit,
                    case_overlap_methods = opt_lst$case_overlap_methods,
                    recurrence_overlap_methods = opt_lst$recurrence_overlap_methods,
                    skip_order = opt_lst$skip_order, custom_sort = opt_lst$custom_sort, group_stats = opt_lst$group_stats,
                    data_source = opt_lst$data_source, data_links = opt_lst$data_links,
                    skip_if_b4_lengths = opt_lst$skip_if_b4_lengths,
                    rolls_max = opt_lst$rolls_max, case_for_recurrence = opt_lst$case_for_recurrence,
                    reference_event = opt_lst$reference_event,
                    episode_type = opt_lst$episode_type, recurrence_length = opt_lst$recurrence_length,
                    case_sub_criteria = opt_lst$case_sub_criteria,
                    recurrence_sub_criteria = opt_lst$recurrence_sub_criteria,
                    case_length_total = opt_lst$case_length_total,
                    recurrence_length_total = opt_lst$recurrence_length_total,
                    skip_unique_strata = FALSE)

  tm_a <- Sys.time()
  rp_lgk <- match(cmbi_cd, cmbi_cd[!rf_lgk])
  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    wf_epid <- epids$epid[rp_lgk]
  }else{
    wf_epid <- epids[rp_lgk]
  }

  wf_epid@sn <- sn
  wf_epid@case_nm[((wf_epid@case_nm %in% c(0, 4) & duplicates_recovered == "any") |
                     (wf_epid@case_nm == 0 & duplicates_recovered == "without_sub_criteria") |
                     (wf_epid@case_nm == 4 & duplicates_recovered == "with_sub_criteria")) & rf_lgk] <- 2
  wf_epid@case_nm[((wf_epid@case_nm %in% c(1, 5) & duplicates_recovered == "all") |
                     (wf_epid@case_nm == 1 & duplicates_recovered == "without_sub_criteria") |
                     (wf_epid@case_nm == 5 & duplicates_recovered == "with_sub_criteria")) & rf_lgk] <- 3
  lgk <- which(wf_epid@case_nm == -1 | (wf_epid@case_nm %in% c(0, 1, 4, 5) & rf_lgk))
  if(length(lgk) > 0){
    wf_epid@.Data[lgk] <- wf_epid@sn[lgk]
    wf_epid@wind_id <- lapply(wf_epid@wind_id, function(x){
      x[lgk] <- wf_epid@sn[lgk]
      return(x)
    })
  }
  tot <- rle(sort(wf_epid@.Data[!seq_len(length(wf_epid)) %in% lgk]))
  wf_epid@epid_total <- tot$lengths[match(wf_epid@.Data, tot$values)]
  wf_epid@epid_total[is.na(wf_epid@epid_total)] <- 1L
  if(!display %in% c("none")){
    rp_data <- di_report(duration = Sys.time() - tm_a,
                         iteration = "Return duplicates",
                         current_tot = length(date),
                         current_tagged = length(rf_lgk[rf_lgk]))
    report_b <- rp_data
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0("\n\n",
                 "Return duplicates\n",
                 "Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                 "Assigned: ", fmt(rp_data$records_tracked), " record(s)","\n",
                 "Time: ", fmt(rp_data$duration, "difftime"),
                 "\n\n"))
    }
  }
  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    report <- rbind(as.data.frame(report_a),
                    as.data.frame(epids$report),
                    as.data.frame(report_b))
    report <- as.list(report)
    class(report) <- "d_report"
    wf_epid <- list(epid = wf_epid,
                    report = report)
  }

  rm(list = ls()[ls() != "wf_epid"])
  return(wf_epid)
}

#' @name windows
#' @aliases windows
#' @title Windows and lengths
#'
#' @param date As used in \bold{\code{\link{episodes}}}.
#' @param lengths The duration (\code{lengths}) between a \code{date} and \code{window}.
#' @param windows The range (\code{windows}) relative to a \code{date} for a given duration (\code{length}).
#' @param episode_unit Time unit of \code{lengths}. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years". See \code{diyar::episode_unit}
#' @param from_last As used in \bold{\code{\link{episodes}}}.
#' @description Covert \code{windows} to and from \code{case_lengths} and \code{recurrence_lengths}.
#'
#' @details
#' \bold{\code{epid_windows}} - returns the corresponding \code{window} for a given a \code{date}, and \code{case_length} or \code{recurrence_length}.
#'
#' \bold{\code{epid_lengths}} - returns the corresponding \code{case_length} or \code{recurrence_length} for a given \code{date} and \code{window}.
#'
#' \bold{\code{index_window}} - returns the corresponding \code{case_length} or \code{recurrence_length} for the \code{date} only.
#'
#' \bold{\code{index_window(date = x)}} is a convenience function for \bold{\code{epid_lengths(date = x, window = x)}}.
#'
#' @return \code{\link{number_line}}.
#'
#' @examples
#' # Which `window` will a given `length` cover?
#' date <- Sys.Date()
#' epid_windows(date, 10)
#' epid_windows(date, number_line(5, 10))
#' epid_windows(date, number_line(-5, 10))
#' epid_windows(date, -5)
#'
#' @export
epid_windows <- function(date, lengths, episode_unit = "days"){
  date <- as.number_line(date)
  if(!inherits(lengths, "number_line")){
    lengths <- number_line(0, as.numeric(lengths))
  }
  is_dt <- ifelse(!inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
  if(isTRUE(is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start, tz = "GMT"),
      r = as.POSIXct(right_point(date), tz = "GMT")
    )
  }

  number_line(right_point(date) + (lengths@start * as.numeric(diyar::episode_unit[episode_unit])),
              right_point(date) + (right_point(lengths) * as.numeric(diyar::episode_unit[episode_unit])))
}

#' @rdname windows
#' @examples
#'
#' # Which `length` is required to cover a given `window`?
#' date <- number_line(Sys.Date(), Sys.Date() + 20)
#' epid_lengths(date, Sys.Date() + 30)
#' epid_lengths(date, number_line(Sys.Date() + 25, Sys.Date() + 30))
#' epid_lengths(date, number_line(Sys.Date() - 10, Sys.Date() + 30))
#' epid_lengths(date, Sys.Date() - 10)
#' @export
epid_lengths <- function(date, windows, episode_unit = "days"){
  date <- as.number_line(date)
  windows <- as.number_line(windows)
  is_dt1 <- ifelse(!inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
  if(isTRUE(is_dt1)){
    date <- number_line(
      l = as.POSIXct(date@start, tz = "GMT"),
      r = as.POSIXct(right_point(date), tz = "GMT")
    )
  }
  is_dt2 <- ifelse(!inherits(windows@start, c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
  if(isTRUE(is_dt2)){
    windows <- number_line(
      l = as.POSIXct(windows@start, tz = "GMT"),
      r = as.POSIXct(right_point(windows), tz = "GMT")
    )
  }

  episode_unit[!is_dt1 | !is_dt2] <- "seconds"
  number_line((as.numeric(windows@start) - as.numeric(right_point(date)))/as.numeric(diyar::episode_unit[episode_unit]),
              (as.numeric(right_point(windows)) - as.numeric(right_point(date)))/as.numeric(diyar::episode_unit[episode_unit]))
}

#' @rdname windows
#' @examples
#'
#' # Which `length` is required to cover the `date`?
#' index_window(20)
#' index_window(number_line(15, 20))
#'
#' @export
index_window <- function(date, from_last = FALSE){
  window <- as.number_line(date)
  window <- number_line(-window@.Data, 0)
  window[from_last] <- invert_number_line(window[from_last])
  window
}

#' @name custom_sort
#' @aliases custom_sort
#' @title Nested sorting
#'
#' @param ... Sequence of \code{atomic} vectors. Passed to \bold{\code{\link{order}}}.
#' @param decreasing Sort order. Passed to \bold{\code{\link{order}}}.
#' @param unique If \code{FALSE} (default), ties get the same rank. If \code{TRUE}, ties are broken.
#'
#' @description Returns a sort order after sorting by a vector within another vector.
#'
#' @return \code{numeric} sort order.
#'
#' @examples
#'
#' a <- c(1, 1, 1, 2, 2)
#' b <- c(2, 3, 2, 1, 1)
#'
#' custom_sort(a, b)
#' custom_sort(b, a)
#' custom_sort(b, a, unique = TRUE)
#'
#' @export
custom_sort <- function(..., decreasing = FALSE, unique = FALSE){
  ord <- order(order(..., decreasing = decreasing))
  if(!unique){
    ord_l <- combi(...)
    ord <- (ord[!duplicated(ord_l)])[match(ord_l, ord_l[!duplicated(ord_l)])]
    ord <- match(ord, sort(ord[!duplicated(ord)]))
  }
  return(ord)
}
