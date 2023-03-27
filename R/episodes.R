#' @name episodes
#' @title Group dated events into episodes.
#'
#' @description Assign unique identifiers to dated events based on case definitions.
#'
#' @param sn \code{[integer]}. Unique record identifier. Useful in creating familiar \code{\link[=epid-class]{epid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Episodes are created separately for each \code{strata}.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Event date or period.
#' @param case_length \code{[integer|\link{number_line}]}. Duration from index event distinguishing one \code{"Case"} from another.
#' @param episodes_max \code{[integer]}. Maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{[character]}. Options are \code{"fixed"} (default), \code{"rolling"} or \code{"recursive"}. See \code{Details}.
#' @param recurrence_length \code{[integer|\link{number_line}]}. Duration from an event distinguishing a \code{"Recurrent"} event from its index event.
#' @param episode_unit \code{[character]}. Time unit for \code{case_length} and \code{recurrence_length}. Options are "seconds", "minutes", "hours", "days" (default), "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max \code{[integer]}. Maximum number of times an index event recurs. Only used if \code{episode_type} is \code{"rolling"} or \code{"recursive"}.
#' @param data_source \code{[character]}. Source of each record. Used to populate the \code{epid_dataset} slot of the \code{\link[=epid-class]{epid}}.
#' @param from_last \code{[logical]}. Chronological order of episode tracking i.e. ascending (\code{TRUE}) or descending (\code{FALSE}).
#' @param case_overlap_methods \code{[character|integer]}. Specific ways event-periods most overlap with a \code{"Case"} event. See (\code{\link{overlaps}}).
#' @param recurrence_overlap_methods \code{[character|integer]}. Specific ways event-periods most overlap with a \code{"Recurrent"} event. See (\code{\link{overlaps}}).
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index events. See \code{\link{custom_sort}}.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), episode-specific information like episode start and end dates are returned.
#' @param display \code{[character]}. Display or produce a status update. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param reference_event \code{[character]}. Specifies which events are used as index events for a subsequent \code{case_length} or \code{recurrence_length}. Options are \code{"last_record"} (default), \code{"last_event"}, \code{"first_record"} or \code{"first_event"}.
#' @param case_for_recurrence \code{[logical]}. If \code{TRUE}, both \code{"Case"} and \code{"Recurrent"} events will have a \code{case_length}.
#' If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"} or \code{"recursive"}.
#' @param skip_order \code{[integer]}. Skip episodes with an index event that is greater than \code{"n"} sort order of \code{custom_sort}.
#' @param data_links \code{[list|character]}. \code{data_source} required in each \code{\link[=epid-class]{epid}}. An episode without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param skip_if_b4_lengths \code{[logical]}. If \code{TRUE} (default), events before a lagged \code{case_length} or \code{recurrence_length} are skipped.
#' @param skip_unique_strata \code{[logical]}. If \code{TRUE}, a strata with a single event is skipped.
#' @param case_sub_criteria \code{[\link{sub_criteria}]}. Nested matching criteria for events in a \code{case_length}.
#' @param recurrence_sub_criteria \code{[\link{sub_criteria}]}. Nested matching criteria for events in a \code{recurrence_length}.
#' @param case_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{case_lengths} required for an episode.
#' @param recurrence_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{recurrence_lengths} required for an episode.
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{episodes_wf_splits}}; \code{\link{custom_sort}}; \code{\link{sub_criteria}}; \code{\link[=windows]{epid_length}}; \code{\link[=windows]{epid_window}}; \code{\link{partitions}}; \code{\link{links}}; \code{\link{overlaps}}; \code{\link{number_line}}; \code{\link{links_sv_probabilistic}}; \code{\link{schema}}
#'
#' @details
#' \bold{\code{episodes()}} links dated records (events) that
#' are within a set duration of each other.
#' Every event is linked to a unique group (episode; \code{\link[=epid-class]{epid}} object).
#' These episodes represent occurrences of interest as defined by the case definition specified through function's arguments.
#'
#' By default, this process occurs in ascending order; beginning with the earliest event and proceeding to the most recent one.
#' This can be changed to a descending (\code{from_last}) or custom order (\code{custom_sort}).
#' Ties are always broken by the chronological order of events.
#'
#' In general, three type of episodes are possible;
#' \itemize{
#' \item \code{"fixed"} - An episode where all events are within fixed durations of one index event.
#' \item \code{"rolling"} - An episode where all events are within recurring durations of one index event.
#' \item \code{"recursive"} - An episode where all events are within recurring durations of multiple index events.
#' }
#'
#' Every event in each episode is categorised as one of the following;
#' \itemize{
#' \item \code{"Case"} - Index event of the episode (without a matching \code{\link{sub_criteria}}).
#' \item \code{"Case_CR"} - Index event of the episode (with a matching \code{\link{sub_criteria}}).
#' \item \code{"Duplicate_C"} - Duplicate of the index event.
#' \item \code{"Recurrent"} - Recurrence of the index event (without a matching \code{\link{sub_criteria}}).
#' \item \code{"Recurrent_CR"} - Recurrence of the index event (with a matching \code{\link{sub_criteria}}).
#' \item \code{"Duplicate_R"} - Duplicate of the recurrent event.
#' \item \code{"Skipped"} - Records excluded from the episode tracking process.
#' }
#'
#' If \code{data_links} is supplied, every element of the list must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements are assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, groups without records from every listed \code{data_source} will be unlinked.
#' \item If named \code{"g"}, groups without records from any listed \code{data_source} will be unlinked.
#' }
#'
#' Records with a missing (\code{NA}) \code{strata} are excluded from the episode tracking process.
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
#'          episodes_max = 1)
#'
#' # Mutliple 16-day episodes with an 11-day recurrence period
#' episodes(date = infections$date,
#'          case_length = 15,
#'          recurrence_length = 10,
#'          episode_type = "rolling")
#'
#' # Overlapping episodes of hospital stays
#' hospital_admissions$admin_period <-
#' number_line(hospital_admissions$admin_dt,
#'             hospital_admissions$discharge_dt)
#' episodes(date = hospital_admissions$admin_period,
#'          case_overlap_methods = "inbetween")
#'
#' @aliases episodes
#' @export
episodes <- function(date, case_length = Inf, episode_type = "fixed", recurrence_length = case_length,
                        episode_unit = "days", strata = NULL, sn = NULL, episodes_max = Inf, rolls_max = Inf,
                        case_overlap_methods = 8, recurrence_overlap_methods = case_overlap_methods,
                        skip_if_b4_lengths = FALSE, data_source = NULL,
                        data_links = "ANY", custom_sort = NULL, skip_order = Inf, reference_event = "last_record",
                        case_for_recurrence = FALSE, from_last = FALSE, group_stats = FALSE,
                        display = "none", case_sub_criteria = NULL, recurrence_sub_criteria = case_sub_criteria,
                        case_length_total = 1, recurrence_length_total = case_length_total,
                        skip_unique_strata = TRUE) {
  web <- list(tm_a = Sys.time())
  # Validations
  errs <- err_episodes_checks_0(sn = sn, date = date, case_length = case_length, strata = strata,
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
                     strata = if(class(strata) == "NULL") NULL else encode(strata),
                     case_length = if(class(case_length) != "list") list(case_length) else case_length,
                     recurrence_length = if(class(recurrence_length) != "list") list(recurrence_length) else recurrence_length,
                     episode_unit = episode_unit,
                     from_last = from_last)
  epid_unit <- as.vector(episode_unit)

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

  web$controls$any_recurrence_epi <- any(episode_type %in% c(2, 3))
  web$controls$any_case_for_rec <- any(case_for_recurrence == TRUE)

  # `date`
  date <- as.number_line(date)
  is_dt <- ifelse(!any(class(date@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  if(isTRUE(is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  epid_unit[!is_dt] <- 1L
  # web$mm_opts <- list()
  # `case_length`
  web$mm_opts$case_length <- length_to_range(
    lengths = case_length,
    date = date,
    from_last = from_last,
    episode_unit = epid_unit,
    skip_if_b4_lengths = skip_if_b4_lengths)
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
  web$mm_opts$case_length$range_dt_a <- web$mm_opts$case_length$range@start
  web$mm_opts$case_length$range_dt_z <- right_point(web$mm_opts$case_length$range)

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
      reverse_number_line(case_length_total[case_length_total@.Data < 0], "decreasing")
  }else{
    case_length_total <- number_line(case_length_total, Inf)
  }
  web$controls$use_case_length_total <-
    any(case_length_total@start != 1 & case_length_total@.Data != Inf)

  if(web$controls$any_recurrence_epi){
    # `recurrence_length`
    web$mm_opts$recurrence_length <- length_to_range(
      lengths = recurrence_length,
      date = date,
      from_last = from_last,
      episode_unit = epid_unit,
      skip_if_b4_lengths = skip_if_b4_lengths)
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
      any(recurrence_length_total@start != 1 &
            recurrence_length_total@.Data != Inf)

    attr(recurrence_length_total, "opts") <- attr(recurrence_length, "opts") <-
      class(case_for_recurrence) <-
      "d_lazy_opts"
  }

  # `reference_event`
  if(class(reference_event) == "logical"){
    reference_event[reference_event] <- "last_record"
    reference_event[reference_event != "last_record"] <- "first_record"
  }
  web$controls$any_rec_from_last <-
    any(reference_event %in% c("first_record", "first_event"))
  # `skip_if_b4_lengths`
  web$controls$use_skip_b4_len <- any(skip_if_b4_lengths == TRUE)

  # `d_lazy_opts`
  class(episodes_max) <- class(rolls_max) <- class(skip_order) <-
    class(from_last) <- class(episode_type) <- class(reference_event) <-
    attr(case_length_total, "opts") <-
    class(epid_unit) <- class(skip_if_b4_lengths) <- "d_lazy_opts"

  pr_sn <- seq_len(web$counts$dataset.n)
  if(!is.null(sn)) {
    sn <- as.integer(sn)
  }else{
    sn <- pr_sn
  }

  # System preference for case-assignment
  ord_a <- abs(
    max(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start))
  ord_z <- abs(
    max(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date)))
  ord_a[!from_last] <- abs(
    min(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start[!from_last]))
  ord_z[!from_last] <- abs(
    min(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date[!from_last])))
  assign_ord <- order(order(ord_a, -ord_z, pr_sn))

  # User-defined preference for case-assignment
  if(!is.null(custom_sort)) {
    if(any(!class(custom_sort) %in% c("numeric", "integer", "double"))){
      custom_sort <- as.integer(as.factor(custom_sort))
    }
    if(length(custom_sort) == 1){
      custom_sort <- rep(custom_sort, web$counts$dataset.n)
    }
    # assign_ord <- order(order(custom_sort, assign_ord))
  }else{
    custom_sort <- rep(0L, web$counts$dataset.n)
  }

  if(!is.null(data_source)) {
    if(length(data_source) == 1){
      data_source <- rep(data_source, web$counts$dataset.n)
    }
  }
  web$tm_ia <- Sys.time()
  web$repo$pr_sn <-
    web$repo$epid <- seq_len(web$counts$dataset.n)

  web$repo$c_hits <- web$repo$ld_pos <- web$repo$wind_id <-
    web$repo$wind_nm <- web$repo$case_nm <- rep(NA_real_, web$counts$dataset.n)

  web$repo$cri <- cri
  web$repo$strata <- strata
  web$repo$assign_ord <- assign_ord

  web$repo$cur_refs <- web$repo$max_refs <- web$repo$epid_n <-
    web$repo$iteration <- rep(0L, web$counts$dataset.n)

  web$repo$epid_unit <- epid_unit
  web$repo$group_stats <- group_stats

  web$repo$tag <- rep(20L, web$counts$dataset.n)
  web$repo$reference_event <- reference_event
  web$repo$episode_type <- episode_type

  web$repo$nwEpi <- rep(FALSE, web$counts$dataset.n)

  web$repo$date <- date
  web$repo$case_sub_criteria <- case_sub_criteria
  web$repo$case_length_total <- case_length_total
  web$repo$skip_if_b4_lengths <- skip_if_b4_lengths
  web$repo$episodes_max <- episodes_max

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

  if(web$controls$any_recurrence_epi){
    web$repo$roll_n <-
      web$repo$rxt_n <- rep(0L, web$counts$dataset.n)
    web$repo$recurrence_sub_criteria <- recurrence_sub_criteria
    web$repo$recurrence_length_total <- recurrence_length_total
    web$repo$rolls_max <- rolls_max
    web$repo$case_for_recurrence <- case_for_recurrence
  }

  # User-specified records to skip
  if(!is.null(web$repo$strata)){
    web$tmp$lgk <- is.na(web$repo$strata)
    web$repo$tag[web$tmp$lgk] <- 10L
    web$repo$case_nm[web$tmp$lgk] <- -1L
    web$repo$iteration[web$tmp$lgk] <- 0L
  }

  # Skip events with non-finite `dates`
  web$tmp$lgk <- is.na(web$repo$date@start) | is.na(web$repo$date@.Data)
  web$repo$tag[web$tmp$lgk] <- 10L
  web$repo$case_nm[web$tmp$lgk] <- -1L
  web$repo$iteration[web$tmp$lgk] <- 0L

  # Skip events from certain `data_source`
  if(!is.null(web$repo$data_source) & !all(toupper(web$controls$dl_lst) == "ANY")){
    web$tmp$lgk <- check_links(
      web$repo$cri,
      web$repo$data_source,
      web$repo$data_links)$rq
    web$repo$tag[!web$tmp$lgk] <- 10L
    web$repo$case_nm[!web$tmp$lgk] <- -1L
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
    web$tmp$lgk <- !web$repo$cri %in% web$repo$cri[lgk]
    web$repo$tag[web$tmp$lgk] <- 10L
    web$repo$case_nm[web$tmp$lgk] <- -1L
    web$repo$iteration[web$tmp$lgk] <- 0L
    web$tmp$min_custom_sort <-  web$tmp$t_cri <- web$tmp$t_csort <- NULL
  }

  # Close strata with only one record.
  web$tmp$lgk <- !duplicated(web$repo$cri, fromLast = TRUE) &
    !duplicated(web$repo$cri, fromLast = FALSE) & web$controls$skip_unique_strata
  web$repo$tag[web$tmp$lgk] <- 10L
  web$repo$case_nm[web$tmp$lgk & is.na(web$repo$case_nm)] <- 0L
  web$repo$wind_nm[web$tmp$lgk & is.na(web$repo$wind_nm)] <- 0L
  web$repo$iteration[web$tmp$lgk] <- 0L
  web$tmp <- NULL

  web$counts$max_indexes <- ite <- 1L

  web$repo$assign_ord <- order(
    web$repo$cri, web$repo$custom_sort, web$repo$assign_ord)

  rm(assign_ord, case_for_recurrence, case_length, case_length_total,
     case_overlap_methods, case_sub_criteria, cri, custom_sort, data_links,
     data_source, date, display, dl_lst, epid_unit, episode_type,
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
    web$report <- c(web$report, list(web$rp_data))
  }

  splits <- 1L
  if(splits == 1){
    web$repo$split_cd <- rep(1L, web$counts$dataset.n)
    web$sys.tmp$splits <- 1L
  }else{
    web$repo$split_cd <- as.integer(cut(web$repo$cri, splits))
    web$sys.tmp$splits <- web$repo$split_cd[!duplicated(web$repo$split_cd)]
  }

  web$tm_ia <- Sys.time()
  for(s.cd in web$sys.tmp$splits){
    web$sys.tmp$ite_pos <- web$repo$pr_sn[
      web$repo$assign_ord[web$repo$split_cd == s.cd]
      ]

    while (max(web$repo$tag[web$repo$split_cd == s.cd]) == 20) {
      if(web$controls$display %in% c("stats_with_report", "stats")){
        msg <- paste0("Iteration ", fmt(ite) ,".\n")
        cat(msg)
      }
      nms_a <- names(web)
      # Priority for reference events
      web$sys.tmp$ite_pos <- web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] != 10]
      web$tmp$tgt_pos <- web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] == 1]
      web$tmp$lgk <-
        web$repo$reference_event[web$repo$ld_pos[web$tmp$tgt_pos]] %in%
        c("last_event", "last_record") &
        web$repo$episode_type[web$repo$ld_pos[web$tmp$tgt_pos]] != 3

      web$sys.tmp$ite_pos <- c(
        # 1. Windows with last record/event
        rev(web$tmp$tgt_pos[web$tmp$lgk]),
        # 2. Windows with first record/event
        web$tmp$tgt_pos[!web$tmp$lgk],
        # 3. Un-linked records
        web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] == 20]
      )

      # Window's reference record
      web$tmp$tr_lgk <- !duplicated(web$repo$cri[web$sys.tmp$ite_pos],
                                    fromLast = FALSE)
      # Excluded from this iteration
      web$tmp$exc_lgk <- rep(FALSE, length(web$sys.tmp$ite_pos))
      #
      if(web$controls$any_recurrence_epi & web$controls$use_rolls_max){
        # End recurrence when rolls_max is reached
        web$tmp$lgk <- (web$repo$roll_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] == web$repo$rolls_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] &
                          !web$repo$case_for_recurrence[web$sys.tmp$ite_pos[web$tmp$ld_lgk]]
        ) |
          (web$repo$rxt_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] == web$repo$rolls_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] * 2 &
             web$repo$case_for_recurrence[web$sys.tmp$ite_pos[web$tmp$ld_lgk]]
          ) |
          web$repo$rolls_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] == 0

        # check this lgk
        web$tmp$lgk <- web$tmp$cri %in% web$tmp$cri[web$tmp$lgk] & web$tmp$tag %in% c(1, 2, -2)
        web$tmp$exc_lgk[web$tmp$lgk] <- TRUE
        # Flag strata with no more recurrence
        web$tmp$lgk <- web$tmp$cri %in% web$tmp$cri[web$tmp$lgk]
        if(web$controls$use_episodes_max){
          # Update episode counter
          web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <- web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$lgk]] + .5
        }
        # Restart recurrence counter
        web$repo$rxt_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <-
          web$repo$roll_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <- 0L
      }
      if(web$controls$use_episodes_max){
        # Strata with `episode_max`
        web$tmp$lgk <-
          (web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] >
             web$repo$episodes_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] &
             web$repo$episode_type[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] == 1) |
          (web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] ==
             web$repo$episodes_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] &
             web$repo$episode_type[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] != 1) |
          web$repo$episodes_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] == 0
        web$tmp$lgk <-
          web$tmp$cri %in%
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
      # Window's reference record based on new selection
      web$tmp$tr_lgk <-
        !duplicated(web$repo$cri[web$sys.tmp$ite_pos], fromLast = FALSE)
      # Flag `strata` forming a new episode
      web$repo$nwEpi[web$sys.tmp$ite_pos] <-
        web$repo$cri[web$sys.tmp$ite_pos] %in% web$repo$cri[
          web$sys.tmp$ite_pos[
            web$tmp$tr_lgk &
              web$repo$tag[web$sys.tmp$ite_pos] == 20
          ]
        ]
      if(length(web$sys.tmp$ite_pos) <= 1){
        break
      }
      # Record-pairs
      web$rec.pairs <- make_pairs_batched_wf_episodes(
        strata = web$repo$cri[web$sys.tmp$ite_pos],
        x = web$sys.tmp$ite_pos,
        index_record = web$tmp$tr_lgk,
        assign_ord = web$repo$assign_ord[web$sys.tmp$ite_pos],
        lgk = web$repo$nwEpi[web$sys.tmp$ite_pos]
      )

      web$rec.pairs$x_pos <-
        web$rec.pairs$y_pos <- NULL
      names(web$rec.pairs)[which(names(web$rec.pairs) == "x_val")] <-
        "cu_pos"
      names(web$rec.pairs)[which(names(web$rec.pairs) == "y_val")] <-
        "tr_pos"
      web$rec.pairs$cri <- web$repo$cri[web$rec.pairs$cu_pos]
      web$rec.pairs$nwEpi <- web$repo$nwEpi[web$rec.pairs$cu_pos]
      # Episode's reference record
      web$repo$ld_pos[web$rec.pairs$cu_pos[web$rec.pairs$nwEpi]] <-
        web$rec.pairs$tr_pos[web$rec.pairs$nwEpi]
      web$rec.pairs$ld_pos <- web$repo$ld_pos[web$rec.pairs$cu_pos]
      # Window's reference record
      web$rec.pairs$ref_rd <-
        web$rec.pairs$cu_pos == web$rec.pairs$tr_pos
      #
      if(any(web$repo$reference_event[web$rec.pairs$ld_pos]
             %in% c("first_event", "last_event") |
             web$repo$episode_type[web$rec.pairs$ld_pos] == 3)
      ){
        # Records for recursive checks (semi batched)
        web$rec.pairs$recursive_index <-
          rep(FALSE, length(web$rec.pairs$cu_pos))
        # Reference events
        web$rec.pairs$recursive_index[
          web$repo$reference_event[web$rec.pairs$ld_pos] %in%
            c("first_event", "last_event") &
            !web$rec.pairs$ref_rd
        ]  <- TRUE
        web$rec.pairs$recursive_index[web$rec.pairs$recursive_index] <-
          overlap(
            web$repo$date[web$rec.pairs$cu_pos[web$rec.pairs$recursive_index]],
            web$repo$date[web$rec.pairs$tr_pos[web$rec.pairs$recursive_index]])
        # Linked records of recursive window
        web$rec.pairs$recursive_index[
          web$repo$episode_type[web$rec.pairs$ld_pos] == 3 &
            web$repo$tag[web$rec.pairs$cu_pos] == 1 &
            !web$rec.pairs$ref_rd] <- TRUE
        #
        web$rec.pairs$recursive_strata <-
          web$rec.pairs$tr_pos %in%
          web$rec.pairs$tr_pos[web$rec.pairs$recursive_index]

        # Additional record-pairs for recursive checks
        if(length(which(web$rec.pairs$recursive_strata)) > 0){
          web$rec.pairs_recursive <- make_pairs_batched_wf_episodes(
            strata = web$rec.pairs$tr_pos[web$rec.pairs$recursive_strata],
            x = web$rec.pairs$cu_pos[web$rec.pairs$recursive_strata],
            index_record = web$rec.pairs$recursive_index[web$rec.pairs$recursive_strata],
            assign_ord = web$repo$assign_ord[web$rec.pairs$cu_pos[web$rec.pairs$recursive_strata]],
            lgk = web$repo$nwEpi[web$rec.pairs$cu_pos[web$rec.pairs$recursive_strata]]
          )

          web$rec.pairs$cu_pos <-
            c(web$rec.pairs$cu_pos, web$rec.pairs_recursive$x_val)
          web$rec.pairs$tr_pos <-
            c(web$rec.pairs$tr_pos, web$rec.pairs_recursive$y_val)
          web$rec.pairs$index_ord <-
            c(web$rec.pairs$index_ord, web$rec.pairs_recursive$index_ord + 1L)
          web$rec.pairs_recursive <- NULL
          web$rec.pairs$ld_pos <- web$repo$ld_pos[web$rec.pairs$cu_pos]
        }
      }
      #
      web$rec.pairs$cri <- web$repo$cri[web$rec.pairs$cu_pos]
      web$rec.pairs$nwEpi <- web$repo$nwEpi[web$rec.pairs$cu_pos]
      # Reference event for each window.
      web$rec.pairs$ref_rd <- web$rec.pairs$cu_pos == web$rec.pairs$tr_pos
      if(web$controls$any_recurrence_epi){
        # Record-pairs split by type of windows
        # "case"
        web$rec.pairs$is_case_window <- web$rec.pairs$nwEpi
        # "recurrence"
        web$rec.pairs$is_case_window[!web$rec.pairs$is_case_window] <- 2
        # "case_of_recurrence"
        web$rec.pairs$is_case_window[
          (!web$rec.pairs$nwEpi &
             web$repo$case_for_recurrence[web$rec.pairs$ld_pos] &
             is_even(web$repo$rxt_n[web$rec.pairs$tr_pos]))
        ] <- 3

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
        web$window_opts_nm <- ifelse(web$window_opts == 1, "case",
                                     ifelse(web$window_opts == 2, "recurrence",
                                            "case_for_recurrence"))
        names(web$rec.pairs) <- web$window_opts_nm
      }else{
        web$rec.pairs$is_case_window <- rep(1, length(web$rec.pairs$cu_pos))
        web$window_opts <- 1
        web$window_opts_nm <- "case"
        web$rec.pairs <- list(case = web$rec.pairs[
          c("cri", "index_ord", "cu_pos", "tr_pos",
            "nwEpi", "ld_pos", "is_case_window", "ref_rd")
        ])
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
        web$tmp$sub_criteria_nm <- paste0(w.name, "_sub_criteria")
        web$tmp$length_nm <- paste0(w.name, "_length")
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
        web$tmp$lgk <-
          web$mm_opts[[web$tmp$overlap_method]][web$rec.pairs[[w.name]]$ovr_pos] == 8 &
          web$repo$date@.Data[web$rec.pairs[[w.name]]$cu_pos.mi] == 0

        web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk] <-
          web$repo$date@start[
            web$rec.pairs[[w.name]]$cu_pos.mi[web$tmp$lgk]] <=
          web$mm_opts[[web$tmp$length_nm]]$range_dt_z[
            web$rec.pairs[[w.name]]$len_pos[web$tmp$lgk]]

        if(any(!web$tmp$lgk)){
          web$rec.pairs[[w.name]]$ep_checks[!web$tmp$lgk] <-
            overlaps(
              x = web$repo$date@start[
                web$rec.pairs[[w.name]]$cu_pos.mi[!web$tmp$lgk]],
              y = web$mm_opts[[web$tmp$length_nm]]$range[
                web$rec.pairs[[w.name]]$len_pos[!web$tmp$lgk]],
              m = web$mm_opts[[web$tmp$overlap_method]][
                web$rec.pairs[[w.name]]$ovr_pos[!web$tmp$lgk]]
            )
        }

        if(web$controls[[paste0("use_", w.type, "_length_total")]]){
          # Number of records matched (Excludes self-matches)
          web$tmp$c_hits <- make_refs_V2(
            x = web$rec.pairs[[w.name]]$tr_pos.mi[web$rec.pairs[[w.name]]$ep_checks],
            y = web$rec.pairs[[w.name]]$ep_checks[web$rec.pairs[[w.name]]$ep_checks],
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
        }

        if(web$counts$w.list.num > 1){
          # Check of >0 overlap-matches across all case_length or recurrence_length
          web$rec.pairs[[w.name]]$ep_checks <-
            rowSums(matrix(web$rec.pairs[[w.name]]$ep_checks,
                           ncol = web$counts$w.list.num))
        }

        # Check user-defined conditions
        web$rec.pairs[[w.name]]$w.match <-
          ((web$rec.pairs[[w.name]]$ep_checks) | web$rec.pairs[[w.name]]$ref_rd)

        if(web$controls$use_sub_cri){
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
          web$tmp$pos <- web$rec.pairs[[w.name]]$cu_pos.mm[web$rec.pairs[[w.name]]$w.match]
        }
        # Index for the multiple window ids
        # Update window ids for matches
        web$repo$wind_nm[web$tmp$pos[is.na(web$repo$wind_id[web$tmp$pos])]] <-
          (w.code - 1)
        web$repo$wind_id[web$tmp$pos[is.na(web$repo$wind_id[web$tmp$pos])]] <-
          web$rec.pairs[[w.name]]$tr_pos[web$rec.pairs[[w.name]]$w.match][
            is.na(web$repo$wind_id[web$tmp$pos])
          ]

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
              web$rec.pairs[[w.name]]$ref_rd
            ]
        }

        if(web$controls$use_skip_b4_len){
          web$tmp$indx <- which(!web$rec.pairs[[w.name]]$w.match &
                                  web$repo$skip_if_b4_lengths[
                                    web$rec.pairs[[w.name]]$cu_pos])
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
          web$rec.pairs[[w.name]]$ld_pos[web$rec.pairs[[w.name]]$w.match]

        # Update episode counter
        if(web$controls$use_episodes_max){
          web$tmp$upd_lgk <-
            web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] == 1 &
            web$repo$tag[web$rec.pairs[[w.name]]$tr_pos] == 20L
          web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] <-
            web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] + 1L

          web$tmp$upd_lgk <-
            web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] %in% c(2, 3) &
            web$repo$tag[web$rec.pairs[[w.name]]$tr_pos] == 20L
          web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] <-
            web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] + .5
        }

        if(web$controls$use_rolls_max){
          if(w.name %in% c("recurrence", "case_for_recurrence")){
            if(w.name == "case_for_recurrence"){
              # Update recurrence counter
              web$tmp$vr <- web$rec.pairs[[w.name]]$cri %in%
                web$rec.pairs[[w.name]]$cri[
                  web$rec.pairs[[w.name]]$w.match & !web$rec.pairs[[w.name]]$nwEpi]
              web$tmp$vr2 <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$vr]
              web$tmp$vr <- web$rec.pairs[[w.name]]$tr_pos[web$tmp$vr]
              web$tmp$vr2 <- c(web$tmp$vr[!duplicated(web$tmp$vr)], web$tmp$vr2)
              web$repo$rxt_n[web$tmp$vr2] <- web$repo$rxt_n[web$tmp$vr2] + 1L
            }

            # Update rolling counter
            web$tmp$vr <- web$rec.pairs[[w.name]]$cri %in%
              web$rec.pairs[[w.name]]$cri[
                web$rec.pairs[[w.name]]$w.match]
            web$tmp$vr2 <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$vr]
            web$tmp$vr <- web$rec.pairs[[w.name]]$tr_pos[web$tmp$vr]
            web$tmp$vr2 <- c(web$tmp$vr[!duplicated(web$tmp$vr)], web$tmp$vr2)

            web$repo$roll_n[web$tmp$vr2] <-
              web$repo$roll_n[web$tmp$vr2] + 1L
          }
        }

        # Update `case_nm` - "Case" records
        if(w.name == "case"){
          web$tmp$indx <- which(web$rec.pairs[[w.name]]$ref_rd)
          web$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx][
            web$rec.pairs[[w.name]]$s.match[web$tmp$indx] &
              web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] == 20
          ]
          web$repo$case_nm[web$tgt_pos] <- 0L

          web$tmp$indx <- which(web$rec.pairs[[w.name]]$ref_rd)
          web$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx][
            !web$rec.pairs[[w.name]]$s.match[web$tmp$indx] &
              web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] == 20
          ]
          web$repo$case_nm[web$tgt_pos] <- 4L
        }

        # Update `case_nm` - Duplicate" records
        web$repo$case_nm[web$rec.pairs[[w.name]]$cu_pos[
          web$rec.pairs[[w.name]]$w.match &
            !web$rec.pairs[[w.name]]$ref_rd &
            is.na(web$repo$case_nm[web$rec.pairs[[w.name]]$cu_pos])
        ]] <- ifelse(w.type == "recurrence", 3L, 2L)

        # Close if episode type is "fixed"
        if(w.name == "case"){
          web$tmp$indx <- web$rec.pairs[[w.name]]$w.match &
            web$repo$episode_type[web$rec.pairs[[w.name]]$cu_pos] == 1

          web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[
            web$tmp$indx
          ]] <- 10L
        }

        if(w.name != "case"){
          # Update `case_nm` - "Recurrent" index records
          web$tmp$indx1 <- !duplicated(web$rec.pairs[[w.name]]$cri[
            !web$rec.pairs[[w.name]]$w.match])
          web$tmp$indx2 <-  web$rec.pairs[[w.name]]$cri[!web$rec.pairs[[w.name]]$w.match] %in%
            web$rec.pairs[[w.name]]$cri[
              web$rec.pairs[[w.name]]$w.match & web$rec.pairs[[w.name]]$s.match]
          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[
            !web$rec.pairs[[w.name]]$w.match][web$tmp$indx1 & !web$tmp$indx2]
          web$repo$case_nm[web$tmp$tgt_pos] <- 1L

          web$tmp$tgt_pos <- web$rec.pairs[[w.name]]$cu_pos[
            !web$rec.pairs[[w.name]]$w.match][web$tmp$indx1 & web$tmp$indx2]
          web$repo$case_nm[web$tmp$tgt_pos] <- 5L

          # Close records previously flagged for recurrence check.
          web$repo$tag[
            web$rec.pairs[[w.name]]$cu_pos[
              web$repo$tag[web$rec.pairs[[w.name]]$cu_pos] == 1
            ]
          ] <- 10L
        }

        if(web$controls$use_skip_b4_len){
          # close if `skip_if_b4_lengths`
          web$repo$tag[
            web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$sk_checks
            ]
          ] <- 10L

          web$repo$case_nm[
            web$rec.pairs[[w.name]]$cu_pos[
              web$rec.pairs[[w.name]]$sk_checks
            ]
          ] <- -1L
        }

        if(web$controls$any_recurrence_epi){
          # Flag next batched to be checked for recurrence
          web$tmp$indx <-
            web$rec.pairs[[w.name]]$w.match &
            web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] %in% 2:3 &
            web$repo$tag[web$rec.pairs[[w.name]]$cu_pos] == 20
          web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] <- 1L

          # Flag strata with no more recurrence
          web$tmp$lgk <- web$repo$tag[web$sys.tmp$ite_pos] == 1
          web$tmp$lgk <- !web$repo$cri[web$sys.tmp$ite_pos] %in%
            web$repo$cri[web$sys.tmp$ite_pos][web$tmp$lgk]

          if(web$controls$use_episodes_max){
            # Update episode counter
            web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <-
              web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$lgk]] + 0.5
          }

          if(web$controls$use_rolls_max){
            # Restart recurrence counter
            web$repo$rxt_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <-
              web$repo$roll_n[web$sys.tmp$ite_pos[web$tmp$lgk]] <- 0L
          }
        }
      }

      web$tmp$lgk <- web$repo$iteration[web$sys.tmp$ite_pos] == 0 &
        web$repo$tag[web$sys.tmp$ite_pos] != 20
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
        web$report <- c(web$report, list(web$rp_data))
      }
      web$tm_ia <- Sys.time()
      ite <- ite + 1L
      nms_z <- names(web)
      web$tmp <- web$rec.pairs <- NULL
    }
  }

  web$tmp$tgt_pos <- web$repo$pr_sn[is.na(web$repo$case_nm)]
  web$repo$case_nm[web$tmp$tgt_pos] <- -1L
  web$repo$wind_nm[web$tmp$tgt_pos] <- -1L
  web$repo$iteration[web$tmp$tgt_pos] <- ite - 1L

  if(!web$controls$display %in% c("none_with_report", "none")) cat("\n")

  web$tmp$index_date <- web$repo$date[web$repo$epid]
  web$repo$dist_epid_index <-
    ((as.numeric(web$repo$date@start) + as.numeric(right_point(web$repo$date))) * .5) -
    ((as.numeric(web$tmp$index_date@start) + as.numeric(right_point(web$tmp$index_date))) * .5)

  web$repo$wind_id <- matrix(web$repo$wind_id, nrow = web$counts$dataset.n)
  web$repo$wind_id <- lapply(1:web$counts$max_indexes, function(i){
    web$repo$wind_id[,i]
  })
  web$repo$wind_nm <- matrix(web$repo$wind_nm, nrow = web$counts$dataset.n)
  web$repo$wind_nm <- lapply(1:web$counts$max_indexes, function(i){
    web$repo$wind_nm[,i]
  })
  names(web$repo$wind_nm) <- paste0("wind_nm", seq_len(length(web$repo$wind_nm)))
  names(web$repo$wind_id) <- paste0("wind_id", seq_len(length(web$repo$wind_id)))

  web$tmp$index_date <- web$repo$date[web$repo$wind_id$wind_id1]
  web$repo$dist_wind_index <- ((as.numeric(web$repo$date@start) + as.numeric(right_point(web$repo$date))) * .5) -
    ((as.numeric(web$tmp$index_date@start) + as.numeric(right_point(web$tmp$index_date))) * .5)

  if(isTRUE(web$controls$is_dt)){
    web$repo$epid_unit <- web$repo$epid_unit[!duplicated(web$repo$epid_unit)]
    if(length(web$repo$epid_unit) > 1){
      web$repo$epid_unit <- 4
    }
    web$tmp$diff_unit <- names(diyar::episode_unit)[web$repo$epid_unit]
    web$tmp$lgk <- web$tmp$diff_unit %in% c("seconds", "minutes")
    web$tmp$diff_unit[web$tmp$lgk] <- paste0(substr(web$tmp$diff_unit[web$tmp$lgk], 1 ,3), "s")

    web$repo$dist_epid_index <-
      web$repo$dist_epid_index / as.numeric(diyar::episode_unit[web$repo$epid_unit])
    web$repo$dist_epid_index <-
      as.difftime(web$repo$dist_epid_index, units = web$tmp$diff_unit)

    if(web$controls$any_recurrence_epi){
      web$repo$dist_wind_index <-
        web$repo$dist_wind_index / as.numeric(diyar::episode_unit[web$repo$epid_unit])
      web$repo$dist_wind_index <-
        as.difftime(web$repo$dist_wind_index, units = web$tmp$diff_unit)
    }else{
      web$repo$dist_wind_index <- web$repo$dist_epid_index
    }
  }

  web$epids <- new("epid",
                   .Data= web$repo$epid,
                   dist_epid_index = web$repo$dist_epid_index,
                   dist_wind_index = web$repo$dist_wind_index,
                   sn = web$repo$pr_sn,
                   iteration = web$repo$iteration,
                   wind_id = web$repo$wind_id,
                   options = options_lst,
                   case_nm = web$repo$case_nm,
                   wind_nm = web$repo$wind_nm)

  class(web$epids@case_nm) <- "d_label"
  attr(web$epids@case_nm, "value") <- -1L : 5L
  attr(web$epids@case_nm, "label") <-
    c("Skipped", "Case", "Recurrent", "Duplicate_C",
      "Duplicate_R", "Case_CR", "Recurrent_CR")
  attr(web$epids@case_nm, "state") <- "encoded"

  web$epids@wind_nm <- lapply(web$epids@wind_nm, function(x){
    class(x) <- "d_label"
    attr(x, "value") <- -1L : 1L
    attr(x, "label") <- c("Skipped", "Case", "Recurrence")
    attr(x, "state") <- "encoded"
    return(x)
  })

  web$tmp$rr <- rle(sort(web$epids@.Data))
  web$epids@epid_total <- web$tmp$rr$lengths[match(web$epids@.Data, web$tmp$rr$values)]

  if(isTRUE(web$repo$group_stats)){
    web$tmp$lgk <- which(web$epids@epid_total != 1)
    web$epids@epid_interval <- as.number_line(web$repo$date)
    web$epids@epid_interval[web$tmp$lgk] <- group_stats(
      strata = web$epids@.Data[web$tmp$lgk],
      start_date = web$epids@epid_interval@start[web$tmp$lgk],
      end_date = right_point(web$epids@epid_interval)[web$tmp$lgk]
    )

    web$tmp$lgk <- from_last[web$epids@.Data]
    web$epids@epid_interval[web$tmp$lgk] <- reverse_number_line(
      web$epids@epid_interval[web$tmp$lgk],
      direction = "increasing")

    if(isTRUE(web$controls$is_dt)){
      web$epids@epid_interval <- number_line(
        as.POSIXct(
          web$epids@epid_interval@start, tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT")),
        as.POSIXct(
          right_point(web$epids@epid_interval), tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT"))
      )
      web$epids@epid_length <- difftime(
        right_point(web$epids@epid_interval),
        web$epids@epid_interval@start,
        units = diff_unit)
    }else{
      web$epids@epid_length <-
        web$epids@epid_interval@start - right_point(web$epids@epid_interval)
    }
  }

  if(!is.null(web$repo$data_source)){
    # Data links
    web$tmp$rst <- check_links(
      web$epids@.Data,
      web$repo$data_source,
      web$repo$data_links)
    web$epids@epid_dataset <- web$tmp$rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- web$tmp$rst$rq
      web$epidsepids <- suppressWarnings(delink(web$epids, !req_links))
      web$epids@epid_dataset[!req_links] <- web$repo$data_source[!req_links]
    }
    web$epids@epid_dataset <- encode(web$epids@epid_dataset)
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

#' @rdname  episodes
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

#' @rdname  episodes
episodes_af_shift <- function(date, case_length = Inf, strata = NULL, episode_type = "fixed"){
  web <- list()
  web$repo$pr_sn <- seq_len(length(date))
  web$repo$date <- as.number_line(date)
  is_dt <- ifelse(
    inherits(web$repo$date@start, c("Date","POSIXct","POSIXt","POSIXlt")),
    FALSE, TRUE)
  web$repo$date_a <- web$repo$date@start
  web$repo$date_z <- right_point(web$repo$date)
  web$repo$tmp.windowEnd <- web$repo$date_z + case_length
  web$repo$strata <- strata
  if(length(web$repo$strata) > 1){
    web$repo$s_ord <- order(web$repo$strata, web$repo$date_a, web$repo$date_z)
  }else{
    web$repo$s_ord <- order(web$repo$date@start, web$repo$date_a, web$repo$date_z)
  }
  web$repo <- lapply(web$repo, function(x) x[web$repo$s_ord])
  web$repo$tmp.PrvWindowEnd <- c(NA, rev(rev(web$repo$tmp.windowEnd)[-1]))

  web$repo$change_lgk <- (web$repo$date_a > web$repo$tmp.PrvWindowEnd)

  if(length(web$repo$strata) > 1){
    web$repo$tmp.Prvstrata <- c(NA, rev(rev(web$repo$strata)[-1]))
    web$repo$diff_lgk <- which(web$repo$strata != web$repo$tmp.Prvstrata)
  }else{
    web$repo$diff_lgk <- 1
  }
  web$repo$change_lgk[web$repo$diff_lgk] <- TRUE
  web$repo$change_lgk[which(is.na(web$repo$change_lgk))] <- FALSE
  web$repo$Cummchange_lgk <- cumsum(web$repo$change_lgk)

  web$repo$epid <- web$repo$Cummchange_lgk + 1

  if(episode_type == "fixed"){
    web$epid <- links_wf_episodes(
      date = web$repo$date,
      strata = web$repo$epid,
      case_length = case_length
    )@.Data
  }

  web$indx <- which(!duplicated(web$repo$epid))
  web$repo$group_id <- web$repo$pr_sn[web$indx][match(web$repo$epid, web$repo$epid[web$indx])]
  web$repo$group_id[order(web$repo$pr_sn)]

  web$repo$group_id
  # browser()
  web$epids <- new("epid",
                   .Data=  web$repo$group_id,
                   sn = web$repo$pr_sn,
                   iteration = rep(1L, length(web$repo$group_id)),
                   wind_id = list(wind_id1 =  web$repo$group_id),
                   options = list())
  web$epids@case_nm <- !(web$repo$pr_sn %in% web$repo$group_id)
  web$epids@case_nm[web$epids@case_nm] <-
    ifelse(episode_type == "rolling", 2L, 3L)

  class(web$epids@case_nm) <- "d_label"
  attr(web$epids@case_nm, "value") <- -1L : 5L
  attr(web$epids@case_nm, "label") <-
    c("Skipped", "Case", "Recurrent", "Duplicate_C",
      "Duplicate_R", "Case_CR", "Recurrent_CR")
  attr(web$epids@case_nm, "state") <- "encoded"

  web$epids@wind_id$wind_nm <- rep(
    ifelse(episode_type == "rolling", 1L, 0L), length(web$repo$group_id)
  )
  class(web$epids@wind_id$wind_nm) <- "d_label"
  attr(web$epids@wind_id$wind_nm, "value") <- -1L : 1L
  attr(web$epids@wind_id$wind_nm, "label") <- c("Skipped", "Case", "Recurrence")
  attr(web$epids@wind_id$wind_nm, "state") <- "encoded"

  web$tmp$rr <- rle(sort(web$epids@.Data))
  web$epids@epid_total <- web$tmp$rr$lengths[match(web$epids@.Data, web$tmp$rr$values)]

  if(isTRUE(web$repo$group_stats)){
    web$tmp$lgk <- which(web$epids@epid_total != 1)
    web$epids@epid_interval <- web$repo$date
    web$epids@epid_interval[web$tmp$lgk] <- group_stats(
      strata = web$epids@.Data[web$tmp$lgk],
      start_date = web$epids@epid_interval@start[web$tmp$lgk],
      end_date = right_point(web$epids@epid_interval)[web$tmp$lgk]
    )

    if(isTRUE(web$controls$is_dt)){
      web$epids@epid_interval <- number_line(
        as.POSIXct(
          web$epids@epid_interval@start, tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT")),
        as.POSIXct(
          right_point(web$epids@epid_interval), tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT"))
      )
      web$epids@epid_length <- difftime(
        right_point(web$epids@epid_interval),
        web$epids@epid_interval@start,
        units = diff_unit)
    }else{
      web$epids@epid_length <-
        web$epids@epid_interval@start - right_point(web$epids@epid_interval)
    }
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
  window[from_last == TRUE] <- invert_number_line(window[from_last == TRUE])
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
