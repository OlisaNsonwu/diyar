#' @name episodes
#' @title Link events to chronological episodes.
#'
#' @description Create temporal links between dated events.
#' Each set of linked records are assigned a unique identifier with relevant group-level information.
#'
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=epid-class]{epid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Episodes are created separately for each \code{strata}.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Event date or period.
#' @param case_length \code{[integer|\link{number_line}]}. Duration from index event distinguishing one \code{"Case"} from another.
#' @param episodes_max \code{[integer]}. The maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{[character]}. Options are \code{"fixed"} (default), \code{"rolling"} or \code{"recursive"}. See \code{Details}.
#' @param recurrence_length \code{[integer|\link{number_line}]}. Duration from an event distinguishing a \code{"Recurrent"} event from its index event.
#' @param episode_unit \code{[character]}. Time units for \code{case_length} and \code{recurrence_length}. Options are "seconds", "minutes", "hours", "days" (default), "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max \code{[integer]}. Maximum number of times the index event recurs. Only used if \code{episode_type} is \code{"rolling"} or \code{"recursive"}.
#' @param data_source \code{[character]}. Data source identifier. Adds the list of data sources in each episode to the \code{\link[=epid-class]{epid}}. Useful when the data is from multiple sources.
#' @param from_last \code{[logical]}. Chronological order of episode tracking i.e. ascending (\code{TRUE}) or descending (\code{FALSE}).
#' @param case_overlap_methods \code{[character|integer]}. Accepted overlaps method for \code{"Case"} and \code{"Duplicate"} events. Relevant when \code{date} is a period (\link{number_line}). See (\code{\link{overlaps}}).
#' @param recurrence_overlap_methods \code{[character|integer]}. Accepted overlaps method for \code{"Recurrent"} and \code{"Duplicate"} events. Relevant when \code{date} is a period (\link{number_line}). See (\code{\link{overlaps}}).
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index events. See \code{\link{custom_sort}}.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), episode-specific information like episode start and end dates are returned.
#' @param display \code{[character]}. Display or produce a status update. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param reference_event \code{[character]}. Specifies which events are used as index events for a subsequent \code{case_length} or \code{recurrence_length}. Options are \code{"last_record"} (default), \code{"last_event"}, \code{"first_record"} or \code{"first_event"}.
#' @param case_for_recurrence \code{[logical]}. If \code{TRUE}, both \code{"Case"} and \code{"Recurrent"} events will have a \code{case_length}.
#' If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"} or \code{"recursive"}.
#' @param skip_order \code{[integer]}. \code{"nth"} level of \code{custom_sort}. Episodes with index events beyond this level of preference are skipped.
#' @param data_links \code{[list|character]}. A set of \code{data_sources} required in each \code{\link[=epid-class]{epid}}. A record-group without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param skip_if_b4_lengths \code{[logical]}. If \code{TRUE} (default), events before a lagged \code{case_length} or \code{recurrence_length} are skipped.
#' @param skip_unique_strata \code{[logical]}. If \code{TRUE}, a strata with a single event are skipped.
#' @param case_sub_criteria \code{[\link{sub_criteria}]}. Additional matching criteria for events in a \code{case_length}.
#' @param recurrence_sub_criteria \code{[\link{sub_criteria}]}. Additional matching criteria for events in a \code{recurrence_length}.
#' @param case_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{case_lengths} required for an episode.
#' @param recurrence_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{recurrence_lengths} required for an episode.
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{episodes_wf_splits}}; \code{\link{custom_sort}}; \code{\link{sub_criteria}}; \code{\link[=windows]{epid_length}}; \code{\link[=windows]{epid_window}}; \code{\link{partitions}}; \code{\link{links}}; \code{\link{overlaps}}; \code{\link{number_line}}; \code{\link{link_records}}; \code{\link{schema}}
#'
#' @details
#' \bold{\code{episodes()}} links dated records (events) that
#' are within specified durations of each other.
#' In each iteration, an index event is selected and compared against every other event.
#'
#' Every event is linked to a unique group (episode; \code{\link[=epid-class]{epid}} object).
#' These episodes represent occurrences of interest as defined by the rules and conditions specified in the function's arguments.
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
#' Every event in each episode is categorise as;
#' \itemize{
#' \item \code{"Case"} - Index event of the episode (without matching \code{\link{sub_criteria}}).
#' \item \code{"Case_CR"} - Index event of the episode (with matching \code{\link{sub_criteria}}).
#' \item \code{"Duplicate_C"} - Duplicate of the index event.
#' \item \code{"Recurrent"} - Recurrence of the index event (without matching \code{\link{sub_criteria}}).
#' \item \code{"Recurrent_CR"} - Recurrence of the index event (with matching \code{\link{sub_criteria}}).
#' \item \code{"Duplicate_R"} - Duplicate of the recurrent event.
#' \item \code{"Skipped"} - Records excluded from the episode tracking process.
#' }
#'
#' If \code{data_links} is supplied, every element of the list must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements are assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, only groups with records from every listed \code{data_source} will be unlinked.
#' \item If named \code{"g"}, only groups with records from any listed \code{data_source} will be unlinked.
#' }
#'
#' \emph{Records with a missing (\code{NA}) \code{strata} are excluded from the episode tracking process.}
#'
#' See \code{vignette("episodes")} for further details.
#'
#' @examples
#' data(infections); db_1 <- infections
#' data(hospital_admissions) ; db_2 <- hospital_admissions
#'
#' db_1$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#'
#' # Fixed episodes
#' # One 16-day (15-day difference) episode per patient
#' db_1$ep1 <- episodes(date = db_1$date,
#'                      strata = db_1$patient_id,
#'                      case_length = 15,
#'                      episodes_max = 1)
#' # Rolling episodes
#' # 16-day episodes with recurrence periods of 11 days
#' db_1$ep2 <- episodes(date = db_1$date,
#'                      case_length = 15,
#'                      recurrence_length = 10,
#'                      episode_type = "rolling")
#'
#' # Interval grouping
#' db_2$admin_period <- number_line(db_2$admin_dt,
#'                                  db_2$discharge_dt)
#' # Episodes of hospital stays
#' db_2$ep3 <- episodes(date = db_2$admin_period,
#'                      case_length = index_window(db_2$admin_period),
#'                      case_overlap_methods = "inbetween")
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
  tm_a <- Sys.time()

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
  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  inp_n <- length(date)
  if(!display %in% c("none")){
    rp_data <- di_report(tm_a, "Data validation", inp_n)
    report <- list(rp_data)
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  tm_ia <- Sys.time()

  # `episode_unit`
  episode_unit <- tolower(episode_unit)
  episode_unit <- match(episode_unit, names(diyar::episode_unit))
  class(episode_unit) <- "d_label"
  attr(episode_unit, "value") <- as.vector(sort(episode_unit[!duplicated(episode_unit)]))
  attr(episode_unit, "label") <- names(diyar::episode_unit)[attr(episode_unit, "value")]
  attr(episode_unit, "state") <- "encoded"
  # `strata`
  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1L, inp_n)
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
  rm(episode_unit)
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
  any_rolling_epi <- any(episode_type %in% c(2, 3))

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
  # `case_overlap_methods`
  if(!all(class(case_overlap_methods) == "list")){
    case_overlap_methods <- list(case_overlap_methods)
  }else{
    case_overlap_methods <- case_overlap_methods
  }
  # `case_length`
  ep_l <- length_to_range(lengths = case_length,
                          date = date,
                          from_last = from_last,
                          episode_unit = epid_unit)

  # `case_length_total`
  if(is.number_line(case_length_total)){
    case_length_total[case_length_total@.Data < 0] <- reverse_number_line(case_length_total[case_length_total@.Data < 0], "decreasing")
  }else{
    case_length_total <- number_line(case_length_total, Inf)
  }
  any_epl_min <- which(case_length_total@start != 1 & case_length_total@.Data != Inf)
  any_epl_min <- length(any_epl_min) > 0

  if(isTRUE(any_rolling_epi)){
    # `case_for_recurrence`
    any_case_for_rec <- any(case_for_recurrence == TRUE)
    # `recurrence_overlap_methods`
    if(!all(class(recurrence_overlap_methods) == "list")){
      recurrence_overlap_methods <- list(recurrence_overlap_methods)
    }else{
      recurrence_overlap_methods <- recurrence_overlap_methods
    }
    # `recurrence_length`
    rc_l <- length_to_range(lengths = recurrence_length,
                            date = date,
                            from_last = from_last,
                            episode_unit = epid_unit)
    # `recurrence_length_total`
    if(is.number_line(recurrence_length_total)){
      recurrence_length_total[recurrence_length_total@.Data < 0] <- reverse_number_line(recurrence_length_total[recurrence_length_total@.Data < 0], "decreasing")
    }else{
      recurrence_length_total <- number_line(recurrence_length_total, Inf)
    }
    any_rcl_min <- which(recurrence_length_total@start != 1 & recurrence_length_total@.Data != Inf)
  }

  # `reference_event`
  if(class(reference_event) == "logical"){
    reference_event[reference_event] <- "last_record"
    reference_event[reference_event != "last_record"] <- "first_record"
  }
  any_rec_from_last <- any(reference_event %in% c("first_record", "first_event"))
  # `skip_if_b4_lengths`
  any_skip_b4_len <- any(skip_if_b4_lengths == TRUE)

  # `d_lazy_opts`
  class(episodes_max) <- class(rolls_max) <- class(skip_order) <-
    class(from_last) <- class(episode_type) <- class(reference_event) <-
    attr(case_length_total, "opts") <- attr(case_length, "opts") <-
    class(epid_unit) <- class(skip_if_b4_lengths) <- "d_lazy_opts"

  case_overlap_methods <- lapply(case_overlap_methods, mk_lazy_opt)

  if(isTRUE(any_rolling_epi)){
    attr(recurrence_length_total, "opts") <- attr(recurrence_length, "opts") <-
      class(case_for_recurrence) <- "d_lazy_opts"
    recurrence_overlap_methods <- lapply(recurrence_overlap_methods, mk_lazy_opt)

    ld_case_for_recurrence <- case_for_recurrence
    ld_recurrence_length_total <- recurrence_length_total
  }

  # Use `overlap_methods` as a record-level input by default
  if(is.null(names(case_overlap_methods))){
    names(case_overlap_methods) <- rep("r", length(case_overlap_methods))
  }else{
    names(case_overlap_methods) <- ifelse(names(case_overlap_methods) %in% c("", NA), "r", names(case_overlap_methods))
  }

  if(isTRUE(any_rolling_epi)){
    if(is.null(names(recurrence_overlap_methods))){
      names(recurrence_overlap_methods) <- rep("r", length(recurrence_overlap_methods))
    }else{
      names(recurrence_overlap_methods) <- ifelse(names(recurrence_overlap_methods) %in% c("", NA), "r", names(recurrence_overlap_methods))
    }
  }

  date@id <- seq_len(inp_n)
  date@gid <- date@id
  if(!is.null(sn)) {
    date@gid <- as.integer(sn)
    ep_l[[1]]@gid <- as.integer(sn)
  }

  # Order of case-assignment
  ord_a <- abs(max(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start))
  ord_z <- abs(max(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date)))
  ord_a[!from_last] <- abs(min(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start[!from_last]))
  ord_z[!from_last] <- abs(min(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date[!from_last])))

  assign_ord <- order(order(ord_a, -ord_z, date@gid))
  rm(ord_a); rm(ord_z)

  # User-defined order of case-assignment
  if(!is.null(custom_sort)) {
    if(any(!class(custom_sort) %in% c("numeric", "integer", "double"))){
      custom_sort <- as.integer(as.factor(custom_sort))
    }
    if(length(custom_sort) == 1){
      custom_sort <- rep(custom_sort, inp_n)
    }
    assign_ord <- order(order(custom_sort, assign_ord))
  }else{
    custom_sort <- rep(0L, inp_n)
  }

  ld_case_length_total <- case_length_total
  ld_reference_event <- reference_event
  ld_skip_if_b4_lengths <- skip_if_b4_lengths
  ld_episode_type <- episode_type
  ld_skip_order <- skip_order
  ld_custom_sort <- custom_sort

  # Flags
  tag <- rep(0L, inp_n)
  iteration <- rep(0L, inp_n)

  e <- date@gid
  wind_id <- date@gid
  wind_id_lst <- list(wind_id1 = wind_id)
  epid_n <- rep(0L, inp_n)

  if(isTRUE(any_rolling_epi)) roll_n <- rep(0L, inp_n)
  case_nm <- rep(NA_integer_, inp_n)
  wind_nm <- case_nm

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, inp_n)
  }
  if(!display %in% c("none")){
    rp_data <- di_report(tm_ia, "Data standardisation", inp_n,)
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  tm_ia <- Sys.time()

  # User-specified records to skip
  if(!is.null(strata)){
    lgk <- is.na(strata)
    tag[lgk] <- 2L
    case_nm[lgk] <- -1L
    iteration[lgk] <- 0L
  }

  # Skip events with non-finite `dates`
  lgk <- is.na(date@start) | is.na(date@.Data)
  tag[lgk] <- 2L
  case_nm[lgk] <- -1L
  iteration[lgk] <- 0L

  # Skip events from certain `data_source`
  if(!is.null(data_source) & !all(toupper(dl_lst) == "ANY")){
    req_links <- check_links(cri, data_source, data_links)$rq
    tag[!req_links] <- 2L
    case_nm[!req_links] <- -1L
    iteration[!req_links] <- 0L
  }

  # Skip events without the required `skip_order`
  if(all(is.infinite(skip_order))){
    lgk <- custom_sort <= skip_order
    lgk <- !cri %in% cri[lgk]
    tag[lgk] <- 2L
    case_nm[lgk] <- -1L
    iteration[lgk] <- 0L
  }

  # Flag a strata with only one event as a case
  lgk <- !duplicated(cri, fromLast = TRUE) & !duplicated(cri, fromLast = FALSE) & skip_unique_strata
  tag[lgk] <- 2L
  case_nm[lgk & is.na(case_nm)] <- 0L
  wind_nm[lgk & is.na(wind_nm)] <- 0L
  iteration[lgk] <- 0L

  excluded <- length(tag[tag == 2])
  pri_pos <- seq_len(inp_n)

  if(!display %in% c("none_with_report", "none")) cat("\n")
  epids_repo <- list("e" = e,
                     "tag" = tag,
                     "int" = date,
                     "case_nm" = case_nm,
                     "wind_nm" = wind_nm,
                     "wind_id" = wind_id,
                     "wind_id_lst" = list(wind_id),
                     "iteration" = iteration)

  # Subset out all linked records
  if(any(tag == 2)){
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)

    e <- e[ntag_lgk]
    tag <- tag[ntag_lgk]
    cri <- cri[ntag_lgk]
    assign_ord <- assign_ord[ntag_lgk]
    epid_n <- epid_n[ntag_lgk]
    custom_sort <- custom_sort[ntag_lgk]
    wind_nm <- wind_nm[ntag_lgk]
    wind_id <- wind_id[ntag_lgk]
    wind_id_lst <- list(wind_id)
    rolls_max <- rolls_max[ntag_lgk]
    iteration <- iteration[ntag_lgk]
    episodes_max <- episodes_max[ntag_lgk]
    skip_order <- skip_order[ntag_lgk]
    case_nm <- case_nm[ntag_lgk]
    episode_type <- episode_type[ntag_lgk]
    reference_event <- reference_event[ntag_lgk]
    skip_if_b4_lengths <- skip_if_b4_lengths[ntag_lgk]
    case_length_total <- case_length_total[ntag_lgk]
    ep_l <- lapply(ep_l, function(x){x[ntag_lgk]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[ntag_lgk]})

    ld_episode_type <- ld_episode_type[ntag_lgk]
    ld_custom_sort <- ld_custom_sort[ntag_lgk]
    ld_skip_order <- ld_skip_order[ntag_lgk]
    ld_reference_event <- ld_reference_event[ntag_lgk]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[ntag_lgk]
    ld_case_length_total <- ld_case_length_total[ntag_lgk]

    if(!is.null(case_sub_criteria) & length(ntag_lgk) > 0){
      case_sub_criteria <- reframe(case_sub_criteria, func = function(x) x[ntag_lgk])
    }

    if(isTRUE(any_rolling_epi)) {
      roll_n <- roll_n[ntag_lgk]
      case_for_recurrence <- case_for_recurrence[ntag_lgk]
      recurrence_length_total <- recurrence_length_total[ntag_lgk]

      ld_case_for_recurrence <- ld_case_for_recurrence[ntag_lgk]
      ld_recurrence_length_total <- ld_recurrence_length_total[ntag_lgk]

      rc_l <- lapply(rc_l, function(x){x[ntag_lgk]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[ntag_lgk]})

      if(!is.null(recurrence_sub_criteria) & length(ntag_lgk) > 0){
        recurrence_sub_criteria <- reframe(recurrence_sub_criteria, func = function(x) x[ntag_lgk])
      }
    }
    date <- date[ntag_lgk]
  }

  if(!display %in% c("none")){
    rp_data <- di_report(tm_ia, "Pre-tracking", inp_n, current_skipped = excluded)
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0("Pre-tracking\n",
                 "Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                 "Skipped: ", fmt(rp_data[[5]]), " record(s)","\n",
                 "Time: ", fmt(rp_data[[2]], "difftime"),
                 "\n\n"))
    }
  }
  tm_ia <- Sys.time()

  ite <- 1L
  while (suppressWarnings(min(tag)) != 2 & length(tag) > 0) {
    if(display %in% c("stats_with_report", "stats")){
      msg <- paste0("Iteration ", fmt(ite) ,".\n")
      cat(msg)
    }
    any_rolling_epi_curr <- any(episode_type %in% c(2, 3))
    any_epl_min_curr <- which(case_length_total@start != 1 & case_length_total@.Data != Inf)
    any_epl_min_curr <- length(any_epl_min_curr) == inp_n

    if(any_rolling_epi_curr){
      any_rcl_min_curr <- which(recurrence_length_total@start != 1 & recurrence_length_total@.Data != Inf)
      any_rcl_min_curr <- length(any_rcl_min_curr) == inp_n
    }

    # Sort dataset on order of case-assignment
    sort_ord <- order(cri, tag, assign_ord, decreasing = TRUE)
    if(any(tag == 2)){
      sort_ord <- sort_ord[tag[sort_ord] != 2]
    }

    e <- e[sort_ord]
    tag <- tag[sort_ord]
    cri <- cri[sort_ord]
    assign_ord <- assign_ord[sort_ord]
    date <- date[sort_ord]
    epid_n <- epid_n[sort_ord]
    custom_sort <- custom_sort[sort_ord]
    wind_nm <- wind_nm[sort_ord]
    wind_id <- wind_id[sort_ord]
    rolls_max <- rolls_max[sort_ord]
    iteration <- iteration[sort_ord]
    episodes_max <- episodes_max[sort_ord]
    skip_order <- skip_order[sort_ord]
    case_nm <- case_nm[sort_ord]
    episode_type <- episode_type[sort_ord]
    case_for_recurrence <- case_for_recurrence[sort_ord]
    reference_event <- reference_event[sort_ord]
    skip_if_b4_lengths <- skip_if_b4_lengths[sort_ord]
    case_length_total <- case_length_total[sort_ord]
    wind_id_lst <- lapply(wind_id_lst, function(x){x[sort_ord]})
    ep_l <- lapply(ep_l, function(x){x[sort_ord]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[sort_ord]})

    ld_episode_type <- ld_episode_type[sort_ord]
    ld_custom_sort <- ld_custom_sort[sort_ord]
    ld_skip_order <- ld_skip_order[sort_ord]
    ld_reference_event <- ld_reference_event[sort_ord]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[sort_ord]
    ld_case_length_total <- ld_case_length_total[sort_ord]

    if(isTRUE(any_rolling_epi_curr)){
      roll_n <- roll_n[sort_ord]
      recurrence_length_total <- recurrence_length_total[sort_ord]
      rc_l <- lapply(rc_l, function(x){x[sort_ord]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[sort_ord]})

      ld_recurrence_length_total <- ld_recurrence_length_total[sort_ord]
      ld_case_for_recurrence <- ld_case_for_recurrence[sort_ord]
    }

    current_tot <- length(tag)

    # Reference (index) events and options
    lgk <- !duplicated(cri, fromLast = TRUE)
    lgk_i <- which(!duplicated(cri, fromLast = TRUE))
    rep_lgk <- match(cri, cri[lgk])

    tr_date <- (date[lgk_i])[rep_lgk]
    tr_tag <- (tag[lgk_i])[rep_lgk]
    tr_rec_from_last <- (reference_event[lgk_i])[rep_lgk]
    tr_episode_type <- (episode_type[lgk_i])[rep_lgk]
    tr_skip_if_b4_lengths <- (skip_if_b4_lengths[lgk_i])[rep_lgk]
    tr_case_length_total <- (case_length_total[lgk_i])[rep_lgk]
    tr_e <- (e[lgk_i])[rep_lgk]
    tr_skip_order <- (skip_order[lgk_i])[rep_lgk]
    tr_custom_sort <- (custom_sort[lgk_i])[rep_lgk]
    if(any(names(case_overlap_methods) == "g") | any(names(case_overlap_methods) == "b")){
      tr_case_overlap_methods <- lapply(case_overlap_methods, function(x){
        (x[lgk_i])[rep_lgk]
      })
    }
    is_new_lgk <- tr_tag == 0
    is_new_idx <- which(is_new_lgk)

    ld_episode_type[is_new_idx] <- tr_episode_type[is_new_idx]
    ld_custom_sort[is_new_idx] <- tr_custom_sort[is_new_idx]
    ld_skip_order[is_new_idx] <- tr_skip_order[is_new_idx]
    ld_skip_if_b4_lengths[is_new_idx] <- tr_skip_if_b4_lengths[is_new_idx]
    ld_case_length_total[is_new_idx] <- tr_case_length_total[is_new_idx]
    ld_reference_event[is_new_idx] <- tr_rec_from_last[is_new_idx]

    ref_rd <- lgk | tag %in% c(-1, -2)
    lgk2 <- (ld_reference_event %in% c("first_event", "last_event") | ld_episode_type == 3) &
      is_new_lgk & lgk == FALSE
    ref_rd[lgk2] <- overlap(date[lgk2], tr_date[lgk2])
    rm(lgk2)
    ref_rd[is.na(ref_rd)] <- FALSE

    cri_indx <- (cri + (0.1 * match(ref_rd, ref_rd[!duplicated(ref_rd)])))
    r2 <- rle(cri_indx)
    cri_indx_ord <- sequence(r2$length)

    if(length(cri_indx_ord[ref_rd]) > 0){
      max_indx_ref <- max(cri_indx_ord[ref_rd]):1
    }else{
      max_indx_ref <- numeric()
    }

    tr_ep_l <- lapply(max_indx_ref, function(y){
      lgk2 <- (ref_rd & cri_indx_ord == y)
      lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
      lapply(ep_l, function(x){
        (x[lgk2])[rep_lgk]
      })
    })

    if(isTRUE(any_rolling_epi_curr)){
      tr_rc_l <- lapply(max_indx_ref, function(y){
        lgk2 <- (ref_rd & cri_indx_ord == y)
        lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
        lapply(rc_l, function(x){
          (x[lgk2])[rep_lgk]
        })
      })
    }

    tr_sn_list <- lapply(max_indx_ref, function(y){
      lgk2 <- (ref_rd & cri_indx_ord == y)
      lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
      (date@gid[lgk2])[rep_lgk]
    })

    if(isTRUE(any_rolling_epi_curr)){
      tr_case_for_recurrence <- (case_for_recurrence[lgk_i])[rep_lgk]
      tr_recurrence_length_total <- (recurrence_length_total[lgk_i])[rep_lgk]
      if(any(names(recurrence_overlap_methods) == "g") | any(names(recurrence_overlap_methods) == "b")){
        tr_recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){
          (x[lgk_i])[rep_lgk]
        })
      }

      ld_case_for_recurrence[is_new_idx] <- tr_case_for_recurrence[is_new_idx]
      ld_recurrence_length_total[is_new_idx] <- tr_recurrence_length_total[is_new_idx]

      lgk_p <- which(tr_tag == -1)
      roll_n[lgk_p] <- roll_n[lgk_p] + 1L
      roll_n[is_new_idx] <- 0L
    }
    epid_n[is_new_idx] <- epid_n[is_new_idx] + 1L

    lgk1 <- epid_n > episodes_max & tag != 2
    case_nm[lgk1] <- -1L
    tag[lgk1] <- 2L
    iteration[which(lgk1 & iteration == 0)] <- ite

    # Implement `skip_order`
    cri_skp <- cri[ld_custom_sort > ld_skip_order]
    cri_skp <- cri_skp[!duplicated(cri_skp)]
    lgk2 <- cri %in% cri_skp
    current_skipped <- length(cri[lgk1 | lgk2])
    tag[lgk2] <- 2L
    case_nm[lgk2] <- -1L
    iteration[lgk2 & iteration == 0] <- ite
    rm(cri_skp); rm(lgk2); rm(lgk1)

    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      current_tagged <- length(tag[tag == 2])
      current_skipped <- length(tag[tag == 0]) + current_skipped
      if(!display %in% c("none")){
        rp_data <- di_report(tm_ia, "Pre-tracking", current_tot, current_tagged, current_skipped)
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          cat(paste0("Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                     "Assigned: ", fmt(rp_data[[4]]), " record(s)\n",
                     "Skipped: ", fmt(rp_data[[5]]), " record(s)\n",
                     "Time: ", fmt(rp_data[[2]], "difftime"),
                     "\n\n"))
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data[[2]], "difftime"), ")"))
        }
        tm_ia <- Sys.time()
      }
      break
    }

    tr_sn <- tr_date@gid
    if(any(names(case_overlap_methods) == "g") | any(names(case_overlap_methods) == "b")){
      # Change `overlap_method_c` to episode-level (g) or both (b) record and episode-level options
      ov_mth_a <- mapply(opt_level, names(case_overlap_methods), case_overlap_methods, tr_case_overlap_methods, SIMPLIFY = FALSE)
    }else{
      ov_mth_a <- case_overlap_methods
    }

    # Check `recurrence_length`s
    if(isTRUE(any_rolling_epi_curr)){
      if(any(names(recurrence_overlap_methods) == "g") | any(names(recurrence_overlap_methods) == "b")){
        # Change `overlap_method_r` to episode-level (g) or both (b) record and episode-level options
        ov_mth_b <- mapply(opt_level, names(recurrence_overlap_methods), recurrence_overlap_methods, tr_recurrence_overlap_methods, SIMPLIFY = FALSE)
      }else{
        ov_mth_b <- recurrence_overlap_methods
      }
    }
    # Check `case_length`s
    ords <- rep(list(lapply(1:length(tr_ep_l[[1]]), function(x) rep(x, current_tot))), length(tr_ep_l))
    ref_wind <- tr_sn_list[[1]]
    checks_lgk <- rep(0L, length(date))
    ep_checks <- lapply(1:length(tr_ep_l), function(i){
      if(i == 1){
        curr_check_lgk <- tr_tag %in% c(0, -2)
      }else{
        curr_check_lgk <- ref_wind != tr_sn_list[[i]] & tr_tag %in% c(0, -2)
      }
      if(length(curr_check_lgk[curr_check_lgk]) > 0){
        curr_result_lgk <- as.matrix(mapply(ovr_chks,
                                            rep(list(date[curr_check_lgk]), length(tr_ep_l[[i]])),
                                            lapply(tr_ep_l[[i]], function(x) x[curr_check_lgk]),
                                            lapply(ov_mth_a, function(x) as.vector(x[curr_check_lgk])),
                                            lapply(ords[[i]], function(x) x[curr_check_lgk])))
        if(length(date) == 1){
          curr_result_lgk <- t(curr_result_lgk)
        }
        checks_lgk[curr_check_lgk] <-  Rfast::rowMaxs(curr_result_lgk, value = TRUE)
      }
      as.integer(checks_lgk)
    })

    cr <- lapply(1:length(ep_checks), function(i){
      tr_tag %in% c(0) &
        (ref_rd  | (ep_checks[[i]] >= 1)) &
        tag != 2
    })
    if(length(cr) == 1){
      vr <- cr[[1]] & ep_checks[[1]]
    }else{
      vr <- as.logical(Rfast::rowMaxs(sapply(cr, function(x) as.numeric(x)), value = TRUE)) & as.logical(Rfast::rowMaxs(sapply(ep_checks, function(x) as.numeric(x)), value = TRUE))
    }

    cr <- lapply(1:length(ep_checks), function(i){
      if(isTRUE(any_epl_min_curr)){
        cr <- cr[[i]]
        ep_checks <- ep_checks[[i]]
        dst <- rle(sort(cri[cr & !duplicated(ep_checks) & ep_checks != 0]))
        ep_phits <- rep(0L, current_tot)
        ep_phits[cr] <- dst$lengths[match(cri[cr], dst$values)]
        cr[!ref_rd & cr & !(ep_phits >= as.numeric(ld_case_length_total@start) & ep_phits <= as.numeric(right_point(ld_case_length_total)))] <- FALSE
        cr
      }else{
        cr[[i]]
      }
    })

    if(length(cr) == 1){
      cr <- cr[[1]]
    }else{
      cr <- as.logical(Rfast::rowMaxs(sapply(cr, function(x) as.numeric(x)), value = TRUE))
    }

    # browser()
    # Implement `case_sub_criteria`
    checks_lgk <- rep(FALSE, length(date))
    if(class(case_sub_criteria) == "sub_criteria"){
      c_sub_cri <- lapply(1:max(cri_indx_ord[ref_rd]), function(i){
        cri_2 <- cri + (cr/10)
        cri_2 <- !duplicated(cri_2, fromLast = TRUE) & !duplicated(cri_2, fromLast = FALSE) & skip_unique_strata
        if(length(cr[cr & !cri_2]) > 0){
          ref_rd[ref_rd & cri_indx_ord != i] <- FALSE
          pos_repo <- make_batch_pairs(strata = cri[cr & !cri_2],
                                       index_record = ref_rd[cr & !cri_2],
                                       sn = order(order(date@id))[cr & !cri_2])
          # Check the `sub_criteria`
          checks_lgk[cr & !cri_2] <- eval_sub_criteria(x = case_sub_criteria,
                                                       x_pos = pos_repo$x_pos,
                                                       y_pos = pos_repo$y_pos)[[1]]
          # checks_lgk[ref_rd] <- 1L
          checks_lgk
          rm(pos_repo)
        }
        checks_lgk
      })

      if(length(c_sub_cri) == 1){
        c_sub_cri <- as.logical(c_sub_cri[[1]]) & vr
      }else{
        c_sub_cri <- as.logical(Rfast::rowMaxs(sapply(c_sub_cri, function(x) as.numeric(x)), value = TRUE)) & vr
      }
      cr[!c_sub_cri & cr & !ref_rd & tr_tag %in% c(0, -2)] <- FALSE
    }else{
      c_sub_cri <- FALSE
    }

    if(isTRUE(any_rolling_epi_curr)){
      lgk <- is_new_lgk & !cri %in% cri[vr & !ref_rd] & case_nm != -1L & !is.na(case_nm) & roll_n < rolls_max
      tr_tag[lgk] <- -1L
      tag[lgk & ref_rd] <- -1L
      case_nm[lgk & ref_rd] <- 0L
      wind_nm[lgk & ref_rd] <- 0L
      roll_n[lgk] <- roll_n[lgk] + 1L

      # Check `case_length`s
      ords <- rep(list(lapply(1:length(tr_rc_l[[1]]), function(x) rep(x, current_tot))), length(tr_rc_l))
      ref_wind <- tr_sn_list[[1]]
      rc_checks <- lapply(1:length(tr_rc_l), function(i){
        if(i == 1){
          curr_check_lgk <- tr_tag %in% c(-1)
        }else{
          curr_check_lgk <- ref_wind != tr_sn_list[[i]] & tr_tag %in% c(-1)
        }
        if(length(curr_check_lgk[curr_check_lgk]) > 0){
          curr_result_lgk <- as.matrix(mapply(ovr_chks,
                                              rep(list(date[curr_check_lgk]), length(tr_rc_l[[i]])),
                                              lapply(tr_rc_l[[i]], function(x) x[curr_check_lgk]),
                                              lapply(ov_mth_b, function(x) as.vector(x[curr_check_lgk])),
                                              lapply(ords[[i]], function(x) x[curr_check_lgk])))
          if(length(date) == 1){
            curr_result_lgk <- t(curr_result_lgk)
          }
          checks_lgk[curr_check_lgk] <-  Rfast::rowMaxs(curr_result_lgk, value = TRUE)
        }

        as.integer(checks_lgk)
      })

      cr2 <- lapply(1:length(rc_checks), function(i){
        (
          (tr_tag %in% c(-1) & (ref_rd | (rc_checks[[i]] >= 1))) |
            (tr_tag %in% c(-2) & (ref_rd | (ep_checks[[i]] >= 1)))
        ) &
          tag != 2
      })

      if(length(cr2) == 1){
        vr2 <- cr2[[1]] & rc_checks[[1]]
      }else{
        vr2 <- as.logical(Rfast::rowMaxs(sapply(cr2, function(x) as.numeric(x)), value = TRUE)) & as.logical(Rfast::rowMaxs(sapply(rc_checks, function(x) as.numeric(x)), value = TRUE))
      }

      cr2 <- lapply(1:length(rc_checks), function(i){
        if(isTRUE(any_rcl_min_curr)){
          cr2 <- cr2[[i]]
          ep_checks <- ep_checks[[i]]
          rc_checks <- rc_checks[[i]]

          dst <- rle(sort(cri[cr2 & (
            (!duplicated(ep_checks) & ep_checks != 0 & tr_tag == -2) |
              (!duplicated(rc_checks) & rc_checks != 0 & tr_tag == -1)
          )
          ]))
          rc_phits <- rep(0, current_tot)
          rc_phits[cr2] <- dst$lengths[match(cri[cr2], dst$values)]
          cr2[!ref_rd & cr2 & !(rc_phits >= as.numeric(ld_recurrence_length_total@start) & rc_phits <= as.numeric(right_point(ld_recurrence_length_total)))] <- FALSE
          cr2
        }else{
          cr2[[i]]
        }
      })

      if(length(cr2) == 1){
        cr2 <- cr2[[1]]
      }else{
        cr2 <- as.logical(Rfast::rowMaxs(sapply(cr2, function(x) as.numeric(x)), value = TRUE))
      }

      if(class(recurrence_sub_criteria) == "sub_criteria"){
        r_sub_cri <- lapply(1:max(cri_indx_ord[ref_rd]), function(i){
          cri_2 <- cri + (cr2/10)
          cri_2 <- !duplicated(cri_2, fromLast = TRUE) & !duplicated(cri_2, fromLast = FALSE) & skip_unique_strata
          if(length(cr2[cr2 & !cri_2]) > 0){
            ref_rd[ref_rd & cri_indx_ord != i] <- FALSE
            pos_repo <- make_batch_pairs(strata = cri[cr2 & !cri_2],
                                         index_record = ref_rd[cr2 & !cri_2],
                                         sn = order(order(date@id))[cr2 & !cri_2])
            # Check the `sub_criteria`
            checks_lgk[cr2 & !cri_2] <- eval_sub_criteria(x = recurrence_sub_criteria,
                                                          x_pos = pos_repo$x_pos,
                                                          y_pos = pos_repo$y_pos)[[1]]
            # checks_lgk[ref_rd & cr2 & !cri_2] <- 1L
            checks_lgk
          }
          rm(pos_repo)
          checks_lgk
        })

        if(length(r_sub_cri) == 1){
          r_sub_cri <- as.logical(r_sub_cri[[1]]) & vr2
        }else{
          r_sub_cri <- as.logical(Rfast::rowMaxs(sapply(r_sub_cri, function(x) as.numeric(x)), value = TRUE)) & vr2
        }
        cr2[!r_sub_cri & cr2 & !ref_rd & tr_tag %in% c(-1)] <- FALSE
      }else{
        r_sub_cri <- FALSE
      }

      cr[!cr & cr2] <- TRUE
      vr[!vr & vr2] <- TRUE
      rm(cr2); rm(vr2)
    }

    # Episode and window IDs
    e[cr & tag == 0 & tr_tag %in% c(0)] <- tr_sn[cr & tag == 0 & tr_tag %in% c(0)]
    wind_id[cr & tag == 0] <- tr_sn[cr & tag == 0]

    if(length(wind_id_lst) < length(tr_sn_list)){
      wind_id_lst <- c(wind_id_lst,
                       rep(wind_id_lst[1], (length(tr_sn_list) - length(wind_id_lst))))
    }else if(length(tr_sn_list) < length(wind_id_lst)){
      tr_sn_list <- c(tr_sn_list,
                      rep(tr_sn_list[1], (length(wind_id_lst) - length(tr_sn_list))))
    }

    wind_id_lst <- lapply(1:length(wind_id_lst), function(i){
      x <- wind_id_lst[[i]]
      x[cr & tag == 0] <- (tr_sn_list[[i]])[cr & tag == 0]
      x
    })

    e[cr & tr_tag %in% c(-1, -2)] <- tr_e[cr & tr_tag %in% c(-1, -2)]
    lgk_p <- which(cr & tr_tag %in% c(0))
    lgk_p1 <- which(cr & tr_tag %in% c(0) & !c_sub_cri)
    lgk_p2 <- which(cr & tr_tag %in% c(0) & c_sub_cri)
    case_nm[lgk_p1[ref_rd[lgk_p1]]] <- 0L
    case_nm[lgk_p2[ref_rd[lgk_p2]]] <- 4L
    case_nm[lgk_p[!ref_rd[lgk_p]]] <- 2L
    wind_nm[cr & tr_tag %in% c(0, -2) & is.na(wind_nm)] <- 0L
    new_hits <- cr & tag != 2 & !ref_rd
    tag[cr] <- 2L

    if(isTRUE(any_rolling_epi_curr)){
      case_nm[cr & tr_tag %in% c(-1, -2) & is.na(case_nm)] <- 3L
      wind_nm[cr & tr_tag %in% c(-1) & is.na(wind_nm)] <- 1L
      sort_ord <- order(cri, new_hits, -assign_ord, date@gid)
      t_cri <- cri[sort_ord]
      t_sn <- date@id[sort_ord]
      t_sn <- t_sn[!duplicated(t_cri, fromLast = TRUE)]
      case_nm[which(date@id %in% t_sn &
                      tr_tag %in% c(-1) &
                      new_hits &
                      !r_sub_cri
      )] <- 1L
      case_nm[which(date@id %in% t_sn &
                      tr_tag %in% c(-1) &
                      new_hits &
                      r_sub_cri
      )] <- 5L

      sort_ord <- order(cri, -tag)
      t_cri <- cri[sort_ord]
      t_tag <- tag[sort_ord]
      lgk <- !duplicated(t_cri, fromLast = TRUE)
      last_tag <- (t_tag[lgk])[match(t_cri, t_cri[lgk])]

      last_tag <- last_tag[match(date@id, date@id[sort_ord])]
      rm(t_tag); rm(t_cri)
      close_epi <- last_tag == 2

      roll_ref <- assign_ord
      if(isTRUE(any_rec_from_last)) roll_ref[ld_reference_event %in% c("first_record", "first_event")] <- -roll_ref[ld_reference_event %in% c("first_record", "first_event")]

      # Reference event for recurrence window
      t_cr <- cr
      t_cr[ref_rd & roll_n >= 1] <- FALSE
      sort_ord <- order(cri, t_cr, tag, roll_ref, date@gid)
      t_cri <- cri[sort_ord]
      t_sn <- date@id[sort_ord]
      t_lgk <- which(!duplicated(t_cri, fromLast = TRUE))
      t_sn <- t_sn[t_lgk]

      lgk <- rep(FALSE, length(date))
      if(any(ld_reference_event %in% c("first_event", "last_event"))){
        tr_t_date <- ((date[sort_ord])[t_lgk])[match(t_cri, t_cri[t_lgk])]
        tr_t_tag <- ((tag[sort_ord])[t_lgk])[match(t_cri, t_cri[t_lgk])]
        lgk2 <- which(ld_reference_event %in% c("first_event", "last_event") &
                        ld_episode_type != 3 &
                        tr_t_tag %in% c(-1, -2, 2))
        lgk[lgk2] <- overlap(date[lgk2], tr_t_date[lgk2])
        rm(lgk2)
      }

      if(any(ld_episode_type == 3)){
        lgk[new_hits & !lgk & ld_episode_type == 3] <- TRUE
      }

      tag[which((date@id %in% t_sn | lgk) &
                  tag != 0 &
                  !close_epi &
                  roll_n < rolls_max &
                  t_cr &
                  ld_episode_type %in% c(2, 3))] <- -1L

      if(isTRUE(any_case_for_rec)){
        # Reference event for recurrence window - `case_for_recurrence`
        tag[which((date@id %in% t_sn | lgk) &
                    !(ref_rd & roll_n >= 1) &
                    tr_tag == -1 &
                    !close_epi &
                    roll_n <= rolls_max &
                    t_cr &
                    ld_episode_type %in% c(2, 3) &
                    ld_case_for_recurrence == TRUE)] <- -2L
      }
    }

    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      current_tagged <- length(tag[tag == 2])
      current_skipped <- length(tag[tag == 0]) + current_skipped
      if(!display %in% c("none")){
        rp_data <- di_report(tm_ia, ite, current_tot, current_tagged, current_skipped)
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          cat(paste0("Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                     "Assigned: ", fmt(rp_data[[4]]), " record(s)\n",
                     "Skipped: ", fmt(rp_data[[5]]), " record(s)\n",
                     "Time: ", fmt(rp_data[[2]], "difftime"),
                     "\n\n"))
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data[[2]], "difftime"), ")"))
        }
        tm_ia <- Sys.time()
      }
      iteration[iteration == 0] <- ite
      break
    }

    # Events in between `case_length`s and `recurrence_length`s
    if(isTRUE(any_skip_b4_len)){
      lgk <- ld_skip_if_b4_lengths & tag != 2 & tr_tag %in% c(0, -2)
      lgk <- check_skips(lgk = lgk,
                         cri = cri,
                         cr = cr,
                         vr = vr,
                         tr_ep_l = unlist(tr_ep_l, recursive = FALSE),
                         tr_date = tr_date,
                         date = date,
                         case_nm = case_nm)
      if(isTRUE(any_rolling_epi_curr)){
        lgk2 <- ld_skip_if_b4_lengths & tag != 2 & tr_tag %in% c(-1)
        lgk2 <- check_skips(lgk = lgk2,
                            cri = cri,
                            cr = cr,
                            vr = vr,
                            tr_ep_l = unlist(tr_rc_l, recursive = FALSE),
                            tr_date = tr_date,
                            date = date,
                            case_nm = case_nm)
        lgk <- c(lgk, lgk2)
        lgk <- lgk[!duplicated(lgk)]
      }

      case_nm[lgk] <- -1L
      tag[lgk] <- 2L
      current_skipped <- current_skipped + length(lgk[lgk])
    }
    iteration[tag != 0 & iteration == 0] <- ite
    current_tagged <- length(tag[tag == 2])
    if(!display %in% c("none")){
      rp_data <- di_report(tm_ia, ite, current_tot, current_tagged - current_skipped, current_skipped)
      report <- c(report, list(rp_data))
      if(display %in% c("stats_with_report", "stats")){
        cat(paste0("Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                   "Assigned: ", fmt(rp_data[[4]]), " record(s)\n",
                   "Skipped: ", fmt(rp_data[[5]]), " record(s)\n",
                   "Time: ", fmt(rp_data[[2]], "difftime"),

                   "\n\n"))
      }
      tm_ia <- Sys.time()
    }

    # Subset out all linked records
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)
    rtag_lgk <- date@id[tag_lgk]

    epids_repo$e[rtag_lgk] <- e[tag_lgk]
    epids_repo$case_nm[rtag_lgk] <- case_nm[tag_lgk]
    epids_repo$wind_nm[rtag_lgk] <- wind_nm[tag_lgk]
    epids_repo$wind_id[rtag_lgk] <- wind_id[tag_lgk]
    epids_repo$iteration[rtag_lgk] <- iteration[tag_lgk]
    epids_repo$date[rtag_lgk] <- date[tag_lgk]
    epids_repo$tag[rtag_lgk] <- tag[tag_lgk]

    e <- e[ntag_lgk]
    cri <- cri[ntag_lgk]
    assign_ord <- assign_ord[ntag_lgk]
    epid_n <- epid_n[ntag_lgk]
    custom_sort <- custom_sort[ntag_lgk]
    skip_order <- skip_order[ntag_lgk]
    case_nm <- case_nm[ntag_lgk]
    wind_nm <- wind_nm[ntag_lgk]
    wind_id <- wind_id[ntag_lgk]
    rolls_max <- rolls_max[ntag_lgk]
    iteration <- iteration[ntag_lgk]
    episodes_max <- episodes_max[ntag_lgk]
    tag <- tag[ntag_lgk]
    episode_type <- episode_type[ntag_lgk]
    reference_event <- reference_event[ntag_lgk]
    skip_if_b4_lengths <- skip_if_b4_lengths[ntag_lgk]
    case_length_total <- case_length_total[ntag_lgk]
    ep_l <- lapply(ep_l, function(x){x[ntag_lgk]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[ntag_lgk]})

    if(!is.null(case_sub_criteria) & length(ntag_lgk) > 0){
      case_sub_criteria <- reframe(case_sub_criteria, func = function(x) x[sort(order(order(date@id))[ntag_lgk])])
    }

    ld_reference_event <- ld_reference_event[ntag_lgk]
    ld_episode_type <- ld_episode_type[ntag_lgk]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[ntag_lgk]
    ld_case_length_total <- ld_case_length_total[ntag_lgk]

    if(length(epids_repo$wind_id_lst) < length(wind_id_lst)){
      w_diff <- length(wind_id_lst) - length(epids_repo$wind_id_lst)
      epids_repo$wind_id_lst <- c(epids_repo$wind_id_lst,
                                  rep(list(rep(NA_real_, inp_n)), w_diff))
    }
    epids_repo$wind_id_lst <- lapply(seq_len(length(wind_id_lst)), function(i){
      epids_repo$wind_id_lst[[i]][rtag_lgk] <- wind_id_lst[[i]][tag_lgk]
      epids_repo$wind_id_lst[[i]]
    })

    wind_id_lst <- lapply(wind_id_lst, function(x) x[ntag_lgk])

    if(isTRUE(any_rolling_epi_curr)){
      roll_n <- roll_n[ntag_lgk]
      case_for_recurrence <- case_for_recurrence[ntag_lgk]
      recurrence_length_total <- recurrence_length_total[ntag_lgk]
      rc_l <- lapply(rc_l, function(x){x[ntag_lgk]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[ntag_lgk]})
      if(!is.null(recurrence_sub_criteria) & length(ntag_lgk) > 0 & any_rolling_epi){
        recurrence_sub_criteria <- reframe(recurrence_sub_criteria, func = function(x) x[sort(order(order(date@id))[ntag_lgk])])
      }

      ld_case_for_recurrence <- ld_case_for_recurrence[ntag_lgk]
      ld_recurrence_length_total <- ld_recurrence_length_total[ntag_lgk]
    }
    date <- date[ntag_lgk]
    ite <- ite + 1L
    if (display %in% c("progress_with_report", "progress")) {
      progress_bar(length(epids_repo$tag[epids_repo$tag == 2]),
                   inp_n, 100,
                   msg = paste0("Iteration ",
                                fmt(ite), " (",
                                fmt(difftime(Sys.time(), tm_ia), "difftime"),
                                ")"))
      tm_ia <- Sys.time()
    }
    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      if(!display %in% c("none")){
        rp_data <- di_report(tm_ia, ite, NA_real_, NA_real_, length(tag[tag == 0]))
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          cat(paste0("Skipped: ", fmt(rp_data[[5]]), " record(s)\n",
                     "Time: ", fmt(rp_data[[2]], "difftime"),
                     "\n\n"))
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data[[2]], "difftime"), ")"))
        }
        tm_ia <- Sys.time()
      }
      break
    }
  }
  if(!display %in% c("none_with_report", "none")) cat("\n")

  if(length(e) > 0){
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)
    rtag_lgk <- date@id[tag_lgk]

    epids_repo$e[rtag_lgk] <- e[tag_lgk]
    epids_repo$case_nm[rtag_lgk] <- case_nm[tag_lgk]
    epids_repo$wind_nm[rtag_lgk] <- wind_nm[tag_lgk]
    epids_repo$wind_id[rtag_lgk] <- wind_id[tag_lgk]
    epids_repo$date[rtag_lgk] <- date[tag_lgk]
    epids_repo$tag[rtag_lgk] <- tag[tag_lgk]
    epids_repo$iteration[rtag_lgk] <- iteration[tag_lgk]

    e <- e[tag_lgk]
    case_nm <- case_nm[tag_lgk]
    wind_nm <- wind_nm[tag_lgk]
    wind_id <- wind_id[tag_lgk]
    date <- date[tag_lgk]
    tag <- tag[tag_lgk]

    if(length(epids_repo$wind_id_lst) < length(wind_id_lst)){
      w_diff <- length(wind_id_lst) - length(epids_repo$wind_id_lst)
      epids_repo$wind_id_lst <- c(epids_repo$wind_id_lst,
                                  rep(list(rep(NA_real_, inp_n)), w_diff))
    }
    epids_repo$wind_id_lst <- lapply(seq_len(length(wind_id_lst)), function(i){
      epids_repo$wind_id_lst[[i]][rtag_lgk] <- wind_id_lst[[i]][tag_lgk]
      epids_repo$wind_id_lst[[i]]
    })
  }

  names(epids_repo$wind_id_lst) <- paste0("wind_id", 1:length(epids_repo$wind_id_lst))

  # Collate all linked records
  e <- epids_repo$e
  case_nm <- epids_repo$case_nm
  wind_nm <- epids_repo$wind_nm
  wind_id <- epids_repo$wind_id
  iteration <- epids_repo$iteration
  date <- epids_repo$int

  wind_nm[which(case_nm == -1L & !is.na(case_nm))] <- -1L
  qfx <- wind_id + wind_nm/10
  qfx <- qfx[!duplicated(qfx) & wind_nm == 1L]
  wind_nm[wind_id %in% as.integer(qfx)] <- 1L
  rm(qfx)

  epid_unit <- epid_unit[match(date@id, seq_len(inp_n))]
  # `dist_epid_index` and `dist_wind_index`
  stat_pos <- date@id
  sort_ord <- order(e, wind_id, as.numeric(date@start))
  e <- e[sort_ord]
  date <- date[sort_ord]
  r <- rle(e)
  epid_n <- rep(r$lengths, r$lengths)
  lgk <- match(r$values, date@gid)
  dist_epid_index <- ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
    rep(((as.numeric(date@start[lgk]) + as.numeric(right_point(date[lgk]))) * .5),  r$lengths)

  if(isTRUE(any_rolling_epi)){
    wind_id <- wind_id[sort_ord]
    r <- rle(wind_id)
    lgk <- match(r$values, date@gid)
    dist_wind_index <- ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
      rep(((as.numeric(date@start[lgk]) + as.numeric(right_point(date[lgk]))) * .5), r$lengths)
  }else{
    dist_wind_index <- dist_epid_index
    wind_id <- e
  }

  # Units for `dist_epid_index` and `dist_wind_index`
  lgk_p <- which(epid_unit %in% c(1, 2))
  diff_unit <- names(diyar::episode_unit)[epid_unit]
  diff_unit[lgk_p] <- paste0(substr(names(diyar::episode_unit)[epid_unit[lgk_p]], 1 ,3), "s")

  diff_unit[diff_unit %in% c("months","year")] <- "days"
  diff_unit <- diff_unit[!duplicated(diff_unit)]
  if(length(diff_unit) > 1) diff_unit <- "days"

  if(isTRUE(is_dt)){
    dist_epid_index <- dist_epid_index / as.numeric(diyar::episode_unit[epid_unit])
    dist_epid_index <- as.difftime(dist_epid_index, units = diff_unit)

    if (isTRUE(any_rolling_epi)){
      dist_wind_index <- dist_wind_index / as.numeric(diyar::episode_unit[epid_unit])
      dist_wind_index <- as.difftime(dist_wind_index, units = diff_unit)
    }else{
      dist_wind_index <- dist_epid_index
    }
  }

  tmp_pos <- date@id
  fd <- match(1:length(date), tmp_pos)
  f_e <- e[fd]

  retrieve_pos <- match(1:length(date), stat_pos)

  if(class(case_length) != "list"){
    case_length <- list(case_length)
  }
  if(class(recurrence_length) != "list"){
    recurrence_length <- list(recurrence_length)
  }
  # `epid` object
  td1 <- lapply(epids_repo$wind_id_lst, function(y) y[retrieve_pos])
  epids <- new("epid",
               .Data= e[fd],
               dist_epid_index = dist_epid_index[fd],
               dist_wind_index = dist_wind_index[fd],
               sn = date@gid[fd],
               iteration = iteration[retrieve_pos],
               wind_id = td1,
               epid_total = epid_n[fd],
               options = options_lst)

  epids@case_nm <- case_nm[retrieve_pos]
  class(epids@case_nm) <- "d_label"
  attr(epids@case_nm, "value") <- -1L : 5L
  attr(epids@case_nm, "label") <- c("Skipped", "Case", "Recurrent", "Duplicate_C", "Duplicate_R", "Case_CR", "Recurrent_CR")
  attr(epids@case_nm, "state") <- "encoded"

  epids@wind_nm <- wind_nm[retrieve_pos]
  class(epids@wind_nm) <- "d_label"
  attr(epids@wind_nm, "value") <- -1L : 1L
  attr(epids@wind_nm, "label") <- c("Skipped", "Case", "Recurrence")
  attr(epids@wind_nm, "state") <- "encoded"

  if(isTRUE(group_stats)){
    # `epid_interval` slot
    lgk <- which(epid_n != 1)
    dts_a <- lapply(split(as.numeric(date@start[lgk]), e[lgk]), min)
    dts_z <- lapply(split(as.numeric(right_point(date[lgk])), e[lgk]), max)

    dts_a <- as.numeric(dts_a)[match(e[lgk], names(dts_a))]
    dts_z <- as.numeric(dts_z)[match(e[lgk], names(dts_z))]

    case_nm <- case_nm[sort_ord]
    frm_last <- from_last[match(tmp_pos[fd], pri_pos)]
    epid_dt_a <- as.numeric(date@start)
    epid_dt_a[frm_last] <- as.numeric(right_point(date[frm_last]))
    epid_dt_z <- as.numeric(right_point(date))
    epid_dt_z[frm_last] <- as.numeric(date@start[frm_last])

    epid_dt_a[lgk] <- dts_a
    epid_dt_a[lgk[frm_last[lgk]]] <- dts_z[frm_last[lgk]]
    epid_dt_z[lgk] <- dts_z
    epid_dt_z[lgk[frm_last[lgk]]] <- dts_a[frm_last[lgk]]

    if(isTRUE(is_dt)){
      epid_dt_a <- as.POSIXct(epid_dt_a, tz = "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_dt_z <- as.POSIXct(epid_dt_z, tz = "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_l <- difftime(epid_dt_z, epid_dt_a, units = diff_unit)
    }else{
      epid_l <- epid_dt_z - epid_dt_a
    }

    epids@epid_interval <- number_line(l = epid_dt_a[fd],
                                       r = epid_dt_z[fd],
                                       gid = f_e)
    # `epid_length` slot
    epids@epid_length <- epid_l[fd]
  }

  # `epid_dataset` slot
  if(!is.null(data_source)){
    data_source <- data_source[match(tmp_pos[fd], seq_len(inp_n))]
    # Data links
    rst <- check_links(e[fd], data_source, data_links)
    epids@epid_dataset <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epids <- suppressWarnings(delink(epids, !req_links))
      epids@epid_dataset[!req_links] <- data_source[!req_links]
    }
    epids@epid_dataset <- encode(epids@epid_dataset)
  }

  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    epids <- list(epid = epids, report = as.list(do.call("rbind", lapply(report, as.data.frame))))
    class(epids$report) <- "d_report"
  }
  tms <- difftime(Sys.time(), tm_a)
  if(!display %in% c("none_with_report", "none")) cat("Episodes tracked in ", fmt(tms, "difftime"), "!\n", sep = "")
  rm(list = ls()[ls() != "epids"])
  return(epids)
}

#' @name episodes_wf_splits
#' @title Track episodes in a reduced dataset.
#'
#' @description Excludes duplicate records from the same day or period prior before passing the analysis to \code{\link{episodes}}.
#' Only duplicate records that will not affect the case definition are excluded.
#' The resulting episode identifiers are recycled for the duplicate records.
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
#' \bold{\code{episodes_wf_splits()}} is a wrapper function of \bold{\code{episodes()}} which reduces or re-frames the dataset to
#' the minimum number of records required to implement a case definition.
#' This leads to the same outcome but with the benefit of a shorter processing time.
#'
#' Duplicate records from the same point or period in time are excluded from \bold{\code{episodes()}}.
#' The resulting \code{\link[=epid-class]{epid}} object is then recycled for the duplicates.
#'
#' The \code{duplicates_recovered} argument determines which identifiers are recycled.
#' If \code{"without_sub_criteria"} is selected, only identifiers created from a matched \code{\link{sub_criteria}} (\code{"Case_CR"} and \code{"Recurrent_CR"}) are recycled.
#' The opposite (\code{"Case"} and \code{"Recurrent"}) is the case if \code{"with_sub_criteria"} is selected.
#' Excluded duplicates of \code{"Duplicate_C"} and \code{"Duplicate_R"} are always recycled.
#'
#' The \code{reframe} argument will either \code{\link{reframe}} or subset a \code{\link{sub_criteria}}.
#' Both will require slightly different functions for \code{match_funcs} or \code{equal_funcs}.
#'
#' @examples
#' # With 10,000 duplicate records of 20 events,
#' # `episodes_wf_splits()` will take less time than `episodes()`
#' dates <- seq(from = as.Date("2019-04-01"), to = as.Date("2019-04-20"), by = 1)
#' dates <- rep(dates, 10000)
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
    if(all(class(x) == "sub_criteria")){
      max(attr_eval(x))
    }else if(all(class(x) == "list")){
      max(as.numeric(lapply(x, length)))
    }else {
      length(x)
      }
  }
  args_len <- unlist(lapply(combi_opt_lst, check_lens), use.names = FALSE)
  combi_opt_lst <- combi_opt_lst[args_len > 1]

  if(length(combi_opt_lst) != 0){
    combi_opt_lst <- lapply(combi_opt_lst, function(x){
      if(all(class(x) == "sub_criteria")){
        attr_eval(x, func = identity, simplify = FALSE)
      }else if(all(class(x) == "list")){
        x
      }else{
        list(x)
      }
    })

    combi_opt_lst <- unlist(combi_opt_lst, recursive = FALSE, use.names = FALSE)
    combi_opt_lst <- lapply(combi_opt_lst, function(x){
      if(all(class(x) == "number_line")){
        list(x@start, x@.Data)
      }else{
        x
      }
    })
    # sub_criteria with d_attributes/number_line objects
    lgk <- unlist(lapply(combi_opt_lst, function(x) all(class(x) == "list")), use.names = FALSE)
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
    if(all(class(opt_lst[[i]]) == "name")) {
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
    if(all(class(opt_lst[[i]]) == "sub_criteria")) {
      if(reframe){
        return(reframe(opt_lst[[i]], func = function(x) split(x, cmbi_cd)))
      }else{
        return(reframe(opt_lst[[i]], func = function(x) x[!rf_lgk]))
      }
    }else if(all(class(opt_lst[[i]]) == "list")){
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
    rp_data <- di_report(tm_a, "Remove duplicates", current_tot = length(rf_lgk), current_skipped = length(rf_lgk[rf_lgk]))
    report_a <- rp_data
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0("Remove duplicates\n",
                 "Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                 "Skipped: ", fmt(rp_data[[5]]), " record(s)","\n",
                 "Time: ", fmt(rp_data[[2]], "difftime"),
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
    rp_data <- di_report(tm_a, "Return duplicates", current_tot = length(date), current_tagged = length(rf_lgk[rf_lgk]))
    report_b <- rp_data
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0("\n\n",
                 "Return duplicates\n",
                 "Checked: ", fmt(rp_data[[3]]), " record(s)\n",
                 "Assigned: ", fmt(rp_data[[4]]), " record(s)","\n",
                 "Time: ", fmt(rp_data[[2]], "difftime"),
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
  if(class(lengths) != "number_line"){
    lengths <- number_line(0, as.numeric(lengths))
  }
  is_dt <- ifelse(!any(class(date@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
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
  is_dt1 <- ifelse(!any(class(date@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
  if(isTRUE(is_dt1)){
    date <- number_line(
      l = as.POSIXct(date@start, tz = "GMT"),
      r = as.POSIXct(right_point(date), tz = "GMT")
    )
  }
  is_dt2 <- ifelse(!any(class(windows@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), FALSE, TRUE)
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
