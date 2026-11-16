#' @name episodes
#' @title Group dated events into episodes.
#'
#' @description Assigns dated events (records) within a specified period of each
#' other to the same group ("episode"). Episodes can represent repeated infections,
#' hospital admissions, outbreaks, or other recurring events.
#' Each episode has a unique ID. Episodes maybe based on a \code{"fixed"} window or
#' a \code{"rolling"} ("recurring") window which extends the episode's length.
#' Each episode has a \code{"Case"} and/or \code{"Recurrent"} (index) record,
#' while all other records within the group are a treated as a \code{"Duplicate"} of either
#' the \code{"Case"} or \code{"Recurrent"} event.
#'
#' @param sn \code{[integer]}. Unique record ID. If provided, group IDs are based on corresponding \code{sn}.
#' @param strata \code{[atomic]}. Subsets of the dataset. Episodes are created separately by each \code{strata}.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Date or period.
#' @param case_length \code{[integer|\link{number_line}]}. Duration from an index event distinguishing one \code{"Case"} from another. This will equal the episode length if \code{episode_type} is \code{"fixed"}.
#' @param episodes_max \code{[integer]}. Maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{[character]}. Options are \code{"fixed"} (default) or \code{"rolling"}. See \code{Details}.
#' @param recurrence_length \code{[integer|\link{number_line}]}. Duration from an index event distinguishing a \code{"Recurrent"} event from its \code{"Case"} or prior \code{"Recurrent"} events.
#' @param episode_unit \code{[character]}. Time unit for \code{case_length} and \code{recurrence_length}. Options are \code{"seconds"}, \code{"minutes"}, \code{"hours"}, \code{"days"} (default), \code{"weeks"}, \code{"months"} or \code{"years"}. See \code{\link{epid_units}} for conversion rates.
#' @param rolls_max \code{[integer]}. Maximum number of times an index event can recur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source \code{[character]}. Source ID for each record. If provided, a list of all sources in each episode is returned to the \code{\link[=epid-class]{epid_dataset}} slot.
#' @param from_last \code{[logical]}. Track episodes beginning from the earliest to the most recent record (\code{FALSE}) or vice versa (\code{TRUE}).
#' @param case_overlap_methods \code{[character|integer]}. Specific ways a period (record) must overlap with a \code{"Case"} event. See (\code{\link{overlaps}}).
#' @param recurrence_overlap_methods \code{[character|integer]}. Specific ways a period (record) must overlap with the index of a \code{"Recurrent"} event. See (\code{\link{overlaps}}).
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index events. See \code{\link{custom_sort}}.
#' @param group_stats \code{[character|logical]}. A selection of group metrics returned with each episode. Most are added to slots of the \code{\link[=epid-class]{epid}} object.
#' Options are \code{NULL} (for none), \code{TRUE} (for all options), or any combination of \code{"case_nm"}, \code{"wind"} and \code{"epid_interval"}.
#' The \code{FALSE} option is included for backward compatibility and is equivalent to \code{"case_nm"}, \code{"wind"} and \code{"epid_interval"}.
#' @param display \code{[character]}. Display progress update and/or generate a linkage report for the analysis. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param reference_event \code{[character]}. Specifies which of the records are used as index events. Options are \code{"last_record"} (default), \code{"last_event"}, \code{"first_record"}. \code{"first_event"} or \code{"all_record"}.
#' @param case_for_recurrence \code{[logical]}. If \code{TRUE}, a \code{case_length} is applied to both \code{"Case"} and \code{"Recurrent"} events.
#' If \code{FALSE} (default), a \code{case_length} is applied to only \code{"Case"} events.
#' @param skip_order \code{[integer]}. End episode tracking in a \code{strata} when an index event's \code{custom_sort} order (value) is greater than the corresponding \code{skip_order}.
#' @param data_links \code{[list|character]}. \code{data_source} required in each episode. Episodes created without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param skip_if_b4_lengths \code{[logical]}. If \code{TRUE} (default), events before a lagged/leading \code{case_length} or \code{recurrence_length} periods are skipped. For example, if the \code{case_length} or \code{recurrence_length} is \code{"2 -> 4"}, records within 0-1 unit-difference from the index event are skipped.
#' @param skip_unique_strata \code{[logical]}. If \code{TRUE}, a strata with a single event is skipped.
#' @param case_sub_criteria \code{[\link{sub_criteria}]}. Additional nested match criteria for events within the \code{case_length} period.
#' @param recurrence_sub_criteria \code{[\link{sub_criteria}]}. Additional nested match criteria for events within the \code{recurrence_length} period.
#' @param case_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{case_lengths} required for an episode.
#' @param recurrence_length_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{recurrence_lengths} required for an episode.
#' @param batched \code{[character]}. Tracks episodes in batches. Options are \code{"yes"}, \code{"no"}, and \code{"semi"} (default).
#' Typically, (\code{"semi"}) leads to higher peak memory usage but shorter run-times while (\code{"no"}) leads to lower peak memory usage but longer run-times
#' @param splits_by_strata \code{[integer]}. Split analysis into \code{n} parts. This typically leads to a lower peak memory usage but longer run-times.
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{episodes_af_shift}};
#' \code{\link{episodes_af_links}}; \code{\link{episodes_wf_repeats}};
#' \code{\link{episodes_wf_splits}}; \code{\link{custom_sort}};
#' \code{\link{sub_criteria}}; \code{\link[=windows]{epid_length}};
#' \code{\link[=windows]{epid_window}}; \code{\link{partitions}};
#' \code{\link{links}}; \code{\link{overlaps}};
#'
#' @details
#' \bold{\code{episodes()}} links records in iterations.
#' Every record is linked to a unique group (episode; \code{\link[=epid-class]{epid}} object).
#' These episodes represent occurrences of interest as specified by function's arguments
#' and defined by the user's case definition.
#'
#' Two type of episodes are possible:
#' \itemize{
#' \item \code{"fixed"} - An episode where all events are within a fixed period of an index event.
#' \item \code{"rolling"} - An episode where all events are within a recurring period of an index event.
#' }
#'
#' Every record in each episode is categorised as one of the following:
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
#' If \code{data_links} is supplied, every element of the list should be named
#' \code{"l"} (links) or \code{"g"} (groups). Unnamed elements are assumed to be \code{"l"}:
#' \itemize{
#' \item If named \code{"l"}, groups without records from every listed \code{data_source} will be unlinked.
#' \item If named \code{"g"}, groups without records from any listed \code{data_source} will be unlinked.
#' }
#'
#' Multiple values for \code{case_length}, \code{recurrence_length}, \code{case_overlaps}
#' and \code{recurrence_overlaps} can be provided. These should be provided as a list.
#' Each element should be named \code{"r"} (record), \code{"w"} (window) or \code{"e"} (episode)
#' based on which level they should be used. Unnamed elements are assumed
#' to be \code{"w"} (\code{case_length} and \code{recurrence_length})
#' or \code{"r"} (\code{case_overlaps}, \code{recurrence_overlaps}).
#'
#' \itemize{
#' \item If named \code{"r"}, each records values is use.
#' \item If named \code{"w"}, the current window's index record's value is used.
#' \item If named \code{"e"}, the current episode's index record's value is used.
#' }

#'
#' All records with a missing (\code{NA}) \code{strata} or \code{date} are skipped.
#'
#' There are wrapper functions or alternative implementations of \bold{\code{episodes()}}
#' for specific use cases or benefits:
#' \itemize{
#' \item \bold{\code{episodes_wf_repeats()}} - See \code{\link[=episodes_wf]{episodes_wf_*}} functions.
#' \item \bold{\code{episodes_wf_splits()}} - See \code{\link[=episodes_wf]{episodes_wf_*}} functions.
#' \item \bold{\code{episodes_af_shift()}} - A mostly vectorised approach to episode tracking.
#' \item \bold{\code{episodes_af_links()}} - Episode tracking achieved with \code{\link{links}}.
#' }
#'
#' See \code{vignette("episodes")} for further details.
#'
#' @examples
#' data(infections)
#' data(hospital_admissions)
#'
#' dfr1 <- infections; dfr2 <- hospital_admissions[2:3]
#' dfr1$epi_len[dfr1$infection == 'UTI'] <- 7
#'
#' # Group records by infection infection type
#'   # One 16-day (15-day difference) fixed episode
#' dfr1$ep1 <- episodes(
#'   date = dfr1$date, strata = dfr1$infection,
#'   case_length = 15, episodes_max = 1, group_stats = FALSE)
#'
#'   # Multiple episodes with different episode lengths
#' dfr1$ep2 <- episodes(
#'   date = dfr1$date, strata = dfr1$infection,
#'   case_length = dfr1$epi_len, group_stats = FALSE)
#'
#'   # Multiple 16-day (15-day difference) rolling episodes
#'   # with an 11-day (10-day difference) recurrence window
#' dfr1$ep3 <- episodes(
#'   date = dfr1$date, case_length = 15, episodes_max = Inf,
#'    episode_type = "rolling", recurrence_length = 10,
#'    group_stats = FALSE)
#'
#' # Choose which record is the starting point for episodes
#' start_at_row_6 <- !(dfr1$rd_id == 6)
#'   # with a case_length that includes two separate time ranges:
#'   # 20-12 day-difference before the index event and
#'   # 15-25 day-difference after the index event
#' ns_epi_len <- list(
#'   number_line(-20, -12),  number_line(15, 25))
#' dfr1$ep4 <- episodes(
#'   date = dfr1$date, episodes_max = 1,
#'   case_length = ns_epi_len,
#'   group_stats = FALSE, custom_sort = start_at_row_6)
#'
#'   # OR includes the range without any gaps in time
#' dfr1$ep5 <- episodes(
#'   date = dfr1$date, episodes_max = 1,
#'   case_length = number_line(-20, 25),
#'   group_stats = FALSE, custom_sort = start_at_row_6)
#'
#' dfr1
#'
#' # Periods rather than dates can also be tracked.
#'   # Identify episodes from hospital stays that overlap in specific ways
#' dfr2$admin_period <- number_line(dfr2$admin_dt, dfr2$discharge_dt)
#' match_index <- index_window(dfr2$admin_period)
#' dfr2$ep1 <- episodes(
#'   date = dfr$admin_period, case_length = match_index,
#'   case_overlap_methods = "overlap", group_stats = FALSE)
#'
#' dfr2$ep2 <- episodes(
#'   date = dfr$admin_period, case_length = match_index,
#'   case_overlap_methods = "inbetween", group_stats = FALSE)
#'
#' dfr2$ep3 <- episodes(
#'   date = dfr$admin_period, case_length = match_index,
#'   case_overlap_methods = "chain", group_stats = FALSE)
#'
#' dfr2$ep4 <- episodes(
#'   date = dfr$admin_period, case_length = match_index,
#'   case_overlap_methods = "aligns_start|aligns_end",
#'   group_stats = FALSE)
#'
#' dfr2
#' as.data.frame(dfr2$ep)
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

  #
  web$counts <- web$mm_opts <- web$controls <- web$opt_levels <-
    web$repo <- web$tm_ia <- web$export <- web$report <- web$tmp <-
    web$tmp2 <- web$rp_data <- web$sys.tmp <- web$rec.pairs <- web$window_opts <-
    web$window_opts_nm <- web$tgt_pos <- web$epids <- list()

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
  opt.dataset.n <- length(date)
  opt.sys.linked.n <- 0
  opts.splits_by_strata <- splits_by_strata
  opts.nrow <- length(date)

  # `opt.epid_unit`
  opt.epid_unit <- tolower(episode_unit)
  opt.epid_unit <- match(opt.epid_unit, names(episode_units))
  class(opt.epid_unit) <- "d_label"
  attr(opt.epid_unit, "value") <- as.vector(sort(opt.epid_unit[!duplicated(opt.epid_unit)]))
  attr(opt.epid_unit, "label") <- names(episode_units)[attr(opt.epid_unit, "value")]
  attr(opt.epid_unit, "state") <- "encoded"
  # `strata`
  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1L, opts.nrow)
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }

  # strata <- if(is.null(strata)) NULL else encode(strata)
  case_length <- if(!inherits(case_length, "list")) list(case_length) else case_length
  recurrence_length <- if(!inherits(recurrence_length, "list")) list(recurrence_length) else recurrence_length

  options_lst = list(
    date = date, strata = strata, case_length = case_length,
    recurrence_length = recurrence_length, episode_unit = opt.epid_unit,from_last = from_last)
  opt.epid_unit <- as.vector(opt.epid_unit)

  # Standardise inputs
  # `display`
  opt.display <- tolower(display)
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

  # opt.use_recurrence <- any(episode_type %in% 2:3)
  opt.use_recurrence <- any(episode_type == 2)

  # `date`
  date <- as.number_line(date)
  is_dt <- inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start, tz = 'GMT'),
      r = as.POSIXct(right_point(date), tz = 'GMT'))
  }

  opt.epid_unit[!is_dt] <- 1L

  # `case_length`
  web$counts$max_indexes <- 1L
  web$counts$w.list.num <- 0L
  for(w.name in c('case', 'recurrence')){
    if(w.name == 'recurrence' & !opt.use_recurrence) break
    tmp.nm <- paste0(w.name,'_length')
    if(w.name == 'case') tmp.length <- case_length
    else tmp.length <- recurrence_length

    web$mm_opts[[tmp.nm]] <- length_to_range(
      x = tmp.length, date = date, from_last = from_last,
      episode_unit = opt.epid_unit, skip_if_b4_lengths = skip_if_b4_lengths)

    web$counts$c.list.num <- length(web$mm_opts[[tmp.nm]]$length)
    web$counts$w.list.num <- ifelse(
      web$counts$w.list.num > web$counts$c.list.num,
      web$counts$w.list.num, web$counts$c.list.num)

    tmp.names <- names(web$mm_opts[[tmp.nm]]$length)
    if(is.null(tmp.names)){
      tmp.names <- rep("w", web$counts$c.list.num)
    }else{
      tmp.names <- ifelse(tmp.names %in% c("", NA),"w", tmp.names)
    }

    web$opt_levels[[tmp.nm]] <- tmp.names

    names(web$mm_opts[[tmp.nm]]$length) <- NULL
    web$mm_opts[[tmp.nm]]$length <- do.call("c", web$mm_opts[[tmp.nm]]$length)
    web$mm_opts[[tmp.nm]]$length_dt_a <- start_point(web$mm_opts[[tmp.nm]]$length)
    web$mm_opts[[tmp.nm]]$length_dt_z <- end_point(web$mm_opts[[tmp.nm]]$length)

    web$mm_opts[[tmp.nm]]$opLgk <- do.call("c", web$mm_opts[[tmp.nm]]$opLgk)

    # `case_overlap_methods`
    # Use `overlap_methods` as a record-level input by default
    tmp.nm <- paste0(w.name,'_overlap_methods')
    if(w.name == 'case') web$mm_opts[[tmp.nm]] <- case_overlap_methods
    else web$mm_opts[[tmp.nm]] <- recurrence_overlap_methods

    if(!inherits(web$mm_opts[[tmp.nm]], "list"))
      web$mm_opts[[tmp.nm]] <- list(web$mm_opts[[tmp.nm]])

    tmp.names <- names(web$mm_opts[[tmp.nm]])
    names(web$mm_opts[[tmp.nm]]) <- NULL
    if(is.null(tmp.names)){
      tmp.names <- rep("r", length(web$mm_opts[[tmp.nm]]))
    }else{
      tmp.names <- ifelse(tmp.names %in% c("", NA), "r", tmp.names)
    }
    web$opt_levels[[tmp.nm]] <- tmp.names
    for(i in seq_len(length(web$mm_opts[[tmp.nm]]))){
      if(length(web$mm_opts[[tmp.nm]][[i]]) == 1){
        web$mm_opts[[tmp.nm]][[i]] <- rep(web$mm_opts[[tmp.nm]][[i]], opts.nrow)
      }
    }

    if(length(web$mm_opts[[tmp.nm]]) == 1 & web$counts$c.list.num > 1){
      web$mm_opts[[tmp.nm]][2:(web$counts$c.list.num)] <-
        rep(web$mm_opts[[tmp.nm]], web$counts$c.list.num-1L)
    }
    web$mm_opts[[tmp.nm]] <- do.call("c", web$mm_opts[[tmp.nm]])

    # `case_length_total`
    tmp.nm <- paste0(w.name,'_length_total')
    if(w.name == 'case') web$repo[[tmp.nm]] <- case_length_total
    else web$repo[[tmp.nm]] <- recurrence_length_total

    if(is.number_line(web$repo[[tmp.nm]])){
      tmp.lgk <- web$repo[[tmp.nm]] < 0
      web$repo[[tmp.nm]][tmp.lgk] <-
        reverse_number_line(web$repo[[tmp.nm]][tmp.lgk], "decreasing")
    }else{
      web$repo[[tmp.nm]] <- number_line(web$repo[[tmp.nm]], Inf)
    }
    web$controls[[paste0('use_',tmp.nm)]] <-
      any(!(web$repo[[tmp.nm]]@start == 1 & web$repo[[tmp.nm]]@.Data == Inf))

    tmp.ov.nm <- paste0(w.name, '_overlap_methods')
    tmp.ln.nm <- paste0(w.name, '_length')

    if(w.name == 'recurrence'){
      opt.use_recurrence_length_total <-
        any(!(web$repo[[tmp.nm]]@start == 1 &
                web$repo[[tmp.nm]]@.Data == Inf))

      web$repo$case_for_recurrence <- case_for_recurrence
      attr(web$repo[[tmp.nm]], "opts") <-
        # attr(recurrence_length, "opts") <-
        class(web$repo$case_for_recurrence) <-
        "d_lazy_opts"
    }

    opt.use_csort <- missing_wf.null(custom_sort) | any(duplicated(custom_sort))
    web$mm_opts[[tmp.ln.nm]]$opLgk  <-
      web$mm_opts[[tmp.ln.nm]]$opLgk &
      web$mm_opts[[tmp.ov.nm]] == 8 &
      date@.Data == 0 &
      !opt.use_csort

    web$controls[[w.name]]$use_operators <-
      all(web$mm_opts[[tmp.ln.nm]]$opLgk)

    if(!web$controls[[w.name]]$use_operators){
      web$controls[[w.name]]$check.use_operators <-
        any(web$mm_opts[[tmp.ln.nm]]$opLgk)
    }else{
      web$controls[[w.name]]$check.use_operators <- FALSE
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
    # attr(case_length_total, "opts") <-
    class(opt.epid_unit) <- class(skip_if_b4_lengths) <- "d_lazy_opts"

  pr_sn <- seq_len(opts.nrow)

  # System preference for case-assignment
  ord_a <- abs(
    max(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start))
  ord_z <- abs(
    max(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date)))
  ord_a[!from_last] <- abs(
    min(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start[!from_last]))
  ord_z[!from_last] <- abs(
    min(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date[!from_last])))

  temporal_ord <- order(order(cri, ord_a, -ord_z, pr_sn))

  # User-defined preference for case-assignment
  if(!is.null(custom_sort)) {
    if(any(!class(custom_sort) %in% c("numeric", "integer", "double"))){
      custom_sort <- as.integer(as.factor(custom_sort))
    }
    if(length(custom_sort) == 1){
      custom_sort <- rep(custom_sort, opts.nrow)
    }
  }else{
    custom_sort <- rep(0L, opts.nrow)
  }

  if(!is.null(data_source)) {
    if(length(data_source) == 1){
      data_source <- rep(data_source, opts.nrow)
    }
  }
  web$tm_ia <- Sys.time()
  web$repo$sn <- sn
  web$repo$pr_sn <-
    web$repo$epid <- pr_sn

  web$repo$c_hits <- web$repo$ld_pos <- rep(NA_real_, opts.nrow)

  web$repo$group_stats <- group_stats
  if(inherits(web$repo$group_stats, "logical")){
    if(web$repo$group_stats){
      web$repo$group_stats <- c("case_nm", "wind", "epid_interval")
    }else{
      web$repo$group_stats <- c("case_nm", "wind")
    }
  }

  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm <- rep(NA_real_, opts.nrow)
  }else{
    web$repo$case_nm <- NULL
  }

  if("wind" %in% web$repo$group_stats){
    web$repo$wind_id <- web$repo$wind_nm <-
      rep(NA_real_, opts.nrow)
  }else{
    web$repo$wind_id <- web$repo$wind_nm <- NULL
  }

  web$repo$cri <- cri
  web$repo$strata <- strata
  web$repo$temporal_ord <- temporal_ord

  web$repo$cur_refs <- rep(1L, opts.nrow)
  web$repo$max_refs <- web$repo$epid_n <-
    web$repo$iteration <- rep(0L, opts.nrow)

  web$repo$tag <- rep(20L, opts.nrow)
  web$repo$reference_event <- reference_event
  web$repo$episode_type <- episode_type

  web$repo$last.batched <- rep(TRUE, opts.nrow)
  web$repo$nwEpi <- rep(FALSE, opts.nrow)

  web$repo$date <- date
  web$repo$case_sub_criteria <- case_sub_criteria
  web$repo$case_length_total <- case_length_total
  web$repo$skip_if_b4_lengths <- skip_if_b4_lengths
  web$repo$episodes_max <- episodes_max
  web$repo$episodes_max[web$repo$episodes_max < 0] <- 0
  web$repo$from_last <- from_last

  web$repo$custom_sort <- custom_sort
  web$repo$skip_order <- skip_order

  opt.data_links <- data_links
  web$repo$data_source <- data_source

  opt.use_skip_order <- any(skip_order < Inf) & !is.null(custom_sort)
  opt.use_episodes_max <- any(episodes_max < Inf)
  opt.use_rolls_max <- any(rolls_max < Inf)

  opt.is_dt <- is_dt
  opt.skip_unique_strata <- skip_unique_strata
  web$export <- web$report <- list()

  opt.dl_lst <- dl_lst

  if(opt.use_recurrence){
    web$repo$roll_n <-
      # web$repo$rxt_n <-
      rep(0L, opts.nrow)
    web$repo$recurrence_sub_criteria <- recurrence_sub_criteria
    web$repo$recurrence_length_total <- recurrence_length_total
    web$repo$rolls_max <- rolls_max
    # web$repo$case_for_recurrence <- case_for_recurrence
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
  if(!is.null(web$repo$data_source) & !all(toupper(opt.dl_lst) == "ANY")){
    web$tmp$lgk <- check_links(
      web$repo$cri,
      web$repo$data_source,
      opt.data_links)$rq
    web$repo$tag[!web$tmp$lgk] <- 10L
    if("case_nm" %in% web$repo$group_stats){
      web$repo$case_nm[!web$tmp$lgk] <- -1L
    }
    web$repo$iteration[!web$tmp$lgk] <- 0L
  }

  # Skip events without the required `skip_order`
  if(opt.use_skip_order){
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
    !duplicated(web$repo$cri, fromLast = FALSE) & opt.skip_unique_strata
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

  web$tmp <- list()

  web$counts$max_indexes <- ite <- 1L

  web$repo$assign_ord <- order(order(
    web$repo$custom_sort, web$repo$temporal_ord))

  rm(
    case_for_recurrence, case_length, case_length_total,
    case_overlap_methods, case_sub_criteria, cri, custom_sort, data_links,
    data_source, date, display, dl_lst, episode_type,
    episode_unit, episodes_max, errs, from_last, group_stats,
    is_dt, ord_a, ord_z, pr_sn, recurrence_length, recurrence_length_total,
    recurrence_overlap_methods, recurrence_sub_criteria, reference_event,
    rolls_max, skip_if_b4_lengths, skip_order, skip_unique_strata,
    sn, strata, tmp.length)

  if(grepl("report$", opt.display)){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_a,
      "Data Prep."
      ,current_tot = opts.nrow
      ,memory_used =
        utils::object.size(web[names(web)[names(web) != "report"]]))
    web$report[[length(web$report) + 1]] <- list(web$rp_data)
  }

  web$sys.tmp$all_pos <- web$repo$pr_sn[
    order(web$repo$assign_ord)
  ]

  opts.split <- 1L
  web$repo$prev.ite.window <-
    web$repo$ite.window <- rep(NA_real_, opts.nrow)
  web$repo$sys.batched <- web$repo$batched <- rep(FALSE, opts.nrow)

  web$tm_ia <- Sys.time()
  if(splits_by_strata > 1){
    web$splits$cri <- web$repo$cri[!duplicated(web$repo$cri)]
    web$splits$order <- as.integer(cut(web$splits$cri, breaks = opts.splits_by_strata))
    web$splits <- split(web$splits$cri, web$splits$order)

    web$sys.tmp$ite_pos <-
      web$sys.tmp$all_pos[
        web$sys.tmp$all_pos %in% web$repo$pr_sn[
          web$repo$cri %in% web$splits[[opts.split]]
        ]
      ]
  }else{
    web$sys.tmp$ite_pos <- web$sys.tmp$all_pos
  }

  while (max(web$repo$tag) == 20) {
    web$sys.tmp$ite_pos <-
      web$sys.tmp$ite_pos[web$repo$tag[web$sys.tmp$ite_pos] != 10]
    web$sys.tmp$rev_ite_pos <- rev(web$sys.tmp$ite_pos)

    if(opts.splits_by_strata > 1 & length(web$sys.tmp$ite_pos) == 0){
      opts.split <- opts.split + 1L
      web$sys.tmp$ite_pos <-
        web$sys.tmp$all_pos[
          web$sys.tmp$all_pos %in% web$repo$pr_sn[
            web$repo$cri %in% web$splits[[opts.split]]
          ]
        ]
    }

    if(opts.splits_by_strata > 1){
      msg_prefix <- paste0(
        'Split ',opts.split, '/', opts.splits_by_strata,': ')
      msg_width <- 110
    }else{
      msg_prefix <- ''
      msg_width <- 100
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
      c("last_event", "last_record", "all_record")

    web$tmp$lgk2 <-
      web$repo$reference_event[web$repo$ld_pos[web$tmp$tgt_pos]] %in%
      c("first_event", "first_record")

    web$sys.tmp$ite_pos <- c(
      # 0.
      web$sys.tmp$ite_pos[web$repo$batched[web$sys.tmp$ite_pos]],
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
    if(opt.use_episodes_max){
      # Strata with `episode_max`
      web$tmp$lgk <-
        web$repo$epid_n[web$sys.tmp$ite_pos[web$tmp$tr_lgk]] ==
        web$repo$episodes_max[web$sys.tmp$ite_pos[web$tmp$tr_lgk]]
      web$tmp$lgk <-
        web$repo$cri[web$sys.tmp$ite_pos] %in%
        web$repo$cri[web$sys.tmp$ite_pos[web$tmp$tr_lgk]][web$tmp$lgk]
      web$tmp$exc_lgk[web$tmp$lgk] <- TRUE
    }

    if(opt.use_skip_order){
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

      if(opts.split < length(web$splits)){
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


    if(grepl("^stats", opt.display)){
      msg <- paste0("Iteration ", fmt(ite) ,".\n")
      cat(msg)
    }
    nms_a <- names(web)
    # Record-pairs
    # Index for rec-pairs
    web$sys.tmp$ite_index <- web$sys.tmp$ite_tr_lgk

    # Records for recursive checks (semi batched)
    web$sys.tmp$ite_rIndex <-
      rep(FALSE, length(web$sys.tmp$ite_pos))
    if(any(web$repo$reference_event[web$repo$ld_pos[web$sys.tmp$ite_pos]]
           %in% c("first_event", "last_event", "all_record"))
    ){
      # Reference events
      web$sys.tmp$ite_rIndex[
        web$repo$reference_event[web$repo$ld_pos[web$sys.tmp$ite_pos]] %in%
          c("first_event", "last_event", "all_record") &
          web$repo$last.batched[web$sys.tmp$ite_tr_pos]
      ]  <- TRUE

      web$sys.tmp$ite_rIndex[web$sys.tmp$ite_rIndex] <-
        overlap(
          web$repo$date[web$sys.tmp$ite_pos[web$sys.tmp$ite_rIndex]],
          web$repo$date[web$sys.tmp$ite_tr_pos[web$sys.tmp$ite_rIndex]])
      # Linked records of recursive window
      web$sys.tmp$ite_rIndex[
        web$repo$reference_event[web$repo$ld_pos[web$sys.tmp$ite_pos]] == "all_record" &
          web$repo$tag[web$sys.tmp$ite_pos] == 1 &
          web$repo$last.batched[web$sys.tmp$ite_tr_pos]
      ] <- TRUE
      #
      web$sys.tmp$ite_index[
        web$sys.tmp$ite_rIndex &
          batched %in% c("semi", "no")
      ] <- TRUE

      web$manual_recursive_index.pos <- web$sys.tmp$ite_pos[web$sys.tmp$ite_rIndex]

      if(batched == "yes"){
        web$repo$batched[web$manual_recursive_index.pos] <- TRUE
      }

      web$manual_recursive_index.tr_pos <-
        web$sys.tmp$ite_tr_pos[web$sys.tmp$ite_rIndex]
      web$manual_recursive_index.tr_pos <-
        web$manual_recursive_index.tr_pos[!duplicated(web$manual_recursive_index.tr_pos)]

      if(batched == "yes"){
        web$repo$sys.batched[
          c(web$manual_recursive_index.pos,
            web$manual_recursive_index.tr_pos)] <- TRUE
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

    web$rec.pairs <- make_pairs_batched(
      strata = web$repo$cri[web$sys.tmp$ite_pos],
      x = web$sys.tmp$ite_pos,
      index_record = web$sys.tmp$ite_index,
      assign_ord = web$repo$tmp.assign_ord,
      include_repeat = TRUE,
      look_back = FALSE
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
    web$rec.pairs$ld_pos <- web$repo$ld_pos[web$rec.pairs$cu_pos]
    # Window's reference record
    web$rec.pairs$ref_rd <-
      web$rec.pairs$cu_pos == web$rec.pairs$tr_pos

    if(opt.use_recurrence){
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
              web$repo$last.batched[web$rec.pairs$tr_pos]]
      ] <- 3L

      web$repo$ite.window[
        web$repo$cri %in%
          web$rec.pairs$cri[
            !web$rec.pairs$nwEpi &
              web$repo$prev.ite.window[web$rec.pairs$tr_pos] %in% c(1, 3) &
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
      if(web$counts$c.list.num > 1){
        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$cu_pos,
          multiples = opts.nrow,
          repeats = web$counts$c.list.num)
        names(web$tmp$tgt_pos) <- paste0("cu_pos.", names(web$tmp$tgt_pos))
        web$rec.pairs[[w.name]] <- c(
          web$rec.pairs[[w.name]], web$tmp$tgt_pos)

        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$tr_pos,
          multiples = opts.nrow,
          repeats = web$counts$c.list.num)
        names(web$tmp$tgt_pos) <- paste0("tr_pos.", names(web$tmp$tgt_pos))
        web$rec.pairs[[w.name]] <- c(
          web$rec.pairs[[w.name]], web$tmp$tgt_pos)

        web$tmp$tgt_pos <- index_multiples(
          x = web$rec.pairs[[w.name]]$ld_pos,
          multiples = opts.nrow,
          repeats = web$counts$c.list.num)
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

      web$tmp$iPeriod <- web$mm_opts[[web$tmp$length_nm]]$length[
        web$rec.pairs[[w.name]]$len_pos]
      web$tmp$iPeriod@start <- web$tmp$iPeriod@start + right_point(web$repo$date[
        web$rec.pairs[[w.name]]$tr_pos])

      # Check overlapping periods
      web$rec.pairs[[w.name]]$ep_checks <-
        rep(FALSE, length(web$rec.pairs[[w.name]]$cu_pos.mi))
      if(web$controls[[w.type]]$use_operators){
        web$tmp$lgk <- TRUE
        web$tmp$lgk <- mk_lazy_opt(web$tmp$lgk)
      }else{
        if(web$controls[[w.type]]$check.use_operators){
          web$tmp$lgk <- web$mm_opts[[web$tmp$length_nm]]$opLgk[web$rec.pairs[[w.name]]$cu_pos.mi]
        }else{
          web$tmp$lgk <- FALSE
          web$tmp$lgk <- mk_lazy_opt(web$tmp$lgk)
        }
      }

      # web$tmp$lgk <- FALSE
      web$tmp$lgk2 <- web$tmp$lgk &
        !web$repo$from_last[web$rec.pairs[[w.name]]$cu_pos.mm]
      web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk2] <-
        web$repo$date@start[
          web$rec.pairs[[w.name]]$cu_pos.mm[web$tmp$lgk2]] <=
        right_point(web$tmp$iPeriod[web$tmp$lgk2])
      #
      web$tmp$lgk2 <- web$tmp$lgk &
        web$repo$from_last[web$rec.pairs[[w.name]]$cu_pos.mm]
      web$rec.pairs[[w.name]]$ep_checks[web$tmp$lgk2] <-
        web$repo$date@start[
          web$rec.pairs[[w.name]]$cu_pos.mi[web$tmp$lgk2]] >=
        left_point(web$tmp$iPeriod[web$tmp$lgk2])

      if(any(!web$tmp$lgk)){
        web$rec.pairs[[w.name]]$ep_checks[!web$tmp$lgk] <-
          overlaps(
            x = web$repo$date[
              web$rec.pairs[[w.name]]$cu_pos.mi[!web$tmp$lgk]],
            y = web$tmp$iPeriod[!web$tmp$lgk],
            methods = web$mm_opts[[web$tmp$overlap_method]][
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
          x_val = web$rec.pairs[[w.name]]$tr_pos.mi[web$rec.pairs[[w.name]]$ep_checks],
          y_val = !duplicated(web$rec.pairs[[w.name]]$tr_pos.ord[web$rec.pairs[[w.name]]$ep_checks]),
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
          opt.skip_unique_strata

        sc_out <- eval_sub_criteria(
          x = web$repo[[web$tmp$sub_criteria_nm]],
          x_pos = web$rec.pairs[[w.name]]$cu_pos[
            web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk],
          y_pos = web$rec.pairs[[w.name]]$tr_pos[
            web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk])

        web$rec.pairs[[w.name]]$s.match[
          web$rec.pairs[[w.name]]$ep_checks][!web$tmp$lgk] <-
          sc_out$logical_test
        tmp.mf.vrs <- grep('^mf', names(sc_out), value = TRUE)
        tmp.ef.vrs <- grep('^ef', names(sc_out), value = TRUE)
        if(length(tmp.mf.vrs) > 0){
          web$export$mf[[paste0('i.', ite)]] <- sc_out[[tmp.mf.vrs]]
        }
        if(length(tmp.ef.vrs) > 0){
          web$export$ef[[paste0('i.', ite)]] <- sc_out[[tmp.ef.vrs]]
        }

        # Check of >0 `sub_criteria`-matches across all case_length or recurrence_length
        web$rec.pairs[[w.name]]$s.match <-
          rowSums(matrix(web$rec.pairs[[w.name]]$s.match,
                         ncol = web$counts$c.list.num))
      }else{
        web$rec.pairs[[w.name]]$s.match <- FALSE
        web$rec.pairs[[w.name]]$s.match <-
          mk_lazy_opt(web$rec.pairs[[w.name]]$s.match)
      }

      if(web$counts$c.list.num > 1){
        # Check of >0 overlap-matches across all case_length or recurrence_length
        web$rec.pairs[[w.name]]$ep_checks <-
          rowSums(matrix(web$rec.pairs[[w.name]]$ep_checks,
                         ncol = web$counts$c.list.num))
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
              rep(NA_real_, opts.nrow),
              (web$tmp$max_indexes - web$counts$max_indexes))

            web$repo$wind_id <- c(web$repo$wind_id, web$tmp$indx)
            web$repo$wind_nm <- c(web$repo$wind_nm, web$tmp$indx)
            web$counts$max_indexes <- web$tmp$max_indexes
          }

          web$tmp$pos <- ((web$tmp$nw_index_ord + web$repo$cur_refs[web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]] - 1L) * opts.nrow) + web$rec.pairs[[w.name]]$cu_pos[web$rec.pairs[[w.name]]$w.match]
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
      if(web$counts$c.list.num > 1){
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

        web$tmp$iPeriod <- web$mm_opts[[web$tmp$length_nm]]$coverage[
          web$rec.pairs[[w.name]]$len_pos[web$tmp$indx]]
        web$tmp$iPeriod@start <- web$tmp$iPeriod@start + right_point(web$repo$date[
          web$rec.pairs[[w.name]]$len_pos[web$tmp$indx]])

        web$rec.pairs[[w.name]]$sk_checks[web$tmp$indx] <- overlap(
          x = web$repo$date[
            web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]],
          y = web$tmp$iPeriod
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
      if(web$counts$c.list.num > 1){
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
      if(opt.use_episodes_max){
        web$tmp$upd_lgk <-
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] == 1 &
          web$repo$tag[web$rec.pairs[[w.name]]$tr_pos] == 20L &
          web$repo$last.batched[web$rec.pairs[[w.name]]$tr_pos]

        web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] <-
          web$repo$epid_n[web$rec.pairs[[w.name]]$cu_pos[web$tmp$upd_lgk]] + 1L

        web$tmp$upd_lgk <-
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] == 2 &
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
            web$repo$rec_rd <- rep(NA_real_, opts.nrow)
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


      if(opt.use_recurrence){
        if(opt.use_rolls_max){
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

          if(opt.use_episodes_max){
            # Update episode counter
            web$repo$epid_n[web$tmp$tgt_pos] <- web$repo$epid_n[web$tmp$tgt_pos] + .5
          }
          # Restart recurrence counter
          web$repo$roll_n[web$tmp$tgt_pos] <-
            0L
          # Close matched records to stop recurrence
          web$repo$tag[web$tmp$tgt_pos][
            web$tmp$tgt_pos %in% web$tmp$cu_pos[web$tmp$lgk] |
              web$repo$tag[web$tmp$tgt_pos] == 1
          ] <- 10L
        }

        # Flag next batched to be checked for recurrence
        web$tmp$indx <-
          web$rec.pairs[[w.name]]$w.match &
          web$repo$episode_type[web$rec.pairs[[w.name]]$ld_pos] == 2 &
          web$repo$tag[web$rec.pairs[[w.name]]$cu_pos] == 20 &
          !web$repo$batched[web$rec.pairs[[w.name]]$cu_pos]

        web$repo$tag[web$rec.pairs[[w.name]]$cu_pos[web$tmp$indx]] <- 1L
      }
    }

    web$tmp$lgk <- web$repo$iteration[web$sys.tmp$ite_pos] == 0 &
      web$repo$tag[web$sys.tmp$ite_pos] != 20
    web$repo$iteration[web$sys.tmp$ite_pos[web$tmp$lgk]] <- ite
    web$tmp$ite.linked.n <- length(which(web$tmp$lgk))
    web$tmp$ite.tot.n <- length(web$sys.tmp$ite_pos)
    opt.sys.linked.n <- opt.sys.linked.n + web$tmp$ite.linked.n

    if(grepl("^progress", opt.display)){
      ite.msg <- paste0(
        "Iteration ", fmt(ite),
        " (", fmt(difftime(Sys.time(), web$tm_ia), "difftime"),")")
      web$msg <- progress_bar(
        n = opt.sys.linked.n, d = opt.dataset.n, max_width = msg_width,
        msg = ite.msg, prefix_msg = msg_prefix)
      cat(web$msg, "\r", sep = "")
    }else if (grepl("^stats", opt.display)){
      web$msg <- update_text(
        tot_records = fmt(opt.dataset.n),
        current_tot = fmt(web$tmp$ite.tot.n),
        current_tagged = fmt(web$tmp$ite.linked.n),
        time = fmt(Sys.time() - web$tm_ia, "difftime"),
        #iteration = ite,
        indent_txt = ""
      )
      cat(web$msg, "\n", sep = "")
    }
    if(grepl("report$", opt.display)){
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

    web$tm_ia <- Sys.time()
    ite <- ite + 1L
    nms_z <- names(web)
    web$tmp <- web$rec.pairs <- list()
  }

  web$tmp <- web$rec.pairs <- list()
  web$tmp$tgt_pos <- web$repo$pr_sn[web$repo$epid > 0]

  if("case_nm" %in% web$repo$group_stats){
    web$repo$case_nm[web$tmp$tgt_pos] <- -1L
  }

  web$repo$iteration[web$tmp$tgt_pos] <- ite - 1L

  if(all(length(web$tmp$tgt_pos) > 0 & "wind" %in% web$repo$group_stats)){
    web$tmp$tgt_pos <- index_multiples(
      web$tmp$tgt_pos,
      multiples = opts.nrow,
      repeats = web$counts$max_indexes
    )

    web$repo$wind_nm[web$tmp$tgt_pos$mm] <- -1L
    web$repo$wind_id[web$tmp$tgt_pos$mm] <- web$repo$pr_sn[web$tmp$tgt_pos$mi]
  }

  if(!grepl("^none", opt.display)){
    cat("\n")
  }

  web$repo$epid <- abs(web$repo$epid)
  web$epids <- make_episodes(
    y_pos = web$repo$epid,
    date =
      if("epid_interval" %in% web$repo$group_stats) web$repo$date else NULL,
    x_pos = web$repo$pr_sn, x_val = web$repo$sn,
    iteration = web$repo$iteration, wind_id = web$repo$wind_id,
    options = options_lst, case_nm = web$repo$case_nm,
    wind_nm = web$repo$wind_nm, from_last = web$repo$from_last,
    episode_unit = names(episode_units)[opt.epid_unit],
    data_source = web$repo$data_source, data_links = opt.data_links)

  if(grepl("report$", opt.display)){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_ia,
      iteration = "End",
      current_tot = opts.nrow,
      memory_used =  utils::object.size(web[names(web)[names(web) != "report"]])
    )
    web$report[length(web$report) + 1] <- list(web$rp_data)
  }
  if(grepl("report$", opt.display)){
    web$epids <- list(epid = web$epids,
                      report = as.list(do.call("rbind", lapply(web$report, as.data.frame))))
    class(web$epids$report) <- "d_report"
  }
  tms <- difftime(Sys.time(), web$tm_a)
  if(!grepl("^none", opt.display)){
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
episodes_af_links <- function(
    date, case_length = Inf, episode_type = "fixed", sn = NULL,
    episode_unit = 'days', strata = NULL, data_source = NULL,
    data_links = 'ANY', group_stats = c("case_nm", "wind", "epid_interval"),  display = "none"){
  is_recurisve <-  episode_type != "fixed"
  recurisve <- ifelse(is_recurisve, "unlinked",  "none")
  check_duplicates <- !isTRUE(is_recurisve)
  if(length(strata) == 0){
    strata <- "p1"
  }

  date <- as.number_line(date)
  if(length(sn) == 0){
    pr_sn <- seq_len(length(date@start))
  }else{
    pr_sn <- sn
  }
  # assign_ord
  a_ord <- order(order(date@start, -date@.Data, pr_sn))

  # `opt.epid_unit`
  opt.epid_unit <- tolower(episode_unit)
  opt.is_dt <- inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(opt.is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  if(!opt.is_dt){
    opt.epid_unit <- "seconds"
  }
  opt.epid_unit <- match(opt.epid_unit, names(episode_units))
  class(opt.epid_unit) <- "d_label"
  attr(opt.epid_unit, "value") <- as.vector(sort(opt.epid_unit[!duplicated(opt.epid_unit)]))
  attr(opt.epid_unit, "label") <- names(episode_units)[attr(opt.epid_unit, "value")]
  attr(opt.epid_unit, "state") <- "encoded"

  window <- length_to_range(
    x = case_length, date = date, from_last = FALSE, output = 'window',
    episode_unit = opt.epid_unit, skip_if_b4_lengths = FALSE)

  f1 <- function(x, y){
    y$dt_z >= x$dt_a
  }

  if(isTRUE(is_recurisve)){
    f2 <- function(x, y){
      lgk <- f1(x = x, y = y)
      lgk[lgk] <- duplicated(combi(y$dt_a[lgk], y$dt_z[lgk]), fromLast = TRUE)
      lgk
    }
  }else{
    f2 <- exact_match
  }

  episodes.scri <- sub_criteria(
    date = attrs(dt_a = left_point(window$length[[1]]),
                 dt_z = right_point(window$length[[1]])),
    match_funcs = c("f1" = f1),
    equal_funcs = c("f2" = f2)
  )

  if(inherits(group_stats, "logical")){
    if(group_stats){
      group_stats <- c("case_nm", "wind", "epid_interval")
    }else{
      group_stats <- c("case_nm", "wind")
    }
  }

  if('wind' %in% group_stats){
    tmp.group_stats <- 'link'
  }else{
    tmp.group_stats <- NULL
  }

  ep <- links(
    criteria = strata, recursive = recurisve,
    check_duplicates = check_duplicates,
    sub_criteria = list(cr1 = episodes.scri),
    tie_sort = a_ord, batched = "yes", permutations_allowed = FALSE,
    repeats_allowed = FALSE, ignore_same_source = FALSE,
    display = display, group_stats = tmp.group_stats)

  if('case_nm' %in% group_stats){
    case_nm <- !(ep@sn %in% ep@.Data)
    case_nm[case_nm] <- ifelse(episode_type == "rolling", 3L, 2L)
  }else{
    case_nm <- NULL
  }

  if('wind' %in% group_stats){
    wind_nm <- rep(ifelse(episode_type == "rolling", 1L, 0L), length(ep@.Data))
    wind_id <- ep@link_id[[1]]
    lgk <- ep@pid_cri == -1
    wind_id[lgk] <- ep@sn[lgk]
    wind_nm[lgk] <- 0
  }else{
    wind_id <- wind_nm <- NULL
  }

  if(!'epid_interbal' %in% group_stats){
    date = NULL
  }

  ep <- make_episodes(
    y_pos = ep@.Data,
    date = date,
    x_pos = ep@sn, x_val = sn, case_nm = case_nm, wind_nm = wind_nm,
    wind_id = wind_id, iteration = ep@iteration, data_links = data_links,
    episode_unit = episode_unit,data_source = data_source)
  return(ep)
}

#' @rdname episodes
#' @export
episodes_af_shift <- function(
    date, case_length = Inf, episode_type = "fixed", sn = NULL,
    episode_unit = 'days', strata = NULL, data_source = NULL,
    data_links = 'ANY', group_stats = FALSE,  display = "none"){
  web <- list()
  web$tm_a <- Sys.time()
  dataset.n <- length(date)
  roll_first <- TRUE
  date <- as.number_line(date)
  opt.is_dt <- inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(opt.is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  if(!opt.is_dt){
    episode_unit <- "seconds"
  }

  opt.display <- display
  web$repo$pr_sn <- seq_len(length(date@start))
  web$repo$date <- date
  web$controls$case_length <- episode_units[[episode_unit]] * case_length

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

  if(length(web$repo$pr_sn) > 0){
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
        web$repo$Nxt.strata <-
          web$repo$strata[c(lead.pos, NA)]
        web$repo$Prv.strata <-
          web$repo$strata_cd[c(NA, lag.pos)]

        end_strata_lgk <- web$repo$strata_cd != web$repo$Nxt.strata
      }else{
        end_strata_lgk <- FALSE
      }

      web$repo$et <- web$repo$tmp.dt_a - web$repo$tmp.dt_z[c(NA, lag.pos)]
      web$repo$change_lgk <- web$repo$et > web$controls$case_length
      lgk <- is.na(web$repo$et)
      web$repo$change_lgk[lgk] <- TRUE

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

      web$repo$epid <- web$repo$Cummchange_lgk
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

      if(length(web$repo$tmp.strata) > 0){
        web$repo$rec_pos <- seq_len(length(web$repo$tmp.strata))
        faC <- as.integer(log10(max(web$repo$rec_pos, na.rm = FALSE))) + 1
        faC <- 10 ^ faC

        web$repo$tmp.strata <- ((max(web$repo$tmp.strata, na.rm = FALSE) + 1)) - web$repo$tmp.strata
        web$repo$tmp.strata <- web$repo$tmp.strata * faC

        web$repo$tmp.rec_pos <- web$repo$tmp.strata + web$repo$rec_pos

        i <- 1L
        opt.sys.linked.n <- 0
        tm_ia <- Sys.time()
        while(any(is.finite(web$repo$tmp.rec_pos))){
          ite.msg <- paste0("Iteration ", fmt(i + 1L), " (", fmt(difftime(Sys.time(), tm_ia), "difftime"),")")
          ite_lgk <- web$repo$tmp.rec_pos != Inf
          web$repo$ld.rec_pos[ite_lgk] <-
            abs(cummax(-web$repo$tmp.rec_pos[ite_lgk])) - web$repo$tmp.strata[ite_lgk]

          web$repo$lgk[ite_lgk] <- (web$repo$wind_z[web$repo$ld.rec_pos[ite_lgk]] >= web$repo$dt_a[ite_lgk])
          web$repo$lgk[ite_lgk] <- web$repo$lgk[ite_lgk] & !is.na(web$repo$lgk[ite_lgk])

          web$repo$epid[ite_lgk][web$repo$lgk[ite_lgk]] <- web$repo$ld.rec_pos[ite_lgk][web$repo$lgk[ite_lgk]]
          web$repo$tmp.rec_pos[ite_lgk][web$repo$lgk[ite_lgk]] <- Inf
          web$repo$iteration[ite_lgk][web$repo$lgk[ite_lgk]] <- i

          if(!grepl("^none", opt.display)){
            ite.linked.n <- length(which(web$repo$lgk[ite_lgk]))
            ite.tot.n <- length(which(ite_lgk)) + ite.linked.n
            cri.linked.n <- length(which(!ite_lgk))
            opt.sys.linked.n <- opt.sys.linked.n + ite.tot.n

            if(grepl("^progress", opt.display)){
              web$msg <- progress_bar(
                n = opt.sys.linked.n, d = dataset.n,
                max_width = msg_width, msg = ite.msg, prefix_msg = msg_prefix)
              cat(web$msg, "\r", sep = "")
            }else if (grepl("^stats", opt.display)){
              web$msg <- update_text(
                tot_records = fmt(dataset.n), current_tot = fmt(ite.tot.n),
                current_tagged = fmt(ite.linked.n), time = fmt(Sys.time() - tm_ia, "difftime"),
                indent_txt = "", # iteration = ite,
              )
              cat(msg, "\n", sep = "")
            }
            tm_ia <- Sys.time()
          }

          i <- i + 1L
        }

        web$repo$epid <- combi(web$repo$tmp.strata, web$repo$epid)
        web$repo$iteration <- web$repo$iteration + 1L
      }

    }else{
      web$repo$iteration <- rep(1L, length(date))
    }

    if(FALSE){
      # Match the default preference for case records used in `episodes()`
      case_s_ord <- order(web$repo$dt_a, -as.numeric(web$repo$dt_z), web$repo$pr_sn)
      web$repo <- lapply(web$repo[c("pr_sn", "epid", "iteration")],
                         function(x) x[case_s_ord])
    }
  }else{
    web$repo$epid <-
      unq.pr_sn.2 <- as.integer()
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
  if(!grepl("^none", opt.display)){
    cat("\nEpisodes tracked in ", fmt(tms, "difftime"), "!\n", sep = "")
  }
  web <- web$epids
  return(web)
}
