#' @name episodes
#' @title Tacking episodes for case definitions and record deduplication.
#'
#' @description Group events into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together. See \code{Details}.
#' @param sn Unique numerical record identifier. Useful for creating familiar episode identifier. Optional.
#' @param strata Subsets of the dataset. Episodes are tracked separately within each subset. \code{\link{record_group}} is useful for creating these.
#' @param date Event date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}).
#' @param case_length Cut-off point (\code{numeric}) or period (\code{\link{number_line}}) distinguishing one \code{"case"} from another.
#' This is the case window.
#' @param episodes_max Maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Cut-off point or period distinguishing a \code{"recurrent"} event from its index \code{"case"}.
#' This is the recurrence window. If \code{NULL} (default), it's assumed to be the same as \code{case_length}.
#' @param episode_unit Time units for \code{case_length} and \code{recurrence_length}. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max Maximum number of times the index \code{"case"} can recur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique data source identifier. Useful when the dataset contains data from multiple sources.
#' @param from_last If \code{TRUE}, track episodes in reverse chronological order. If \code{TRUE} (default), track episodes in chronological order.
#' @param overlap_method Deprecated. Please use \code{overlap_methods}. Methods of overlap considered when tacking event. All event are checked by the same set of \code{overlap_method}.
#' @param overlap_methods Methods of overlap considered when tacking event. Different events can be checked by different sets of \code{overlap_methods}
#' @param custom_sort  Preferential order for selecting index (\code{"case"}) events. Useful for tracking episodes in a non-chronological order.
#' @param bi_direction If \code{TRUE}, \code{"duplicate"} events before and after the index event are tracked. If  \code{FALSE} (default), \code{"duplicate"} events are tracked in one direction only.
#' @param group_stats If \code{TRUE} (default), episode specific information like episode start and end points are returned. See \code{Value}.
#' @param display Message printed on screen. Options are; \code{"none"} (default) or, \code{"progress"} and \code{"stats"} for a progress update or a more detailed breakdown of the tracking process.
#' @param to_s4 If \code{TRUE} (default), episodes are returned as an \code{\link[=epid-class]{epid}} object.
#' @param recurrence_from_last If \code{TRUE} (default), the reference event for a \code{recurrence window} will be the last event from the previous window.
#' If \code{FALSE} (default), it will be the first event. Only used if \code{episode_type} is \code{"rolling"}.
#' @param case_for_recurrence If \code{TRUE}, both \code{"case"} and \code{"recurrent"} events will have a case window.
#' If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"}.
#' @param skip_order \code{"nth"} level of \code{custom_sort}. Episodes with index events beyound this level of preference are skipped.
#' @param data_links A set of \code{data_sources} required in each episode. \code{stratas} without these will be skipped, and episodes without these will be unlinked. See \code{Details}.
#' @param skip_if_b4_lengths If \code{TRUE} (default), \code{events} before the cut-off points or periods are skipped.
#' @param include_index_period If \code{TRUE}, overlaps with the index event or period are grouped together even if they are outside the cut-off period.
#' @return
#'
#' @return \code{\link[=epid-class]{epid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE}
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided (or generated)
#' \item \code{epid | .Data} - unique episode identifier
#' \item \code{wind_id} - unique window identifier
#' \item \code{wind_nm} - type of window i.e. "Case" or "Recurrence"
#' \item \code{case_nm} - record type in regards to case assignment
#' \item \code{dist_from_wind} - duration of each event from its window's reference event
#' \item \code{dist_from_epid} - duration of each event from its episode's reference event
#' \item \code{epid_dataset} - data sources in each episode
#' \item \code{epid_interval} - episode start and end dates. A \code{\link{number_line}} object.
#' \item \code{epid_length} - difference between episode start and end dates (\code{difftime}). If possible, it's the same unit as \code{episode_unit} otherwise, a difference in days is returned
#' \item \code{epid_total} - number of records in each episode
#' }
#'
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlaps}} and \code{\link{number_line}}
#'
#' @details
#' Episodes are tracked from an index event in chronological or reverse chronological order as determined by \code{from_last}.
#' You can use \code{custom_sort} for a non-chronological order however, ties will be broken by \code{from_last}.
#'
#' A \code{"fixed"} episode has a fixed maximum duration determined by \code{case_length}.
#' However, a \code{"rolling"} episode can continue to recur therefore, its maximum duration is variable.
#' A \code{"rolling"} episode will last as long as is permitted (\code{rolls_max}) and there's an event within its period of recurrence (\code{recurrence_length}).
#'
#' \bold{\code{diyar::episodes()}} will group every record into 5 type of events;
#'
#' \itemize{
#' \item \code{"Case"} - Index case of the episode.
#' \item \code{"Duplicate_C"} - Duplicate of the index case.
#' \item \code{"Recurrent"} - Recurrent event of index case.
#' \item \code{"Duplicate_R"} - Duplicate of the recurrent event.
#' \item \code{"Skipped"} - Those skipped from the episode tracking process.
#' }
#'
#' \code{data_source} - including this returns \code{epid_dataset}. This lists the source of every event in each episode.
#'
#' \code{data_links} should be a \code{list} with every element named 'l' (links) or 'g' (groups).
#'
#' \code{data_links} and \code{skip_order} are useful for skipping episodes that are not required to minimise processing time.
#'
#' \bold{\code{episode_group()}} as it existed before \code{v0.2.0} has been retired.
#' Its current implementation only exists to support existing code with minimal disruption. Please use \bold{\code{diyar::episodes()}} moving forward.
#'
#' \bold{\code{fixed_diyar::episodes()}} and \bold{\code{rolling_diyar::episodes()}} are wrapper functions for two main use cases - tracking \code{"fixed"} and \code{"rolling"} episodes respectively.
#' They exist for convenience and to support existing code with minimal disruption.
#'
#' @examples
#' library(diyar)
#' data(infections)
#' data(hospital_admissions)
#'
#' db_1 <- infections
#' db_1$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#'
#' # Fixed episodes
#' # One 16-day (15-day difference) episode per patient
#' db_1$epids_p <- fixed_diyar::episodes(date=db_1$date,
#'                                strata = db_1$patient_id,
#'                                case_length = 15,
#'                                episodes_max = 1)
#' # Rolling episodes
#' # Case length of 16 days and recurrence periods of 11 days
#' db_1$rd_b <- rolling_diyar::episodes(db_1$date,
#'                               case_length = 15,
#'                               recurrence_length = 10)
#'
#' # Interval grouping
#' hospital_admissions$admin_period <- number_line(hospital_admissions$admin_dt,
#'                                                 hospital_admissions$discharge_dt)
#' admissions <- hospital_admissions[c("admin_period","epi_len")]
#'
#' # Episodes of overlaping periods of admission
#' hospital_admissions$epids_i<- fixed_diyar::episodes(date=hospital_admissions$admin_period,
#'                                             case_length = 0,
#'                                             overlap_methods = "inbetween")
#'
#' @aliases episodes
#' @export
#'
episodes <- function(date, case_length = Inf, episode_type = "fixed", recurrence_length = NULL,
                     episode_unit = "days", episodes_max = Inf, rolls_max = Inf,
                     sn = NULL, strata = NULL, skip_if_b4_lengths = TRUE, data_source = NULL,
                     data_links = "ANY", custom_sort = NULL, skip_order = Inf, from_last = FALSE,
                     overlap_methods = "overlap", bi_direction = FALSE, group_stats = FALSE,
                     display = "none",recurrence_from_last = TRUE,
                     case_for_recurrence = FALSE, include_index_period = TRUE) {

  errs <- err_checks_epid(sn = sn, date = date, case_length = case_length, strata = strata,
                          display=display, episodes_max = episodes_max, from_last = from_last,
                          episode_unit = episode_unit, overlap_methods = overlap_methods,
                          skip_order = skip_order, custom_sort = custom_sort, group_stats = group_stats,
                          data_source=data_source, data_links = data_links, include_index_period = include_index_period,
                          skip_if_b4_lengths = skip_if_b4_lengths, bi_direction = bi_direction,
                          rolls_max = rolls_max, case_for_recurrence = case_for_recurrence, recurrence_from_last = recurrence_from_last,
                          episode_type = episode_type, recurrence_length=recurrence_length)
  display <- tolower(display)
  if(errs!=F) stop(errs, call. = F)

  dl_lst <- unlist(data_links, use.names = F)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links)=="", "l", names(data_links))

  int <- as.number_line(date)
  is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)

  ep_units <- tolower(episode_unit)
  if(length(ep_units) == 1) ep_units <- rep(ep_units, length(int))
  ep_units[!is_dt] <- "seconds"

  if(is_dt == T){
    int <- number_line(
      l = as.POSIXct(format(int@start, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S"),
      r = as.POSIXct(format(right_point(int), "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
    )
  }

  r <- prep_lengths(case_length, overlap_methods, int,
                    ep_units, bi_direction, from_last)
  ep_l <- r$lengths
  mths_a <- r$method

  episode_type <- tolower(episode_type)
  if(length(episode_type) == 1) episode_type <- rep(episode_type, length(int))
  any_rolling <- any(episode_type == "rolling")
  lead_epid_type <- episode_type[!duplicated(episode_type)]
  one_epid_type <- length(lead_epid_type) == 1
  if(one_epid_type != T){
    lead_epid_type <- rep("", length(int))
  }

  if(any_rolling == T){
    if(is.null(recurrence_length)){
      rc_l <- ep_l
      mths_b <- mths_a
    }else{
      r <- prep_lengths(recurrence_length, overlap_methods, int,
                        ep_units, bi_direction, from_last,
                        include_index_period)
      rc_l <- r$lengths
      mths_b <- r$method
    }
  }

  if(length(case_for_recurrence) == 1) case_for_recurrence <- rep(case_for_recurrence, length(int))
  any_case_for_rec <- any(case_for_recurrence == T)
  lead_case_for_rec <- case_for_recurrence[!duplicated(case_for_recurrence)]
  one_case_for_rec <- length(lead_case_for_rec) == 1
  if(one_case_for_rec != T){
    lead_case_for_rec <- rep(NA, length(int))
  }

  if(length(recurrence_from_last) == 1) recurrence_from_last <- rep(recurrence_from_last, length(int))
  any_rec_from_last <- any(recurrence_from_last == F)
  lead_rec_from_last <- recurrence_from_last[!duplicated(recurrence_from_last)]
  one_rec_from_last <- length(lead_rec_from_last) == 1
  if(one_rec_from_last != T){
    lead_rec_from_last <- rep(NA, length(int))
  }

  if(length(skip_if_b4_lengths) == 1) skip_if_b4_lengths <- rep(skip_if_b4_lengths, length(int))
  any_skip_b4_len <- any(skip_if_b4_lengths == T)
  lead_skip_b4_len <- skip_if_b4_lengths[!duplicated(skip_if_b4_lengths)]
  one_skip_b4_len <- length(lead_skip_b4_len) == 1
  if(one_skip_b4_len != T){
    lead_skip_b4_len <- rep(NA, length(int))
  }

  if(length(episodes_max) == 1) episodes_max <- rep(episodes_max, length(int))
  if(length(rolls_max) == 1) rolls_max <- rep(rolls_max, length(int))
  if(length(skip_order) == 1) skip_order <- rep(skip_order, length(int))
  if(length(from_last) == 1) from_last <- rep(from_last, length(int))

  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1, length(int))
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }
  int@id <- seq_len(length(int))
  int@gid <- int@id
  if(!is.null(sn)) int@gid <- sn

  ord_a <- abs(max(as.numeric(int@start)) - as.numeric(int@start))
  ord_z <- abs(max(as.numeric(right_point(int))) - as.numeric(right_point(int)))

  ord_a[!from_last] <- abs(min(as.numeric(int@start)) - as.numeric(int@start[!from_last]))
  ord_z[!from_last] <- abs(min(as.numeric(right_point(int))) - as.numeric(right_point(int[!from_last])))

  assign_ord <- order(ord_a, -ord_z)
  rm(ord_a); rm(ord_z)

  # if(from_last==T){
  #   assign_ord <- order(abs(max(as.numeric(int@start)) - as.numeric(int@start)),
  #                       -abs(max(as.numeric(right_point(int))) - as.numeric(right_point(int))))
  # }else{
  #   assign_ord <- order(abs(min(as.numeric(int@start)) - as.numeric(int@start)),
  #                       -abs(min(as.numeric(right_point(int))) - as.numeric(right_point(int))))
  # }
  assign_ord <- match(seq_len(length(int)), assign_ord)

  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort)==1) c_sort <- rep(c_sort, length(int))
    assign_ord <- order(as.factor(c_sort), assign_ord)
    assign_ord <- match(seq_len(length(int)), assign_ord)
  }else{
    c_sort <- rep(0, length(int))
  }

  tag <- rep(0, length(int))
  names(cri) <- int@id; e <- int@gid;
  names(e) <- int@id; wind_id <- int@gid
  names(int) <- int@id
  names(ep_units) <- int@id
  epid_n <- rep(0, length(int))

  if(any_rolling == T) roll_n <- rep(0, length(int))
  case_nm <- rep("", length(int))
  wind_nm <- case_nm

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, length(int))
    names(data_source) <- int@id
  }

  ite <- 1
  lgk <- is.na(strata)
  tag[lgk] <- 2
  case_nm[lgk] <- "Skipped"

  if(!is.null(data_source) & !all(toupper(dl_lst) == "ANY")){
    req_links <- check_links(cri, data_source, data_links)$rq
    tag[req_links == F] <- 2
    case_nm[req_links == F] <- "Skipped"
  }

  excluded <- length(tag[tag == 2])
  tot <- length(int)

  if(display != "none") cat("\n")
  while (min(tag) != 2) {

    if(display == "stats" & excluded >0 & ite ==1) cat(paste0(fmt(tot), " record(s); ", fmt(excluded)," excluded from episode grouping. ", fmt(tot-excluded), " left to group.\n"))
    if(display == "stats"){
      msg <- paste0("Episode or recurrence window ", fmt(ite) ,".")
      cat(msg, "\n", sep="")
    }

    current_tot <- length(tag[tag!=2])
    sort_ord <- order(cri, tag, assign_ord, int@gid, decreasing = T)

    for(i in c("e","tag","cri","assign_ord",
               "int","epid_n", "c_sort",
               "skip_order", "case_nm",
               "wind_nm", "wind_id",
               "rolls_max",
               "episodes_max")){
      assign(i, get(i)[sort_ord])
    }

    if(any_rolling == T) roll_n <- roll_n[sort_ord]
    if(one_epid_type != T){
      lead_epid_type <- lead_epid_type[sort_ord]
      episode_type <- episode_type[sort_ord]
    }

    if(one_case_for_rec != T){
      lead_case_for_rec <- lead_case_for_rec[sort_ord]
      case_for_recurrence <- case_for_recurrence[sort_ord]
    }

    if(one_rec_from_last != T){
      lead_rec_from_last <- lead_rec_from_last[sort_ord]
      recurrence_from_last <- recurrence_from_last[sort_ord]
    }

    if(one_skip_b4_len != T){
      lead_skip_b4_len <- lead_skip_b4_len[sort_ord]
      skip_if_b4_lengths <- skip_if_b4_lengths[sort_ord]
    }

    ep_l <- lapply(ep_l, function(x){x[sort_ord]})
    mths_a <- lapply(mths_a, function(x){x[sort_ord]})

    if(any_rolling == T){
      rc_l <- lapply(rc_l, function(x){x[sort_ord]})
      mths_b <- lapply(mths_b, function(x){x[sort_ord]})
    }

    if(is.null(names(mths_a))){
      names(mths_a) <- rep("a", length(mths_a))
    }else{
      names(mths_a) <- ifelse(names(mths_a) %in% c("",NA), "a", names(mths_a))
    }

    if(any_rolling == T){
      if(is.null(names(mths_b))){
        names(mths_b) <- rep("a", length(mths_b))
      }else{
        names(mths_b) <- ifelse(names(mths_b) %in% c("",NA), "a", names(mths_b))
      }
    }

    r <- rle(cri)
    p <- as.numeric(names(r$values))
    cri_tot <- r$lengths
    tr_ep_int <- lapply(ep_l, function(x){
      rep(x[which(names(cri) %in% p)], r$lengths)
    })

    if(any_rolling == T){
      tr_rc_int <- lapply(rc_l, function(x){
        rep(x[which(names(cri) %in% p)], r$lengths)
      })
    }

    tr_tag <- rep(tag[which(names(cri) %in% p)], r$lengths)
    tr_e <- rep(e[which(names(cri) %in% p)], r$lengths)
    tr_int <- rep(int[which(names(cri) %in% p)], cri_tot)

    if(any(names(mths_a) == "b") | any(names(mths_a) == "c")){
      tr_mths_a <- lapply(mths_a, function(x){
        rep(x[which(names(cri) %in% p)], r$lengths)})
    }
    if(any_rolling == T){
      if(any(names(mths_b) == "b") | any(names(mths_b) == "c")){
        tr_mths_b <- lapply(mths_b, function(x){
          rep(x[which(names(cri) %in% p)], r$lengths)
        })
      }
    }

    if(one_epid_type != T){
      tr_epid_type <- rep(episode_type[which(names(cri) %in% p)], r$lengths)
      lead_epid_type <- ifelse(tr_tag == 0, tr_epid_type, lead_epid_type)
    }

    if(one_case_for_rec != T){
      tr_case_for_rec <- rep(case_for_recurrence[which(names(cri) %in% p)], r$lengths)
      lead_case_for_rec <- ifelse(tr_tag == 0, tr_case_for_rec, lead_case_for_rec)
    }

    if(one_rec_from_last != T){
      tr_rec_from_last <- rep(recurrence_from_last[which(names(cri) %in% p)], r$lengths)
      lead_rec_from_last <- ifelse(tr_tag == 0, tr_rec_from_last, lead_rec_from_last)
    }

    if(one_skip_b4_len != T){
      tr_skip_b4_len <- rep(skip_if_b4_lengths[which(names(cri) %in% p)], r$lengths)
      lead_skip_b4_len <- ifelse(tr_tag == 0, tr_skip_b4_len, lead_skip_b4_len)
    }

    if (any_rolling == T) {
      roll_n <- ifelse(tr_tag == 0, 0, ifelse(tr_tag == -1, roll_n + 1, roll_n))
    }
    epid_n <- ifelse(tr_tag == 0, epid_n + 1, epid_n)

    lgk1 <- epid_n > episodes_max & tag != 2
    case_nm[lgk1] <- "Skipped"
    tag[lgk1] <- 2

    # Skip order
    cri_skp <- cri[c_sort > skip_order];
    cri_skp <- cri_skp[!duplicated(cri_skp)]
    lgk2 <- cri %in% cri_skp
    current_skipped <- length(cri[lgk1 | lgk2])
    tag[lgk2] <- 2
    case_nm[lgk2] <- "Skipped"
    rm(cri_skp); rm(lgk2); rm(lgk1)

    if(min(tag) == 2){
      if(display== "stats"){
        msg <- paste0(fmt(current_tot), " record(s); ", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_skipped))," assigned to unique episodes.")
        cat(msg, "\n", sep="")
      }else if (tolower(display)=="progress") {
        progress_bar(length(tag[tag == 2])/tot, 100, msg = "Tracking episodes")
      }
      break
    }

    tr_sn <- tr_ep_int[[1]]@gid
    ref_rd <- int@gid %in% tr_sn

    if(any(names(mths_a) == "b") | any(names(mths_a) == "c")){
      opt_level <- function(opt, mth, tr_mth){
        if(opt =="b") {
          tr_mth
        }else if(opt =="c"){
          ifelse(mth != tr_mth, paste0(mth, "|", tr_mth), mth)
        }else{
          mth
        }
      }
      ov_mth_a <- mapply(opt_level, names(mths_a), mths_a, tr_mths_a, SIMPLIFY = F)
    }else{
      ov_mth_a <- mths_a
    }

    if(any_rolling == T){
      if(any(names(mths_b) == "b") | any(names(mths_b) == "c")){
        opt_level <- function(opt, mth, tr_mth){
          if(opt =="b") {
            tr_mth
          }else if(opt =="c"){
            ifelse(mth != tr_mth, paste0(mth, "|", tr_mth), mth)
          }else{
            mth
          }
        }
        ov_mth_b <- mapply(opt_level, names(mths_b), mths_b, tr_mths_b, SIMPLIFY = F)
      }else{
        ov_mth_b <- mths_b
      }
    }

    ovr_chks <- function(tr, int, mths) diyar::overlaps(tr, int, methods =mths)
    ep_checks <- rowSums(mapply(ovr_chks, tr_ep_int, rep(list(int), length(tr_ep_int)), ov_mth_a)) > 0

    if(any_rolling == T){
      rc_checks <- rowSums(mapply(ovr_chks, tr_rc_int, rep(list(int), length(tr_rc_int)), ov_mth_b)) > 0
    }

    cr <- ifelse(tr_tag %in% c(0, -2) &
                   (ref_rd  | ep_checks==1) &
                   tag != 2,
                 T, F)

    if(any_rolling == T){
      cr <- ifelse(tr_tag == -1 &
                     (ref_rd  | rc_checks==1) &
                     tag != 2,
                   T, cr)
    }

    if(include_index_period == T){
      ref_period <- overlap(int, tr_int)
      cr[ref_period & !cr] <- T
    }

    e[cr & tag == 0 & tr_tag == 0] <- tr_sn[cr & tag == 0 & tr_tag ==  0]
    wind_id[cr & tag == 0] <- tr_sn[cr & tag == 0]
    e[cr & tr_tag %in% c(-1, -2)] <- tr_e[cr & tr_tag %in% c(-1, -2)]

    case_nm[cr & tr_tag == 0] <- ifelse(ref_rd[cr & tr_tag == 0], "Case", "Duplicate_C")
    wind_nm[cr & tr_tag %in% c(0, -2) & wind_nm == ""] <- "Case"
    tag[cr] <- 2

    if(include_index_period != T){
      ref_period <- overlap(int, tr_int)
    }

    if(any_rolling == T){
      case_nm[cr & tr_tag %in% c(-1, -2) & case_nm == ""] <- "Duplicate_R"
      wind_nm[cr & tr_tag == -1 & wind_nm == ""] <- "Recurrence"
      sort_ord <- order(cri, cr, -ref_period, tag, -assign_ord, int@gid)
      r <- rle(cri[sort_ord])
      case_nm[which(int@id %in% names(r$values) &
                      tr_tag %in% c(-1) &
                      !ref_period
      )] <- "Recurrent"

      t_cri <- cri[order(cri, -tag)]
      t_tag <- tag[order(cri, -tag)]
      last_tag <- rle(t_cri)
      last_tag <- rep(t_tag[which(names(t_cri) %in% names(last_tag$values))], last_tag$lengths)
      last_tag <- last_tag[match(names(cri), names(t_cri))]
      rm(t_tag); rm(t_cri)
      close_epi <- last_tag == 2

      roll_ref <- assign_ord
      if(any_rec_from_last == T) roll_ref[!lead_rec_from_last] <- -roll_ref[!lead_rec_from_last]

      t_cr <- ifelse(ref_rd & roll_n >= 1, F, cr)
      sort_ord <- order(cri, t_cr, tag, roll_ref, int@gid)
      r <- rle(cri[sort_ord])
      tag[which(int@id %in% names(r$values) & tag !=0 & !close_epi & roll_n < rolls_max & t_cr & lead_epid_type == "rolling")] <- -1

      if(any_case_for_rec == T){
        tag[which(int@id %in% names(r$values) &
                    !(ref_rd & roll_n >= 1) &
                    tr_tag == -1 &
                    !close_epi &
                    roll_n <= rolls_max &
                    t_cr  &
                    lead_epid_type == "rolling" &
                    lead_case_for_rec == T)] <- -2
      }
    }

    if(min(tag) == 2){
      if(display== "stats"){
        msg <- paste0(fmt(current_tot), " record(s); ", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_skipped))," assigned to unique episodes.")
        cat(msg, "\n", sep="")
      }else if (tolower(display)=="progress") {
        progress_bar(length(tag[tag==2])/tot, 100, msg = "Tracking episodes")
      }
      break
    }

    if(any_skip_b4_len == T){
      ep_l_min_a <- Rfast::rowMinsMaxs(sapply(tr_ep_int[lead_skip_b4_len], start_point))
      ep_l_min_z <- Rfast::rowMinsMaxs(sapply(tr_ep_int[lead_skip_b4_len], end_point))

      ep_l_bounds_a <- start_point(tr_int[lead_skip_b4_len])
      ep_l_bounds_z <- end_point(tr_int[lead_skip_b4_len])

      ep_l_bounds_a <- ifelse(ep_l_min_a[1,] < ep_l_bounds_a, ep_l_min_a[1,], ep_l_bounds_a)
      ep_l_bounds_z <- ifelse(ep_l_min_z[2,] > ep_l_bounds_z, ep_l_min_z[2,], ep_l_bounds_z)

      epc_bnds <- suppressWarnings(
        number_line(
          l = ep_l_bounds_a,
          r = ep_l_bounds_z))

      ep_obds_checks <- suppressWarnings(overlap(int[lead_skip_b4_len], epc_bnds))

      if(any_rolling == T){
        rc_l_min_a <- Rfast::rowMinsMaxs(sapply(tr_rc_int[lead_skip_b4_len], start_point))
        rc_l_min_z <- Rfast::rowMinsMaxs(sapply(tr_rc_int[lead_skip_b4_len], end_point))

        rc_l_bounds_a <- start_point(tr_int[lead_skip_b4_len])
        rc_l_bounds_z <- end_point(tr_int[lead_skip_b4_len])

        rc_l_bounds_a <- ifelse(rc_l_min_a[1,] < rc_l_bounds_a, rc_l_min_a[1,], rc_l_bounds_a)
        rc_l_bounds_z <- ifelse(rc_l_min_z[2,] > rc_l_bounds_z, rc_l_min_z[2,], rc_l_bounds_z)

        rc_l_bnds <- suppressWarnings(
          number_line(
            l = rc_l_bounds_a,
            r = rc_l_bounds_z))

        rc_obds_checks <- suppressWarnings(overlap(int[lead_skip_b4_len], rc_l_bnds))
      }

      skp_crxt <- cri[cr & !ref_period]
      skp_crxt <- skp_crxt[!duplicated(skp_crxt)]
      indx <- (ep_obds_checks &
                 !cr &
                 cri[lead_skip_b4_len] %in% skp_crxt &
                 tr_tag[lead_skip_b4_len] %in% c(0, -2) &
                 case_nm[lead_skip_b4_len] == "")
      if(any_rolling == T){
        indx <- ifelse((rc_obds_checks &
                          !cr &
                          cri[lead_skip_b4_len] %in% skp_crxt &
                          tr_tag[lead_skip_b4_len] == -1 &
                          case_nm[lead_skip_b4_len] == ""),
                       T, indx)
      }

      case_nm[lead_skip_b4_len][indx] <- "Skipped"
      tag[lead_skip_b4_len][indx] <- 2
      rm(skp_crxt); rm(indx)
    }

    current_tagged <- length(cr[cr])
    if(display== "stats"){
      msg <- paste0(fmt(current_tot), " record(s); ", fmt(current_tagged)," grouped to episodes", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_tagged + current_skipped))," left to group.")
      cat(msg, "\n", sep="")
    }else if (tolower(display)=="progress") {
      progress_bar(length(tag[tag==2])/tot, 100, msg = "Tracking episodes")
    }
    ite <- ite + 1
  }
  if(display != "none") cat("\n")

  wind_nm[which(case_nm == "Skipped")] <- "Skipped"
  ep_units <- ep_units[match(names(e), names(ep_units))]
  diff_unit <- ifelse(ep_units %in% c("second","minutes"),
                      paste0(substr(ep_units, 1 ,3), "s"),
                      ep_units)
  diff_unit <- ifelse(diff_unit %in% c("months","year"), "days", diff_unit)
  diff_unit <- diff_unit[!duplicated(diff_unit)]
  if(length(diff_unit) > 1) diff_unit <- "days"

  stat_pos <- int@id
  sort_ord <- order(e, wind_id, as.numeric(int@start))
  e <- e[sort_ord]
  int <- int[sort_ord]

  epid_n <- rle(e)
  epid_n <- rep(epid_n$lengths, epid_n$lengths)
  lgk <- !duplicated(e)
  dist_from_epid <- ((as.numeric(int@start) + as.numeric(right_point(int))) * .5) -
    rep(((as.numeric(int@start[lgk]) + as.numeric(right_point(int[lgk]))) * .5), epid_n[lgk])

  if(any_rolling == T){
    wind_id <- wind_id[sort_ord]
    wind_n <- rle(wind_id)
    wind_n <- rep(wind_n$lengths, wind_n$lengths)
    lgk <- !duplicated(wind_id)
    dist_from_wind <- ((as.numeric(int@start) + as.numeric(right_point(int)))*.5) -
      rep(((as.numeric(int@start[lgk]) + as.numeric(right_point(int[lgk])))*.5), wind_n[lgk])
  }else{
    dist_from_wind <- dist_from_epid
    wind_id <- e
  }

  if(is_dt==T){
    dist_from_epid <- dist_from_epid / as.numeric(diyar::episode_unit[ep_units])
    dist_from_epid <- as.difftime(dist_from_epid, units = diff_unit)

    if (any_rolling == T){
      dist_from_wind <- dist_from_wind / as.numeric(diyar::episode_unit[ep_units])
      dist_from_wind <- as.difftime(dist_from_wind, units = diff_unit)
    }else{
      dist_from_wind <- dist_from_epid
    }
  }

  tmp_pos <- names(e)
  fd <- match(1:length(int), tmp_pos)
  f_e <- e[fd]; names(e) <- NULL; names(f_e) <- NULL

  retrieve_pos <- match(1:length(int), stat_pos)
  epid <- new("epid",
              .Data= e[fd],
              dist_from_epid = dist_from_epid[fd],
              dist_from_wind = dist_from_wind[fd],
              sn = int@gid[fd],
              case_nm= case_nm[retrieve_pos],
              wind_nm = wind_nm[retrieve_pos],
              wind_id = wind_id[fd])
  names(epid@wind_id) <- NULL

  if(!is.null(data_source)){
    data_source <- data_source[match(tmp_pos[fd], names(data_source))]
    # Data links
    names(e) <- tmp_pos
    rst <- check_links(e[fd], data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epid@dist_from_epid[req_links == F] <- 0
      epid@dist_from_wind[req_links == F] <- 0
      epid@case_nm[req_links == F] <- "Skipped"
      epid@.Data[req_links == F] <- epid@sn[req_links == F]
      epid@wind_id[req_links == F] <- epid@sn[req_links == F]
      datasets[req_links == F] <- data_source[req_links == F]
    }
    epid@epid_dataset <- datasets
  }

  epid_tot <- epid_n[fd]
  if(group_stats == T){
    ep_func_a <- function(dts, frm_lst) if(all(frm_lst == F)) min(dts) else max(dts)
    ep_func_z <- function(dts, frm_lst) if(all(frm_lst == F)) max(dts) else min(dts)

    frm_lst <- split(from_last, e)
    epid_dt_a <- mapply(ep_func_a, split(as.numeric(int@start), e), frm_lst)
    epid_dt_z <- mapply(ep_func_z, split(as.numeric(right_point(int)), e), frm_lst)
    rm(frm_lst)

    epid_dt_a <- as.numeric(epid_dt_a)[match(e, names(epid_dt_a))]
    epid_dt_z <- as.numeric(epid_dt_z)[match(e, names(epid_dt_z))]

    if(is_dt ==T){
      epid_dt_a <- as.POSIXct(epid_dt_a, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      epid_dt_z <- as.POSIXct(epid_dt_z, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      epid_l <- difftime(epid_dt_z, epid_dt_a, units = diff_unit)
    }else{
      epid_l <- epid_dt_z - epid_dt_a
    }

    names(epid_n) <- NULL
    names(epid_l) <- NULL
    names(e) <- NULL
    names(epid_tot) <- NULL

    epid@epid_interval <- number_line(l = epid_dt_a[fd],
                                      r = epid_dt_z[fd],
                                      gid = f_e)
    epid@epid_total <- epid_tot
    epid@epid_length <- epid_l[fd]
  }

  names(epid) <- NULL

  if(display != "none"){
    summ <- paste0("Summary.\n",
                   "Records: ", fmt(length(epid)), "\n",
                   "Skipped records: ", fmt(length(epid[epid@case_nm == "Skipped"])), "\n",
                   "Episodes with unique events: ", fmt(length(epid[epid@case_nm == "Case" & epid_tot == 1])), "\n",
                   "Episodes with multiple events: ", fmt(length(epid[epid@case_nm == "Case" & epid_tot > 1])), "\n")
    cat(summ)
  }

  if(display == "none") cat("Episode tracking complete!\n")
  return(epid)
}

#' @rdname episodes
#' @export
fixed_episodes <- function(..., date, episode_type = "fixed", to_s4 = T,
                           overlap_methods = "overlap", deduplicate = F,
                           display = "progress", overlap_method = "overlap",
                           x){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = F)
  }
  # Deprecated arguments and behaviour
  if(missing(overlap_methods) & !missing(overlap_method)) {
    overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated and will be removed in the next release:\n",
                   "i - Please use `overlap_methods` instead.\n",
                   "i - Your values were passed to `overlap_methods`."), call. = F)
  }
  if(missing(date) & !missing(x)) {
    date <- x
    warning(paste0("`x` is deprecated and will be removed in the next release:\n",
                   "i - Please use `date` instead.\n",
                   "i - Your values were passed to `date`."), call. = F)
  }
  if(class(display) == "logical"){
    display <- ifelse(display == F, "none", "stats")
  }
  epids <- diyar::episodes(date = date, episode_type = episode_type, overlap_methods = overlap_methods, display = display, ...)
  if(deduplicate == T) {
    epids <- epids[!epids@case_nm %in% c("Duplicate_C", "Duplicate_R")]
  }
  if(to_s4 == F){
    epids <- to_df(epids)
  }
  return(epids)
}

#' @rdname episodes
#' @export
rolling_episodes <- function(..., date, episode_type = "rolling", to_s4 = T,
                             overlap_methods = "overlap",  deduplicate = F,
                             display = "progress", overlap_method = "overlap",
                             x) {

  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = F)
  }

  # Deprecated arguments and behaviour
  if(missing(overlap_methods) & !missing(overlap_method)) {
    overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated and will be removed in the next release:\n",
                   "i - Please use `overlap_methods` instead.\n",
                   "i - Your values were passed to `overlap_methods`."), call. = F)
  }

  if(missing(date) & !missing(x)) {
    date <- x
    warning(paste0("`x` is deprecated and will be removed in the next release:\n",
                   "i - Please use `date` instead.\n",
                   "i - Your values were passed to `date`."), call. = F)
  }
  if(class(display) == "logical"){
    display <- ifelse(display == F, "none", "stats")
  }
  epids <- diyar::episodes(date = date, episode_type = episode_type, overlap_methods = overlap_methods, ...)
  if(to_s4 == F){
    epids <- to_df(epids)
  }
  if(deduplicate == T) {
    epids <- epids[!epids@case_nm %in% c("Duplicate_C", "Duplicate_R")]
  }
  return(epids)
}

#' @rdname episodes
#' @export
episode_group <- function(df, ..., episode_type = "fixed"){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i- `episode_group()` has been retired!\n",
                  "i - Your values will be passed to `links()`.\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = F)
  }

  out <- bridge_episode_group(df=df, args=args, episode_type = episode_type)
  if(out$err_cd == F) stop(out$err_nm, call. = F)

  # Warn
  warning(paste0("`episode_group()` has been retired!:\n",
                 "i - Please use `diyar::episodes()`, `fixed_diyar::episodes()` or `rolling_diyar::episodes()` instead.\n",
                 "i - Your values were passed to `diyar::episodes()`."), call. = F)
  return(out$err_nm)
}

#' @name windows
#' @title Window and lengths
#'
#' @description Interpret \code{windows}, \code{case_lengths} and \code{recurrence_lengths} as used in \code{\link{episodes}}.
#'
#' @details
#' \bold{\code{epid_windows}} - returns the corresponding period for a given a \code{date}, and \code{case_length} or \code{recurrence_length}.
#' \bold{\code{epid_lengths}} - returns the corresponding \code{case_length} or \code{recurrence_length} for a given \code{date} and period.
#'
#' @return \code{\link{number_line}}.
#'
#' @examples
#' library(diyar)
#'
#' `epid_windows`
#' epid_windows(Sys.Date(), 10)
#' epid_windows(Sys.Date(), number_line(5, 10))
#' epid_windows(Sys.Date(), number_line(-5, 10))
#' epid_windows(Sys.Date(), -5)
#'
#' @export
epid_windows <- function(date, lengths){
  date <- as.number_line(date)
  if(class(lengths) != "number_line"){
    lengths <- number_line(0, as.numeric(lengths))
  }
  number_line(right_point(date) + lengths@start,
              right_point(date) + right_point(lengths))
}

#' @rdname windows
#' @examples
#' `epid_lengths`
#' epid_lengths(number_line(01, 20), 30)
#' epid_lengths(number_line(01, 20), number_line(25, 30))
#' epid_lengths(number_line(01, 20), number_line(-10, 30))
#' epid_lengths(number_line(01, 20), -10)
#' @export
epid_lengths <- function(date, windows){
  date <- as.number_line(date)
  windows <- as.number_line(windows)
  number_line(windows@start - right_point(date),
              right_point(windows) - right_point(date))
}
