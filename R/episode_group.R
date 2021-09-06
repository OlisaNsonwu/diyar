#' @name episode_group
#' @title Link events to chronological episodes.
#'
#' @description Link dated events (records) which ave similar attributes and occur within specified durations of each other.
#' Each set of linked records are assigned a unique identifier with relevant group-level information.
#'
#' @param df \code{[data.frame]}. Deprecated. One or more datasets appended together. See \code{Details}.
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=epid-class]{epid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Episodes are created separately for each \code{strata}.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Event date or period.
#' @param case_length \code{[integer|\link{number_line}]}. Duration from index event distinguishing one \code{"case"} from another.
#' This is the case window.
#' @param episodes_max \code{[integer]}. The maximum number of episodes permitted within each \code{strata}.
#' @param episode_type \code{[character]}. Options are \code{"fixed"} (default), \code{"rolling"} or \code{"recursive"}. See \code{Details}.
#' @param recurrence_length \code{[integer|\link{number_line}]}. Duration from the last \code{"duplicate"} event distinguishing a \code{"recurrent"} event from its index event. This is the recurrence window.
#' @param episode_unit \code{[character]}. Time units for \code{case_length} and \code{recurrence_length}. Options are "seconds", "minutes", "hours", "days" (default), "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max \code{[integer]}. Maximum number of times the index \code{"case"} can recur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source \code{[character]}. Unique data source identifier. Adds the list of datasets in each episode to the \code{\link[=epid-class]{epid}}. Useful when the dataset has data from multiple sources.
#' @param from_last \code{[logical]}. Chronological order of episode tracking i.e. ascending (\code{TRUE}) or descending (\code{FALSE}).
#' @param overlap_method \code{[character]}. Deprecated. Please use \code{case_overlap_methods} or \code{recurrence_overlap_methods}. Methods of overlap considered when tracking event. All event are checked by the same set of \code{overlap_method}.
#' @param overlap_methods \code{[character]}. Deprecated. Please use \code{case_overlap_methods} or \code{recurrence_overlap_methods}. Methods of overlap considered when tracking duplicate event. See (\code{\link{overlaps}})
#' @param case_overlap_methods \code{[character|integer]}. Methods of overlap considered when tracking duplicates of \code{"case"} events. See (\code{\link{overlaps}})
#' @param recurrence_overlap_methods \code{[character|integer]}. Methods of overlap considered when tracking duplicates of \code{"recurrent"} events. See (\code{\link{overlaps}})
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index or reference events.
#' @param bi_direction \code{[logical]}. Deprecated. If \code{TRUE}, \code{"duplicate"} events before and after the index event are tracked.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), episode-specific information like episode start and end dates are returned.
#' @param display \code{[character]}. The progress messages printed on screen. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param reference_event \code{[character]}. Specifies which of the duplicates are used as reference events for subsequent windows. Options are "last_record" (default), "last_event", "first_record" or ""firs_event".
#' @param case_for_recurrence \code{[logical]}. If \code{TRUE}, both \code{"case"} and \code{"recurrent"} events will have a case window.
#' If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"}.
#' @param skip_order \code{[integer]}. \code{"nth"} level of \code{custom_sort}. Episodes with index events beyond this level of preference are skipped.
#' @param data_links \code{[list|character]}. A set of \code{data_sources} required in each \code{\link[=epid-class]{epid}}. An episode without records from these \code{data_sources} will be unlinked. See \code{Details}.
#' @param skip_if_b4_lengths \code{[logical]}. If \code{TRUE} (default), when using lagged \code{case_length} or \code{recurrence_length}, \code{events} before the cut-off point or period are skipped.
#' @param skip_unique_strata \code{[logical]}. If \code{TRUE} (default), all strata with a single record are skipped skipped.
#' @param include_index_period \code{[logical]}. Deprecated. If \code{TRUE}, events overlapping with the index event or period are linked even if they are outside the cut-off period.
#' @param deduplicate \code{[logical]}. Deprecated. If \code{TRUE}, \code{"duplicate"} events are excluded from the \code{\link[=epid-class]{epid}}.
#' @param x \code{[date|datetime|integer|\link{number_line}]}. Deprecated. Record date or period. Please use \code{date}.
#' @param case_sub_criteria \code{[\link{sub_criteria}]}. Matching conditions for "case" windows in addition to temporal links.
#' @param recurrence_sub_criteria \code{[\link{sub_criteria}]}. Matching conditions for "recurrence" windows in addition to temporal links.
#' @param case_length_total \code{[integer|\link{number_line}]}. Minimum number of matched case windows required for an episode.
#' @param recurrence_length_total \code{[integer|\link{number_line}]}. Minimum number of matched recurrence windows required for an episode.
#' @param to_s4 \code{[logical]}. Deprecated. Output type - \code{\link[=epid-class]{epid}} (\code{TRUE}) or \code{data.frame} (\code{FALSE}).
#' @param ... Arguments passed to \code{episodes}.
#' @return
#'
#' @return \code{\link[=epid-class]{epid}}; \code{list}
#'
#' @seealso
#' \code{\link{custom_sort}}; \code{\link{sub_criteria}}; \code{\link[=windows]{epid_length}}; \code{\link[=windows]{epid_window}}; \code{\link{partitions}}; \code{\link{links}}; \code{\link{overlaps}}; \code{\link{number_line}}; \code{\link{schema}}
#'
#' @details
#' These functions are superseded. Moving forward, please use \code{\link{episodes}}.
#'
#' @aliases episode_group
#' @export
episode_group <- function(df, ..., episode_type = "fixed"){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i- `episode_group()` has been retired!\n",
                  "i - Your values will be passed to `episodes()`.\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = FALSE)
  }

  out <- bridge_episode_group(df = df, args = args, episode_type = episode_type)
  if(out$err_cd == FALSE) stop(out$err_nm, call. = FALSE)

  # Warn
  warning(paste0("`episode_group()` has been retired!:\n",
                 "i - Please use `episodes()` instead.\n",
                 "i - Your values were passed to `episodes()`."), call. = FALSE)
  rm(list = ls()[ls() != "out"])
  return(out$err_nm)
}

#' @rdname episode_group
#' @export
fixed_episodes <- function(date, case_length = Inf, episode_unit = "days",
                           to_s4 = TRUE, case_overlap_methods = 8, deduplicate = FALSE,
                           display = "none", bi_direction = FALSE,
                           recurrence_length = case_length,
                           recurrence_overlap_methods = case_overlap_methods,
                           include_index_period = TRUE, ...,
                           overlap_methods = 8, overlap_method = 8, x){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = FALSE)
  }
  # Deprecated arguments and behaviour
  if(missing(case_overlap_methods) & !missing(overlap_methods)) {
    case_overlap_methods <- overlap_methods
    warning(paste0("`overlap_methods` is deprecated:\n",
                   "i - Please use `case_overlap_methods` instead.\n",
                   "i - Your values were passed to `case_overlap_methods`."), call. = FALSE)
  }else if(missing(case_overlap_methods) & !missing(overlap_method)) {
    overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated:\n",
                   "i - Please use `case_overlap_methods` instead.\n",
                   "i - Your values were passed to `case_overlap_methods`."), call. = FALSE)
  }

  if(missing(date) & !missing(x)) {
    date <- x
    warning(paste0("`x` is deprecated and will be removed in the next release:\n",
                   "i - Please use `date` instead.\n",
                   "i - Your values were passed to `date`."), call. = FALSE)
  }
  if(class(display) == "logical"){
    display <- ifelse(display == FALSE, "none", "stats")
  }
  err <- err_episodes_checks_1(date = date,
                               case_length = case_length,
                               recurrence_length = case_length,
                               episode_type = "fixed",
                               episode_unit = episode_unit,
                               case_overlap_methods = case_overlap_methods,
                               recurrence_overlap_methods = case_overlap_methods,
                               deduplicate = deduplicate,
                               display = display,
                               bi_direction = bi_direction,
                               include_index_period = include_index_period,
                               to_s4 = to_s4)

  if(isTRUE(err)){
    stop(err, call. = FALSE)
  }

  episode_unit <- tolower(episode_unit)
  if(length(episode_unit) == 1){
    episode_unit <- rep(episode_unit, length(date))
  }

  r <- prep_lengths(case_length, case_overlap_methods, as.number_line(date),
                    episode_unit, bi_direction)
  case_length <- r$lengths
  case_overlap_methods <- r$method

  if(isTRUE(include_index_period)){
    case_length <- c(case_length, list(index_window(date)))
    case_overlap_methods <- c(case_overlap_methods, list(rep(8, length(date))))
  }

  epids <- episodes(date = date, episode_type = "fixed", case_overlap_methods = case_overlap_methods,
                    recurrence_overlap_methods = case_overlap_methods, display = display,
                    case_length = case_length, recurrence_length = case_length,
                    episode_unit = episode_unit, ...)
  if(isTRUE(deduplicate)) {
    epids <- epids[!epids@case_nm %in% c(2L, 3L)]
  }
  if(isFALSE(to_s4)){
    epids <- to_df(epids)
  }
  rm(list = ls()[ls() != "epids"])
  return(epids)
}

#' @rdname episode_group
#' @export
rolling_episodes <- function(date, case_length = Inf, recurrence_length = case_length,
                             episode_unit = "days", to_s4 = TRUE, case_overlap_methods = 8,
                             recurrence_overlap_methods = case_overlap_methods, deduplicate = FALSE,
                             display = "none", bi_direction = FALSE,
                             include_index_period = TRUE, ...,
                             overlap_methods = 8, overlap_method = 8, x) {

  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i - Please specify any argument you've used.")
    stop(err, call. = FALSE)
  }

  # Deprecated arguments and behaviour
  if(missing(case_overlap_methods) & !missing(overlap_methods)) {
    case_overlap_methods <- overlap_methods
    warning(paste0("`overlap_methods` is deprecated:\n",
                   "i - Please use `case_overlap_methods` instead.\n",
                   "i - Your values were passed to `case_overlap_methods`."), call. = FALSE)
  }else if(missing(case_overlap_methods) & !missing(overlap_method)) {
    case_overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated:\n",
                   "i - Please use `case_overlap_methods` instead.\n",
                   "i - Your values were passed to `overlap_methods`."), call. = FALSE)
  }
  if(missing(recurrence_overlap_methods) & !missing(overlap_methods)) {
    recurrence_overlap_methods <- overlap_methods
    warning(paste0("`overlap_methods` is deprecated:\n",
                   "i - Please use `recurrence_overlap_methods` instead.\n",
                   "i - Your values were passed to `recurrence_overlap_methods`."), call. = FALSE)
  }else if(missing(recurrence_overlap_methods) & !missing(overlap_method)) {
    recurrence_overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated:\n",
                   "i - Please use `recurrence_overlap_methods` instead.\n",
                   "i - Your values were passed to `recurrence_overlap_methods`."), call. = FALSE)
  }

  if(missing(date) & !missing(x)) {
    date <- x
    warning(paste0("`x` is deprecated and will be removed in the next release:\n",
                   "i - Please use `date` instead.\n",
                   "i - Your values were passed to `date`."), call. = FALSE)
  }
  if(class(display) == "logical"){
    display <- ifelse(display == FALSE, "none", "stats")
  }
  err <- err_episodes_checks_1(date = date,
                               case_length = case_length,
                               recurrence_length = recurrence_length,
                               episode_type = "rolling",
                               episode_unit = episode_unit,
                               case_overlap_methods = case_overlap_methods,
                               recurrence_overlap_methods = recurrence_overlap_methods,
                               deduplicate = deduplicate,
                               display = display,
                               bi_direction = bi_direction,
                               include_index_period = include_index_period,
                               to_s4 = to_s4)

  if(isTRUE(err)){
    stop(err, call. = FALSE)
  }

  episode_unit <- tolower(episode_unit)
  if(length(episode_unit) == 1){
    episode_unit <- rep(episode_unit, length(date))
  }

  r <- prep_lengths(case_length, case_overlap_methods, as.number_line(date),
                    episode_unit, bi_direction)
  case_length <- r$lengths
  case_overlap_methods <- r$method

  r <- prep_lengths(recurrence_length, recurrence_overlap_methods, as.number_line(date),
                    episode_unit, bi_direction)
  recurrence_length <- r$lengths
  recurrence_overlap_methods <- r$method

  if(isTRUE(include_index_period)){
    case_length <- c(case_length, list(index_window(date)))
    recurrence_length <- c(recurrence_length, list(index_window(date)))
    case_overlap_methods <- c(case_overlap_methods, list(rep(8, length(date))))
    recurrence_overlap_methods <- c(recurrence_overlap_methods, list(rep(8, length(date))))
  }

  epids <- episodes(date = date, episode_type = "rolling",
                    case_overlap_methods = case_overlap_methods, recurrence_overlap_methods = recurrence_overlap_methods,
                    display = display, case_length = case_length, recurrence_length = recurrence_length,
                    episode_unit = episode_unit, ...)
  if(isFALSE(to_s4)){
    epids <- to_df(epids)
  }
  if(isTRUE(deduplicate)) {
    epids <- epids[!epids@case_nm %in% c(2L, 3L)]
  }
  rm(list = ls()[ls() != "epids"])
  return(epids)
}
