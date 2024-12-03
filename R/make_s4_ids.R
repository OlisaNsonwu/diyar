#' @name make_s4_ids
#' @aliases make_s4_ids
#' @title Create \code{epid} and \code{pid} objects with index of matching records
#'
#' @param x_pos \code{[integer]}. Index of one half of a record pair.
#' @param y_pos \code{[integer]}. Index of one half of a record pair.
#' @param x_val \code{[integer]}. Value of one half of a record pair.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Record date or period.
#' @param case_nm \code{[integer|character]} Record type in regards to case assignment (\code{\link{sub_criteria}[Encoded]}).
#' @param episode_unit \code{[character]}. Time unit for \code{case_length} and \code{recurrence_length}. See \code{\link{episodes}}
#' @param iteration The iteration when a record was matched to it's group (\code{.Data}).
#' @param options \code{[list]}. Some options passed to the instance of \code{\link{episodes}}.
#' @param wind_id \code{[integer]}. Unique reference ID for each match.
#' @param link_id \code{[integer]}. Unique reference ID for each match.
#' @param wind_nm \code{[list]}. Type of window i.e. "Case" or "Recurrence".
#' @param from_last \code{[logical]}. Chronological order of episode tracking i.e. ascending (\code{TRUE}) or descending (\code{FALSE}).
#' @param pid_cri Match stage of the step-wise linkage.
#' @param data_links \code{[list|character]}. \code{data_source} required in each record-group. A record-group without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}.
#' @param data_source \code{[character]}. Source ID for each record.
#' @export
make_episodes <- function(
    x_pos,
    y_pos,
    x_val,
    date,
    case_nm,
    wind_id,
    wind_nm,
    from_last,
    data_source,
    data_links,
    iteration,
    options,
    episode_unit){

  if(!missing_wf.null(date)){
    if(inherits(date, "number_line")){
      is_dt <- inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt"))
    }else{
      is_dt <- inherits(date, c("Date","POSIXct","POSIXt","POSIXlt"))
      date <- as.number_line(date)
    }
  }

  epids <- new("epid", .Data = as.integer(y_pos))

  if(!missing_wf.null(iteration)){
    epids@iteration <- iteration
  }
  if(!missing_wf.null(options)){
    epids@options <- options
  }

  if(!missing_wf.null(case_nm)){
    epids@case_nm <- case_nm
    class(epids@case_nm) <- "d_label"
    attr(epids@case_nm, "value") <- -1L : 5L
    attr(epids@case_nm, "label") <-
      c("Skipped", "Case", "Recurrent", "Duplicate_C",
        "Duplicate_R", "Case_CR", "Recurrent_CR")
    attr(epids@case_nm, "state") <- "encoded"
  }

  if(!missing_wf.null(wind_nm)){
    wind_nm <- matrix(wind_nm, nrow = length(epids@.Data))
    ncol <- ncol(wind_nm)
    epids@wind_nm <- lapply(1:ncol, function(i){
      wind_nm[,i]
    })
    names(epids@wind_nm) <- paste0("wind_nm", 1:ncol)
    epids@wind_nm <- lapply(epids@wind_nm, function(x){
      class(x) <- "d_label"
      attr(x, "value") <- -1L : 2L
      attr(x, "label") <- c("Skipped", "Case", "Recurrence", "Case for Recurrence")
      attr(x, "state") <- "encoded"
      return(x)
    })
  }

  rr <- rle(sort(epids@.Data))
  epids@epid_total <-
    rr$lengths[match(epids@.Data, rr$values)]

  if(!missing_wf.null(date)){
    index_date <- date[epids@.Data]
    epids@dist_epid_index <-
      ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
      ((as.numeric(index_date@start) + as.numeric(right_point(index_date))) * .5)

    if(!missing_wf.null(wind_id)){
      index_date <- date[wind_id[1:length(x_pos)]]
      epids@dist_wind_index <- ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
        ((as.numeric(index_date@start) + as.numeric(right_point(index_date))) * .5)
    }else{
      epids@dist_wind_index <- epids@dist_epid_index
    }

    if(is_dt){
      if(!missing_wf.null(episode_unit)){
        diff_unit <- episode_unit
        diff_unit[diff_unit %in% c("months", "years")] <- "days"
        diff_unit <- diff_unit[!duplicated(diff_unit)]
        if(length(diff_unit) > 1){
          diff_unit <- "days"
        }
        diff_unit <- diff_unit
        diff_unit[diff_unit == "seconds"] <- "secs"
        diff_unit[diff_unit == "minutes"] <- "mins"
      }else{
        diff_unit <- "days"
      }

      epids@dist_epid_index <-
        epids@dist_epid_index / as.numeric(episode_units[episode_unit])
      epids@dist_epid_index <-
        as.difftime(epids@dist_epid_index, units = diff_unit)

      if(!missing_wf.null(wind_id)){
        epids@dist_wind_index <-
          epids@dist_wind_index / as.numeric(episode_units[episode_unit])
        epids@dist_wind_index <-
          as.difftime(epids@dist_wind_index, units = diff_unit)
      }else{
        epids@dist_wind_index <- epids@dist_epid_index
      }
    }

    lgk <- which(epids@epid_total != 1)
    epids@epid_interval <- date
    epids@epid_interval@start <- as.numeric(epids@epid_interval@start)
    epids@epid_interval[lgk] <- group_stats(
      strata = epids@.Data[lgk],
      start_date = epids@epid_interval@start[lgk],
      end_date = right_point(epids@epid_interval)[lgk]
    )

    if(!missing_wf.null(from_last)){
      lgk <- from_last[epids@.Data]
      epids@epid_interval[lgk] <- reverse_number_line(
        epids@epid_interval[lgk],
        direction = "increasing")
    }

    if(is_dt){
      epids@epid_interval <- number_line(
        as.POSIXct(
          epids@epid_interval@start, tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT")),
        as.POSIXct(
          right_point(epids@epid_interval), tz = "GMT",
          origin = as.POSIXct("1970-01-01", "GMT"))
      )
      epids@epid_length <- difftime(
        right_point(epids@epid_interval),
        epids@epid_interval@start,
        units = diff_unit)
    }else{
      epids@epid_length <-
        right_point(epids@epid_interval) - epids@epid_interval@start
    }
  }

  if(!missing_wf.null(data_source) & !missing_wf.null(data_links)){
    if(length(data_source) > 0){
      rst <- check_links(
        epids@.Data,
        data_source,
        data_links)
      epids@epid_dataset <- rst$ds

      if(!all(toupper(unlist(data_links, use.names = FALSE)) == "ANY")){
        req_links <- rst$rq
        epidsepids <- suppressWarnings(delink(epids, !req_links))
        epids@epid_dataset[!req_links] <- data_source[!req_links]
      }
      epids@epid_dataset <- encode(epids@epid_dataset)
    }
  }

  if(!missing_wf.null(x_val)){
    epids@.Data <- as.integer(x_val)[epids@.Data]
  }
  if(!missing_wf.null(x_pos)){
    if(!missing_wf.null(x_val)){
      epids@sn <- as.integer(x_val)
    }else{
      epids@sn <- as.integer(x_pos)
    }
  }

  if(!missing_wf.null(wind_id)){
    if(!missing_wf.null(x_val)){
      wind_id <- as.integer(x_val)[as.integer(wind_id)]
    }
    wind_id <- matrix(wind_id, nrow = length(epids@.Data))
    ncol <- ncol(wind_id)
    epids@wind_id <- lapply(1:ncol, function(i){
      wind_id[,i]
    })
    names(epids@wind_id) <- paste0("wind_id", 1:ncol)
  }

  return(epids)
}

#' @rdname make_s4_ids
make_pids <- function(x_pos,
                      y_pos,
                      x_val,
                      link_id,
                      pid_cri,
                      data_source,
                      data_links,
                      iteration){
  pids <- new("pid", .Data = as.integer(y_pos))

  if(!missing_wf.null(iteration)){
    pids@iteration <- iteration
  }
  if(!missing_wf.null(pid_cri)){
    pids@pid_cri <- pid_cri
  }
  #
  r <- rle(sort(pids@.Data))
  pids@pid_total <- r$lengths[match(pids@.Data, r$values)]
  #
  if(!missing_wf.null(data_source) & !missing_wf.null(data_links)){
    if(length(data_source) > 0){
      rst <- check_links(
        pids@.Data,
        data_source,
        data_links)
      pids@pid_dataset <- rst$ds

      if(!all(toupper(data_links) == "ANY")){
        req_links <- rst$rq
        pidspids <- suppressWarnings(delink(pids, !req_links))
        pids@pid_dataset[!req_links] <- data_source[!req_links]
      }
      pids@pid_dataset <- encode(pids@pid_dataset)
    }
  }

  if(!missing_wf.null(x_val)){
    pids@.Data <- as.integer(x_val)[pids@.Data]
  }
  if(!missing_wf.null(x_pos)){
    if(!missing_wf.null(x_val)){
      pids@sn <- as.integer(x_val)
    }else{
      pids@sn <- as.integer(x_pos)
    }
  }
  if(!missing_wf.null(link_id)){
    if(!missing_wf.null(x_val)){
      link_id <- as.integer(x_val)[as.integer(link_id)]
    }
    link_id <- matrix(link_id, nrow = length(pids@.Data))
    ncol <- ncol(link_id)
    pids@link_id <- lapply(1:ncol, function(i){
      link_id[,i]
    })
    names(pids@link_id) <- paste0("link_id", 1:ncol)
  }

  return(pids)
}
