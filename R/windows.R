#' @name windows
#' @aliases windows
#' @title Windows and lengths
#'
#' @param date Date or period as used in \bold{\code{\link{episodes}}}.
#' @param lengths The duration between a \code{date} and \code{window}.
#' @param windows The range relative to a \code{date} for a given duration (\code{length}).
#' @param episode_unit Time unit of \code{lengths}. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years". See \code{episode_units}
#' @param from_last Episode tracking direction as used in \bold{\code{\link{episodes}}}.
#' @description Covert between \code{windows} and \code{case_lengths} or \code{recurrence_lengths}.
#'
#' @details
#' \bold{\code{epid_windows}} - Returns the corresponding \code{window} for a given a \code{date}, and \code{case_length} or \code{recurrence_length}.
#'
#' \bold{\code{epid_lengths}} - Returns the corresponding \code{case_length} or \code{recurrence_length} for a given \code{date} and \code{window}.
#'
#' \bold{\code{index_window}} - Returns the \code{case_length} or \code{recurrence_length}
#' required to cover the supplied date or period.
#'
#' \bold{\code{epid_window}} - Returns the equivalent \code{case_length} or \code{recurrence_length} in other episode units.
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

  number_line(right_point(date) + (lengths@start * as.numeric(episode_units[episode_unit])),
              right_point(date) + (right_point(lengths) * as.numeric(episode_units[episode_unit])))
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
  number_line((as.numeric(windows@start) - as.numeric(right_point(date)))/as.numeric(episode_units[episode_unit]),
              (as.numeric(right_point(windows)) - as.numeric(right_point(date)))/as.numeric(episode_units[episode_unit]))
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

#' @rdname windows
#' @examples
#'
#' # What is the equivalent `length` in other episode units?
#' epid_units(1, 'months')
#' epid_units(31, 'days')
#'
#' @export
epid_units <- function(lengths, episode_unit = 'months') {
  data('episode_units')
  lengths <- lengths * episode_units[[episode_unit]]
  return(
    lapply(episode_units, function(x) lengths/x))
}
