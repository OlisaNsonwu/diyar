#' @title Change the returned outputs of \code{diyar} functions
#'
#' @description Convert the returned output of \code{\link{number_line}}, \code{\link{record_group}}, \code{\link{episode_group}}, \code{\link{fixed_episodes}} and \code{\link{rolling_episodes}}
#' from a \code{data.frame} to \link[=number_line-class]{number_line}, \link[=pid-class]{pid} or \link[=epid-class]{epid} objects, and vice versa.
#'
#' @aliases to_s4
#'
#' @param df \code{data.frame}
#' @examples
#' data(infections)
#' dates <- infections$date
#' output <- fixed_episodes(dates, case_length=30)
#' output
#'
#' # from the a pid/epid object to a data.frame
#' df_output <- to_df(output)
#' df_output
#'
#' # from a data.frame to pid/epid object
#' s4_output <- to_s4(df_output)
#' s4_output
#'
#' all(s4_output == output)
#'
#' @return to_s4 - \link[=pid-class]{pid} or \link[=epid-class]{epid} objects
#' @export
to_s4 <- function(df){
  if(missing(df)) stop("argument 'df' is missing, with no default")
  if(!is.data.frame(df)) stop("'df' must be a data.frame")

  if(any(names(df)=="epid")){
    s4 <- methods::new("epid", .Data=df$epid)
  }else if(any(names(df)=="pid")){
    s4 <- methods::new("pid", .Data=df$pid)
  }else if(any(names(df)=="gid")){
    s4 <- methods::new("number_line", .Data= ifelse(as.numeric(df$start) > as.numeric(df$end),  -as.numeric(df$start - df$end),  as.numeric(df$end - df$start)))
  }

  vrs <- subset(names(df), names(df) %in% methods::slotNames(s4))

  for(i in 1:length(vrs)){
    methods::slot(s4, vrs[i]) <- df[[vrs[i]]]
  }
  s4
}

#' @rdname to_s4
#' @param s4 \link[=pid-class]{pid} or \link[=epid-class]{epid} objects
#' @return to_df - \code{data.frame} object
#' @export
to_df <- function(s4){
  if(missing(s4)) stop("argument 's4' is missing, with no default")
  if(!class(s4) %in% c("epid","pid","number_line", "pane")) stop("'s4' must be an `epid`, `pid`, `pane` or `number_line` object")
  if(all(class(s4)=="epid")){
    if(length(s4) == 0){
      return(
        structure(list(epid = integer(0),
                       sn = integer(0),
                       wind_id = integer(0),
                       wind_nm = character(0),
                       case_nm = character(0),
                       dist_wind_index = numeric(0),
                       dist_epid_index = numeric(0),
                       epid_total = integer(0),
                       iteration = numeric(0)),
                  row.names = integer(0),
                  class = "data.frame")
      )
    }else{
      df <- data.frame(epid = s4@.Data, stringsAsFactors = FALSE)
    }

  }else if(all(class(s4) == "pid")){
    if(length(s4) == 0){
      return(
        structure(list(pid = numeric(0),
                       sn = integer(0),
                       pid_cri = numeric(0),
                       link_id = numeric(0),
                       pid_total = integer(0),
                       iteration = numeric(0)),
                  row.names = integer(0),
                  class = "data.frame")
      )
    }else{
      df <- data.frame(pid = s4@.Data, stringsAsFactors = FALSE)
    }
  }else if(all(class(s4) == "number_line")){
    if(length(s4) == 0){
      return(
        structure(list(end = integer(0),
                       start = integer(0),
                       id = integer(0),
                       gid = integer(0)),
                  row.names = integer(0),
                  class = "data.frame")
      )
    }else{
      df <- data.frame(end = s4@start + s4@.Data, stringsAsFactors = FALSE)
    }
  }else if(all(class(s4) == "pane")){
    if(length(s4) == 0){
      return(
        structure(list(pane = integer(0),
                       sn = integer(0),
                       case_nm = character(0),
                       dist_pane_index = numeric(0),
                       window_matched = numeric(0),
                       pane_total = integer(0)),
                  row.names = integer(0),
                  class = "data.frame")
      )
    }else{
      df <- data.frame(pane = s4@.Data, stringsAsFactors = FALSE)
    }
  }

  vrs <- methods::slotNames(s4)

  for(i in 1:length(vrs)){
    if(!vrs[i] %in% c("options", "window_list")){
      if (length(methods::slot(s4, vrs[i])) !=0 & vrs[i] !=".Data"){
        if(vrs[i] == "epid_interval"){
          df$epid_start <- left_point(methods::slot(s4, vrs[i]))
          if(length(right_point(methods::slot(s4, vrs[i]))) == 0) df$epid_end <- NULL else df$epid_end <- right_point(methods::slot(s4, vrs[i]))
        }else if(vrs[i] == "pane_interval"){
          df$pane_start <- left_point(methods::slot(s4, vrs[i]))
          if(length(right_point(methods::slot(s4, vrs[i]))) == 0) df$pane_end <- NULL else df$pane_end <- right_point(methods::slot(s4, vrs[i]))
        }
        else{
          df[[vrs[i]]] <- methods::slot(s4, vrs[i])
        }
      }
    }
  }
  df
}
