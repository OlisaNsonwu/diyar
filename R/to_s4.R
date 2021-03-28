#' @title Change S4 objects in \code{diyar} to data frames and vice versa
#'
#' @description Convert \link[=pid-class]{pid}, \link[=epid-class]{epid}, \link[=pane-class]{pane} or \link[=number_line-class]{number_line} objects
#' to a \code{data.frame} and vice versa.
#'
#' @aliases to_s4
#'
#' @param df \code{[data.frame]}
#' @examples
#' data(infections)
#' dates <- infections$date
#' output <- episodes(dates, case_length=30)
#' output
#'
#' # Change a pid, epid, pane or number_line object to a data.frame
#' df_output <- to_df(output)
#' df_output
#'
#' # Change a data.frame to a pid, epid, pane or number_line object
#' s4_output <- to_s4(df_output)
#' s4_output
#'
#' @rdname to_s4
#' @param s4 \code{[\link[=pid-class]{pid}|\link[=epid-class]{epid}|\link[=pane-class]{pane}|\link[=number_line-class]{number_line}]}
#' @param ... Arguments passed to \code{data.frame}
#' @return to_df - \code{data.frame} object
#' @details \code{to_df} has been retired. Moving forward, please use \code{as.data.frame}.
#' @export
to_df <- function(s4, ...) as.data.frame(df, ...)

#' @return to_s4 - \link[=pid-class]{pid}, \link[=epid-class]{epid}, \link[=pane-class]{pane} or \link[=number_line-class]{number_line} objects
#' @export
to_s4 <- function(df){
  if(missing(df)) stop("argument 'df' is missing, with no default")
  if(!is.data.frame(df)) stop("'df' must be a data.frame")

  if(any(names(df) == "epid")){
    s4 <- methods::new("epid", .Data=df$epid)
  }else if(any(names(df) == "pid")){
    s4 <- methods::new("pid", .Data=df$pid)
  }else if(any(names(df) == "gid")){
    s4 <- methods::new("number_line", .Data = as.numeric(df$end) - as.numeric(df$start))
  }

  vrs <- subset(names(df), names(df) %in% methods::slotNames(s4))

  for(i in 1:length(vrs)){
    methods::slot(s4, vrs[i]) <- df[[vrs[i]]]
  }
  s4
}
