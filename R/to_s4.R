#' @title Change the returned outputs of \code{diyar} functions from \code{data.frames} to S4 objects, and vice versa
#'
#' @description Convert the returned output of \code{\link{record_group}}, \code{\link{episode_group}}, \code{\link{fixed_episodes}} and \code{\link{rolling_episodes}}
#' from the current default (\code{data.frame}) to \link[=pid-class]{pid} or \link[=epid-class]{epid} objects, and vice versa.
#'
#' @aliases to_s4
#'
#' @param df \code{data.frame}. Returned output of \code{\link{record_group}}, \code{\link{episode_group}}, \code{\link{fixed_episodes}} and \code{\link{rolling_episodes}}
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
  if(!is.data.frame(df)) stop("'df' must be a data.frame")

  if(any(names(df)=="epid")){
    s4 <- methods::new("epid", .Data=df$epid)
  }else if(any(names(df)=="pid")){
    s4 <- methods::new("pid", .Data=df$pid)
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
  if(!class(s4) %in% c("epid","pid")) stop("'s4' must be an epid or pid object")
  if(all(class(s4)=="epid")){
    df <- data.frame(epid = s4@.Data, stringsAsFactors = FALSE)
  }else if(all(class(s4)=="pid")){
    df <- data.frame(pid = s4@.Data, stringsAsFactors = FALSE)
  }

  vrs <- methods::slotNames(s4)

  for(i in 1:length(vrs)){
    if (length(methods::slot(s4, vrs[i])) !=0 & vrs[i] !=".Data"){
      df[[vrs[i]]] <- methods::slot(s4, vrs[i])
    }
  }
  df
}
