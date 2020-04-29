#' @title Change the returned outputs of \code{diyar} functions
#'
#' @description Convert the returned output of \code{\link{number_line}}, \code{\link{record_group}}, \code{\link{episode_group}}, \code{\link{fixed_episodes}} and \code{\link{rolling_episodes}}
#' from the a \code{data.frame} to \link[=number_line-class]{number_line}, \link[=pid-class]{pid} or \link[=epid-class]{epid} objects, and vice versa.
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
  if(!class(s4) %in% c("epid","pid","number_line")) stop("'s4' must be an epid or pid object")
  if(all(class(s4)=="epid")){
    df <- data.frame(epid = s4@.Data, stringsAsFactors = FALSE)
  }else if(all(class(s4)=="pid")){
    df <- data.frame(pid = s4@.Data, stringsAsFactors = FALSE)
  }else if(all(class(s4)=="number_line")){
    df <- data.frame(end = s4@start + s4@.Data, stringsAsFactors = FALSE)
  }

  vrs <- methods::slotNames(s4)

  for(i in 1:length(vrs)){
    if (length(methods::slot(s4, vrs[i])) !=0 & vrs[i] !=".Data"){
      df[[vrs[i]]] <- methods::slot(s4, vrs[i])
    }
  }
  df
}
