#' @name transfer
#' @title Converting
#'
#' @description Convert
#'
#' @aliases transfer
#'
#' @param df \code{df}
#' @return \code{epid} object class
#' @export
to_epid <- function(df){
  if(!is.data.frame(df)) stop("'df' must be a data.frame")
  s4 <- methods::new("epid", .Data=df$epid)
  vrs <- subset(names(df), names(df) %in% methods::slotNames(s4))

  for(i in 1:length(vrs)){
    methods::slot(s4, vrs[i]) <- df[[vrs[i]]]
  }
  s4
}

#' @rdname transfer
#' @param s4 s4
#' @return \code{data.frame} object class
#' @export
to_df <- function(s4){
  if(!class(s4) %in% c("epid","pid")) stop("'s4' must be an epid or pid object")
  df <- data.frame(epid = s4@.Data, stringsAsFactors = FALSE)
  vrs <- methods::slotNames(s4)

  for(i in 1:length(vrs)){
    if (length(methods::slot(s4, vrs[i])) !=0 & vrs[i] !=".Data"){
      df[[vrs[i]]] <- methods::slot(s4, vrs[i])
    }
  }
  df
}
