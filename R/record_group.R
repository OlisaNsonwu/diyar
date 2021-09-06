#' @name record_group
#' @title Multistage deterministic record linkage
#'
#' @description Match records in successive stages with different matching conditions.
#' Each set of linked records are assigned a unique identifier with relevant group-level information.
#'
#' @param df \code{[data.frame]}. Deprecated. One or more datasets appended together. See \code{Details}.
#' @param ... Arguments passed to \code{links}.
#'
#' @return \code{\link[=pid-class]{pid}}
#'
#' @seealso \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}; \code{\link{schema}}
#'
#' @details
#'
#' @aliases record_group
#' @export
record_group <- function(df, ..., to_s4 = TRUE){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i- `record_group()` has been retired!\n",
                  "i - Your values will be passed to `links()`.\n",
                  "i - Please specify any argument you've use.")
    stop(err, call. = FALSE)
  }

  out <- bridge_record_group(df = df, args = args)
  if(out$err_cd == FALSE) {
    stop(out$err_nm, call. = FALSE)
  }
  warning(paste0("`record_group()` has been retired!:\n",
                 "i - Please use `links()` instead.\n",
                 "i - Your values were passed to `links()`."), call. = FALSE)
  if(to_s4 != TRUE){
    out <- to_df(out$err_nm)
  }else{
    out <- out$err_nm
  }
  rm(list = ls()[ls() != "out"])
  return(out)
}
