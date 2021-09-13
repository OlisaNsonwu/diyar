#' @name reframe
#' @aliases reframe
#' @title \code{sub_criteria} object
#'
#' @description
#' Modify the attributes of a \code{\link{sub_criteria}} object.
#'
#' @param x \code{[\link[=sub_criteria-class]{sub_criteria}]}.
#' @param func \code{[function]}. Transformation function.
#'
#' @seealso \code{\link{sub_criteria}}; \code{\link{eval_sub_criteria}}; \code{\link{attr_eval}}
#'
#' @examples
#' s_cri <- sub_criteria(
#' month.abb,
#' month.name
#' )
#'
#' reframe(s_cri, func = function(x) x[12])
#' reframe(s_cri, func = function(x) x[12:1])
#' reframe(s_cri, func = function(x) attrs(x[1:6], x[7:12]))
#' @export
reframe <- function(x, ...) UseMethod("reframe")
#'
#' @rdname reframe
#' @export
reframe.sub_criteria <- function(x, func = identity){
  for (i in seq_len(length(x))) {
    attr <- (x[[i]][[1]])
    if(all(class(attr) == "sub_criteria")){
      x[[i]][[1]] <- reframe.sub_criteria(attr, func = func)
    }else if(all(class(attr) == "d_attribute")){
      attr <- lapply(attr, function(x){
        func(x)
      })
      class(attr) <- "d_attribute"
      x[[i]][[1]] <- attr
    }else{
      x[[i]][[1]] <- func(attr)
    }
  }

  err <- err_sub_criteria_3dot_1(x)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  rm(list = ls()[ls() != "x"])
  return(x)
}
