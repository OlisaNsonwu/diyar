#' @name reframe
#' @aliases reframe
#' @title Modify \code{sub_criteria} objects.
#'
#' @description
#' Modify the attributes of a \code{\link{sub_criteria}} object.
#'
#' @param x \code{[\link{sub_criteria}]}.
#' @param ... Arguments passed to methods.
#' @param func \code{[function]}. Transformation function.
#'
#' @seealso \code{\link{sub_criteria}}; \code{\link{eval_sub_criteria}}; \code{\link{attr_eval}}
#'
#' @examples
#' s_cri <- sub_criteria(month.abb, month.name)
#' reframe(s_cri, func = function(x) x[12])
#' reframe(s_cri, func = function(x) x[12:1])
#' reframe(s_cri, func = function(x) attrs(x[1:6], x[7:12]))
#'
#' x <- sub_criteria(rep(1, 5), rep(5 * 10, 5), operator = 'and')
#' x <- sub_criteria(x, c(1,9,1,11,5), operator = 'or')
#' x
#' format(x, show_levels = TRUE)
#' unpack_sub_criteria(x)
#' flatten_list(unpack_sub_criteria(x), depth = 0)
#' lapply(flatten_list(unpack_sub_criteria(x), depth = 0), max)
#'
#' @export
reframe <- function(x, ...) UseMethod("reframe")
#'
#' @rdname reframe
#' @export
reframe.sub_criteria <- function(x, func = identity, ...){
  for(i in seq_len(length(x))){
    if(inherits(x[[i]][[1]], 'sub_criteria')){
      x[[i]][[1]] <- reframe.sub_criteria(x[[i]][[1]], func = func)
    }else{
      x[[i]][[1]] <- func(x[[i]][[1]])
    }
  }
  return(x)
}

#' @rdname reframe
#' @export
unpack_sub_criteria <- function(x, part = 'attribute'){
  part_num <- match(part, c('attribute', 'match_func', 'equal_func'))
  for(i in seq_len(length(x))){
    if(inherits(x[[i]][[1]], 'sub_criteria')){
      x[[i]] <- unpack_sub_criteria(x[[i]][[1]], part = part)
    }else{
      x[[i]] <- x[[i]][[part_num]]
    }
  }
  class(x) <- NULL
  attr(x, "operator") <- NULL
  return(x)
}

#' @rdname reframe
#' @export
flatten_list <- function(x, depth = 1){
  classes <- unlist(lapply(x, class))
  lgk <- classes %in% c("list", "d_attribute")
  x.1 <- x[!lgk]
  x.2 <- unlist(x[lgk], recursive = FALSE)
  if(length(x.1) > 0){
    names(x.1) <- paste0("var_", depth, ".", seq_len(length(x.1)))
  }
  if(length(x.2) > 0){
    names(x.2) <- paste0("var_", depth + 1, ".", seq_len(length(x.2)))
  }
  x <- c(x.1, x.2)
  classes <- unlist(lapply(x, class))
  lgk <- classes == "list"
  if(any(lgk)){
    flatten_list(x, depth = depth + 1)
  }else{
    x
  }
}
