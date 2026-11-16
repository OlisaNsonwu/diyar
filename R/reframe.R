#' @name reframe
#' @aliases reframe
#' @title Access and modify \code{sub_criteria} objects.
#'
#' @description
#' Access and modify the attributes of a \code{\link{sub_criteria}} object.
#'
#' @param x \code{[\link{sub_criteria}]}.
#' @param ... Arguments passed to methods.
#' @param part \code{[character]} Component of the \code{\link{sub_criteria}} object to extract.
#' Options are one of \code{"attribute"}, \code{"match_func"} or \code{"equal_func"}.
#' @param func \code{[function]}. Transformation function.
#'
#' @details
#' \code{reframe()} - Modifies a \code{\link{sub_criteria}} object by transforming
#' its attributes. \code{reframe()} automatically validates the modified
#' \code{\link{sub_criteria}} object to ensure it is still structurally valid.
#'
#' \code{unpack_sub_criteria()} - Extracts nested attributes,
#' match functions, or equality functions from a \code{sub_criteria} object.
#'
#' The combination of both functions can be used to inspect and modify
#' a \code{sub_criteria} object.
#'
#' @seealso \code{\link{sub_criteria}}; \code{\link{eval_sub_criteria}}; \code{\link{validate_sub_criteria}}
#'
#' @examples
#' # Modify `sub_criteria` object
#'   # Modifying attributes
#' s_cri <- sub_criteria(month.abb, month.name)
#' s_cri
#'   # Retain the 12th element of each attribute
#' reframe(s_cri, func = function(x) x[12])
#'   # Reverse the order of each attribute
#' reframe(s_cri, func = function(x) rev(x))
#'   # Split each attribute into a two-element d_attribute object
#' reframe(s_cri, func = function(x) attrs(x[1:6], x[7:12]))
#'
#' # Extracting components from a `sub_criteria` object
#' x <- sub_criteria(rep(1, 5), rep(5 * 10, 5), operator = 'and')
#' x <- sub_criteria(x, c(1,9,1,11,5), operator = 'or')
#' format(x, show_levels = TRUE)
#'  # Extract the `sub_criteria` match functions
#' unpack_sub_criteria(x, part = 'match_func')
#'  # Extract the `sub_criteria` attributes
#' unpack_sub_criteria(x, part = 'attribute')
#'  # Flatten before modifying the extracted `sub_criteria` attributes
#' flatten_list(unpack_sub_criteria(x))
#' lapply(flatten_list(unpack_sub_criteria(x)), max)
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

  y <- try(validate_sub_criteria(x), silent = TRUE)
  if(inherits(y, 'try-error')){
    stop(call. = FALSE,
      paste0(
        'The modified `sub_criteria` is no longer valid\n',
        'x - The reframed `sub_criteria` object failed validation with the following error:\n',
        'x - \"', gsub('\\n$','', format(y)), '\"\n'))
  }
  return(x)
}

#' @rdname reframe
#' @export
reframe.d_attribute <- function(x, func = identity, ...){
  for(i in seq_len(length(x))){
    if(inherits(x[[i]], 'd_attribute')){
      x[[i]] <- reframe.d_attribute(x[[i]], func = func)
    }else{
      x[[i]] <- func(x[[i]])
    }
  }

  class(x) <- NULL
  y <- try(x <- do.call(attrs, x), silent = TRUE)
  if(inherits(y, 'try-error')){
    stop(call. = FALSE,
         paste0(
           'The modified `d_attribute` is no longer valid\n',
           'x - The reframed `d_attribute` object failed validation with the following error:\n',
           'x - \"', gsub('\\n$','', format(y)), '\"\n'))
  }
  return(x)
}

#' @rdname reframe
#' @export
reframe.list <- function(x, func = identity, ...){
  for(i in seq_len(length(x))){
    if(all(class(x[[i]]) == 'list')){
      x[[i]] <- reframe.list(x[[i]], func = func)
    }else{
      x[[i]] <- func(x[[i]])
    }
  }
  return(x)
}

#' @rdname reframe
#' @export
unpack_sub_criteria <- function(x, part = 'attribute'){
  if(length(part) != 1){
    stop(call. = FALSE,
      paste0(
        'Invalid option for `part`.\n',
        'i - Only one part of a `sub_criteria` object can be selected.\n',
        "i - Select only one of 'attribute', 'match_func' or 'equal_func'.\n",
        "x - You've supplied ", listr(paste0("'", part, "'"))
        ))
  }
  part_num <- match(part, c('attribute', 'match_func', 'equal_func'))

  for(i in seq_len(length(x))){
    part_num <- match(part, names(x[[i]]))
    if(is.na(part_num)){
      x[[i]] <- NA
    }else if(inherits(x[[i]][[1]], 'sub_criteria')){
      x[[i]] <- unpack_sub_criteria(x[[i]][[1]], part = part)
    }else{
      x[[i]] <- x[[i]][[part_num]]
    }
  }
  class(x) <- NULL
  attr(x, "operator") <- NULL
  return(x)
}

