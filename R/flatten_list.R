#' @name flatten_list
#' @title Flatten lists and d_attribute objects.
#' @description Flatten nested lists and d_attribute objects into a single list.
#'
#' @param x \code{[list|d_attribute]}.
#'
#' @return list
#'
#' @examples
#' # Nested list
#' x1 <- list(1, list(2, list(3, 4)))
#' # Nested d_attribute object
#' x2 <- attrs(1, attrs(2, attrs(3, 4)))
#' # Combined nested list and d_attribute object
#' x3 <- list(x1, x2)
#'
#' flatten_list(x1)
#' flatten_list(x2)
#' flatten_list(x3)
#'
#' @export
flatten_list <- function(x){
  .flatten_list(x, depth = 1)
}
