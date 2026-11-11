#' @name custom_sort
#' @aliases custom_sort
#' @title Nested ranking
#'
#' @param ... \code{[atomic]}. Sequence of \code{atomic} vectors. Passed to \bold{\code{\link{order}}}.
#' @param decreasing \code{[logical]}. Sort order. Passed to \bold{\code{\link{order}}}.
#' @param unique If \code{FALSE} (default), ties get the same rank.
#' If \code{TRUE}, ties are broken and all observations receive a unique nested rank.
#'
#' @description Returns tied or untied ranks from nested sorting of vectors.
#'
#' @return \code{numeric} Ranking.
#'
#' @examples
#'
#' a <- c(1, 1, 1, 2, 2)
#' b <- c(2, 3, 2, 1, 1)
#'
#' # Sort by a, then b
#' custom_sort(a, b)
#' # Sort by b, then a
#' custom_sort(b, a)
#' # Break ties
#' custom_sort(b, a, unique = TRUE)
#'
#' @export
custom_sort <- function(..., decreasing = FALSE, unique = FALSE){
  if(!unique){
    bys_position(..., from_last = decreasing)
  }else{
    bys_rank(..., from_last = decreasing)
  }
}
