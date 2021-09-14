#' @name sub_criteria
#' @aliases sub_criteria
#' @title Sub-criteria
#'
#' @description Additional matching criteria for each iteration of \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}.
#' @param ... \code{[atomic]}. Attributes.
#' @param .obj \code{[data.frame|list]}. Attributes
#' @param match_funcs \code{[function]}. User defined logical test for matches.
#' @param equal_funcs \code{[function]}. User defined logical test for identical record sets (all attributes of the same record).
#' @param operator \code{[character]}. Options are \code{"and"} or \code{"or"}.
#' @seealso
#' \code{\link{predefined_tests}}; \code{\link{links}}; \code{\link{episodes}}; \code{\link{eval_sub_criteria}}
#'
#' @return \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{sub_criteria()}} - The mechanism for providing matching criteria to an iteration of \bold{\code{links}} or \bold{\code{episodes}}.
#' It creates a \code{sub_criteria} class object which contains the attributes to be compared,
#' logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
#' another set of logical tests to determine identical records.
#'
#' \bold{\code{attrs()}} - Passes a collection of attributes to \bold{\code{sub_criteria()}}.
#'
#' Every attribute, including those in a collection, must have
#' the same length or a length of 1.
#'
#' @examples
#' # Sub-criteria
#' s_cri1 <- sub_criteria(c(30, 28, 40, 25, 25, 29, 27),
#'                        match_funcs = range_match)
#' s_cri2 <- sub_criteria(c(30, 28, 40, 25, 25, 29, 27),
#'                        match_funcs = exact_match)
#'
#' # Nested sub-criteria
#' s_cri3 <- sub_criteria(s_cri1, s_cri2, operator = "or")
#' s_cri4 <- sub_criteria(s_cri1, s_cri3, operator = "and")
#'
#' @export
sub_criteria <- function(...,
                         match_funcs = diyar::exact_match,
                         equal_funcs = diyar::exact_match,
                         operator = "or"){

  err <- err_sub_criteria_0(...,
                            match_funcs = match_funcs,
                            equal_funcs = equal_funcs,
                            operator = operator)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(class(match_funcs) != "list") match_funcs <- list(match_funcs)
  if(class(equal_funcs) != "list") equal_funcs <- list(equal_funcs)

  if(length(match_funcs) == 1){
    match_funcs <- rep(list(match_funcs), length(list(...)))
    match_funcs <- unlist(match_funcs)
  }
  if(length(equal_funcs) == 1){
    equal_funcs <- rep(list(equal_funcs), length(list(...)))
    equal_funcs <- unlist(equal_funcs)
  }

  x <- function(x, y, z) list(x, y, z)
  sub_cris <- mapply(x, list(...), match_funcs, equal_funcs, SIMPLIFY = F)

  class(sub_cris) <- "sub_criteria"
  attr(sub_cris, "operator") <- operator

  err <- err_sub_criteria_5.0(sub_cris, funcs_l = "match_funcs", funcs_pos = 2)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  err <- err_sub_criteria_5.0(sub_cris, funcs_l = "equal_funcs", funcs_pos = 3)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  rm(list = ls()[ls() != "sub_cris"])
  sub_cris
}


#' @rdname sub_criteria
#' @examples
#' # Objects of the same length
#' attrs(month.abb, month.name)
#'
#' # Or a data.frame or list with elements of the same length
#' attrs(.obj = mtcars)
#'
#' # Or a combinaiton of the both
#' attrs(mtcars$mpg, mtcars$cyl, .obj = mtcars)
#'
#' # Each can then be passed to a `sub-criteria`
#' sub_criteria(
#' month.abb,
#' month.name,
#' attrs(month.abb, month.name)
#' )
#'
#' @export
attrs <- function(..., .obj = NULL){
  x <- c(list(...), as.list(.obj))
  err <- err_3dot_lens(x)
  if(!isFALSE(err)) stop(err, call. = FALSE)
  rm(list = ls()[ls() != "x"])
  class(x) <- "d_attribute"
  x
}
