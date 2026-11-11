#' @name sub_criteria
#' @aliases sub_criteria
#' @title Match criteria
#'
#' @description Create, evaluate and validate match criteria for record linkage with \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}
#' @param ...
#' \code{[atomic]} in \code{print(), format(), eval_sub_criteria()} - Arguments passed to methods.
#'
#' \code{[Any object]} in \code{attrs()} - Attributes i.e. match criteria.
#'
#' \code{[atomic|d_attribute|sub_criteria]} in \code{sub_criteria()} - Attributes i.e. match criteria.
#'
#' @param x \code{[sub_criteria]}.
#' @param .obj \code{[data.frame|list]}. Attributes i.e. match criteria.
#' @param match_funcs \code{[function]}. User defined logical test for matches.
#' @param equal_funcs \code{[function]}. User defined logical test for identical records (i.e. rows).
#' @param operator \code{[character]}. Options are \code{"and"} or \code{"or"}.
#' @param show_levels \code{[logical]}. If \code{TRUE}, show recursive depth for each logic statement of the match criteria.

#' @param x_pos \code{[integer]}. Index of one half of a record pair.
#' @param y_pos \code{[integer]}. Index of one half of a record pair.
#' @param check_duplicates \code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.
#' @seealso
#' \code{\link{predefined_tests}}; \code{\link{links}}; \code{\link{episodes}};
#' \code{\link{eval_sub_criteria}}; \code{\link{unpack_sub_criteria}};
#' \code{\link{reframe}}
#' @return \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{sub_criteria()}} - Creates a match criteria as a \code{sub_criteria} object.
#' A \code{sub_criteria} object contains attributes to be compared,
#' logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
#' another set of logical tests to determine identical records.
#'
#' \bold{\code{attrs()}} - Creates a \code{d_attribute} object - a collection of objects that can be passed to \bold{\code{sub_criteria()}} as a single attribute.
#'
#' \bold{\code{eval_sub_criteria()}} - Evaluates a \code{sub_criteria} object.
#'
#' \bold{\code{validate_sub_criteria()}} - Checks that a \code{sub_criteria} object is structurally valid.
#'
#' At each iteration of \code{\link{links}} or \code{\link{episodes}}, record-pairs are created from each attribute of a \code{sub_criteria} object.
#' \code{eval_sub_criteria()} evaluates each record-pair using the \code{match_funcs} and \code{equal_funcs} functions of a \code{sub_criteria} object.
#' See \bold{\code{\link{predefined_tests}}} for examples of \code{match_funcs} and \code{equal_funcs}.
#'
#' User-defined functions are also permitted as \code{match_funcs} and \code{equal_funcs}.
#' Such functions must meet three requirements:
#' \enumerate{
#' \item It must be able to compare the attributes.
#' \item It must have two arguments named \code{`x`} and \code{`y`}, where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
#' \item It must return a \code{logical} object i.e. \code{TRUE} or \code{FALSE}, or a list whose first element is the \code{logical} object i.e. \code{list(TRUE, ...)} or \code{list(FALSE, ...)}
#' }
#'
#' \code{attrs()} is useful when the match criteria requires an interaction between the multiple attributes.
#' For example, attribute 1 + attribute 2 > attribute 3.
#' Objects assigned to \code{...} are put in a list while those passed to \code{.obj}
#' are converted to a list.
#'
#' Every attribute, including those in \code{attrs()}, must have the same length or a length of 1.
#'
#' @examples
#' # Attributes
#' attr_1 <- c(30, 28, 40, 25, 25, 29, 27)
#' attr_2 <- c("M", "F", "U", "M", "F", "U", "M")
#'
#' # A match criteria
#' # Example 1 - A maximum difference of 10 in attribute 1
#' s_cri1 <- sub_criteria(attr_1, match_funcs = range_match)
#' s_cri1
#'
#' # Evaluate the match criteria
#'  # Compare the first element (default) of 'attr_1'
#'  # against all other elements
#' eval_sub_criteria(s_cri1)
#'  # Compare the second element of 'attr_1'
#'  # against all other elements
#' x_pos_val <- seq_len(max(attr_eval(s_cri1)))
#' eval_sub_criteria(
#'   s_cri1, x_pos = x_pos_val,
#'   y_pos = rep(2, length(x_pos_val)))
#'
#' # Example 2 - `s_cri1` AND an exact match on attribute 2
#' s_cri2 <- sub_criteria(
#'   s_cri1,
#'   sub_criteria(attr_2, match_funcs = exact_match),
#'   operator = "and")
#' s_cri2
#'
#' ## Example 3 - `s_cri1` OR an exact match on attribute 2
#' s_cri3 <- sub_criteria(
#'   s_cri1,
#'   sub_criteria(attr_2, match_funcs = exact_match),
#'   operator = "or")
#' s_cri3
#'
#' # Evaluate the match criteria
#' eval_sub_criteria(s_cri2)
#' eval_sub_criteria(s_cri3)
#'
#' # Alternatively, using `attrs()`
#' AND_func <- function(x, y) range_match(x$a1, y$a1) & x$a2 == y$a2
#' OR_func <- function(x, y) range_match(x$a1, y$a1) | x$a2 == y$a2
#'
#' ## Create a match criteria
#' s_cri2b <- sub_criteria(
#'   attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'    match_funcs = AND_func)
#' s_cri3b <- sub_criteria(
#'   attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'   match_funcs = OR_func)
#'
#' # Evaluate the match criteria
#' eval_sub_criteria(s_cri2b)
#' eval_sub_criteria(s_cri3b)
#'
#' @export
sub_criteria <- function(
    ..., match_funcs = c("exact" = exact_match),
    equal_funcs = c("exact" = exact_match), operator = "or"){

  err <- err_sub_criteria_0(...,
                            match_funcs = match_funcs,
                            equal_funcs = equal_funcs,
                            operator = operator)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(!inherits(match_funcs, "list")) match_funcs <- list(match_funcs)
  if(!inherits(equal_funcs, "list")) equal_funcs <- list(equal_funcs)

  if(length(match_funcs) == 1){
    match_funcs <- rep(list(match_funcs), length(list(...)))
    match_funcs <- unlist(match_funcs)
  }
  if(length(equal_funcs) == 1){
    equal_funcs <- rep(list(equal_funcs), length(list(...)))
    equal_funcs <- unlist(equal_funcs)
  }

  attr <- list(...)
  sub_cris <- lapply(1:length(attr), function(i){
    x <- c(attr[i], match_funcs[i], equal_funcs[i])
    names(x) <- c('attribute', 'match_func', 'equal_func')
    return(x)
  })

  class(sub_cris) <- "sub_criteria"
  attr(sub_cris, "operator") <- operator

  err <- err_sub_criteria_5.0(sub_cris, funcs_l = "match_funcs", funcs_pos = 2)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  err <- err_sub_criteria_5.0(sub_cris, funcs_l = "equal_funcs", funcs_pos = 3)
  if(!isFALSE(err)) stop(err, call. = FALSE)
  sub_cris
}

#' @rdname sub_criteria
#'
#' @export
attrs <- function(..., .obj = NULL){
  x <- c(list(...), as.list(.obj))
  err <- err_3dot_lens(x)
  if(!isFALSE(err)) stop(err, call. = FALSE)
  class(x) <- "d_attribute"
  x
}

#' @rdname sub_criteria
#' @export
eval_sub_criteria <- function(x, ...) UseMethod("eval_sub_criteria")

#' @rdname sub_criteria
#' @export
print.sub_criteria <- function(x, ...){
  format(x, ...)
}

#' @rdname sub_criteria
#' @export
format.sub_criteria <- function(x, show_levels = FALSE, ...){
  if(isTRUE(show_levels)){
    cat(paste0("logical_test-", fmt_sub_criteria(x, show_levels = show_levels), "\n"))
  }else{
    cat(paste0(fmt_sub_criteria(x, show_levels = show_levels), "\n"))
  }
}

#' @rdname sub_criteria
#' @export
eval_sub_criteria.sub_criteria <- function(
    x,
    x_pos = seq_len(max(attr_eval(x))),
    y_pos = rep(1L, length(x_pos)),
    check_duplicates = TRUE,
    ...){
  .evaluate_sub_criteria(
    x = x, x_pos = x_pos, y_pos = y_pos, check_duplicates = check_duplicates)
}

#' @rdname sub_criteria
#' @export
validate_sub_criteria <- function(x){
  .traverse_sub_criteria(x)
}
