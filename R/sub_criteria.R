#' @name sub_criteria
#' @aliases sub_criteria
#' @title Match criteria
#'
#' @description Match criteria for record linkage with \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}
#' @param ... \code{[atomic]} Attributes passed to or \code{eval_sub_criteria()} or \code{eval_sub_criteria()}
#'
#' Arguments passed to methods for \code{eval_sub_criteria()}
#' @param x \code{[sub_criteria]}. Attributes.
#' @param .obj \code{[data.frame|list]}. Attributes.
#' @param match_funcs \code{[function]}. User defined logical test for matches.
#' @param equal_funcs \code{[function]}. User defined logical test for identical record sets (all attributes of the same record).
#' @param operator \code{[character]}. Options are \code{"and"} or \code{"or"}.
#' @param show_levels \code{[logical]}. If \code{TRUE}, show recursive depth for each logic statement of the match criteria.

#' @param x_pos \code{[integer]}. Index of one half of a record pair.
#' @param y_pos \code{[integer]}. Index of one half of a record pair.
#' @param check_duplicates \code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.
#' @param depth \code{[integer]}. First order of recursion.
#' @seealso
#' \code{\link{predefined_tests}}; \code{\link{links}}; \code{\link{episodes}}; \code{\link{eval_sub_criteria}}
#'
#' @return \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{sub_criteria()}} - Create a match criteria as a \code{sub_criteria} object.
#' A \code{sub_criteria} object contains attributes to be compared,
#' logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
#' another set of logical tests to determine identical records.
#'
#' \bold{\code{attrs()}} - Create a \code{d_attribute} object - a collection of atomic objects that can be passed to \bold{\code{sub_criteria()}} as a single attribute.
#'
#' \bold{\code{eval_sub_criteria()}} - Evaluates a \code{sub_criteria} object.
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
#' \item It must return a \code{logical} object i.e. \code{TRUE} or \code{FALSE}.
#' }
#'
#' \code{attrs()} is useful when the match criteria requires an interaction between the multiple attributes. For example, attribute 1 + attribute 2 > attribute 3.
#'
#' Every attribute, including those in \code{attrs()}, must have the same length or a length of 1.
#'
#' @examples
#' # Attributes
#' attr_1 <- c(30, 28, 40, 25, 25, 29, 27)
#' attr_2 <- c("M", "F", "U", "M", "F", "U", "M")
#'
#' # A match criteria
#' ## Example 1 - A maximum difference of 10 in attribute 1
#' s_cri1 <- sub_criteria(attr_1, match_funcs = range_match)
#' s_cri1
#'
#' # Evaluate the match criteria
#' ## Compare the first element of 'attr_1' against all other elements
#' eval_sub_criteria(s_cri1)
#' ## Compare the second element of 'attr_1' against all other elements
#' x_pos_val <- seq_len(max(attr_eval(s_cri1)))
#' eval_sub_criteria(s_cri1,
#'                   x_pos = x_pos_val,
#'                   y_pos = rep(2, length(x_pos_val)))
#'
#' ## Example 2 - `s_cri1` AND an exact match on attribute 2
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
#' # Alternatively, using `attr()`
#' AND_func <- function(x, y) range_match(x$a1, y$a1) & x$a2 == y$a2
#' OR_func <- function(x, y) range_match(x$a1, y$a1) | x$a2 == y$a2
#'
#' ## Create a match criteria
#' s_cri2b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'                         match_funcs = AND_func)
#' s_cri3b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'                         match_funcs = OR_func)
#'
#' # Evaluate the match criteria
#' eval_sub_criteria(s_cri2b)
#' eval_sub_criteria(s_cri3b)
#'
#' @export
sub_criteria <- function(...,
                         match_funcs = c("exact" = exact_match),
                         equal_funcs = c("exact" = exact_match),
                         operator = "or"){

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
    c(attr[i], match_funcs[i], equal_funcs[i])
  })

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

#' @rdname sub_criteria
#' @export
eval_sub_criteria <- function(x, ...) UseMethod("eval_sub_criteria")

#' @rdname sub_criteria
#' @export
print.sub_criteria <- function(x, ...){
  format(x)
}

#' @rdname sub_criteria
#' @export
format.sub_criteria <- function(x, show_levels =  FALSE, ...){
  if(isTRUE(show_levels)){
    cat(paste0("logical_test-", fmt_sub_criteria(x, show_levels = show_levels), "\n"))
  }else{
    cat(paste0(fmt_sub_criteria(x, show_levels = show_levels), "\n"))
  }
}

#' @rdname sub_criteria
#' @export
eval_sub_criteria.sub_criteria <- function(x,
                                           x_pos = seq_len(max(attr_eval(x))),
                                           y_pos = rep(1L, length(x_pos)),
                                           check_duplicates = TRUE,
                                           depth = 0,
                                           ...){
  if(length(x_pos) == 0 & length(y_pos) == 0){
    x <- list(logical_test = integer())
    if(isFALSE(check_duplicates)){
      x$equal_test <- integer()
    }
    return(x)
  }
  curr_ds_len <- length(y_pos)
  matches <- lapply(1:length(x), function(j){
    a <- x[[j]]
    x <- a[[1]]
    f1 <- a[[2]]

    if(inherits(x,"sub_criteria")){
      xx <- eval_sub_criteria(x = x, x_pos = x_pos, y_pos = y_pos,
                             check_duplicates = check_duplicates,
                             depth = depth + 1)
      request_info <- any(grepl("^mf|^ef", names(xx)))
      if(isTRUE(request_info)){
        xx[[paste0("mf.", depth, ".", j)]] <- xx$logical_test
      }
      if(isFALSE(check_duplicates)){
        xx$logical_test <- as.numeric(as.logical(c(xx$logical_test, xx$equal_test)))
        xx$equal_test <- NULL
      }
      return(xx)
    }else{
      if(inherits(x,"d_attribute")){
        y <- rc_dv(x, func = function(x){
          if(length(x) <= 1) x else x[y_pos]
        })
        x <- rc_dv(x, func = function(x){
          if(length(x) <= 1) x else x[x_pos]
        })
      }else{
        if(length(x) == 1) x <- rep(x, curr_ds_len)
        y <- x[y_pos]
        x <- x[x_pos]
      }

      lgk <- try(f1(x, y), silent = TRUE)
      request_info <- !is.atomic(lgk)
      if(isTRUE(request_info)){
        out1_match <- lgk[[1]]
        out1_export <- lgk[[-1]]
      }else{
        out1_match <- lgk
      }
      if(inherits(out1_match, "try-error") | !inherits(out1_match, "logical")){
        if(inherits(out1_match, "try-error")){
          err <- attr(out1_match, "condition")$message
        }else{
          err <- "Output is not a `logical` object"
        }

        err <- paste0("Unable to evaluate `match_funcs`:\n",
                      "i - Each function in `match_funcs` must have the following syntax and output.\n",
                      "i - Syntax ~ `function(x, y, ...)`.\n",
                      "i - Output ~ `TRUE`, `FALSE`, `list(TRUE, ... )` or `list(FALSE, ... )`.\n",
                      "X - Issue with `match_funcs`: ", err, ".")
        stop(err, call. = F)
      }
      out1_match[is.na(out1_match)] <- 0
      if(length(out1_match) == 1) out1_match <- rep(out1_match, curr_ds_len)
      if(!length(out1_match) %in% c(1, curr_ds_len)){
        err <- paste0("Output length of `match_funcs` must be 1 or the same as `criteria`:\n",
                      "i - Unexpected length for `match_funcs`:\n",
                      "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                      "X - Length is ", length(out1_match), ".")
        stop(err, call. = F)
      }
      output <- list(logical_test = out1_match)
      if(isTRUE(request_info)){
        output[[paste0("mf.", depth, ".", j)]] <- out1_export
      }

      if(isFALSE(check_duplicates)){
        f2 <- a[[3]]
        lgk <- try(f2(x, y), silent = T)
        request_info <- !is.atomic(lgk)
        if(isTRUE(request_info)){
          out2_match <- lgk[[1]]
          out2_export <- lgk[-1]
        }else{
          out2_match <- lgk
        }
        if(inherits(out2_match, "try-error") | !inherits(out2_match, "logical")){
          if(inherits(out2_match, "try-error")){
            err <- attr(out2_match, "condition")$message
          }else{
            err <- "Output is not a `logical` object"
          }

          err <- paste0("Unable to evaluate `equal_funcs`:\n",
                        "i - Each function in `equal_funcs` must have the following syntax and output.\n",
                        "i - Syntax ~ `function(x, y, ...)`.\n",
                        "i - Output ~ `TRUE`, `FALSE`, `list(TRUE, ... )` or `list(FALSE, ... )`.\n",
                        "X - Issue with `equal_funcs`: ", err, ".")
          stop(err, call. = F)
        }
        out2_match[is.na(out2_match)] <- 0
        if(length(out2_match) == 1) out2_match <- rep(out2_match, curr_ds_len)
        if(!length(out2_match) %in% c(1,curr_ds_len)){
          err <- paste0("Output length of `equal_funcs` must be 1 or the same as `criteria`:\n",
                        "i - Unexpected length for `equal_funcs`:\n",
                        "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                        "X - Length is ", length(out2_match), ".")
          stop(err, call. = F)
        }
        output$logical_test <- c(out1_match, out2_match)
        if(isTRUE(request_info)){
          output[[paste0("ef.", depth, ".", j)]] <- out2_export
        }

      }
      return(output)
    }

  })
  matches_info <- lapply(matches, function(x){
    x[-1]
  })
  matches_info <- unlist(matches_info, recursive = FALSE)
  matches <- sapply(matches, function(x) x$logical_test)

  if(isFALSE(is.matrix(matches))){
    matches <- t(as.matrix(matches))
  }

  attr_n <- ncol(matches)
  if(attr_n >= 1){
    operator <- attr(x, "operator")
    if(tolower(operator) == "or"){
      matches <- rowSums(matches)
      if(isFALSE(check_duplicates)){
        m2 <- matches == attr_n
        indx <- seq((length(matches)/2) + 1, length(matches))
        matches[indx] <- m2[indx]
        rm(m2); rm(indx)
      }
      matches[matches > 0] <- 1
    }else if (tolower(operator) == "and"){
      matches <- rowSums(matches) == ncol(matches)
      matches <- as.numeric(matches)
    }
  }
  matches <- list(logical_test = matches)
  if(isFALSE(check_duplicates)){
    matches <- split(matches$logical_test,
                 cut(seq_len(length(matches$logical_test)), 2))
      names(matches) <- c("logical_test","equal_test")
  }
  matches <- c(matches, matches_info)
  rm(list = ls()[ls() != "matches"])
  return(matches)
}

