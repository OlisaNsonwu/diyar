#' @name sub_criteria
#' @aliases sub_criteria
#' @title Nested linkage criteria
#'
#' @description Nested matching criteria for each iteration of \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}.
#' @param ... \code{[atomic]} Attributes passed to or \code{eval_sub_criteria()} or \code{eval_sub_criteria()}
#'
#' Arguments passed to methods for \code{eval_sub_criteria()}
#' @param .obj \code{[data.frame|list]}. Attributes.
#' @param match_funcs \code{[function]}. User defined logical test for matches.
#' @param equal_funcs \code{[function]}. User defined logical test for identical record sets (all attributes of the same record).
#' @param operator \code{[character]}. Options are \code{"and"} or \code{"or"}.

#' @param x_pos \code{[integer]}. Index of one half of a record pair.
#' @param y_pos \code{[integer]}. Index of one half of a record pair.
#' @param check_duplicates \code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.
#' @seealso
#' \code{\link{predefined_tests}}; \code{\link{links}}; \code{\link{episodes}}; \code{\link{eval_sub_criteria}}
#'
#' @return \code{\link{sub_criteria}}
#'
#' @details
#' \bold{\code{sub_criteria()}} - Creates a \code{sub_criteria} object - a nested criteria evaluated at each iteration of \bold{\code{links}} or \bold{\code{episodes}}.
#' A \code{sub_criteria} object contains attributes to be compared,
#' logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
#' another set of logical tests to determine identical records.
#'
#' \bold{\code{attrs()}} - Create as \code{d_attribute} object - a collection of objects that can be passed as a single attribute to \bold{\code{sub_criteria()}}.
#'
#' \bold{\code{eval_sub_criteria()}} - Evaluates a \code{sub_criteria} object.
#'
#' At each iteration of \code{\link{links}} or \code{\link{episodes}}, record-pairs are created from each attribute of the \code{sub_criteria} object.
#' \code{eval_sub_criteria()} evaluates each record-pair using the \code{match_funcs} and \code{equal_funcs} functions of the \code{sub_criteria} object.
#' See \bold{\code{\link{predefined_tests}}} for examples of \code{match_funcs} and \code{equal_funcs}.
#' \code{attrs()} is useful when the criteria requires an interaction between the multiple attributes. For example, attribute 1 + attribute 2 > attribute 3.
#'
#' Every attribute, including those in \code{attrs()}, must have the same length or a length of 1.
#'
#' @examples
#' # Attributes
#' attr_1 <- c(30, 28, 40, 25, 25, 29, 27)
#' attr_2 <- c("M", "F", "U", "M", "F", "U", "M")
#'
#' # Creating a nested criteria
#' ## Example 1 - A maximum difference of 10 in attribute 1
#' s_cri1 <- sub_criteria(attr_1, match_funcs = range_match)
#' s_cri1
#'
#' # Evaluating the nested criteria
#' ## .. at the 1st iteration of linkage
#' eval_sub_criteria(s_cri1)
#' ## .. at the 2nd iteration of linkage
#' x_pos_val <- seq_len(max(attr_eval(s_cri1)))
#' eval_sub_criteria(s_cri1,
#'                   x_pos = x_pos_val,
#'                   y_pos = rep(2, length(x_pos_val)))
#'
#' ## Example 2 - `s_cri1` AND an exact match on attribue 2
#' s_cri2 <- sub_criteria(
#'   s_cri1,
#'   sub_criteria(attr_2, match_funcs = exact_match),
#'   operator = "and")
#' s_cri2
#'
#' ## Example 3 - `s_cri1` OR an exact match on attribue 2
#' s_cri3 <- sub_criteria(
#'   s_cri1,
#'   sub_criteria(attr_2, match_funcs = exact_match),
#'   operator = "or")
#' s_cri3
#'
#' # Evaluating the nested criteria
#' eval_sub_criteria(s_cri2)
#' eval_sub_criteria(s_cri3)
#'
#' # Alternatively, using `attr()`
#' AND_func <- function(x, y) range_match(x$a1, y$a1) & x$a2 == y$a2
#' OR_func <- function(x, y) range_match(x$a1, y$a1) | x$a2 == y$a2
#'
#' ## Creating the nested criteria
#' s_cri2b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'                         match_funcs = AND_func)
#' s_cri3b <- sub_criteria(attrs(.obj = list(a1 = attr_1, a2 = attr_2)),
#'                         match_funcs = OR_func)
#'
#' # Evaluate the nested criteria
#' eval_sub_criteria(s_cri2b)
#' eval_sub_criteria(s_cri3b)
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
print.sub_criteria <- function(object, ...){
  cat(fmt_sub_criteria(object), "\n")
}

#' @rdname sub_criteria
#' @export
eval_sub_criteria.sub_criteria <- function(x,
                                           x_pos = seq_len(max(attr_eval(x))),
                                           y_pos = rep(1L, length(x_pos)),
                                           check_duplicates = TRUE,
                                           nest = TRUE,
                                           ...){
  curr_ds_len <- length(y_pos)
  matches <- lapply(1:length(x), function(j){
    a <- x[[j]]
    x <- a[[1]]
    f1 <- a[[2]]
    if(all(class(x) == "sub_criteria")){
      x <- eval_sub_criteria(x = x, x_pos = x_pos, y_pos = y_pos,
                             check_duplicates = check_duplicates)
      if(isTRUE(nest)){
        if(isTRUE(check_duplicates)){
          return(as.logical(x$logical_test + x$equal_test))
        }else{
          return(x$logical_test)
        }
      }else{
        return(x)
      }
    }

    if(all(class(x) == "d_attribute")){
      x <- lapply(x, function(x) if(length(x) == 1) rep(x, curr_ds_len) else x)
      y <- lapply(x, function(x) x[y_pos])
      x <- lapply(x, function(x) x[x_pos])
    }else{
      if(length(x) == 1) x <- rep(x, curr_ds_len)
      y <- x[y_pos]
      x <- x[x_pos]
    }

    lgk <- try(f1(x, y), silent = TRUE)
    if(class(lgk) == "try-error" | (isTRUE(nest) & all(class(lgk) != "logical"))){
      if(class(lgk) == "try-error"){
        err <- attr(lgk, "condition")$message
      }else{
        err <- "Output is not a `logical` object"
      }

      err <- paste0("Unable to evaluate `match_funcs`:\n",
                    "i - Each function in `match_funcs` must have the following syntax and output.\n",
                    "i - Syntax ~ `function(x, y, ...)`.\n",
                    "i - Output ~ `TRUE` or `FALSE` if `nest` is `TRUE`.\n",
                    "X - Issue with `match_funcs`: ", err, ".")
      stop(err, call. = F)
    }

    out1 <- lgk
    out1[is.na(lgk)] <- 0
    if(!length(out1) %in% c(1, curr_ds_len)){
      err <- paste0("Output length of `match_funcs` must be 1 or the same as `criteria`:\n",
                    "i - Unexpected length for `match_funcs`:\n",
                    "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                    "X - Length is ", length(out1), ".")
      stop(err, call. = F)
    }

    if(isFALSE(check_duplicates)){
      f2 <- a[[3]]
      lgk <- try(f2(x, y), silent = T)
      if(class(lgk) == "try-error" | (isTRUE(nest) & all(class(lgk) != "logical"))){
        if(class(lgk) == "try-error"){
          err <- attr(lgk, "condition")$message
        }else{
          err <- "Output is not a `logical` object"
        }

        err <- paste0("Unable to evaluate `equal_funcs`:\n",
                      "i - Each function in `equal_funcs` must have the following syntax and output.\n",
                      "i - Syntax ~ `function(x, y, ...)`.\n",
                      "i - Output ~ `TRUE` or `FALSE` if `nest` is `TRUE`.\n",
                      "X - Issue with `equal_funcs`: ", err, ".")
        stop(err, call. = F)
      }
      lgk <- as.numeric(lgk)
      out2 <- lgk
      out2[is.na(lgk)] <- 0
      if(length(out2) == 1) out2 <- rep(out2, curr_ds_len)
      if(!length(out2) %in% c(1,curr_ds_len)){
        err <- paste0("Output length of `equal_funcs` must be 1 or the same as `criteria`:\n",
                      "i - Unexpected length for `equal_funcs`:\n",
                      "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                      "X - Length is ", length(out2), ".")
        stop(err, call. = F)
      }
      out1 <- c(out1, out2)
    }
    return(out1)
  })

  if(isFALSE(nest)){
    matches <- unpack(matches)
    if(isFALSE(check_duplicates)){
      matches <- lapply(matches, function(x){
        x <- split(x, cut(seq_len(length(x)), 2))
        names(x) <- c("-logical_test","-equal_test")
        x
      })

      matches <- unlist(matches, recursive = FALSE, use.names = TRUE)
      str <- gsub("^.*-", "", names(matches))
      names(matches) <- gsub(".-.*$", "", names(matches))
      matches <- split(matches, factor(str, levels = c("logical_test", "equal_test")))
      return(matches)
    }else{
      return(matches)
    }
  }

  matches <- sapply(matches, identity)
  if(isFALSE(is.matrix(matches))){
    matches <- t(as.matrix(matches))
  }

  operator <- attr(x, "operator")
  if(tolower(operator) == "or"){
    set_match <- rowSums(matches)
    if(isFALSE(check_duplicates)){
      m2 <- set_match == ncol(matches)
      lgk <- which(seq_len(nrow(matches)) >= nrow(matches)/2)
      set_match[lgk] <- m2[lgk]
      rm(m2); rm(lgk)
    }
    set_match[set_match > 0] <- 1
  }else if (tolower(operator) == "and"){
    set_match <- rowSums(matches) == ncol(matches)
    set_match <- as.numeric(set_match)
  }

  if(isFALSE(check_duplicates)){
    x <- split(set_match, cut(seq_len(length(set_match)), 2))
    names(x) <- c("logical_test","equal_test")
  }else{
    x <- list(logical_test = set_match)
  }
  rm(list = ls()[ls() != "x"])
  return(x)
}

