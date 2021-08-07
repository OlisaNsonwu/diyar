#' @name sub_criteria
#' @aliases sub_criteria
#' @title Sub-criteria
#'
#' @description Matching criteria for each iteration of \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}.
#' @param ... \code{[atomic]}.. Attributes.
#' @param match_funcs \code{[function]}. User defined logical test for matches.
#' @param equal_funcs \code{[function]}. User defined logical test for identical record sets (all attributes of the same record).
#' @param operator \code{[character]}. Options are \code{"and"} or \code{"or"}.
#' @seealso
#' \code{\link{predefined_tests}}, \code{\link{links}} and \code{\link{episodes}}
#'
#' @return \code{sub_criteria}
#'
#' @details
#' \bold{\code{sub_criteria()}} is the mechanism for providing matching criteria to an iteration of \bold{\code{links}} or \bold{\code{episodes}}.
#' It creates a \code{sub_criteria} class object which contains the attributes to be compared,
#' logical tests for the comparisons (see \bold{\code{\link{predefined_tests}}} for examples) and
#' another set of logical tests to determine identical records.
#'
#' \emph{*Determining identical records reduces processing time.}
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
#' @param x \code{[sub_criteria]}
#' @return \code{Logical}
#' @export
eval_sub_criteria <- function(x, ...) UseMethod("eval_sub_criteria")

#' @rdname sub_criteria
#' @param strata \code{[integer]}. Subsets of the dataset
#' @param index_record \code{[logical]}. Represents the \code{y}-value of the \code{x}-\code{y} record pair to be compared.
#' See (\bold{\code{\link{predefined_tests}}}).
#' @param sn \code{[integer]} Unique index for each record.
#' @param check_duplicates \code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.
#'
#' @examples
#' `eval_sub_criteria`
#' # 3 values
#' strata <- rep(1, 3)
#' index_record <- c(TRUE, FALSE, FALSE)
#' sn <- 1:3
#'
#' # Test for a match in either attribute
#' sub_cri_1 <- sub_criteria(c(1, 1, 0), c(2, 1, 2))
#' eval_sub_criteria(sub_cri_1, strata, index_record, sn)
#'
#' # Test for a match in both attributes
#' sub_cri_2 <- sub_criteria(c(1, 1, 0), c(2, 1, 2), operator = "and")
#' eval_sub_criteria(sub_cri_2, strata, index_record, sn)
#' @export
eval_sub_criteria.sub_criteria <- function(x,
                                           strata = seq_len(max(attr_eval(x))),
                                           index_record = c(TRUE, rep(FALSE, length(strata) - 1)),
                                           sn = seq_len(length(strata)),
                                           check_duplicates = TRUE, ...){
  curr_ds_len <- length(strata)
  curr_strata <- strata
  sc_ord <- order(curr_strata, -index_record, decreasing = TRUE)

  curr_sn <- sn[sc_ord]
  curr_strata <- curr_strata[sc_ord]

  rrr <- rle(curr_strata)
  lgk <- !duplicated(curr_strata, fromLast = TRUE)

  matches <- sapply(1:length(x), function(j){
    a <- x[[j]]
    x <- a[[1]]
    if(class(x) == "sub_criteria"){
      return(
        unlist(eval_sub_criteria(x = x,
                                 strata = strata,
                                 index_record = index_record,
                                 sn = sn,
                                 check_duplicates = check_duplicates),
               use.names = FALSE)
      )
    }

    if(class(x) == "list"){
      ds_len <- length(x[[1]])
      x <- lapply(x, function(x){
        if(length(x) == 1){
          x <- rep(x, curr_ds_len)
        }else{
          x <- x[match(curr_sn, seq_len(ds_len))]
        }
        x
      })

      y <- lapply(x, function(x){
        y <- rep(x[lgk], rrr$lengths[match(curr_strata[lgk], rrr$values)])
        y
      })
    }else{
      ds_len <- length(x)
      if(length(x) == 1){
        x <- rep(x, curr_ds_len)
      }else{
        x <- x[match(curr_sn, seq_len(ds_len))]
      }
      y <- rep(x[lgk], rrr$lengths[match(curr_strata[lgk], rrr$values)])
    }

    f1 <- a[[2]]
    lgk <- try(f1(x, y), silent = TRUE)
    if(class(lgk) == "try-error" | class(lgk) != "logical"){
      if(class(lgk) == "try-error"){
        err <- attr(lgk, "condition")$message
      }else{
        err <- "Output is not a `logical` object"
      }

      err <- paste0("Unable to evaluate `match_funcs`:\n",
                    "i - Each function in `match_funcs` must have the following syntax and output.\n",
                    "i - Syntax ~ `function(x, y, ...)`.\n",
                    "i - Output ~ `TRUE` or `FALSE`.\n",
                    "X - Issue with `match_funcs`: ", err, ".")
      stop(err, call. = F)
    }

    out1 <- lgk
    out1[is.na(lgk)] <- 0
    if(!length(out1) %in% c(1, length(curr_strata))){
      err <- paste0("Output length of `match_funcs` must be 1 or the same as `criteria`:\n",
                    "i - Unexpected length for `match_funcs`:\n",
                    "i - Expecting a length of 1 of ", length(curr_strata), ".\n",
                    "X - Length is ", length(out1), ".")
      stop(err, call. = F)
    }

    if(isFALSE(check_duplicates)){
      f2 <- a[[3]]
      lgk <- try(f2(x, y), silent = T)
      if(class(lgk) == "try-error" | class(lgk) != "logical"){
        if(class(lgk) == "try-error"){
          err <- attr(lgk, "condition")$message
        }else{
          err <- "Output is not a `logical` object"
        }

        err <- paste0("Unable to evaluate `equal_funcs`:\n",
                      "i - Each function in `equal_funcs` must have the following syntax and output.\n",
                      "i - Syntax ~ `function(x, y, ...)`.\n",
                      "i - Output ~ `TRUE` or `FALSE`.\n",
                      "X - Issue with `equal_funcs`: ", err, ".")
        stop(err, call. = F)
      }
      lgk <- as.numeric(lgk)
      out2 <- lgk
      out2[is.na(lgk)] <- 0
      if(length(out2) == 1) out2 <- rep(out2, length(curr_strata))
      if(!length(out2) %in% c(1,length(curr_strata))){
        err <- paste0("Output length of `equal_funcs` must be 1 or the same as `criteria`:\n",
                      "i - Unexpected length for `equal_funcs`:\n",
                      "i - Expecting a length of 1 of ", length(curr_strata), ".\n",
                      "X - Length is ", length(out2), ".")
        stop(err, call. = F)
      }
      out1 <- c(out1[match(sn, curr_sn)], out2[match(sn, curr_sn)])
    }
    return(out1)
  })
  if(length(sn) == 1 | length(matches) == 1){
    matches <- t(as.matrix(matches))
  }
  operator <- attr(x, "operator")
  if(operator == "or"){
    if(isFALSE(check_duplicates)){
      set_match <- rowSums(matches)
      m2 <- rowSums(matches) == ncol(matches) | index_record
      lgk <- which(seq_len(nrow(matches)) >= nrow(matches)/2)
      set_match[lgk] <- m2[lgk]
      rm(m2); rm(lgk)
    }else{
      set_match <- rowSums(matches)
    }
    set_match[set_match > 0] <- 1
  }else if (operator == "and"){
    set_match <- rowSums(matches) == ncol(matches) | index_record
    set_match <- as.numeric(set_match)
  }

  if(isFALSE(check_duplicates)){
    set_match.rf <- set_match[((length(set_match)/2)+1):length(set_match)]
    set_match <- set_match[1:(length(set_match)/2)]
    return(list(logical_test = set_match,
                equal_test = set_match.rf))
  }else{
    return(list(logical_test = set_match))
  }

}
