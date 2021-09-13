#' @name eval_sub_criteria
#' @title Evaluate a \code{\link{sub_criteria}}.
#' @param x \code{[\link{sub_criteria}]}.
#' @param ... Arguments passed to methods.
#' @return \code{logical}; \code{list}
#'
#' @seealso \code{\link{sub_criteria}}; \code{\link{reframe}}
#' @export
eval_sub_criteria <- function(x, ...) UseMethod("eval_sub_criteria")

#' @rdname eval_sub_criteria
#' @param strata \code{[integer]}. Subsets of the dataset
#' @param index_record \code{[logical]}. Index record (\code{y}) being compared against other records (\code{x}).
#' See (\bold{\code{\link{predefined_tests}}}).
#' @param sn \code{[integer]} Unique index for each record.
#' @param check_duplicates \code{[logical]}. If \code{FALSE}, does not check duplicate values. The result of the initial check will be recycled.
#'
#' @examples
#' Consider three records
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
  rc_sc_ord <- order(sc_ord)

  curr_sn <- sn[sc_ord]
  curr_strata <- curr_strata[sc_ord]

  rrr <- rle(curr_strata)
  lgk <- which(!duplicated(curr_strata, fromLast = TRUE))

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
    # browser()

    if(class(x) == "d_attribute"){
      ds_len <- length(x[[1]])
      x <- lapply(x, function(x){
        if(length(x) == 1){
          x <- rep(x, curr_ds_len)
        }else{
          # x <- x[match(curr_sn, seq_len(ds_len))]
          x <- x[curr_sn]
        }
        x
      })

      y <- lapply(x, function(x){
        # y <- rep(x[lgk], rrr$lengths[match(curr_strata[lgk], rrr$values)])
        y <- rep(x[lgk], rrr$lengths)
        y
      })
    }else{
      ds_len <- length(x)
      if(length(x) == 1){
        x <- rep(x, curr_ds_len)
      }else{
        # browser()
        # x <- x[match(curr_sn, seq_len(ds_len))]
        x <- x[curr_sn]
      }
      # y <- rep(x[lgk], rrr$lengths[match(curr_strata[lgk], rrr$values)])
      y <- rep(x[lgk], rrr$lengths)
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
    if(!length(out1) %in% c(1, curr_ds_len)){
      err <- paste0("Output length of `match_funcs` must be 1 or the same as `criteria`:\n",
                    "i - Unexpected length for `match_funcs`:\n",
                    "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                    "X - Length is ", length(out1), ".")
      stop(err, call. = F)
    }
    out1 <- out1[rc_sc_ord]

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
      if(length(out2) == 1) out2 <- rep(out2, curr_ds_len)
      if(!length(out2) %in% c(1,curr_ds_len)){
        err <- paste0("Output length of `equal_funcs` must be 1 or the same as `criteria`:\n",
                      "i - Unexpected length for `equal_funcs`:\n",
                      "i - Expecting a length of 1 of ", curr_ds_len, ".\n",
                      "X - Length is ", length(out2), ".")
        stop(err, call. = F)
      }
      # out1 <- c(out1[match(sn, curr_sn)], out2[match(sn, curr_sn)])
      out1 <- c(out1, out2[rc_sc_ord])
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
    x <- list(logical_test = set_match,
              equal_test = set_match.rf)
  }else{
    x <- list(logical_test = set_match)
  }
  rm(list = ls()[ls() != "x"])
  return(x)
}
