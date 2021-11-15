#' @name predefined_tests
#' @aliases predefined_tests
#' @title Predefined logical tests in \bold{\code{diyar}}
#' @description A collection of predefined logical tests used with \bold{\code{\link{sub_criteria}}} objects.
#'
#' @param x Value of an attribute(s) to be compare against.
#' @param y Value of an attribute(s) to be compare by.
#'
#' @details
#' \bold{\code{exact_match()}} - test that  \code{x == y}
#'
#' @examples
#' `exact_match`
#' exact_match(x = 1, y = 1)
#' exact_match(x = 1, y = 2)
#'
#' @export
exact_match <- function(x, y) {
  x == y & !is.na(x) & !is.na(y)
}

#' @rdname predefined_tests
#' @param range Difference between \code{y} and \code{x}.
#' @details
#' \bold{\code{range_match()}} - test that \code{x} \eqn{\le} \code{y} \eqn{\le} \code{(x + range)}
#' @examples
#' `range_match`
#' range_match(x = 10, y = 16, range = 6)
#' range_match(x = 16, y = 10, range = 6)
#'
#' @export
range_match <- function(x, y, range = 10){
  x <- as.numeric(x); y <- as.numeric(y)
  (x <= y) & (y <= x + range) & !is.na(x) & !is.na(y)
}

#' @rdname predefined_tests
#' @details
#' \bold{\code{range_match_legacy()}} - test that \code{overlap(as.number_line(x@gid), y)} is \code{TRUE}.
#' @examples
#' `range_match_legacy`
#' x_nl <- number_line(10, 16, gid = 10)
#' y_nl1 <- number_line(16, 10)
#' y_nl2 <- number_line(16, 10)
#'
#' range_match_legacy(x = x_nl, y = y_nl1)
#' range_match_legacy(x = x_nl, y = y_nl2)
#'
#' @export
range_match_legacy <- function(x, y) {
  lg <- overlap(as.number_line(x@gid), x)
  lg[is.na(lg)] <- F
  if(any(!lg)) {
    rng_i <- paste(head(which(!lg), 5), collapse = ", ", sep="")
    rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]
    stop(paste0("Range matching error: Actual value (gid) is out of range in ", "[", rng_i, "]"))
  }
  overlap(as.number_line(x@gid), y)
}

#' @rdname predefined_tests
#' @param cmp_func Logical tests such as string comparators. See \code{\link{links_wf_probabilistic}}.
#' @param attr_threshold Matching set of weight thresholds for each result of \code{cmp_func}. See \code{\link{links_wf_probabilistic}}.
#' @param probabilistic If \code{TRUE}, matches determined through a score derived base on Fellegi-Sunter model for probabilistic linkage. See \code{\link{links_wf_probabilistic}}.
#' @param score_threshold Score threshold determining matched or linked records. See \code{\link{links_wf_probabilistic}}.
#' @param return_weights If \code{TRUE}, returns the match-weights and score-thresholds for record pairs. See \code{\link{links_wf_probabilistic}}.
#' @details
#' \bold{\code{prob_link()}} - Test that a record-sets \code{x} and \code{y} are from the same entity based on calculated weights and probability scores.
#' @examples
#'
#' @export
prob_link <- function(x, y,
                      cmp_func,
                      attr_threshold,
                      score_threshold,
                      probabilistic,
                      return_weights){
  # Number of attributes
  attr_n <- length(x)/3
  attr_nm <- names(x)[seq_len(attr_n)]

  # Weights from a string comparator
  wts <- lapply(seq_len(attr_n), function(i){
    curr_func <- cmp_func[[i]]
    wts <- curr_func(x[[i]], y[[i]])
    wts[is.na(wts)] <- 0
    wts
  })

  out_2 <- sapply(wts, identity)

  if(is.null(nrow(out_2))){
    sum_wt <- sum(out_2)
  }else{
    sum_wt <- rowSums(out_2)
  }

  # If weight based, matches are assigned based on the results of the comparators
  if(isFALSE(probabilistic)){
    lgk <- (sum_wt >= as.numeric(score_threshold@start) & sum_wt <= as.numeric(right_point(score_threshold)))
  }else{
    lgk <- rep(NA_real_, length(sum_wt))
  }

  if(is.null(ncol(out_2))){
    out_2 <- t(out_2)
  }

  out_a <- cbind(as.data.frame(out_2), sum_wt, lgk)
  colnames(out_a) <- c(paste0("cmp.", attr_nm), "cmp.weight", "record.match")

  if(isFALSE(probabilistic)){
    if(isTRUE(return_weights)) return(out_a) else return(lgk)
  }
  out_a$record.match <- NULL
  # If probability based, matches are based on scores derived from m- and u-probabilities
  pwts <- sapply(seq_len(attr_n), function(i){
    pwts <- rep(0, length(wts[[i]]))
    # Agreement/disagreement based on string comparators
    curr_match <- (wts[[i]])
    curr_match <- (curr_match >= as.numeric(attr_threshold[i]@start) & curr_match <= as.numeric(right_point(attr_threshold[i])))
    curr_match <- curr_match & !is.na(curr_match)

    curr_uprob <- (x[[i + (attr_n * 2)]])
    curr_mprob <- (x[[i + attr_n]])

    lgk <- which(x[[i]] != y[[i]] & !is.na(x[[i]]) & !is.na(y[[i]]))
    curr_uprob[lgk] <- curr_uprob[lgk] * (y[[i + (attr_n * 2)]])[lgk]
    curr_mprob[lgk] <- curr_mprob[lgk] * (y[[i + attr_n]])[lgk]

    # agreements
    pwts_a <- log2(curr_mprob/curr_uprob)
    # disagreements
    pwts_b <- log2((1 - curr_mprob)/(1 - curr_uprob))

    pwts <- pwts_a
    pwts[!curr_match] <- pwts_b[!curr_match]
    pwts
  })

  if(is.null(nrow(pwts))){
    sum_wt <- sum(pwts)
  }else{
    sum_wt <- rowSums(pwts)
  }
  lgk <- (sum_wt >= as.numeric(score_threshold@start) & sum_wt <= as.numeric(right_point(score_threshold)))
  if(isTRUE(return_weights)){
    if(is.null(ncol(pwts))){
      pwts <- t(pwts)
    }
    out_b <- cbind(as.data.frame(pwts), sum_wt, as.logical(lgk))
    colnames(out_b) <- c(paste0("prb.", attr_nm),
                         "prb.weight",
                         "record.match")
    return(cbind(out_a, out_b))
  }else{
    return(lgk)
  }
}
