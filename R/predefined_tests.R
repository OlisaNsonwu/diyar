#' @name predefined_tests
#' @aliases predefined_tests
#' @title Predefined logical tests in \bold{\code{diyar}}
#' @description A collection of predefined logical tests used with \bold{\code{\link{sub_criteria}}} objects
#'
#' @param x Attribute(s) to be compared against.
#' @param y Attribute(s) to be compared by.
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
#' @param return_weights If \code{TRUE}, returns the match-weights and score-thresholds for record pairs.
#' @details
#' \bold{\code{prob_link()}} - Test that a record-pair relate to the same entity based on Fellegi and Sunter (1969) model for deciding if two records belong to the same entity.
#'
#' In summary, record-pairs are created and categorised as matches and non-matches (\code{attr_threshold}) with user-defined functions (\code{cmp_func}).
#' If \code{probabilistic} is \code{TRUE}, two probabilities (\code{m} and \code{u}) are used to calculate weights for matches and non-matches.
#' The \code{m}-probability is the probability that matched records are actually from the same entity i.e. a true match,
#' while \code{u}-probability is the probability that matched records are not from the same entity i.e. a false match.
#' Record-pairs whose total score are above a certain threshold (\code{score_threshold}) are assumed to belong to the same entity.
#'
#' Agreement (match) and disagreement (non-match) scores are calculated as described by Asher et al. (2020).
#'
#' For each record pair, an agreement for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}(m_{i}/u_{i})}{log_2 (m_i / u_i)}
#'
#' For each record pair, a disagreement score for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}((1-m_{i})/(1-u_{i}))}{log_2 ((1-m_i) / (1-u_i))}
#'
#' where \eqn{m_{i}}{m_i} and \eqn{u_{i}}{u_i} are the \code{m} and \code{u}-probabilities for each value of attribute \eqn{i}.
#'
#' Note that each probability is calculated as a combined probability for the record pair.
#' For example, if the values of the record-pair have \code{u}-probabilities of \code{0.1} and \code{0.2} respectively,
#' then the \code{u}-probability for the pair will be \code{0.02}.
#'
#' Missing data (\code{NA}) are considered non-matches and assigned a \code{u}-probability of \code{0}.
#'
#' @export
prob_link <- function(x, y,
                      cmp_func,
                      attr_threshold,
                      score_threshold,
                      probabilistic,
                      return_weights = FALSE){
  # Data validation
  err <- list(x = class(x), y = class(y))
  err <- lapply(err, function(x){
    if(all(x %in% c("list", "d_attribute"))){
      return(NA_character_)
    }else{
      listr(x)
    }
  })
  err <- unlist(err)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid object type for `x` or `y`:\n",
                  "i - `x` and `y` must be a `d_attribute` or `list` object.\n",
                  paste0("X - `", names(err),"` ", "is a `", err ,"` object.", collapse = "\n"))
    stop(err, call. = TRUE)
  }

  vars <- c("attribute", "sn")
  if(isTRUE(probabilistic)){
    vars <- c(vars, "m_probability", "u_probability")
  }

  err <- vars[!vars %in% names(x)]
  if(length(err) > 0){
    err <- paste0("i - If `probabilistic` is `FALSE`, `x` and `y` must have two elements named \"sn\" and \"attribute\".\n",
                  "i - If `probabilistic` is `TRUE`, `x` and `y` must have four element named \"sn\", \"attribute\", \"m_probability\" and \"u_probability\".\n",
                  paste0("X - \"", err,"\" ", "not found.", collapse = "\n"))
    stop(err, call. = TRUE)
  }

  x <- x[vars]

  attr_n <- length(x$attribute)
  attr_nm <- names(x$attribute)

  if(is.null(attr_nm)){
    attr_nm <-
      names(x$attribute) <-
      paste0("var_", seq_len(attr_n))
  }

  if(isTRUE(probabilistic)){
    names(x$u_probability) <-
      names(x$m_probability) <- attr_nm
  }

  err <- unlist(rc_dv(x, func = is.atomic), use.names = TRUE)
  err <- err[!err]
  if(length(err) > 0){
    err <- paste0("Each element of ", paste0("`.$", vars, "`", collapse = ","), " must be an atomic vector:\n",
                  paste0("X - ", "`", names(err) , "` is not an atomic vector", collapse = "\n"))
    stop(err, call. = TRUE)
  }

  thresh_repo <- prep_cmps_thresh(attr_nm = attr_nm,
                                  cmp_func = cmp_func,
                                  attr_threshold = attr_threshold,
                                  score_threshold = score_threshold)
  cmp_func <- thresh_repo$cmp_func
  attr_threshold <- thresh_repo$attr_threshold
  score_threshold <- thresh_repo$score_threshold

  # Weights
  wts <- lapply(seq_len(attr_n), function(i){
    curr_func <- cmp_func[[i]]
    wts <- curr_func(x$attribute[[i]],
                     y$attribute[[i]])
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

  out_a <- cbind(out_2, sum_wt, lgk)
  out_a <- cbind(
    as.matrix(x$sn[[1]]),
    as.matrix(y$sn[[1]]),
    out_a
  )
  colnames(out_a) <- c("sn_x", "sn_y", paste0("cmp.", attr_nm), "cmp.weight", "record.match")

  if(isFALSE(probabilistic)){
    if(isTRUE(return_weights)) return(out_a) else return(lgk)
  }
  # remove 'record.match'
  out_a <- out_a[,1:(ncol(out_a)-1)]
  # If probability based, matches are based on scores derived from m- and u-probabilities
  pwts <- sapply(seq_len(attr_n), function(i){
    # browser()
    pwts <- rep(0, length(wts[[i]]))
    # Agreement/disagreement based on string comparators
    curr_match <- (wts[[i]])
    curr_match <- (curr_match >= as.numeric(attr_threshold[i]@start) & curr_match <= as.numeric(right_point(attr_threshold[i])))
    curr_match <- curr_match & !is.na(curr_match)

    curr_uprob <- x$u_probability[[i]]
    curr_mprob <- x$m_probability[[i]]
    lgk <- which(x$attribute[[i]] != y$attribute[[i]] &
                   !is.na(x$attribute[[i]]) &
                   !is.na(y$attribute[[i]]))
    curr_uprob[lgk] <- curr_uprob[lgk] * (y$u_probability[[i]])[lgk]
    curr_mprob[lgk] <- curr_mprob[lgk] * (y$m_probability[[i]])[lgk]

    # agreements
    pwts_a <- log2(curr_mprob/curr_uprob)
    # disagreements
    pwts_b <- log2((1 - curr_mprob)/(1 - curr_uprob))

    if(length(pwts_a) == 1){
      pwts_a <- rep(pwts_a, length(curr_match))
    }
    if(length(pwts_b) == 1){
      pwts_b <- rep(pwts_b, length(curr_match))
    }
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
    out_b <- cbind(pwts, sum_wt, as.logical(lgk))
    colnames(out_b) <- c(paste0("prb.", attr_nm),
                         "prb.weight",
                         "record.match")
    return(cbind(out_a, out_b))
  }else{
    return(lgk)
  }
}

#' @rdname predefined_tests
#' @export
true <- function(x, y) TRUE

#' @rdname predefined_tests
#' @export
false <- function(x, y) FALSE
