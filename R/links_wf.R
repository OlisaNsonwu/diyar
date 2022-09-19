#' @name link_wf
#' @title Record linkage
#'
#' @description Deterministic and probabilistic record linkage
#' Assign unique identifiers to records based on partial, nested or calculated probabilities.
#'
#' @param attribute \code{[atomic|list|data.frame|matrix|d_attribute]}. Attributes to compare.
#' @param cmp_func \code{[list|function]}. String comparators for each \code{attribute}. See \code{Details}.
#' @param attr_threshold \code{[list|numeric|\link{number_line}]}. Weight-thresholds for each \code{cmp_func}. See \code{Details}.
#' @param probabilistic \code{[logical]}. If \code{TRUE}, scores are assigned base on Fellegi-Sunter model for probabilistic record linkage. See \code{Details}.
#' @param m_probability \code{[list|numeric]}. The probability that a matching records are the same entity.
#' @param u_probability \code{[list|numeric]}. The probability that a matching records are not the same entity.
#' @param score_threshold \code{[numeric|\link{number_line}]}. Score-threshold for linked records. See \code{Details}.
#' @param id_1 \code{[list|numeric]}. Record id or index of one half of a record-pair.
#' @param id_2 \code{[list|numeric]}. Record id or index of one half of a record-pair.
#' @param ... Arguments passed to \bold{\code{links}}
#' @param blocking_attribute \code{[atomic]}. Passed to \bold{\code{criteria}} in \bold{\code{\link{links}}}.
#' @param repeats_allowed \code{[logical]} Passed to \bold{\code{repeats_allowed}} in \bold{\code{\link{links}}}.
#' @param permutations_allowed \code{[logical]} Passed to \bold{\code{permutations_allowed}} in \bold{\code{\link{links}}}.
#' @param ignore_same_source \code{[logical]} Passed to \bold{\code{ignore_same_source}} in \bold{\code{\link{links}}}.
#' @param data_source \code{[character]}. Passed to \bold{\code{data_source}} in \bold{\code{\link{links}}}.
#' @param display \code{[character]}. Passed to \bold{\code{display}} in \bold{\code{\link{links}}}.
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{links}}
#'
#' @details
#' \bold{\code{links_wf_probabilistic()}} - A wrapper function of \code{\link{links}} with a
#' with a specific \code{\link{sub_criteria}} and to achieve to achieve probabilistic record linkage
#' It excludes functionalities for the nested and mutli-stage linkage.
#'  \code{links_wf_probabilistic()} requires a \code{score_threshold} in advance.
#' To help with this, \code{prob_score_range()} can be used to return the range of scores attainable for a given set of \code{attribute}, \code{m} and \code{u}-probabilities.
#' Additionally, \code{id_1} and \code{id_2} can be used to link specific records pairs, aiding the review of potential scores.
#'
#'
#' \bold{\code{links_sv_probabilistic()}} - A simpler version of \code{\link{links}}.
#' It excludes functionalities for the batched, nested and mutli-stage linkage.
#' \code{links_sv_probabilistic()} requires a \code{score_threshold} in advance,
#' however, since it exports the match weights, the \code{score_threshold}
#' can be changed after the analysis.
#'
#' @references
#' Fellegi, I. P., & Sunter, A. B. (1969). A Theory for Record Linkage. \emph{Journal of the Statistical Association}, 64(328), 1183 - 1210. https://doi.org/10.1080/01621459.1969.10501049
#'
#' Asher, J., Resnick, D., Brite, J., Brackbill, R., & Cone, J. (2020). An Introduction to Probabilistic Record Linkage with a Focus on Linkage Processing for WTC Registries. \emph{International journal of environmental research and public health}, 17(18), 6937. https://doi.org/10.3390/ijerph17186937.
#' See \code{vignette("links")} for more information.

#' @aliases links_wf
#'
#' @examples
#' data(patient_records)
#' # Weighted (probabilistic) comparison of forename, middlename and surname
#' criteria_1 <- as.list(patient_records[c("forename", "middlename", "surname")])
#'
#' # Possible scores when m-probability is 0.95
#' prob_scores <- prob_score_range(attribute = criteria_1,
#'                                 m_probability = 0.95,
#'                                 u_probability = NULL)
#' \dontrun{
#' # Probablistic record linkage with 'links_sv_probabilistic()'
#' pids_1a <- links_sv_probabilistic(attribute = criteria_1,
#'                         cmp_func = exact_match,
#'                         attr_threshold = 1,
#'                         probabilistic = TRUE,
#'                         m_probability = 0.95,
#'                         score_threshold = prob_scores$mid_scorce,
#'                         display = "stats")
#'
#' # Equivalent with 'links_wf_probabilistic()'
#' pids_1b <- links_wf_probabilistic(attribute = criteria_1,
#'                                   cmp_func = exact_match,
#'                                   attr_threshold = 1,
#'                                   probabilistic = TRUE,
#'                                   m_probability = 0.95,
#'                                   score_threshold = prob_scores$mid_scorce,
#'                                   display = "progress",
#'                                   recursive = TRUE,
#'                                   check_duplicates = TRUE)
#'
#' # Less thorough but faster equivalent with `links_wf_probabilistic()`
#' pids_1c <- links_wf_probabilistic(attribute = criteria_1,
#'                                   cmp_func = exact_match,
#'                                   attr_threshold = 1,
#'                                   probabilistic = TRUE,
#'                                   m_probability = 0.95,
#'                                   score_threshold = prob_scores$mid_scorce,
#'                                   display = "progress",
#'                                   recursive = FALSE,
#'                                   check_duplicates = FALSE)
#'
#' # Each implementation can lead to different results
#' summary(pids_1a$pid)
#' summary(pids_1b$pid)
#' summary(pids_1c$pid)
#' }
#'
#' # Weighted (non-probabilistic) comparison of forename, middlename and age difference
#' criteria_2 <- as.list(patient_records[c("forename", "middlename", "dateofbirth")])
#' age_diff <- function(x, y){
#'   diff <- abs(as.numeric(x) - as.numeric(y))
#'   wgt <-  diff %in% 0:(365 * 10) & !is.na(diff)
#'   wgt
#' }
#'
#' pids_2a <- links_sv_probabilistic(attribute = criteria_2,
#'                         blocking_attribute = patient_records$surname,
#'                         cmp_func = c(exact_match, exact_match, age_diff),
#'                         score_threshold = number_line(3, 5),
#'                         probabilistic = FALSE,
#'                         display = "stats")
#'
#' # Larger weights can be assigned to particular attributes through `cmp_func`
#' # For example, a smaller age difference can contribute a higher score (e.g 0 to 3)
#' age_diff_2 <- function(x, y){
#'   diff <- as.numeric(abs(x - y))
#'   wgt <-  diff %in% 0:(365 * 10) & !is.na(diff)
#'   wgt[wgt] <- match(as.numeric(cut(diff[wgt], 3)), 3:1)
#'   wgt
#' }
#' pids_2b <- links_sv_probabilistic(attribute = criteria_2,
#'                         blocking_attribute = patient_records$surname,
#'                         cmp_func = c(exact_match, exact_match, age_diff_2),
#'                         score_threshold = number_line(3, 5),
#'                         probabilistic = FALSE,
#'                         display = "stats")
#'
#' head(pids_2a$pid_weights, 10)
#' head(pids_2b$pid_weights, 10)
#'
#' @rdname links_wf
#' @export
links_sv_probabilistic <- function(attribute,
                                   blocking_attribute = NULL,
                                   cmp_func = diyar::exact_match,
                                   attr_threshold = 1,
                                   probabilistic = TRUE,
                                   m_probability = .95,
                                   u_probability = NULL,
                                   score_threshold = 1,
                                   repeats_allowed = FALSE,
                                   permutations_allowed = FALSE,
                                   data_source = NULL,
                                   ignore_same_source = TRUE,
                                   display = "none"){
  tm_a <- Sys.time()
  mem_ia <- sum(gc()[, 1])
  err <- err_links_wf_probablistic_0(attribute = attribute,
                                     blocking_attribute = blocking_attribute,
                                     cmp_func = cmp_func,
                                     attr_threshold = attr_threshold,
                                     probabilistic = probabilistic,
                                     m_probability = m_probability,
                                     score_threshold = score_threshold,
                                     id_1 = NULL, id_2 = NULL,
                                     u_probability = u_probability)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(!display %in% c("none")){
    rp_data <- di_report(duration = tm_a, iteration = "Data validation",
                         current_tot = length(attrs(attribute)[[1]]),
                         memory_used = mem_ia)
    tm_ia <- Sys.time()
    report <- list(rp_data)
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }

  if(class(attribute) %in% c("list", "data.frame")){
    attribute <- attrs(.obj = attribute)
  }else if(class(attribute) %in% c("matrix")){
    attribute <- attrs(.obj = as.data.frame(attribute))
  }else if(class(attribute) %in% c("d_attribute")){
  }else{
    attribute <- attrs(attribute)
  }

  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }

  # Attribute names
  attr_nm <- names(attribute)
  rd_n <- length(attribute[[1]])

  lgk <- unlist(lapply(attribute, function(x){
    if(is.number_line(x)){
      length(unique(x)) == 1
    }else{
      length(x[!duplicated(x)]) == 1
    }
  }), use.names = FALSE)
  if(any(lgk)){
    warning(paste0("Attributes with identicial values in every record are ignored:\n",
                   paste0("i - `", attr_nm[lgk], "` was ignored!", collapse = "\n")), call. = FALSE)
  }
  if(all(lgk)){
    stop("Linkage stopped because all attributes were ignored.", call. = FALSE)
  }
  if(!is.null(blocking_attribute)){
    if(all(is.na(blocking_attribute))){
      stop("Linkage stopped because all records have a missing (`NA`) `strata`.", call. = FALSE)
    }
  }
  attribute <- attribute[!lgk]
  attr_nm <- names(attribute)
  probs_repo <- prep_prob_link_args(attribute = attribute,
                                    m_probability = m_probability,
                                    u_probability = u_probability)
  thresh_repo <- prep_cmps_thresh(attr_nm = attr_nm,
                                  cmp_func = cmp_func,
                                  attr_threshold = attr_threshold,
                                  score_threshold = score_threshold)

  probs_repo$m_probability$x <- lapply(probs_repo$m_probability$x, mk_lazy_opt)
  probs_repo$u_probability$x <- lapply(probs_repo$u_probability$x, mk_lazy_opt)

  if(!is.null(blocking_attribute)){
    blocking_attribute <- as.vector(blocking_attribute)
    strata <- match(blocking_attribute, blocking_attribute[!duplicated(blocking_attribute)])
    strata[is.na(blocking_attribute)] <- ((seq_len(rd_n) + max(strata))[is.na(blocking_attribute)])
  }else{
    strata <- blocking_attribute
  }

  # Create record-pairs
  if(isTRUE(ignore_same_source) & !is.null(data_source)){
    r_pairs <- make_pairs_wf_source(seq_len(rd_n),
                                    strata = strata,
                                    repeats_allowed = repeats_allowed,
                                    permutations_allowed = permutations_allowed,
                                    data_source = data_source)
  }else{
    r_pairs <- make_pairs(seq_len(rd_n),
                          strata = strata,
                          repeats_allowed = repeats_allowed,
                          permutations_allowed = permutations_allowed)
  }

  if(length(r_pairs$x_pos) == 0){
    pid_weights <- data.frame(sn_x = integer(0),
                              sn_y = integer(0))
    if(!is.null(data_source)){
      pid_weights$source_x <-
        pid_weights$source_y <- integer(0)
    }
    wts <- lapply(c(attr_nm, "weight"), function(x) numeric(0))
    names(wts) <- paste0("cmp.", c(attr_nm, "weight"))
    pid_weights <- c(pid_weights, wts)
    if(isTRUE(probabilistic)){
      wts <- lapply(c(attr_nm, "weight"), function(x) numeric(0))
      names(wts) <- paste0("prb.", c(attr_nm, "weight"))
      pid_weights <- c(pid_weights, wts)
    }
    pid_weights$record.match <- logical(0)
    pid_weights <- as.data.frame(pid_weights)
    pids <- list(pid = as.pid(seq_len(rd_n)),
                 pid_weights = pid_weights)
    rm(list = ls()[ls() != "pids"])
    return(pids)
  }
  x <- lapply(attribute, function(k) k[r_pairs$x_pos])
  y <- lapply(attribute, function(k) k[r_pairs$y_pos])
  rp_n <- length(x[[1]])

  if(!display %in% c("none")){
    rp_data <- di_report(duration = tm_ia, iteration = "Pairs created",
                         current_tot = length(x[[1]]),
                         memory_used = mem_ia)
    tm_ia <- Sys.time()
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }

  pid_weights <- prob_link(x = attrs(.obj = list(attribute = x,
                                                 m_probability = lapply(probs_repo$m_probability$x, function(k) k[r_pairs$x_pos]),
                                                 u_probability = lapply(probs_repo$u_probability$x, function(k) k[r_pairs$x_pos]))),
                           y = attrs(.obj = list(attribute = y,
                                                 m_probability = lapply(probs_repo$m_probability$x, function(k) k[r_pairs$y_pos]),
                                                 u_probability = lapply(probs_repo$u_probability$x, function(k) k[r_pairs$y_pos]))),
                           attr_threshold = thresh_repo$attr_threshold,
                           score_threshold = thresh_repo$score_threshold,
                           return_weights = TRUE,
                           cmp_func = thresh_repo$cmp_func,
                           probabilistic = probabilistic)
  if(!display %in% c("none")){
    rp_data <- di_report(duration = tm_ia, iteration =  "Weights calculated",
                         current_tot = length(x[[1]]),
                         memory_used = mem_ia)
    tm_ia <- Sys.time()
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  # Output
  if(!is.null(data_source)){
    pid_weights <- cbind(data.frame(r_pairs$x_pos, r_pairs$y_pos,
                                    data_source[r_pairs$x_pos], data_source[r_pairs$y_pos], stringsAsFactors = FALSE),
                         pid_weights)
    colnames(pid_weights)[1:4] <- c("sn_x","sn_y", "source_x", "source_y")
  }else{
    pid_weights <- cbind(data.frame(r_pairs$x_pos, r_pairs$y_pos, stringsAsFactors = FALSE), pid_weights)
    colnames(pid_weights)[1:2] <- c("sn_x","sn_y")
  }

  pids <- pid_weights[c("sn_x", "sn_y", "record.match")]
  pids <- pids[pids$record.match,]

  pids <- make_ids(pids$sn_x, pids$sn_y, rd_n)
  tots <- rle(sort(pids$group_id))
  pids <- methods::new("pid",
                       .Data = pids$group_id,
                       sn = pids$sn,
                       pid_cri = as.integer(pids$linked),
                       link_id = pids$link_id,
                       pid_total = tots$lengths[match(pids$group_id, tots$values)],
                       iteration = rep(1L, length(pids$sn)))

  if(!is.null(data_source)){
    rst <- check_links(pids@.Data, data_source, list(l = "ANY"))
    pids@pid_dataset <- encode(rst$ds)
  }
  if(!display %in% c("none")){
    rp_data <- di_report(duration = tm_ia, iteration = "`pid` created",
                         current_tot = length(x[[1]]),
                         current_tagged = nrow(pid_weights[pid_weights$record.match,]),
                         memory_used = mem_ia)
    tm_ia <- Sys.time()
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }

  pids <- list(pid = pids,
               pid_weights = pid_weights)

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- fmt(tms, "difftime")

  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    pids$report <- as.list(do.call("rbind", lapply(report, as.data.frame)))
    class(pids$report) <- "d_report"
  }

  if(!display %in% c("none", "none_with_report")) cat("Records linked in ", tms, "!\n", sep = "")
  rm(list = ls()[ls() != "pids"])
  return(pids)
}

#' @rdname links_wf
#' @export
links_wf_probabilistic <- function(attribute,
                                   blocking_attribute = NULL,
                                   cmp_func = diyar::exact_match,
                                   attr_threshold = 1,
                                   probabilistic = TRUE,
                                   m_probability = .95,
                                   u_probability = NULL,
                                   score_threshold = 1,
                                   id_1 = NULL, id_2 = NULL,
                                   return_weights = FALSE,
                                   ...){

  args <- list(...)
  err <- err_links_wf_probablistic_0(attribute = attribute,
                                     blocking_attribute = blocking_attribute,
                                     cmp_func = cmp_func,
                                     attr_threshold = attr_threshold,
                                     probabilistic = probabilistic,
                                     m_probability = m_probability,
                                     u_probability = u_probability,
                                     score_threshold = score_threshold,
                                     id_1 = id_1, id_2 = id_2)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(class(attribute) %in% c("list", "data.frame")){
    attribute <- attrs(.obj = attribute)
  }else if(class(attribute) %in% c("matrix")){
    attribute <- attrs(.obj = as.data.frame(attribute))
  }else if(class(attribute) %in% c("d_attribute")){
  }else{
    attribute <- attrs(attribute)
  }

  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }

  # Attribute names
  attr_nm <- names(attribute)
  rd_n <- length(attribute[[1]])

  lgk <- unlist(lapply(attribute, function(x){
    if(is.number_line(x)){
      length(unique(x)) == 1
    }else{
      length(x[!duplicated(x)]) == 1
    }
  }), use.names = FALSE)
  if(any(lgk)){
    warning(paste0("Attributes with identicial values in every record are ignored:\n",
                   paste0("i - `", attr_nm[lgk], "` was ignored!", collapse = "\n")), call. = FALSE)
  }
  if(all(lgk)){
    stop("Linkage stopped since all attributes were ignored.", call. = FALSE)
  }
  attribute <- attribute[!lgk]
  attr_nm <- names(attribute)

  probs_repo <- prep_prob_link_args(attribute = attribute,
                                    m_probability = m_probability,
                                    u_probability = u_probability)
  thresh_repo <- prep_cmps_thresh(attr_nm = attr_nm,
                                  cmp_func = cmp_func,
                                  attr_threshold = attr_threshold,
                                  score_threshold = score_threshold)

  probs_repo$m_probability$x <- lapply(probs_repo$m_probability$x, function(x) if(length(x) == 1) rep(x, rd_n) else x)
  probs_repo$u_probability$x <- lapply(probs_repo$u_probability$x, function(x) if(length(x) == 1) rep(x, rd_n) else x)

  probs_repo$m_probability$x <- lapply(probs_repo$m_probability$x, mk_lazy_opt)
  probs_repo$u_probability$x <- lapply(probs_repo$u_probability$x, mk_lazy_opt)

  # Weight or probabilistic matching
  prob_link_wf <- function(x, y){
    wgt <- prob_link(x, y,
                     attr_threshold = thresh_repo$attr_threshold,
                     score_threshold = thresh_repo$score_threshold,
                     return_weights = return_weights,
                     probabilistic = probabilistic,
                     cmp_func = thresh_repo$cmp_func)
    if(isFALSE(return_weights)){
      return(as.logical(wgt))
    }else{
      return(list(as.logical(wgt[,which(colnames(wgt) == "record.match")]),
                  wgt))
    }

  }

  # Identify identical records to skip repeat checks
  same_rec_func <- function(x, y){
    attr_n <- length(x$attribute)
    lgk <- sapply(seq_len(attr_n), function(i){
      lgk <- x$attribute[[i]] == y$attribute[[i]] | (is.na(x$attribute[[i]]) & is.na(y$attribute[[i]]))
      lgk[is.na(lgk)] <- FALSE
      lgk
    })
    if(is.null(nrow(lgk))){
      sum(lgk) == attr_n
    }else{
      rowSums(lgk) == attr_n
    }
  }

  # Re-calculate weights for linked records
  if(any(names(args) == "sn")){
    sn <- args$sn
  }else{
    sn <- seq_len(rd_n)
  }
  x <- list(attribute = attribute,
            m_probability = probs_repo$m_probability$x,
            u_probability = probs_repo$u_probability$x,
            sn = list(sn))
  pids <- links(criteria = "place_holder",
                strata = blocking_attribute,
                sub_criteria = list("cr1" = sub_criteria(attrs(.obj = x),
                                                         match_funcs = prob_link_wf,
                                                         equal_funcs = false)),
                ...)

  # Output
  rm(list = ls()[ls() != "pids"])
  return(pids)
}


#' @rdname links_wf
#' @export
prob_score_range <- function(attribute,
                             m_probability = .95,
                             u_probability = NULL){
  if(class(attribute) %in% c("list", "data.frame")){
    attribute <- attrs(.obj = attribute)
  }else if(class(attribute) %in% c("matrix")){
    attribute <- attrs(.obj = as.data.frame(attribute))
  }else if(class(attribute) %in% c("d_attribute")){
  }else{
    attribute <- attrs(attribute)
  }
  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }

  if(is.null(u_probability)){
    u_probability <- lapply(attribute, function(x){
      x_cd <- match(x, x[!duplicated(x)])
      x_cd[is.na(x)] <- NA_real_
      r <- rle(x_cd[order(x_cd)])
      n <- r$lengths[match(x_cd, r$values)]
      p <- n/length(x_cd)
      p[is.na(x_cd)] <- 0
      p
    })
  }

  lgk <- unlist(lapply(attribute, function(x) length(x[!duplicated(x)]) == 1), use.names = FALSE)
  if(any(lgk)){
    warning(paste0("Attributes with identicial values in every record are ignored:\n",
                   paste0("i - `", names(attribute)[lgk], "` was ignored!", collapse = "\n")), call. = FALSE)

    attribute <- attribute[!lgk]
    u_probability <- u_probability[!lgk]
  }

  if(class(m_probability) != "list"){
    m_probability <- list(m_probability)
  }
  if(length(m_probability) != 1 & any(lgk[lgk])){
    m_probability <- m_probability[!lgk]
  }
  if(length(m_probability) == 1 & length(attribute) > 1){
    m_probability <- rep(m_probability, length(attribute))
  }

  max_thresh <- sapply(seq_len(length(u_probability)), function(i){
    curr_uprob <- u_probability[[i]]
    # Exclude u-probability of '0'
    # These are from missing data and will never be an agreement
    curr_uprob[curr_uprob == 0] <- 1
    curr_mprob <- m_probability[[i]]
    log2((curr_mprob ^ 2) / (curr_uprob ^ 2))
  })
  if(is.null(nrow(max_thresh))){
    max_thresh <- max(sum(max_thresh))
  }else{
    max_thresh <- max(rowSums(max_thresh))
  }
  min_thresh <- sapply(seq_len(length(u_probability)), function(i){
    curr_uprob <- u_probability[[i]]
    curr_mprob <- m_probability[[i]]
    log2((1 - (curr_mprob ^ 2))/(1 - (curr_uprob ^ 2)))
  })
  if(is.null(nrow(min_thresh))){
    min_thresh <- min(sum(min_thresh))
  }else{
    min_thresh <- min(rowSums(min_thresh))
  }
  list(minimum_score = min_thresh,
       mid_scorce = (min_thresh + max_thresh)/2,
       maximum_score = max_thresh)
}
