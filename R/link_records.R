#' @name link_records
#' @title Record linkage
#'
#' @description Deterministic and probabilistic record linkage with partial or evaluated matches.
#'
#' @param blocking_attribute \code{[atomic]}. Subsets of the dataset.
#' @param attribute \code{[atomic|list|data.frame|matrix|d_attribute]}. Attributes to compare.
#' @param cmp_func \code{[list|function]}. String comparators for each \code{attribute}. See \code{Details}.
#' @param attr_threshold \code{[list|numeric|\link{number_line}]}. Weight-thresholds for each \code{cmp_func}. See \code{Details}.
#' @param probabilistic \code{[logical]}. If \code{TRUE}, scores are assigned base on Fellegi-Sunter model for probabilistic record linkage. See \code{Details}.
#' @param m_probability \code{[list|numeric]}. The probability that a matching records are the same entity.
#' @param u_probability \code{[list|numeric]}. The probability that a matching records are not the same entity.
#' @param score_threshold \code{[numeric|\link{number_line}]}. Score-threshold for linked records. See \code{Details}.
#' @param id_1 \code{[list|numeric]}. Record id or index of one half of a record pair.
#' @param id_2 \code{[list|numeric]}. Record id or index of one half of a record pair.
#' @param ... Arguments passed to \bold{\code{links}}
#' @param repeats_allowed \code{[logical]} If \code{TRUE}, repetition are included.
#' @param permutations_allowed \code{[logical]} If \code{TRUE}, permutations are included.
#' @param ignore_same_source \code{[logical]} If \code{TRUE}, only records from different \bold{\code{data_source}} are compared.
#' @param data_source \code{[character]}. Data source identifier. Adds the list of data sources in each record-group to the \code{\link[=pid-class]{pid}}. Useful when the data is from multiple sources.
#'
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{links}}
#'
#' @details
#' \code{link_records()} and \code{links_wf_probabilistic()} are functions to implement probabilistic record linkage.
#' \code{link_records()} compares every record-pair in one instance,
#' while \code{links_wf_probabilistic()} is a wrapper function of \code{\link{links}} and so compares batches of record-pairs in iterations.
#'
#' \code{link_records()} is more thorough in the sense that it compares every combination of record-pairs.
#' This makes it faster but is memory intensive, particularly if there's no \code{blocking_attribute}.
#' In contrast, \code{links_wf_probabilistic()} is less memory intensive but takes longer since it compares batches of record-pairs in iterations.
#'
#' The implementation of probabilistic record linkage is based on Fellegi and Sunter (1969) model for deciding if two records belong to the same entity.
#'
#' In summary, record-pairs are created and categorised as matches and non-matches (\code{attr_threshold}) with user-defined functions (\code{cmp_func}).
#' Two probabilities (\code{m} and \code{u}) are then estimated for each record-pair to score the matches and non-matches.
#' The \code{m}-probability is the probability that matched records are actually from the same entity i.e. a true match,
#' while \code{u}-probability is the probability that matched records are not from the same entity i.e. a false match.
#' By default, \code{u}-probabilities are calculated as the frequency of each value of an \code{attribute} however,
#' they can also be supplied along with \code{m}-probabilities.
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
#' Missing data (\code{NA}) are considered non-matches and assigned a \code{u}-probability of \code{0}.
#'
#' By default, matches and non-matches for each \code{attribute} are determined as an \code{\link{exact_match}} with a binary outcome.
#' Alternatively, user-defined functions (\code{cmp_func}) are used to create similarity scores.
#' Pairs with similarity scores within (\code{attr_threshold}) are then considered matches for the corresponding \code{attribute}.
#'
#' If \code{probabilistic} is \code{FALSE},
#' the sum of all similarity scores is used as the \code{score_threshold} instead of deriving one from the \code{m} and \code{u}-probabilities.
#'
#' A \code{blocking_attribute} can be used to reduce the processing time by restricting comparisons to subsets of the dataset.
#'
#' In \code{link_records()}, \code{score_threshold} is a convenience argument because every combination of record-pairs are returned
#' therefore, a new \code{score_threshold} can be selected after reviewing the final scores.
#' However, in \code{links_wf_probabilistic()}, the \code{score_threshold} is more important
#' because a final selection is made at each iteration.
#'
#' As a result, \code{links_wf_probabilistic()} requires an acceptable \code{score_threshold} in advance.
#' To help with this, \code{prob_score_range()} can be used to return the range of scores attainable for a given set of \code{attribute}, \code{m} and \code{u}-probabilities.
#' Additionally, \code{id_1} and \code{id_2} can be used to link specific records pairs, aiding the review of potential scores.


#' @references
#' Fellegi, I. P., & Sunter, A. B. (1969). A Theory for Record Linkage. \emph{Journal of the Statistical Association}, 64(328), 1183–1210. https://doi.org/10.1080/01621459.1969.10501049
#'
#' Asher, J., Resnick, D., Brite, J., Brackbill, R., & Cone, J. (2020). An Introduction to Probabilistic Record Linkage with a Focus on Linkage Processing for WTC Registries. \emph{International journal of environmental research and public health}, 17(18), 6937. https://doi.org/10.3390/ijerph17186937.
#'
#' @aliases link_records
#'
#' @examples
#' # Using exact matches
#' dfr <- missing_staff_id[c(2, 4, 5, 6)]
#' link_records(dfr, attr_threshold = 1, probabilistic = FALSE, score_threshold = 2)
#' links_wf_probabilistic(dfr, attr_threshold = 1, probabilistic = FALSE,
#'                        score_threshold = 2, recursive = TRUE)
#'
#' link_records(dfr, attr_threshold = 1, probabilistic = TRUE, score_threshold = -16)
#' links_wf_probabilistic(dfr, attr_threshold = 1, probabilistic = TRUE,
#'                        score_threshold = -16, recursive = TRUE)
#'
#' # Using string comparators
#' # For example, matching last word in `hair_colour` and `branch_office`
#' last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
#' last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
#'
#' link_records(dfr, attr_threshold = 1,
#'              cmp_func = c(diyar::exact_match,
#'                           diyar::exact_match,
#'                           last_word_cmp,
#'                           last_word_cmp),
#'              score_threshold = -4)
#' links_wf_probabilistic(dfr, attr_threshold = 1,
#'                     cmp_func = c(diyar::exact_match,
#'                                  diyar::exact_match,
#'                                  last_word_cmp,
#'                                  last_word_cmp),
#'                     score_threshold = -4,
#'                     recursive = TRUE)
#'
#' @export
link_records <- function(attribute,
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
    rp_data <- di_report(tm_a, "Data validation", current_tot = length(attrs(attribute)[[1]]))
    report <- list(rp_data)
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  tm_ia <- Sys.time()

  args_repo <- prep_prob_link_args(attribute = attribute,
                                   blocking_attribute = blocking_attribute,
                                   cmp_func = cmp_func,
                                   attr_threshold = attr_threshold,
                                   probabilistic = probabilistic,
                                   m_probability = m_probability,
                                   score_threshold = score_threshold,
                                   u_probability = u_probability,
                                   repeats_allowed = repeats_allowed,
                                   permutations_allowed = permutations_allowed,
                                   ignore_same_source = ignore_same_source,
                                   data_source = data_source)
  if(!display %in% c("none")){
    rp_data <- di_report(tm_a, "Pairs created", current_tot = length(args_repo$x[[1]]))
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  tm_ia <- Sys.time()
  pid_weights <- prob_link(x = c(args_repo$x, args_repo$m_probability, args_repo$u_probability),
                            y = args_repo$y,
                            attr_threshold = args_repo$attr_threshold,
                            score_threshold = args_repo$score_threshold,
                            return_weights = TRUE,
                            cmp_func = args_repo$cmp_func,
                            probabilistic = probabilistic)
  if(!display %in% c("none")){
    rp_data <- di_report(tm_a, "Weights calculated", current_tot = length(args_repo$x[[1]]))
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  # Output
  if(!is.null(data_source)){
    pid_weights <- cbind(data.frame(args_repo$r_pairs$x_pos, args_repo$r_pairs$y_pos,
                         data_source[args_repo$r_pairs$x_pos], data_source[args_repo$r_pairs$y_pos], stringsAsFactors = FALSE),
                         pid_weights)
    colnames(pid_weights)[1:4] <- c("sn_x","sn_y", "source_x", "source_y")
  }else{
    pid_weights <- cbind(data.frame(args_repo$r_pairs$x_pos, args_repo$r_pairs$y_pos, stringsAsFactors = FALSE), pid_weights)
    colnames(pid_weights)[1:2] <- c("sn_x","sn_y")
  }

  pids <- pid_weights[c("sn_x", "sn_y", "record.match")]
  pids <- pids[pids$record.match,]

  pids <- make_ids(pids$sn_x, pids$sn_y, max(c(pid_weights[,1], pid_weights[,2])))
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
    rp_data <- di_report(tm_a, "`pid` created", current_tot = length(args_repo$x[[1]]), current_tagged = nrow(pid_weights[pid_weights$record.match,]))
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", fmt(rp_data[[2]], "difftime"), "\n"))
    }
  }
  pids <- list(pid = pids,
               pid_weights = pid_weights)
  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    pids$report <- as.list(do.call("rbind", lapply(report, as.data.frame)))
    class(pids$report) <- "d_report"
  }
  rm(list = ls()[ls() != "pids"])
  return(pids)
}

#' @rdname link_records
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
                                   ...
){

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

  args_repo <- prep_prob_link_args(attribute = attribute,
                                   blocking_attribute = blocking_attribute,
                                   cmp_func = cmp_func,
                                   attr_threshold = attr_threshold,
                                   probabilistic = probabilistic,
                                   m_probability = m_probability,
                                   score_threshold = score_threshold,
                                   u_probability = u_probability,
                                   method = "links")

  # Weight or probabilistic matching
  prob_link_wf <- function(x, y){
    prob_link(x, y,
               attr_threshold = args_repo$attr_threshold,
               score_threshold = args_repo$score_threshold,
               return_weights = FALSE,
               probabilistic = probabilistic,
               cmp_func = args_repo$cmp_func)
  }

  # Identify identical records to skip repeat checks
  same_rec_func <- function(x, y){
    attr_n <- length(x)
    lgk <- sapply(seq_len(attr_n), function(i){
      lgk <- x[[i]] == y[[i]] | (is.na(x[[i]]) & is.na(y[[i]]))
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
  if(!is.null(id_1) & !is.null(id_2)){
    pids <- NULL
    x <- c(args_repo$attribute, args_repo$m_probability, args_repo$u_probability)
    y <- lapply(x, function(k) k[id_2])
    x <- lapply(x, function(k) k[id_1])
    thresh_lgk <- integer()
  }else{
    pids <- links(criteria = "place_holder",
                  strata = args_repo$blocking_attribute,
                  sub_criteria = list("cr1" = sub_criteria(attrs(.obj = c(args_repo$attribute, args_repo$m_probability, args_repo$u_probability)),
                                                           match_funcs = prob_link_wf,
                                                           equal_funcs = same_rec_func)),
                  ...)
    x <- c(args_repo$attribute, args_repo$m_probability, args_repo$u_probability)
    y <- lapply(x, function(k) k[match(pids@link_id, pids@sn)])
    id_1 <- pids@sn
    id_2 <- pids@link_id
    thresh_lgk <- which(pids@pid_cri %in% -1:0)
  }

  pid_weights <- prob_link(x, y,
                           attr_threshold = args_repo$attr_threshold,
                           score_threshold = args_repo$score_threshold,
                           return_weights = TRUE,
                           cmp_func = args_repo$cmp_func,
                           probabilistic = probabilistic)
  # Mask unlinked records
  pid_weights[thresh_lgk,] <- NA
  pid_weights <- cbind(data.frame(id_1, id_2, stringsAsFactors = FALSE), pid_weights)
  colnames(pid_weights)[1:2] <- c("sn_x","sn_y")

  # Output
  pids <- list(pid = pids,
               pid_weights = pid_weights)
  rm(list = ls()[ls() != "pids"])
  return(pids)
}

#' @rdname link_records
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
    log2(curr_mprob/curr_uprob)
  })
  if(is.null(nrow(max_thresh))){
    max_thresh <- max(sum(max_thresh))
  }else{
    max_thresh <- max(rowSums(max_thresh))
  }
  min_thresh <- sapply(seq_len(length(u_probability)), function(i){
    curr_uprob <- u_probability[[i]]
    curr_mprob <- m_probability[[i]]
    log2((1 - curr_mprob)/(1 - curr_uprob))
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
