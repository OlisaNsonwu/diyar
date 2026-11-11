#' @name merge_identifiers
#' @aliases merge_identifiers
#' @title Merge group identifiers
#'
#' @description
#' Consolidate two group identifiers.
#'
#' @details
#' Groups in \code{id1} are expanded or shrunk by groups in \code{id2}.
#'
#' A unique group with only one record is considered a non-matching record.
#'
#' Note that the \code{expand} and \code{shrink} features are not interchangeable.
#' The outcome when \code{shrink} is \code{TRUE} is not the same when \code{expand} is \code{FALSE}.
#' See \code{Examples}.
#'
#' @param id1 \code{[integer|\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}.
#' @param id2 \code{[integer|\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}.
#' @param ... Other arguments
#' @param tie_sort \code{[atomic]}. Preferential order for breaking tied matches.
#' @param expand \code{[logical]}. If \code{TRUE}, \code{id1} gains new records if \code{id2} indicates a match. \emph{Not interchangeable with \code{shrink}}.
#' @param shrink \code{[logical]}. If \code{TRUE}, \code{id1} loses existing records \code{id2} does not indicate a match. \emph{Not interchangeable with \code{expand}}.
#'
#' @seealso \code{\link{links}}; \code{\link{links_af_probabilistic}}
#'
#' @examples
#' id1 <- rep(1, 5)
#' id2 <- c(2, 2, 3, 3, 3)
#' merge_ids(id1, id2, stepwise_method = 'shrink_to_last_match')
#'
#' id3 <- c(rep(1, 3), 6, 7)
#' id4 <- c(2,2,3,3,3)
#' merge_ids(id3, id4, stepwise_method = 'shrink_to_last_match')
#' merge_ids(id3, id4, stepwise_method = 'exclude_previous_matches')
#'
#' id5 <- rep(1, 5)
#' id6 <- c(1:3, 4, 4)
#' merge_ids(id5, id6, stepwise_method = 'shrink_to_last_match')
#' merge_ids(id5, id6, stepwise_method = 'exclude_previous_matches')
#'
#' data(missing_staff_id)
#' dfr <- missing_staff_id
#' id7 <- links(dfr$hair_colour)
#' id8 <- links(dfr$branch_office)
#' merge_ids(id7, id8)
#' @export
merge_ids <- function(...) UseMethod("merge_ids")

#' @rdname merge_identifiers
#' @export
merge_ids.default <- function(id1, id2, tie_sort = NULL, stepwise_method = 'expand_with_priority', expand = TRUE, shrink = FALSE, ...){
  err <- err_atomic_vectors(id1, "id1")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_atomic_vectors(id2, "id2")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  dfr <- data.table::data.table(id1 = id1, id2 = id2, bkp_id = id1)
  dfr[is.na(id1) != TRUE, old_n := .N, by = id1]
  dfr[is.na(id1), old_n := 1]
  dfr[is.na(id2) != TRUE, new_n := .N, by = id2]
  dfr[is.na(id2), new_n := 1]
  dfr[, c('old_match', 'new_match') := .(old_match = old_n > 1, new_match = new_n > 1)]

  # dfr <- data.frame(id1, id2)
  # dfr$old_match <- bys_count(id1) > 1 & !is.na(id1)
  # dfr$new_match <- bys_count(id2) > 1 & !is.na(id2)
  dfr <- data.table::as.data.table(dfr)
  sort_var <- c('id2', 'old_match')
  sort_ord <- c(1, -1)
  if(!is.null(tie_sort)){
    dfr[, tie_sort := tie_sort]
    sort_var <- c(sort_var, 'tie_sort')
    sort_ord <- c(sort_ord, 1)
  }
  dfr[, c('include', 'sn') := .(include = new_match, sn = .I)]

  if(stepwise_method == 'shrink_to_last_match'){
    dfr[include == TRUE, include := old_match]
    dfr[include == TRUE, id2 := combi(id1, id2)]
    dfr[include == TRUE, id1 := id2]
  }
  data.table::setorderv(dfr, sort_var, order = sort_ord)
  if(stepwise_method == 'exclude_previous_matches'){
    dfr[old_match == TRUE, include := FALSE]
  }
  dfr[,f_match := FALSE]
  dfr[include == TRUE, c('temp_id', 'f_match') := .(temp_id = id1[1], f_match = .N > 1), by = id2]

  if(stepwise_method == 'shrink_to_last_match'){
    dfr[, update := f_match == TRUE]
  }else{
    dfr[, update := new_match == TRUE & old_match == FALSE]
  }
  if(stepwise_method == 'expand_without_priority'){
    dfr[new_match == TRUE, update := TRUE]
  }
  dfr[update == TRUE, id1 := temp_id]
  if(stepwise_method == 'shrink_to_last_match'){
    dfr[include == FALSE, c("id1", "update") := .(id1 = bkp_id, update = FALSE)]
  }

  data.table::setorder(dfr, sn)
  dfr[, stg := as.integer(old_match)]
  dfr[update == TRUE, stg := 2L]
  if(stepwise_method == 'shrink_to_last_match'){
    dfr[include == TRUE & update == FALSE, stg := 0L]
  }
  return(list(id = dfr$id1, stg = dfr$stg))
}

#' @rdname merge_identifiers
#' @export
merge_ids.pid <- function(id1, id2, tie_sort = NULL,
                          stepwise_method = 'expand_with_priority', expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "pid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- custom_sort(id1@.Data, id1@pid_cri, id1@iteration, id1@sn)
  }else{
    tie_sort <- custom_sort(id1@.Data, id1@pid_cri, id1@iteration, tie_sort, id1@sn)
  }

  new_pid <- merge_ids.default(
    id1 = id1@.Data, id2 = id2@.Data, tie_sort = tie_sort,
    stepwise_method = stepwise_method)

  new_pid <- as.pid(new_pid$id, pid_cri = new_pid$stg)
  new_pid
}

#' @rdname merge_identifiers
#' @export
merge_ids.epid <- function(id1, id2, tie_sort = NULL,
                           stepwise_method = 'expand_with_priority', expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "epid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- custom_sort(id1@.Data, id1@iteration, id1@sn)
  }else{
    tie_sort <- custom_sort(id1@.Data, id1@iteration, tie_sort, id1@sn)
  }
  new_epid <- merge_ids.default(
    id1 = id1@.Data, id2 = id2@.Data, tie_sort = tie_sort,
    stepwise_method = stepwise_method)
  new_epid <- as.epid(new_epid$id)
  new_epid
}

#' @rdname merge_identifiers
#' @export
merge_ids.pane <- function(id1, id2, tie_sort = NULL,
                           stepwise_method = 'expand_with_priority', expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "pane")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- id1@sn
  }else{
    tie_sort <- custom_sort(id1@iteration, tie_sort, id1@sn)
  }
  merge_ids.default(
    id1 = id1@.Data, id2 = id2@.Data, tie_sort = tie_sort,
    stepwise_method = stepwise_method)
}






