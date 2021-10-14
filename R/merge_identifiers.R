#' @name merge_identifiers
#' @aliases merge_identifiers
#' @title Merge group identifiers
#'
#' @description
#' Consolidate two group identifiers.
#'
#' @details
#' Groups in \code{id1} are expanded by groups \code{id2}.
#'
#' @param id1 \code{[\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}.
#' @param id2 \code{[\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}.
#' @param ... Other arguments
#' @param tie_sort \code{[atomic]}. Preferential order for breaking tied matches.
#'
#' @seealso \code{\link{links}}; \code{\link{link_records}}; \code{\link{episodes}}; \code{\link{partitions}}
#'
#' @examples
#' data(missing_staff_id)
#' dfr <- missing_staff_id
#' id1 <- links(dfr[[5]])
#' id2 <- links(dfr[[6]])
#' id1; id2; merge_ids(id1, id2)
#' @export
merge_ids <- function(...) UseMethod("merge_ids")

#' @rdname merge_identifiers
#' @export
merge_ids.default <- function(id1, id2, tie_sort = NULL, ...){
  err <- err_atomic_vectors(id1, "id1")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_atomic_vectors(id2, "id2")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  sn1 <- seq_len(length(id1))
  sn2 <- seq_len(length(id2))

  id1 <- match(id1, id1[!duplicated(id1)])
  id2 <- match(id2, id2[!duplicated(id2)])

  pr_match <- !(!duplicated(id1, fromLast = TRUE) & !duplicated(id1, fromLast = FALSE))
  if(is.null(tie_sort)){
    ord <- order(id2, -pr_match, sn1)
  }else{
    ord <- order(id2, -pr_match, tie_sort, sn1)
  }

  repo <- list(pr_match = pr_match,
               id1 = id1, id2 = id2)

  repo <- lapply(repo, function(x) x[ord])
  repo$tr_pr_match <- repo$pr_match[match(repo$id2, repo$id2)]
  repo$tr_id1 <- repo$id1[match(repo$id2, repo$id2)]
  repo$new_id <- repo$id1
  lgk <- repo$tr_pr_match & !repo$pr_match
  repo$new_id[lgk] <- repo$tr_id1[lgk]

  repo <- repo$new_id
  repo <- repo[order(sn1[ord])]

  return(repo)
}

#' @rdname merge_identifiers
#' @export
merge_ids.pid <- function(id1, id2, tie_sort = NULL, ...){
  err <- err_object_types(id2, "id2", "pid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  mp1 <- list(seq_len(length(id1)),
              id1@sn)
  mp2 <- list(seq_len(length(id2)),
              id2@sn)

  id1@sn <- mp1[[1]]
  id1@link_id <- mp1[[1]][match(id1@link_id, mp1[[2]])]
  id1@.Data <- mp1[[1]][match(id1@.Data, mp1[[2]])]

  id2@sn <- mp2[[1]]
  id2@link_id <- mp2[[1]][match(id2@link_id, mp2[[2]])]
  id2@.Data <- mp2[[1]][match(id2@.Data, mp2[[2]])]

  pr_match <- !id1@pid_cri %in% c(-1:0)
  if(is.null(tie_sort)){
    ord <- order(id2@.Data, -pr_match, id1@pid_cri, id1@iteration, id1@sn)
  }else{
    ord <- order(id2@.Data, -pr_match, id1@pid_cri, id1@iteration, tie_sort, id1@sn)
  }

  repo <- list(pr_match = pr_match,
               id1 = id1, id2 = id2)

  repo <- lapply(repo, function(x) x[ord])
  repo$tr_pr_match <- repo$pr_match[match(repo$id2, repo$id2)]
  repo$tr_id1 <- repo$id1[match(repo$id2, repo$id2)]
  repo$new_id <- repo$id1

  new_cri <- max(repo$id1@pid_cri)
  new_cri <- ifelse(new_cri == 0, 2L, new_cri + 1L)
  new_iteration <- max(repo$id1@iteration) + 1L
  lgk <- (repo$tr_pr_match & !repo$pr_match & !duplicated(repo$id1))
  repo$new_id@link_id[lgk] <- repo$tr_id1@link_id[lgk]
  lgk <- repo$tr_pr_match & !repo$pr_match
  repo$new_id@.Data[lgk] <- repo$tr_id1@.Data[lgk]
  repo$new_id@pid_cri[lgk] <- new_cri
  repo$new_id@iteration[lgk] <- new_iteration

  new_cri <- max(repo$id1@pid_cri)
  new_cri <- ifelse(new_cri == 0, 1L, new_cri)
  new_cri <- new_cri + max(repo$id2@pid_cri)
  new_iteration <- max(repo$id1@iteration) + max(repo$id2@iteration) + 1L
  lgk <- (!repo$tr_pr_match & !repo$id2@pid_cri %in% c(-1:0))
  repo$new_id@link_id[lgk] <- repo$id2@link_id[lgk]
  repo$new_id@.Data[lgk] <- repo$id2@.Data[lgk]
  repo$new_id@pid_cri[lgk] <- new_cri
  repo$new_id@iteration[lgk] <- new_iteration

  repo$new_id@pid_dataset <- NA_character_
  repo <- repo$new_id
  repo <- repo[order(repo@sn)]

  repo@sn <- mp1[[2]][match(repo@sn, mp1[[1]])]
  repo@link_id <- mp1[[2]][match(repo@link_id, mp1[[1]])]
  repo@.Data <- mp1[[2]][match(repo@.Data, mp1[[1]])]

  tots <- rle(sort(repo@.Data))
  repo@pid_total <- tots$lengths[match(repo@.Data, tots$values)]
  return(repo)
}

#' @rdname merge_identifiers
#' @export
merge_ids.epid <- function(id1, id2, tie_sort = NULL, ...){
  err <- err_object_types(id2, "id2", "epid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- custom_sort(id1@iteration, id1@sn)
  }else{
    tie_sort <- custom_sort(id1@iteration, tie_sort, id1@sn)
  }
  merge_ids.default(id1 = as.integer(id1),
                    id2 = as.integer(id2),
                    tie_sort = tie_sort)
}

#' @rdname merge_identifiers
#' @export
merge_ids.pane <- function(id1, id2, tie_sort = NULL, ...){
  err <- err_object_types(id2, "id2", "pane")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- id1@sn
  }else{
    tie_sort <- custom_sort(id1@iteration, tie_sort, id1@sn)
  }
  merge_ids.default(id1 = as.integer(id1),
                    id2 = as.integer(id2),
                    tie_sort = tie_sort)
}






