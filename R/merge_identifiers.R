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
#' @seealso \code{\link{links}}; \code{\link{links_sv_probabilistic}}
#'
#' @examples
#' id1 <- rep(1, 5)
#' id2 <- c(2, 2, 3, 3, 3)
#' merge_ids(id1, id2, shrink = TRUE)
#'
#' id1 <- c(rep(1, 3), 6, 7)
#' id2 <- c(2,2,3,3,3)
#' merge_ids(id1, id2, shrink = TRUE)
#' merge_ids(id1, id2, expand = FALSE)
#'
#' id1 <- rep(1, 5)
#' id2 <- c(1:3, 4, 4)
#' merge_ids(id1, id2, shrink = TRUE)
#' merge_ids(id1, id2, expand= FALSE)
#'
#' data(missing_staff_id)
#' dfr <- missing_staff_id
#' id1 <- links(dfr[[5]])
#' id2 <- links(dfr[[6]])
#' merge_ids(id1, id2)
#' @export
merge_ids <- function(...) UseMethod("merge_ids")

#' @rdname merge_identifiers
#' @export
merge_ids.default <- function(id1, id2, tie_sort = NULL,
                              expand = TRUE, shrink = FALSE, ...){
  #
  overwrite <- FALSE
  #
  err <- err_atomic_vectors(id1, "id1")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_atomic_vectors(id2, "id2")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  sn1 <- seq_len(length(id1))
  pr_match <- !(!duplicated(id1, fromLast = TRUE) & !duplicated(id1, fromLast = FALSE))

  if(isTRUE(shrink)){
    # id1[!pr_match] <- max(c(id1, id2)) + 1L
    new_id <- combi(id1, id2)
    if(is.null(tie_sort)){
      ord <- order(new_id, sn1)
    }else{
      ord <- order(new_id, tie_sort, sn1)
    }
    repo <- list(id1 = id1, sn1 = sn1, new_id = new_id, pr_match = pr_match,
                 cr_match = !(!duplicated(new_id, fromLast = TRUE) & !duplicated(new_id, fromLast = FALSE)))
    repo <- lapply(repo, function(x) x[ord])

    repo$new_id <- repo$id1 + ((repo$cr_match + 1)/10)
    lgk <- !repo$cr_match & repo$pr_match
    repo$new_id[lgk] <- id1[lgk]
    repo$new_id <- repo$sn1[match(repo$new_id, repo$new_id)]
  }else{
    if(isTRUE(expand)){
      pr_match_ord <- -pr_match
    }else{
      pr_match_ord <- pr_match
    }
    if(is.null(tie_sort)){
      ord <- order(id2, pr_match_ord, sn1)
    }else{
      ord <- order(id2, pr_match_ord, tie_sort, sn1)
    }
    repo <- list(pr_match = pr_match, id1 = id1, id2 = id2, sn1 = sn1)
    repo <- lapply(repo, function(x) x[ord])
    if(isTRUE(expand)){
      repo$tr_pr_match <- repo$pr_match[match(repo$id2, repo$id2)]
      repo$tr_id1 <- repo$id1[match(repo$id2, repo$id2)]
    }else{
      repo$tr_pr_match <- rep(TRUE, length(repo[[1]]))
      repo$tr_id1 <- repo$sn1[match(repo$id2, repo$id2)]
    }
    repo$cr_match <- !(!duplicated(repo$id2, fromLast = TRUE) & !duplicated(repo$id2, fromLast = FALSE))
    repo$new_id <- repo$id1
    lgk <- (repo$tr_pr_match & (!repo$pr_match | overwrite)) |
      (!repo$tr_pr_match & repo$cr_match)
    repo$new_id[lgk] <- repo$tr_id1[lgk]
  }
  repo <- repo$new_id[order(repo$sn1)]
  return(repo)
}

#' @rdname merge_identifiers
#' @export
merge_ids.pid <- function(id1, id2, tie_sort = NULL,
                          expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "pid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- custom_sort(id1@.Data, id1@pid_cri, id1@iteration, id1@sn)
  }else{
    tie_sort <- custom_sort(id1@.Data, id1@pid_cri, id1@iteration, tie_sort, id1@sn)
  }

  new_pid <- id1
  new_pid@.Data <- merge_ids.default(id1 = id1@.Data,
                                     id2 = id2@.Data,
                                     tie_sort = tie_sort)
  new_pid@link_id <- list(link_id1 = new_pid@link_id)
  # new_pid@link_id <- merge_ids.default(id1 = id1@link_id,
  #                                      id2 = id2@link_id,
  #                                      tie_sort = tie_sort)
  lgk <- id1@.Data != new_pid@.Data
  lgk <- new_pid@.Data %in% new_pid@.Data[lgk]
  mx_cri <- max(id1@pid_cri)
  mx_cri <- ifelse(mx_cri == 0, 1L, mx_cri)
  new_pid@pid_cri[
    (lgk & new_pid@pid_cri == 0)
  ] <-  mx_cri + 1L

  new_pid@pid_total <- id_total(new_pid@.Data)
  new_pid
}

#' @rdname merge_identifiers
#' @export
merge_ids.epid <- function(id1, id2, tie_sort = NULL,
                           expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "epid")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- custom_sort(id1@.Data, id1@iteration, id1@sn)
  }else{
    tie_sort <- custom_sort(id1@.Data, id1@iteration, tie_sort, id1@sn)
  }
  new_epid <- merge_ids.default(id1 = id1@.Data,
                                id2 = id2@.Data,
                                tie_sort = tie_sort)
  new_epid <- as.epid(new_epid)
  new_epid
}

#' @rdname merge_identifiers
#' @export
merge_ids.pane <- function(id1, id2, tie_sort = NULL,
                           expand = TRUE, shrink = FALSE, ...){
  err <- err_object_types(id2, "id2", "pane")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(id2, "id1", length(id1), "id2")
  if(err != FALSE) stop(err, call. = FALSE)

  if(is.null(tie_sort)){
    tie_sort <- id1@sn
  }else{
    tie_sort <- custom_sort(id1@iteration, tie_sort, id1@sn)
  }
  merge_ids.default(id1 = id1@.Data,
                    id2 = id2@.Data,
                    tie_sort = tie_sort)
}






