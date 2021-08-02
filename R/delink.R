#' @name delink
#' @aliases delink
#' @title Unlink group identifiers
#'
#' @description Unlink records from an episode (\code{\link[=epid-class]{epid}}), record group (\code{\link[=pid-class]{pid}}) or pane (\code{\link[=pane-class]{pane}}) object.
#'
#' @param x [\code{\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}
#' @param lgk \code{[logical]}. Subset of records to unlink.
#' @param ... Other arguments.
#'
#' @return \link[=epid-class]{epid}; \link[=pid-class]{pid}; \link[=pane-class]{pane}
#'
#' @examples
#' ep <- episodes(1:8)
#' unlinked_ep <- delink(ep, ep@sn %in% c(3, 8))
#' ep; unlinked_ep
#'
#' pn <- partitions(1:8, length.out = 2, separate = T)
#' unlinked_pn <- delink(pn, pn@.Data == 5)
#' pn; unlinked_pn
#'
#' pd <- links(list(c(1, 1, 1, NA, NA),
#'                  c(NA, NA, 2, 2, 2),
#'                  c(NA, NA, NA, NA)))
#' unlinked_pd <- delink(pd, pd@pid_cri == 1)
#' pd; unlinked_pd
#'
#' # A warning is given if an index record is unlinked as this will lead to seemly impossible links.
#' ep2 <- episodes(1:8, 2, episode_type = "rolling")
#' unlinked_ep2 <- delink(ep2, ep2@sn %in% c(3, 5))
#' schema(ep2, custom_label = decode(ep2@case_nm), seed = 2)
#' schema(unlinked_ep2, custom_label = decode(unlinked_ep2@case_nm), seed = 2)
#' @export
delink <- function(x, lgk, ...){
  if(!is.logical(lgk)){
    stop("`lgk` must be a `logical` object", call. = FALSE)
  }
  if(!length(lgk) %in% c(1, length(x))){
    stop("Length of `lgk` must be 1 or the same as `x`", call. = FALSE)
  }
  if(all(!lgk)){
    return(x)
  }
  UseMethod("delink")
}

#' @rdname delink
#' @export
delink.epid <- function(x, lgk, ...){
  w_msg <- unlist(lapply(x@wind_id, function(y){
    any(x@sn[lgk] %in% y)
  }), use.names = FALSE)
  if(isTRUE(w_msg)){
    warning("An index/reference record has been unlinked!", call. = FALSE)
  }
  x@wind_id <- lapply(x@wind_id, function(y){
    y[lgk] <- x@sn[lgk]
    return(y)
    })
  x@.Data[lgk] <- x@sn[lgk]
  x@case_nm[lgk] <- x@wind_nm[lgk] <- -1L
  x@dist_epid_index[lgk] <- x@dist_wind_index[lgk] <- 0L
  if(!is.null(x@epid_dataset)) x@epid_dataset[lgk] <- NA_character_
  if(!is.null(x@epid_total)) x@epid_total[lgk] <- 1L
  if(!is.null(x@epid_length)) x@epid_length[lgk] <- 0L
  if(length(x@epid_interval) > 0) x@epid_interval[lgk] <- number_line(x@options$date[lgk], x@options$date[lgk])
  return(x)
}

#' @rdname delink
#' @export
delink.pane <- function(x, lgk, ...) {
  w_msg <- any(x@sn[lgk] %in% x@.Data)
  if(isTRUE(w_msg)){
    warning("An index/reference record has been unlinked!", call. = FALSE)
  }
  x@.Data[lgk] <- x@sn[lgk]
  x@case_nm[lgk] <- -1L
  x@dist_pane_index[lgk] <- x@window_matched[lgk] <- 0L
  if(!is.null(x@pane_dataset)) x@pane_dataset[lgk] <- NA_character_
  if(!is.null(x@pane_total)) x@pane_total[lgk] <- 1L
  if(!is.null(x@pane_length)) x@pane_length[lgk] <- 0L
  if(length(x@pane_interval) > 0) x@pane_interval[lgk] <- number_line(x@options$date[lgk], x@options$date[lgk])
  return(x)
}

#' @rdname delink
#' @export
delink.pid <- function(x, lgk, ...){
  w_msg <- any(x@sn[lgk] %in% x@link_id)
  if(isTRUE(w_msg)){
    warning("An index/reference record has been unlinked!", call. = FALSE)
  }
  x@.Data[lgk] <- x@link_id[lgk] <- x@sn[lgk]
  x@pid_cri[lgk] <- 0L
  x@pid_total[lgk] <- 1L
  if(!is.null(x@pid_dataset)) x@pid_dataset[lgk] <- NA_character_
  return(x)
}
