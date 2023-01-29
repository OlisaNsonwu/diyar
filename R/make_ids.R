#' @name make_ids
#' @aliases make_ids
#' @title Convert an edge list to record identifiers.
#'
#' @param x_pos \code{[integer]}. Index of first half of a record-pair.
#' @param y_pos \code{[integer]}. Index of second half of a record-pair.
#' @param id_length Length of the record identifier.
#'
#' @details
#' Record groups from non-recursive links have the lowest record ID (\code{sn}) in the set as their group ID.
#'
#' @examples
#' make_ids(x_pos = rep(7, 7), y_pos = 1:7)
#' make_ids(x_pos = c(1, 6), y_pos = 6:7)
#' make_ids(x_pos = 1:5, y_pos = c(1, 1, 2, 3, 4))
#' @return \code{list}
#' @export
make_ids <- function(x_pos, y_pos, id_length = max(x_pos, y_pos)){
  sn <- seq_len(id_length)
  if(length(x_pos) == 0 & length(y_pos) == 0){
    return(
      list(sn = sn,
           linked = rep(FALSE, id_length),
           link_id = sn,
           group_id = sn)
    )
  }

  ul <- sn[!sn %in% c(x_pos, y_pos)]
  y.f <- y_pos
  x.f <- x_pos
  lgk <- which(y_pos > x_pos)
  y.f[lgk] <- x_pos[lgk]
  x.f[lgk] <- y_pos[lgk]
  lgk <- which(!duplicated(y.f + (1/x.f)))
  x.f <- x.f[lgk]
  y.f <- y.f[lgk]

  # Order for consistent results
  lgk <- order(y.f, x.f)
  x.f <- x.f[lgk]
  y.f <- y.f[lgk]

  # XXX
  lgk <- which(y.f != x.f)
  x.f <- x.f[lgk]
  y.f <- y.f[lgk]

  lst <- list(
    x.f_bk = x.f,
    y.f_bk = y.f,
    x.f = x.f,
    y.f = y.f,
    y3 = y.f)

  lst <- dv(lst)

  pos <- list(
    x = c(y.f, x.f),
    group_id = c(lst$y.f, lst$y.f),
    link_id = c(lst$y.f, lst$y.f)
  )

  sn <- seq_len(id_length)
  ids <- list(sn = sn,
              linked = NULL,
              group_id = pos$group_id[match(sn, pos$x)])

  ids$group_id[is.na(ids$group_id)] <- ids$sn[is.na(ids$group_id)]
  ids$linked <- duplicated(ids$group_id, fromLast = TRUE) | duplicated(ids$group_id, fromLast = FALSE)

  rm(list = ls()[ls() != "ids"])
  return(ids)
}
