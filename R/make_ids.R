#' @name make_ids
#' @aliases make_ids
#' @title Convert and edge list to record identifiers.
#'
#' @description
#' Create record-pair combination of a vector's elements.
#'
#' @param x_pos \code{[integer]}.  Index of one half of a record pair
#' @param y_pos \code{[integer]}.  Index of one half of a record pair
#' @param id_length Target number of records.
#'
#' @examples
#' make_ids(x_pos = rep(7, 7), y_pos = 1:7)
#' make_ids(x_pos = c(1, 6), y_pos = 6:7)
#' make_ids(x_pos = 1:5, y_pos = c(1, 1, 2, 3, 4))
#' @return \code{list}
#' @export
make_ids <- function(x_pos, y_pos, id_length = max(x_pos, y_pos)){
  ord <- order(x_pos, y_pos)
  x <- x_pos <- as.integer(x_pos[ord])
  y <- y_pos <- as.integer(y_pos[ord])

  rp <- y < x
  x[rp] <- y_pos[rp]
  y[rp] <- x_pos[rp]

  # browser()
  y1 <- x[match(x, y)]
  # tmp <- y[match(y, x)]
  # y1[is.na(y1)] <- tmp[is.na(y1)]
  y1[is.na(y1)] <- x[is.na(y1)]
  y2 <- y1[match(y1, y)]
  lgk <- which(is.na(y2) & !(x != y1 | (x == y1 & y1 %in% y1[x != y1])))
  y2[lgk] <- y[lgk]
  lgk <- which(is.na(y2) & (x != y1 | (x == y1 & y1 %in% y1[x != y1])))
  y2[lgk] <- y1[lgk]

  sn <- seq_len(id_length)
  link_id <- y1[match(sn, y)]
  tmp <- y2[match(sn, x)]
  link_id[is.na(link_id)] <- tmp[is.na(link_id)]
  group_id <- y2[match(sn, y)]
  tmp <- y2[match(sn, x)]
  group_id[is.na(group_id)] <- tmp[is.na(group_id)]
  matched <- !is.na(link_id)
  group_id[!matched] <- link_id[!matched] <- sn[!matched]

  group_id <- sn[match(group_id, group_id)]
  rm(list = ls()[!ls() %in% c("sn", "link_id", "group_id", "matched")])
  return(list(sn = sn, link_id = link_id, group_id = group_id, linked = matched))
}
