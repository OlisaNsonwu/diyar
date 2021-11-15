#' @name make_ids
#' @aliases make_ids
#' @title Convert and edge list to record identifiers.
#'
#' @description
#' Create record-pair combination of a vector's elements.
#'
#' @param x_pos \code{[integer]}. Index of one half of a record-pair
#' @param y_pos \code{[integer]}. Index of one half of a record-pair
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

  # Flag recursive link
  t1 <- c(x.f, y.f)
  dup <- duplicated(t1, fromLast = TRUE) | duplicated(t1, fromLast = FALSE)
  is_recursive_p <- dup[match(x.f, t1)] & dup[match(y.f, t1)] & x.f != y.f
  is_recursive <- x.f %in% c(x.f[is_recursive_p], y.f[is_recursive_p]) |
    y.f %in% c(x.f[is_recursive_p], y.f[is_recursive_p])

  if(any(!is_recursive)){
    x.t <- x.f[!is_recursive]
    y.t <- y.f[!is_recursive]

    # Since, it's not a recursive link, move multi-hits to the same side
    rev_lgk <- x.t %in% y.t
    x.t2 <- c(x.t[!rev_lgk], y.t[rev_lgk])
    y.t2 <- c(y.t[!rev_lgk], x.t[rev_lgk])
    x.t <- x.t2
    x.t <- x.t2

    is_multi_link_a <-
      (!(!duplicated(x.t, fromLast = TRUE) &
           !duplicated(x.t, fromLast = FALSE)))
    is_multi_link_b <-
      (!(!duplicated(y.t, fromLast = TRUE) &
           !duplicated(y.t, fromLast = FALSE)))

    # If there are no multi-hits, assign the lower record id as a proxy multi-hit.
    # Makes the lower record id the preference for group/link id
    adj_lgk <- !is_multi_link_a & !is_multi_link_b & x.t < y.t
    is_multi_link_a[adj_lgk] <- !is_multi_link_a[adj_lgk]
    adj_lgk <- !is_multi_link_a & !is_multi_link_b & x.t > y.t
    is_multi_link_b[adj_lgk] <- !is_multi_link_b[adj_lgk]

    tp.y <- c(y.t[is_multi_link_b], x.t[is_multi_link_a], y.t[is_multi_link_b], x.t[is_multi_link_a])
    tp.x <- c(x.t[!is_multi_link_a], y.t[!is_multi_link_b], y.t[is_multi_link_b], x.t[is_multi_link_a])
    pr_sn <- tp.x[!duplicated(tp.x)]
    pr_sn <- sort(pr_sn)

    ids_1 <- list(sn = pr_sn,
                  link_id = tp.y[match(pr_sn, tp.x)],
                  group_id = tp.y[match(pr_sn, tp.x)])

    # Earliest record ID used for consistency for links()
    ids_1$link_id <- ids_1$sn[match(ids_1$link_id, ids_1$link_id)]
    ids_1$group_id <- ids_1$sn[match(ids_1$group_id, ids_1$group_id)]
  }else{
    ids_1 <- list()
  }
  # Second, create IDs for recursive links
  if(any(is_recursive)){
    x.t <- x.f[is_recursive]
    y.t <- y.f[is_recursive]

    # Links
    # lk_x <- x.t[is_recursive_p[is_recursive]]
    # lk_y <- y.t[is_recursive_p[is_recursive]]
    lk_x <- x.t
    lk_y <- y.t
    # Recurse
    y1 <- lk_y[match(lk_y, lk_x)]
    x1 <- c(lk_x, lk_y)
    y1 <- c(y1, y1)
    rp <- list(x = x1, y = y1, p = c(seq_len(length(lk_x)), seq_len(length(lk_x))))
    rp <- lapply(rp, function(x) x[order(rp$y, rp$x)])
    rp$y <- rp$y[match(rp$x, rp$x)]
    rp <- lapply(rp, function(x) x[order(rp$y, rp$x)])
    rp$y <- rp$y[match(rp$p, rp$p)]
    rp$y[is.na(rp$y)] <- rp$x[is.na(rp$y)]

    # All records part of a recursive link
    tp.x <- x.t
    tp.y <- y.t
    id <- rp$y[match(y.t, rp$x)]
    # Record level ID
    pr_sn <- c(x.t, y.t)
    pr_sn <- pr_sn[!duplicated(pr_sn)]
    pr_sn <- sort(pr_sn)
    #
    x2 <- c(x.t, y.t)
    l2 <- c(y.t, y.t)
    y2 <- c(id, id)

    ids_2 <- list(sn = pr_sn,
                  link_id = l2[match(pr_sn, x2)],
                  group_id = y2[match(pr_sn, x2)])
  }else{
    ids_2 <- list()
  }
  ids <- list(sn = c(ids_1$sn, ids_2$sn, ul),
              linked = NULL,
              link_id = c(ids_1$link_id, ids_2$link_id, ul),
              group_id = c(ids_1$group_id, ids_2$group_id, ul))
  ids$linked <- duplicated(ids$group_id, fromLast = TRUE) | duplicated(ids$group_id, fromLast = FALSE)
  lgk <- order(ids$sn)
  ids <- lapply(ids, function(x) x[lgk])
  rm(list = ls()[ls() != "ids"])
  return(ids)
}
