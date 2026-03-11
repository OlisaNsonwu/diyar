#' @name bys_funcs_legacy
#'
#' @title Vectorised approach to group operations.
#'
#' @param ... \code{[atomic]}. Sort levels.
#' @param by \code{[atomic]}. Groups.
#' @param n \code{[integer]} Position.
#' @param nmax \code{[logical]} If \code{TRUE}, use \code{length([by])} when \code{n} is greater than the number of records in a group.
#' @param from_last \code{[logical]} Sort order - \code{TRUE} (descending) or \code{FALSE} (ascending).
#' @param val \code{[atomic]}. Value.
#'
#' @return \code{[atomic]}
#'
#' @aliases bys_funcs_legacy
#' @examples
#' x <- data.frame(
#'   group = c(2, 2, 1, 2, 1, 1, 1, 2, 1, 1),
#'   value = c(13, 14, 20, 9, 2, 1, 8, 18, 3, 17))
#'
#' bys_count_legacy(x$group)
#' bys_position_legacy(x$value, by = x$group, from_last = TRUE)
#' bys_rank_legacy(by = x$group, val = x$value, from_last = TRUE)
#' bys_val_legacy(x$value, by = x$group, val = x$value, from_last = TRUE)
#' bys_nval_legacy(x$value, by = x$group, val = x$value, from_last = TRUE, n = 2)
#' bys_min_legacy(by = x$group, val = x$value)
#' bys_max_legacy(by = x$group, val = x$value)
#' bys_sum_legacy(by = x$group, val = x$value)
#' bys_prod_legacy(by = x$group, val = x$value)
#' bys_cummin_legacy(by = x$group, val = x$value)
#' bys_cummax_legacy(by = x$group, val = x$value)
#' bys_cumsum_legacy(by = x$group, val = x$value)
#' bys_cumprod_legacy(by = x$group, val = x$value)
#' bys_lag_legacy(by = x$group, val = x$value)
#' bys_lead_legacy(by = x$group, val = x$value)

#' @rdname bys_funcs_legacy
#' @export
bys_count_legacy <- function(by, unique.var = NULL){
  if(!is.atomic(by)) stop("`by` must be an `atomic` vector!")
  if(length(by) == 0) stop("`by` has a length of 0!")

  by <- match(by, by[!duplicated(by)])
  s_ord <- order(by)
  by <- by[s_ord]
  rp <- rle(by)

  if(!is.null(unique.var)){
    unique.var <- unique.var[s_ord]
    unique.var <- combi(by, unique.var)
    by <- by[!duplicated(unique.var)]
    rp2 <- rle(by)
    x <- rep(rp2$lengths, rp$lengths)[order(s_ord)]
  }else{
    x <- rep(rp$lengths, rp$lengths)[order(s_ord)]
  }
  return(x)
}

#' @rdname bys_funcs_legacy
#' @export
bys_rank_legacy <- function(..., by = NULL, from_last = FALSE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, max(sapply(list(...), length)))
  }else{
    if(!inherits(by, c('numeric', 'integer'))){
      by <- match(by, by[!duplicated(by)])
    }
  }
  if(length(list(...)) == 0){
    s_ord <- order(by, decreasing = from_last, na.last = TRUE)
  }else{
    s_ord <- order(by, ..., decreasing = from_last, na.last = TRUE)
  }

  by <- by[s_ord]
  rp <- rle(by)
  x <- sequence(rp$lengths)[order(s_ord)]
  return(x)
}

#' @rdname bys_funcs_legacy
#' @param ordered If \code{TRUE}, the output is sequential.
#' @export
bys_position_legacy <- function(val, by = NULL, from_last = FALSE, ordered = TRUE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
  }
  s_ord <- order(by, val, decreasing = from_last, na.last = TRUE)

  by <- by[s_ord]
  rp <- rle(by)
  x <- sequence(rp$lengths)

  st <- by
  by <- combi(by, val[s_ord])
  rp <- rle(by)
  faC <- as.integer(log10(max(rp$lengths, na.rm = TRUE))) + 1L
  faC <- 10 ^ faC

  if(!ordered){
    x <- rep(
      x[!duplicated(by)] + (rp$lengths)/faC,
      rp$lengths)
  }else{
    x <- rep(
      ((by[!duplicated(by)] - bys_min_legacy(by = st[!duplicated(by)], val = by[!duplicated(by)])) + 1L) + (rp$lengths)/faC,
      rp$lengths
    )
  }

  x <- x[order(s_ord)]
  return(x)
}

#' @rdname bys_funcs_legacy
#' @export
bys_val_legacy <- function(..., val, by = NULL, from_last = FALSE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
  }
  if(length(list(...)) == 0){
    s_ord <- order(by, decreasing = from_last, na.last = TRUE)
  }else{
    s_ord <- order(by, ..., decreasing = from_last, na.last = TRUE)
  }

  by <- by[s_ord]
  val <- val[s_ord]
  rp <- rle(by)
  val <- rep(val[!duplicated(by)], rp$lengths)[order(s_ord)]
  return(val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_nval_legacy <- function(..., val, by = NULL, from_last = FALSE, n = 1, nmax = FALSE){
  ord <- as.integer(bys_position_legacy(..., by = by, from_last = from_last))
  s_ord <- !(ord >= n)

  if(!nmax){
    val[s_ord] <- NA
  }

  return(
    bys_val_legacy(
      s_ord, ord,
      by = by, from_last = FALSE, val = val)
  )
}

#' @rdname bys_funcs_legacy
#' @param na.rm If \code{TRUE}, remove \code{NA} values
#' @export
bys_min_legacy <- function(val, by = NULL, na.rm = TRUE){
  val2 <- bys_val_legacy(val, by = by, from_last = FALSE, val = val)
  if(!na.rm){
    val2[
      by %in% by[is.na(val)]
    ] <- NA
  }
  return(val2)
}

#' @rdname bys_funcs_legacy
#' @export
bys_max_legacy <- function(val, by = NULL, na.rm = TRUE){
  val2 <- bys_val_legacy(val, by = by, from_last = TRUE, val = val)
  if(!na.rm){
    val2[
      by %in% by[is.na(val)]
    ] <- NA
  }
  return(val2)
}

#' @rdname bys_funcs_legacy
#' @export
bys_sum_legacy <- function(val, by = NULL, na.rm = TRUE, cumulative = FALSE){
  lgk <- !duplicated(by)
  if(length(which(lgk)) < 2){
    out.val <- sum(val, na.rm = na.rm)
  }else{
    by <- match(by, by[lgk])

    if(!na.rm){
      NA_by <- by[is.na(val)]
    }
    val[is.na(val)] <- 0

    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]

    rp <- rle(by)
    cum.val <- cumsum(val)
    lgk <- !duplicated(by, fromLast = TRUE)
    max.val <- cum.val[lgk]
    by <- by[lgk]

    lag_pos <- 1:(length(max.val) - 1)
    prv_max.val <- c(0, max.val[lag_pos])

    if(cumulative){
      out.val <- cum.val - rep(prv_max.val, rp$lengths)
    }else{
      out.val <- max.val - c(0, max.val[lag_pos])
      out.val <- rep(out.val, rp$lengths)
    }

    if(!na.rm){
      out.val[rep(rp$values, rp$lengths) %in% NA_by] <- NA
    }
    out.val <- out.val[order(s_ord)]
  }

  return(out.val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_prod_legacy <- function(val, by = NULL, na.rm = TRUE, cumulative = FALSE){
  if(na.rm){

  }

  lgk <- !duplicated(by)
  if(length(which(lgk)) < 2){
    if(cumulative){
      cum.val <- cumprod(val)
    }else{
      cum.val <- prod(val, na.rm)
    }

  }else{
    by <- match(by, by[!duplicated(by)])

    if(!na.rm){
      NA_by <- by[is.na(val)]
    }
    val[is.na(val)] <- 0

    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]

    rp <- rle(by)
    cum.val <- cumprod(val)
    lgk <- !duplicated(by, fromLast = TRUE)
    max.val <- cum.val[lgk]
    by <- by[lgk]

    lag_pos <- 1:(length(max.val) - 1)
    prv_max.val <- c(1, max.val[lag_pos])

    if(cumulative){
      out.val <- cum.val / rep(prv_max.val, rp$lengths)
    }else{
      out.val <- max.val / c(1, max.val[lag_pos])
      out.val <- rep(out.val, rp$lengths)
    }

    if(!na.rm){
      out.val[rep(rp$values, rp$lengths) %in% NA_by] <- NA
    }

    out.val <- out.val[order(s_ord)]
  }

  return(out.val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_cummin_legacy <- function(val, by = NULL, na.rm = TRUE){
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
    s_ord <- seq_len(length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]
  }

  indx <- which(!is.na(val))
  if(length(indx) == 0){
    return(rep(NA, length(val)))
  }
  RNG <- range(val[indx])
  if(RNG[[1]] == RNG[[2]]){
    return(rep(RNG[[1]], length(val)))
  }

  faC <- as.integer(log10(RNG[[2]] - RNG[[1]])) + 1L
  faC <- as.integer(10L ^ faC)

  if(na.rm){
    val[is.na(val)] <- 0L
  }

  by <- ((max(by, na.rm = TRUE) + 1L) - by) * faC
  val <- -(by + val)
  val <- abs(cummax(val)) - by
  if(!null.by){
    val <- val[order(s_ord)]
  }

  return(val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_cummax_legacy <- function(val, by = NULL, na.rm = FALSE){
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
    s_ord <- seq_len(length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]
  }

  indx <- which(!is.na(val))
  if(length(indx) == 0){
    return(rep(NA, length(val)))
  }
  RNG <- range(val[indx])
  if(RNG[[1]] == RNG[[2]]){
    return(rep(RNG[[1]], length(val)))
  }

  if(na.rm){
    val[is.na(val)] <- 0L
  }

  faC <- as.integer(log10(RNG[[2]] - RNG[[1]])) + 1L
  faC <- as.integer(10L ^ faC)


  by <- by * faC
  val <- by + val
  val <- cummax(val) - by
  if(!null.by){
    val <- val[order(s_ord)]
  }

  return(val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_cumsum_legacy <- function(val, by = NULL, na.rm = TRUE){
  bys_sum_legacy(val = val, by = by, na.rm = na.rm, cumulative = TRUE)
}

#' @rdname bys_funcs_legacy
#' @export
bys_cumprod_legacy <- function(val, by = NULL, na.rm = TRUE){
  bys_prod_legacy(val = val, by = by, na.rm = na.rm, cumulative = TRUE)
}

#' @rdname bys_funcs_legacy
#' @export
bys_lag_legacy <- function(val, by = NULL, n = 1){
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
    s_ord <- seq_len(length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]
  }

  v.pos <- length(val) - n
  if(n > length(val)){
    lag.pos <- rep(NA, length(val))
  }else{
    lag.pos <- 1:(length(val) - n)
  }

  lag.pos <- c(rep(NA, n), lag.pos)

  l.by <- by[lag.pos]
  val <- val[lag.pos]
  val[by != l.by] <- NA

  if(!null.by){
    val <- val[order(s_ord)]
  }

  return(val)
}

#' @rdname bys_funcs_legacy
#' @export
bys_lead_legacy <- function(val, by = NULL, n = 1){
  null.by <- is.null(by)
  if(null.by){
    by <- rep(1L, length(val))
    s_ord <- seq_len(length(val))
  }else{
    by <- match(by, by[!duplicated(by)])
    s_ord <- order(by, decreasing = FALSE, na.last = TRUE)

    by <- by[s_ord]
    val <- val[s_ord]
  }

  v.pos <- length(val) - n
  if(n > length(val)){
    lead.pos <- rep(NA, length(val))
  }else{
    lead.pos <- (n + 1):length(val)
  }

  lead.pos <- c(lead.pos, rep(NA, n))

  l.by <- by[lead.pos]
  val <- val[lead.pos]
  val[by != l.by] <- NA

  if(!null.by){
    val <- val[order(s_ord)]
  }

  return(val)
}
