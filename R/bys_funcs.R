#' @name bys_funcs
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
#' @aliases bys_funcs
#' @examples
#' x <- data.frame(
#'   group = c(2, 2, 1, 2, 1, 1, 1, 2, 1, 1),
#'   value = c(13, 14, 20, 9, 2, 1, 8, 18, 3, 17))
#'
#' bys_count(x$group)
#' bys_position(x$value, by = x$group, from_last = TRUE)
#' bys_rank(by = x$group, val = x$value, from_last = TRUE)
#' bys_val(x$value, by = x$group, val = x$value, from_last = TRUE)
#' bys_nval(x$value, by = x$group, val = x$value, from_last = TRUE, n = 2)
#' bys_min(by = x$group, val = x$value)
#' bys_max(by = x$group, val = x$value)
#' bys_sum(by = x$group, val = x$value)
#' bys_prod(by = x$group, val = x$value)
#' bys_cummin(by = x$group, val = x$value)
#' bys_cummax(by = x$group, val = x$value)
#' bys_cumsum(by = x$group, val = x$value)
#' bys_cumprod(by = x$group, val = x$value)
#' bys_lag(by = x$group, val = x$value)
#' bys_lead(by = x$group, val = x$value)

#' @rdname bys_funcs
#' @importFrom data.table ":="
#' @export
bys_count <- function(by, unique.var = NULL){
  if(!is.atomic(by)) stop("`by` must be an `atomic` vector!")
  if(length(by) == 0) stop("`by` has a length of 0!")

  if(!is.null(unique.var)){
    by <- data.table::data.table(V1 = by, sn = seq_len(length(by)), unique.var = unique.var)
    data.table::setorder(by, V1, unique.var)
    by[, N :=  length(unique(unique.var)), by = V1]
  }else{
    by <- data.table::data.table(V1 = by, sn = seq_len(length(by)))
    data.table::setorder(by, V1)
    by[, N :=  .N, by = V1]
  }
  data.table::setorder(by, sn)
  return(by$N)
}

#' @rdname bys_funcs
#' @export
bys_rank <- function(..., by = NULL, from_last = FALSE){
  dfr <- bys(..., val = 1, by = by, from_last = from_last)
  dfr[, rank := seq_len(.N), by = by]
  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }
  return(dfr$rank)
}

#' @rdname bys_funcs
#' @param ordered If \code{TRUE}, the output is sequential.
#' @export
bys_position <- function(..., by = NULL, from_last = FALSE, ordered = TRUE){
  dfr <- bys(..., val = 1, by = by, from_last = from_last)
  vrs <- names(dfr)
  dfr[, positon := data.table::rleidv(dfr, cols = vrs[vrs != 'sn'])]
  if(!is.null(by) & ordered){
    dfr[, positon := (positon - min(positon)) + 1, by = by]
  }
  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }
  return(dfr$positon)
}

#' @rdname bys_funcs
#' @export
bys_val <- function(..., val, by = NULL, from_last = FALSE, na.last = TRUE){
  dfr <- bys(..., val = val, by = by, from_last = from_last, na.last = na.last)
  dfr[, val := val[1], by = by]
  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }
  return(dfr$val)
}

#' @rdname bys_funcs
#' @export
bys_nval <- function(..., val, by = NULL, from_last = FALSE, n = 1, nmax = FALSE, na.last = TRUE){
  dfr <- bys(..., val = val, by = by, from_last = from_last, na.last = na.last)
  dfr[, val := val[ifelse(nmax & .N < n, .N, n)], by = by]
  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }
  return(dfr$val)
}

#' @rdname bys_funcs
#' @param na.rm If \code{TRUE}, remove \code{NA} values
#' @export
bys_min <- function(val, by = NULL, na.rm = TRUE){
  return(bys_val(val, by = by, from_last = FALSE, val = val, na.last = na.rm))
}

#' @rdname bys_funcs
#' @export
bys_max <- function(val, by = NULL, na.rm = TRUE){
  return(bys_val(val, by = by, from_last = TRUE, val = val, na.last = na.rm))
}

#' @rdname bys_funcs
#' @export
bys_sum <- function(val, by = NULL, na.rm = TRUE){
  bys_func(val = val, by = by, func = function(x) sum(x, na.rm = na.rm))
}

#' @rdname bys_funcs
#' @export
bys_prod <- function(val, by = NULL, na.rm = TRUE){
  bys_func(val = val, by = by, func = function(x) prod(x, na.rm = na.rm))
}

#' @rdname bys_funcs
#' @export
bys_cummin <- function(val, by = NULL){
  bys_func(val = val, by = by, func = cummin)
}

#' @rdname bys_funcs
#' @export
bys_cummax <- function(val, by = NULL){
  bys_func(val = val, by = by, func = cummax)
}

#' @rdname bys_funcs
#' @export
bys_cumsum <- function(val, by = NULL){
  bys_func(val = val, by = by, func = cumsum)
}

#' @rdname bys_funcs
#' @export
bys_cumprod <- function(val, by = NULL){
  bys_func(val = val, by = by, func = cumprod)
}

#' @rdname bys_funcs
#' @export
bys_lag <- function(..., val, by = NULL, n = 1, from_last = FALSE){
  return(
    bys_shift(..., val = val, by = by, from_last = from_last, n = -abs(n))
  )
}

#' @rdname bys_funcs
#' @export
bys_lead <- function(..., val, by = NULL, n = 1, from_last = FALSE){
  return(
    bys_shift(..., val = val, by = by, from_last = from_last, n = abs(n))
  )
}

#' @rdname bys_funcs
# @export
bys <- function(..., val, by = NULL, from_last = FALSE, na.last = TRUE){
  if(!is.logical(from_last)) stop("`from_last` must be `TRUE` or `FALSE`!")
  nmax <- length(list(...))
  if(nmax > 0){
    dfr <- data.table::data.table(...)
    names(dfr) <- paste0('X', seq_len(length(dfr)))
    vrs <- names(dfr)
    dfr[, sn := seq_len(.N)]
    if(!is.null(by)){
      dfr[, by := by]
    }
    dfr[, val := val]

    vrs <- c('by', vrs, 'sn')
    vrs <- vrs[vrs %in% names(dfr)]
    ords <- rep(1, length(vrs))
    ords <- ifelse(from_last & !vrs %in% c('sn', 'by') , -ords, ords)
    data.table::setorderv(dfr, vrs, ords, na.last = na.last)
  }else{
    if(!is.null(by)){
      dfr <- data.table::data.table(by = by, val = val)
      vrs <- 'by'
    }else{
      stop('X - At least one of `...` or `by` must be provided.', call. = FALSE)
    }
  }
  return(dfr)
}

#' @rdname bys_funcs
#' @export
bys_shift <- function(..., val, by = NULL, n = 1, from_last = FALSE){
  dfr <- bys(..., val = val, by = by, from_last = from_last)

  if(abs(n) > nrow(dfr)){
    tgt.pos <- rep(NA, nrow(dfr))
  }else if (n > 0){
    tgt.pos <- c((n + 1):nrow(dfr), rep(NA, abs(n)))
  }else if (n < 0){
    tgt.pos <- c(rep(NA, abs(n)),seq_len((nrow(dfr) - abs(n))))
  }

  dfr[, val2 := val[tgt.pos]]
  if(!is.null(by)){
    dfr[, by2 := by[tgt.pos]]
    dfr[by != by2, val2 := NA]
  }

  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }

  return(dfr$val2)
}

#' @rdname bys_funcs
#' @export
bys_func <- function(..., val, by = NULL, from_last = FALSE, func = sum){
  dfr <- bys(..., val = val, by = by, from_last = from_last)
  dfr[, val := func(val), by = by]
  if('sn' %in% names(dfr)){
    data.table::setorder(dfr, sn)
  }
  return(dfr$val)
}
