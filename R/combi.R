#' @name combi
#' @title Vector combinations
#' @description Numeric codes for unique combination of vectors.
#' @param ... \code{[atomic]}.
#' @param ordered \code{[logical]}. If \code{TRUE}, the output is sequential.
#' @return \code{numeric}
#'
#' @examples
#' x <- c("A", "B", "A", "C", "B", "B")
#' y <- c("X", "X", "Z", "Z", "X", "Z")
#' combi(x, y)
#' @export
combi <- function(..., ordered = FALSE){
  # ... must be vectors
  x <- list(...)
  is_list <- unlist(lapply(x, function(x){
    inherits(x, "list")
  }), use.names = FALSE)
  x <- c(unlist(x[is_list], recursive = FALSE), x[!is_list])
  # Validations
  err_txt <- unlist(lapply(seq_len(length(x)), function(i){
    x <- err_atomic_vectors(x[[i]], paste0("vector ", i))
    x[x == FALSE] <- NA_character_
    x
  }), use.names = FALSE)
  err_txt <- err_txt[!is.na(err_txt)]
  if(length(err_txt) > 0) stop(err_txt, call. = FALSE)

  vec_lens <- unlist(lapply(x, length), use.names = FALSE)
  dd_err <- vec_lens[!duplicated(vec_lens)]
  if(!(length(dd_err) == 1 | (length(dd_err) == 2 & 1 %in% dd_err))){
    err_txt <- paste0("Length of each vector in `...` must be the same or equal to 1:\n",
                      paste0("X - Length of vector ",
                             seq_len(length(vec_lens)),
                             " is ", vec_lens, ".",
                             collapse = "\n"))
    stop(err_txt, call. = FALSE)
  }

  x[vec_lens == 1] <- NULL
  if(length(x) == 0){
    return(rep(1, max(dd_err)))
  }else{
    x <- as.integer(data.table::frankv(x))
    if(ordered){
      x <- match(x, x[!duplicated(x)])
    }
    return(x)
  }
}
