#' @name combi
#' @title Vector combinations
#' @description Generate numeric codes representing unique combinations of vectors.
#' @param ... \code{[atomic]} One or more vectors to be combined.
#' @param ordered \code{[logical]}. If \code{TRUE}, the codes are assigned sequentially in the order combinations are encountered.
#' @return \code{numeric}
#'
#' @details
#' Each unique combination of values across the supplied vectors is
#' assigned a unique numeric code. Records with the same values across all
#' are assigned the same code.
#'
#' @examples
#'
#' x <- c("A", "B", "A", "C", "B", "B")
#' y <- c("X", "X", "Z", "Z", "X", "Z")
#'
#' # Create a unique code for each x-y combination
#' data.frame(x, y, combi(x, y))
#'
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

#' @rdname combi
#' @export
combi_legacy <- function(...){
  # ... must be vectors
  combi <- list(...)
  is_list <- unlist(lapply(combi, function(x){
    inherits(x, "list")
  }), use.names = FALSE)
  combi <- c(unlist(combi[is_list], recursive = FALSE),
             combi[!is_list])
  # Validations
  err_txt <- unlist(lapply(seq_len(length(combi)), function(i){
    x <- err_atomic_vectors(combi[[i]], paste0("vector ", i))
    x[x == FALSE] <- NA_character_
    x
  }), use.names = FALSE)
  err_txt <- err_txt[!is.na(err_txt)]
  if(length(err_txt) > 0) stop(err_txt, call. = FALSE)

  vec_lens <- unlist(lapply(combi, length), use.names = FALSE)
  dd_err <- vec_lens[!duplicated(vec_lens)]
  if(!(length(dd_err) == 1 | (length(dd_err) == 2 & 1 %in% dd_err))){
    err_txt <- paste0("Length of each vector in `...` must be the same or equal to 1:\n",
                      paste0("X - Length of vector ",
                             seq_len(length(vec_lens)),
                             " is ", vec_lens, ".",
                             collapse = "\n"))
    stop(err_txt, call. = FALSE)
  }

  combi[vec_lens == 1] <- NULL
  if(length(combi) == 0){
    return(rep(1, max(dd_err)))
  }
  combi_cd <- match(combi[[1]], (combi[[1]])[!duplicated(combi[[1]])])
  for (j in seq_len(length(combi))[-1]){
    k <- match(combi[[j]], (combi[[j]])[!duplicated(combi[[j]])])

    # faC <- rep(as.integer(log10(max(k))), length(k))
    faC <- as.integer(log10(max(k)))
    # trail_lgk <- k %% 10 == 0
    faC <- faC + 1
    # faC[trail_lgk] <- (faC[trail_lgk] * 2)
    faC <- 10 ^ faC
    combi_cd <- combi_cd + (k / faC)
    combi_cd <- match(combi_cd, combi_cd[!duplicated(combi_cd)])
  }
  rm(list = ls()[ls() != "combi_cd"])
  return(combi_cd)
}
