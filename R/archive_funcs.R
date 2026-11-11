combi_archive <- function(...){
  # ... must be vectors
  combi <- list(...)
  # Validations
  err_txt <- unlist(lapply(seq_len(length(combi)), function(i){
    x <- diyar::err_atomic_vectors(combi[[i]], paste0("vector ", i))
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

  combi_cd <- sapply(seq_len(length(combi)), function(i){
    x <- combi[[i]]
    if(length(x) == 1){
      x <- rep(1L, max(vec_lens))
    }else{
      x <- match(x, x[!duplicated(x)])
    }
    x
  })
  nrows <- nrow(combi_cd)
  ncols <- ncol(combi_cd)
  combi_cd <- as.integer(combi_cd)
  rows_pos <- rep(seq_len(nrows), ncols)
  combi_cd <- split(combi_cd, rows_pos)
  combi_cd <- match(combi_cd, combi_cd)
  return(combi_cd)
}
