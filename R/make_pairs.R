#' @name make_pairs
#' @aliases make_pairs
#' @title Combinations and permutations of record-sets.
#'
#' @param x \code{[atomic]}. Vector.
#' @param n \code{[integer]}. Size of the vector.
#' @param r \code{[integer]}. Number of elements in a set.
#' @param strata \code{[atomic]}. Subsets of \code{x}. Blocking attribute. Limits the creation of combinations or permutations to those from the same \code{strata}.
#' @param repeats_allowed \code{[logical]}. If \code{TRUE}, sets with repeat values are included.
#' @param permutations_allowed \code{[logical]}. If \code{TRUE}, permutations of the same set are included.
#' @param ... Arguments passed to \code{\link{make_pairs}}.
#' @param data_source \code{[character]}. Data source identifier. Limits the creation of combinations or permutations to those from a different \code{data_source}
#' @return A \code{list} of the vector's elements and corresponding indexes.
#'
#' @details
#' \bold{\code{set()}} - Create \code{r}-set combinations or permutations of \code{n} observations.
#'
#' \bold{\code{make_sets()}} - Create \code{r}-set combinations or permutations of vector \code{x}.
#'
#' \bold{\code{make_pairs()}} - Create \code{2}-set combinations or permutations of vector \code{x}.
#'
#' \bold{\code{make_pairs_wf_source()}} - Create \code{2}-set combinations or permutations of vector \code{x} that are from different sources (\code{data_source}).
#'
#' @seealso \code{\link{eval_sub_criteria}}
#'
#' @examples
#' # Generate 2-record sets from 4 observations
#' sets(4, 2)
#'   # ... Same but without repeats and permutations
#' sets(4, 2, repeats_allowed = FALSE, permutations_allowed = FALSE)
#'
#' # `make_sets()` includes both the values and
#' # their positions in the original vector
#' make_sets(month.abb[1:4], 2)
#' make_sets(month.abb[1:4], 3)
#'
#' # An optimised version for creating 2-sets (record pairs)
#' make_pairs(month.abb[1:4])
#' # Restrict record pairs to the same strata
#' make_pairs(month.abb[1:4], strata = c(1, 1, 2, 2))
#'
#' # Generate record pairs from different sources only
#' make_pairs_wf_source(month.abb[1:4], data_source = c(1, 1, 2, 2))
#'
#' @export
#' @rdname make_pairs
sets <- function(n, r, permutations_allowed = TRUE, repeats_allowed = TRUE){
  p <- n ^ r
  pairs <- sapply(seq_len(r), function(i){
    rev_i <- (r + 1) - i
    V3 <- rep(seq_len(n), rep(p/(n ^ i), n))
    V3 <- unlist(rep(list(V3), p/(n ^ rev_i)))
    V3
  })
  if(isFALSE(permutations_allowed)){
    ncols <- seq_len(ncol(pairs))[-1]
    for (i in ncols) {
      pairs <- pairs[!(pairs[,i] < pairs[,i-1]),]
    }
  }
  if(isFALSE(repeats_allowed)){
    ncols <- make_pairs(seq_len(ncol(pairs)),
                        permutations_allowed = FALSE,
                        repeats_allowed = FALSE)
    for (i in seq_len(length(ncols$x_pos))) {
      x_vr <- ncols$x_pos[i]
      y_vr <- ncols$y_pos[i]
      pairs <- pairs[which(pairs[,x_vr] != pairs[,y_vr]),]
    }
    rm(ncols)
  }
  return(pairs)
}

#' @export
#' @rdname make_pairs
make_sets <- function(x, r, strata = NULL, permutations_allowed = TRUE, repeats_allowed = TRUE){
  if(!is.null(strata)){
    strata <- match(strata, strata)
    pos <- seq_len(length(x))
    s_ord <- order(strata)
    strata <- strata[s_ord]
    pos <- pos[s_ord]
    rl <- rle(strata)
    n <- max(rl$lengths)
  }else{
    n <- length(x)
  }
  repo <- sets(n = n,
               r = r,
               permutations_allowed = permutations_allowed,
               repeats_allowed = repeats_allowed)
  if(!is.null(strata)){
    sn <- seq_len(length(x))
    rl$start_sn <- sn[!duplicated(strata)]
    repo <- lapply(rl$values, function(x){
      exc_indx <- which(repo > rl$lengths[rl$values == x])
      exc_indx <- exc_indx %% nrow(repo)
      exc_indx[exc_indx == 0] <- nrow(repo)
      x.repo <- repo[!seq_len(nrow(repo)) %in% exc_indx,]
      x.repo <- x.repo  + (rl$start_sn[rl$values == x] - 1)
      x.repo
    })
    repo <- do.call("rbind", repo)
    repo <- pos[repo]
    repo <- matrix(repo, ncol = r)
  }

  repo <- split(repo,
                rep(seq_len(r), rep(nrow(repo), r)))
  repo <- c(repo,
            lapply(repo, function(i) x[i]))
  names(repo) <-
    paste0("x", rep(1:r, 2), rep(c("_pos", "_val"), rep(r, 2)))
  return(repo)
}

#' @export
#' @rdname make_pairs
make_pairs <- function(x, strata = NULL, repeats_allowed = TRUE, permutations_allowed = FALSE){
  errs <- err_make_pairs_1(x = x, strata = strata,
                           repeats_allowed = repeats_allowed,
                           permutations_allowed = permutations_allowed)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(length(strata) %in% c(0, 1)){
    strata <- rep(TRUE, length(x))
  }

  repo <- make_pairs_batched(
    x = x, strata = strata,look_back = permutations_allowed,
    include_repeat = repeats_allowed, index_record = rep(TRUE, length(x)))

  return(repo)
}

#' @export
#' @rdname make_pairs
make_pairs_wf_source <- function(..., data_source = NULL){
  # Validations
  errs <- err_make_pairs_1(...)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(is.null(data_source) | length(data_source[!duplicated(data_source)]) < 2){
    return(make_pairs(...))
  }

  opt_lst <- list(...)
  def_list <- formals(make_pairs)
  named_pos <- which(names(opt_lst) %in% names(def_list))
  unamed_pos <- seq_len(length(opt_lst))[!seq_len(length(opt_lst)) %in% named_pos]
  unnamed_args <- names(def_list)[!names(def_list) %in% names(opt_lst)]
  unnamed_args <- unnamed_args[unamed_pos]
  names(opt_lst)[unamed_pos] <- unnamed_args

  opt_lst <- c(
    opt_lst,
    def_list[names(def_list)[!names(def_list) %in% names(opt_lst)]]
  )
  rm(def_list, unnamed_args, named_pos, unamed_pos)

  if(length(opt_lst$x) != length(data_source)){
    stop("`data_source` must be `NULL` or have the same lenght as `x`", call. = FALSE)
  }
  if(!length(opt_lst$strata) %in% c(0:1,length(opt_lst$x))){
    stop("`strata` must be `NULL` or have the same lenght as `x`", call. = FALSE)
  }
  if(length(opt_lst$strata) %in% c(0:1)){
    opt_lst$strata <- rep(1L, length(opt_lst$x))
  }

  lvls <- combi(data_source, opt_lst$strata, ordered = TRUE)
  lvls_strata <- opt_lst$strata[!duplicated(lvls)]
  r_sets <- split(seq_len(length(opt_lst$x)), lvls)
  tmp_x <- seq_len(length(r_sets))
  repo <- make_pairs(
    x  = tmp_x, strata = lvls_strata[tmp_x],
    permutations_allowed = opt_lst$permutations_allowed,
    repeats_allowed = FALSE)

  repo2 <- list()
  for(i in seq_len(length(repo[[1]]))){
    xi <- repo$x_pos[[i]]
    yi <- repo$y_pos[[i]]
    if(lvls_strata[xi] != lvls_strata[yi]) next

    x_pos <- rep(r_sets[[xi]], length(r_sets[yi][[1]]), use.names = FALSE)
    y_pos <- rep(r_sets[[yi]], rep(length(r_sets[xi][[1]]), length(r_sets[yi][[1]]))  , use.names = FALSE)

    repo2[[length(repo2) + 1]] <- list(x_pos = x_pos, y_pos = y_pos)
  }

  repo <- list(
    x_pos = unlist(lapply(repo2, function(x) x$x_pos)),
    y_pos = unlist(lapply(repo2, function(x) x$y_pos)))

  repo$x_val <- opt_lst$x[repo$x_pos]
  repo$y_val <- opt_lst$x[repo$y_pos]

  return(repo)
}

