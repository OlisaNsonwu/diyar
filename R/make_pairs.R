#' @name make_pairs
#' @aliases make_pairs
#' @title Record-pair combinations.
#'
#' @description
#' Create record-pair combinations of a vector's elements.
#'
#' @param x \code{[atomic]}.
#' @param strata Subsets of \code{x}. A blocking attribute limiting the combinations created.
#' @param repeats_allowed \code{[logical]} If \code{TRUE}, repetitions are included.
#' @param permutations_allowed \code{[logical]} If \code{TRUE}, permutations are included.
#' @param ... Arguments passed to \code{\link{make_pairs}}.
#' @param data_source \code{[character]}. Data source identifier. Limits to record-pairs to those from different sources.
#' @return A \code{list} of indexes and values of record-pair combinations.
#'
#' @seealso \code{\link{eval_sub_criteria}}
#'
#' @examples
#' make_pairs(month.abb[1:4])
#' make_pairs(month.abb[1:4], strata = c(1, 1, 2, 2))
#'
#' @export
make_pairs <- function(x, strata = NULL, repeats_allowed = TRUE, permutations_allowed = FALSE){
  # Validations
  errs <- err_make_pairs_1(x = x, strata = strata,
                           repeats_allowed = repeats_allowed,
                           permutations_allowed = permutations_allowed)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(!is.null(strata)){
    if(length(strata[!duplicated(strata)]) == 1){
      strata <- NULL
    }
  }
  pos <- seq_len(length(x))
  repo <- list()
  if(!is.null(strata)){
    strata <- match(strata, strata[!duplicated(strata)])
    s_ord <- order(strata)

    x <- x[s_ord]
    strata <- strata[s_ord]
    sn <- pos[s_ord]

    rl <- rle(strata)
  }else{
    rl <- list(lengths = length(x), values = 1)
    sn <- pos
  }

  ord <- sequence(rl$lengths)
  repo$x_pos <- sequence(ord)
  repo$y_pos <- rep(ord, ord)

  pts <- rl$lengths[!duplicated(rl$lengths)]
  pts_val <- unlist(lapply(pts, function(x){
    if(x == 1){
      x
    }else{
      permute_num(x)
    }
  }), use.names = FALSE)
  pts_val <- as.integer(pts_val)
  if(!is.null(strata)){
    lgk <- which(!duplicated(strata, fromLast = TRUE))
  }else{
    lgk <- length(x)
  }
  if(!is.null(strata)){
    repo$y_max_pos <- rep(pos[lgk], pts_val[match(rl$lengths, pts)])
    ref <- (repo$y_max_pos + 1L)
    repo$y_pos <- ref - repo$y_pos
    repo$x_pos <- ref - repo$x_pos
  }

  repo$x_val <- x[repo$x_pos]
  repo$y_val <- x[repo$y_pos]

  repo$x_pos <- sn[repo$x_pos]
  repo$y_pos <- sn[repo$y_pos]
  repo$y_max_pos <- NULL

  if(isTRUE(permutations_allowed)){
    lgk <- which(repo$x_pos != repo$y_pos)
    repo <- list(x_pos = c(repo$x_pos[lgk], repo$y_pos),
                 y_pos = c(repo$y_pos[lgk], repo$x_pos),
                 x_val = c(repo$x_val[lgk], repo$y_val),
                 y_val = c(repo$y_val[lgk], repo$x_val)
    )
  }

  if(isFALSE(repeats_allowed)){
    lgk <- which(repo$x_pos != repo$y_pos)
    repo <- lapply(repo, function(x) x[lgk])
  }

  s_ord <- order(repo$x_pos, repo$y_pos)
  repo <- lapply(repo, function(x) x[s_ord])

  rm(list = ls()[ls() != "repo"])
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

  r_sets <- split(seq_len(length(opt_lst$x)), data_source)
  repo <- make_pairs(seq_len(length(r_sets)),
                     permutations_allowed = opt_lst$repeats_allowed,
                     # repeats_allowed = opt_lst$permutations_allowed
                     repeats_allowed = FALSE
                     )

  repo <- lapply(seq_len(length(repo[[1]])), function(i){
    xi <- repo$x_pos[[i]]
    yi <- repo$y_pos[[i]]

    x_pos <- rep(r_sets[[xi]], length(r_sets[yi][[1]]), use.names = FALSE)
    y_pos <- rep(r_sets[[yi]], length(r_sets[xi][[1]]), use.names = FALSE)

    if(length(r_sets[[xi]]) < length(r_sets[[yi]])){
      x_pos <- sort(x_pos)
    }else{
      y_pos <- sort(y_pos)
    }

    list(x_pos = x_pos, y_pos = y_pos)
  })

  repo <- list(x_pos = unlist(lapply(repo, function(x) x$x_pos)),
               y_pos = unlist(lapply(repo, function(x) x$y_pos)))

  repo$x_val <- opt_lst$x[repo$x_pos]
  repo$y_val <- opt_lst$x[repo$y_pos]

  if(!is.null(opt_lst$strata)){
    lgk <- which(opt_lst$strata[repo$x_pos] == opt_lst$strata[repo$y_pos])
    repo <- lapply(repo, function(x) x[lgk])
  }

  return(repo)
}
