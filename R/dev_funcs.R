d_nodes <- function(x){
  nodes <- data.frame(id = x@sn, label = x@sn)
  edges <- lapply(x@wind_id, function(w){
    data.frame(from = x@sn[x@sn != w & !is.na(w)], to = w[x@sn != w & !is.na(w)])
  })
  edges <- do.call("rbind", edges)
  row.names(edges) <- NULL
  edges <- edges[!duplicated(edges),]

  nodes$x <- as.numeric(x@options$date)
  nodes$x <- (nodes$x - min(nodes$x))/max(nodes$x - min(nodes$x)) * 1000
  list(nodes = nodes, edges = edges)
}

merge_ids.epid <- function(id1, id2, tie_sort, ...){
  mp1 <- list(seq_len(length(id1)),
              id1@sn)
  mp2 <- list(seq_len(length(id2)),
              id2@sn)

  id1@sn <- mp1[[1]]
  id1@wind_id[[1]] <- mp1[[1]][match(id1@wind_id[[1]], mp1[[2]])]
  id1@.Data <- mp1[[1]][match(id1@.Data, mp1[[2]])]

  id2@sn <- mp2[[1]]
  id2@wind_id[[1]] <- mp2[[1]][match(id2@wind_id[[1]], mp2[[2]])]
  id2@.Data <- mp2[[1]][match(id2@.Data, mp2[[2]])]

  pr_match <- !(!duplicated(id1@.Data, fromLast = TRUE) & !duplicated(id1@.Data, fromLast = FALSE))
  if(is.null(tie_sort)){
    ord <- order(id2@.Data, -pr_match, id1@iteration, id1@sn)
  }else{
    ord <- order(id2@.Data, -pr_match, id1@iteration, tie_sort, id1@sn)
  }

  repo <- list(pr_match = pr_match,
               id1 = id1, id2 = id2)

  repo <- lapply(repo, function(x) x[ord])
  repo$tr_pr_match <- repo$pr_match[match(repo$id2, repo$id2)]
  repo$tr_id1 <- repo$id1[match(repo$id2, repo$id2)]
  repo$new_id <- repo$id1
  lgk <- repo$tr_pr_match & !repo$pr_match & !duplicated(repo$id1)
  repo$new_id@wind_id[[1]][lgk] <- repo$tr_id1@wind_id[[1]][lgk]
  lgk <- repo$tr_pr_match & !repo$pr_match
  repo$new_id@.Data[lgk] <- repo$tr_id1@.Data[lgk]
  repo$new_id@pid_cri[lgk] <- max(repo$new_id@pid_cri + 1L)
  repo$new_id@iteration[lgk] <- max(repo$new_id@iteration + 1L)

  repo$new_id@pid_dataset <- NA_character_
  repo <- repo$new_id
  repo <- repo[order(repo@sn)]

  repo@sn <- mp1[[2]][match(repo@sn, mp1[[1]])]
  repo@wind_id[[1]] <- mp1[[2]][match(repo@wind_id[[1]], mp1[[1]])]
  repo@.Data <- mp1[[2]][match(repo@.Data, mp1[[1]])]
  return(repo)
}

# To use 'search_and_unpack', every element in 'l' must be named
# and the last element at the bottom of the recursion
# must match `tgt_nm_regx`
search_and_unpack <- function(l, tgt_nm_regx = "^mf\\."){
  lgk <- unlist(lapply(l, names), use.names = FALSE)
  lgk <- any(grepl(tgt_nm_regx, lgk))
  if(isTRUE(lgk)){
    unlist(l, recursive = FALSE)
  }else{
    lapply(l, search_and_unpack)
  }
}

rbind_wf_matrix <- function(m1, m2){
  if(ncol(m2) > ncol(m1)){
    ncol_diff <- ncol(m2) - ncol(m1)
    m1 <- cbind(m1, matrix(rep(NA_real_, nrow(m1) * ncol_diff), ncol = ncol_diff))
  }else if(ncol(m1) > ncol(m2)){
    ncol_diff <- ncol(m1) - ncol(m2)
    m2 <- cbind(m2, matrix(rep(NA_real_, nrow(m2) * ncol_diff), ncol = ncol_diff))
  }
  rbind(m1, m2)
}

roll_sum <- function(val, by = 1000000){
  by <- abs(by)
  p <- 1:length(val)
  roll_seq <- function(pos){
    if(pos >= by) {
      pos <- ((pos-(by-1)):pos)
    }else{
      pos <- (1:pos)
    }
    sum(val[pos])
  }
  unlist(lapply(p, roll_seq))
}

make_ids_v2 <- function(x_pos, y_pos, id_length = max(x_pos, y_pos), ref_ord = NULL){
  repo <- seq_len(length(x_pos))
  repo <- list(
    val = c(x_pos, y_pos),
    # indx = c(repo, repo + max(repo)),
    row.n = rep(repo, 2)
  )

  s_ord <- order(repo$val)
  repo <- lapply(repo, function(x) x[s_ord])

  refs <- make_refs_v2(x_pos = repo$val, y_pos = repo$row.n, ref_ord = repo$row.n)
  col.n <- ncol(refs)

  x = rep(as.integer(refs[,1]), col.n-1)
  y = as.integer(refs[,2:col.n])
  lgk <- is.na(x) & is.na(y)
  x  <- x[!lgk]
  y  <- y[!lgk]

  y[is.na(y)] <-
    x[is.na(x)] <- -1
  g <- merge_ids(x, y, overwrite = TRUE)

  repo <- list(
    x_pos = x_pos,
    y_pos = y_pos,
    group_id = rep(NA, length(x_pos))
  )

  repo$group_id[x[x>0]] <- g[x>0]
  repo$group_id[y[y>0]] <- g[y>0]
  repo$group_id <- repo$x_pos[repo$group_id]
  repo$group_id
}

make_ids_v3 <- function(x_pos, y_pos, id_length = max(x_pos, y_pos)){
  link_id <- make_refs_v2(x_pos, y_pos, id_length = max(x_pos, y_pos))

  # x_pos <- c(x_pos, seq_len(id_length))
  # y_pos <- c(y_pos, seq_len(id_length))
  # browser()

  pts <- c(x_pos, y_pos)
  pts <- pts[!duplicated(pts)]

  x = c(x_pos, pts)
  y = c(y_pos, pts)
  x2 <- c(x, y)
  y2 <- c(y, x)

  y1 <- merge_ids(x2, y2, overwrite = TRUE)
  y2 <- merge_ids(y1, x2, overwrite = TRUE)

  group_id <- sn <- seq_len(id_length)
  # group_id[c(x_pos, y_pos)] <- c(rfs, rfs)
  group_id[c(y_pos, x_pos, x_pos, y_pos)] <- c(rfs, rfs)
  ids <- list(sn = sn,
              linked = NULL,
              link_id = link_id,
              group_id = group_id)

  ids$linked <- duplicated(ids$group_id, fromLast = TRUE) | duplicated(ids$group_id, fromLast = FALSE)
  rm(list = ls()[ls() != "ids"])
  ids
}

rle3 <- function(x) {
  ntot <- length(x)
  x[TRUE] <- x[order(x)]
  tot <- tots(x)
  i <- c(TRUE, x[-1] != x[seq_len(ntot)-1])
  list(values = x[i], lengths = tot)
}

datasets <- function(by, val, sep = ","){
  datasets <- split(val, by)
  func <- function(x) paste0(sort(x[!duplicated(x)]), collapse = sep)
  datasets <- lapply(datasets, func)
  #datasets <- as.character(datasets)[match(by, names(datasets))]
  datasets <- unlist(datasets[match(by, names(datasets))], use.names = F)
  datasets
}

combns <- function(x, m, FUN = NULL, simplify = TRUE, ...){
  funx <- function(v){
    v <- v[!duplicated(v)]
    lapply(seq_len(length(v)), function(i){
      shuffle <- c(0:(i-1), i+1, i, (i+2):length(v))
      shuffle <- shuffle[shuffle %in% 1:length(v)]
      shuffle <- shuffle[!duplicated(shuffle)]
      v[shuffle]
    })
  }

  pos <- seq_len(length(x))
  all_pos <- lapply(m, function(i){
    utils::combn(pos, m = i, simplify = F, FUN = funx)
  })
  all_pos <- unlist(unlist(all_pos, recursive = F), recursive = F)
  all_pos <- lapply(all_pos, function(y){x[y]})

  if(inherits(FUN, "NULL")){
    func <- function(x, ...) x
  }else{
    func <- function(x, ...) FUN(x, ...)
  }
  all_pos <- sapply(all_pos, func, simplify = simplify)
  return(all_pos)
}

prep_lengths <- function(
    length, overlap_methods, int,episode_unit, bi_direction,
    #from_last,
    include_index_period = F){
  length <- length
  if(!inherits(length, "list")){
    length <- list(length)
  }
  if(!inherits(overlap_methods, "list")){
    overlap_methods <- list(overlap_methods)
  }
  if(length(overlap_methods) == 1){
    overlap_methods <- rep(overlap_methods, length(length))
  }

  length <- sep_bdr_nl(length)
  r <- rle(names(length))
  overlap_methods <- rep(overlap_methods[as.numeric(r$values)], r$lengths)

  if(any(bi_direction == T)){
    is_bdr <- names(length) %in% r$values[r$lengths == 2]
    n_length  <- length[!is_bdr]
    if(length(n_length) > 0){
      n_length <- lapply(n_length, function(x){
        x[bi_direction] <- invert_number_line(x)[bi_direction]
        return(x)
      })
      length <- c(n_length, length)
      overlap_methods <- c(overlap_methods[!is_bdr], overlap_methods)
    }
  }

  overlap_methods <- lapply(overlap_methods, function(x){
    if(length(x)==1){
      x <- rep(x, length(int))
    }else{
      x
    }
  })

  list(
    lengths = length,
    method = overlap_methods
  )
}

f_rbind <- function(..., deparse.level = 1){
  dfs <- list(...)
  dfs_nms <- unlist(lapply(dfs, names), use.names = FALSE)
  dfs_nms <- dfs_nms[!duplicated(dfs_nms)]
  dfs <- lapply(dfs, function(x){
    for (i in dfs_nms) {
      if(!i %in% names(x) & nrow(x) > 0){
        x[[i]] <- NA
      }else if (!i %in% names(x) & nrow(x) == 0){
        x[[i]] <- logical()
      }
    }
    return(x)
  })
  t.rbind <- function(..., x = deparse.level) rbind(..., deparse.level = x)
  dfs <- do.call("t.rbind", dfs)
  return(dfs)
}

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

combi_v2 <- function(...){
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
  }

  x_cd <- match.alt(x[[1]])
  for (j in seq_len(length(x))[-1]){
    x_cd <- data.table::rleid(x_cd, match.alt(x[[j]]))
  }
  return(x_cd)
}

combi_v3 <- function(..., ordered = FALSE){
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
    x <- as.integer(data.table::frankv(x1))
    if(ordered){
      x <- match(x, x[!duplicated(x)])
    }
    return(x)
  }
}

make_pairs_batched_wf_episodes <- function(
    x, index_record, strata, assign_ord,
    ignore_same_source, data_source, lgk){

  a_indx <- which(lgk)
  b_indx <- which(!lgk)
  a <- make_pairs_batched(
    strata = strata[a_indx],
    x = x[a_indx],
    index_record = index_record[a_indx],
    assign_ord = assign_ord[a_indx],
    include_repeat = TRUE,
    look_back = TRUE
  )

  b <- make_pairs_batched(
    strata = strata[b_indx],
    x = x[b_indx],
    index_record = index_record[b_indx],
    assign_ord = assign_ord[b_indx],
    include_repeat = TRUE,
    look_back = FALSE
  )

  a$x_pos <- c(a$x_pos, b$x_pos)
  a$y_pos <- c(a$y_pos, b$y_pos)
  a$x_val <- c(a$x_val, b$x_val)
  a$y_val <- c(a$y_val, b$y_val)
  a$index_ord <- c(a$index_ord, b$index_ord)
  b <- NULL

  return(a)
}

make_sets_v2 <- function (x, r, strata = NULL, permutations_allowed = TRUE, repeats_allowed = TRUE){
  if (!is.null(strata)) {
    strata <- match(strata, strata)
    pos <- seq_len(length(x))
    s_ord <- order(strata)
    strata <- strata[s_ord]
    pos <- pos[s_ord]
    rl <- rle(strata)
    n <- max(rl$lengths)
  }
  else {
    n <- length(x)
  }
  repo <- sets(n = n, r = r, permutations_allowed = permutations_allowed,
               repeats_allowed = repeats_allowed)
  if (!is.null(strata)) {
    sn <- seq_len(length(x))
    rl$start_sn <- sn[!duplicated(strata)]
    unq_lens <- rl$lengths[!duplicated(rl$lengths)]
    repo <- lapply(unq_lens, function(x) {
      exc_indx <- which(repo > x)
      exc_indx <- exc_indx%%nrow(repo)
      exc_indx[exc_indx == 0] <- nrow(repo)
      x.repo <- repo[!seq_len(nrow(repo)) %in% exc_indx,
      ]
      x.repo
    })
    repo <- repo[match(rl$lengths, unq_lens)]
    reps <- (unq_lens^2)
    reps <- reps[match(rl$lengths, unq_lens)]
    repo <- do.call("rbind", repo)
    start_sn <- rep(rep(rl$start_sn, reps), r)
    repo <- repo + (start_sn - 1)
    repo <- pos[repo]
    repo <- matrix(repo, ncol = r)
  }
  repo <- split(repo, rep(seq_len(r), rep(nrow(repo), r)))
  repo <- c(repo, lapply(repo, function(i) x[i]))
  names(repo) <- paste0("x", rep(1:r, 2), rep(c("_pos", "_val"),
                                              rep(r, 2)))
  rm(list = ls()[ls() != "repo"])
  return(repo)
}

make_pairs_batched_old <- function(
    x,
    index_record = rep(TRUE, length(x)),
    strata = index_record,
    assign_ord = order(x),
    ignore_same_source = FALSE,
    data_source = NULL,
    look_back = FALSE,
    include_repeat = FALSE){

  tmp <- list(sn = order(strata, assign_ord))
  tmp$pr_sn <- seq_len(length(tmp[[1]]))
  tmp$strata <- strata[tmp$sn]
  lgk <- which(!duplicated(tmp$strata, fromLast = FALSE))
  rrr <- rle(tmp$strata)
  class(rrr) <- NULL

  tmp$strata_tot <- rep(rrr$lengths, rrr$lengths)
  tmp$rec_ord <- sequence(rrr$lengths)
  if(isFALSE(look_back)){
    tmp$strata_a_ord <- tmp$rec_ord
    tmp$strata_a_indx <- tmp$pr_sn
  }else{
    tmp$strata_a_ord <- rep(1, length(tmp[[1]]))
    tmp$strata_a_indx <- rep(lgk, rrr$lengths)
  }
  if(isFALSE(include_repeat)){
    tmp$strata_tot <- tmp$strata_tot - 1
  }

  tmp$reference_record <- index_record[tmp$sn]
  tmp_indexes <- lapply(tmp, function(x){
    x[tmp$reference_record == 1]
  })
  tmp_indexes$strata_tot_new <- tmp_indexes$strata_tot - tmp_indexes$strata_a_ord + 1

  rrr <- rle(tmp_indexes$strata)
  tmp_indexes$index_ord <- sequence(rrr$lengths)

  tmp2 <- list()
  tmp2$x_ord <- sequence(tmp_indexes$strata_tot_new)
  if(isFALSE(include_repeat)){
    if(isTRUE(look_back)){
      tmp2$y_ord <- rep(tmp_indexes$rec_ord, tmp_indexes$strata_tot_new)
      tmp2$y_tot <- rep(tmp_indexes$strata_tot_new, tmp_indexes$strata_tot_new)
      tmp2$lgk <- tmp2$x_ord == tmp2$y_ord
      tmp2$x_ord[tmp2$lgk] <- tmp2$y_tot[tmp2$lgk] + 1
    }else{
      tmp_indexes <- lapply(tmp_indexes, function(x){
        x[!tmp_indexes$strata_tot_new == 0]
      })
      tmp_indexes$strata_a_indx <- tmp_indexes$strata_a_indx + 1
    }
  }
  tmp2$x_pos <- tmp2$x_ord + rep(tmp_indexes$strata_a_indx, tmp_indexes$strata_tot_new) - 1
  tmp2$y_pos <- rep(tmp_indexes$pr_sn, tmp_indexes$strata_tot_new)
  tmp2$index_ord <- rep(tmp_indexes$index_ord, tmp_indexes$strata_tot_new)

  tmp2 <- tmp2[c("x_pos", "y_pos", "index_ord")]

  tmp2$x_pos <- tmp$sn[tmp2$x_pos]
  tmp2$y_pos <- tmp$sn[tmp2$y_pos]

  if(!is.null(data_source) & isTRUE(ignore_same_source)){
    lgk <- which(data_source[tmp2$x_pos] != data_source[tmp2$y_pos])
    tmp2 <- lapply(tmp2, function(x) x[lgk])
  }

  tmp2$x_val <- x[tmp2$x_pos]
  tmp2$y_val <- x[tmp2$y_pos]
  if(!is.null(strata)){
    tmp2$strata <- strata[tmp2$y_pos]
  }

  rm(list = ls()[ls() != "tmp2"])
  return(tmp2)
}
