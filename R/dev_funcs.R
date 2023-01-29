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

id_total <- function(x){
  rr <- rle(sort(x))
  x <- rr$lengths[match(x, rr$values)]
  x
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

unpack <- function(xx, depth = 1){
  classes <- unlist(lapply(xx, class))
  lgk <- classes == "list"
  xx.1 <- xx[!lgk]
  xx.2 <- unlist(xx[lgk], recursive = FALSE)
  if(length(xx.1) > 0){
    names(xx.1) <- paste0("var_", depth, ".", seq_len(length(xx.1)))
  }
  if(length(xx.2) > 0){
    names(xx.2) <- paste0("var_", depth + 1, ".", seq_len(length(xx.2)))
  }
  xx <- c(xx.1, xx.2)
  classes <- unlist(lapply(xx, class))
  lgk <- classes == "list"
  if(any(lgk)){
    # rm(list = ls()[!ls() %in% c("xx", "depth", "lgk")])
    unpack(xx, depth = depth + 1)
  }else{
    # rm(list = ls()[!ls() %in% c("xx", "depth", "lgk")])
    xx
  }
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
