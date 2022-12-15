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
