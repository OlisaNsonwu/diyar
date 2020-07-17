# @title Convenience functions
#
# @description Convenience functions
#
# @param x x
# @aliases finite_check
#
finite_check <- function(x){
  e <- which(!is.finite(as.numeric(x)))
  if(length(e) %in% 1:10) {
    paste0("[",listr(fmt(e)),"]")
  }else if(length(e) > 10) {
    paste0("[",paste0(fmt(e[1:10]), collapse = ", "),", ...]")
  }else if(length(e) == 0)
    TRUE
}


missing_check <- function(x){
  e <- which(is.na(x))
  if(length(e) %in% 1:10) {
    paste0("[",listr(fmt(e)),"]")
  }else if(length(e) > 10) {
    paste0("[",paste0(fmt(e[1:10]), collapse = ", "),", ...]")
  }else if(length(e) == 0)
    TRUE
}

# @rdname finite_check
enq_vr <- function(x){
  x <- as.character(x)
  if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
  if(length(x)==0) x <- NULL
  x
}

# @rdname finite_check
fmt <- function(x) formatC(x, format="d", big.mark=",")

# @rdname finite_check
listr <- function(x, conj="and"){
  p <- x[length(x)]
  f <- x[1:(length(x)-1)]
  f <- paste(f, collapse = ", ")
  x <- ifelse(length(x) > 1, paste0(f, " ", conj, " ",  p), f)
  return(x)
}

# @rdname finite_check
duplicates_check <- function(x){
  pos <- 1:length(x)
  lgk <- duplicated(x)
  dups <- x[lgk]
  dups <- dups[!duplicated(dups)]

  lgk2 <- x %in% head(dups, 3)
  y <- x[lgk2]
  y_pos <- pos[lgk2]

  y <- unlist(lapply(split(y_pos, y), function(x){
    ifelse(length(x)>3,
           paste0("[",paste0(x[1:3], collapse = ", "),",..]"),
           paste0("[",paste0(x, collapse = ", "),"]"))
  }), use.names = F)


  if(length(dups)>3){
    paste0(paste0(y, collapse=", "), " ...")
    }else if (length(y)!=0) {
      listr(y)
    }else{
      TRUE
      }
}
# @rdname finite_check
logicals_check <- function(x){
  log_vals <-  lapply(x, function(y){
    is.logical(eval(parse(text=y), envir = parent.frame(3)))
  })

  log_vals <- x[as.logical(log_vals)==F]
  if(length(log_vals) ==0){
    TRUE
  }else{
    paste0(listr(paste0("'",log_vals,"'")), " must be either TRUE or FALSE")
  }
}

# @rdname finite_check
sep_bdr_nl <- function(x){
  nl_prp <- function(i){

    x <- x[[i]]
    if(!is.number_line(x)){
      x <- as.number_line(x)
      left_point(x) <- 0
    }

    x@start <- as.numeric(x@start)
    x <- reverse_number_line(x, "decreasing")
    crx <- x@start/abs(x@start) != diyar::right_point(x)/abs(diyar::right_point(x))
    crx[is.na(crx)] <- F

    nl_a <- x
    left_point(nl_a[crx]) <- 0

    lst <- list(nl_a)
    if(any(crx)){
      nl_b <- x
      right_point(nl_b[crx]) <- 0
      lst[[2]] <- nl_b
    }

    names(lst) <- rep(i, length(lst))
    lst
  }

  b <- lapply(1:length(x), nl_prp)
  txt <- paste0("c(",paste0("b[[",1:length(b),"]]", collapse = ", " ), ")")
  b <- eval(parse(text=txt))
  return(b)
}

progress_bar <- function(prop_complete, max_width){
  pct_l <- paste0(" Tracking episodes; ", fmt(prop_complete*100),"% complete.")
  status_width <- max_width - nchar(pct_l)
  bar_width <- as.integer(prop_complete*status_width)
  space_width <- status_width - bar_width

  status <-paste0(paste0(rep("-", bar_width), collapse = ""),
                  paste0(rep(" ", space_width), collapse = ""),
                  pct_l,
                  "\r")

  if(prop_complete==1){
    # cat(paste0("\033[0;32m",status,"\033[0m"))
    cat(status, "\r",sep="")
  }else{
    cat(status, "\r",sep="")
  }
}

datasets_xx <- function(by, val, sep = ","){
  #by_uniq <- by[!duplicated(by)]

  datasets <- split(by, val)

  datasets <- lapply(1:length(datasets), function(x){
    ifelse(by %in% datasets[[x]], names(datasets)[x], NA_character_)
  })

  ds <- rep("", length(by))
  for(i in 1:length(datasets)){
    ds <- ifelse(!is.na(datasets[[i]]),
                 ifelse(ds =="",
                        paste(ds, datasets[[i]], sep=""),
                        paste(ds, datasets[[i]], sep=",")),
                 ds)
  }
  ds
  #ds[match(by, by_uniq)]
}

datasets <- function(by, val, sep = ","){
  datasets <- split(val, by)
  func <- function(x) paste0(sort(x[!duplicated(x)]), collapse = sep)
  datasets <- lapply(datasets, func)
  #datasets <- as.character(datasets)[match(by, names(datasets))]
  datasets <- unlist(datasets[match(by, names(datasets))], use.names = F)
  datasets
}


check_links <- function(cri, data_source, data_links){
  dl_lst <- unlist(data_links, use.names = F)
  func <- function(x, y, e) {
    if(tolower(e)=="l"){
      all(y %in% x & length(x)>1)
    }else if (tolower(e)=="g"){
      any(y %in% x)
    }
  }

  lsts <- lapply(split(data_source, cri), function(x, l=data_links){
    xlst <- rep(list(a =x[!duplicated(x)]), length(l))
    r <- list(ds = paste0(sort(unique(x)), collapse=","))
    if(!all(toupper(dl_lst) == "ANY")) r["rq"] <- any(unlist(mapply(func, xlst, l, names(l), SIMPLIFY = F)))
    return(r)
  })

  dset <- lapply(lsts, function(x){x$ds})
  dset <- unlist(dset[match(cri, names(dset))], use.names = F)
  r <- list(ds=dset)
  if(!all(toupper(dl_lst) == "ANY")){
    dlks <- lapply(lsts, function(x){x$ds})
    dlks <- unlist(dlks[match(cri, names(dlks))], use.names = F)
    r$rq <- dlks
  }
  r
}

prep_lengths <- function(length, overlap_methods, int,
                         episode_unit, bi_direction, from_last,
                         include_index_period = F){
  length <- length
  if(class(length) != "list") length <- list(length)
  if(class(overlap_methods) != "list") overlap_methods <- list(overlap_methods)

  length <- sep_bdr_nl(length)
  r <- rle(names(length))
  overlap_methods <- rep(overlap_methods[as.numeric(r$values)], r$lengths)

  if(bi_direction == T){
    is_bdr <- names(length) %in% r$values[r$lengths == 2]
    n_length  <- length[!is_bdr]
    if(length(n_length) > 0){
      n_length <- lapply(n_length, invert_number_line)
      length <- c(n_length, length)
      overlap_methods <- c(overlap_methods[!is_bdr], overlap_methods)
    }
  }

  if(from_last == T) length <- lapply(length, invert_number_line)

  length <- lapply(length, function(x){
    number_line(l= right_point(int) + (left_point(x) * diyar::episode_unit[[episode_unit]]),
                r= right_point(int) + (right_point(x) * diyar::episode_unit[[episode_unit]]),
                gid = seq_len(length(int)))})

  if(include_index_period == T){
    length <- c(length, list(int))
    overlap_methods <- c(overlap_methods, list("overlap"))
  }

  overlap_methods <- lapply(overlap_methods, function(x){
    if(length(x)==1) x <- rep(x, length(int))
  })

  list(
    lengths = length,
    method = overlap_methods
  )
}
