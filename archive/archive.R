
arch_rolling_episodes <- function(x, strata=NULL, case_length, recurrence_length=NULL, from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), deduplicate = FALSE, display = TRUE){

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!(all(is.finite(recurrence_length) & recurrence_length >= 0) | is.null(recurrence_length)) ) stop(paste("'recurrence_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(x))) stop(paste("All 'x' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(x)))) stop(paste("length of 'case_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(x)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(strata) %in% c(1, length(x)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'x'",sep=""))

  fmt <- function(g) formatC(g, format="d", big.mark=",")
  if(is.null(recurrence_length)){
    recurrence_length <- case_length
  }

  if(any(!diyar::is.number_line(x))){
    x  <- diyar::as.number_line(x)
  }

  x@gid <- 1:length(x)
  if(is.null(strata)){
    x <- sort.number_line(x, decreasing = from_last)
    s <- rep(1, length(x))
  }else{
    x <- data.frame(id= x@id, s = strata, l = x@start, nl = x)
    if(from_last) x$l <- -as.numeric(x$l)
    db <- x[order(x$s, x$l, x$id),]
    x <- db$nl
    s <- as.numeric(as.factor(db$s))
    rm(db)
  }

  pr_sn <- diyar::number_line(0,0, gid =x@gid, id =x@id)
  if(any(duplicated(x@id) | is.na(x@id))) stop(paste("'id' slot of the number_line object must be contain unique finite numbers",sep=""))
  x@gid <- x@id

  x <- diyar::reverse_number_line(x, "decreasing")

  x@id <- 1:length(x@id)

  j <- 0
  l <- NULL
  t <- rep(0, length(x))
  pt <- ifelse(from_last,"start","end")
  while (min(t) ==0 & j!=length(x)){
    total_1 <- length(t[t==0])
    if(display){cat(paste("Episode window ",j+1,".\n", sep=""))}

    if(length(l)==0 | is.null(l)){
      l <- x[t==0][1]
      l_s <- s[t==0][1]
      e <- case_length
    }else{
      e <- recurrence_length
    }

    h <- (x@id == l@id | (diyar::overlap(diyar::expand_number_line(l, by=e, pt), x, method = overlap_method) & s==l_s))
    x[which(h)]@.Data <- as.numeric(max(diyar::right_point(x[which(h),]))) - as.numeric(min(x[which(h),]@start))
    x[which(h)]@start <- min(x[which(h),]@start)
    x[which(h)]@gid <- l@gid

    if(min(t[which(h)])==1){
      l <- NULL
    }else{
      l <- x[which(h)]
      l <- l[length(l)]

      l_s <- s[which(h)]
      l_s <- l_s[length(l_s)]
    }

    tagged_1 <- length(h[h & t==0])
    t[which(h)] <- 1
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }
    if(min(t)==1) break
    j <- j + 1
  }

  x@id <- pr_sn@id
  db <- data.frame(nl=x, sn=pr_sn@gid)
  db <- db[order(db$sn),]
  x <- db$nl
  rm(db)
  if(deduplicate) x <- unique.number_line(x)

  if(from_last) x <- diyar::reverse_number_line(x)
  return(x)
}


arch_fixed_episodes <- function(x, strata = NULL, case_length, episodes_max = Inf, from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), deduplicate = FALSE, display = TRUE){

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(x))) stop(paste("All 'x' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(x)))) stop(paste("length of 'case_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(strata) %in% c(1, length(x)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'x'",sep=""))

  fmt <- function(g) formatC(g, format="d", big.mark=",")
  if(any(!diyar::is.number_line(x))){
    x  <- diyar::as.number_line(x)
  }

  x@gid <- 1:length(x)

  if(is.null(strata)){
    x <- sort.number_line(x, decreasing = from_last)
    s <- rep(1, length(x))
  }else{
    x <- data.frame(id= x@id, s = strata, l = x@start, nl = x)
    db <- x[order(x$s, x$l, x$id, decreasing = from_last),]
    x <- db$nl
    s <- as.numeric(as.factor(db$s))
    rm(db)
  }

  pr_sn <- diyar::number_line(0,0, gid =x@gid, id =x@id)

  if(any(duplicated(x@id) | is.na(x@id))) stop(paste("'id' slot of the number_line object must be contain unique finite numbers",sep=""))

  x@gid <- x@id
  x <- diyar::reverse_number_line(x, "decreasing")

  j <- 0
  t <- rep(0, length(x))
  pt <- ifelse(from_last,"start","end")
  if(length(case_length)==1) case_length <- rep(case_length, length(x@id))
  while (min(t) ==0 & j!=length(x)){
    total_1 <- length(t[t==0])
    if(display){cat(paste("Episode window ",j+1,".\n", sep=""))}
    l <- x[t==0][1]
    l_l <- case_length[t==0][1]
    l_s <- s[t==0][1]
    h <- (x@id == l@id | (diyar::overlap(diyar::expand_number_line(l, l_l, pt), x, method = overlap_method) & s ==l_s)) & t != 1
    x[which(h)]@.Data <- as.numeric(max(diyar::right_point(x[which(h),]))) - as.numeric(min(x[which(h),]@start))
    x[which(h)]@start <- min(x[which(h),]@start)
    x[which(h)]@gid <- l@gid

    tagged_1 <- length(h[h])
    t[which(h)] <- 1
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }
    if(min(t)==1) break
    j <- j + 1
  }

  x@id <- pr_sn@id
  db <- data.frame(nl=x, sn=pr_sn@gid)
  x <- db[order(db$sn),]$nl
  rm(db)

  if(deduplicate) x <- unique.number_line(x)
  if(from_last) x <- diyar::reverse_number_line(x)
  return(x)
}

# Up to v.0.4.0
# mths <- c("across", "inbetween", "chain", "reverse", "aligns_start", "aligns_end", "exact", "overlap", "none")
# mths <- sort(mths)
# mths_lst <- lapply(seq_len(length(mths)), function(i){
#   combn(length(mths), i, simplify = FALSE)
# })
# mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
# mths_lst <- lapply(mths_lst, function(x){
#   paste0(mths[x], collapse ="|")
# })
# mths_lst <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)
# mths_lst_cd <- lapply(mths, function(x){
#   if(!x %in% c("overlap", "none")){
#     which(grepl(x, mths_lst) & !grepl("overlap|none", mths_lst))
#   }else if (x == "overlap"){
#     which(grepl(x, mths_lst) & !grepl("none", mths_lst))
#   }else if (x == "none"){
#     which(grepl(x, mths_lst))
#   }
# })
# names(mths_lst_cd) <- mths
# overlap_methods <- list(options = mths_lst, methods = mths_lst_cd)
# save(list = "overlap_methods", file = "data/overlap_methods.RData")

mths <- c("across", "inbetween", "chain", "reverse", "aligns_start", "aligns_end", "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse ="|")
})
mths_lst_1 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths <- c("x_across_y", "y_across_x", "x_inbetween_y",
          "y_inbetween_x", "x_chain_y", "y_chain_x",
          "reverse", "aligns_start", "aligns_end",
          "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse = "|")
})
mths_lst_2 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths <- c("x_across_y", "y_across_x", "x_inbetween_y",
          "y_inbetween_x", "x_chain_y", "y_chain_x",
          "x_aligns_start_y", "y_aligns_start_x", "x_aligns_end_y",
          "y_aligns_end_x", "reverse",
          "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse = "|")
})
mths_lst_3 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths_lst <- c(mths_lst_1, mths_lst_2, mths_lst_3)
mths_lst[grepl("overlap", mths_lst)] <- "overlap"
mths_lst[grepl("none", mths_lst)] <- "none"
mths_lst <- mths_lst[!duplicated(mths_lst)]

mths_lst_cd <- lapply(c(mths, "across", "chain", "inbetween", "aligns_start", "aligns_end"), function(x){
  which(grepl(x, mths_lst))
})
names(mths_lst_cd) <- c(mths, "across", "chain", "inbetween", "aligns_start", "aligns_end")

mths_lst <- list(cd = seq_len(length(mths_lst)), nm = mths_lst)

# Retire reverse overlaps option and codes
retired_codes <- which(grepl("reverse", mths_lst$nm))
mths_lst_cd <- lapply(mths_lst_cd, function(x){
  x[!x %in% retired_codes]
})

mths_lst_cd <- mths_lst_cd[names(mths_lst_cd) != "reverse"]
mths_lst <- lapply(mths_lst, function(x) x[which(!grepl("reverse", mths_lst$nm))])

# Retire overlaps option codes for the now wrapper overlap methods
exc_lgk <- lapply(mths_lst$nm, function(x){
  any(unlist(strsplit(x, "\\|"), use.names = FALSE) %in% c("across", "chain", "inbetween", "aligns_start", "aligns_end"))
})
exc_lgk <- unlist(exc_lgk, use.names = FALSE)

retired_codes <- mths_lst$cd[exc_lgk]
mths_lst_cd <- lapply(mths_lst_cd, function(x){
  x[!x %in% retired_codes]
})
mths_lst <- lapply(mths_lst, function(x) x[which(!exc_lgk)])
mths_lst_cd <- mths_lst_cd[!names(mths_lst_cd) %in% c("across", "chain", "inbetween", "aligns_start", "aligns_end")]

overlap_methods <- list(options = mths_lst, methods = mths_lst_cd)
save(list = "overlap_methods", file = "data/overlap_methods.RData")
