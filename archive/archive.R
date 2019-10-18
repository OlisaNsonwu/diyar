
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

