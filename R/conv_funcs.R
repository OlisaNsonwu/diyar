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
    paste0("[",paste0(fmt(e[1:10]), collapse = ","),", ...]")
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
fmt <- function(x){
  formatC(x, format="d", big.mark=",")
}

# @rdname finite_check
listr <- function(x){
  p <- x[length(x)]
  f <- x[1:(length(x)-1)]
  f <- paste(f, collapse = ", ")
  x <- ifelse(length(x) > 1,paste(f,p, sep=" and "), f)
  return(x)
}

# @rdname finite_check
duplicates_check <- function(x){
  names(x) <- 1:length(x)
  x <- head(sort(x[x %in% x[duplicated(x)]]), 10)
  l <- length(x)
  x <- lapply(split(as.numeric(names(x)),x), function(x){
    ifelse(length(x)>5,
           paste0("[",paste0(x[1:5], collapse = ","),",...]"),
           paste0("[",paste0(x, collapse = ","),"]"))
  })

  if(length(x) >5 | l>10){
    paste0(paste0(as.character(x),collapse = " ,"), " ...")
  }else if (length(x) %in% 1:5) {
    listr(as.character(x))
  }else if(length(x)==0){
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
space_out_y <- function(x_axis){
  rows_n <- length(x_axis)
  sn_change <- y_axis <- rep(1, rows_n)
  while (max(sn_change) ==1) {
    c <- diyar::compress_number_line(x_axis, deduplicate = F, collapse = T)

    ord <- lapply(split(1:rows_n, paste0(c@gid,"-", y_axis)), order)
    lk_sn <- lapply(split(y_axis, paste0(c@gid)), function(x){
      rep(max(x), length(x))
    })

    ord <- unsplit(ord, paste0(c@gid,"-", y_axis))
    lk_sn <- unsplit(lk_sn, paste0(c@gid))
    new_y_axis <- ifelse(ord==2, lk_sn+1, y_axis)

    sn_change <- ifelse(y_axis != new_y_axis,1,0)
    y_axis <- new_y_axis
  }

  return(y_axis)
}
