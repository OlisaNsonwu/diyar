#' @title Convenience funcitons
#'
#' @description Convenience funcitons
#'
#' @param x x
#' @aliases finite_check
#'
finite_check <- function(x){
  e <- which(!is.finite(as.numeric(x)))
  if(length(e) %in% 1:10) {
    paste0("c(",listr(fmt(e)),")")
  }else if(length(e) > 10) {
    paste0("c(",paste0(fmt(e[1:10]), collapse = ", "),", ...)")
  }else if(length(e) == 0)
    TRUE
  }

#' @rdname finite_check
enq_vr <- function(x){
  x <- as.character(x)
  if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
  if(length(x)==0) x <- NULL
  x
}

#' @rdname finite_check
fmt <- function(g){
  formatC(g, format="d", big.mark=",")
}

#' @rdname finite_check
listr <- function(x){
  p <- x[length(x)]
  f <- x[1:(length(x)-1)]
  f <- paste(f, collapse = ", ")
  x <- ifelse(length(x) > 1,paste(f,p, sep=" and "), f)
  return(x)
}

#' @rdname finite_check
duplicates_check <- function(x){
  names(x) <- 1:length(x)
  x <- head(sort(x[x %in% x[duplicated(x)]]), 10)
  l <- length(x)
  x <- lapply(split(as.numeric(names(x)),x), function(x){
    ifelse(length(x)>5,
           paste0("c(",paste0(x[1:5], collapse = ","),",...)"),
           paste0("c(",paste0(x, collapse = ","),")"))
  })

  if(length(x) >5 | l>10){
    paste0(paste0(as.character(x),collapse = " ,"), " ...")
  }else if (length(x) %in% 1:5) {
    listr(as.character(x))
  }else if(length(x)==0){
    TRUE
  }
}

#' @rdname finite_check
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
