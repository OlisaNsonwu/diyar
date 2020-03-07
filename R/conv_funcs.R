#' @title Check for numeric values that are not finite
#'
#' @description Check for numeric values that are not finite
#'
#' @param x \code{numeric} object or can be coerced to \code{numeric} object
#' @aliases conv_funcs
#' @return \code{character} object
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

#' @rdname conv_funcs
enq_vr <- function(x){
  x <- as.character(x)
  if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
  if(length(x)==0) x <- NULL
  x
}

#' @rdname conv_funcs
fmt <- function(g){
  formatC(g, format="d", big.mark=",")
}

#' @rdname conv_funcs
listr <- function(x){
  p <- x[length(x)]
  f <- x[1:(length(x)-1)]
  f <- paste(f, collapse = ", ")
  x <- ifelse(length(x) > 1,paste(f,p, sep=" and "), f)
  return(x)
}

#' @rdname conv_funcs
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
