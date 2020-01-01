#' @title Overlapping number line objects
#'
#' @description A set of functions to identify overlapping \code{number_line} objects
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @param method Method of overlap. Check multiple pairs of \code{number_line} objects with the same \code{method}. Deprecated use \code{methods} instead.
#' @param methods Methods of overlap. Check multiple pairs of \code{number_line} objects with the different \code{methods}
#' @aliases overlap
#' @return \code{logical} object
#'
#' @examples
#' a <- number_line(-100, 100)
#' b <- number_line(10, 11.2)
#' c <- number_line(100, 200)
#' d <- number_line(100, 120)
#' e <- number_line(50, 120)
#' g <- number_line(100,100)
#' @export

overlap <- function(x, y, method = c("across","chain","aligns_start","aligns_end","inbetween"),
                    methods = "across|chain|aligns_start|aligns_end|inbetween"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))
  if(!(length(methods) %in% c(1, length(x)))) stop(paste("length of 'methods' must be 1 or the same as 'x'",sep=""))
  o <- unique(unlist(strsplit(methods, split="\\|")))
  o <- o[!o %in% c("across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste("\n'", "Valid 'methods' are 'across','chain','aligns_start','aligns_end' or 'inbetween' \n\n",
                              "Syntax ~ \"method1|method2|method3...\" \n",
                              "                 OR                   \n",
                              "Use ~ include_overlap_method() or exclude_overlap_method()", sep=""))
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  c <- rep(F, length(x))
  if(length(m)==1) m <- rep(m, length(x))
  c <- ifelse(grepl("across", tolower(m)) & c == F, diyar::across(x, y),c)
  c <- ifelse(grepl("chain", tolower(m)) & c == F, diyar::chain(x, y),c)
  c <- ifelse(grepl("aligns_start", tolower(m)) & c == F, diyar::aligns_start(x, y),c)
  c <- ifelse(grepl("aligns_end", tolower(m)) & c == F, diyar::aligns_end(x, y),c)
  c <- ifelse(grepl("inbetween", tolower(m)) & c == F, diyar::inbetween(x, y),c)
  c <- ifelse(grepl("across", tolower(m)) & c == F, diyar::across(x, y),c)

  return(c)
}

#' @rdname overlap
#' @examples
#' across(a, b)
#' across(a, e)
#' @export
across <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  r <- (y@start > x@start & y@start < (x@start + x@.Data) & (y@start + y@.Data) > (x@start + x@.Data) ) |
    (x@start > y@start & x@start < (y@start + y@.Data) & (x@start + x@.Data) > (y@start + y@.Data) )
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' chain(c, d)
#' chain(a, c)
#' @export
chain <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  r <- x@start == (y@start + y@.Data) | (x@start + x@.Data) == y@start
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' aligns_start(c, d)
#' aligns_start(a, c)
#' @export
aligns_start <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  r <- x@start==y@start
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' aligns_end(d, e)
#' aligns_end(a, c)
#' @export
aligns_end <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  r <- (x@start + x@.Data) == (y@start + y@.Data)
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' inbetween(a, g)
#' inbetween(b, a)
#' @export
inbetween <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  r <- (x@start > y@start & (x@start + x@.Data) < (y@start + y@.Data)) | (y@start > x@start & (y@start + y@.Data) < (x@start + x@.Data))
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @return \code{character} object
#' @examples
#' overlap_method(a, c)
#' overlap_method(d, c)
#' overlap_method(a, g)
#' overlap_method(b, e)
#' @export
overlap_method <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  #if(!(length(x) %in% c(1, length(y)))) stop(paste("length of 'x' must be 1 or the same as 'y'",sep=""))

  m <- ""
  m <- ifelse(diyar::across(x, y), paste(m,"across", sep="|"), m)
  m <- ifelse(diyar::chain(x, y), paste(m,"chain", sep="|"), m)
  m <- ifelse(diyar::aligns_start(x, y), paste(m,"aligns_start", sep="|"), m)
  m <- ifelse(diyar::aligns_end(x, y), paste(m,"aligns_end", sep="|"), m)
  m <- ifelse(diyar::inbetween(x, y), paste(m,"inbetween", sep="|"), m)

  m <- gsub("^\\|","",m)
  m <- ifelse(m=="", "none", m)
  m
}

#' @rdname overlap
#' @examples
#' include_overlap_method("across")
#' include_overlap_method(c("across", "chain"))
#' @export
include_overlap_method <- function(methods){
  lst <- c( "across", "chain", "aligns_start", "aligns_end", "inbetween")
  method <- method[method %in% lst]
  method <- paste(method,sep="", collapse = "|")
  method
}

#' @rdname overlap
#' @examples
#' exclude_overlap_method("across")
#' exclude_overlap_method(c("across", "chain"))
#' @export
exclude_overlap_method <- function(methods){
  lst <- c( "across", "chain", "aligns_start", "aligns_end", "inbetween")
  method <- lst[!lst %in% method]
  method <- paste(method,sep="", collapse = "|")
  method
}

