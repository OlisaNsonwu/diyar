#' @title Overlapping number line objects
#'
#' @description A set of functions to identify overlapping \code{number_line} objects
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @param method Method of overlap
#'
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

overlap <- function(x, y, method = c("across","chain","aligns_start","aligns_end","inbetween")){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))

  c <- FALSE
  if ("across" %in% tolower(method)) c <- ifelse(diyar::across(x, y), TRUE, c)
  if ("chain" %in% tolower(method)) c <- ifelse(diyar::chain(x, y), TRUE, c)
  if ("aligns_start" %in% tolower(method)) c <- ifelse(diyar::aligns_start(x, y), TRUE, c)
  if ("aligns_end" %in% tolower(method)) c <- ifelse(diyar::aligns_end(x, y), TRUE, c)
  if ("inbetween" %in% tolower(method)) c <- ifelse(diyar::inbetween(x, y), TRUE, c)

  return(c)
}

#' @rdname overlap
#'
#' @examples
#' across(a, b)
#' across(a, e)
#' @export
across <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

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

  m <- ""
  m <- ifelse(diyar::across(x, y), paste(m,"across", sep=","), m)
  m <- ifelse(diyar::chain(x, y), paste(m,"chain", sep=","), m)
  m <- ifelse(diyar::aligns_start(x, y), paste(m,"aligns_start", sep=","), m)
  m <- ifelse(diyar::aligns_end(x, y), paste(m,"aligns_end", sep=","), m)
  m <- ifelse(diyar::inbetween(x, y), paste(m,"inbetween", sep=","), m)

  m <- stringr::str_replace(m, "^,","")
  m <- ifelse(m=="", "none", m)
  m
}
