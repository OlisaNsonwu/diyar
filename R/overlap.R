#' @title Overlaping number lines
#'
#' @description A function to check for overlaps in \code{number_line} objects
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @param method Method of overlap
#'
#' @aliases overlap
#' @return \code{logical} object
#'
#' @examples
#' library(diyar)
#'
#' a <- number_line(-100, 100)
#' b <- number_line(10, 11.2)
#' c <- number_line(100, 200)
#' d <- number_line(100, 120)
#' e <- number_line(50, 120)
#' g <- number_line(100,100)
#'
#' @export
#'

overlap <- function(x,y, method = c("across","chain","aligns_start","aligns_end","within")){
  c <- FALSE
  if ("across" %in% method) c <- ifelse(diyar::across(x,y), TRUE, c)
  if ("chain" %in% method) c <- ifelse(diyar::chain(x,y), TRUE, c)
  if ("aligns_start" %in% method) c <- ifelse(diyar::aligns_start(x,y), TRUE, c)
  if ("aligns_end" %in% method) c <- ifelse(diyar::aligns_end(x,y), TRUE, c)
  if ("within" %in% method) c <- ifelse(diyar::within(x,y), TRUE, c)

  return(c)
}

#' @rdname overlap
#'
#' @examples
#' across(a, b)
#' across(a, e)
#' @export
across <- function(x,y){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("`y` is not a number_line object"))

  (y@start > x@start & y@start < (x@start + x@.Data) & (y@start + y@.Data) > (x@start + x@.Data) ) |
    (x@start > y@start & x@start < (y@start + y@.Data) & (x@start + x@.Data) > (y@start + y@.Data) )
}

#' @rdname overlap
#' @examples
#' chain(c, d)
#' chain(a, c)
#'
#' @export
chain <- function(x,y){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("`y` is not a number_line object"))

  x@start == (y@start + y@.Data) | (x@start + x@.Data) == y@start
}

#' @rdname overlap
#' @examples
#' aligns_start(c, d)
#' aligns_start(a, c)
#'
#' @export
aligns_start <- function(x,y){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("`y` is not a number_line object"))

  x@start==y@start
}

#' @rdname overlap
#' @examples
#' aligns_end(d, e)
#' aligns_end(a, c)
#'
#' @export
aligns_end <- function(x,y){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("`y` is not a number_line object"))

  (x@start + x@.Data) == (y@start + y@.Data)
}

#' @rdname overlap
#' @examples
#' within(a, g)
#' within(b, a)
#'
#' @export
within <- function(x,y){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("`y` is not a number_line object"))

  (x@start > y@start & (x@start + x@.Data) < (y@start + y@.Data)) | (y@start > x@start & (y@start + y@.Data) < (x@start + x@.Data))
}



#' @rdname overlap
#' @return \code{character} object (\code{overlap_method})
#' @examples
#' overlap_method(a, c)
#' overlap_method(d, c)
#' overlap_method(a, g)
#' overlap_method(b, e)
#'
#' @export
overlap_method <- function(x,y){
  m <- ""
  m <- ifelse(diyar::across(x,y), paste(m,"across", sep=","), m)
  m <- ifelse(diyar::chain(x,y), paste(m,"chain", sep=","), m)
  m <- ifelse(diyar::aligns_start(x,y), paste(m,"aligns_start", sep=","), m)
  m <- ifelse(diyar::aligns_end(x,y), paste(m,"aligns_end", sep=","), m)
  m <- ifelse(diyar::within(x,y), paste(m,"within", sep=","), m)

  m <- stringr::str_replace(m, "^,","")
  m <- ifelse(m=="", "none", m)
  m
}