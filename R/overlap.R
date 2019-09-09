#' @title Overlaping number lines
#'
#' @description A set of functions to check for overlaps in \code{number_line} objects
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @param method Method of overlap
#'
#' @aliases overlap
#' @return \code{logical} object
#'
#' @examples
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

overlap <- function(x, y, method = c("across","chain","aligns_start","aligns_end","within")){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("across","chain","aligns_start","aligns_end","within"))) stop(paste("`method` must be either 'across','chain','aligns_start','aligns_end' or'within'"))

  c <- FALSE
  if ("across" %in% tolower(method)) c <- ifelse(diyar::across(x, y), TRUE, c)
  if ("chain" %in% tolower(method)) c <- ifelse(diyar::chain(x, y), TRUE, c)
  if ("aligns_start" %in% tolower(method)) c <- ifelse(diyar::aligns_start(x, y), TRUE, c)
  if ("aligns_end" %in% tolower(method)) c <- ifelse(diyar::aligns_end(x, y), TRUE, c)
  if ("within" %in% tolower(method)) c <- ifelse(diyar::within(x, y), TRUE, c)

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

  r <- ifelse(!is.logical(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' chain(c, d)
#' chain(a, c)
#'
#' @export
chain <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- x@start == (y@start + y@.Data) | (x@start + x@.Data) == y@start

  r <- ifelse(!is.logical(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' aligns_start(c, d)
#' aligns_start(a, c)
#'
#' @export
aligns_start <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- x@start==y@start

  r <- ifelse(!is.logical(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' aligns_end(d, e)
#' aligns_end(a, c)
#'
#' @export
aligns_end <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- (x@start + x@.Data) == (y@start + y@.Data)
  r <- ifelse(!is.logical(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @examples
#' within(a, g)
#' within(b, a)
#'
#' @export
within <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- (x@start > y@start & (x@start + x@.Data) < (y@start + y@.Data)) | (y@start > x@start & (y@start + y@.Data) < (x@start + x@.Data))
  r <- ifelse(!is.logical(r), FALSE, r)
  return(r)
}

#' @rdname overlap
#' @return \code{character} object
#' @examples
#' overlap_method(a, c)
#' overlap_method(d, c)
#' overlap_method(a, g)
#' overlap_method(b, e)
#'
#' @export
overlap_method <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  m <- ""
  m <- ifelse(diyar::across(x, y), paste(m,"across", sep=","), m)
  m <- ifelse(diyar::chain(x, y), paste(m,"chain", sep=","), m)
  m <- ifelse(diyar::aligns_start(x, y), paste(m,"aligns_start", sep=","), m)
  m <- ifelse(diyar::aligns_end(x, y), paste(m,"aligns_end", sep=","), m)
  m <- ifelse(diyar::within(x, y), paste(m,"within", sep=","), m)

  m <- stringr::str_replace(m, "^,","")
  m <- ifelse(m=="", "none", m)
  m
}

#' @rdname overlap
#' @description Compress overlaping \code{number_line} objects into new wider \code{number_line} objects that covers the original \code{number_line} objects
#' @details The start and end points of the overlaping \code{number_line} objects are changed to that of the new wider \code{number_line} objects.
#' This results in the overlaping \code{number_line} objects having new but identiical start and end point.
#' If a familiar (but unique) \code{id} is used when creating the \code{number_line} objects,
#' [compress] can be a simple alternative to [episode_group]. See XX vignette.
#'
#' @param ... \code{number_line} objects
#' @param deduplicate \code{TRUE} to retain only one of the overlaping \code{number_line} objects
#'
#' @examples
#' c(number_line(1,5), number_line(2,4), number_line(10,10))
#' compress(c(number_line(1,5), number_line(2,4), number_line(10,10)))
#'
#' c(number_line(10,10), number_line(10,20), number_line(5,30),  number_line(30,40))
#' compress(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40))
#' compress(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "within")
#' compress(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "chain")
#' compress(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "across")
#'
#' @export

compress <- function(..., method = c("across","chain","aligns_start","aligns_end","within"), deduplicate = TRUE){

  x <- c(...)

  if(!diyar::is.number_line(x)) stop(paste("'...' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("across","chain","aligns_start","aligns_end","within"))) stop(paste("`method` must be either 'across','chain','aligns_start','aligns_end' or'within'"))

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)
  x <- diyar::reverse(x, "decreasing")

  c <- rep(0, length(x))
  for (i in 1:length(x)){
    if(c[i]==1) next
    h <- x@id == x[i]@id | diyar::overlap(x[i], x, method=method)
    x[which(h)]@.Data <- as.numeric(max(x[which(h),]@start + x[which(h),]@.Data)) - as.numeric(min(x[which(h),]@start))
    x[which(h)]@start <- min(x[which(h),]@start)
    c[which(h)] <- 1
    if(min(c)==1) break
  }

  if(deduplicate) x <- unique.number_line(x)
  return(x)
}
