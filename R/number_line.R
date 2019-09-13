#' @title Number line objects
#'
#' @description A set of functions to create and manipulate \code{number_line} objects.
#'
#' @details
#' A \code{number_line} object represents a series of real numbers on a number line.
#'
#' Visually, it's presented as the left (\code{l}) and right (\code{r}) points of the series. This may differ from start and end points.
#' The start point is the lowest number in the series, regardless of whether it's at the left or right point..
#'
#' The location of the start point - left or right, indicate if it's an \code{"increasing"} or \code{"decreasing"} series.
#' This is refered to as the \code{direction} of the \code{number_line} object.
#'
#' @param l Left point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param r Right point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param id Unique \code{numeric} ID. Providing this is optional.
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' library(lubridate)
#'
#' number_line(-100, 100); number_line(10, 11.2)
#'
#' # other numeric based object classes are also compatible for numeric_line objects
#' number_line(dmy_hms("15/05/2019 13:15:07"), dmy_hms("15/05/2019 15:17:10"))
#'
#' # a warning is given if 'l' and 'r' have different classes. Consider if these need to be corrected
#' number_line(2, dmy("05/01/2019"))
#'
#' @export
number_line <- function(l, r, id = NULL){
  er1 <- try(as.numeric(l), silent = TRUE)
  er2 <- try(as.numeric(r), silent = TRUE)
  er3 <- try(as.numeric(r) - as.numeric(l), silent = TRUE)

  if(!is.numeric(er1) | !is.numeric(er2) | !is.numeric(er3)) stop(paste("'l' or 'r' aren't compatible for a number_line object",sep=""))
  if(!(is.numeric(id) | is.null(id))) stop(paste("'id' must be numeric",sep=""))

  if(all(class(l)!=class(r))) warning("'l' and 'r' have different classes. It may need to be reconciled")

  if(is.null(id) | any(duplicated(id)) | any(!is.finite(id)) ) id <- 1:length(l)
  nl <- methods::new("number_line", .Data = as.numeric(r) - as.numeric(l), start=l, id = id)
  return(nl)
}

#' Number line objects
#' S4 objects representing a series of finite numbers on a number line
#' Used for range matching in \code{\link{record_grouping}} and interval grouping in \code{\link{episode_grouping}}
#' @slot start Start of the number line
#' @slot id Unique \code{numeric} ID. Providing this is optional.
#' @slot .Data Length/with and direction of the number line
#' @export
setClass("number_line", contains = "numeric", representation(start = "ANY", id = "numeric"))

setMethod("show", signature(object="number_line"), function(object){
  print(format.number_line(object))
})

setMethod("rep", signature(x = "number_line"), function(x, ...) {
  methods::new("number_line", rep(x@.Data, ...), start = rep(x@start, ...), id = rep(x@id, ...))
})

setMethod("[", signature(x = "number_line"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i], id = x@id[i])
          })

setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i], id = x@id[i])
          })

 setMethod("[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
   if (is.number_line(value)) {
     x@.Data[i] <- value@.Data
     x@start[i] <- value@start
     x@id[i] <- value@id
     new("number_line", x@.Data, start = x@start, id = x@id)
   }
 })

 setMethod("[[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
   if (is.number_line(value)) {
     x@.Data[i] <- value@.Data
     x@start[i] <- value@start
     x@id[i] <- value@id
     new("number_line", x@.Data, start = x@start, id = x@id)
   }
 })

setMethod("$", signature(x = "number_line"), function(x, name) {
  methods::slot(x, name)
})

setMethod("$<-", signature(x = "number_line"), function(x, name, value) {
  methods::slot(x, name) <- value
  x
})

 setMethod("c", signature(x = "number_line"), function(x,...) {
   a <- lapply(list(x, ...), function(y) as.number_line(y)@start)
   for(i in 1:length(a)){
     if(i==1) ai <- a[[i]]
     if(i>1) ai <- c(ai, a[[i]])
   }

   id <- unlist(lapply(list(x, ...), function(y) as.number_line(y)@id))
   zi <- unlist(list(x, ...))

   methods::new("number_line", .Data = zi, id = id, start= ai)

 })

#' @rdname number_line
#' @examples
#' # convert numeric objects to number_line objects
#' as.number_line(5.1); as.number_line(dmy("21/10/2019"))
#'
#' @export
as.number_line <- function(x){

  er1 <- try(as.numeric(x), silent = TRUE)
  er2 <- try(as.numeric(x) + 0, silent = TRUE)

  if(!is.numeric(er1) | !is.numeric(er2)) stop(paste("'x' can't be coerced to a number_line object",sep=""))

  if(!diyar::is.number_line(x)){
    x <- methods::new("number_line", .Data = 0, start= x, id = length(x))
  }

  return(x)
}

#' @rdname number_line
#' @examples
#' # test for number_line objects
#' a <- number_line(0, -100)
#' b <- number_line(dmy("25/04/2019"), dmy("01/01/2019"))
#' is.number_line(a); is.number_line(b)
#'
#' @export
is.number_line <- function(x) class(x)=="number_line"

#' @rdname number_line
#' @param x \code{number_line} object
#' @param direction Type of \code{"number_line"} objects whose direction are to be reversed. Options are; \code{"increasing"}, \code{"decreasing"} or \code{"both"}.
#' @details
#' \code{reverse_number_line()} - reverses the direction of a \code{number_line} object. A reversed \code{number_line} object has its \code{l} and \code{r} points swapped but maintains the same width or length.
#' The \code{direction} argument determines which type of \code{number_line} objects will be reversed.
#' \code{number_line} objects with non-finite numeric starts or end points i.e. (\code{NA}, \code{NaN} and \code{Inf}) can't be reversed.
#' @examples
#' #reverse number_line objects
#' reverse_number_line(number_line(dmy("25/04/2019"), dmy("01/01/2019")))
#' reverse_number_line(number_line(200,-100), "increasing")
#' reverse_number_line(number_line(200,-100), "decreasing")
#'
#' @export
reverse_number_line <- function(x, direction = "both"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(!(length(direction)==1 & is.character(direction))) stop(paste("'direction' must be a character of length 1"))
  if(!tolower(direction) %in% c("increasing","decreasing","both") ) stop(paste("`direction` must be either 'increasing', 'decreasing', or 'both'"))
  f <- x

  if(tolower(direction) == "decreasing"){
    f@.Data <- ifelse(x@.Data <0 & is.finite(x@.Data), -x@.Data, x@.Data)
    c <- ifelse(x@.Data <0 & is.finite(x@.Data), x@.Data, 0)
  }else if(tolower(direction) == "increasing"){
    f@.Data <- ifelse(x@.Data >0 & is.finite(x@.Data), -x@.Data, x@.Data)
    c <- ifelse(x@.Data >0 & is.finite(x@.Data), x@.Data, 0)
  } else if(tolower(direction) == "both"){
    f@.Data <- ifelse(is.finite(x@.Data), -x@.Data, x@.Data)
    c <- ifelse(is.finite(x@.Data), x@.Data, 0)
  }

  f@start <- f@start + c

  return(f)
}

#' @rdname number_line
#' @details
#' \code{shift_number_line()} - a convenience function to shift a \code{number_line} object towards the positive or negative end of the number line.
#' @examples
#' # Shift number_line objects
#' number_line(5,6)
#' # Towards the positive end of the number line
#' shift_number_line(number_line(5,6), 2)
#' # Towards the negative end of the number line
#' shift_number_line(number_line(6,1), -2)
#'
#' @export
shift_number_line <- function(x, by=1){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(!(is.finite(by) & length(by) ==1)) stop(paste("'by' must be a numeric object of length 1",sep=""))

  if(!(!is.finite(x@start) | !is.finite(x@.Data))) x@start <- x@start + by

  return(x)
}

#' @rdname number_line
#' @details
#' \code{expand_number_line()} - a convenience function to increase or decrease the width or length of a \code{number_line} object.
#' @examples
#' # Increase or reduce the width or length of a \code{number_line} object
#' c(number_line(3,6), number_line(6,3))
#' expand_number_line(c(number_line(3,6), number_line(6,3)), 2)
#' expand_number_line(c(number_line(3,6), number_line(6,3)), -1)
#' expand_number_line(c(number_line(3,6), number_line(6,3)), 2, "start")
#' expand_number_line(c(number_line(3,6), number_line(6,3)), -2, "end")
#'
#' @export
expand_number_line <- function(x, by=1, point ="both"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(!(is.finite(by) & length(by) ==1)) stop(paste("'by' must be a numeric object of length 1",sep=""))
  if(!is.character(point)) stop(paste("'point' must be a character object"))
  if(all(!tolower(point) %in% c("both","start","end"))) stop(paste("`point` must be either 'start','end' or 'both'"))

    by <- ifelse(x@.Data<0 & is.finite(x@.Data),-by, by)
    if(point == "both") x <- diyar::number_line(x@start - by, (x@start + x@.Data) + by)
    if(point == "start") x <- diyar::number_line(x@start - by, (x@start + x@.Data))
    if(point == "end") x <- diyar::number_line(x@start, (x@start + x@.Data) + by)

    return(x)
}

#' @rdname number_line
#' @details
#' \code{compress_number_line()} - Collapses overlaping \code{number_line} objects into a new \code{number_line} objects that covers the start and end points of the originals.
#' This results in duplicate \code{number_line} objects with start and end points of the new expanded \code{number_line} object.
#' See \code{\link{overlap}} for further details on overlaping \code{number_line} objects.
#' If a familiar (but unique) \code{id} is used when creating the \code{number_line} objects,
#' \code{compress_number_line()} can be a simple alternative to \code{\link{record_group}} or \code{\link{episode_group}}.
#'
#' @param deduplicate if \code{TRUE}, retains only one of duplicates
#'
#' @examples
#' # collapse number lines
#' c(number_line(1,5), number_line(2,4), number_line(10,10))
#' compress_number_line(c(number_line(1,5), number_line(2,4), number_line(10,10)))
#'
#' c(number_line(10,10), number_line(10,20), number_line(5,30),  number_line(30,40))
#' compress_number_line(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40))
#' compress_number_line(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "inbetween")
#' compress_number_line(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "chain")
#' compress_number_line(number_line(10,10), number_line(10,20), number_line(5,30), number_line(30,40), method = "across")
#'
#' @export

compress_number_line <- function(..., method = c("across","chain","aligns_start","aligns_end","inbetween"), deduplicate = TRUE){

  x <- c(...)

  if(!diyar::is.number_line(x)) stop(paste("'...' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)
  x <- diyar::reverse_number_line(x, "decreasing")

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

#' @rdname number_line
#' @param by increment or decrement
#' @details
#' \code{series()} - a convenience function to convert a \code{number_line} object into a sequence of finite numbers. The sequence will also include the start and end points.
#' The direction of the sequence will correspond to that of the \code{number_line} object.
#' @examples
#' # Convert a number line object to its series of real numbers
#' series(number_line(1, 5))
#' series(number_line(5, 1), .5)
#' series(number_line(dmy("01/04/2019"), dmy("10/04/2019")), 1)
#'
#' # The length of the vector depends on the object class
#' series(number_line(dmy("01/04/2019"), dmy("04/04/2019")), 1.5)
#' series(number_line(dmy_hms("01/04/2019 00:00:00"), dmy_hms("04/04/2019 00:00:00")), 1.5)
#' series(number_line(dmy_hms("01/04/2019 00:00:00"), dmy_hms("04/04/2019 00:00:00")), duration(1.5,"days"))
#'
#' @export
series <- function(x, by=1){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(!(is.finite(by) & length(by) ==1)) stop(paste("'by' must be a numeric object of length 1",sep=""))

  if(!is.finite(x@start) | !is.finite(x@.Data)){
    s <- c(x@start, x@.Data)
  }else{
    by <- ifelse(x@.Data>0, abs(by), -abs(by))
    s <- unique(c(x@start, seq(x@start, x@start + x@.Data, by), x@start + x@.Data))
  }

  return(s)
}

#' @rdname number_line
#' @param ... arguments for particular methods | \code{number_line} objects in \code{compress_number_line()}
#' @export
unique.number_line <- function(x, ...){

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)

  x <- unique(data.frame(l = x@start, r = x@start + x@.Data, row.names = x@id))

  x <- diyar::number_line(l =x$l, r = x$r, id = as.numeric(row.names(x)))

  return(x)
}

#' @rdname number_line
#' @export
format.number_line <- function(x, ...){
  x <- x[1:length(x@start)]
  s <- ifelse(x@start + x@.Data > x@start, "->","<-")
  s <- ifelse(x@start + x@.Data == x@start, "==",s)
  s <- ifelse(!is.finite(x@start + x@.Data) , "??",s)

  paste(x@start, s, x@start + x@.Data, sep=" ")
}
