#' @title Number lines
#'
#' @description A set of functions to create and manipulate \code{number_line} objects.
#' @param a Start of the number line. Should be, or can be coerced to a \code{numeric} object
#' @param z End of the number line. Should be, or can be coerced to a \code{numeric} object
#' @param id Unique \code{numeric} element ID
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' library(lubridate)
#'
#' number_line(-100, 100)
#' number_line(10, 11.2)
#'
#' # other numeric based object classes are also compatible for numeric_line objects
#' number_line(dmy("12/03/2019"), dmy("05/01/2019"))
#' number_line(dmy_hms("15/05/2019 13:15:07"), dmy_hms("15/05/2019 15:17:10"))
#'
#' # a prompt is given if 'a' and 'z' have different classes. Consider if these need to be corrected
#' number_line(2, dmy("05/01/2019"))
#' number_line(dmy("05/01/2019"), 2)
#'
#' @export
#'

number_line <- function(a, z, id = NULL){
  er1 <- try(as.numeric(a), silent = TRUE)
  er2 <- try(as.numeric(z), silent = TRUE)
  er3 <- try(as.numeric(z) - as.numeric(a), silent = TRUE)

  if(!is.finite(er1) | !is.finite(er2) | !is.finite(er3)) stop(paste("'a' or 'z' aren't compatible for a number_line object",sep=""))
  if(!(is.numeric(id) | is.null(id))) stop(paste("'id' must be numeric",sep=""))

  if(all(class(a)!=class(z))) warning("'a' and 'z' have different classes. It may need to be reconciled")

  if(is.null(id) | any(duplicated(id)) | any(!is.finite(id)) ) id <- 1:length(a)
  nl <- methods::new("number_line", .Data = as.numeric(z) - as.numeric(a), start=a, id = id)
  return(nl)
}

#' Number lines
#' S4 objects representing a number line
#' Used for range matching in [record_grouping] and interval grouping in [episode_grouping]
#' @slot start Start of the number line
#' @slot id \code{numeric} element ID
#' @slot .Data Length or width of the number line
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
#' as.number_line(5.1)
#' as.number_line(dmy("21/10/2019"))
#'
#' @export
as.number_line <- function(x){

  er1 <- try(as.numeric(x), silent = TRUE)
  er2 <- try(as.numeric(x) + 0, silent = TRUE)

  if(!is.finite(er1) | !is.finite(er2)) stop(paste("'x' can't be coerced to a number_line object",sep=""))

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
#' is.number_line(a)
#' is.number_line(b)
#'
#' @export
is.number_line <- function(x) class(x)=="number_line"

#' @rdname number_line
#' @param x \code{number_line} object
#' @param direction Type of \code{"number_line"} objects whose start and end points will be reversed. Either \code{"increasing"} or \code{"decreasing"}.
#' @details
#' A number lines has an \code{"increasing"} direction if its start point is less than its end point.
#' It has a \code{"decreasing"} direction, if its end point is less than its start point.
#' \code{reverse} will reverse this direction. \code{direction} argument determines which type of number lines are reversed.
#'
#' @return \code{logical} object
#' @examples
#' #reverse number_line objects
#' reverse(number_line(dmy("25/04/2019"), dmy("01/01/2019")))
#' reverse(number_line(200,-100), "increasing")
#' reverse(number_line(200,-100), "decreasing")
#' @export
reverse <- function(x, direction = "both"){
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
#' @param ... arguments for particular methods.
#' @export
unique.number_line <- function(x, ...){

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)

  x <- unique(data.frame(a= x@start, z = x@start + x@.Data, row.names = x@id))

  x <- diyar::number_line(a =x$a, z= x$z, id = as.numeric(row.names(x)))

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
