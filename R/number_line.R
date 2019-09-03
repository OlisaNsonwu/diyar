#' @title Number line
#'
#' @description A set of function to create and manipulate \code{number_line} objects
#' @param a Start of the number line. Should be, or can be coerced to a \code{numeric} object
#' @param z End of the number line. Should be, or can be coerced to a \code{numeric} object
#'
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' library(lubridate)
#'
#' number_line(-100, 100)
#' number_line(10, 11.2)
#' number_line(dmy("12/03/2019"), dmy("05/01/2019"))
#' number_line(dmy_hms("15/05/2019 13:15:07"), dmy_hms("15/05/2019 15:17:10"))
#' @export
#'

number_line <- function(a, z){

  if(all(class(a)!=class(z))) warning("'a' and 'z' have different classes. It may need to be reconciled")

  nl <- methods::new("number_line", .Data = as.numeric(z) - as.numeric(a) , start=a)
  return(nl)
}

#' @slot start Start of a number line
#' @export
setClass("number_line", contains = c("ANY"), slots = c(start = "ANY"))

setMethod("show", signature(object="number_line"), function(object){
  s <- ifelse(object@start + object@.Data > object@start, "->","<-")
  s <- ifelse(object@start + object@.Data == object@start, "==",s)
  s <- ifelse(is.na(object@start + object@.Data) , "??",s)

  print(paste(object@start, s, object@start + object@.Data, sep=" "))}
  )

setMethod("rep", signature(x = "number_line"), function(x, ...) {
  methods::new("number_line", rep(x@.Data, ...), start = rep(x@start, ...))
})

setMethod("[", signature(x = "number_line"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i])
          }
)

setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i])
          }
)

setMethod("$", signature(x = "number_line"), function(x, name) {
  methods::slot(x, name)
})

setMethod("$<-", signature(x = "number_line"), function(x, name, value) {
  methods::slot(x, name) <- value
  x
})

#' @rdname number_line
#' @param x \code{R} object
#'
#' @examples
#' a <- number_line(0, -100)
#' b <- number_line(dmy("25/04/2019"), dmy("01/01/2019"))
#' is.number_line(a)
#' is.number_line(b)
#'
#' @export
is.number_line <- function(x) class(x)=="number_line"

#' @rdname number_line
#' @param x \code{number_line} object
#' @param direction Either \code{"increasing"} or \code{"decreasing"}. Type of \code{"number_line"} objects whose start and end points will be reversed
#' @examples
#'
#' reverse(number_line(dmy("25/04/2019"), dmy("01/01/2019")))
#' reverse(number_line(200,-100), "increasing")
#' reverse(number_line(200,-100), "decreasing")
#'
#' @export
reverse <- function(x, direction = "both"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!(length(direction)==1 & is.character(direction))) stop(paste("'direction' must be a character of length 1"))
  if(!tolower(direction) %in% c("increasing","decreasing","both") ) stop(paste("`direction` must be either 'increasing', 'decreasing', or 'both'"))
  f <- x

  if(tolower(direction) == "decreasing"){
    f@.Data <- ifelse(x@.Data <0, -x@.Data, x@.Data)
    c <- ifelse(x@.Data <0, x@.Data, 0)
  }else if(tolower(direction) == "increasing"){
    f@.Data <- ifelse(x@.Data >0, -x@.Data, x@.Data)
    c <- ifelse(x@.Data >0, x@.Data, 0)
  } else if(tolower(direction) == "both"){
    f@.Data <- -x@.Data
    c <- x@.Data
  }

  f@start <- f@start + c

  return(f)
}
