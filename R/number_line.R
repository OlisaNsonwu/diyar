#' @title Number line
#'
#' @description A function to create number lines
#' @param a Start of the number line. It should a \code{numeric} object, or can be coerced to a \code{numeric} object
#' @param z End of the number line. It should a \code{numeric} object, or can be coerced to a \code{numeric} object
#'
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' library(lubridate)
#' library(diyar)
#'
#' number_line(-100, 100)
#' number_line(10, 11.2)
#' number_line(dmy("12/03/2019"), dmy("05/01/2019"))
#' number_line(dmy_hms("15/05/2019 13:15:07"), dmy_hms("15/05/2019 15:17:10"))
#' @export
#'

number_line <- function(a, z){
  ac <- suppressWarnings(as.numeric(a))
  zc <- suppressWarnings(as.numeric(z))

  if(any(!is.numeric(ac))) stop("Can't coerce `a` to start a number_line object")
  if(any(!is.numeric(zc))) stop("Can't coerce `z` to end a number_line object")

  if(all(class(a)!=class(z))) warning("Class objects of `a` and `z` are different. It may need to be reconciled")

  nl <- methods::new("number_line", .Data = as.numeric(z) - as.numeric(a) , start=a)
  return(nl)
}

#' @slot start Start of a number line
#' @slot end End of a number line
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
#' @param x object
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
#' @param x object
#' @examples
#' swap(number_line(200,-100))
#' swap(number_line(200,-100))
#' swap(number_line(dmy("25/04/2019"), dmy("01/01/2019")))
#'
#' @export
swap <- function(x){
  if(!diyar::is.number_line(x)) stop(paste("`x` is not a number_line object"))

  f <- x

  if(f@start > (f@start + f@.Data)) {
    f@start <- (x@start + x@.Data)
    f@.Data <- x@start - (x@start + x@.Data)

  }

  return(f)
}
