#' @title Number line
#'
#' @description A function to create number lines
#' @param a Start of the number line. Should be a vector of \code{numeric}, \code{double} or \code{integer}
#' @param z End of the number line. Should be a vector of \code{numeric}, \code{double} or \code{integer}
#'
#' @return \code{number_line} object
#' @examples
#'
#' @aliases number_line
#' @examples
#' number_line(-100, 100)
#' number_line(10, 11.2)
#' number_line(100, 200)
#' number_line(100, 120)
#' number_line(50, 120)
#' number_line(100,100)
#' @export
#'

number_line <- function(a, z){
  if(!all(class(a) %in% c("integer","double","numeric"))) stop(paste("`a` must be a vector of integer, numeric or double data type"))
  if(!all(class(z) %in% c("integer","double","numeric"))) stop(paste("`z` must be a vector of integer, numeric or double data type"))

  nl <- methods::new("number_line", .Data = z - a , start=a)
  return(nl)
}

#' @slot start Start of a number line
#' @slot end End of a number line
setClass("number_line", contains = c("numeric"), slots = c(start = "numeric"))

setMethod("show", signature(object="number_line"), function(object){
  s <- ifelse(object@start + object@.Data > object@start, "->","<-")
  s <- ifelse(object@start + object@.Data == object@start, "==",s)

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
#' @export
is.number_line <- function(x) class(x)=="number_line"

#' @rdname number_line
#' @export
as.number_line <- function(a, z){
  a <- as.numeric(a)
  z <- as.numeric(z)
  nl <- methods::new("number_line", .Data = z - a , start=a)
  return(nl)
}
