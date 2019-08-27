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

  nl <- new("number_line", a, end=z)
  return(nl)
}

#' @slot start Start of a number line
#' @slot end End of a number line
setClass("number_line", contains = c("numeric"), slots = c(end = "numeric"))

fmt_number_line <- function(x) print(paste(x@.Data, x@end, sep=" -> "))
setMethod("show", signature(object="number_line"), function(object){print(fmt_number_line(object))} )


setMethod("rep", signature(x = "number_line"), function(x, ...) {
  new("number_line", rep(x@.Data, ...), end = rep(x@end, ...))
})

setMethod("[", signature(x = "number_line"),
          function(x, i, j, ..., drop = TRUE) {
            new("number_line", x@.Data[i], end = x@end[i])
          }
)

setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            new("number_line", x@.Data[i], end = x@end[i])
          }
)

setMethod("[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  x@.Data[i] <- value@.Data
  x@end[i] <- value@.end
  new("number_line", x@.Data, end = x@end)
})

setMethod("[[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  x@.Data[i] <- value@.Data
  x@end[i] <- value@.end
    new("number_line", x@.Data, end = x@end)
})

setMethod("$", signature(x = "number_line"), function(x, name) {
  slot(x, name)
})

setMethod("$<-", signature(x = "number_line"), function(x, name, value) {
  slot(x, name) <- value
  x
})

#' @rdname number_line
#' @export
is.number_line <- function(x) class(x)=="number_line"
