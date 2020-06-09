#' @title Set operations on number line objects
#' @name set_operations
#' @description Perform set operations on a pair of \code{number_line} objects.
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @aliases set_operations
#' @return \code{number_line}; \code{list}
#'
#' @details
#'
#' \bold{\code{union_number_lines()}} - Combined range of \code{x} and \code{y}
#'
#' \bold{\code{intersect_number_line()}} - Subset of \code{x} that overlaps with \code{y} and vice versa
#'
#' \bold{\code{subtract_number_lines()}} - Subset of \code{x} that does not overlap with \code{y} and vice versa.
#' Returns a list with two elements;
#' \itemize{
#' \item \code{n1} - subset before the overlapped range
#' \item \code{n2} - subset before the overlapped range
#' }
#'
#' The \code{direction} of the returned \code{number_line} will be that of the widest one (\code{x} or \code{y}).
#' If \code{x} and \code{y} have the same length, it'll be an \code{"increasing direction"}.
#'
#' If  \code{x} and \code{y} do not overlap, \code{NA} (\code{"NA ?? NA"}) is returned.
#'
#' @seealso
#' \code{\link{number_line}} and \code{\link{overlaps}}
#'
#' @examples
#'
#' nl_1 <- c(number_line(1, 5), number_line(1, 5), number_line(5, 9))
#' nl_2 <- c(number_line(1, 2), number_line(2, 3), number_line(0, 6))
#'
#' # Union
#' nl_1; nl_2; union_number_lines(nl_1, nl_2)
#'
#' @export
union_number_lines <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(length(x) ==0 & length(y)==0) return(logical())
  if(length(x)!= length(y)) stop(paste("length('x') must be equal to length('y')"))

  x <- c(x[1], x); y <- c(x[1], y)
  o <- c(x[1], diyar::as.number_line(rep(NA_real_, length(x)-1)))

  lg <- which(diyar::overlap(x, y))

  o[lg] <- number_line(
    start_point(number_line(start_point(x[lg]), start_point(y[lg]))),
    end_point(number_line(end_point(x[lg]), end_point(y[lg])))
  )

  dir <- ifelse(abs(x@.Data[lg]) > abs(y@.Data[lg]), x@.Data[lg]/abs(x@.Data[lg]), y@.Data[lg]/abs(y@.Data[lg]))
  o[lg] <- diyar::reverse_number_line(o[lg], direction= ifelse(dir==1,"decreasing","increasing"))

  return(o[-1])
}

#' @rdname set_operations
#' @examples
#'
#' nl_1 <- number_line(as.Date(c("01/01/2020", "03/01/2020","09/01/2020"), "%d/%m/%Y"),
#'                     as.Date(c("09/01/2020", "09/01/2020","25/12/2020"), "%d/%m/%Y"))
#'
#' nl_2 <- number_line(as.Date(c("04/01/2020","01/01/2020","01/01/2020"), "%d/%m/%Y"),
#'                     as.Date(c("05/01/2020","05/01/2020","03/01/2020"), "%d/%m/%Y"))
#'
#' # Intersect
#' nl_1; nl_2; intersect_number_lines(nl_1, nl_2)
#' @export
intersect_number_lines <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(length(x) ==0 & length(y)==0) return(logical())
  if(length(x)!= length(y)) stop(paste("length('x') must be equal to length('y')"))

  x <- c(x[1], x); y <- c(x[1], y)
  o <- c(x[1], diyar::as.number_line(rep(NA_real_, length(x)-1)))

  lg <- which(diyar::overlap(x, y))

  cnd <- lg[which(diyar::overlaps(as.number_line(start_point(x[lg])), y[lg]))]
  o[cnd] <- number_line(l= start_point(x[cnd]), r = right_point(o[cnd]))

  cnd <- lg[which(diyar::overlaps(as.number_line(start_point(y[lg])), x[lg]))]
  o[cnd] <- number_line(l= start_point(y[cnd]), r = right_point(o[cnd]))

  cnd <- lg[which(diyar::overlaps(as.number_line(end_point(x[lg])), y[lg]))]
  o[cnd] <- number_line(l= end_point(x[cnd]), r = left_point(o[cnd]))

  cnd <- lg[which(diyar::overlaps(as.number_line(end_point(y[lg])), x[lg]))]
  o[cnd] <- number_line(l= end_point(y[cnd]), r = left_point(o[cnd]))

  dir <- ifelse(abs(x@.Data[lg]) > abs(y@.Data[lg]), x@.Data[lg]/abs(x@.Data[lg]), y@.Data[lg]/abs(y@.Data[lg]))
  o[lg] <- diyar::reverse_number_line(o[lg], direction= ifelse(dir==1,"decreasing","increasing"))
  return(o[-1])
}

#' @rdname set_operations
#' @examples
#'
#' # Subtract
#' nl_1; nl_2; subtract_number_lines(nl_1, nl_2)
#'
#' @export
subtract_number_lines <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(length(x) ==0 & length(y)==0) return(logical())
  if(length(x)!= length(y)) stop(paste("length('x') must be equal to length('y')"))

  x <- c(x[1], x); y <- c(x[1], y)
  o2 <- o1 <- c(x[1], diyar::as.number_line(rep(NA_real_, length(x)-1)))

  lg <- which(diyar::overlap(x, y))

  cnd <- lg[which(!diyar::overlaps(as.number_line(start_point(x[lg])), y[lg]))]
  o1[cnd] <- number_line(l= start_point(x[cnd]), r = start_point(y[cnd]))

  cnd <- lg[which(!diyar::overlaps(as.number_line(start_point(y[lg])), x[lg]))]
  o1[cnd] <- number_line(l= start_point(y[cnd]), r = start_point(x[cnd]))

  cnd <- lg[which(!diyar::overlaps(as.number_line(end_point(x[lg])), y[lg]))]
  o2[cnd] <- number_line(l= end_point(y[cnd]), r = end_point(x[cnd]))

  cnd <- lg[which(!diyar::overlaps(as.number_line(end_point(y[lg])), x[lg]))]
  o2[cnd] <- number_line(l= end_point(x[cnd]), r = end_point(y[cnd]))

  list(
    n1 = o1[-1],
    n2 = o2[-1]
  )
}

