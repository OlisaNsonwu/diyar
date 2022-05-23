#' @title \code{number_line}
#'
#' @description A range of \code{numeric} values.
#'
#' @details
#' A \code{number_line} object represents a range of numbers.
#' It is made up of a \code{start} and \code{end} point as the lower and upper ends of the range respectively.
#' The location of the \code{start} point - \code{left} or \code{right},
#' determines whether it is an \code{"increasing"} or \code{"decreasing"} \code{number_line}.
#' This is the \code{direction} of the \code{number_line}.
#'
#' @seealso
#' \code{\link{overlaps}}; \code{\link{set_operations}}; \code{\link{episodes}}; \code{\link{links}}
#'
#' @param l \code{[numeric-based]}. Left point of the \code{number_line}.
#' @param r \code{[numeric-based]}. Right point of the \code{number_line}. Must be able to be coerced to a \code{numeric} object.
#' @param id \code{[integer]}. Unique element identifier. Optional.
#' @param gid \code{[integer]}. Unique group identifier. Optional.
#'
#' @return \code{number_line}
#'
#' @aliases number_line
#' @examples
#' number_line(-100, 100)
#'
#' # Also compatible with other numeric based object classes
#' number_line(as.POSIXct("2019-05-15 13:15:07", tz = "UTC"),
#'             as.POSIXct("2019-05-15 15:17:10", tz = "UTC"))
#'
#' @export
number_line <- function(l, r, id = NULL, gid = NULL){
  if(missing(l) & missing(r)) return(new("number_line"))
  if(length(l) == 0 & length(r) == 0) return(new("number_line"))

  er1 <- suppressWarnings(try(as.numeric(l), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(r), silent = TRUE))
  er3 <- suppressWarnings(try(as.numeric(r) - as.numeric(l), silent = TRUE))

  mxa <- max(c(length(l), length(r), length(id), length(gid)))
  if(length(l) == 1) l <- rep(l, mxa)
  if(length(r) == 1) r <- rep(r, mxa)
  if(length(id) == 1) id <- rep(id, mxa)
  if(length(gid) == 1) gid <- rep(gid, mxa)
  if(is.null(id) | any(!is.finite(id)) ) id <- 1:length(l)
  if(is.null(gid) | any(!is.finite(gid)) ) gid <- 1:length(l)
  if(length(l)!= mean(c(length(r),length(id),length(gid)))) stop("Argument lengths differ or are not equal to 1", call. = FALSE)
  if(!is.numeric(er1) | !is.numeric(er2) | !is.numeric(er3)) stop(paste0("'l' or 'r' aren't compatible for a `number_line` object"), call. = FALSE)
  if(!(is.numeric(id) | is.null(id))) stop(paste0("'id' must be `numeric`"), call. = FALSE)
  if(!(is.numeric(gid) | is.null(gid))) stop(paste0("`gid` must be `numeric`"), call. = FALSE)
  if(all(class(l) != class(r))) warning("`l` and `r` have different classes. They may need to be reconciled.", call. = FALSE)

  nl <- suppressWarnings(methods::new("number_line",
                                      .Data = as.numeric(ifelse(as.numeric(r) == as.numeric(l) & !is.na(r), 0, as.numeric(r) - as.numeric(l))),
                                      start = l,
                                      id = as.integer(id),
                                      gid = as.integer(gid)))
  return(nl)
}


#' @rdname number_line
#' @examples
#' # Coerce compatible object classes to `number_line` objects
#' as.number_line(5.1); as.number_line(as.Date("2019-10-21"))
#'
#' @export
as.number_line <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  if(all(class(x) == "number_line")) return(x)
  er1 <- suppressWarnings(try(as.numeric(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(x) + 0, silent = TRUE))

  if(!is.numeric(er1) | !is.numeric(er2)) stop("`x` can't be coerced to a `number_line` object.", call. = FALSE)
  if(all(!is.number_line(x))){
    x <- methods::new("number_line",
                      .Data = rep(0, length(x)),
                      start = x,
                      id = 1:length(x),
                      gid = 1:length(x))
  }

  return(x)
}

#' @rdname number_line
#' @examples
#' # A test for number_line objects
#' a <- number_line(as.Date("2019-04-25"), as.Date("2019-01-01"))
#' is.number_line(a)
#'
#' @export
is.number_line <- function(x) all(class(x) == "number_line")

#' @rdname number_line
#' @examples
#' # Structure of a number_line object
#' left_point(a); right_point(a); start_point(a); end_point(a)
#'
#' @export
left_point <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  x@start
}

#' @rdname number_line
#' @param value {[\code{numeric} based]}
#' @export
"left_point<-" <- function(x, value) {
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(number_line())
  x@.Data <- as.numeric(x@start +x@.Data) - as.numeric(value)
  x@start <- (as.numeric(x@start) * 0) + value
  x
}

#' @rdname number_line
#' @export
right_point <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  x@start + x@.Data
}

#' @rdname number_line
#' @export
"right_point<-" <- function(x, value) {
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(number_line())
  x@.Data <- as.numeric(value) - as.numeric(x@start)
  x
}

#' @rdname number_line
#' @export
start_point <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  y <- x@start
  indx <- which(x@.Data < 0)
  y[indx] <- x@start[indx] + x@.Data[indx]
  rm(x, indx)
  return(y)
}

#' @rdname number_line
#' @export
"start_point<-" <- function(x, value) {
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)

  value <- mk_lazy_opt(value)
  lgk <- x@.Data < 0
  x@.Data[!lgk] <- as.numeric(x@start[!lgk] + x@.Data[!lgk]) - value[!lgk]
  x@start[!lgk] <- (as.numeric(x@start[!lgk]) * 0) + value[!lgk]
  x@.Data[lgk] <- as.numeric(value[lgk]) - as.numeric(x@start[lgk])
  x
}

#' @rdname number_line
#' @export
end_point <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  y <- x@start + x@.Data
  indx <- which(x@.Data < 0)
  y[indx] <- x@start[indx]
  rm(x, indx)
  return(y)
}

#' @rdname number_line
#' @export
"end_point<-" <- function(x, value) {
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)

  value <- mk_lazy_opt(value)
  lgk <- x@.Data < 0
  x@.Data[lgk] <- as.numeric(x@start[lgk] + x@.Data[lgk]) - value[lgk]
  x@start[lgk] <- (as.numeric(x@start[lgk]) * 0) + value[lgk]
  x@.Data[!lgk] <- as.numeric(value[!lgk]) - as.numeric(x@start[!lgk])
  x
}

#' @rdname number_line
#' @export
number_line_width <- function(x){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  right_point(x) - left_point(x)
}

#' @rdname number_line
#' @param x \code{[number_line]}
#' @param direction \code{[character]}. Type of \code{number_line} reverse.
#' Options are; \code{"increasing"}, \code{"decreasing"} or \code{"both"} (default).
#' @details
#' \bold{\code{reverse_number_line()}} - reverse the direction of a \code{number_line}.
#' A reversed \code{number_line} has its \code{left} and \code{right} points swapped.
#' The \code{direction} argument specifies which type of \code{number_line} will be reversed.
#' \code{number_line} with non-finite \code{start} or \code{end} points (i.e. \code{NA}, \code{NaN} and \code{Inf}) can't be reversed.
#' @examples
#' # Reverse number_line objects
#' reverse_number_line(number_line(as.Date("2019-04-25"), as.Date("2019-01-01")))
#' reverse_number_line(number_line(200, -100), "increasing")
#' reverse_number_line(number_line(200, -100), "decreasing")
#'
#' @export
reverse_number_line <- function(x, direction = "both"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  err <- err_match_ref_len(direction, "x", c(1, length(x)), "direction")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(direction, "direction", "character")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_invalid_opts(direction, "direction", c("increasing","decreasing","both"))
  if(err != FALSE) stop(err, call. = FALSE)

  direction <- tolower(direction)
  fnt <- is.finite(as.numeric(x@start)) & is.finite(as.numeric(x@.Data))

  y <- x
  indx <- which(((direction %in% c("decreasing", "both") & y@.Data < 0) |
                  (direction %in% c("increasing", "both") & y@.Data > 0)) & fnt)

  left_point(x[indx]) <- right_point(y[indx])
  right_point(x[indx]) <- left_point(y[indx])

  rm(indx, y)
  return(x)
}

#' @rdname number_line
#' @details
#' \bold{\code{shift_number_line()}} - Shift a \code{number_line} towards the positive or negative end of the number line.
#' @examples
#' c <- number_line(5, 6)
#' # Shift number_line objects towards the positive end of the number line
#' shift_number_line(x = c(c, c), by = c(2, 3))
#' # Shift number_line objects towards the negative end of the number line
#' shift_number_line(x = c(c, c), by = c(-2, -3))
#'
#' @export
shift_number_line <- function(x, by = 1){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  err <- err_match_ref_len(by, "x", c(1, length(x)), "by")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(by, "by", c("numeric", "integer"))
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_missing_check(by, "by")
  if(err != FALSE) stop(err, call. = FALSE)

  by[!is.finite(by)] <- NA_real_
  n <- is.finite(x@start) & is.finite(x@.Data)
  by <- by * n

  x@start <- x@start + by
  return(x)
}

#' @rdname number_line
#' @param point \code{[character]}. \code{"start"}, \code{"end"}, \code{"left"} or \code{"right"} point.
#' @details
#' \bold{\code{expand_number_line()}} - Increase or decrease the width of a \code{number_line}.
#' @examples
#' # Change the duration, width or length of a number_line object
#' d <- c(number_line(3, 6), number_line(6, 3))
#'
#' expand_number_line(d, 2)
#' expand_number_line(d, -2)
#' expand_number_line(d, c(2,-1))
#' expand_number_line(d, 2, "start")
#' expand_number_line(d, 2, "end")
#'
#' @export
expand_number_line <- function(x, by = 1, point = "both"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  err <- err_match_ref_len(by, "x", c(1, length(x)), "by")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(point, "x", c(1, length(x)), "point")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(by, "by", c("numeric", "integer"))
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_missing_check(by, "by")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(point, "point", "character")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_invalid_opts(point, "point", c("both","start","end","left","right"))
  if(err != FALSE) stop(err, call. = FALSE)

  point <- tolower(point)
  by[!is.finite(by)] <- NA_real_
  n <- ifelse(x@.Data<0 & is.finite(x@.Data),-1,1)
  by <- by * n
  g <- ifelse(x@.Data >= 0, T,FALSE)
  if(any(point == "both")) x@start[point == "both"] <- x@start[point == "both"] - by[point == "both"]
  if(any(point == "both")) x@.Data[point == "both"] <- x@.Data[point == "both"] + (by[point == "both"] *2)

  if(any(point %in% c("left", "start"))) x@start[point == "left" | (point == "start" & g)] <- x@start[point == "left" | (point == "start" & g)] - by[point == "left" | (point == "start" & g)]
  if(any(point %in% c("left", "start"))) x@.Data[point == "left" | (point == "start" & g)] <- x@.Data[point == "left" | (point == "start" & g)] + by[point == "left" | (point == "start" & g)]

  if(any(point %in% c("right", "end"))) x@.Data[point == "right"| (point == "end" & g)] <- x@.Data[point == "right"| (point == "end" & g)] + by[point == "right"| (point == "end" & g)]

  if(any(point == "start")) x@.Data[point == "start" & !g] <- x@.Data[point == "start" & !g] - by[point == "start" & !g]

  if(any(point == "end")) x@start[point == "end" & !g] <- x@start[point == "end" & !g] - by[point == "end" & !g]
  if(any(point == "end")) x@.Data[point == "end" & !g] <- x@.Data[point == "end" & !g] + by[point == "end" & !g]
  return(x)
}

#' @rdname number_line
#' @details
#' \bold{\code{invert_number_line()}} - Change the \code{left} or \code{right} points from a negative to positive value or vice versa.
#' @examples
#' # Invert `number_line` objects
#' e <- c(number_line(3, 6), number_line(-3, -6), number_line(-3, 6))
#' e
#' invert_number_line(e)
#' invert_number_line(e, "start")
#' invert_number_line(e, "end")
#'
#' @export
invert_number_line <- function(x, point = "both"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  err <- err_match_ref_len(point, "x", c(1, length(x)), "point")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(point, "point", "character")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_invalid_opts(point, "point", c("both","start","end","left","right"))
  if(err != FALSE) stop(err, call. = FALSE)

  point <- tolower(point)
  if(length(point) == 1){
    point <- rep(point, length(x))
  }

  x@start <- as.numeric(as.numeric(x@start))
  indx <- which(point %in% c("left", "both"))
  left_point(x[indx]) <- -x@start[indx]
  indx <- which(point %in% c("right", "both"))
  right_point(x[indx]) <- -(x@start[indx] + x@.Data[indx])
  indx <- which(point == "start")
  start_point(x[indx]) <- -start_point(x[indx])
  indx <- which(point == "end")
  end_point(x[indx]) <- -end_point(x[indx])

  return(x)
}
# @details
# \bold{\code{compress_number_line()}} - \code{"compress"} or \code{"collapse"} overlapping \code{number_line} into a new \code{number_line} that covers the \code{start} and \code{end} points of the originals.
# This results in duplicate \code{number_line} with the \code{start} and \code{end} points of the newly expanded \code{number_line}
# See \code{\link{overlaps}} for further details on overlapping \code{number_line}.
# \bold{\code{compress_number_line}} can be an alternative for simple implementations of \code{\link{links}} or \code{\link{episodes}}
#
# @param method \code{[character]}. Method of overlap. Check every pair of \code{number_line} by the same \code{method}. Deprecated. Please use \code{methods} instead.
# @param methods \code{[character]}. Methods of overlap. Check different pairs of \code{number_line} by different \code{methods}
# @param collapse \code{[lgocial]}. If \code{TRUE}, collapse the compressed results yet again.
# @param deduplicate \code{[lgocial]}. If \code{TRUE}, retains only one \code{number_line} per overlapping set
#
# @examples
# # Collapse `number_line` objects
# x <- c(number_line(10,10), number_line(10,20), number_line(5,30),  number_line(30,40))
# compress_number_line(x, deduplicate = FALSE)
# compress_number_line(x)
# compress_number_line(x, collapse=TRUE)
# compress_number_line(x, collapse=TRUE, methods = "inbetween")
compress_number_line <- function(x, methods = 8, collapse = FALSE,
                                 deduplicate = TRUE,  method = 8){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  err <- err_match_ref_len(m, "", 1, "methods")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(m, "methods", c("character", "numeric", "integer"))
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_overlap_methods_1(overlap_methods = m, "methods")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(collapse, "collapse", "logical")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(collapse, "", 1, "collapse")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_missing_check(collapse, "collapse")
  if(err != FALSE) stop(err, call. = FALSE)

  err <- err_object_types(deduplicate, "deduplicate", "logical")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(deduplicate, "", 1, "deduplicate")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_missing_check(deduplicate, "deduplicate")
  if(err != FALSE) stop(err, call. = FALSE)

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)

  j <- 1
  t <- rep(0, length(x))
  if(length(collapse) == 1) collapse <- rep(collapse, length(x))
  while (min(t) == 0 & j <= length(x)){
    l <- x[t == 0][1]
    lg <- overlaps(x, l, methods = m)
    lg[is.na(lg)] <- F
    h <- (x@id == l@id | lg) & ifelse(collapse, TRUE, (t != 1))

    if(length(h)>=1){
      mx_in <- ifelse(length(x[h & x@.Data >= 0]) > 0, max(x[h & x@.Data >= 0]@.Data), 0)
      mx_dc <- ifelse(length(x[h & x@.Data  < 0]) > 0, max(abs(x[h & x@.Data < 0 ]@.Data)), 0)

      if(mx_in >= mx_dc){
        x[h] <- number_line(min(start_point(x)[h]),
                            max(end_point(x)[h]),
                            gid = sort(x[h])[1]@id,
                            id = x[h]@id)
      }else{
        x[h] <- number_line(max(end_point(x)[h]),
                            min(start_point(x)[h]),
                            gid = sort(x[h])[1]@id,
                            id = x[h]@id)
      }
    }

    t[h] <- 1
    if(min(t) == 1) break
    j <- j + 1
  }

  if(deduplicate) x <- unique.number_line(x)
  return(x)
}

#' @rdname number_line
#' @param by \code{[integer]}. Increment or decrement. Passed to \code{seq()} in \code{number_line_sequence()}.
#' @param length.out \code{[integer]}. Number of splits. For example, \code{1} for two parts or \code{2} for three parts. Passed to \code{seq()}.
#' @param fill \code{[logical]}. Retain (\code{TRUE}) or drop (\code{FALSE}) the remainder of an uneven split.
#' @param simplify \code{[logical]}. If \code{TRUE}, returns a sequence of finite numbers.
#'
#' @details
#' \bold{\code{number_line_sequence()}} - Split a \code{number_line} into equal parts (\code{length.out}) or by a fixed recurring width (\code{by}).
#'
#' @examples
#' # Split number line objects
#' x <- number_line(Sys.Date() - 5, Sys.Date())
#' x
#' number_line_sequence(x, by = 2)
#' number_line_sequence(x, by = 4)
#' number_line_sequence(x, by = 4, fill = FALSE)
#' number_line_sequence(x, length.out = 2)
#' @export
#'
number_line_sequence <- function(x,
                                 by = NULL,
                                 length.out = 1,
                                 fill = TRUE,
                                 simplify = FALSE){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)

  errs <- err_split_nl_1(x = x,
                         by = by,
                         length.out = length.out,
                         fill = fill,
                         simplify = simplify)

  if(errs != FALSE) stop(errs, call. = FALSE)
  # change_dir <- x@.Data < 0
  change_dir <- FALSE
  if(!is.null(by)){
    if(length(fill) == 1) fill <- rep(fill, length(x))
    seq.dyr <- function(..., to = 1L, fill = TRUE){
      x <- seq(..., to = to)
      if(isTRUE(fill) & to != x[length(x)]) x <- c(x, to)
      x
    }
    x <- mapply(seq.dyr, from = left_point(x), to = right_point(x), by = by, SIMPLIFY = FALSE, fill = fill)
  }else{
    x <- mapply(seq, from = left_point(x), to = right_point(x), length.out = length.out + 1, SIMPLIFY = FALSE)
  }

  if(isTRUE(simplify)){
    return(
      if(length(x) == 1){
        x[[1]]
      }else{
        x
      }
    )
  }

  split_nl <- function(set, change_dir){
    if(length(set) == 1){
      as.number_line(set)
    }else{
      number_line(set[-length(set)], set[-1])
    }
  }

  x <- mapply(split_nl, x, change_dir, SIMPLIFY = FALSE)

  if(length(x) == 1){
    x[[1]]
  }else{
    x
  }
}

