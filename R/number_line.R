#' @title Number line objects
#'
#' @description A range number based values on a number line .
#'
#' @details
#' A \code{number_line} object represents a series of real numbers on a number line.
#'
#' Visually, it's presented as the left (\code{l}) and right (\code{r}) points of the series. This may differ from \code{start} and \code{end} points.
#' The \code{start} point is the lowest number in the series, regardless of whether it's at the \code{left} or \code{right} point.
#'
#' The location of the \code{start} point - \code{left} or \code{right}, indicates whether it's an \code{"increasing"} or \code{"decreasing"} series.
#' This is the \code{direction} of the \code{number_line} object.
#'
#' @param l Left point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param r Right point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param id Unique \code{numeric} ID. Optional
#' @param gid Unique \code{numeric} Group ID. Optional
#'
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' date <- function(x) as.Date(x, "%d/%m/%Y")
#' dttm <- function(x) as.POSIXct(x, "UTC",format="%d/%m/%Y %H:%M:%S")
#'
#' number_line(-100, 100); number_line(10, 11.2)
#'
#' # Other numeric based object classes are also compatible
#' number_line(dttm("15/05/2019 13:15:07"), dttm("15/05/2019 15:17:10"))
#'
#' # However, a warning is given if 'l' and 'r' have different classes.
#' # Consider if this needs to be corrected.
#' number_line(2, date("05/01/2019"))
#'
#' @export
number_line <- function(l, r, id = NULL, gid = NULL){
  er1 <- try(as.numeric(l), silent = TRUE)
  er2 <- try(as.numeric(r), silent = TRUE)
  er3 <- try(as.numeric(r) - as.numeric(l), silent = TRUE)

  if(missing(l) & missing(r) & missing(id) & missing(gid)) return(new("number_line"))
  if(length(l)!=length(r)) stop("'l' and 'r' have different lengths")
  if(!is.numeric(er1) | !is.numeric(er2) | !is.numeric(er3)) stop(paste("'l' or 'r' aren't compatible for a number_line object",sep=""))
  if(!(is.numeric(id) | is.null(id))) stop(paste("'id' must be numeric",sep=""))
  if(!(is.numeric(gid) | is.null(gid))) stop(paste("'gid' must be numeric",sep=""))

  if(all(class(l)!=class(r))) warning("'l' and 'r' have different classes. It may need to be reconciled")

  if(is.null(id) | any(!is.finite(id)) ) id <- 1:length(l)
  if(is.null(gid) | any(!is.finite(gid)) ) gid <- 1:length(l)
  nl <- methods::new("number_line", .Data = as.numeric(r) - as.numeric(l), start=l, id = id, gid = gid)
  return(nl)
}


#' @rdname number_line
#' @examples
#' # Convert numeric based objects to number_line objects
#' as.number_line(5.1); as.number_line(date("21/10/2019"))
#'
#' @export
as.number_line <- function(x){

  if(missing(x)) stop("argument 'x' is missing, with no default")

  er1 <- suppressWarnings(try(as.numeric(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(x) + 0, silent = TRUE))

  if(!is.numeric(er1) | !is.numeric(er2)) stop(paste("'x' can't be coerced to a `number_line` object",sep=""))

  if(all(!diyar::is.number_line(x))){
    x[!is.finite(as.numeric(x))] <- NA
    x <- methods::new("number_line", .Data = as.numeric(x-x), start= x, id = 1:length(x), gid = 1:length(x))
  }

  return(x)
}

#' @rdname number_line
#' @examples
#' # A test for number_line objects
#' a <- number_line(0, -100)
#' b <- number_line(date("25/04/2019"), date("01/01/2019"))
#' is.number_line(a); is.number_line(b)
#'
#' @export
is.number_line <- function(x) class(x)=="number_line"

#' @rdname number_line
#' @examples
#' # Structure of a number_line object
#' left_point(a); right_point(a); start_point(a); end_point(a)
#'
#' @export
left_point <- function(x){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x@start
}

#' @rdname number_line
#' @export
right_point <- function(x){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x@start + x@.Data
}

#' @rdname number_line
#' @export
start_point <- function(x){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x <- diyar::reverse_number_line(x,"decreasing")
  x@start
}

#' @rdname number_line
#' @export
end_point <- function(x){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x <- diyar::reverse_number_line(x,"decreasing")
  x@start + x@.Data
}

#' @rdname number_line
#' @export
number_line_width <- function(x){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  diyar::right_point(x) - diyar::left_point(x)
}

#' @rdname number_line
#' @param x \code{number_line} object
#' @param direction Type of \code{"number_line"} objects to be reversed. Options are; \code{"increasing"}, \code{"decreasing"} or \code{"both"}.
#' @details
#' \code{reverse_number_line()} - reverses the direction of a \code{number_line} object. A reversed \code{number_line} object has its \code{l} and \code{r} points swapped.
#' The \code{direction} argument determines which type of \code{number_line} objects will be reversed.
#' \code{number_line} objects with non-finite numeric starts or end points i.e. (\code{NA}, \code{NaN} and \code{Inf}) can't be reversed.
#' @examples
#' # Reverse number_line objects
#' reverse_number_line(number_line(date("25/04/2019"), date("01/01/2019")))
#' reverse_number_line(number_line(200,-100), "increasing")
#' reverse_number_line(number_line(200,-100), "decreasing")
#'
#' @export
reverse_number_line <- function(x, direction = "both"){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(length(x)==0) return(x)
  if(!(length(direction) %in% c(1, length(x)) & is.character(direction))) stop(paste("'direction' must be a character of length 1"))
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
#' c <- number_line(5, 6)
#' # Towards the positive end of the number line
#' shift_number_line(x=c(c,c), by=c(2,3))
#' # Towards the negative end of the number line
#' shift_number_line(x=c(c,c), by=c(-2,-3))
#'
#' @export
shift_number_line <- function(x, by=1){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(length(x)==0) return(x)
  if(!(length(by) %in% c(1, length(x)))) stop(paste("length of 'by' must be 1 or the same as 'x'",sep=""))

  by[!is.finite(by)] <- NA_real_
  n <- ifelse(is.finite(x@start) & is.finite(x@.Data),1,0)
  by <- by * n

  x@start <- x@start + by
  return(x)
}

#' @rdname number_line
#' @param point \code{"start"} or \code{"end"} point
#' @details
#' \code{expand_number_line()} - a convenience function to increase or decrease the width or length of a \code{number_line} object.
#' @examples
#' # Change the width or length of a number_line object
#' d <- c(number_line(3,6), number_line(6,3))
#'
#' expand_number_line(d, 2)
#' expand_number_line(d, -2)
#' expand_number_line(d, c(2,-1))
#' expand_number_line(d, 2, "start")
#' expand_number_line(d, 2, "end")
#'
#' @export
expand_number_line <- function(x, by=1, point ="both"){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(length(x)==0) return(x)
  if(!all(is.character(point))) stop(paste("'point' must be a character object"))
  if(all(!tolower(point) %in% c("both","start","end","left","right"))) stop(paste("`point` must be either 'left', 'right', 'start', 'end' or 'both'"))
  if(!(length(by) %in% c(1, length(x)))) stop(paste("length of 'by' must be 1 or the same as 'x'",sep=""))
  if(!(length(point) %in% c(1, length(x)))) stop(paste("length of 'point' must be 1 or the same as 'x'",sep=""))

  point <- tolower(point)
  by[!is.finite(by)] <- NA_real_
  n <- ifelse(x@.Data<0 & is.finite(x@.Data),-1,1)
  by <- by * n
  g <- ifelse(x@.Data >=0, T,F)
  if(any(point=="both")) x@start[point == "both"] <- x@start[point == "both"] - by[point == "both"]
  if(any(point=="both")) x@.Data[point == "both"] <- x@.Data[point == "both"] + (by[point == "both"] *2)

  if(any(point %in% c("left", "start"))) x@start[point == "left" | (point == "start" & g)] <- x@start[point == "left" | (point == "start" & g)] - by[point == "left" | (point == "start" & g)]
  if(any(point %in% c("left", "start"))) x@.Data[point == "left" | (point == "start" & g)] <- x@.Data[point == "left" | (point == "start" & g)] + by[point == "left" | (point == "start" & g)]

  if(any(point %in% c("right", "end"))) x@.Data[point == "right"| (point == "end" & g)] <- x@.Data[point == "right"| (point == "end" & g)] + by[point == "right"| (point == "end" & g)]

  if(any(point=="start")) x@.Data[point == "start" & !g] <- x@.Data[point == "start" & !g] - by[point == "start" & !g]

  if(any(point=="end")) x@start[point == "end" & !g] <- x@start[point == "end" & !g] - by[point == "end" & !g]
  if(any(point=="end")) x@.Data[point == "end" & !g] <- x@.Data[point == "end" & !g] + by[point == "end" & !g]
  return(x)
}

#' @rdname number_line
#' @details
#' \code{compress_number_line()} - 'compress' or 'collapse' overlapping \code{number_line} objects into a new \code{number_line} objects that covers the \code{start} and \code{end} points of the originals.
#' This results in duplicate \code{number_line} objects with the \code{start} and \code{end} points of the new expanded \code{number_line} object.
#' See \code{\link{overlaps}} for further details on overlapping \code{number_line} objects.
#' If a familiar (but unique) \code{id} is used when creating the \code{number_line} objects,
#' \code{compress_number_line()} can be an alternative for simple implementations of \code{\link{record_group}} or \code{\link{episode_group}}.
#'
#' @param method Method of overlap. Check every pair of \code{number_line} objects with the same \code{method}. Deprecated use \code{methods} instead.
#' @param methods Methods of overlap. Check different pairs of \code{number_line} objects with the different \code{methods}
#' @param collapse If \code{TRUE}, collapse the compressed results yet again.
#' @param deduplicate if \code{TRUE}, retains only one \code{number_line} object among duplicates
#' @examples
#' # Collapse number line objects
#' x <- c(number_line(10,10), number_line(10,20), number_line(5,30),  number_line(30,40))
#' compress_number_line(x, deduplicate = FALSE)
#' compress_number_line(x)
#' compress_number_line(x, collapse=TRUE)
#' compress_number_line(x, collapse=TRUE, method = "inbetween")
#'
#' @export

compress_number_line <- function(x, method = c("exact", "across","chain","aligns_start","aligns_end","inbetween"), collapse =FALSE, deduplicate = TRUE, methods = "exact|across|chain|aligns_start|aligns_end|inbetween"){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(length(x)==0) return(x)
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(!(is.logical(collapse) & is.logical(deduplicate) )) stop(paste("'collapse' and 'deduplicate' must be TRUE or FALSE"))
  if(all(!tolower(method) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`method` must be either 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween'"))
  if(!(length(collapse) %in% c(1, length(x)))) stop(paste("length of 'collapse' must be 1 or the same as 'x'",sep=""))
  o <- unique(unlist(strsplit(methods, split="\\|")))
  o <- o[!o %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste("\n'", "Valid 'methods' are 'exact', 'across','chain','aligns_start','aligns_end' or 'inbetween' \n\n",
                              "Syntax ~ \"method1|method2|method3...\" \n",
                              "                 OR                   \n",
                              "Use ~ include_overlap_method() or exclude_overlap_method()", sep=""))
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  if(any(duplicated(x@id) | is.na(x@id))) x@id <- 1:length(x@id)
  #x <- diyar::reverse_number_line(x, "decreasing")

  j <- 1
  t <- rep(0, length(x))
  if(length(collapse)==1) collapse <- rep(collapse, length(x))
  while (min(t) ==0 & j<=length(x)){
    l <- x[t==0][1]
    h <- (x@id == l@id | diyar::overlaps(x, l, methods=m)) & ifelse(collapse, TRUE, (t!=1))

    if(length(h)>=1){
      mx_in <- ifelse(length(x[h & x@.Data >=0]) >0, max(x[h & x@.Data >=0]@.Data), 0)
      mx_dc <- ifelse(length(x[h & x@.Data  <0]) >0, max(abs(x[h & x@.Data <0]@.Data)), 0)

      if(mx_in >= mx_dc){
        x[h] <- number_line(min(diyar::start_point(x)[h]),
                            max(diyar::end_point(x)[h]),
                            gid = sort(x[h])[1]@id,
                            id = x[h]@id)
      }else{
        x[h] <- number_line(max(diyar::end_point(x)[h]),
                            min(diyar::start_point(x)[h]),
                            gid = sort(x[h])[1]@id,
                            id = x[h]@id)
      }
    }

    t[h] <- 1
    if(min(t)==1) break
    j <- j + 1
  }

  if(deduplicate) x <- unique.number_line(x)
  return(x)
}

#' @rdname number_line
#' @param by increment or decrement. Passed to \code{seq()} in \code{number_line_sequence()}
#' @param length.out desired length of the sequence. Passed to \code{seq()}
#' @details
#' \code{number_line_sequence()} - a convenience function to convert a \code{number_line} object into a sequence of finite numbers.
#' The direction of the sequence will correspond to that of the \code{number_line} object.
#' @examples
#' # Convert a number line object to its series of real numbers
#' number_line_sequence(number_line(1, 5))
#' number_line_sequence(number_line(5, 1), .5)
#' number_line_sequence(number_line(5:1, 1:5), 1:5)
#'
#' nl <- number_line(as.POSIXlt("01/04/2019 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"),
#' as.POSIXlt("04/04/2019 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
#'
#' number_line_sequence(c(nl, nl), c(episode_unit[["days"]] * 1.5, episode_unit[["hours"]] * 12))
#'
#' @export
number_line_sequence <- function(x, by=1, length.out = NULL){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))

  fn_check <- finite_check(x)
  if(fn_check!=T) stop(paste0("Finite values for 'x' required in ",fn_check))

  fn_check <- finite_check(by)
  if(fn_check!=T) stop(paste0("Finite values for 'by' required in ",fn_check))

  by <- ifelse(is.nan(x@.Data/abs(x@.Data)), by , x@.Data/abs(x@.Data)  * abs(by))
  if(is.null(length.out)){
    mapply(seq, from=left_point(x), to = right_point(x), by=by, SIMPLIFY = F)
  }else{
    mapply(seq, from=diyar::left_point(x), to = diyar::right_point(x), length.out = length.out, SIMPLIFY = F)
  }
}
