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
#' This is referred to as the \code{direction} of the \code{number_line} object.
#'
#' @param l Left point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param r Right point of the \code{number_line} object. Should be, or can be coerced to a \code{numeric} object
#' @param id Unique \code{numeric} ID. Providing this is optional
#' @param gid Unique \code{numeric} Group ID. Providing this is optional
#' @return \code{number_line} object
#'
#' @aliases number_line
#' @examples
#' library(lubridate)
#'
#' number_line(-100, 100); number_line(10, 11.2)
#'
#' # Other numeric based object classes are also compatible
#' number_line(dmy_hms("15/05/2019 13:15:07"), dmy_hms("15/05/2019 15:17:10"))
#'
#' # However, a warning is given if 'l' and 'r' have different classes.
#' # Consider if this needs to be corrected.
#' number_line(2, dmy("05/01/2019"))
#'
#' @export
number_line <- function(l, r, id = NULL, gid = NULL){
  er1 <- try(as.numeric(l), silent = TRUE)
  er2 <- try(as.numeric(r), silent = TRUE)
  er3 <- try(as.numeric(r) - as.numeric(l), silent = TRUE)

  if(missing(l) & missing(r) & missing(id) & missing(gid)) return(new("number_line"))
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
#' as.number_line(5.1); as.number_line(dmy("21/10/2019"))
#'
#' @export
as.number_line <- function(x){
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
#' b <- number_line(dmy("25/04/2019"), dmy("01/01/2019"))
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
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x@start
}

#' @rdname number_line
#' @export
right_point <- function(x){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x@start + x@.Data
}

#' @rdname number_line
#' @export
start_point <- function(x){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x <- diyar::reverse_number_line(x,"decreasing")
  x@start
}

#' @rdname number_line
#' @export
end_point <- function(x){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  x <- diyar::reverse_number_line(x,"decreasing")
  x@start + x@.Data
}

#' @rdname number_line
#' @export
number_line_width <- function(x){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  diyar::right_point(x) - diyar::left_point(x)
}

#' @rdname number_line
#' @param x \code{number_line} object
#' @param direction Type of \code{"number_line"} objects whose direction are to be reversed. Options are; \code{"increasing"}, \code{"decreasing"} or \code{"both"}.
#' @details
#' \code{reverse_number_line()} - reverses the direction of a \code{number_line} object. A reversed \code{number_line} object has its \code{l} and \code{r} points swapped but maintains the same width or length.
#' The \code{direction} argument determines which type of \code{number_line} objects will be reversed.
#' \code{number_line} objects with non-finite numeric starts or end points i.e. (\code{NA}, \code{NaN} and \code{Inf}) can't be reversed.
#' @examples
#' # Reverse number_line objects
#' reverse_number_line(number_line(dmy("25/04/2019"), dmy("01/01/2019")))
#' reverse_number_line(number_line(200,-100), "increasing")
#' reverse_number_line(number_line(200,-100), "decreasing")
#'
#' @export
reverse_number_line <- function(x, direction = "both"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
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
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
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
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  if(!all(is.character(point))) stop(paste("'point' must be a character object"))
  if(all(!tolower(point) %in% c("both","start","end"))) stop(paste("`point` must be either 'start','end' or 'both'"))
  if(!(length(by) %in% c(1, length(x)))) stop(paste("length of 'by' must be 1 or the same as 'x'",sep=""))

  by[!is.finite(by)] <- NA_real_
  n <- ifelse(x@.Data<0 & is.finite(x@.Data),-1,1)
  by <- by * n

  if(point == "both"){
    x@start <- x@start - by
    x@.Data <- x@.Data + (by *2)
  }
  if(point == "start"){
    x@start <- x@start - by
    x@.Data <- x@.Data + by
  }
  if(point == "end"){
    x@.Data <- x@.Data + by
  }

  return(x)
}

#' @rdname number_line
#' @details
#' \code{compress_number_line()} - collapses overlapping \code{number_line} objects into a new \code{number_line} objects that covers the start and end points of the originals.
#' This results in duplicate \code{number_line} objects with start and end points of the new expanded \code{number_line} object.
#' See \code{\link{overlap}} for further details on overlapping \code{number_line} objects.
#' If a familiar (but unique) \code{id} is used when creating the \code{number_line} objects,
#' \code{compress_number_line()} can be a simple alternative to \code{\link{record_group}} or \code{\link{episode_group}}.
#'
#' @param method Method of overlap. Check each pair of \code{number_line} objects with the same set of \code{method}. Deprecated use \code{methods} instead.
#' @param methods Methods of overlap. Check multiple pairs of \code{number_line} objects with the different sets of \code{methods}
#' @param collapse If \code{TRUE}, collapse the compressed results based on \code{method} of overlaps
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
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
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
    h <- (x@id == l@id | diyar::overlap(x, l, methods=m)) & ifelse(collapse, TRUE, (t!=1))

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
#' @param by number: increment of the sequence. Passed to \code{seq()}
#' @param length.out desired length of the sequence. Passed to \code{seq()}
#' @details
#' \code{number_line_sequence()} - a convenience function to convert a \code{number_line} object into a sequence of finite numbers. The sequence will also include the start and end points.
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
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object",sep=""))
  # if(!all(is.finite(by))) stop(paste("'by' must be a finite number",sep=""))

  fn_check <- finite_check(x)
  if(fn_check!=T) stop(paste0("Finite values of 'x' required in indexes ",fn_check))

  fn_check <- finite_check(by)
  if(fn_check!=T) stop(paste0("Finite values of 'by' required in indexes ",fn_check))

  by <- ifelse(is.nan(x@.Data/abs(x@.Data)), by , x@.Data/abs(x@.Data)  * abs(by))
  if(is.null(length.out)){
    mapply(seq, from=left_point(x), to = right_point(x), by=by, SIMPLIFY = F)
  }else{
    mapply(seq, from=left_point(x), to = right_point(x), by=by, length.out = length.out, SIMPLIFY = F)
  }
}

# @rdname number_line
# @param x \code{number_line} object
# @details
# \code{plot_number_line()} - visulaises \code{number_line} objects and how they've overlapped with each other.
# @examples
# d <- c(number_line(1,5), number_line(1,2), number_line(2,3),
# number_line(4,7), number_line(4,5), number_line(1,5))
# diyar:::plot_number_line(d[c(1,6)])
# diyar:::plot_number_line(d[c(2,3)])
# diyar:::plot_number_line(d[c(1,3)])
# diyar:::plot_number_line(d[c(1:3)])

plot_number_line <- function(x, show_overlap = FALSE){
  df <- diyar::to_df(x)
  df$x <- x
  df$y <- diyar::reverse_number_line(df$x, "decreasing")

  # df <- df[order(-(df$end-df$start)), ]
  #x <- x[order(-(df$end-df$start))]

  #df$sn <- 1:nrow(df) * 1
  df$sn <- 1
  sn_change <- 1
  #df <- df[nrow(df):1,]
  while (max(sn_change) ==1) {
    df$c <- diyar::compress_number_line(df$y, deduplicate = F)

    ord <- lapply(split(1:nrow(df), paste0(df$c@gid,"-", df$sn)), order)
    lk_sn <- lapply(split(df$sn, paste0(df$c@gid)), function(x){
      rep(max(x), length(x))
    })

    df$ord <- unsplit(ord, paste0(df$c@gid,"-", df$sn))
    df$lk_sn <- unsplit(lk_sn, paste0(df$c@gid))
    new_sn <- ifelse(df$ord==2, df$lk_sn+1, df$sn)

    sn_change <- ifelse(df$sn != new_sn,1,0)
    df$sn <- new_sn
  }

  gid <- T

  pl_cols <- grDevices::colours()
  pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,3))]
  df$cols <- ifelse(gid==rep(T, nrow(df)), rev(pl_cols)[as.numeric(as.factor(df$gid))], rev(pl_cols)[1:nrow(df)])

  x_lim <- c(min(df$start, df$end), max(df$start, df$end))

  dec_chk <- function(x) ifelse(x-floor(as.numeric(x))==0,F,T)
  # x_lim[1 %in% 1:2 & is.numeric(x_lim[1])] <- floor(as.numeric(x_lim[1 %in% 1:2 & is.numeric(x_lim[1])]))
  # x_lim[2 %in% 1:2 & is.numeric(x_lim[2])] <- ceiling(as.numeric(x_lim[2 %in% 1:2 & is.numeric(x_lim[2])]))

  x_lim[1][dec_chk(x_lim[1])] <- floor(as.numeric(x_lim[1][dec_chk(x_lim[1])]))
  x_lim[2][dec_chk(x_lim[2])] <- ceiling(as.numeric(x_lim[2][dec_chk(x_lim[2])]))

  y_lim <- c(.8, max(df$sn) * 1)

  x_p <- length(x_lim)/12
  y_p <- length(y_lim)/12

  sf <- x_p/y_p

  graphics::par(bg="black")
  graphics::par(mar=c(2,2,2,2))
  graphics::plot(x =x_lim, y=y_lim, type="n")
  graphics::axis(1,  at=seq(x_lim[1], x_lim[2], 1), labels= format(seq(x_lim[1], x_lim[2], 1)), col="white", col.axis="white", cex.axis = .8)

  for (i in 1:nrow(df)) {
    if (df$start[i]!=df$end[i]){
      graphics::arrows(length=0.1, angle=20, x0 =df$start[i], x1= df$end[i], y0 = df$sn[i], y1= df$sn[i], col = df$cols[i])
    }
    graphics::points(y=df$sn[i], x = df$start[i], pch = 21, bg=df$cols[i], col=df$cols[i])

    if(show_overlap == T){
      for (j in 1:nrow(df)){
        om <- overlap_method(df$x[i], df$x[j])
        om_l <- gsub("aligns_","aligns\n  ",om)
        if(j>i & om != "none"){
          if(om %in% c("exact", "inbetween")){
            x1 <- ifelse(df$start[j]<df$start[i], df$start[i], df$start[j])
            x2 <- ifelse(df$end[j]<df$end[i], df$end[j], df$end[i])
            y <- ifelse(df$start[j]<df$start[i], df$sn[i], df$sn[j])
            o <- ifelse(df$start[j]<df$start[i], -1, .6)

            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(x1, x1), lty=2, col=df$cols[i])
            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(x2, x2), lty=2, col=df$cols[i])
            graphics::text(y = y, x =  mean(c(x1, x2)), labels = om_l, col=df$cols[i], cex = (sf * .8), pos = 1, offset =o)
          }else if(om=="aligns_end"){
            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(df$end[j], df$end[j]), lty=2, col=df$cols[i])
            graphics::text(srt = 90, y = df$sn[i] + (sf * .05), x =  df$end[j] + (sf * .05), labels = om_l, col=df$cols[i], cex = (sf * .8), pos = 4, offset =.6)
          }else if(om=="across"){
            x <- ifelse(df$start[j] <= df$end[i] & df$start[j] >= df$start[i], df$start[j], df$end[j])
            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(x, x), lty=2, col=df$cols[i])
            graphics::text(srt = 90, y = df$sn[i] + (sf * .05), x =  x + (sf * .05), labels = om_l, col=df$cols[i], cex = (sf * .8), pos = 4, offset =.6)
          }else if(om=="chain"){
            x <- ifelse(df$start[j] <= df$end[j] & df$start[j] >= df$start[i], df$start[j], df$end[j])
            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(x, x), lty=2, col=df$cols[i])
            graphics::text(srt = 90, y = df$sn[i] + (sf * .05), x =  x + (sf * .05), labels = om_l, col=df$cols[i], cex = (sf * .8), pos = 4, offset =.6)
          }else{
            graphics::lines(y=c(df$sn[i], df$sn[j]), x=c(df$start[j], df$start[j]), lty=2, col=df$cols[i])
            graphics::text(srt = 90, y = df$sn[i] + (sf * .05), x =  df$start[j] + (sf * .05), labels = om_l, col=df$cols[i], cex = (sf * .8), pos = 4, offset =.6)
          }
        }
      }
    }

  }

  plt <- grDevices::recordPlot()
  return(plt)
}
