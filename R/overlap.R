#' @title Overlapping number line objects
#'
#' @description Identify overlapping \code{number_line} objects
#'
#' @param x \code{number_line} object.
#' @param y \code{number_line} object.
#' @param method Deprecated. Please use \code{methods} instead. Method of overlap. Check every pair of \code{number_line} objects by the same \code{method}.
#' @param methods Methods of overlap. Check different pairs of \code{number_line} objects by different \code{methods}. Options are \code{"exact"}, \code{"reverse"}, \code{"inbetween"}, \code{"across"}, \code{"chain"}, \code{"aligns_start"} and \code{"aligns_end"}.
#' @aliases overlaps
#' @return \code{logical}; \code{character}
#'
#' @details
#'
#' \bold{8 logical test;}
#'
#' \bold{\code{exact()}} - Identical left and right points.
#'
#' \bold{\code{reverse()}} - Swapped left and right points.
#'
#' \bold{\code{inbetween()}} - start and endpoints of one \code{number_line} object is within the start and endpoints of another.
#'
#' \bold{\code{across()}} - start or endpoints of one \code{number_line} object is within the start and endpoints of another.
#'
#' \bold{\code{chain()}} - endpoint of one \code{number_line} object is the same as the start point of another.
#'
#' \bold{\code{aligns_start()}} - identical start points only.
#'
#' \bold{\code{aligns_end()}} - identical endpoints only.
#'
#' \bold{\code{overlap()}} - any kind of overlap. A convenient \code{method} for "ANY" and "ALL" methods of overlap.
#'
#' \bold{\code{overlaps()}} - overlap by any set of the 7 methods above.
#'
#' \bold{Describe methods of overlap;}
#'
#' \bold{\code{overlap_method()}} - Shows if and how a pair of \code{number_line} object have overlapped.
#' Does not show \code{"overlap"} since \bold{\code{overlap()}} is always \code{TRUE} when any other method is \code{TRUE}.
#'
#' \bold{\code{include_overlap_method()}} and \bold{\code{exclude_overlap_method()}} - Conveniently create the required values for \code{methods} and \code{overlap_methods} in \code{\link{episodes}}.
#'
#' @seealso
#' \code{\link{number_line}} and \code{\link{set_operations}}
#'
#' @examples
#' a <- number_line(-100, 100)
#' b <- number_line(10, 11.2)
#' c <- number_line(100, 200)
#' d <- number_line(100, 120)
#' e <- number_line(50, 120)
#' g <- number_line(100, 100)
#' f <- number_line(120, 50)
#'
#' overlaps(a, g)
#' overlaps(a, g, methods = "exact|chain")
#' @export
overlaps <- function(x, y, methods = "overlap", method = "overlap"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y)== 0) return(logical())
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  if(length(x) == 1 & length(y) != 1){
    x <- rep(x, length(y))
  }else if(length(y) == 1 & length(x) != 1){
    y <- rep(y, length(x))
  }else{
    err <- err_match_ref_len(x, "y", c(1, length(y)), "x")
    if(err != F) stop(err, call. = F)

    err <- err_match_ref_len(y, "x", c(1, length(x)), "y")
    if(err != F) stop(err, call. = F)
  }

  if(length(m) == 1 & length(x) != 1){
    m <- rep(m, length(x))
  }else{
    err <- err_match_ref_len(m, "x", c(1, length(x)), "method")
    if(err != F) stop(err, call. = F)
  }

  err <- err_object_types(m, "methods", "character")
  if(err != F) stop(err, call. = F)
  err <- err_overlap_methods_1(overlap_methods = m, "methods")
  if(err != F) stop(err, call. = F)

  # final check
  p <- rep(FALSE, length(x))
  sets <- split(1:length(x), m)

  # Mutually inclusive methods
  names(sets)[grepl("none", names(sets))] <- "none"
  names(sets)[grepl("overlap", names(sets))] <- "overlap"

  um1 <- names(sets)
  um1 <- um1[!duplicated(um1)]
  um1 <- unlist(strsplit(um1,split="\\|"))
  um1 <- um1[!duplicated(um1)]

  m_ab <- function(x){
    ifelse(x=="aligns_start", "as", ifelse(x=="aligns_end", "ae", substr(x,1,2)))
  }

  none <- function(x, y) rep(F, length(x))
  for (i in um1){
    assign("tp", sets)
    ab <- m_ab(i)
    names(tp) <- ifelse(grepl(i, names(sets)), i, "")
    tp <- tp[names(tp) != ""]
    tp <- unlist(tp, use.names = F)

    func <- get(i)
    tp <- tp[tp %in% which(p %in% c(FALSE, NA))]
    lgk <- func(x[tp], y[tp])
    tp <- tp[which(lgk)]
    p[tp] <- TRUE
  }
  return(p)
}

#' @rdname overlaps
#' @examples
#'
#' overlap(a, b)
#' overlap(a, e)
#' @export
overlap <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- ((x@start >= y@start & x@start <= y@start + y@.Data) | (x@start <= y@start & x@start >= y@start + y@.Data)) |
    ((x@start + x@.Data >= y@start & x@start + x@.Data <= y@start + y@.Data) | (x@start + x@.Data <= y@start & x@start + x@.Data >= y@start + y@.Data)) |
    ((y@start >= x@start & y@start <= x@start + x@.Data) | (y@start <= x@start & y@start >= x@start + x@.Data)) |
    ((y@start + y@.Data >= x@start & y@start + y@.Data <= x@start + x@.Data) | (y@start + y@.Data <= x@start & y@start + y@.Data >= x@start + x@.Data))

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' exact(a, g)
#' exact(a, a)
#' @export
exact <- function(x, y){
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- y@start == x@start & x@.Data == y@.Data

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' reverse(e, e)
#' reverse(e, f)
#' @export
reverse <- function(x, y){
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- x@start == y@start + y@.Data & x@start + x@.Data == y@start & abs(x@.Data) != 0

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' across(a, b)
#' across(a, e)
#' @export
across <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- ((start_point(x) > start_point(y) & start_point(x) < end_point(y)) & ((end_point(x) < start_point(y)) | (end_point(x) > end_point(y)))) |
    ((start_point(y) > start_point(x) & start_point(y) < end_point(x)) & ((end_point(y) < start_point(x)) | (end_point(y) > end_point(x))))

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' chain(c, d)
#' chain(a, c)
#' @export
chain <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- ((y@start + y@.Data) == x@start & x@.Data != 0 & y@.Data != 0) |
    ((x@start + x@.Data) == y@start & x@.Data != 0 & y@.Data != 0)
  r <- ifelse(!is.finite(r) | x@.Data * y@.Data < 0 | is.na(x@.Data * y@.Data), FALSE, r)

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' aligns_start(c, d)
#' aligns_start(a, c)
#' @export
aligns_start <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- x@start==y@start & !diyar::exact(x, y)

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' aligns_end(d, e)
#' aligns_end(a, c)
#' @export
aligns_end <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- (x@start + x@.Data) == (y@start + y@.Data) & !diyar::exact(x, y)

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' inbetween(a, g)
#' inbetween(b, a)
#' @export
inbetween <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(logical())

  r <- ((x@start > y@start & x@start < y@start + y@.Data) & (x@start + x@.Data > y@start & x@start + x@.Data < y@start + y@.Data)) |
    ((y@start > x@start & y@start < x@start + x@.Data) & (y@start + y@.Data > x@start & y@start + y@.Data < x@start + x@.Data)) |
    ((x@start < y@start & x@start > y@start + y@.Data) & (x@start + x@.Data < y@start & x@start + x@.Data > y@start + y@.Data)) |
    ((y@start < x@start & y@start > x@start + x@.Data) & (y@start + y@.Data < x@start & y@start + y@.Data > x@start + x@.Data))

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' overlap_method(a, c)
#' overlap_method(d, c)
#' overlap_method(a, g)
#' overlap_method(b, e)
#' @export
overlap_method <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)

  if(length(x) == 0 & length(y)== 0) return(character())

  m <- ""
  m <- ifelse(diyar::exact(x, y), paste(m,"exact", sep="|"), m)
  m <- ifelse(diyar::across(x, y), paste(m,"across", sep="|"), m)
  m <- ifelse(diyar::chain(x, y), paste(m,"chain", sep="|"), m)
  m <- ifelse(diyar::aligns_start(x, y), paste(m,"aligns_start", sep="|"), m)
  m <- ifelse(diyar::aligns_end(x, y), paste(m,"aligns_end", sep="|"), m)
  m <- ifelse(diyar::inbetween(x, y), paste(m,"inbetween", sep="|"), m)
  m <- ifelse(diyar::reverse(x, y), paste(m,"reverse", sep="|"), m)
  m <- gsub("^\\|","",m)
  m <- ifelse(m=="", "none", m)
  m
}

#' @rdname overlaps
#' @examples
#'
#' include_overlap_method("across")
#' include_overlap_method(c("across", "chain"))
#' @export
include_overlap_method <- function(methods){
  err <- err_object_types(methods, "methods", "character")
  if(err != F) stop(err, call. = F)
  lst <- c("overlap", "none", "exact", "across", "chain", "aligns_start", "aligns_end", "inbetween", "reverse")
  methods <- tolower(methods[!duplicated(methods)])
  methods <- methods[methods %in% lst]
  if(any(methods == "overlap")){
    "overlap"
  }else if (any(methods == "none")){
    "none"
  }else{
    paste(methods,sep="", collapse = "|")
  }
}

#' @rdname overlaps
#' @examples
#'
#' exclude_overlap_method("across")
#' exclude_overlap_method(c("across", "chain"))
#' @export
exclude_overlap_method <- function(methods){
  err <- err_object_types(methods, "methods", "character")
  if(err != F) stop(err, call. = F)

  lst <- c("exact", "across", "chain", "aligns_start", "aligns_end", "inbetween", "reverse")
  methods <- lst[!lst %in% methods]
  methods <- paste(methods,sep="", collapse = "|")
  methods
}

