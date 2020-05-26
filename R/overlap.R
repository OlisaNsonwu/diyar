#' @title Overlapping number line objects
#'
#' @description Identify overlapping \code{number_line} objects
#'
#' @param x \code{number_line} object
#' @param y \code{number_line} object
#' @param method Method of overlap. Check every pair of \code{number_line} objects with the same \code{method}. Deprecated use \code{methods} instead.
#' @param methods Methods of overlap. Check different pairs of \code{number_line} objects using different \code{methods}
#' @aliases overlaps
#' @return \code{logical} object
#'
#' @details
#'
#' exact() - Identical start and end points
#'
#' inbetween() - Start and end points of one \code{number_line} object is in between the start and end points of another.
#'
#' across() - Start or end points of one \code{number_line} object is in between the start and end points of another.
#'
#' chain() - Chained i.e. end point of one \code{number_line} object is the same as the start point of another.
#'
#' aligns_start() - Identical start points only.
#'
#' aligns_end() - Identical end points only.
#'
#' overlap() - Any kind of overlap.
#'
#' overlaps() - Overlap by any or all 7 methods above.
#'
#' overlap_method() - Shows if and how a pair of \code{number_line} object overlaps.
#' Does not show \code{"overlap"} since \code{overlap()} is always \code{TRUE} when any other method is \code{TRUE}.
#'
#' include_overlap_method() and exclude_overlap_method() - Conveniently generate the required values for the \code{method}
#' @examples
#' a <- number_line(-100, 100)
#' b <- number_line(10, 11.2)
#' c <- number_line(100, 200)
#' d <- number_line(100, 120)
#' e <- number_line(50, 120)
#' g <- number_line(100,100)
#'
#' overlaps(a, g)
#' overlaps(a, g, methods = "exact|chain")
#' @export
overlaps <- function(x, y, method = c("exact","across","chain","aligns_start","aligns_end","inbetween"),
                    methods = "overlap"){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))
  if(!is.character(method)) stop(paste("'method' must be a character object"))
  if(all(!tolower(method) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween", "overlap"))) stop(paste("`method` must be either 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween'"))
  mths <- names(split(rep(1, length(methods)), methods))
  mths <- unique(unlist(strsplit(mths, split="\\|")))

  # invaid methods
  o <- mths[!tolower(mths) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween","overlap")]
  if (length(o)>0) stop(paste("\n'", "Valid 'methods' are 'overlap', 'exact', 'across','chain','aligns_start','aligns_end' or 'inbetween' \n\n",
                              "Syntax ~ \"method1|method2|method3...\" \n",
                              "                 OR                   \n",
                              "Use ~ include_overlap_method() or exclude_overlap_method()", sep=""))

  if(missing(methods) & !missing(method)) {
    # For continuity, use `method` if `methods` is not supplied but warn that it's deprecated
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    # `methods` overrides `method` if it's supplied
    m <- methods
  }

  # warnings for difference in length(x) and length(y)
  if(length(x) != length(y) & (max(length(x), length(y))/min(length(x), length(y))) %% 1 !=0 ) warning("\n  length('x') != length('y')\n  longer object length is not a multiple of shorter object length")
  if(!all(c(length(x),length(y) %in% length(m))) &
     ((max(length(m), length(y))/min(length(m), length(y))) %% 1 !=0 |
      (max(length(m), length(x))/min(length(m), length(x))) %% 1 !=0
     )) warning("\n  length('method') != length('y') OR length('method') != length('x')\n  longer object length is not a multiple of shorter object length")

  # valid methods
  mths <- tolower(mths)
  mths <- mths[mths %in% c("overlap", "exact", "across","chain","aligns_start","aligns_end","inbetween")]

  # final check
  p <- rep(F, length(x))
  sets <- split(1:length(x), m)
  um1 <- names(sets)

  m_ab <- function(x){
    x <- ifelse(x=="aligns_start", "as", ifelse(x=="aligns_end", "ae", substr(x,1,2)))
  }

  for (i in mths){
    assign("tp", sets)
    ab <- m_ab(i)
    names(tp) <- ifelse(grepl(i, names(sets)), i, "")
    tp <- tp[names(tp) != ""]
    tp <- unlist(tp, use.names = F)
    #lgk <- rep(F, length(x))
    lgk <- p
    lgk[tp] <- T
    assign(ab, lgk)
  }

  for (j in mths) {
    func <- get(j)
    tst <- get(m_ab(j))
    chg <- func(x, y)

    p[p==F & chg == T & tst == T] <- T

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
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  x <- diyar::reverse_number_line(x, direction = "decreasing")
  y <- diyar::reverse_number_line(y, direction = "decreasing")

  r <- y@start <= x@start & x@start <= (y@start + y@.Data) |
    x@start <= y@start & y@start <= (x@start + x@.Data)
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' exact(a, g)
#' exact(a, a)
#' @export
exact <- function(x, y){
  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- y@start == x@start & x@.Data == y@.Data
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' across(a, b)
#' across(a, e)
#' @export
across <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  x <- diyar::reverse_number_line(x, direction = "decreasing")
  y <- diyar::reverse_number_line(y, direction = "decreasing")

  r <- (y@start > x@start & y@start < (x@start + x@.Data) & (y@start + y@.Data) > (x@start + x@.Data) ) |
    (x@start > y@start & x@start < (y@start + y@.Data) & (x@start + x@.Data) > (y@start + y@.Data) )
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' chain(c, d)
#' chain(a, c)
#' @export
chain <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- ((y@start + y@.Data) == x@start & x@.Data != 0 & y@.Data != 0) |
    ((x@start + x@.Data) == y@start & x@.Data != 0 & y@.Data != 0)
  r <- ifelse(!is.finite(r) | x@.Data * y@.Data <0, FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' aligns_start(c, d)
#' aligns_start(a, c)
#' @export
aligns_start <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- x@start==y@start & !diyar::exact(x, y)
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' aligns_end(d, e)
#' aligns_end(a, c)
#' @export
aligns_end <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  r <- (x@start + x@.Data) == (y@start + y@.Data) & !diyar::exact(x, y)
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' inbetween(a, g)
#' inbetween(b, a)
#' @export
inbetween <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  x <- diyar::reverse_number_line(x, direction = "decreasing")
  y <- diyar::reverse_number_line(y, direction = "decreasing")

  r <- (x@start > y@start & (x@start + x@.Data) < (y@start + y@.Data)) | (y@start > x@start & (y@start + y@.Data) < (x@start + x@.Data))
  r <- ifelse(!is.finite(r), FALSE, r)
  return(r)
}

#' @rdname overlaps
#' @return \code{character} object
#' @examples
#'
#' overlap_method(a, c)
#' overlap_method(d, c)
#' overlap_method(a, g)
#' overlap_method(b, e)
#' @export
overlap_method <- function(x, y){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  if(missing(y)) stop("argument 'y' is missing, with no default")

  if(!diyar::is.number_line(x)) stop(paste("'x' is not a number_line object"))
  if(!diyar::is.number_line(y)) stop(paste("'y' is not a number_line object"))

  m <- ""
  m <- ifelse(diyar::exact(x, y), paste(m,"exact", sep="|"), m)
  m <- ifelse(diyar::across(x, y), paste(m,"across", sep="|"), m)
  m <- ifelse(diyar::chain(x, y), paste(m,"chain", sep="|"), m)
  m <- ifelse(diyar::aligns_start(x, y), paste(m,"aligns_start", sep="|"), m)
  m <- ifelse(diyar::aligns_end(x, y), paste(m,"aligns_end", sep="|"), m)
  m <- ifelse(diyar::inbetween(x, y), paste(m,"inbetween", sep="|"), m)

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
  lst <- c("overlap", "exact", "across", "chain", "aligns_start", "aligns_end", "inbetween")
  methods <- methods[methods %in% lst]
  methods <- paste(methods,sep="", collapse = "|")
  methods
}

#' @rdname overlaps
#' @examples
#'
#' exclude_overlap_method("across")
#' exclude_overlap_method(c("across", "chain"))
#' @export
exclude_overlap_method <- function(methods){
  lst <- c("overlap", "exact", "across", "chain", "aligns_start", "aligns_end", "inbetween")
  methods <- lst[!lst %in% methods]
  methods <- paste(methods,sep="", collapse = "|")
  methods
}

