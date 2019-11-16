#' @name number_line-class
#' @aliases number_line-class
#' @title \code{number_line} object
#'
#' @description
#' S4 objects representing a series of finite numbers on a number line
#' Used for range matching in \code{\link{record_group}} and interval grouping in \code{\link{fixed_episodes}}, \code{\link{rolling_episodes}} and \code{\link{episode_group}}
#'
#' @slot start Start of the number line
#' @slot id Unique \code{numeric} ID. Providing this is optional.
#' @slot gid Unique \code{numeric} Group ID. Providing this is optional.
#' @slot .Data Length/with and direction of the \code{number_line} object.
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("number_line", contains = "numeric", representation(start = "ANY", id = "numeric", gid = "numeric"))

#' @rdname number_line-class
#' @param object object
setMethod("show", signature(object="number_line"), function(object){
  print(format.number_line(object))
})

#' @rdname number_line-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "number_line"), function(x, ...) {
  methods::new("number_line", rep(x@.Data, ...), start = rep(x@start, ...), id = rep(x@id, ...), gid = rep(x@gid, ...))
})

#' @aliases [,number_line-method
#' @rdname number_line-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "number_line"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i], id = x@id[i], gid = x@gid[i])
          })

#' @aliases [[,number_line-method
#' @rdname number_line-class
#' @param exact exact
setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("number_line", x@.Data[i], start = x@start[i], id = x@id[i], gid = x@gid[i])
          })

#' @aliases [<-,number_line-method
#' @rdname number_line-class
#' @param value value
setMethod("[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  if (is.number_line(value)) {
    x@.Data[i] <- value@.Data
    x@start[i] <- value@start
    x@id[i] <- value@id
    x@gid[i] <- value@gid
    new("number_line", x@.Data, start = x@start, id = x@id, gid = x@gid)
  }
})

#' @aliases [[<-,number_line-method
#' @rdname number_line-class
setMethod("[[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  if (is.number_line(value)) {
    x@.Data[i] <- value@.Data
    x@start[i] <- value@start
    x@id[i] <- value@id
    x@gid[i] <- value@gid
    new("number_line", x@.Data, start = x@start, id = x@id, gid = x@gid)
  }
})

#' @rdname number_line-class
#' @param name slot name
setMethod("$", signature(x = "number_line"), function(x, name) {
  methods::slot(x, name)
})

#' @rdname number_line-class
setMethod("$<-", signature(x = "number_line"), function(x, name, value) {
  methods::slot(x, name) <- value
  x
})

#' @rdname number_line-class
setMethod("c", signature(x = "number_line"), function(x,...) {
  a <- lapply(list(x, ...), function(y) as.number_line(y)@start)
  for(i in 1:length(a)){
    if(i==1) ai <- a[[i]]
    if(i>1) ai <- c(ai, a[[i]])
  }

  id <- unlist(lapply(list(x, ...), function(y) as.number_line(y)@id))
  gid <- unlist(lapply(list(x, ...), function(y) as.number_line(y)@gid))
  zi <- unlist(list(x, ...))

  methods::new("number_line", .Data = zi, id = id, gid = gid, start= ai)

})

#' @rdname number_line-class
#' @export
unique.number_line <- function(x, ...){
  db <- unique(data.frame(l = left_point(x), r = right_point(x), ob = x))
  db$cri <- paste(as.numeric(db$l), as.numeric(db$r),sep="")
  db <- subset(db, !duplicated(db$cri))
  x <- db$ob
  return(x)
}

#' @rdname number_line-class
#' @param decreasing logical. Should the sort be increasing or decreasing
#' @export
sort.number_line <- function(x, decreasing = FALSE, ...){
  db <- data.frame(sd = as.numeric(diyar::start_point(x)), id = x@id, nl= x)

  if(decreasing) db$sd <- -db$sd
  x <- db[order(db$sd, db$id),]$nl
  return(x)
}

#' @rdname number_line-class
#' @export
format.number_line <- function(x, ...){
  x <- x[1:length(x@start)]
  s <- ifelse(x@start + x@.Data > x@start, "->","<-")
  s <- ifelse(x@start + x@.Data == x@start, "==",s)
  s <- ifelse(!is.finite(x@start + x@.Data) , "??",s)

  paste(x@start, s, x@start + x@.Data, sep=" ")
}

#' @name epid-class
#' @title \code{epid} object
#'
#' @description
#' An S4 object to store the results of \code{\link{fixed_episodes}}, \code{\link{rolling_episodes}} and \code{\link{episode_group}}
#'
#' @aliases epid-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("epid", contains = "numeric", representation(sn = "numeric", case_nm= "character", epid_interval = "number_line",
                                                      epid_length= "ANY", epid_total = "numeric", epid_dataset ="character"))

#' @rdname epid-class
#' @export
format.epid <- function(x, ...){
  paste("E-",formatC(x@.Data, width= nchar(max(x@.Data)), flag="0"), ifelse(is.na(x@epid_interval),"", paste(" ",format.number_line(x@epid_interval),sep="")), " (", substr(x@case_nm,1,1), ")", sep="")
}

#' @rdname epid-class
#' @export
unique.epid <- function(x, ...){
  db <- unique(data.frame(c = x@case_nm, ob = x))
  db <- subset(db, db$c=="Case")
  x <- db$ob
  return(x)
}

#' @rdname epid-class
#' @param object object
setMethod("show", signature(object="epid"), function(object){
  print(format.epid(object))
})

#' @rdname epid-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "epid"), function(x, ...) {
  methods::new("epid", rep(x@.Data, ...), case_nm = rep(x@case_nm, ...), epid_interval = rep(x@epid_interval, ...),
               epid_length = rep(x@epid_length, ...), epid_total = rep(x@epid_total, ...), epid_dataset = rep(x@epid_dataset, ...))
})

#' @aliases [,epid-method
#' @rdname epid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "epid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], sn = x@sn[i],
                         epid_length = x@epid_length[i], epid_total = x@epid_total[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })

#' @aliases [[,epid-method
#' @rdname epid-class
#' @param exact exact
setMethod("[[", signature(x = "epid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], sn = x@sn[i],
                         epid_length = x@epid_length[i], epid_total = x@epid_total[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })

#' @rdname epid-class
setMethod("c", signature(x = "epid"), function(x,...) {
  e <- lapply(list(x, ...), function(y) y@epid_interval)
  for(i in 1:length(e)){
    if(i==1) ei <- e[[i]]
    if(i>1) ei <- c(ei, e[[i]])
  }

  sn <- unlist(lapply(list(x, ...), function(y) y@sn))
  case_nm <- unlist(lapply(list(x, ...), function(y) y@case_nm))
  epid_length <- unlist(lapply(list(x, ...), function(y) y@epid_length))
  epid_total <- unlist(lapply(list(x, ...), function(y) y@epid_total))
  epid_dataset <- unlist(lapply(list(x, ...), function(y) y@epid_dataset))
  zi <- unlist(list(x, ...))

  methods::new("epid", zi, case_nm = case_nm, sn = sn,
               epid_length = epid_length, epid_total = epid_total, epid_dataset = epid_dataset,
               epid_interval = ei)

})

#' @name pid-class
#'
#' @title \code{pid} objects
#'
#' @description
#' An S4 object to store the results of \code{\link{record_group}}
#'
#' @aliases pid-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("pid", contains = "numeric", representation(sn = "numeric", pid_cri= "numeric",
                                                     pid_dataset ="character", pid_total = "numeric"))

#' @rdname pid-class
#' @export
format.pid <- function(x, ...){

  paste("P-", formatC(x@.Data, width= nchar(max(x@.Data)), flag="0"), " (",
        ifelse(x@pid_cri==0,"No Hit", paste("CRI ", formatC(x@pid_cri, width = 2, flag=0), sep="")),
        ")", sep="")
}

#' @rdname pid-class
#' @export
unique.pid <- function(x, ...){
  db <- unique(data.frame(p = x@.Data, ob = x))
  db <- subset(db, !duplicated(db$p))
  x <- db$ob
  return(x)
}

#' @rdname pid-class
#' @param object object
setMethod("show", signature(object="pid"), function(object){
  print(format.pid(object))
})

#' @rdname pid-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "pid"), function(x, ...) {
  methods::new("pid", rep(x@.Data, ...), sn = rep(x@sn, ...), pid_total = rep(x@pid_total, ...),
               pid_dataset = rep(x@pid_dataset, ...), pid_cri = rep(x@pid_cri, ...))
})

#' @aliases [,pid-method
#' @rdname pid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "pid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("pid", x@.Data[i], pid_cri = x@pid_cri[i], sn = x@sn[i],
                         pid_total = x@pid_total[i], pid_dataset = x@pid_dataset[i])
          })

#' @aliases [[,pid-method
#' @rdname pid-class
#' @param exact exact
setMethod("[[", signature(x = "pid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("pid", x@.Data[i], pid_cri = x@pid_cri[i], sn = x@sn[i],
                         pid_total = x@pid_total[i], pid_dataset = x@pid_dataset[i])
          })

#' @rdname pid-class
setMethod("c", signature(x = "pid"), function(x,...) {

  sn <- unlist(lapply(list(x, ...), function(y) y@sn))
  pid_cri <- unlist(lapply(list(x, ...), function(y) y@pid_cri))
  pid_total <- unlist(lapply(list(x, ...), function(y) y@pid_total))
  pid_dataset <- unlist(lapply(list(x, ...), function(y) y@pid_dataset))
  zi <- unlist(list(x, ...))

  methods::new("pid", zi, pid_cri = pid_cri, sn = sn,
               pid_total = pid_total, pid_dataset = pid_dataset)

})
