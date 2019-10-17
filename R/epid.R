#' @include number_line.R
#' @title Number line objects
#' @aliases epid
#'
#' @export
#'
epid <- function(x){

 methods::new("epid", id=x$sn, epid = x$epid, case_nm = x$case_nm,
                     epid_interval = x$epid_interval, epid_total = x$epid_total,
                     epid_length = x$epid_length)
}

#' @rdname epid
#' @param ... arguments for particular methods
#' @export
unique.epid <- function(x, ...){

  db <- unique(data.frame(c = x@se_nm, ep = x))
  db <- subset(db, db$c !="Duplicate")
  x <- db$ep
  return(x)
}

#' @rdname epid
#' @export
format.epid <- function(x, ...){
  paste(show(x@.Data), "(", substr(toupper(x@case_nm)) + ")", sep=" ")
}

#' S4 objects containing the results of \code{episode_grouping()}, \code{fixed_epiosdes()} and \code{rolling_episode()}
#'
#' @slot start Start of the number line
#' @slot id Unique \code{numeric} ID. Providing this is optional.
#' @slot gid Unique \code{numeric} Group ID. Providing this is optional.
#' @slot .Data Length/with and direction of the \code{number_line} object.
#' @aliases epid-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("epid", contains = c("ANY"), representation(id = "numeric", case_nm= "character", epid = "numeric", epid_interval = "ANY",
                                                                    epid_length= "ANY", epid_total = "numeric", epid_dataset ="character"))

#' @rdname epid-class
#' @param object object
setMethod("show", signature(object="epid"), function(object){
  print(format.epid(object))
})

#' @rdname epid-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "epid"), function(x, ...) {
  methods::new("epid", rep(x@.Data, ...), start = rep(x@start, ...), id = rep(x@id, ...), gid = rep(x@gid, ...))
})

#' @aliases [,epid-method
#' @rdname epid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "epid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], id = x@id[i], epid = x@epid[i],
                         epid_length = x@epid_interval[i], epid_total = x@epid_interval[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })

#' @aliases [[,epid-method
#' @rdname epid-class
#' @param exact exact
setMethod("[[", signature(x = "epid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], id = x@id[i], epid = x@epid[i],
                         epid_length = x@epid_interval[i], epid_total = x@epid_interval[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })

#' @rdname epid-class
#' @param name slot name
setMethod("$", signature(x = "epid"), function(x, name) {
  methods::slot(x, name)
})

