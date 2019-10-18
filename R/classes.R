#' S4 classes in \code{diyar}
#' @include number_line.R
#' 'epid' - An object to store the results of \code{fixed_episodes}, \code{rolling_episodes} and \code{episode_group}
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
  paste(format.number_line(x@epid_interval), " (", substr(x@case_nm,1,1), ")", sep="")
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
               epid_length = rep(x@epid_length, ...), epid_total = rep(x@epid_total, ...), data_set = rep(x@data_set, ...))
})

#' @aliases [,epid-method
#' @rdname epid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "epid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], sn = x@sn[i],
                         epid_length = x@epid_interval[i], epid_total = x@epid_interval[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })

#' @aliases [[,epid-method
#' @rdname epid-class
#' @param exact exact
setMethod("[[", signature(x = "epid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("epid", x@.Data[i], case_nm = x@case_nm[i], sn = x@sn[i],
                         epid_length = x@epid_interval[i], epid_total = x@epid_interval[i], epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i])
          })
