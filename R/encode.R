#' @name encode
#' @aliases encode
#' @title Labelling in \code{diyar}
#'
#' @description Encode and decode character and numeric values.
#'
#' @param x \code{[d_label|atomic]}
#' @param ... Other arguments.
#' @return \code{d_label}; \code{atomic}
#'
#' @details
#' To minimise memory usage, most components of \code{\link[=pid-class]{pid}}, \code{\link[=epid-class]{epid}} and \code{\link[=pane-class]{pane}} are \code{integer} objects with labels.
#' \bold{\code{encode()}} and \bold{\code{decode()}} translates these codes and labels as required.
#'
#' @export
#' @examples
#' cds <- encode(rep(LETTERS[1:5], 3))
#' cds
#'
encode <- function(x, ...) UseMethod("encode")

#' @rdname encode
#' @export
#' @examples
#' nms <- decode(cds)
#' nms
decode <- function(x, ...) UseMethod("decode")

#' @rdname encode
#' @export
encode.default <- function(x, ...){
  x_cd <- match(x, x[!duplicated(x)])
  val_cd <- seq_len(length(x[!duplicated(x_cd)]))
  val_nm <- sort(x[!duplicated(x_cd)], na.last = TRUE)
  rm(x_cd)
  x <- val_cd[match(x, val_nm)]
  class(x) <- "d_label"
  attr(x, "value") <- val_cd
  attr(x, "label") <- val_nm
  attr(x, "state") <- "encoded"
  return(x)
}

#' @rdname encode
#' @export
encode.d_label <- function(x, ...){
  if(attr(x, "state") == "encoded") return(x)
  y <- attr(x, "value")[match(x, attr(x, "label"))]
  class(y) <- "d_label"
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  attr(y, "state") <- "encoded"
  return(y)
}

#' @rdname encode
#' @export
decode.default <- function(x, ...){
  x_cd <- match(x, x[!duplicated(x)])
  val_cd <- seq_len(length(x[!duplicated(x_cd)]))
  val_nm <- sort(x[!duplicated(x_cd)], na.last = TRUE)
  rm(x_cd)
  class(x) <- "d_label"
  attr(x, "value") <- val_cd
  attr(x, "label") <- val_nm
  attr(x, "state") <- "decoded"
  return(x)
}

#' @rdname encode
#' @export
decode.d_label <- function(x, ...){
  if(attr(x, "state") == "decoded") return(x)
  y <- attr(x, "label")[match(x, attr(x, "value"))]
  class(y) <- "d_label"
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  attr(y, "state") <- "decoded"
  return(y)
}

#' @rdname encode
#' @export
rep.d_label <- function(x, ...){
  y <- rep(as.vector(x), ...)
  class(y) <- "d_label"
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  attr(y, "state") <- attr(x, "state")

  y
}

#' @rdname encode
#' @param i i
#' @param drop drop
#' @export
`[.d_label` <- function(x, i, ..., drop = TRUE) {
  y <- as.vector(x)[i]
  class(y) <- "d_label"
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  attr(y, "state") <- attr(x, "state")
  y
}

#' @rdname encode
#' @export
`[[.d_label` <- function(x, i, ..., drop = TRUE) {
  y <- as.vector(x)[i]
  class(y) <- "d_label"
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  attr(y, "state") <- attr(x, "state")
  y
}
