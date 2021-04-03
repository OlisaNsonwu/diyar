#' @name diyar_label
#' @aliases diyar_label
#' @title Labelling in \code{diyar}
#'
#' @description Encode and decode character and numeric values.
#'
#' @param x \code{[d_label|atomic]}
#'
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

#' @rdname diyar_label
#' @export
#' @examples
#' nms <- decode(cds)
#' nms
decode <- function(x, ...) UseMethod("decode")

#' @rdname diyar_label
#' @export
encode.default <- function(x){
  x_cd <- match(x, x)
  val_cd <- seq_len(length(x[!duplicated(x_cd)]))
  val_nm <- sort(x[!duplicated(x_cd)], na.last = TRUE)
  rm(x_cd)
  x <- val_cd[match(x, val_nm)]
  attr(x, "value") <- val_cd
  attr(x, "label") <- val_nm
  class(x) <- "d_label"
  return(x)
}

#' @rdname diyar_label
#' @export
decode.default <- function(x){
  return(x)
}

#' @rdname diyar_label
#' @export
decode.d_label <- function(x){
  return(
    attr(x, "label")[match(x, attr(x, "value"))]
  )
}

#' @rdname diyar_label
#' @export
rep.d_label <- function(x, ...){
  y <- x
  class(y) <- NULL
  y <- rep(y, ...)
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  class(y) <- "d_label"
  y
}

#' @rdname diyar_label
#' @export
`[.d_label` <- function(x, i, ..., drop = TRUE) {
  class(x) <- NULL
  y <- x[i]
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  class(y) <- "d_label"
  y
}

#' @rdname diyar_label
#' @export
`[[.d_label` <- function(x, i, ..., drop = TRUE) {
  class(x) <- NULL
  y <- x[i]
  attr(y, "value") <- attr(x, "value")
  attr(y, "label") <- attr(x, "label")
  class(y) <- "d_label"
  y
}
