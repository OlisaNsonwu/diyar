#' @title Overlapping number line objects
#'
#' @description Identify overlapping \code{\link{number_line}} objects
#'
#' @param x \code{[\link{number_line}]}
#' @param y \code{[\link{number_line}]}
#' @param methods \code{[charater|integer]}. Type of overlap. See \code{as.data.frame(diyar::overlap_methods$options)} for options.
#' @aliases overlaps
#' @return \code{logical}; \code{character}
#'
#' @details
#'
#' There are 6 mutually exclusive types of overlap;
#' \itemize{
#' \item \bold{\code{exact()}} - identical \code{\link{start_point}} and \code{\link{end_point}} points.
#' \item \bold{\code{inbetween()}} - Both \code{\link{start_point}} and \code{\link{end_point}} of one \code{\link{number_line}} object are within the \code{\link{start_point}} and \code{\link{end_point}} of another.
#' \item \bold{\code{across()}} - Only the \code{\link{start_point}} or \code{\link{end_point}} of one \code{\link{number_line}} object is in between the \code{\link{start_point}} and \code{\link{end_point}} of another.
#' \item \bold{\code{chain()}} - \code{\link{end_point}} of one \code{\link{number_line}} object is identical to the \code{\link{start_point}} of another.
#' \item \bold{\code{aligns_start()}} - identical \code{\link{start_point}} only.
#' \item \bold{\code{aligns_end()}} - identical \code{\link{end_point}} only.
#' }
#'
#' Except \bold{\code{exact()}}, each type of overlap has two variations;
#' \itemize{
#' \item \bold{\code{x_`method`_y()}} - \code{\link{number_line}}-\code{x} starts before \code{\link{number_line}}-\code{y}.
#' \item \bold{\code{y_`method`_x()}} - \code{\link{number_line}}-\code{y} starts before \code{\link{number_line}}-\code{x}.
#' }
#'
#' There are two mutually inclusive types of overlap;
#' \itemize{
#' \item \bold{\code{overlap()}} - a convenient option to select "ANY" and "ALL" type of overlap.
#' \item \bold{\code{none()}} - a convenient option to select "NO" type of overlap.
#' }
#'
#' Selecting multiple types of overlap;
#' \itemize{
#' \item \bold{\code{overlaps()}} - select specific type(s) of overlap.
#' \item \bold{\code{overlap_method()}} - return the type of overlap for a pair of \code{\link{number_line}} objects.
#' \item \bold{\code{overlap_method_codes()}} - return the corresponding overlap method code for a specific type(s) of overlap.
#' \item \bold{\code{overlap_method_names()}} - return the corresponding type(s) of overlap for a specific overlap code.
#' \item \bold{\code{include_overlap_method()}} - return a \bold{\code{character(1)}} value for specified type(s) of overlap.
#' \item \bold{\code{exclude_overlap_method()}} - return a \bold{\code{character(1)}} value for all type(s) of overlap except those specified.
#' }
#'
#' @seealso
#' \code{\link{number_line}};  \code{\link{set_operations}}
#'
#' @examples
#' a <- number_line(-100, 100)
#' g <- number_line(100, 100)
#' overlaps(a, g)
#'
#' # It's neither an "exact" or "chain"-overlap
#' overlaps(a, g, methods = "exact|chain")
#'
#' # It's an "aligns_end"-overlap
#' overlap_method(a, g)
#' overlaps(a, g, methods = "exact|chain|x_aligns_end_y")
#'
#' # Corresponding overlap code
#' overlap_method_codes("exact|chain|x_aligns_end_y")
#' include_overlap_method(c("exact", "chain", "x_aligns_end_y"))
#'
#' # Corresponding overlap name
#' overlap_method_names(overlap_method_codes("exact|chain|x_aligns_end_y"))
#'
#' # Every other type overlap
#' exclude_overlap_method(c("exact", "chain", "x_aligns_end_y"))
#' overlap_method_names(exclude_overlap_method(c("exact", "chain", "x_aligns_end_y")))
#'
#' # All the above is based on tests for each specific type of overlap as seen below
#' none(a, g)
#' exact(a, g)
#' across(a, g)
#' x_across_y(a, g)
#' y_across_x(a, g)
#' chain(a, g)
#' x_chain_y(a, g)
#' y_chain_x(a, g)
#' inbetween(a, g)
#' x_inbetween_y(a, g)
#' y_inbetween_x(a, g)
#' aligns_start(a, g)
#' x_aligns_start_y(a, g)
#' y_aligns_start_x(a, g)
#' aligns_end(a, g)
#' x_aligns_end_y(a, g)
#' y_aligns_end_x(a, g)
#'
#' @export
overlaps <- function(x, y, methods = 8){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0){
    return(logical())
  }

  err <- err_overlap_methods_1(overlap_methods = methods, "methods")
  if(err != F) stop(err, call. = F)

  if(inherits(methods, "character")){
    methods <- overlap_method_codes(methods)
  }

  ov_lgk <- overlap(x, y)
  lgk_2 <- rep(FALSE, length(ov_lgk))

  # None
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["none"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     !ov_lgk)
  lgk_2[mth_lgk] <- TRUE
  if(all(lgk_2 == TRUE & !is.na(lgk_2))) {
    rm(list = ls()[ls() != "lgk_2"])
    return(lgk_2)
  }
  # Overlap
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["overlap"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  lgk_2[mth_lgk] <- TRUE

  if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
    rm(list = ls()[ls() != "lgk_2"])
    return(lgk_2)
  }
  # x_across_y
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["x_across_y"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- x_across_y(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # y_across_x
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["y_across_x"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- y_across_x(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # exact
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["exact"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- exact(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # x_inbetween_y
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["x_inbetween_y"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- x_inbetween_y(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # y_inbetween_x
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["y_inbetween_x"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- y_inbetween_x(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # x_aligns_start_y
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["x_aligns_start_y"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- x_aligns_start_y(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # y_aligns_start_x
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["y_aligns_start_x"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- y_aligns_start_x(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # x_aligns_end_y
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["x_aligns_end_y"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- x_aligns_end_y(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # y_aligns_end_x
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["y_aligns_end_x"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- y_aligns_end_x(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # x_chain_y
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["x_chain_y"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- x_chain_y(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # y_chain_x
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["y_chain_x"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- y_chain_x(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }

  rm(list = ls()[ls() != "lgk_2"])
  return(lgk_2)
}

#' @rdname overlaps
#' @export
overlap <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start >= y@start & x@start <= y@start + y@.Data) | (x@start <= y@start & x@start >= y@start + y@.Data)) |
    ((x@start + x@.Data >= y@start & x@start + x@.Data <= y@start + y@.Data) | (x@start + x@.Data <= y@start & x@start + x@.Data >= y@start + y@.Data)) |
    ((y@start >= x@start & y@start <= x@start + x@.Data) | (y@start <= x@start & y@start >= x@start + x@.Data)) |
    ((y@start + y@.Data >= x@start & y@start + y@.Data <= x@start + x@.Data) | (y@start + y@.Data <= x@start & y@start + y@.Data >= x@start + x@.Data))

  return(r)
}

#' @rdname overlaps
#' @export
none <- function(x, y) !(overlap(x, y))

#' @rdname overlaps
#' @export
exact <- function(x, y){
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- y@start == x@start & x@.Data == y@.Data

  return(r)
}

#' @rdname overlaps
#' @export
across <- function(x, y){
  x_across_y(x, y) | y_across_x(x, y)
}

#' @rdname overlaps
#' @export
x_across_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")

  sp_y <- y@start
  ed_x <- (x@start + x@.Data)
  ed_y <- (y@start + y@.Data)

  r <- sp_y > x@start &
    sp_y < ed_x &
    ed_y > ed_x

  rm(sp_y, ed_x, ed_y)
  return(r)
}

#' @rdname overlaps
#' @export
y_across_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")

  sp_x <- x@start
  ed_x <- (x@start + x@.Data)
  ed_y <- (y@start + y@.Data)

  r <- sp_x > y@start &
    sp_x < ed_y &
    ed_x > ed_y

  rm(sp_x, ed_x, ed_y)
  return(r)
}

#' @rdname overlaps
#' @export
chain <- function(x, y){
  x_chain_y(x, y) | y_chain_x(x, y)
}

#' @rdname overlaps
#' @export
x_chain_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- ((x@start + x@.Data) == y@start & x@.Data != 0 & y@.Data != 0)

  return(r)
}

#' @rdname overlaps
#' @export
y_chain_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- ((y@start + y@.Data) == x@start & x@.Data != 0 & y@.Data != 0)

  return(r)
}

#' @rdname overlaps
#' @export
aligns_start <- function(x, y){
  x_aligns_start_y(x, y) | y_aligns_start_x(x, y)
}

#' @rdname overlaps
#' @export
x_aligns_start_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- (x@start == y@start) & as.numeric((y@start + y@.Data)) > as.numeric((x@start + x@.Data))
  return(r)
}

#' @rdname overlaps
#' @export
y_aligns_start_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- (x@start == y@start) & as.numeric((y@start + y@.Data)) < as.numeric((x@start + x@.Data))
  return(r)
}

#' @rdname overlaps
#' @export
aligns_end <- function(x, y){
  x_aligns_end_y(x, y) | y_aligns_end_x(x, y)
}

#' @rdname overlaps
#' @export
x_aligns_end_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- ((x@start + x@.Data) == (y@start + y@.Data)) & as.numeric(y@start) > as.numeric(x@start)
  return(r)
}

#' @rdname overlaps
#' @export
y_aligns_end_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- ((x@start + x@.Data) == (y@start + y@.Data)) & as.numeric(y@start) < as.numeric(x@start)
  return(r)
}

#' @rdname overlaps
#' @export
inbetween <- function(x, y){
  x_inbetween_y(x, y) | y_inbetween_x(x, y)
}

#' @rdname overlaps
#' @export
x_inbetween_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- x@start > y@start & (x@start + x@.Data) < (y@start + y@.Data)
  return(r)
}

#' @rdname overlaps
#' @export
y_inbetween_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  x <- reverse_number_line(x, direction = "decreasing")
  y <- reverse_number_line(y, direction = "decreasing")
  r <- y@start > x@start & (y@start + y@.Data) < (x@start + x@.Data)
  return(r)
}

#' @rdname overlaps
#' @export
overlap_method <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(character())

  m <- rep("none", max(c(length(x), length(y))))

  m_lgk <- which(m == "none")
  lgk <- which(x_across_y(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "x_across_y"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(y_across_x(x, y[m_lgk]))
  m[m_lgk[lgk]] <- "y_across_x"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(x_inbetween_y(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "x_inbetween_y"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(y_inbetween_x(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "y_inbetween_x"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(exact(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "exact"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(x_aligns_start_y(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "x_aligns_start_y"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(y_aligns_start_x(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "y_aligns_start_x"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(x_aligns_end_y(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "x_aligns_end_y"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(y_aligns_end_x(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "y_aligns_end_x"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(x_chain_y(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "x_chain_y"

  m_lgk <- which(m == "none")
  if(length(m_lgk) == 0){
    rm(list = ls()[ls() != "m"])
    return(m)
  }
  lgk <- which(y_chain_x(x[m_lgk], y[m_lgk]))
  m[m_lgk[lgk]] <- "y_chain_x"

  m
}

#' @rdname overlaps
#' @export
include_overlap_method <- function(methods){
  err <- err_object_types(methods, "methods", "character")
  if(err != F) stop(err, call. = F)
  lst <- c("overlap", "none", "exact",
           "x_across_y", "y_across_x",
           "x_chain_y", "y_chain_x",
           "x_aligns_start_x", "x_aligns_start_y",
           "x_aligns_end_y", "y_aligns_end_x",
           "x_inbetween_y", "y_inbetween_x")
  if(any(methods == "across")){
    methods <- methods[methods != "across"]
    methods <- c(methods, "x_across_y", "y_across_x")
  }
  if(any(methods == "chain")){
    methods <- methods[methods != "chain"]
    methods <- c(methods, "x_chain_y", "y_chain_x")
  }
  if(any(methods == "inbetween")){
    methods <- methods[methods != "inbetween"]
    methods <- c(methods, "x_inbetween_y", "y_inbetween_x")
  }
  if(any(methods == "aligns_start")){
    methods <- methods[methods != "aligns_start"]
    methods <- c(methods, "x_aligns_start_y", "y_aligns_start_x")
  }
  if(any(methods == "aligns_end")){
    methods <- methods[methods != "aligns_end"]
    methods <- c(methods, "x_aligns_end_y", "y_aligns_end_x")
  }
  methods <- tolower(methods[!duplicated(methods)])
  methods <- methods[methods %in% lst]
  if(any(methods == "overlap")){
    overlap_method_codes("overlap")
  }else if (any(methods == "none") | all(methods == "")){
    overlap_method_codes("none")
  }else{
    methods <- paste0(sort(methods), collapse = "|")
    overlap_method_codes(methods)
  }
}

#' @rdname overlaps
#' @export
exclude_overlap_method <- function(methods){
  err <- err_object_types(methods, "methods", "character")
  if(err != F) stop(err, call. = F)

  lst <- c("exact", "x_across_y", "y_across_x",
           "x_chain_y", "y_chain_x",
           "x_aligns_start_y", "y_aligns_start_x",
           "x_aligns_end_y", "y_aligns_end_x",
           "x_inbetween_y", "y_inbetween_x")
  if(any(methods == "across")){
    methods <- methods[methods != "across"]
    methods <- c(methods, "x_across_y", "y_across_x")
  }
  if(any(methods == "chain")){
    methods <- methods[methods != "chain"]
    methods <- c(methods, "x_chain_y", "y_chain_x")
  }
  if(any(methods == "inbetween")){
    methods <- methods[methods != "inbetween"]
    methods <- c(methods, "x_inbetween_y", "y_inbetween_x")
  }
  if(any(methods == "aligns_start")){
    methods <- methods[methods != "aligns_start"]
    methods <- c(methods, "x_aligns_start_y", "y_aligns_start_x")
  }
  if(any(methods == "aligns_end")){
    methods <- methods[methods != "aligns_end"]
    methods <- c(methods, "x_aligns_end_y", "y_aligns_end_x")
  }
  methods <- lst[!lst %in% methods]
  methods <- paste0(sort(methods), collapse = "|")
  overlap_method_codes(methods)
}

#' @rdname overlaps
#' @export
overlap_method_codes <- function(methods){
  x2 <- methods[!duplicated(methods)]
  x2_cd <-  lapply(x2, function(x){
    lgk <- sapply(strsplit(as.vector(x), "\\|"), function(x){
      if(any(x == "overlap")){
        return(diyar::overlap_methods$options$cd[match("overlap", diyar::overlap_methods$options$nm)])
      }
      if(any(x == "none")){
        return(diyar::overlap_methods$options$cd[match("none", diyar::overlap_methods$options$nm)])
      }
      if(any(x == "across")){
        x <- x[x != "across"]
        x <- c(x, "x_across_y", "y_across_x")
      }
      if(any(x == "chain")){
        x <- x[x != "chain"]
        x <- c(x, "x_chain_y", "y_chain_x")
      }
      if(any(x == "inbetween")){
        x <- x[x != "inbetween"]
        x <- c(x, "x_inbetween_y", "y_inbetween_x")
      }
      if(any(x == "aligns_start")){
        x <- x[x != "aligns_start"]
        x <- c(x, "x_aligns_start_y", "y_aligns_start_x")
      }
      if(any(x == "aligns_end")){
        x <- x[x != "aligns_end"]
        x <- c(x, "x_aligns_end_y", "y_aligns_end_x")
      }
      x <- x[x %in% names(diyar::overlap_methods$methods)]
      x <- x[!duplicated(x)]
      x <- paste0(sort(x), collapse = "|")
      diyar::overlap_methods$options$cd[match(x, diyar::overlap_methods$options$nm)]
    })
    lgk
  })
  x2_cd <- unlist(x2_cd)
  x2_cd <- x2_cd[match(methods, x2)]
  return(x2_cd)
}

#' @rdname overlaps
#' @export
overlap_method_names <- function(methods){
  nm <- rep(NA_character_, length(methods))
  lgk <- methods %in% unlist(diyar::overlap_methods$methods, use.names = FALSE)
  nm[lgk] <- diyar::overlap_methods$options$nm[match(methods[lgk], diyar::overlap_methods$options$cd)]
  return(nm)
}
