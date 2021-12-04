#' @title Overlapping number line objects
#'
#' @description Identify overlapping \code{number_line} objects
#'
#' @param x \code{[\link{number_line}]}
#' @param y \code{[\link{number_line}]}
#' @param methods \code{[charater|integer]}. Methods of overlap. Check different pairs of \code{number_line} objects by different \code{methods}. Options are \code{"exact"}, \code{"reverse"}, \code{"inbetween"}, \code{"across"}, \code{"chain"}, \code{"aligns_start"} and \code{"aligns_end"}.
#' Combinations are also supported see \code{diyar::overlap_methods$options}.
#' @aliases overlaps
#' @return \code{logical}; \code{character}
#'
#' @details
#'
#' \bold{9 logical test;}
#'
#' \bold{\code{exact()}} - Identical left and right points.
#'
#' \bold{\code{reverse()}} - Swapped left and right points.
#'
#' \bold{\code{inbetween()}} - start and end point of one \code{number_line} object is within the start and end point of another.
#' Split into \bold{\code{x_inbetween_y()}} and \bold{\code{y_inbetween_x()}}.
#'
#' \bold{\code{across()}} - start or end point of one \code{number_line} object is in between the start and end point of another.
#' Split into \bold{\code{x_across_y()}} and \bold{\code{y_across_x()}}.
#'
#' \bold{\code{chain()}} - endpoint of one \code{number_line} object is the same as the start point of another.
#' Split into \bold{\code{x_chain_y()}} and \bold{\code{y_chain_x()}}.
#'
#' \bold{\code{aligns_start()}} - identical start points only.
#'
#' \bold{\code{aligns_end()}} - identical end point only.
#'
#' \bold{\code{overlap()}} - any kind of overlap. A convenient \code{method} for "ANY" and "ALL" overlap methods.
#'
#' \bold{\code{overlaps()}} - overlap by a specified combination of the methods.
#'
#' \bold{Describe methods of overlap;}
#'
#' \bold{\code{overlap_method()}} - Shows how a pair of \code{number_line} object has overlapped.
#' Does not show \code{"overlap"} since \bold{\code{overlap()}} is always \code{TRUE} when any other method is \code{TRUE}.
#'
#' \bold{\code{include_overlap_method()}} and \bold{\code{exclude_overlap_method()}} - Conveniently create the required values for \code{methods}, and \code{case_overlap_methods} and \code{recurrence_overlap_methods} in \code{\link{episodes}}.
#'
#' \bold{\code{overlap_method_codes()}} - Numeric codes for the supported combination of overlap methods.
#'
#' @seealso
#' \code{\link{number_line}};  \code{\link{set_operations}}
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
overlaps <- function(x, y, methods = 8){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

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

  if(length(methods) == 1 & length(x) != 1){
    methods <- rep(methods, length(x))
  }else{
    err <- err_match_ref_len(methods, "x", c(1, length(x)), "methods")
    if(err != F) stop(err, call. = F)
  }

  err <- err_overlap_methods_1(overlap_methods = methods, "methods")
  if(err != F) stop(err, call. = F)

  if(class(methods) == "character"){
    methods <- overlap_method_codes(methods)
  }

  ov_lgk <- overlap(x, y)
  lgk_2 <- rep(FALSE, length(x))

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
  # across
  # mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["across"]] &
  #                    lgk_2 %in% c(FALSE, NA) &
  #                    ov_lgk)
  # if(length(mth_lgk) > 0){
  #   lgk_2[mth_lgk] <- across(x[mth_lgk], y[mth_lgk])
  #   if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
  #     rm(list = ls()[ls() != "lgk_2"])
  #     return(lgk_2)
  #   }
  # }
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
  # inbetween
  # mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["inbetween"]] &
  #                    lgk_2 %in% c(FALSE, NA) &
  #                    ov_lgk)
  # if(length(mth_lgk) > 0){
  #   lgk_2[mth_lgk] <- inbetween(x[mth_lgk], y[mth_lgk])
  #   if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
  #     rm(list = ls()[ls() != "lgk_2"])
  #     return(lgk_2)
  #   }
  # }
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
  # aligns_start
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["aligns_start"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- aligns_start(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # aligns_end
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["aligns_end"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- aligns_end(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }
  # chain
  # mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["chain"]] &
  #                    lgk_2 %in% c(FALSE, NA) &
  #                    ov_lgk)
  # if(length(mth_lgk) > 0){
  #   lgk_2[mth_lgk] <- chain(x[mth_lgk], y[mth_lgk])
  #   if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
  #     rm(list = ls()[ls() != "lgk_2"])
  #     return(lgk_2)
  #   }
  # }
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
  # reverse
  mth_lgk <- which(methods %in% diyar::overlap_methods$methods[["reverse"]] &
                     lgk_2 %in% c(FALSE, NA) &
                     ov_lgk)
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- reverse(x[mth_lgk], y[mth_lgk])
    if(all((lgk_2 == TRUE & !is.na(lgk_2)) | (!ov_lgk & !lgk_2))){
      rm(list = ls()[ls() != "lgk_2"])
      return(lgk_2)
    }
  }

  rm(list = ls()[ls() != "lgk_2"])
  return(lgk_2)
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
  if(length(x) == 0 & length(y) == 0) return(logical())

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
  if(length(x) == 0 & length(y) == 0) return(logical())

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
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start == y@start + y@.Data & x@start + x@.Data == y@start) |
    (y@start == x@start + x@.Data & y@start + y@.Data == x@start)) &
    abs(x@.Data) == abs(y@.Data)

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' across(a, e)
#' x_across_y(a, e)
#' y_across_x(a, e)
#' @export
across <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)

  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- x_across_y(x, y) | y_across_x(x, y)
  return(r)
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

  sp_x <- start_point(x)
  sp_y <- start_point(y)
  ed_x <- end_point(x)
  ed_y <- end_point(y)

  r <- ((sp_x > sp_y & sp_x < ed_y) & ((ed_x < sp_y) | (ed_x > ed_y)))

  rm(sp_x, sp_y, ed_x, ed_y)
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

  sp_x <- start_point(x)
  sp_y <- start_point(y)
  ed_x <- end_point(x)
  ed_y <- end_point(y)

  r <- ((sp_y > sp_x & sp_y < ed_x) & ((ed_y < sp_x) | (ed_y > ed_x)))

  rm(sp_x, sp_y, ed_x, ed_y)
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
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- x_chain_y(x, y) | y_chain_x(x, y)

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' x_chain_y(c, d)
#' x_chain_y(a, c)
#' @export
x_chain_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start + x@.Data) == y@start & x@.Data != 0 & y@.Data != 0)
  r[which(!is.finite(r) | x@.Data * y@.Data < 0 | is.na(x@.Data * y@.Data))] <- FALSE

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' y_chain_x(c, d)
#' y_chain_x(a, c)
#' @export
y_chain_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((y@start + y@.Data) == x@start & x@.Data != 0 & y@.Data != 0)
  r[which(!is.finite(r) | x@.Data * y@.Data < 0 | is.na(x@.Data * y@.Data))] <- FALSE

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
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start == y@start) |
    (x@start == y@start + y@.Data & y@.Data < 0 & x@.Data >= 0) |
    (y@start == x@start + x@.Data & x@.Data < 0 & y@.Data >= 0) |
    (y@start + y@.Data == x@start + x@.Data & x@.Data < 0 & y@.Data < 0)) &
    !exact(x, y) & !reverse(x, y)

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
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start + x@.Data == y@start + y@.Data) |
          (x@start == y@start + y@.Data & x@.Data < 0 & y@.Data >= 0) |
          (y@start == x@start + x@.Data & y@.Data < 0 & x@.Data >= 0) |
          (y@start == x@start & x@.Data < 0 & y@.Data < 0)) &
    !exact(x, y) & !reverse(x, y)

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
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- x_inbetween_y(x, y) | y_inbetween_x(x, y)
  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' x_inbetween_y(a, g)
#' x_inbetween_y(b, a)
#' @export
x_inbetween_y <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((x@start > y@start & x@start < y@start + y@.Data) & (x@start + x@.Data > y@start & x@start + x@.Data < y@start + y@.Data))

  return(r)
}

#' @rdname overlaps
#' @examples
#'
#' y_inbetween_x(a, g)
#' y_inbetween_x(b, a)
#' @export
y_inbetween_x <- function(x, y){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  if(missing(y)) stop("argument `y` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", "number_line")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(y, "y", "number_line")
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())

  r <- ((y@start > x@start & y@start < x@start + x@.Data) & (y@start + y@.Data > x@start & y@start + y@.Data < x@start + x@.Data))

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
  if(length(x) == 0 & length(y) == 0) return(character())

  m <- rep("none", max(c(length(x), length(y))))
  lgk <- which(x_across_y(x, y) & m == "none")
  m[lgk] <- "x_across_y"
  lgk <- which(y_across_x(x, y) & m == "none")
  m[lgk] <- "y_across_x"
  lgk <- which(x_inbetween_y(x, y) & m == "none")
  m[lgk] <- "x_inbetween_y"
  lgk <- which(y_inbetween_x(x, y) & m == "none")
  m[lgk] <- "y_inbetween_x"
  lgk <- which(exact(x, y) & m == "none")
  m[lgk] <- "exact"
  lgk <- which(reverse(x, y) & m == "none")
  m[lgk] <- "reverse"
  lgk <- which(aligns_start(x, y) & m == "none")
  m[lgk] <- "aligns_start"
  lgk <- which(aligns_end(x, y) & m == "none")
  m[lgk] <- "aligns_end"
  lgk <- which(x_chain_y(x, y) & m == "none")
  m[lgk] <- "x_chain_y"
  lgk <- which(y_chain_x(x, y) & m == "none")
  m[lgk] <- "y_chain_x"

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
  lst <- c("overlap", "none", "exact",
           "x_across_y", "y_across_x",
           "x_chain_y", "y_chain_x",
           "aligns_start", "aligns_end",
           "x_inbetween_y", "y_inbetween_x",
           "reverse")
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
#' @examples
#'
#' exclude_overlap_method("across")
#' exclude_overlap_method(c("across", "chain"))
#' @export
exclude_overlap_method <- function(methods){
  err <- err_object_types(methods, "methods", "character")
  if(err != F) stop(err, call. = F)

  lst <- c("exact", "x_across_y", "y_across_x",
           "x_chain_y", "y_chain_x",
           "aligns_start", "aligns_end",
           "x_inbetween_y", "y_inbetween_x",
           "reverse")
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
  methods <- lst[!lst %in% methods]
  methods <- paste0(sort(methods), collapse = "|")
  overlap_method_codes(methods)
}

#' @rdname overlaps
#' @examples
#'
#' overlap_method_codes("across")
#' overlap_method_codes("across|chain|exact")
#' @export
overlap_method_codes <- function(methods){
  x2 <- methods[!duplicated(methods)]
  x2_cd <-  lapply(x2, function(x){
    lgk <- sapply(strsplit(as.vector(x), "\\|"), function(x){
      x <- x[x %in% names(diyar::overlap_methods$methods)]
      if(any(x == "overlap")){
        return(match("overlap", diyar::overlap_methods$options))
      }
      if(any(x == "none")){
        return(match("none", diyar::overlap_methods$options))
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
      x <- x[!duplicated(x)]
      x <- paste0(sort(x), collapse = "|")
      match(x, diyar::overlap_methods$options)
    })
    lgk
  })
  x2_cd <- unlist(x2_cd)
  x2_cd <- x2_cd[match(methods, x2)]
  return(x2_cd)
}

#' @rdname overlaps
#' @examples
#'
#' overlap_method_names(100)
#' overlap_method_names(561)
#' @export
overlap_method_names <- function(methods){
  nm <- rep(NA_character_, length(methods))
  lgk <- methods %in% unlist(diyar::overlap_methods$methods, use.names = FALSE)
  nm[lgk] <- diyar::overlap_methods$options[methods[lgk]]
  return(nm)
}
