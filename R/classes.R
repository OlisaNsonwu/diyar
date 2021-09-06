#' @name number_line-class
#' @aliases number_line-class
#' @title \code{number_line} object
#'
#' @description
#' S4 objects representing a range of numeric values
#'
#' @slot start First value in the range.
#' @slot id Unique element id. Optional.
#' @slot gid Unique group id. Optional.
#' @slot .Data Length, duration or width of the range.
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("number_line",
         contains = "numeric",
         representation(start = "ANY",
                        id = "integer",
                        gid = "integer"))

#' @rdname number_line-class
#' @param object object
setMethod("show", signature(object = "number_line"), function(object){
  print(format.number_line(object))
})

#' @rdname number_line-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "number_line"), function(x, ...) {
  methods::new("number_line",
               rep(x@.Data, ...),
               start = rep(x@start, ...),
               id = rep(x@id, ...),
               gid = rep(x@gid, ...))
})

#' @aliases [,number_line-method
#' @rdname number_line-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "number_line"),
          function(x, i, j, ..., drop = TRUE) {
            is_lazy_opt <- !is.null(attr(x, "opts"))
            is_lazy_opt[is_lazy_opt] <- attr(x, "opts") == "d_lazy_opts"
            if(is_lazy_opt){
             i <- 1
             attr(x, "opts") <- NULL
            }
            x@.Data <- x@.Data[i]
            x@start <- x@start[i]
            x@id <- x@id[i]
            x@gid <- x@gid[i]
            if(is_lazy_opt){
              attr(x, "opts") <- "d_lazy_opts"
            }
            return(x)
          })

#' @aliases [[,number_line-method
#' @rdname number_line-class
#' @param exact exact
setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            is_lazy_opt <- !is.null(attr(x, "opts"))
            is_lazy_opt[is_lazy_opt] <- attr(x, "opts") == "d_lazy_opts"
            if(is_lazy_opt){
              i <- 1
              attr(x, "opts") <- NULL
            }
            x@.Data <- x@.Data[i]
            x@start <- x@start[i]
            x@id <- x@id[i]
            x@gid <- x@gid[i]
            if(is_lazy_opt){
              attr(x, "opts") <- "d_lazy_opts"
            }
            return(x)
          })

#' @aliases [<-,number_line-method
#' @rdname number_line-class
#' @param value value
setMethod("[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  if (is.number_line(value)) {
    is_lazy_opt <- !is.null(attr(x, "opts"))
    is_lazy_opt[is_lazy_opt] <- attr(x, "opts") == "d_lazy_opts"
    if(is_lazy_opt){
      i <- 1
      attr(x, "opts") <- NULL
    }
    x@.Data[i] <- value@.Data
    x@start[i] <- value@start
    x@id[i] <- value@id
    x@gid[i] <- value@gid
    if(is_lazy_opt){
      attr(x, "opts") <- "d_lazy_opts"
    }
    return(x)
  }
})

#' @aliases [[<-,number_line-method
#' @rdname number_line-class
setMethod("[[<-", signature(x = "number_line"), function(x, i, j, ..., value) {
  if (is.number_line(value)) {
    is_lazy_opt <- !is.null(attr(x, "opts"))
    is_lazy_opt[is_lazy_opt] <- attr(x, "opts") == "d_lazy_opts"
    if(is_lazy_opt){
      i <- 1
      attr(x, "opts") <- NULL
    }
    x@.Data[i] <- value@.Data
    x@start[i] <- value@start
    x@id[i] <- value@id
    x@gid[i] <- value@gid
    if(is_lazy_opt){
      attr(x, "opts") <- "d_lazy_opts"
    }
    return(x)
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
  x <- to_s4(do.call("rbind", lapply(list(x, ...), function(y) as.data.frame(as.number_line(y)))))
  # x@id <- x@gid <- seq_len(length(x))
  return(x)
})

#' @rdname number_line-class
#' @export
unique.number_line <- function(x, ...){
  x <- x[!duplicated(combi(x@start, x@.Data))]
  return(x)
}

#' @rdname number_line-class
#' @param fill \code{[logical]}. Retain (\code{TRUE}) or drop (\code{FALSE}) the remainder of an uneven split
#' @param simplify \code{[logical]}. Split into \code{number_line} or sequence of finite numbers
#' @export
seq.number_line <- function(x,
                            fill = TRUE,
                            simplify = FALSE,
                            ...){
  x <- number_line_sequence(x,
                            fill = fill,
                            simplify = simplify,
                            ...)
  return(x)
}

#' @rdname number_line-class
#' @param decreasing If \code{TRUE}, sort in descending order.
#' @export
sort.number_line <- function(x, decreasing = FALSE, ...){
  x <- x[order(start_point(x),
               end_point(x),
               decreasing = decreasing)]
  return(x)
}

#' @rdname number_line-class
#' @export
format.number_line <- function(x, ...){
  if (length(x) == 0) "number_line(0)"
  else{
    s <- rep("??", length(x))
    s[x@.Data > 0 & !is.na(x@.Data) & !is.nan(x@.Data)] <- "->"
    s[x@.Data < 0 & !is.na(x@.Data) & !is.nan(x@.Data)] <- "<-"
    s[x@.Data == 0 & !is.na(x@.Data) & !is.nan(x@.Data)] <- "=="

    paste0(x@start, " ",
           s, " ",
           x@start + x@.Data)
  }
}

#' @rdname number_line-class
#' @export
as.list.number_line <- function(x, ...){
  x_df <- as.data.frame(x)
  cmbi_cd <-  combi(x_df$start, x_df$end, x_df$id, x_df$gid)
  x_dups <- x[!duplicated(cmbi_cd)]
  y <- lapply(seq_len(length(x_dups)), function(j) x_dups[j])
  y <- y[match(cmbi_cd, cmbi_cd[!duplicated(cmbi_cd)])]
  return(y)
}

#' @rdname number_line-class
#' @export
as.data.frame.number_line <- function(x, ...){
  y <- data.frame(start = x@start,
                  end = x@start + x@.Data,
                  id = x@id,
                  gid = x@gid,
                  ...)
  return(y)
}

#' @name epid-class
#' @title \code{epid} object
#'
#' @slot sn Unique record identifier.
#' @slot .Data Unique \code{episode} identifier.
#' @slot wind_id Unique window identifier.
#' @slot wind_nm Type of window i.e. "Case" or "Recurrence".
#' @slot case_nm Record type in regards to case assignment.
#' @slot dist_wind_index Unit difference between each record and its window's reference record.
#' @slot dist_epid_index Unit difference between each record and its episode's reference record.
#' @slot epid_dataset Data sources in each \code{episode}.
#' @slot epid_interval The start and end dates of each \code{episode}. A \code{\link{number_line}} object.
#' @slot epid_length The duration or length of (\code{epid_interval}).
#' @slot epid_total The number of records in each \code{episode}.
#' @slot iteration The iteration of the tracking process when a record was linked to its episode.
#' @slot options Some options passed to the instance of \code{\link{episodes}}.
#'
#' @description
#' S4 objects storing the result of \code{\link{episodes}}.
#'
#' @aliases epid-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("epid",
         contains = "integer",
         representation(sn = "integer",
                        wind_id = "list",
                        wind_nm = "ANY",
                        case_nm = "ANY",
                        dist_wind_index = "ANY",
                        dist_epid_index = "ANY",
                        epid_interval = "number_line",
                        epid_length = "ANY",
                        epid_total = "integer",
                        epid_dataset = "ANY",
                        iteration = "integer",
                        options = "list"))

#' @rdname epid-class
#' @examples
#' # A test for `epid` objects
#' ep <- episodes(date = 1)
#' is.epid(ep); is.epid(2)
#'
#' @export
is.epid <- function(x) all(class(x) == "epid")

#' @rdname epid-class
#' @export
as.epid <- function(x){
  x <- match(x, x[!duplicated(x)])
  tots <- rle(sort(x))

  x <- methods::new("epid",
                    .Data = x,
                    sn = seq_len(length(x)),
                    wind_id = list(wind_id1 = x))

  x@dist_epid_index <- x@dist_wind_index <-
    x@case_nm <- x@wind_nm <-
    rep(0L, length(x))
  x@case_nm[duplicated(x@.Data)] <- 2L
  x@wind_nm[duplicated(x@.Data)] <- 1L

  class(x@case_nm) <- "d_label"
  attr(x@case_nm, "value") <- -1L : 5L
  attr(x@case_nm, "label") <- c("Skipped", "Case", "Recurrent", "Duplicate_C", "Duplicate_R", "Case_CR", "Recurrent_CR")
  attr(x@case_nm, "state") <- "encoded"

  class(x@wind_nm) <- "d_label"
  attr(x@wind_nm, "value") <- -1L : 1L
  attr(x@wind_nm, "label") <- c("Skipped", "Case", "Recurrence")
  attr(x@wind_nm, "state") <- "encoded"

  x@epid_total <- tots$lengths[match(x, tots$values)]
  x@iteration <- rep(1L, length(x))

  x@options <- list(date = x@.Data,
                    strata = x@.Data,
                    case_length = list(Inf),
                    recurrence_length = list(Inf))

  x@options$episode_unit <- "seconds"
  x@options$episode_unit <- match(x@options$episode_unit, names(diyar::episode_unit))

  class(x@options$episode_unit) <- "d_label"
  attr(x@options$episode_unit, "value") <- as.vector(sort(x@options$episode_unit[!duplicated(x@options$episode_unit)]))
  attr(x@options$episode_unit, "label") <- names(diyar::episode_unit)[attr(x@options$episode_unit, "value")]
  attr(x@options$episode_unit, "state") <- "encoded"

  x@options$from_last <- FALSE

  return(x)
}

#' @rdname epid-class
#' @export
format.epid <- function(x, ...){
  if (length(x) == 0) {
    return("epid(0)")
  }else {
    int_l <- rep("", length(x))
    int_l[!is.na(x@epid_interval)] <- paste0(" ",
                                             format.number_line(x@epid_interval[!is.na(x@epid_interval)]))
    return(paste0("E.",
                  formatC(x@.Data, width = nchar(max(x@.Data)), flag = 0, format = "fg"),
                  int_l,
                  " (", c("S", "C", "R", "D", "D", "C","R")[x@case_nm + 2L], ")"))
  }
}

#' @rdname epid-class
#' @export
unique.epid <- function(x, ...){
  return(x[x@case_nm == 0])
}

#' @rdname epid-class
#' @export
summary.epid <- function(object, ...){
  summ <- list()
  summ$iterations <- max(object@iteration)
  summ$total_records <- length(object)
  summ$total_episodes <- length(object[object@case_nm == 0])
  x <- object[order(-object@wind_nm)]
  x <- x[x@case_nm != -1]
  x <- decode(x@wind_nm[!duplicated(x@.Data)])
  x[x == "Case"] <- "Fixed"
  x[x == "Recurrence"] <- "Rolling"
  summ$episode_type <- dst_tab(x = x, order_by_label = c("Fixed", "Rolling"))
  x <- object[order(object@wind_id$wind_id1)]
  x <- x[x@wind_nm == 1 & !duplicated(x@wind_id$wind_id1)]
  x <- rle(x@.Data)
  summ$recurrence <- dst_tab(x = paste0(x$lengths[order(x$lengths)]))
  if(length(summ$recurrence$values) > 0){
    summ$recurrence$values <- paste0(summ$recurrence$values, " times")
    summ$recurrence$values[summ$recurrence$values == "1 times"] <- "1 time"
  }
  summ$case_nm <- dst_tab(x = decode(object@case_nm[order(object@case_nm)]), order_by_label = c("Case", "Duplicate_C", "Recurrent", "Duplicate_R", "Skipped"))
  x <- object@epid_total[object@case_nm == 0]
  summ$epid_total <- dst_tab(x[order(x)])
  if(length(summ$epid_total$values) > 0){
    summ$epid_total$values <- paste0(summ$epid_total$values, " records")
    summ$epid_total$values[summ$epid_total$values == "1 records"] <- "1 record"
  }
  summ$epid_length <- if(is.null(object@epid_length)) list(values = numeric(), length = numeric()) else dst_tab(x = format((object@epid_length[object@case_nm == 0])[order(object@epid_length[object@case_nm == 0])], trim = TRUE))
  summ$data_source <- if(is.null(object@epid_dataset)) list(values = numeric(), length = numeric()) else dst_tab(x = decode((object@epid_dataset[object@case_nm == 0])[order(object@epid_dataset[object@case_nm == 0])]), order_by_label = sort(attr(object@epid_dataset, "label")))
  class(summ) <- "epid_summary"
  return(summ)
}

#' @rdname epid-class
#' @export

print.epid_summary <- function(x, ...){
  dsts <- c("case_nm", "data_source",
            "epid_total","epid_length","episode_type",
            "recurrence")
  mx_ds_len <- lapply(dsts, function(l){
    val <- x[[l]]$values
    nchar(if(length(val) > 5) val[1:5] else val)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    val <- x[[l]]$values
    xlen <- fmt(x[[l]]$lengths)
    if(length(val) > 5) val <- c(val[1:5], "..truncated..")
    if(length(xlen) > 5) xlen <- c(xlen[1:5], "..truncated..")
    ds_len <- nchar(val)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(val) > 20,
                     paste0(substr(val, 1, 20), "~"),
                     val)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     ", ds_txt, ":", pd_txt,
                       xlen, collapse = "\n")
    }else{
      ds_txt <- "     N/A"
    }
    ds_txt <- gsub("..truncated..:", "..truncated..", ds_txt)
  })
  ds_txts <- unlist(ds_txts, use.names = FALSE)
  names(ds_txts) <- dsts
  mx_ds_len <- mx_ds_len + mx_pd_len

  msg <- paste0("Iterations:", paste0(paste0(rep(" ", (mx_ds_len - 6) + 7), collapse = ""), fmt(x$iteration)), "\n",
                "Total records:", paste0(paste0(rep(" ", (mx_ds_len - 9) + 7), collapse = ""), fmt(x$total_records)), "\n",
                " by record type:", "\n",
                paste0(ds_txts["case_nm"], "\n"),
                "Total episodes:", paste0(paste0(rep(" ", (mx_ds_len - 10) + 7), collapse = ""), fmt(x$total_episodes)), "\n",
                " by episode type:", "\n",
                paste0(ds_txts["episode_type"], "\n"),
                " by episode dataset:", "\n",
                paste0(ds_txts["data_source"], "\n"),
                " by episodes duration:", "\n",
                paste0(ds_txts["epid_length"], "\n"),
                " by records per episode:", "\n",
                paste0(ds_txts["epid_total"], "\n"),
                " by recurrence:", "\n",
                paste0(ds_txts["recurrence"], "\n"))

  cat(msg)
}

#' @rdname epid-class
#' @export
as.data.frame.epid <- function(x, ...){
  y <- data.frame(epid = x@.Data,
                  sn = x@sn,
                  wind_nm = as.vector(decode(x@wind_nm)),
                  case_nm = as.vector(decode(x@case_nm)),
                  dist_wind_index = x@dist_wind_index,
                  dist_epid_index = x@dist_epid_index,
                  epid_total = x@epid_total,
                  iteration = x@iteration,
                  ...)
  y <- cbind(y, as.data.frame(x@wind_id, ...))
  if(length(x@epid_interval@start) != 0){
    y$epid_start <- x@epid_interval@start
    y$epid_end <- right_point(x@epid_interval)
  }else{
    y$epid_start <- NA_integer_
    y$epid_end <- NA_integer_
  }
  if(length(x@epid_length) != 0){
    y$epid_length = x@epid_length
  }else{
    y$epid_length = NA_integer_
  }
  if(length(x@epid_dataset) != 0){
    y$epid_dataset = as.vector(decode(x@epid_dataset))
  }else{
    y$epid_dataset = NA_character_
  }
  return(y)
}

#' @rdname epid-class
#' @export
as.list.epid <- function(x, ...){
  y <- list(epid = x@.Data,
            sn = x@sn,
            wind_nm = x@wind_nm,
            case_nm = x@case_nm,
            dist_wind_index = x@dist_wind_index,
            dist_epid_index = x@dist_epid_index,
            epid_total = x@epid_total,
            iteration = x@iteration,
            ...)
  y <- c(y, x@wind_id)
  if(length(x@epid_interval@start) != 0){
    y$epid_start <- x@epid_interval@start
    y$epid_end <- right_point(x@epid_interval)
  }else{
    y$epid_start <- rep(NA_integer_, length(x))
    y$epid_end <- rep(NA_integer_, length(x))
  }
  if(length(x@epid_length) != 0){
    y$epid_length <- x@epid_length
  }else{
    y$epid_length <- rep(NA_integer_, length(x))
  }
  if(length(x@epid_dataset) != 0){
    y$epid_dataset <- x@epid_dataset
  }else{
    y$epid_dataset <- rep(NA_character_, length(x))
  }
  return(y)
}

#' @rdname epid-class
#' @param object object
setMethod("show", signature(object = "epid"), function(object){
  print(format.epid(object))
})

#' @rdname epid-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "epid"), function(x, ...) {
  methods::new("epid",
               rep(x@.Data, ...),
               sn = rep(x@sn, ...),
               wind_id = lapply(x@wind_id, function(y) rep(y, ...)),
               dist_epid_index = rep(x@dist_epid_index, ...),
               dist_wind_index = rep(x@dist_wind_index, ...),
               wind_nm = rep(x@wind_nm, ...),
               case_nm = rep(x@case_nm, ...),
               epid_interval = rep(x@epid_interval, ...),
               epid_length = rep(x@epid_length, ...),
               epid_total = rep(x@epid_total, ...),
               epid_dataset = rep(x@epid_dataset, ...),
               iteration = rep(x@iteration, ...))
})

#' @aliases [,epid-method
#' @rdname epid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "epid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("epid",
                         x@.Data[i],
                         case_nm = x@case_nm[i],
                         sn = x@sn[i],
                         wind_id = lapply(x@wind_id, function(y) y[i]),
                         wind_nm = x@wind_nm[i],
                         dist_epid_index = x@dist_epid_index[i],
                         dist_wind_index = x@dist_wind_index[i],
                         epid_length = x@epid_length[i],
                         epid_total = x@epid_total[i],
                         epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i],
                         iteration = x@iteration[i],
                         options = list(date = x@options$date[i],
                                        strata = x@options$date[i],
                                        case_length = lapply(x@options$case_length, function(y) if(length(y) == 1) y else y[i]),
                                        recurrence_length = lapply(x@options$recurrence_length, function(y) if(length(y) == 1) y else y[i]),
                                        episode_type = if(length(x@options$episode_type) == 1) x@options$episode_type else x@options$episode_type[i],
                                        episode_unit = if(length(x@options$episode_unit) == 1) x@options$episode_unit else x@options$episode_unit[i],
                                        from_last = if(length(x@options$from_last) == 1) x@options$from_last else x@options$from_last[i]))
          })

#' @aliases [[,epid-method
#' @rdname epid-class
#' @param exact exact
setMethod("[[", signature(x = "epid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("epid",
                         x@.Data[i],
                         case_nm = x@case_nm[i],
                         sn = x@sn[i],
                         wind_id = lapply(x@wind_id, function(y) y[i]),
                         wind_nm = x@wind_nm[i],
                         dist_epid_index = x@dist_epid_index[i],
                         dist_wind_index = x@dist_wind_index[i],
                         epid_length = x@epid_length[i],
                         epid_total = x@epid_total[i],
                         epid_dataset = x@epid_dataset[i],
                         epid_interval = x@epid_interval[i],
                         iteration = x@iteration[i],
                         options = list(date = x@options$date[i],
                                        strata = x@options$date[i],
                                        case_length = lapply(x@options$case_length, function(y) if(length(y) == 1) y else y[i]),
                                        recurrence_length = lapply(x@options$recurrence_length, function(y) if(length(y) == 1) y else y[i]),
                                        episode_type = if(length(x@options$episode_type) == 1) x@options$episode_type else x@options$episode_type[i],
                                        episode_unit = if(length(x@options$episode_unit) == 1) x@options$episode_unit else x@options$episode_unit[i],
                                        from_last = if(length(x@options$from_last) == 1) x@options$from_last else x@options$from_last[i]))
          })

#' @rdname epid-class
setMethod("c", signature(x = "epid"), function(x,...) {
  x <- to_s4(do.call("rbind", lapply(list(x, ...), as.data.frame)))
  for (vr in methods::slotNames(x)){
    if(all(is.na(methods::slot(x, vr)))){
      methods::slot(x, vr) <- NULL
    }
  }
  x
})


#' @name pane-class
#' @title \code{pane} object
#'
#' @description
#' S4 objects storing the result of \code{\link{partitions}}.
#'
#' @slot sn Unique record identifier.
#' @slot .Data Unique \code{pane} identifier.
#' @slot case_nm Record type in regards to index assignment.
#' @slot window_list A list of considered \code{windows} for each \code{pane}.
#' @slot dist_pane_index The difference between each event and it's index event.
#' @slot pane_dataset Data sources in each \code{pane}.
#' @slot pane_interval The start and end dates of each \code{pane}. A \code{\link{number_line}} object.
#' @slot pane_length The duration or length of (\code{pane_interval}).
#' @slot pane_total The number of records in each \code{pane}.
#' @slot options Some options passed to the instance of \code{\link{partitions}}.
#' @slot window_matched A list of matched \code{windows} for each \code{pane}.
#'
#' @aliases pane-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("pane",
         contains = "integer",
         representation(sn = "integer",
                        case_nm = "ANY",
                        dist_pane_index = "ANY",
                        window_list = "list",
                        window_matched = "integer",
                        pane_interval = "number_line",
                        pane_length = "ANY",
                        pane_total = "integer",
                        pane_dataset = "ANY",
                        options = "list"))

#' @rdname pane-class
#' @examples
#' # A test for pane objects
#' pn <- partitions(date = 1, by = 1)
#' is.pane(pn); is.pane(2)
#'
#' @export
is.pane <- function(x) all(class(x) == "pane")

#' @rdname pane-class
#' @export
as.pane <- function(x){
  x <- match(x, x[!duplicated(x)])
  tots <- rle(sort(x))

  x <- methods::new("pane",
                    .Data = x,
                    sn = seq_len(length(x)))

  x@dist_pane_index <- x@case_nm <-
    rep(0L, length(x))
  x@case_nm[duplicated(x@.Data)] <- 1L

  class(x@case_nm) <- "d_label"
  attr(x@case_nm, "value") <- c(-1, 0, 1)
  attr(x@case_nm, "label") <- c("Skipped", "Index", "Duplicate_I")
  attr(x@case_nm, "state") <- "encoded"

  x@pane_total <- tots$lengths[match(x, tots$values)]
  x@window_list <- rep(list("1" = number_line(0, Inf)), length(x))
  x@window_matched <- rep(1L, length(x))
  x@options <- list(date = x@.Data,
                    strata = x@.Data,
                    separate = FALSE)
  return(x)
}

#' @rdname pane-class
#' @export
format.pane <- function(x, ...){
  if (length(x) == 0) {
    return("pane(0)")
  }else {
    return(paste0("PN.",
                  formatC(x@.Data, width = nchar(max(x@.Data)), flag = 0, format = "fg"),
                  ifelse(is.na(x@pane_interval),
                         "",
                         paste0(" ", format.number_line(x@pane_interval))),
                  " (", c("S", "I", "D")[x@case_nm + 2L], ")"))
  }
}

#' @rdname pane-class
#' @export
unique.pane <- function(x, ...){
  x <- x[x@case_nm == 0]
  return(x)
}

#' @rdname pane-class
#' @export
summary.pane <- function(object, ...){
  summ <- list()
  summ$total_records <- length(object)
  summ$total_panes <- length(object[object@case_nm == 0])
  summ$case_nm <- dst_tab(x = decode(object@case_nm[order(object@case_nm)]), order_by_label = c("Index", "Duplicate_I", "Skipped"))
  x <- object@pane_total[object@case_nm == 0]
  summ$pane_total <- dst_tab(x[order(x)])
  if(length(summ$pane_total$values) > 0){
    summ$pane_total$values <- paste0(summ$pane_total$values, " records")
    summ$pane_total$values[summ$pane_total$values == "1 records"] <- "1 record"
  }
  summ$pane_length <- if(is.null(object@pane_length)) list(values = numeric(), length = numeric()) else dst_tab(x = format((object@pane_length[object@case_nm == 0])[order(object@pane_length[object@case_nm == 0])]))
  summ$data_source <- if(is.null(object@pane_dataset)) list(values = numeric(), length = numeric()) else dst_tab(x = decode((object@pane_dataset[object@case_nm == 0])[order(object@pane_dataset[object@case_nm == 0])]), order_by_label = sort(attr(object@pane_dataset, "label")))
  class(summ) <- "pane_summary"
  return(summ)
}

#' @rdname pane-class
#' @export
print.pane_summary <- function(x, ...){
  dsts <- c("case_nm", "data_source",
            "pane_total","pane_length")
  mx_ds_len <- lapply(dsts, function(l){
    val <- x[[l]]$values
    nchar(if(length(val) > 5) val[1:5] else val)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    val <- x[[l]]$values
    xlen <- fmt(x[[l]]$lengths)
    if(length(val) > 5) val <- c(val[1:5], "..truncated..")
    if(length(xlen) > 5) xlen <- c(xlen[1:5], "..truncated..")
    ds_len <- nchar(val)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(val) > 20,
                     paste0(substr(val, 1, 20), "~"),
                     val)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     ", ds_txt, ":", pd_txt,
                       xlen, collapse = "\n")
    }else{
      ds_txt <- "     N/A"
    }
    ds_txt <- gsub("..truncated..:", "..truncated..", ds_txt)
    ds_txt
  })
  ds_txts <- unlist(ds_txts, use.names = FALSE)
  names(ds_txts) <- dsts
  mx_ds_len <- mx_ds_len + mx_pd_len

  msg <- paste0("Iterations:", paste0(paste0(rep(" ", (mx_ds_len - 6) + 7), collapse = ""), "N/A"), "\n",
                "Total records:", paste0(paste0(rep(" ", (mx_ds_len - 9) + 7), collapse = ""), fmt(x$total_records)), "\n",
                " by record type:", "\n",
                paste0(ds_txts["case_nm"], "\n"),
                "Total panes:", paste0(paste0(rep(" ", (mx_ds_len - 10) + 7), collapse = ""), fmt(x$total_episodes)), "\n",
                " by pane dataset:", "\n",
                paste0(ds_txts["data_source"], "\n"),
                " by pane duration:", "\n",
                paste0(ds_txts["pane_length"], "\n"),
                " by records per pane:", "\n",
                paste0(ds_txts["pane_total"], "\n"))

  cat(msg)
}

#' @rdname pane-class
#' @export
as.data.frame.pane <- function(x, ...){
  y <- data.frame(pane = x@.Data,
                  sn = x@sn,
                  case_nm = as.vector(decode(x@case_nm)),
                  dist_pane_index = x@dist_pane_index,
                  window_matched = x@window_matched,
                  pane_total = x@pane_total,
                  ...)
  if(length(x@pane_interval) != 0){
    y$pane_start <- x@pane_interval@start
    y$pane_end <- right_point(x@pane_interval)
  }else{
    y$pane_start <- NA_integer_
    y$pane_end <- NA_integer_
  }
  if(length(x@pane_length) != 0){
    y$pane_length <- x@pane_length
  }else{
    y$pane_length <- NA_integer_
  }
  if(length(x@pane_dataset) != 0){
    y$pane_dataset <- decode(x@pane_dataset)
  }else{
    y$pane_dataset <- NA_character_
  }
  window_list <- lapply(x@window_list[!duplicated(x@window_list)], function(x){
    listr(format(number_line(round(x@start, 2), round(right_point(x), 2))), conj = ",")
  })
  y$window_list <- as.character(window_list[match(names(x@window_list), names(window_list))])
  return(y)
}

#' @rdname pane-class
#' @export
as.list.pane <- function(x, ...){
  y <- list(pane = x@.Data,
            sn = x@sn,
            case_nm = decode(x@case_nm),
            dist_pane_index = x@dist_pane_index,
            dist_pane_index = x@dist_pane_index,
            window_matched = x@window_matched,
            pane_total = x@pane_total,
            ...)
  if(length(x@pane_interval) != 0){
    y$pane_start <- x@pane_interval@start
    y$pane_end <- right_point(x@pane_interval)
  }else{
    y$pane_start <- rep(NA_integer_, length(x))
    y$pane_end <- rep(NA_integer_, length(x))
  }
  if(length(x@pane_length) != 0){
    y$pane_length <- x@pane_length
  }else{
    y$pane_length <- rep(NA_integer_, length(x))
  }
  if(length(x@pane_dataset) != 0){
    y$pane_dataset <- decode(x@pane_dataset)
  }else{
    y$pane_dataset <- rep(NA_character_, length(x))
  }
  y$window_list <- x@window_list
  return(y)
}

#' @rdname pane-class
#' @param object object
setMethod("show", signature(object = "pane"), function(object){
  print(format.pane(object))
})

#' @rdname pane-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "pane"), function(x, ...) {
  methods::new("pane",
               rep(x@.Data, ...),
               sn = rep(x@sn, ...),
               window_matched = rep(x@window_matched, ...),
               dist_pane_index = rep(x@dist_pane_index, ...),
               case_nm = rep(x@case_nm, ...),
               pane_interval = rep(x@pane_interval, ...),
               pane_length = rep(x@pane_length, ...),
               pane_total = rep(x@pane_total, ...),
               pane_dataset = rep(x@pane_dataset, ...))
})

#' @aliases [,pane-method
#' @rdname pane-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "pane"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("pane", x@.Data[i],
                         case_nm = x@case_nm[i],
                         sn = x@sn[i],
                         window_matched = x@window_matched[i],
                         dist_pane_index = x@dist_pane_index[i],
                         pane_length = x@pane_length[i],
                         pane_total = x@pane_total[i],
                         pane_dataset = x@pane_dataset[i],
                         pane_interval = x@pane_interval[i],
                         window_list = x@window_list[i],
                         options = list(date = x@options$date[i],
                                        separate = if(length(x@options$separate) == 1) x@options$separate else x@options$separate[i],
                                        strata = x@options$strata[i]))
          })

#' @aliases [[,pane-method
#' @rdname pane-class
#' @param exact exact
setMethod("[[", signature(x = "pane"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("pane",
                         x@.Data[i],
                         case_nm = x@case_nm[i],
                         sn = x@sn[i],
                         window_matched = x@window_matched[i],
                         dist_pane_index = x@dist_pane_index[i],
                         pane_length = x@pane_length[i],
                         pane_total = x@pane_total[i],
                         pane_dataset = x@pane_dataset[i],
                         pane_interval = x@pane_interval[i],
                         window_list = x@window_list[i],
                         options = list(date = x@options$date[i],
                                        separate = if(length(x@options$separate) == 1) x@options$separate else x@options$separate[i],
                                        strata = x@options$strata[i]))
          })

#' @rdname pane-class
setMethod("c", signature(x = "pane"), function(x,...) {
  x <- to_s4(do.call("rbind", lapply(list(x, ...), as.data.frame)))
  for (vr in methods::slotNames(x)){
    if(all(is.na(methods::slot(x, vr)))){
      methods::slot(x, vr) <- NULL
    }
  }
  x
})

#' @name pid-class
#'
#' @title \code{pid} objects
#'
#' @description
#' S4 objects storing the result of \code{\link{links}}.
#'
#' @slot sn Unique record identifier.
#' @slot .Data Unique group identifier.
#' @slot link_id Unique record identifier for matching records.
#' @slot pid_cri Matching criteria.
#' @slot pid_dataset Data sources in each group.
#' @slot pid_total The number of records in each group.
#' @slot iteration The iteration of the linkage process when a record was linked to its group.
#'
#' @aliases pid-class
#' @importFrom "methods" "new"
#' @importFrom "utils" "head"
#' @export
setClass("pid",
         contains = "integer",
         representation(sn = "integer",
                        pid_cri = "integer",
                        link_id = "integer",
                        pid_dataset = "ANY",
                        pid_total = "integer",
                        iteration = "integer"))

#' @rdname pid-class
#' @examples
#' # A test for pid objects
#' pd <- links(criteria = 1)
#' is.pid(pd); is.pid(2)
#'
#' @export
is.pid <- function(x) all(class(x) == "pid")

#' @rdname pid-class
#' @export
as.pid <- function(x, ...){
  x <- match(x, x[!duplicated(x)])
  tots <- rle(sort(x))
  x <- methods::new("pid",
                    .Data = x,
                    sn = seq_len(length(x)),
                    pid_cri = rep(1L, length(x)),
                    link_id = x,
                    pid_total = tots$lengths[match(x, tots$values)],
                    pid_dataset = NULL,
                    iteration = rep(1L, length(x)))
  return(x)
}

#' @rdname pid-class
#' @export
format.pid <- function(x, ...){
  if (length(x) == 0) {
    return("pid(0)")
  }else{
    return(paste0("P.",
                  formatC(x@.Data, width = nchar(max(x@.Data)), flag = 0, format = "fg"),
                  " (", pid_cri_l(x@pid_cri), ")" ))
  }
}

#' @rdname pid-class
#' @export
unique.pid <- function(x, ...){
  x <- x[!duplicated(x@.Data)]
  return(x)
}

#' @rdname pid-class
#' @export
summary.pid <- function(object, ...){
  summ <- list()
  summ$iterations <- max(object@iteration)
  summ$total_records <- length(object)
  summ$total_groups <- length(object[!duplicated(object@.Data)])
  x <- object@pid_total[!duplicated(object@.Data)]
  summ$pid_total <- dst_tab(x[order(x)])
  if(length(summ$pid_total$values) > 0){
    summ$pid_total$values <- paste0(summ$pid_total$values, " records")
    summ$pid_total$values[summ$pid_total$values == "1 records"] <- "1 record"
  }
  x <- object@pid_cri
  l <- x[!duplicated(x)]
  l <- l[!l %in% -1:0]
  l <- c(sort(l), 0, -1)
  summ$pid_cri <- dst_tab(object@pid_cri[order(object@pid_cri)], order_by_label = l)
  rm(x, l)
  summ$pid_cri$values[summ$pid_cri$values == 0] <- "No hits"
  summ$pid_cri$values[summ$pid_cri$values == "-1"] <- "Skipped"
  lgk <- !summ$pid_cri$values %in% c("Skipped", "No hits")
  summ$pid_cri$values[lgk] <- paste0("Criteria ", summ$pid_cri$values[lgk])
  summ$data_source <- if(is.null(object@pid_dataset)) list(values = numeric(), length = numeric()) else dst_tab(x = decode((object@pid_dataset[!duplicated(object@.Data)])[order(object@pid_dataset[!duplicated(object@.Data)])]), order_by_label = sort(attr(object@pid_dataset, "label")))
  class(summ) <- "pid_summary"
  return(summ)
}

#' @rdname pid-class
#' @export
print.pid_summary <- function(x, ...){
  dsts <- c("pid_cri", "data_source", "pid_total")
  mx_ds_len <- lapply(dsts, function(l){
    val <- x[[l]]$values
    nchar(if(length(val) > 5 & l != "pid_cri") val[1:5] else val)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    val <- x[[l]]$values
    xlen <- fmt(x[[l]]$lengths)
    if(length(val) > 5 & l != "pid_cri") val <- c(val[1:5], "..truncated..")
    if(length(xlen) > 5 & l != "pid_cri") xlen <- c(xlen[1:5], "..truncated..")
    ds_len <- nchar(val)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(val) > 20,
                     paste0(substr(val, 1, 20), "~"),
                     val)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     ", ds_txt, ":", pd_txt,
                       xlen, collapse = "\n")
    }else{
      ds_txt <- "     N/A"
    }
    ds_txt
  })
  ds_txts <- unlist(ds_txts, use.names = FALSE)
  names(ds_txts) <- dsts
  mx_ds_len <- mx_ds_len + mx_pd_len

  msg <- paste0("Iterations:", paste0(paste0(rep(" ", (mx_ds_len - 6) + 7), collapse = ""), fmt(x$iteration)), "\n",
                "Total records:", paste0(paste0(rep(" ", (mx_ds_len - 9) + 7), collapse = ""), fmt(x$total_records)), "\n",
                " by matching criteria:", "\n",
                paste0(ds_txts["pid_cri"], "\n"),
                "Total record groups:", paste0(paste0(rep(" ", (mx_ds_len - 10) + 7), collapse = ""), fmt(x$total_episodes)), "\n",
                " by group dataset:", "\n",
                paste0(ds_txts["data_source"], "\n"),
                " by records per group:", "\n",
                paste0(ds_txts["pid_total"], "\n"))

  cat(msg)
}

#' @rdname pid-class
#' @export
as.data.frame.pid <- function(x, ...){
  y <- data.frame(pid = x@.Data,
                  sn = x@sn,
                  pid_cri = x@pid_cri,
                  link_id = x@link_id,
                  pid_total = x@pid_total,
                  iteration = x@iteration,
                  ...)
  if(length(x@pid_dataset) != 0){
    y$pid_dataset <- as.vector(decode(x@pid_dataset))
  }else{
    y$pid_dataset <- NA_character_
  }
  return(y)
}

#' @rdname pid-class
#' @export
as.list.pid <- function(x, ...){
  y <- list(pid = x@.Data,
            sn = x@sn,
            pid_cri = x@pid_cri,
            link_id = x@link_id,
            pid_total = x@pid_total,
            iteration = x@iteration,
            ...)
  if(length(x@pid_dataset) != 0){
    y$pid_dataset <- x@pid_dataset
  }else{
    y$pid_dataset <- rep(NA_character_, length(x))
  }
  return(y)
}

#' @rdname pid-class
#' @param object object
setMethod("show", signature(object = "pid"), function(object){
  print(format.pid(object))
})

#' @rdname pid-class
#' @param x x
#' @param ... ...
setMethod("rep", signature(x = "pid"), function(x, ...) {
  methods::new("pid", rep(x@.Data, ...),
               sn = rep(x@sn, ...),
               pid_total = rep(x@pid_total, ...),
               link_id = rep(x@link_id, ...),
               pid_dataset = rep(x@pid_dataset, ...),
               pid_cri = rep(x@pid_cri, ...),
               iteration = rep(x@iteration, ...))
})

#' @aliases [,pid-method
#' @rdname pid-class
#' @param i i
#' @param j j
#' @param drop drop
setMethod("[", signature(x = "pid"),
          function(x, i, j, ..., drop = TRUE) {
            methods::new("pid", x@.Data[i],
                         pid_cri = x@pid_cri[i],
                         sn = x@sn[i],
                         link_id = x@link_id[i],
                         pid_total = x@pid_total[i],
                         pid_dataset = x@pid_dataset[i],
                         iteration = x@iteration[i])
          })

#' @aliases [[,pid-method
#' @rdname pid-class
#' @param exact exact
setMethod("[[", signature(x = "pid"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("pid", x@.Data[i],
                         pid_cri = x@pid_cri[i],
                         sn = x@sn[i],
                         link_id = x@link_id[i],
                         pid_total = x@pid_total[i],
                         pid_dataset = x@pid_dataset[i],
                         iteration = x@iteration[i])
          })

#' @rdname pid-class
setMethod("c", signature(x = "pid"), function(x,...) {
  x <- to_s4(do.call("rbind", lapply(list(x, ...), as.data.frame)))
  for (vr in methods::slotNames(x)){
    if(all(is.na(methods::slot(x, vr)))){
      methods::slot(x, vr) <- NULL
    }
  }
  x
})

#' @name d_report
#' @title d_report
#' @aliases d_report
#' @export
plot.d_report <- function(x, ...){
  . <- NULL
  t <- length(x$iteration)
  x <- data.frame(x = c(x$iteration, x$iteration, x$iteration, x$iteration),
                   y = c(as.numeric(x$duration), x$records_checked, x$records_tracked, x$records_skipped),
                   l = c(rep(paste0("duration (", attr(x$duration, "units"), ")"), t),
                         rep("records_checked", t), rep("records_tracked", t),
                         rep("records_skipped", t)),
                   stringsAsFactors = FALSE)
  x$x_cd <- match(x$x, x$x)
  x_breaks <- x$x_cd[!duplicated(x$x)]
  x_labs <- x$x[!duplicated(x$x)]

  ggplot2::ggplot(data = x, ggplot2::aes(.data$x_cd, .data$y)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~ .data$l, ncol =2, scales = "free") +
    ggplot2::scale_x_continuous("Iteration", labels = x_labs[seq(1, length(x_labs), length.out = 10)],
                                breaks = x_breaks[seq(1, length(x_labs), length.out = 10)])
}

#' @rdname d_report
#' @export
as.list.d_report <- function(x, ...){
  class(x) <- NULL
  return(as.list(x, ..))
}

#' @rdname d_report
#' @export
as.data.frame.d_report <- function(x, ...){
  return(as.data.frame(as.list(x), ...))
}


`[.d_lazy_opts` <- function(x, i, ..., drop = TRUE) {
  if(length(x) == 1){
    return(x)
  }else{
    x <- as.vector(x)
    x <- x[i]
    class(x) <- "d_lazy_opts"
    return(x)
  }
}

`[<-.d_lazy_opts` <- function(x, i, j, ..., value) {
  if(length(x) == 1){
    i <- 1
  }
  x <- as.vector(x)
  x[i] <- value
  class(x) <- "d_lazy_opts"
  return(x)
}
