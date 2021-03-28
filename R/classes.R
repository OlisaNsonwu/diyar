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
            methods::new("number_line",
                         x@.Data[i],
                         start = x@start[i],
                         id = x@id[i],
                         gid = x@gid[i])
          })

#' @aliases [[,number_line-method
#' @rdname number_line-class
#' @param exact exact
setMethod("[[", signature(x = "number_line"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("number_line",
                         x@.Data[i],
                         start = x@start[i],
                         id = x@id[i],
                         gid = x@gid[i])
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
    new("number_line",
        x@.Data,
        start = x@start,
        id = x@id,
        gid = x@gid)
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
    new("number_line",
        x@.Data,
        start = x@start,
        id = x@id,
        gid = x@gid)
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
  x <- to_s4(do.call("rbind", lapply(list(x, ...), function(y) to_df(as.number_line(y)))))
  x@id <- x@gid <- seq_len(length(x))
  return(x)
})

#' @rdname number_line-class
#' @export
unique.number_line <- function(x, ...){
  x <- x[!duplicated(data.frame(x@start, x@.Data))]
  return(x)
}

#' @rdname number_line-class
#' @param fill XXXXX
#' @param simplify XXXXX
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
    # x <- x[1:length(x@start)]
    s <- ifelse(x@.Data > 0 & !is.na(x@.Data) & !is.nan(x@.Data), "->", "??")
    s <- ifelse(x@.Data < 0 & !is.na(x@.Data) & !is.nan(x@.Data), "<-", s)
    s <- ifelse(x@.Data == 0 & !is.na(x@.Data) & !is.nan(x@.Data), "==", s)
    paste(x@start, s, x@start + x@.Data, sep = " ")
  }
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
#' @slot options A list of some option calls in \code{\link{episodes}}
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
  er1 <- suppressWarnings(try(as.integer(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.integer(x) + 0L, silent = TRUE))

  if(!is.integer(er1) | !is.integer(er2)) stop(paste0("`x` can't be coerced to an `epid` object"))
  y <- x
  x <- as.integer(x)
  x[!is.finite(as.integer(x))] <- NA
  x <- methods::new("epid", .Data = x, sn = seq_len(length(x)),
                    wind_id = list(wind_id1 = rep(NA_integer_, length(x))),
                    dist_wind_index = rep(NA_real_, length(x)),
                    dist_epid_index = rep(NA_real_, length(x)),
                    case_nm = rep(NA_integer_, length(x)),
                    wind_nm = rep(NA_integer_, length(x)),
                    epid_interval = as.number_line(rep(NA_real_, length(x))),
                    epid_total = rep(NA_integer_, length(x)),
                    epid_dataset = rep(NA_integer_, length(x)),
                    iteration = rep(NA_integer_, length(x))
                    )

  if(class(y) == "number_line"){
    x@epid_interval <- y
    x@sn <- y@id
    x@.Data <- y@gid
  }
  return(x)
}

#' @rdname epid-class
#' @export
format.epid <- function(x, ...){
  if (length(x) == 0) {
    return("epid(0)")
  }else {
      return(paste0("E.",
                    formatC(x@.Data, width = nchar(max(x@.Data)), flag = 0, format = "fg"),
                    ifelse(is.na(x@epid_interval),
                           "",
                           paste0(" ", format.number_line(x@epid_interval))),
                    " (", c("S", "C", "R", "D", "D")[x@case_nm + 2L], ")"))}
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
  x <- decode(x@wind_nm[!duplicated(x@.Data)])
  x[x == "Case"] <- "Fixed"
  x[x == "Recurrence"] <- "Rolling"
  summ$episode_type <- dst_tab(x = x, order_by_label = c("Fixed", "Rolling", "Skipped"))
  x <- object[order(object@wind_id$wind_id1)]
  x <- x[x@wind_nm == 1 & !duplicated(x@wind_id$wind_id1)]
  x <- rle(x@.Data)
  summ$recurrence <- dst_tab(x = paste0(x$lengths[order(x$lengths)]))
  if(length(summ$recurrence$values) > 0){
    summ$recurrence$values <- paste0(summ$recurrence$values, " times")
  }
  summ$case_nm <- dst_tab(x = decode(object@case_nm[order(object@case_nm)]), order_by_label = c("Case", "Duplicate_C", "Recurrent", "Duplicate_R", "Skipped"))
  x <- object@epid_total[object@case_nm == 0]
  summ$epid_total <- dst_tab(x[order(x)])
  if(length(summ$epid_total$values) > 0){
    summ$epid_total$values <- paste0(summ$epid_total$values, " records")
  }
  summ$epid_length <- if(is.null(object@epid_length)) list(values = numeric(), length = numeric()) else dst_tab(x = format((object@epid_length[object@case_nm == 0])[order(object@epid_length[object@case_nm == 0])]))
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
    nchar(x[[l]]$values)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    ds_len <- nchar(x[[l]]$values)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(x[[l]]$values) > 20,
           paste0(substr(x[[l]]$values, 1, 20), "~"),
           x[[l]]$values)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     \"", ds_txt, "\":", pd_txt,
                       fmt(x[[l]]$lengths), collapse = "\n")
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
                  wind_nm = decode(x@wind_nm),
                  case_nm = decode(x@case_nm),
                  dist_wind_index = x@dist_wind_index,
                  dist_epid_index = x@dist_epid_index,
                  epid_total = x@epid_total,
                  iteration = x@iteration,
                  ...)
  y <- cbind(y, as.data.frame(x@wind_id, ...))
  if(length(x@epid_interval) != 0){
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
    y$epid_dataset = decode(x@epid_dataset)
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
  if(length(x@epid_interval) != 0){
    y$epid_start <- x@epid_interval@start
    y$epid_end <- right_point(x@epid_interval@epid_end)
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
  to_s4(do.call("rbind", lapply(list(x, ...), to_df)))
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
#' @slot window_list A list of \code{windows} considered for each \code{pane}.
#' @slot dist_pane_index The difference between each event and it's index event.
#' @slot pane_dataset Data sources in each \code{pane}.
#' @slot pane_interval The start and end dates of each \code{pane}. A \code{\link{number_line}} object.
#' @slot pane_length The duration or length of (\code{pane_interval}).
#' @slot pane_total The number of records in each \code{pane}.
#' @slot options XXXXXX.
#' @slot window_matched XXXX.
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
  er1 <- suppressWarnings(try(as.integer(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.integer(x) + 0L, silent = TRUE))

  if(!is.integer(er1) | !is.integer(er2)) stop(paste0("`x` can't be coerced to an `pane` object"))
  y <- x
  x <- as.integer(x)
  x[!is.finite(as.integer(x))] <- NA
  x <- methods::new("pane",
                    .Data = x,
                    sn = seq_len(length(x)),
                    window_matched = rep(NA_integer_, length(x)),
                    dist_pane_index = rep(NA_real_, length(x)),
                    case_nm = rep(NA_integer_, length(x)),
                    pane_interval = as.number_line(rep(NA_integer_, length(x))),
                    pane_total = rep(NA_integer_, length(x)),
                    pane_dataset = rep(NA_integer_, length(x)))

  if(class(y) == "number_line"){
    x@pane_interval <- y
    x@sn <- y@id
    x@.Data <- y@gid
  }
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
  if(length(summ$epid_total$values) > 0){
    summ$pane_total$values <- paste0(summ$pane_total$values, " records")
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
    nchar(x[[l]]$values)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    ds_len <- nchar(x[[l]]$values)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(x[[l]]$values) > 20,
                     paste0(substr(x[[l]]$values, 1, 20), "~"),
                     x[[l]]$values)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     \"", ds_txt, "\":", pd_txt,
                       fmt(x[[l]]$lengths), collapse = "\n")
    }else{
      ds_txt <- "     N/A"
    }
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
                  case_nm = decode(x@case_nm),
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
  window_list <- lapply(x@window_list[!duplicated(x@window_list)], function(x) listr(format(x), conj = ","))
  y$window_list <- as.character(window_list[match(names(x@window_list), names(window_list))])
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
  to_s4(do.call("rbind", lapply(list(x, ...), to_df)))
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
  er1 <- suppressWarnings(try(as.integer(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.integer(x) + 0L, silent = TRUE))

  if(!is.integer(er1) | !is.integer(er2)) stop(paste0("`x` can't be coerced to a `pid` object"))

  x[!is.finite(as.integer(x))] <- NA
  x <- methods::new("pid",
                    .Data = as.integer(x),
                    sn = seq_len(length(x)),
                    pid_cri = rep(NA_integer_, length(x)),
                    link_id = rep(NA_integer_, length(x)),
                    pid_total = rep(NA_integer_, length(x)),
                    pid_dataset = rep(NA_integer_, length(x)),
                    iteration = rep(NA_integer_, length(x)))
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
  if(length(summ$epid_total$values) > 0){
    summ$pid_total$values <- paste0(summ$pid_total$values, " records")
  }
  x <- object@pid_cri
  l <- x[!duplicated(x)]
  l <- l[!l %in% -1:0]
  l <- c(sort(l), 0, -1)
  summ$pid_cri <- dst_tab(object@pid_cri[order(object@pid_cri)], order_by_label = l)
  rm(x, l)
  summ$pid_cri$values[summ$pid_cri$values == 0] <- "Skipped"
  summ$pid_cri$values[summ$pid_cri$values == "-1"] <- "No hits"
  summ$pid_cri$values[!summ$pid_cri$values %in% c("Skipped", "No hits")] <- paste0("Criteria ", summ$pid_cri$values[!summ$pid_cri$values %in% c("Skipped", "No hits")])
  summ$data_source <- if(is.null(object@pid_dataset)) list(values = numeric(), length = numeric()) else dst_tab(x = decode((object@pid_dataset[!duplicated(object@.Data)])[order(object@pid_dataset[!duplicated(object@.Data)])]), order_by_label = sort(attr(object@pid_dataset, "label")))
  class(summ) <- "pid_summary"
  return(summ)
}

#' @rdname pid-class
#' @export
print.pid_summary <- function(x, ...){
  dsts <- c("pid_cri", "data_source", "pid_total")
  mx_ds_len <- lapply(dsts, function(l){
    nchar(x[[l]]$values)
  })
  mx_ds_len <- unlist(mx_ds_len, use.names = FALSE)
  mx_ds_len <- max(mx_ds_len)
  mx_ds_len <- max(if(length(mx_ds_len) == 0) 0 else mx_ds_len)
  mx_pd_len <- ifelse(mx_ds_len > 20, 1, 20 - mx_ds_len)

  ds_txts <- lapply(dsts, function(l){
    ds_len <- nchar(x[[l]]$values)
    pd_len <- ifelse(ds_len > 20, 1, 20 - ds_len)
    pd_txt <- unlist(lapply(pd_len, function(j) paste0(rep(" ", j), collapse = "")), use.names = FALSE)
    ds_txt <- ifelse(nchar(x[[l]]$values) > 20,
                     paste0(substr(x[[l]]$values, 1, 20), "~"),
                     x[[l]]$values)

    if(length(ds_len) > 0){
      ds_txt <- paste0("     \"", ds_txt, "\":", pd_txt,
                       fmt(x[[l]]$lengths), collapse = "\n")
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
    y$pid_dataset <- decode(x@pid_dataset)
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
    y$pid_dataset <- attr(x@pid_dataset, "label")[match(x@pid_dataset, attr(x@pid_dataset, "value"))]
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
  to_s4(do.call("rbind", lapply(list(x, ...), to_df)))
})
