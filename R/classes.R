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
                        id = "numeric",
                        gid = "numeric"))

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
  to_s4(do.call("rbind", lapply(list(x, ...), function(y) to_df(as.number_line(y)))))
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
  db <- data.frame(sd = as.numeric(diyar::start_point(x)),
                   id = x@id,
                   nl= x)
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
    x <- x[1:length(x@start)]
    s <- ifelse(x@.Data > 0 & !is.na(x@.Data) & !is.nan(x@.Data), "->", "??")
    s <- ifelse(x@.Data < 0 & !is.na(x@.Data) & !is.nan(x@.Data), "<-", s)
    s <- ifelse(x@.Data== 0 & !is.na(x@.Data) & !is.nan(x@.Data), "==", s)
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
         contains = "numeric",
         representation(sn = "numeric",
                        wind_id = "numeric",
                        wind_nm = "character",
                        case_nm = "character",
                        dist_wind_index = "ANY",
                        dist_epid_index = "ANY",
                        epid_interval = "number_line",
                        epid_length = "ANY",
                        epid_total = "numeric",
                        epid_dataset = "character",
                        iteration = "numeric",
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
  er1 <- suppressWarnings(try(as.numeric(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(x) + 0, silent = TRUE))

  if(!is.numeric(er1) | !is.numeric(er2)) stop(paste0("`x` can't be coerced to an `epid` object"))
  y <- x
  x <- as.numeric(x)
  x[!is.finite(as.numeric(x))] <- NA
  x <- methods::new("epid", .Data = x, sn = 1:length(x), wind_id = rep(NA_real_, length(x)),
                    dist_wind_index = rep(NA_real_, length(x)),
                    dist_epid_index = rep(NA_real_, length(x)),
                    case_nm = rep(NA_character_, length(x)),
                    wind_nm = rep(NA_character_, length(x)),
                    epid_interval = as.number_line(rep(NA_real_, length(x))),
                    epid_total = rep(NA_real_, length(x)),
                    epid_dataset = rep(NA_character_, length(x)),
                    iteration = rep(NA_real_, length(x))
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
                    " (", substr(x@case_nm, 1, 1), ")"))}
}

#' @rdname epid-class
#' @export
unique.epid <- function(x, ...){
  return(x[x@case_nm == "Case"])
}

#' @rdname epid-class
#' @export
summary.epid <- function(object, ...){
  summ <- paste0("Iterations:        ", fmt(max(object@iteration)), "\n",
                 "Records:\n",
                 "  Total:           ", fmt(length(object)), "\n",
                 "    Skipped:       ", fmt(length(object[object@case_nm == "Skipped"])), "\n",
                 "Episodes:\n",
                 "  Total:           ", fmt(length(object[object@case_nm == "Case"])), "\n",
                 "    Single record: ", fmt(length(object[object@case_nm == "Case" & object@epid_total == 1])), "\n")
  cat(summ)
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
               wind_id = rep(x@wind_id, ...),
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
                         wind_id = x@wind_id[i],
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
                         wind_id = x@wind_id[i],
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
         contains = "numeric",
         representation(sn = "numeric",
                        case_nm = "character",
                        dist_pane_index = "ANY",
                        window_list = "list", window_matched = "numeric",
                        pane_interval = "number_line",
                        pane_length= "ANY", pane_total = "numeric",
                        pane_dataset = "character",
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
  er1 <- suppressWarnings(try(as.numeric(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(x) + 0, silent = TRUE))

  if(!is.numeric(er1) | !is.numeric(er2)) stop(paste0("`x` can't be coerced to an `pane` object"))
  y <- x
  x <- as.numeric(x)
  x[!is.finite(as.numeric(x))] <- NA
  x <- methods::new("pane",
                    .Data = x,
                    sn = 1:length(x),
                    window_matched = rep(NA_real_, length(x)),
                    dist_pane_index = rep(NA_real_, length(x)),
                    case_nm = rep(NA_character_, length(x)),
                    pane_interval = as.number_line(rep(NA_real_, length(x))),
                    pane_total = rep(NA_real_, length(x)),
                    pane_dataset = rep(NA_character_, length(x)))

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
                  " (", substr(x@case_nm, 1, 1), ")"))
    }
}

#' @rdname pane-class
#' @export
unique.pane <- function(x, ...){
  x <- x[x@case_nm == "Case"]
  return(x)
}

#' @rdname pane-class
#' @export
summary.pane <- function(object, ...){
  summ <- paste0("Records:\n",
                 "  Total:           ", fmt(length(object)), "\n",
                 "    Skipped:       ", fmt(length(object[object@case_nm == "Skipped"])), "\n",
                 "Panes:\n",
                 "  Total:           ", fmt(length(object[object@case_nm == "Index"])), "\n",
                 "    Single record: ", fmt(length(object[object@case_nm == "Index" & object@pane_total == 1])), "\n")
  cat(summ)
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
         contains = "numeric",
         representation(sn = "numeric",
                        pid_cri= "numeric",
                        link_id = "numeric",
                        pid_dataset = "character",
                        pid_total = "numeric",
                        iteration = "numeric"))

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
  er1 <- suppressWarnings(try(as.numeric(x), silent = TRUE))
  er2 <- suppressWarnings(try(as.numeric(x) + 0, silent = TRUE))

  if(!is.numeric(er1) | !is.numeric(er2)) stop(paste0("`x` can't be coerced to a `pid` object"))

  x[!is.finite(as.numeric(x))] <- NA
  x <- methods::new("pid",
                    .Data = as.numeric(x),
                    sn = 1:length(x),
                    pid_cri = rep(NA_real_, length(x)),
                    link_id = rep(NA_real_, length(x)),
                    pid_total = rep(NA_real_, length(x)),
                    pid_dataset = rep(NA_character_, length(x)),
                    iteration = rep(NA_real_, length(x)))
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
  cri_dst <- table(object@pid_cri)
  cri_n <- as.numeric(names(cri_dst))
  cri_dst <- c(cri_dst[cri_n > 0], cri_dst[cri_n == 0], cri_dst[cri_n == -1])
  cri_dst <- cri_dst[!is.na(cri_dst)]
  cri_n <- as.numeric(names(cri_dst))
  cri_dst <- paste0("       ", pid_cri_l(cri_n), ":    ", fmt(cri_dst), collapse = "\n")
  summ <- paste0("Iterations:        ", fmt(max(object@iteration)), "\n",
                 "Records:\n",
                 "  Total:           ", fmt(length(object)), "\n",
                 "    Stages:\n",
                 cri_dst, "\n",
                 "Groups:\n",
                 "   Total:          ", fmt(length(object@.Data[!duplicated(object@.Data)])), "\n",
                 "    Single record: ", fmt(length(object@.Data[!duplicated(object@.Data) & object@pid_total == 1])), "\n"
  )
  cat(summ)
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


