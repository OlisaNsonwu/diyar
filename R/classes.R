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
  a <- lapply(list(x, ...), function(y) as.number_line(y)@start)
  for(i in 1:length(a)){
    if(i == 1) ai <- a[[i]]
    if(i > 1) ai <- c(ai, a[[i]])
  }

  zi <- unlist(lapply(list(x, ...), function(x) as.number_line(x)@.Data), use.names = F)
  id <- gid <- 1:length(zi)
  methods::new("number_line",
               .Data = zi,
               id = id,
               gid = gid,
               start = ai)
})

#' @rdname number_line-class
#' @export
unique.number_line <- function(x, ...){
  x <- x[!duplicated(data.frame(x@start, x@.Data))]
  return(x)
}

#' @rdname number_line-class
#' @export
seq.number_line <- function(x,
                            precision = FALSE,
                            fill = TRUE,
                            simplify = FALSE,
                            ...){
  x <- number_line_sequence(x,
                            precision = precision,
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
                        iteration = "numeric"))

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
                    iteration = rep(NA_real_, length(x)))

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
summary.epid <- function(x, ...){
  r <- rle(sort(x@.Data))
  epid_tot <- r$lengths[match(x@.Data, r$values)]
  summ <- paste0("Iterations:     ", fmt(max(x@iteration)), "\n",
                 "Records:\n",
                 "  Total:        ", fmt(length(x)), "\n",
                 "  Skipped:      ", fmt(length(x[x@case_nm == "Skipped"])), "\n",
                 "Episodes:\n",
                 "  Total:        ", fmt(length(x[x@case_nm == "Case"])), "\n",
                 "  With one record: ", fmt(length(x[x@case_nm == "Case" & epid_tot == 1])), "\n")
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
                         iteration = x@iteration[i])
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
                         iteration = x@iteration[i])
          })

#' @rdname epid-class
setMethod("c", signature(x = "epid"), function(x,...) {
  e <- lapply(list(x, ...), function(y) y@epid_interval)
  for(i in 1:length(e)){
    if(i == 1) ei <- e[[i]]
    if(i > 1) ei <- c(ei, e[[i]])
  }

  sn <- unlist(lapply(list(x, ...), function(y) y@sn))
  wind_id <- unlist(lapply(list(x, ...), function(y) y@wind_id))
  dist_epid_index <- unlist(lapply(list(x, ...), function(y) y@dist_epid_index))
  dist_wind_index <- unlist(lapply(list(x, ...), function(y) y@dist_wind_index))
  wind_nm <- unlist(lapply(list(x, ...), function(y) y@wind_nm))
  case_nm <- unlist(lapply(list(x, ...), function(y) y@case_nm))
  epid_length <- unlist(lapply(list(x, ...), function(y) y@epid_length))
  epid_total <- unlist(lapply(list(x, ...), function(y) y@epid_total))
  epid_dataset <- unlist(lapply(list(x, ...), function(y) y@epid_dataset))
  iteration <- unlist(lapply(list(x, ...), function(y) y@iteration))
  zi <- unlist(list(x, ...))

  methods::new("epid",
               zi,
               case_nm = case_nm,
               sn = sn,
               wind_id = wind_id,
               wind_nm = wind_nm,
               epid_length = epid_length,
               epid_total = epid_total,
               epid_dataset = epid_dataset,
               epid_interval = ei,
               dist_epid_index = dist_epid_index,
               dist_wind_index = dist_wind_index,
               iteration = iteration)

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
                        pane_dataset = "character"))

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
                    window_list = rep(NA_character_, length(x)),
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
  x <- x[!x@case_nm %in% c("Duplicate_C", "Duplicate_R")]
  return(x)
}

#' @rdname pane-class
#' @export
summary.pane <- function(x, ...){
  r <- rle(sort(x@.Data))
  pane_tot <- r$lengths[match(x@.Data, r$values)]
  summ <- paste0("Records:\n",
                 "  Total:        ", fmt(length(x)), "\n",
                 "  Skipped:      ", fmt(length(x[x@case_nm == "Skipped"])), "\n",
                 "Panes:\n",
                 "  Total:        ", fmt(length(x[x@case_nm == "Index"])), "\n",
                 "  With one record: ", fmt(length(x[x@case_nm == "Index" & pane_tot == 1])), "\n")
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
               window_list = rep(x@window_list, ...),
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
                         window_list = x@window_list[i],
                         sn = x@sn[i],
                         window_matched = x@window_matched[i],
                         dist_pane_index = x@dist_pane_index[i],
                         pane_length = x@pane_length[i],
                         pane_total = x@pane_total[i],
                         pane_dataset = x@pane_dataset[i],
                         pane_interval = x@pane_interval[i])
          })

#' @aliases [[,pane-method
#' @rdname pane-class
#' @param exact exact
setMethod("[[", signature(x = "pane"),
          function(x, i, j, ..., exact = TRUE) {
            methods::new("pane",
                         x@.Data[i],
                         case_nm = x@case_nm[i],
                         window_list = x@window_list[i],
                         sn = x@sn[i],
                         window_matched = x@window_matched[i],
                         dist_pane_index = x@dist_pane_index[i],
                         pane_length = x@pane_length[i],
                         pane_total = x@pane_total[i],
                         pane_dataset = x@pane_dataset[i],
                         pane_interval = x@pane_interval[i])
          })

#' @rdname pane-class
setMethod("c", signature(x = "pane"), function(x,...) {
  e <- lapply(list(x, ...), function(y) y@pane_interval)
  for(i in 1:length(e)){
    if(i == 1) ei <- e[[i]]
    if(i > 1) ei <- c(ei, e[[i]])
  }

  sn <- unlist(lapply(list(x, ...), function(y) y@sn))
  window_matched <- unlist(lapply(list(x, ...), function(y) y@window_matched))
  dist_pane_index <- unlist(lapply(list(x, ...), function(y) y@dist_pane_index))
  case_nm <- unlist(lapply(list(x, ...), function(y) y@case_nm))
  window_list <- unlist(lapply(list(x, ...), function(y) y@window_list))
  pane_length <- unlist(lapply(list(x, ...), function(y) y@pane_length))
  pane_total <- unlist(lapply(list(x, ...), function(y) y@pane_total))
  pane_dataset <- unlist(lapply(list(x, ...), function(y) y@pane_dataset))
  zi <- unlist(list(x, ...))

  methods::new("pane",
               zi,
               case_nm = case_nm,
               window_list = window_list,
               sn = sn,
               window_matched = window_matched,
               pane_length = pane_length,
               pane_total = pane_total,
               pane_dataset = pane_dataset,
               pane_interval = ei,
               dist_pane_index = dist_pane_index)

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
summary.pid <- function(x, ...){
  r <- rle(sort(x@.Data))
  pid_tot <- r$lengths[match(x@.Data, r$values)]
  cri_dst <- table(x@pid_cri)
  cri_n <- as.numeric(names(cri_dst))
  cri_dst <- c(cri_dst[cri_n > 0], cri_dst[cri_n == 0], cri_dst[cri_n == -1])
  cri_dst <- cri_dst[!is.na(cri_dst)]
  cri_n <- as.numeric(names(cri_dst))
  cri_dst <- paste0("       ", pid_cri_l(cri_n), ":   ", fmt(cri_dst), collapse = "\n")
  summ <- paste0("Iterations:       ", fmt(max(x@iteration)), "\n",
                 "Records:\n",
                 "  Total:          ", fmt(length(x)), "\n",
                 "    Stages:\n",
                 cri_dst, "\n",
                 "Groups:\n",
                 "   Total:         ", fmt(length(x@.Data[!duplicated(x@.Data)])), "\n",
                 "   With one record: ", fmt(length(x@.Data[!duplicated(x@.Data) & pid_tot == 1])), "\n"
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
  sn <- unlist(lapply(list(x, ...), function(y) y@sn))
  pid_cri <- unlist(lapply(list(x, ...), function(y) y@pid_cri))
  link_id <- unlist(lapply(list(x, ...), function(y) y@link_id))
  pid_total <- unlist(lapply(list(x, ...), function(y) y@pid_total))
  pid_dataset <- unlist(lapply(list(x, ...), function(y) y@pid_dataset))
  iteration <- unlist(lapply(list(x, ...), function(y) y@iteration))
  zi <- unlist(list(x, ...))

  methods::new("pid",
               zi,
               pid_cri = pid_cri,
               sn = sn,
               pid_total = pid_total,
               pid_dataset = pid_dataset,
               link_id = link_id,
               iteration = iteration)
})


