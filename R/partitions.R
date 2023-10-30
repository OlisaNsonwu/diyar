#' @name partitions
#' @title Distribute events into specified intervals.
#'
#' @description Distribute events into groups defined by time or numerical intervals.
#' Each set of linked records are assigned a unique identifier with relevant group-level data.
#'
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=pane-class]{pane}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Panes are created separately for each \code{strata}.
#' @param windows_total \code{[integer|\link{number_line}]}. Minimum number of matched \code{windows} required for a pane. See \code{details}
#' @param separate \code{[logical]}. If \code{TRUE}, events matched to different \code{windows} are not linked.
#' @param date \code{[date|datetime|integer|\link{number_line}]}. Event date or period.
#' @param window \code{[integer|\link{number_line}]}. Numeric or time intervals.
#' @param data_source \code{[character]}. Unique data source identifier. Adds the list of datasets in each pane to the \code{\link[=pane-class]{pane}}. Useful when the data is from multiple sources.
#' @param custom_sort \code{[atomic]}. Preferred order for selecting \code{"index"} events.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), the returned \code{pane} object will include group specific information like panes start and end dates.
#' @param data_links \code{[list|character]}. A set of \code{data_sources} required in each \code{\link[=pane-class]{pane}}. A \code{\link[=pane-class]{pane}} without records from these \code{data_sources} will be unlinked. See \code{Details}.
#' @param by \code{[integer]}. Width of splits.
#' @param length.out \code{[integer]}. Number of splits.
#' @param fill \code{[logical]}. Retain (\code{TRUE}) or drop (\code{FALSE}) the remainder of an uneven split.
#' @param display \code{[character]}. Display a status update. Options are; \code{"none"} (default), \code{"progress"} or \code{"stats"}.
#' @return \code{\link[=pane-class]{pane}}
#'
#' @seealso
#' \code{\link[=pane-class]{pane}}; \code{\link{number_line_sequence}}; \code{\link{episodes}}; \code{\link{links}}; \code{\link{overlaps}}; \code{\link{number_line}}; \code{\link{schema}}
#'
#' @details
#' Each assigned group is referred to as a \code{\link[=pane-class]{pane}} A \code{\link[=pane-class]{pane}} consists of events within a specific time or numerical intervals (\code{window}).
#'
#' Each \code{window} must cover a separate interval. Overlapping \code{windows} are merged before events are distributed into panes.
#' Events that occur over two \code{windows} are assigned to the last one listed.
#'
#' Alternatively, you can create \code{windows} by splitting a period into equal parts (\code{length.out}), or into a sequence of intervals with fixed widths (\code{by}).
#'
#' By default, the earliest event is taken as the \code{"Index"} event of the \code{\link[=pane-class]{pane}}.
#' An alternative can be chosen with \code{custom_sort}.
#' Note that this is simply a convenience option because it has no bearing on how groups are assigned.
#'
#' \bold{\code{partitions()}} will categorise records into 3 types;
#' \itemize{
#' \item \code{"Index"} - Index event/record of the pane.
#' \item \code{"Duplicate_I"} - Duplicate of the \code{"Index"} record.
#' \item \code{"Skipped"} - Records that are not assigned to a pane.
#' }
#'
#' Every element in \code{data_links} must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements of \code{data_links} will be assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, only groups with records from every listed \code{data_source} will be retained.
#' \item If named \code{"g"}, only groups with records from any listed \code{data_source} will be retained.
#' }
#'
#' \emph{\code{NA} values in \code{strata} excludes records from the partitioning process}.
#'
#' See \code{vignette("episodes")} for more information.
#'
#' @examples
#' events <- c(30, 2, 11, 10, 100)
#' windows <- number_line(c(1, 9, 25), c(3, 12, 35))
#'
#' events
#' partitions(date = events, length.out = 3, separate = TRUE)
#' partitions(date = events, by = 10, separate = TRUE)
#' partitions(date = events, window = windows, separate = TRUE)
#' partitions(date = events, window = windows, separate = FALSE)
#' partitions(date = events, window = windows, separate = FALSE, windows_total = 4)
#'
#' @aliases partitions
#' @export
#'
partitions <- function(date, window = NULL, windows_total = 1, separate = FALSE, sn = NULL, strata = NULL,
                       data_links = "ANY", custom_sort = NULL, group_stats = FALSE,
                       data_source = NULL, by = NULL, length.out = NULL, fill =  TRUE, display = "none", precision = 1){
  tm_a <- Sys.time()

  # Validations
  errs <- err_partitions_checks_0(
    date = date,
    window = window,
    windows_total = windows_total,
    separate = separate,
    sn = sn,
    strata = strata,
    data_source = data_source,
    data_links = data_links,
    custom_sort = custom_sort,
    group_stats = group_stats,
    by = by,
    length.out = length.out,
    fill = fill,
    display = display
  )

  if(errs != FALSE) stop(errs, call. = FALSE)

  options_lst = list(date = as.number_line(date),
                     strata = strata,
                     separate = separate)

  dl_lst <- unlist(data_links, use.names = FALSE)
  # Standardise inputs
  # `data_links`
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))){
    names(data_links) <- rep("l", length(data_links))
    }
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))
  # `date`
  int <- as.number_line(date)
  int@id <- seq_len(length(int))
  # `strata`
  if(length(strata) == 1) {
    cri <- rep(1L, length(int))
    cri_l <- rep(strata, length(int))
  }else if(is.null(strata)){
    cri <- rep(1, length(int))
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
    cri_l <- strata
  }
  # `sn`
  if(is.null(sn)) {
    sn <- seq_len(length(int))
  }else{
    sn <- as.integer(sn)
  }
  # `custom_sort`
  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort) == 1) c_sort <- rep(c_sort, length(int))
  }else{
    c_sort <- rep(0L, length(int))
  }
  # `windows_total`
  if(is.number_line(windows_total)){
    windows_total[windows_total@.Data < 0] <-
      reverse_number_line(windows_total[windows_total@.Data < 0], "decreasing")
  }else{
    windows_total <- number_line(windows_total, Inf)
  }
  windows_total[windows_total@.Data < 0] <-
    reverse_number_line(windows_total[windows_total@.Data < 0], "decreasing")

  if(length(windows_total) == 1){
    windows_total <- rep(windows_total, length(int))
  }

  if(inherits(window, "number_line")){
    window <- list(window)
  }

  splits <- split(int, cri)
  if(!is.null(window)){
    if(length(window) == 1){
      splits_windows <- splits
      splits_windows[1:length(splits_windows)] <- window
    }else{
      splits_windows <- lapply(split(window, cri), function(x) x[[1]])
    }
  }else{
    splits_windows <- lapply(splits, function(x){
      x <- x[order(x@start, x@.Data)]
      x <- number_line(x[1]@start, right_point(x[length(x)]))
      if(is.null(by)){
        seq(x, length.out = length.out, precision = precision)
      }else{
        seq(x, by = by, precision = precision)
      }
    })

  }

  rp <- list()
  rp$cri <-
    rep(names(splits), unlist(lapply(splits, length), use.names = FALSE))
  names(splits) <- NULL
  rp$dts <- do.call("c", splits)
  rp$source <- rep(FALSE, length(rp$dts))

  rp$cri <-
    c(
      rp$cri,
      rep(names(splits_windows), unlist(lapply(splits_windows, length), use.names = FALSE))
      )

  split_nms <- names(splits_windows)
  names(splits_windows) <- NULL
  rp$dts <- c(
    rp$dts,
    do.call("c", splits_windows)
  )
  split_nms -> names(splits_windows)

  rp$source <- c(
    rp$source,
    rep(TRUE, length(rp$dts) - length(rp$source))
  )

  rec.pairs <- make_pairs_batched(
    strata = rp$cri,
    x = rp$dts,
    index_record = rp$source,
    data_source = rp$source,
    ignore_same_source = TRUE,
    include_repeat = FALSE,
    look_back = TRUE,
    assign_ord = rep(1, length(rp$cri))
  )
  rec.pairs$w.match <- overlap(
    rec.pairs$x_val,
    rec.pairs$y_val
  )

  repo <- list()
  repo$pane <-
    repo$pr_sn <- 1:length(date)
  repo$tag <- rep(20L, length(date))
  repo$cri <- cri
  repo$case_nm <- rep(-1L, length(date))
  repo$window_matched <- rep(0L, length(date))

  # Partition records into `windows`
  repo$tag[rec.pairs$x_pos[rec.pairs$w.match]] <- 10L
  repo$case_nm[repo$tag == 10] <- 1L
  repo$window_matched[rec.pairs$x_pos[rec.pairs$w.match]] <-
    rec.pairs$index_ord[rec.pairs$w.match]
  if(separate){
    repo$pane[rec.pairs$x_pos[rec.pairs$w.match]] <- rec.pairs$y_pos[rec.pairs$w.match]
  }else{
    repo$pane[rec.pairs$x_pos[rec.pairs$w.match]] <- repo$cri[rec.pairs$y_pos[rec.pairs$w.match]]
  }

  # Implement `windows_total`
  dst <- rle(sort(repo$cri[!duplicated(repo$pane)]))
  phits <- dst$lengths[match(cri, dst$values)]
  lgk <- !(phits >= as.numeric(windows_total@start) &
             phits <= as.numeric(right_point(windows_total))) | is.na(repo$pane)
  repo$pane[lgk] <- repo$pr_sn[lgk]

  # Index records - `custom_sort`
  ord <- order(
    repo$pane, c_sort,
    as.numeric(int@start),
    -as.numeric(right_point(int)))
  s_pane <- repo$pane[ord]
  s_sn <- repo$pr_sn[ord]

  lgk <- !duplicated(s_pane)
  repo$case_nm[s_sn[lgk]] <- 0L

  panes <- make_episodes(
    y_pos = repo$pane,
    date = (
      if(group_stats){
        int
      }else{
        NULL
      }),
    x_pos = repo$pr_sn,
    data_source = data_source,
    data_links = data_links)

  class(repo$case_nm) <- "d_label"
  attr(repo$case_nm, "value") <- c(-1, 0, 1)
  attr(repo$case_nm, "label") <- c("Skipped", "Index", "Duplicate_I")
  attr(repo$case_nm, "state") <- "encoded"

  panes <- new("pane",
               .Data = panes@.Data,
               dist_pane_index = panes@dist_epid_index,
               window_matched = repo$window_matched,
               sn = panes@sn,
               case_nm = repo$case_nm,
               pane_total = panes@epid_total,
               options = options_lst)
  indx <- which(!duplicated(panes@.Data))
  panes@window_list <- splits_windows[match(repo$cri[indx], names(splits_windows))]
  names(panes@window_list) <- panes@.Data[indx]

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  if(tolower(display) != "none"){
    cat("Records partitioned in ", fmt(tms, "difftime"), "!\n", sep = "")
    }
  return(panes)
}
