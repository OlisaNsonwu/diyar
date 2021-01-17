#' @name partitions
#' @title Distribute events into time intervals.
#'
#' @description Distribute events into groups defined by time or numerical boundaries.
#' Records in each group are assigned a unique identifier with relevant group-level data.
#'
#' @param sn Unique numerical record identifier. Useful for creating familiar pane identifiers.
#' @param strata Subsets of the dataset. Panes are created separately for each \code{strata}. Assigning \code{NA} to \code{strata} will skip that record.
#' @param windows_total Minimum number of matched \code{windows} required for pane. See \code{details}
#' @param separate If \code{TRUE}, events matched to different \code{windows} are not linked.
#' @param date Event date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}).
#' @param window Numeric or time intervals. Supplied as \code{number_line} objects.
#' @param data_source Unique data source identifier. Includes a list of data sources of for each record in a \code{panes}.
#' @param custom_sort Preferred order for selecting \code{"index"} events.
#' @param group_stats If \code{TRUE} (default), the returned \code{pane} object will include pane-specific information like panes' start and endpoints.
#' @param data_links A set of \code{data_sources} required in a \code{pane}. A \code{pane} without records from these \code{data_sources} are skipped or unlinked. See \code{Details}.
#' @param schema Return a schema of the \code{pane} object. Options are; \code{"none"} (default), \code{"by_pane"}, \code{"by_strata"} or \code{"by_ALL"}.
#' @param group_stats If \code{TRUE} (default), returns group-specific information like record counts. See \code{Value}.
#' @return
#'
#' @return \code{\link[=pane-class]{pane}} or \code{list} (\code{\link[=pane-class]{pane}} and \code{ggplot}) object
#'
#' @seealso
#' \code{\link[=pane-class]{pane}}, \code{\link{number_line_sequence}}, \code{\link{episodes}}, \code{\link{links}}, \code{\link{overlaps}} and \code{\link{number_line}}
#'
#' @details
#' Each group is referred to as a pane. A pane consists of events within a specific time or numerical intervals (\code{window}).
#'
#' Each \code{window} must cover a separate interval. Overlapping \code{windows} are merged before events are distributed into panes.
#' Events that occur over two \code{windows} are assigned to the last one listed.
#'
#' Alternatively, you can create \code{windows} by splitting a period into equal parts (\code{length.out}), or into a sequence of intervals with fixed widths (\code{by}).
#'
#' By default, the earliest event is taken as the \code{"Index"} event of the pane.
#' An alternative can be chosen with \code{custom_sort}.
#'
#' \bold{\code{partitions()}} will categorise records into 3 types;
#' \itemize{
#' \item \code{"Index"} - Index event/record of the pane.
#' \item \code{"Duplicate_I"} - Duplicate of the \code{"Index"} record.
#' \item \code{"Skipped"} - Records that are not assigned to a pane.
#' }
#'
#' \code{data_links} must be a \code{list} of \code{atomic} vectors, with every element named \code{"l"} (links) or \code{"g"} (groups).
#'
#' \itemize{
#' \item if named \code{"l"}, only \code{panes} with records from every listed \code{data_source} will be retained.
#' \item if named \code{"g"}, only \code{panes} with records from any listed \code{data_source} will be retained.
#' }
#'
#' See \code{vignette("partitions")} for more information.
#'
#' @examples
#' events <- c(30, 2, 11, 10, 100)
#' windows <- number_line(c(1, 9, 25), c(3, 12, 35))
#'
#' events
#' partitions(date = events, length.out = 3, separate = TRUE, schema = "by_ALL")
#' partitions(date = events, by = 10, separate = TRUE, schema = "by_ALL")
#' partitions(date = events, window = windows, separate = TRUE, schema = "by_ALL")
#' partitions(date = events, window = windows, separate = FALSE, schema = "by_ALL")
#' partitions(date = events, window = windows, separate = FALSE, schema = "by_ALL", windows_total = 4)
#'
#' @aliases partitions
#' @export
#'
partitions <- function(date, window = number_line(0, Inf), windows_total = 1, separate = FALSE, sn = NULL, strata = NULL,
                  data_links = "ANY", custom_sort = NULL, group_stats = FALSE,
                  data_source = NULL, by = NULL, length.out = NULL, fill =  TRUE,
                  schema = "none", ...){
  tm_a <- Sys.time()

  # Validations
  errs <- err_partitions_checks_0(date = date,
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
                             fill = fill
                             )

  if(errs != FALSE) stop(errs, call. = FALSE)
  dl_lst <- unlist(data_links, use.names = FALSE)
  # Standardise inputs
    # `data_links`
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links)=="", "l", names(data_links))
  # `date`
  int <- as.number_line(date)
  int@id <- seq_len(length(int))
  # `strata`
  if(length(strata) == 1) {
    cri <- rep(1, length(int))
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
  }
  # `custom_sort`
  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort) == 1) c_sort <- rep(c_sort, length(int))
  }else{
    c_sort <- rep(0, length(int))
  }
  # `windows_total`
  if(is.number_line(windows_total)){
    windows_total[windows_total@.Data < 0] <- reverse_number_line(windows_total[windows_total@.Data < 0], "decreasing")
  }else{
    windows_total <- number_line(windows_total, Inf)
  }
  if(length(windows_total) == 1){
    windows_total <- rep(windows_total, length(int))
  }
  # Strata-specific `windows`
  if(is.number_line(window)){
    window <- list(compress_number_line(as.number_line(window), deduplicate = TRUE, collapse = TRUE))
  }
  if(!is.null(by) | !is.null(length.out) | length(window) > 1){
    split_cri <- cri
  }else{
    split_cri <- rep(1, length(int))
  }
  splits <- split(int, split_cri)
  splits_sn <- split(int@id, split_cri)
  if(!is.null(by)){
    if (length(by) == 1) by <- rep(by, length(int))
    split_bys <- split(by, split_cri)
    if (length(fill) == 1) fill <- rep(fill, length(int))
    split_fills <- split(fill, split_cri)
    splits_func <- function(x, n, f) {
      n <- n[!duplicated(n)]
      f <- f[!duplicated(f)]
      number_line_sequence(number_line(min(x@start), max(x@start + x@.Data)), by = n, fill = f)
    }
    splits_windows <- mapply(splits_func, splits, split_bys, split_fills, SIMPLIFY = FALSE)
  }else if(is.null(by) & !is.null(length.out)){
    if (length(length.out) == 1) length.out <- rep(length.out, length(int))
    split_lnts <- split(length.out, split_cri)
    if (length(fill) == 1) fill <- rep(fill, length(int))
    split_fills <- split(fill, split_cri)
    splits_func <- function(x, n, f) {
      n <- n[!duplicated(n)]
      f <- f[!duplicated(f)]
      number_line_sequence(number_line(min(x@start), max(x@start + x@.Data)), length.out = n, fill = f)
    }
    splits_windows <- mapply(splits_func, splits, split_lnts, split_fills, SIMPLIFY = FALSE)
  }else{
    if(length(window) == 1){
      splits_windows <- window
      names(splits_windows) <- "1"
    }else{
      splits_windows <- split(window, split_cri)
      splits_windows <- lapply(splits_windows, function(x) x[!duplicated(x)])
      splits_windows <- unlist(splits_windows, recursive = FALSE)
      splits_windows <- lapply(splits_windows, function(x) compress_number_line(x, deduplicate = TRUE, collapse = TRUE))
    }
  }

  nms <- names(splits_windows)
  window_list <- splits_windows[match(split_cri, nms)]

  # Partition records into `windows`
  window_matched <- mapply(pane_checks, splits, splits_windows, SIMPLIFY = FALSE)
  window_matched <- unlist(window_matched, use.names = FALSE)
  splits_sn <- unlist(splits_sn, use.names = FALSE)
  window_matched <- window_matched[match(seq_len(length(int)), splits_sn)]

  case_nm <- ifelse(window_matched == 0, "Skipped", "Duplicate_I")
  tag <- ifelse(window_matched == 0,
                NA_real_, cri + exp(-window_matched))
  pane <- ifelse(!is.na(tag) & rep(!separate, length(int)),
                 cri + exp(-1),
                 tag)
  # Implement `windows_total`
  dst <- rle(sort(cri[!is.na(tag) & !duplicated(tag)]))
  phits <- dst$lengths[match(cri, dst$values)]
  lgk <- !(phits >= as.numeric(windows_total@start) & phits <= as.numeric(right_point(windows_total))) | is.na(pane)
  pane[lgk] <- seq_len(length(int))[lgk]

  # Index records - `custom_sort`
  ord <- order(pane, -c_sort, -as.numeric(int@start), -as.numeric(right_point(int)))
  s_pane <- pane[ord]
  s_sn <- sn[ord]
  names(s_pane) <- s_sn
  index_sn <- rle(s_pane)

  pp <- as.numeric(names(index_sn$values))
  qq <- as.numeric(names(s_pane))
  pane <- rep(s_sn[match(pp, qq)], index_sn$lengths)
  pane <- pane[match(sn, s_sn)]
  case_nm[which(sn %in% s_sn[match(pp, qq)] & case_nm != "Skipped")] <- "Index"

  # `pane_total`
  pane_n <- rep(index_sn$lengths, index_sn$lengths)
  pane_n <- pane_n[match(sn, s_sn)]

  # Distance of records from index record
  ei <- pane[which(case_nm %in% c("Index", "Skipped"))]
  ii <- int[which(case_nm %in% c("Index", "Skipped"))]
  lgk <- match(pane, ei)
  dist_pane_index <- ((as.numeric(int@start) + as.numeric(right_point(int))) * .5) -
    ((as.numeric(ii@start[lgk]) + as.numeric(right_point(ii[lgk]))) * .5)

  # output - `pane` object
  panes <- new("pane",
               .Data= pane,
               dist_pane_index = dist_pane_index,
               window_matched = window_matched,
               sn = sn,
               case_nm = case_nm,
               window_list = window_list,
               pane_total = pane_n)

  if(!is.null(data_source)){
    # Implement `data_links`
    rst <- check_links(panes, data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      panes@dist_pane_index[!req_links] <- 0
      panes@case_nm[!req_links] <- "Skipped"
      panes@.Data[!req_links] <- panes@sn[!req_links]
      # `pane_datasets`
      datasets[!req_links] <- data_source[!req_links]
    }
    panes@pane_dataset <- datasets
  }

  # `group_stats`
  if(group_stats == TRUE){
    lgk <- which(pane_n != 1)
    # `pane_interval`
    dts_a <- lapply(split(as.numeric(int@start[lgk]), pane[lgk]), min)
    dts_z <- lapply(split(as.numeric(right_point(int[lgk])), pane[lgk]), max)
    dts_a <- as.numeric(dts_a)[match(pane[lgk], names(dts_a))]
    dts_z <- as.numeric(dts_z)[match(pane[lgk], names(dts_z))]

    pane_dt_a <- as.numeric(int@start)
    pane_dt_z <- right_point(int)

    pane_dt_a[lgk] <- dts_a
    pane_dt_z[lgk] <- dts_z
    is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
    if(is_dt == TRUE){
      pane_dt_a <- as.POSIXct(pane_dt_a, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      pane_dt_z <- as.POSIXct(pane_dt_z, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      pane_l <- difftime(pane_dt_z, pane_dt_a, units = "days")
    }else{
      pane_l <- pane_dt_z - pane_dt_a
    }
    panes@pane_interval <- number_line(l = pane_dt_a,
                                      r = pane_dt_z,
                                      gid = pane)
    # `pane_length`
    panes@pane_length <- pane_l
  }

  # schema diagrams
  if(schema != "none"){
    if(schema == "by_pane"){
      p_cri <- as.numeric(panes@.Data)
      plot_sets <- p_cri[pane_n > 1]
      plot_sets <- plot_sets[!duplicated(plot_sets)]
      title_seq <- "Pane - PN."
    }else if (schema == "by_strata" & !is.null(strata)){
      p_cri <- cri_l
      plot_sets <- p_cri
      plot_sets <- plot_sets[!duplicated(plot_sets)]
      title_seq <- "Strata - "
    }else if (schema == "by_ALL" | (schema == "by_strata" & is.null(strata))){
      p_cri <- "ALL"
      plot_sets <- p_cri
      plot_sets <- plot_sets[!duplicated(plot_sets)]
      title_seq <- ""
    }

    plots <- lapply(plot_sets, function(x){
      plt_cri <- cri[p_cri == x]
      plt_cri <- plt_cri[!duplicated(plt_cri)]
      if(!is.null(by) | !is.null(length.out)){
        splt_wns <- splits_windows[paste0(plt_cri)]
      }else{
        splt_wns <- splits_windows
      }

      lgk <- (p_cri == x | (cri == plt_cri & pane_n == 1))
      schema(x = panes[lgk],
             date = int[lgk],
             title = paste0(title_seq, x),
             separate = separate,
             ...)
    })
    names(plots) <- plot_sets
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))

  cat("Records partitioned in ", tms, "!\n", sep = "")

  # Output
  if(schema == "none"){
    panes
  }else{
    list("panes" = panes, "plots" = plots)
  }
}
