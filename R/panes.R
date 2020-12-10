#' @name panes
#' @title Track episodes for case definitions and record deduplication - Panes.
#'
#' @description Distribute events into panes defined by time or numerical intervals.
#'
#' @param sn Unique numerical record identifier. Useful for creating familiar episode identifiers.
#' @param strata Subsets. Episodes are tracked separately within each subset. \code{\link{links}} is useful for creating these.
#' @param windows_min Minimum number of overlapping windows in an in an episode. See \code{details}
#' @param separate If \code{TRUE}, events under one window are considered separate episodes from those in another. The default is \code{FALSE}.
#' @param date Event date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}).
#' @param window Numeric or time interval supplied as \code{number_line} objects.
#' @param data_source Unique data source identifier. Useful when the dataset has data from multiple sources.
#' @param custom_sort Preferential order for selecting index (\code{"index"}) events.
#' @param group_stats If \code{TRUE} (default), episode-specific information like episode start and endpoints are returned. See \code{Value}.
#' @param display The messages printed on screen. Options are; \code{"none"} (default) or and \code{"stats"} for a summary of thes process.
#' @param data_links A set of \code{data_sources} required in each episode. A \code{strata} without records from these data sources will be skipped, and episodes without these will be unlinked. See \code{Details}.
#' @return
#'
#' @return \code{\link[=epid-class]{epid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE}
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided (or generated)
#' \item \code{epid | .Data} - unique episode identifier
#' \item \code{wind_id} - unique window identifier
#' \item \code{wind_nm} - type of window i.e. "Index" or "Skipped"
#' \item \code{case_nm} - record type in regards to index assignment
#' \item \code{dist_from_wind} - duration of each event from its window's reference event
#' \item \code{dist_from_epid} - duration of each event from its episode's reference event
#' \item \code{epid_dataset} - data sources in each episode
#' \item \code{epid_interval} - episode start and end dates. A \code{\link{number_line}} object.
#' \item \code{epid_length} - the difference between episode start and end dates (\code{difftime}).
#' \item \code{epid_total} - number of records in each episode
#' \item \code{iteration} - iteration of the process when each event was tracked to its episode.
#' }
#'
#' @seealso
#' \code{\link{episodes}}, \code{\link{links}}, \code{\link{overlaps}} and \code{\link{number_line}}
#'
#' @details
#' Panes are events that overlaps with particular time or numerical intervals (\code{window}).
#' Events that overlap with one window are considered part of the same pane and different from events that overlap with another window.
#'
#' Unlike \code{\link{episodes}}, panes are not tracked in a sequential order.
#' However, by default, the earliest event or smallest record is selected as the \code{"Index"} event.
#' \code{custom_sort} provies some flexibility with regards to which record is taken as the index assignment.
#'
#' \bold{\code{panes()}} will categorise records into 3 types of events;
#'
#' \itemize{
#' \item \code{"Index"} - Index event/record of the pane.
#' \item \code{"Duplicate_I"} - Duplicate of the index case.
#' \item \code{"Skipped"} - Those skipped from the pane distribution process.
#' }
#'
#' \code{data_source} - including this populates the \code{epid_dataset} slot. See \code{Value}.
#'
#' \code{data_links} should be a \code{list} of \code{atomic} vectors with every element named \code{"l"} (links) or \code{"g"} (groups).
#' \itemize{
#' \item \code{"l"} - Episodes with records from every listed data source will be retained.
#' \item \code{"g"} - Episodes with records from any listed data source will be retained.
#' }
#' \code{data_links} is useful for skipping events or records that are not required to minimise processing time.
#'
#' See \code{vignette("panes")} for more information.
#'
#' @examples
#' library(diyar)
#'
#' events <- c(30, 2, 11, 10, 100)
#' windows <- number_line(c(1, 9, 25), c(3, 12, 35))
#'
#' events
#' panes(date = events, window = windows, separate = T)
#' panes(date = events, window = windows, separate = F)
#'
#' panes(date = events, window = windows, separate = T, windows_min = 3)
#' panes(date = events, window = windows, separate = F, windows_min = 4)
#'
#' @aliases panes
#' @export
#'
panes <- function(date, window = Inf, windows_min = Inf, separate = FALSE, sn = NULL, strata = NULL,
                     data_links = "ANY", custom_sort = NULL, group_stats = FALSE,
                     display = "none", data_source = NULL){


  errs <- err_panes_checks_0(date = date,
                   window = window,
                   windows_min = windows_min,
                   separate = separate,
                   display = display,
                   sn = sn,
                   strata = strata,
                   data_source = data_source,
                   data_links = data_links,
                   custom_sort = custom_sort,
                   group_stats = group_stats)

  if(errs!=F) stop(errs, call. = F)

  # mx_win <- max(left_point(windows_min), right_point(windows_min))
  # if(mx_win > length(window)) stop(paste0("x - `windows_min` must be less than the number of windows.\n",
  #                                         "i - You've supplied ", mx_win, "`windows`." ), call. = FALSE)

  dl_lst <- unlist(data_links, use.names = F)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links)=="", "l", names(data_links))

  int <- as.number_line(date)
  windows_min <- number_line(0, windows_min)
  if(!all(class(window) == "list")) window <- as.list(window)
  window <- lapply(window, as.number_line)

  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1, length(int))
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }
  if(is.null(sn)) {
    sn <- seq_len(length(int))
  }
  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort)==1) c_sort <- rep(c_sort, length(int))
  }else{
    c_sort <- rep(0, length(int))
  }

  if(!is.number_line(windows_min)){
    windows_min <- as.number_line(windows_min)
    left_point(windows_min) <- 0
  }

  # union overlapping windows
  correct <- combn(seq_len(length(window)), 2, simplify = F)
  check <- lapply(correct, function(x){
    union_number_lines(window[[x[1]]], window[[x[2]]])
  })

  lgk <- unlist(lapply(check, start_point), use.names = F)
  lgk <- ifelse(is.na(lgk), FALSE, TRUE)

  window <- window[!seq_len(length(window)) %in% unlist(correct[lgk], use.names = F)]
  window <- c(check[lgk], window)

  # check for events under each window
  fnx <- function(x){
    y <- diyar:::ovr_chks(window[[x]], int, rep("overlap", length(int)))
    ifelse(y, x, 0)
  }

  checks <- as.matrix(sapply(seq_len(length(window)), fnx))
  if(length(int) == 1){
    checks <- t(checks)
  }

  checks <- Rfast::rowMinsMaxs(checks)
  lgk <- checks[1,] != checks[2,] & checks[1,] * checks[2,] != 0
  ep_checks <- checks[2,]
  ep_checks[lgk] <- (checks[1,])[lgk]

  case_nm <- ifelse(ep_checks == 0, "Skipped", "Duplicate_I")
  tag <- ifelse(ep_checks == 0,
                 NA_real_, cri + exp(-ep_checks))
  epid <- ifelse(!is.na(tag) & rep(!separate, length(int)),
                 cri + exp(-1),
                 tag)

  dst <- rle(sort(cri[!is.na(tag) & !duplicated(tag)]))
  phits <- as.number_line(dst$lengths[match(cri, dst$values)])
  lgk <- !overlap(phits, windows_min) | is.na(epid)
  epid[lgk] <- seq_len(length(int))[lgk]

  epid <- sn[match(epid, epid[!duplicated(epid)])]
  ord <- order(int@start, right_point(int))

  i <- seq_len(length(int))[ord]; s <- epid[ord]
  i <- i[!duplicated(s) & case_nm[i] != "Skipped"]
  case_nm[i] <- "Index"

  r <- rle(epid[order(epid)])
  epid_n <- rep(r$lengths, r$lengths)

  ei <- epid[which(case_nm %in% c("Index", "Skipped"))]
  ii <- int[which(case_nm %in% c("Index", "Skipped"))]
  lgk <- match(epid, ei)

  dist_from_epid <- ((as.numeric(int@start) + as.numeric(right_point(int))) * .5) -
    ((as.numeric(ii@start[lgk]) + as.numeric(right_point(ii[lgk]))) * .5)

  wind_nm <- case_nm
  wind_nm[wind_nm != "Skipped"] <- "Index"
  epids <- new("epid",
      .Data= epid,
      dist_from_epid = dist_from_epid,
      dist_from_wind = dist_from_epid,
      sn = sn,
      case_nm = case_nm,
      iteration = 1,
      wind_nm = wind_nm,
      wind_id = epid)

  if(!is.null(data_source)){
    # Data links
    rst <- check_links(epids, data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epids@dist_from_epid[!req_links] <- 0
      epids@dist_from_wind[!req_links] <- 0
      epids@case_nm[!req_links] <- "Skipped"
      epids@.Data[!req_links] <- epids@sn[!req_links]
      epids@wind_id[!req_links] <- epids@sn[!req_links]
      datasets[!req_links] <- data_source[!req_links]
    }
    epids@epid_dataset <- datasets
  }

  return(epids)
}

#' @aliases panes
#' @export
#'
panes_2.0 <- function(date, window = Inf, windows_min = Inf, separate = FALSE, sn = NULL, strata = NULL,
                  data_links = "ANY", custom_sort = NULL, group_stats = FALSE,
                  display = "none", data_source = NULL, by = NULL, length.out = NULL, fill =  TRUE, schema = "none"){
  tm_a <- Sys.time()

  errs <- err_panes_checks_0(date = date,
                             window = window,
                             windows_min = windows_min,
                             separate = separate,
                             display = display,
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

  if(errs!=F) stop(errs, call. = F)
  # mx_win <- max(left_point(windows_min), right_point(windows_min))
  # if(mx_win > length(window)) stop(paste0("x - `windows_min` must be less than the number of windows.\n",
  #                                         "i - You've supplied ", mx_win, "`windows`." ), call. = FALSE)

  dl_lst <- unlist(data_links, use.names = F)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links)=="", "l", names(data_links))

  int <- as.number_line(date)
  int@id <- seq_len(length(int))
  #windows_min <- number_line(0, windows_min)
  # if(!all(class(window) == "list")) window <- as.list(window)
  # window <- lapply(window, as.number_line)

  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1, length(int))
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }
  if(is.null(sn)) {
    sn <- seq_len(length(int))
  }
  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort)==1) c_sort <- rep(c_sort, length(int))
  }else{
    c_sort <- rep(0, length(int))
  }

  if(length(windows_min) == 1){
    windows_min <- rep(windows_min, length(int))
  }

  if(!is.number_line(windows_min)){
    windows_min <- as.number_line(windows_min)
    left_point(windows_min) <- 0
  }

  if(is.null(by) | is.null(length.out)){
    split_cri <- cri
  }else{
    split_cri <- rep(1, length(int))
  }

  splits <- split(int, split_cri)
  splits_sn <- split(int@id, split_cri)

  if(!is.null(by)){
    if (length(by) == 1) by <- rep(by, length(int))
    split_bys <- split(by, split_cri)
    splits_func <- function(x, n) {
      n <- n[!duplicated(n)]
      split_number_line(number_line(min(x@start), max(x@start + x@.Data)), by = n, fill = fill)
    }
    splits_windows <- mapply(splits_func, splits, split_bys, SIMPLIFY = FALSE)
  }else if(is.null(by) & !is.null(length.out)){
    if (length(length.out) == 1) length.out <- rep(length.out, length(int))
    split_lnts <- split(length.out, split_cri)
    splits_func <- function(x, n) {
      n <- n[!duplicated(n)]
      split_number_line(number_line(min(x@start), max(x@start + x@.Data)), length.out = n, fill = fill)
    }
    splits_windows <- mapply(splits_func, splits, split_lnts, SIMPLIFY = FALSE)
  }else{
    splits_windows <- list(compress_number_line(as.number_line(window), deduplicate = TRUE, collapse = TRUE))
  }

  checks <- function(dates, windows){
    fnx <- function(x, int = dates){
      y <- diyar:::ovr_chks(windows[[x]], int, rep("overlap", length(int)))
      ifelse(y, x, 0)
    }

    checks <- as.matrix(sapply(as.numeric(seq_len(length(windows))), fnx))
    if(length(int) == 1){
      checks <- t(checks)
    }
    checks <- Rfast::rowMinsMaxs(checks)
    lgk <- checks[1,] != checks[2,] & checks[1,] * checks[2,] != 0
    ep_checks <- checks[2,]
    ep_checks[lgk] <- (checks[1,])[lgk]
    ep_checks
  }

  ep_checks <- mapply(checks, splits, splits_windows, SIMPLIFY = FALSE)
  ep_checks <- unlist(ep_checks, use.names = FALSE)
  splits_sn <- unlist(splits_sn, use.names = FALSE)
  ep_checks <- ep_checks[match(seq_len(length(int)), splits_sn)]

  case_nm <- ifelse(ep_checks == 0, "Skipped", "Duplicate_I")
  tag <- ifelse(ep_checks == 0,
                NA_real_, cri + exp(-ep_checks))
  epid <- ifelse(!is.na(tag) & rep(!separate, length(int)),
                 cri + exp(-1),
                 tag)

  dst <- rle(sort(cri[!is.na(tag) & !duplicated(tag)]))
  phits <- as.number_line(dst$lengths[match(cri, dst$values)])
  lgk <- !overlap(phits, windows_min) | is.na(epid)
  epid[lgk] <- seq_len(length(int))[lgk]

  lgk <- !duplicated(epid)
  epid <- (sn[lgk])[match(epid, epid[lgk])]
  ord <- order(int@start, right_point(int))

  i <- seq_len(length(int))[ord]; s <- epid[ord]
  i <- i[!duplicated(s) & case_nm[i] != "Skipped"]
  case_nm[i] <- "Index"

  r <- rle(epid[order(epid)])
  epid_n <- rep(r$lengths, r$lengths)

  ei <- epid[which(case_nm %in% c("Index", "Skipped"))]
  ii <- int[which(case_nm %in% c("Index", "Skipped"))]
  lgk <- match(epid, ei)

  dist_from_epid <- ((as.numeric(int@start) + as.numeric(right_point(int))) * .5) -
    ((as.numeric(ii@start[lgk]) + as.numeric(right_point(ii[lgk]))) * .5)

  wind_nm <- case_nm
  wind_nm[wind_nm != "Skipped"] <- "Index"
  epids <- new("epid",
               .Data= epid,
               dist_from_epid = dist_from_epid,
               dist_from_wind = dist_from_epid,
               sn = sn,
               case_nm = case_nm,
               iteration = 1,
               wind_nm = wind_nm,
               wind_id = epid)

  if(!is.null(data_source)){
    # Data links
    rst <- check_links(epids, data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epids@dist_from_epid[!req_links] <- 0
      epids@dist_from_wind[!req_links] <- 0
      epids@case_nm[!req_links] <- "Skipped"
      epids@.Data[!req_links] <- epids@sn[!req_links]
      epids@wind_id[!req_links] <- epids@sn[!req_links]
      datasets[!req_links] <- data_source[!req_links]
    }
    epids@epid_dataset <- datasets
  }

  if(group_stats == T){
    lgk <- which(epid_n != 1)
    dts_a <- lapply(split(as.numeric(int@start[lgk]), epid[lgk]), min)
    dts_z <- lapply(split(as.numeric(right_point(int[lgk])), epid[lgk]), max)

    dts_a <- as.numeric(dts_a)[match(epid[lgk], names(dts_a))]
    dts_z <- as.numeric(dts_z)[match(epid[lgk], names(dts_z))]

    epid_dt_a <- as.numeric(int@start)
    epid_dt_z <- right_point(int)

    epid_dt_a[lgk] <- dts_a
    epid_dt_z[lgk] <- dts_z
    is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
    if(is_dt ==T){
      epid_dt_a <- as.POSIXct(epid_dt_a, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_dt_z <- as.POSIXct(epid_dt_z, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_l <- difftime(epid_dt_z, epid_dt_a, units = "secs")
    }else{
      epid_l <- epid_dt_z - epid_dt_a
    }

    epids@epid_interval <- number_line(l = epid_dt_a,
                                      r = epid_dt_z,
                                      gid = epid)
    epids@epid_total <- epid_n
    epids@epid_length <- epid_l
  }

  if(schema == "by_epid"){
    p_cri <- as.numeric(epids@.Data)
    plot_sets <- p_cri[epid_n > 1]
    plot_sets <- plot_sets[!duplicated(plot_sets)]
    plots <- lapply(plot_sets, function(x){
      plt_cri <- cri[p_cri == x]
      plt_cri <- plt_cri[!duplicated(plt_cri)]
      if(!is.null(by) | !is.null(length.out)){
        splt_wns <- splits_windows[paste0(plt_cri)]
      }else{
        splt_wns <- splits_windows
      }

      plot_panes(int = int[p_cri == x | (cri == plt_cri & epid_n ==1)],
                 epids = epids[p_cri == x | (cri == plt_cri & epid_n ==1)],
                 splits_windows = splt_wns,
                 ep_checks = ep_checks[p_cri == x | (cri == plt_cri & epid_n ==1)],
                 title = paste0("E.",x),
                 separate = separate)
    })
    names(plots) <- plot_sets
  }else if (schema == "by_strata" & !is.null(strata)){
    p_cri <- cri
    plot_sets <- p_cri
    plot_sets <- plot_sets[!duplicated(plot_sets)]
    plots <- lapply(plot_sets, function(x){
      if(!is.null(by) | !is.null(length.out)){
        plt_cri <- cri[p_cri == x]
        plt_cri <- plt_cri[!duplicated(plt_cri)]
        splt_wns <- splits_windows[paste0(plt_cri)]
      }else{
        splt_wns <- splits_windows
      }

      plot_panes(int = int[p_cri == x],
                 epids = epids[p_cri == x],
                 splits_windows = splt_wns,
                 ep_checks = ep_checks[p_cri == x],
                 title = paste0("Strata - ",x),
                 separate = separate)
    })
    names(plots) <- plot_sets
  }else if (schema == "ALL" | (schema == "by_strata" & is.null(strata))){
    p_cri <- "ALL"
    plot_sets <- p_cri
    plot_sets <- plot_sets[!duplicated(plot_sets)]
    plots <- lapply(plot_sets, function(x){
      if(!is.null(by) | !is.null(length.out)){
        plt_cri <- cri[p_cri == x]
        plt_cri <- plt_cri[!duplicated(plt_cri)]
        splt_wns <- splits_windows[paste0(plt_cri)]
      }else{
        splt_wns <- splits_windows
      }

      plot_panes(int = int[p_cri == x],
                 epids = epids[p_cri == x],
                 splits_windows = splt_wns,
                 ep_checks = ep_checks[p_cri == x],
                 title = "ALL",
                 separate = separate)
    })
    names(plots) <- plot_sets
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))
  if(display != "none"){
    summ <- paste0("Summary.\n",
                   "Time elapsed:   ", tms, "\n",
                   "Records:\n",
                   "  Total:        ", fmt(length(epids)), "\n",
                   "  Skipped:      ", fmt(length(epid[epids@case_nm == "Skipped"])), "\n",
                   "Episodes:\n",
                   "  Total:        ", fmt(length(epid[epids@case_nm == "Index"])), "\n",
                   "  Single-event: ", fmt(length(epid[epids@case_nm == "Index" & epid_n == 1])), "\n")
    cat(summ)
  }else if(display == "none"){
    cat(paste0("Episode tracking completed in ", tms, "!\n"))
  }
  if(schema == "none"){
    epids
  }else{
    list("epids" = epids, "plots" = plots)
  }
}
