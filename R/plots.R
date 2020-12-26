#' @name schema
#' @aliases schema
#' @title schema
#'
#' @description schema
#' @param pids \code{pid} object
#' @param title Title of \code{ggplot} object
#' @return \code{ggplot}
#' @details
# ' text
#'
#' @examples
#' @export
schema <- function(x, ...) UseMethod("schema")

#' @rdname schema
#' @export
schema.pane <- function(x, int, separate, title = NULL, show_labels = FALSE) {
  panes <- x
  plt_df <- to_df(panes)
  plt_df$start <- int@start
  plt_df$end <- right_point(int)
  plt_df$epid <- as.character(panes@.Data)
  plt_df$pane_id <- panes@.Data
  plt_df$case_nm <- panes@case_nm
  if(isTRUE(separate)){
    plt_df$pane_n <- as.character(plt_df$window_matched)
  }else{
    plt_df$pane_n <- "1"
  }

  # sp should not be more than 1
  #sp <- length(plt_df$pane_n[plt_df$pane_n != 0])/100*2
  #sp <- 1
  plt_df$pane_n[plt_df$window_matched == 0] <- -seq_len(length(plt_df$pane_n[plt_df$window_matched == 0]))
  plt_df$pane_n <- as.character(plt_df$pane_n)

  splits_windows <- plt_df$window_list
  splits_windows <- splits_windows[!duplicated(splits_windows)]
  splits_windows <- lapply(strsplit(splits_windows, ","), diyar:::txt_to_nl)

  border <- do.call(rbind, lapply(splits_windows, function(x){
    x <- to_df(x)
    x$pane_n <- as.character(seq_len(nrow(x)))
    x
  }))

  if(isFALSE(separate)){
    border$pane_n <- "1"
  }

  border$y2 <- 2.05
  border$y1 <- 0.00
  border$win_l <- format(number_line(border$start, border$end))
  panes_n <- nrow(border)

  #plt_df$y <- sample(seq(1-sp , 1 + sp, length.out = nrow(plt_df)),  nrow(plt_df))

  windows_dy <- sort(plt_df$pane_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$pane_id, windows_dy$values)]

  # Data points without finite coordinates.
  # These can't be plotted on the number_line
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  # Alternating range for separate `windows`
  pl_winds <- plt_df$pane_id
  pl_winds[plt_df$wind_total == 1] <- sample(max(as.numeric(plt_df$pane_id)) + 1:2,
                                             length(pl_winds[plt_df$wind_total == 1]),
                                             replace = TRUE)
  epid_winds_n <- length(pl_winds[!duplicated(pl_winds)])

  wind_br_a <- rep(0, epid_winds_n)
  wind_br_z <- rep(0.9, epid_winds_n)
  wind_br_a[seq_len(epid_winds_n) %% 2 == 1] <- 1.1
  wind_br_z[seq_len(epid_winds_n) %% 2 == 1] <- 2
  winds_sn <- split(plt_df$sn, pl_winds)

  # Random `y` coordinates within each window's boundary
  cord_y <- lapply(seq_len(epid_winds_n), function(i){
    if(length(winds_sn[[i]]) > 1){
      sample(seq(wind_br_a[i], wind_br_z[i], length.out = length(winds_sn[[i]])),
             length(winds_sn[[i]]))
    }else{
      wind_br_a[i]
    }
  })
  winds_sn <- unlist(winds_sn, use.names = FALSE)
  cord_y <- unlist(cord_y, use.names = FALSE)
  plt_df$y <- cord_y[match(plt_df$sn, winds_sn)]


  plt_df$mid_x <- (plt_df$start + plt_df$end)/2
  link_sn <- plt_df[plt_df$sn %in% plt_df$pane_id, c("sn", "mid_x", "y")]

  plt_df$x_lead <- link_sn$mid_x[match(plt_df$pane_id, link_sn$sn)]
  plt_df$y_lead <- link_sn$y[match(plt_df$pane_id, link_sn$sn)]

  if(isTRUE(show_labels)){
    show_labels <- c("sn", "pane", "date", "case_nm")
  }

  if(!isFALSE(show_labels)){
    plt_df$event_type <- ""
    plt_df$event_nm <- ""
    if("pane" %in%  show_labels){
      plt_df$event_type <- paste0("PN.", plt_df$epid)
    }
    if("case_nm" %in%  show_labels){
      plt_df$event_type <- paste0(plt_df$event_type, "\n",
                                  plt_df$case_nm, "\n",
                                  ifelse(plt_df$sn %in% plt_df$pane_id & plt_df$case_nm != "Skipped",
                                         "(reference)\n",""),
                                  "event")
    }
    if("date" %in%  show_labels){
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$end)
      plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                                format(left_point(plt_df$event_nm)),
                                format(plt_df$event_nm))
    }
    if("sn" %in%  show_labels){
      plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ",
                                plt_df$event_nm)
    }
  }

  border$end[border$end > max(plt_df$end) | is.infinite(border$end)] <- max(plt_df$end)
  border$start[border$start < min(plt_df$start) | is.infinite(border$start)] <- min(plt_df$start)

  plt_df <- plt_df[plt_df$finite,]
  plot_pts <- nrow(plt_df)
  min_x <- min(c(plt_df$start, border$start))
  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = y, yend = y, colour = pane_n), size = scale_size(c(.1,1), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = start, y = y, color = pane_n), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = end, y = y, color = pane_n), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_segment(ggplot2::aes(x = start, y= y, colour = pane_n, xend = end, yend = y)) +
    ggplot2::geom_segment(ggplot2::aes(x = mid_x, y= y, colour = pane_n, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = start, xmax = end, ymin = y1, ymax =y2, fill = pane_n), data = border, alpha = .2) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = start, y = y1, yend = y2, color = pane_n), data = border, alpha = .7) +
    ggplot2::geom_segment(ggplot2::aes(x = end, xend = end, y = y1, yend = y2, color = pane_n), data = border, alpha = .7) +
    ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start + end))/2, y= y2, colour = pane_n, label = win_l), data = border, nudge_y = .05, size = 5)

  if(!isFALSE(show_labels)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = pane_n, label = event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "bottom", alpha= .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = pane_n, label = event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "top", alpha= .7)
  }

  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y= 2.2, label = title), colour = "white", size = 5)
  }
  f <- f +
    ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  f
}

#' @rdname schema
#' @export
schema.epid <- function(x, date, episode_unit, case_length,
                       recurrence_length = case_length, episode_type,
                       from_last, title = NULL, show_labels = FALSE,
                       show_skipped = TRUE, show_non_finite = FALSE){
  epid <- x
  # Validations
  errs <- err_episodes_checks_0(date = date, case_length = case_length,
                                recurrence_length = recurrence_length,
                                episode_unit = episode_unit,
                                episode_type = episode_type, from_last = from_last)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "epid", "date", "case_nm"))
  if(!isFALSE(err)) stop(err, call. = FALSE)
  # Standardise inputs
  # `date`
  int <- as.number_line(date)
  is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  if(isTRUE(is_dt)){
    int <- number_line(
      l = as.POSIXct(int@start),
      r = as.POSIXct(right_point(int))
    )
  }
  # `episode_unit`
  episode_unit[!is_dt] <- "seconds"
  # `case_length`
  ep_l <- length_to_range(lengths = case_length,
                          int = int,
                          from_last = from_last,
                          ep_units = episode_unit)
  any_rolling <- any(episode_type == "rolling")
  if(any_rolling){
    # `recurrence_length`
    rc_l <- length_to_range(lengths = recurrence_length,
                            int = int,
                            from_last = from_last,
                            ep_units = episode_unit)
  }

  # Plot data
  plt_df <- to_df(epids)
  if(isFALSE(show_skipped)){
    lgk <- plt_df$case_nm != "Skipped"
    int <- int[lgk]
    plt_df <- plt_df[plt_df$case_nm != "Skipped",]
    ep_l <- lapply(ep_l, function(x) x[lgk])
    if(any_rolling){
      rc_l <- lapply(rc_l, function(x) x[lgk])
    }

  }
  plt_df$start <- left_point(int)
  plt_df$end <- right_point(int)
  plt_df$epid <- as.character(plt_df$epid)

  windows_dy <- sort(plt_df$wind_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$wind_id, windows_dy$values)]

  # Data points without finite coordinates.
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  # Alternating ranges (y-axis) for each window
  pl_winds <- plt_df$wind_id
  pl_winds[plt_df$wind_total == 1] <- sample(max(plt_df$wind_id) + 1:2,
                                             length(pl_winds[plt_df$wind_total == 1]),
                                             replace = TRUE)
  epid_winds_n <- length(pl_winds[!duplicated(pl_winds)])

  wind_br_a <- rep(0, epid_winds_n)
  wind_br_z <- rep(0.9, epid_winds_n)
  wind_br_a[seq_len(epid_winds_n) %% 2 == 1] <- 1.1
  wind_br_z[seq_len(epid_winds_n) %% 2 == 1] <- 2
  winds_sn <- split(plt_df$sn, pl_winds)

  # Random `y` coordinates within each window's boundary
  cord_y <- lapply(seq_len(epid_winds_n), function(i){
    if(length(winds_sn[[i]]) > 1){
      sample(seq(wind_br_a[i], wind_br_z[i], length.out = length(winds_sn[[i]])),
             length(winds_sn[[i]]))
    }else{
      wind_br_a[i]
    }
  })
  winds_sn <- unlist(winds_sn, use.names = FALSE)
  cord_y <- unlist(cord_y, use.names = FALSE)
  plt_df$y <- cord_y[match(plt_df$sn, winds_sn)]

  winds_cord_y <- split(plt_df$y, pl_winds)
  mid_y <- lapply(winds_cord_y, function(x){
    rep(mean(x), length(x))
  })
  # Midpoint of each window's boundaries (y-axis)
  mid_y <- unlist(mid_y, use.names = FALSE)
  plt_df$mid_y <- mid_y[match(plt_df$sn, winds_sn)]

  plt_df$start <- as.numeric(plt_df$start)
  plt_df$end <- as.numeric(plt_df$end)

  plt_df$episode_unit <- episode_unit
  # Case length arrows
  case_l_ar <- l_ar(ep_l, plt_df, "Case", is_dt)

  if(any_rolling == T){
    # Recurrence length arrows
    rc_l_ar <- l_ar(rc_l, plt_df, "Recurrence", is_dt)
    case_l_ar <- c(case_l_ar, rc_l_ar)
  }
  case_l_ar <- do.call(rbind, case_l_ar)

  any_finite <- length(plt_df$start[plt_df$finite]) > 0
  if(any_finite){
    breaks <- seq(min(as.numeric(plt_df$start[plt_df$finite])), max(as.numeric(plt_df$start[plt_df$finite])), length.out = 10)
    labels <- seq(min(plt_df$start[plt_df$finite]), max(plt_df$start[plt_df$finite]), length.out = 10)

    if(is_dt == TRUE){
      # Sensible labels for time points. Based on `episode_unit`
      if(min(which(names(diyar::episode_unit) == episode_unit)) >= 4){
        labels <- as.Date(as.POSIXct(labels, "GMT", origin = as.POSIXct("1970-01-01", "GMT")))
        plt_df$event_nm <- number_line(as.Date(as.POSIXct(plt_df$start, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))),
                                       as.Date(as.POSIXct(plt_df$end, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))))
      }else{
        labels <- as.POSIXct(labels, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
        plt_df$event_nm <- number_line(as.POSIXct(plt_df$start, "GMT", origin = as.POSIXct("1970-01-01", "GMT")),
                                       as.POSIXct(plt_df$end, "GMT", origin = as.POSIXct("1970-01-01", "GMT")))
      }
    }else{
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$start)
    }
  }else{
    breaks <- 0
    labels <- "Unknown"
  }

  if(length(!duplicated(breaks)) == 1){
    int <- 0
  }else{
    int <- (breaks[2]-breaks[1])
  }

  # Labels to plot
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "epid", "date", "case_nm")
  }

  if(!isFALSE(show_labels)){
    plt_df$event_type <- ""
    plt_df$event_nm <- ""
    if("epid" %in%  show_labels){
      plt_df$event_type <- paste0("E.", plt_df$epid)
    }
    if("case_nm" %in%  show_labels){
      plt_df$event_type <- paste0(plt_df$event_type, "\n",
                                  plt_df$case_nm, "\n",
                                  ifelse(plt_df$sn %in% plt_df$wind_id & plt_df$case_nm != "Skipped",
                                         "(reference)\n",""),
                                  "event")
    }
    if("date" %in%  show_labels){
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$end)
      plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                                format(left_point(plt_df$event_nm)),
                                format(plt_df$event_nm))
    }
    if("sn" %in%  show_labels){
      plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ",
                                plt_df$event_nm)
    }
  }

  if(isTRUE(show_non_finite)){
    # Add non-finite `dates` to plot.
    plt_df$start[!plt_df$finite] <- sample(seq(max(breaks) + (int * 1),
                                               max(breaks) + (int * 3),
                                               length.out = length(plt_df$start[!plt_df$finite])),
                                           length(plt_df$start[!plt_df$finite]))

    plt_df$end[!plt_df$finite] <- plt_df$start[!plt_df$finite]

    breaks2 <- seq(max(breaks), int, length.out = 3)
    labels2 <- rep("", 3)
    labels2[median(seq_len(length(labels2)))] <- "Unknown"
  }else{
    breaks2 <- breaks[0]
    labels2 <- labels[0]
    plt_df <- plt_df[plt_df$finite,]
  }

  # Link between `dates`
  plt_df$mid_x <- (as.numeric(plt_df$start) + as.numeric( plt_df$end))/2
  link_sn <- plt_df[plt_df$sn %in% plt_df$wind_id, c("sn", "mid_x", "y")]
  plt_df$x_lead <- link_sn$mid_x[match(plt_df$wind_id, link_sn$sn)]
  plt_df$y_lead <- link_sn$y[match(plt_df$wind_id, link_sn$sn)]

  case_l_ar$start <- as.numeric(case_l_ar$start)
  case_l_ar$end <- as.numeric(case_l_ar$end)
  if(nrow(case_l_ar) > 0){
    case_l_ar$end[case_l_ar$end > max(plt_df$end) | is.infinite(case_l_ar$end)] <- max(plt_df$end)
    case_l_ar$start[case_l_ar$start < min(plt_df$start) | is.infinite(case_l_ar$start)] <- min(plt_df$start)
    case_l_ar$no_ar <- as.numeric(case_l_ar$nl_s) == 0 & as.numeric(case_l_ar$nl_e) == 0
    case_l_ar$lab_y[case_l_ar$no_ar] <- case_l_ar$y[case_l_ar$no_ar]
    case_l_ar$nl_l <- paste0(case_l_ar$wind_nm_l, "\n",
                             format(number_line(case_l_ar$nl_s, case_l_ar$nl_e)),
                             " ", ifelse(is_dt, gsub("s$", "-", episode_unit), "unit-"),
                             "difference.")
  }else{
    case_l_ar$no_ar <- logical()
    case_l_ar$nl_l <- character()
  }

  plot_pts <- nrow(plt_df)
  min_x <- min(plt_df$start)
  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = y, yend = y, colour = epid), size = scale_size(c(.1,1), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = start, y = y, colour = epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = end, y = y, colour = epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_segment(ggplot2::aes(x = mid_x, y= y, colour = epid, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_segment(ggplot2::aes(x = start, y= y, xend = end, yend = mid_y_lead, linetype = wind_nm_l), color = "white", alpha= .9, data = case_l_ar[case_l_ar$nl_nm == "len" & !case_l_ar$no_ar & case_l_ar$wind_total > 1,], arrow = ggplot2::arrow(length = ggplot2::unit(scale_size(c(.5,.2), 500, plot_pts),"cm"), ends="last", type = "open")) +
    ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= lab_y, label = nl_l), data = case_l_ar[case_l_ar$nl_nm == "len",], nudge_y = scale_size(c(.02, .06), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), color = "white", alpha= .9, vjust = "bottom")
  if(!isFALSE(show_labels)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = epid, label = event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "bottom", alpha= .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = epid, label = event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "top", alpha= .7)
  }
  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y= 2.15, label = title), colour = "white", size = 5)
  }
  if(isTRUE(show_non_finite)){
    t1 <- plt_df[!plt_df$finite,]
    x1 <- min(t1$x); x2 <- max(t1$x)
    f <- f +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x1), alpha = 1, color = "white", linetype = 3) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(x1) + as.numeric(x2))/2, y = 1), color = "white", label = "Non-finite\nevents)", size = scale_size(c(2,5), 500, plot_pts), alpha= .9, hjust = "middle")
  }
  f <- f + ggplot2::theme(
    legend.position = "none",
    legend.background = ggplot2::element_rect(fill = "black"),
    legend.text = ggplot2::element_text(colour = "white"),
    plot.background = ggplot2::element_rect(fill = "black"),
    panel.background = ggplot2::element_rect(fill = "black"),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )
  f
}
#' @rdname schema
#' @export
schema.pid <- function(x, title = NULL, show_labels = FALSE){
  pids <- x
  pl_dt <- to_df(pids)
  cris <- length(unique(pl_dt$pid_cri))
  pl_dt$pid_cri <- ifelse(pl_dt$pid_cri == 0, "No Hits", ifelse(pl_dt$pid_cri == -1, "Skipped", paste0("CRI ", pl_dt$pid_cri)))
  order <- ceiling(cris / 8)
  boxes_w <- 10
  border <-  lapply(seq_len(order), function(x) box_ring(order = x, boxes_w = boxes_w))
  border <- do.call(rbind, c(list(box(boxes_w)), border))
  border <- head(border, cris)
  mx <- boxes_w * order
  border$pid_box <- seq_len(nrow(border))

  pl_dt$pid_box <- match(pl_dt$pid_cri,  pl_dt$pid_cri[!duplicated(pl_dt$pid_cri)] )

  atc_ljoin <- function(x, y, key, data){
    split(data[[x]], data[[key]])
  }

  pl_dt$x1 <- border$x1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$x2 <- border$x2[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y1 <- border$y1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y2 <- border$y2[match(pl_dt$pid_box, border$pid_box)]

  cords <- function(cord, dt = pl_dt, bw = boxes_w){
    pts <- split(reverse_number_line(number_line(dt[[paste0(cord,"1")]], dt[[paste0(cord,"2")]]), direction = "decreasing"), dt$pid_box)
    pts <- unlist(lapply(pts, function(x){
      y <- unique(x)
      wd <- y@.Data
      y <- seq(left_point(y), right_point(y), length.out = length(x))
      y[1] <- y[1] + (wd * .1)
      if(length(y) > 1){
        y[length(y)] <- y[length(y)] + (wd * -.1)
        y <- sample(y, length(y))
      }
      y
    }), use.names = F)
    sn <- unlist(split(seq_len(nrow(dt)), dt$pid_box), use.names = F)
    pts <- pts[order(sn)]
    pts
  }

  pl_dt$x <- cords("x")
  pl_dt$y <- cords("y")

  link_sn <- pl_dt[pl_dt$sn %in% pl_dt$link_id, c("sn", "x", "y")]

  pl_dt$x_lead <- link_sn$x[match(pl_dt$link_id, link_sn$sn)]
  pl_dt$y_lead <- link_sn$y[match(pl_dt$link_id, link_sn$sn)]

  pl_dt$pid <- as.character(pl_dt$pid)
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "pid")
  }

  if(!isFALSE(show_labels)){
    pl_dt$event_nm <- ""
    pl_dt$pid_l <- ""
    if("sn" %in% show_labels){
      pl_dt$event_nm <- paste0("SN ", pl_dt$sn)
    }
    if("pid" %in% show_labels){
      pl_dt$pid_l <- paste0("P.", pl_dt$pid)
    }
  }


  border$pid_cri <- pl_dt$pid_cri[match(border$pid_box, pl_dt$pid_box)]
  tx_l <- min(pl_dt$x1)
  tx_h <- max(pl_dt$y2)
  boxes_n <- nrow(border)
  #return(border)
  f <- ggplot2::ggplot(data = pl_dt) +
    ggplot2::geom_point(ggplot2::aes(x = x, y= y, colour = pid), size = scale_size(c(1,3), 125, boxes_n), alpha =.7) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y= y, colour = pid, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax =y2,  fill = pid_cri), data = border, alpha = .1) +
    ggplot2::geom_text(ggplot2::aes(x = (x1 + x2)/2, y= (y1 + y2)/2, colour = pid_cri, label = pid_cri), size = scale_size(c(9, 30), 125, boxes_n), alpha = scale_size(c(.1, .2), 125, boxes_n, decreasing = FALSE), data = border)
  if(!isFALSE(show_labels)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = x, y= y, colour = pid, label = event_nm), nudge_y = scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "bottom", size = scale_size(c(2,5), 125, boxes_n), alpha =.7) +
      ggplot2::geom_text(ggplot2::aes(x = x, y= y, colour = pid, label = pid_l), nudge_y = -scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "top", size = scale_size(c(2,5), 125, boxes_n), alpha =.7)
  }

  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min(c(border$x1, border$x2)), y = max(c(border$y1, border$y2))) , colour = "white", label = title, size = 5, nudge_y = scale_size(c(.3, .3), 125, boxes_n, decreasing = FALSE))
  }
  f <- f +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 10)) +
    ggplot2::theme(
      legend.position = "none",
      legend.text = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
  f
}

plot_pids <- function(pids, title = NULL, show_labels = FALSE){
  pl_dt <- to_df(pids)
  cris <- length(unique(pl_dt$pid_cri))
  pl_dt$pid_cri <- ifelse(pl_dt$pid_cri == 0, "No Hits", ifelse(pl_dt$pid_cri == -1, "Skipped", paste0("CRI ", pl_dt$pid_cri)))
  order <- ceiling(cris / 8)
  boxes_w <- 10
  border <-  lapply(seq_len(order), function(x) box_ring(order = x, boxes_w = boxes_w))
  border <- do.call(rbind, c(list(box(boxes_w)), border))
  border <- head(border, cris)
  mx <- boxes_w * order
  border$pid_box <- seq_len(nrow(border))

  pl_dt$pid_box <- match(pl_dt$pid_cri,  pl_dt$pid_cri[!duplicated(pl_dt$pid_cri)] )

  atc_ljoin <- function(x, y, key, data){
    split(data[[x]], data[[key]])
  }

  pl_dt$x1 <- border$x1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$x2 <- border$x2[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y1 <- border$y1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y2 <- border$y2[match(pl_dt$pid_box, border$pid_box)]

  cords <- function(cord, dt = pl_dt, bw = boxes_w){
    pts <- split(reverse_number_line(number_line(dt[[paste0(cord,"1")]], dt[[paste0(cord,"2")]]), direction = "decreasing"), dt$pid_box)
    pts <- unlist(lapply(pts, function(x){
      y <- unique(x)
      wd <- y@.Data
      y <- seq(left_point(y), right_point(y), length.out = length(x))
      y[1] <- y[1] + (wd * .1)
      if(length(y) > 1){
        y[length(y)] <- y[length(y)] + (wd * -.1)
        y <- sample(y, length(y))
      }
      y
    }), use.names = F)
    sn <- unlist(split(seq_len(nrow(dt)), dt$pid_box), use.names = F)
    pts <- pts[order(sn)]
    pts
  }

  pl_dt$x <- cords("x")
  pl_dt$y <- cords("y")

  link_sn <- pl_dt[pl_dt$sn %in% pl_dt$link_id, c("sn", "x", "y")]

  pl_dt$x_lead <- link_sn$x[match(pl_dt$link_id, link_sn$sn)]
  pl_dt$y_lead <- link_sn$y[match(pl_dt$link_id, link_sn$sn)]

  pl_dt$pid <- as.character(pl_dt$pid)
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "pid")
  }

  if(!isFALSE(show_labels)){
    pl_dt$event_nm <- ""
    pl_dt$pid_l <- ""
    if("sn" %in% show_labels){
      pl_dt$event_nm <- paste0("SN ", pl_dt$sn)
    }
    if("pid" %in% show_labels){
      pl_dt$pid_l <- paste0("P.", pl_dt$pid)
    }
  }


  border$pid_cri <- pl_dt$pid_cri[match(border$pid_box, pl_dt$pid_box)]
  tx_l <- min(pl_dt$x1)
  tx_h <- max(pl_dt$y2)
  boxes_n <- nrow(border)
  #return(border)
  f <- ggplot2::ggplot(data = pl_dt) +
    ggplot2::geom_point(ggplot2::aes(x = x, y= y, colour = pid), size = scale_size(c(1,3), 125, boxes_n), alpha =.7) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y= y, colour = pid, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax =y2,  fill = pid_cri), data = border, alpha = .1) +
    ggplot2::geom_text(ggplot2::aes(x = (x1 + x2)/2, y= (y1 + y2)/2, colour = pid_cri, label = pid_cri), size = scale_size(c(9, 30), 125, boxes_n), alpha = scale_size(c(.1, .2), 125, boxes_n, decreasing = FALSE), data = border)
    if(!isFALSE(show_labels)){
      f <- f +
        ggplot2::geom_text(ggplot2::aes(x = x, y= y, colour = pid, label = event_nm), nudge_y = scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "bottom", size = scale_size(c(2,5), 125, boxes_n), alpha =.7) +
        ggplot2::geom_text(ggplot2::aes(x = x, y= y, colour = pid, label = pid_l), nudge_y = -scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "top", size = scale_size(c(2,5), 125, boxes_n), alpha =.7)
      }

  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min(c(border$x1, border$x2)), y = max(c(border$y1, border$y2))) , colour = "white", label = title, size = 5, nudge_y = scale_size(c(.3, .3), 125, boxes_n, decreasing = FALSE))
  }
  f <- f +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 10)) +
    ggplot2::theme(
      legend.position = "none",
      legend.text = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
  f
}

plot_partitions <- function(panes, int, title = NULL, separate, show_labels = FALSE){

  plt_df <- to_df(panes)
  plt_df$start <- int@start
  plt_df$end <- right_point(int)
  plt_df$epid <- as.character(panes@.Data)
  plt_df$pane_id <- panes@.Data
  plt_df$case_nm <- panes@case_nm
  if(isTRUE(separate)){
    plt_df$pane_n <- as.character(plt_df$window_matched)
  }else{
    plt_df$pane_n <- "1"
  }

  # sp should not be more than 1
  #sp <- length(plt_df$pane_n[plt_df$pane_n != 0])/100*2
  #sp <- 1
  plt_df$pane_n[plt_df$window_matched == 0] <- -seq_len(length(plt_df$pane_n[plt_df$window_matched == 0]))
  plt_df$pane_n <- as.character(plt_df$pane_n)

  splits_windows <- plt_df$window_list
  splits_windows <- splits_windows[!duplicated(splits_windows)]
  splits_windows <- lapply(strsplit(splits_windows, ","), diyar:::txt_to_nl)

  border <- do.call(rbind, lapply(splits_windows, function(x){
    x <- to_df(x)
    x$pane_n <- as.character(seq_len(nrow(x)))
    x
  }))

  if(isFALSE(separate)){
    border$pane_n <- "1"
  }

  border$y2 <- 2.05
  border$y1 <- 0.00
  border$win_l <- format(number_line(border$start, border$end))
  panes_n <- nrow(border)

  #plt_df$y <- sample(seq(1-sp , 1 + sp, length.out = nrow(plt_df)),  nrow(plt_df))

  windows_dy <- sort(plt_df$pane_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$pane_id, windows_dy$values)]

  # Data points without finite coordinates.
  # These can't be plotted on the number_line
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  # Alternating range for separate `windows`
  pl_winds <- plt_df$pane_id
  pl_winds[plt_df$wind_total == 1] <- sample(max(as.numeric(plt_df$pane_id)) + 1:2,
                                             length(pl_winds[plt_df$wind_total == 1]),
                                             replace = TRUE)
  epid_winds_n <- length(pl_winds[!duplicated(pl_winds)])

  wind_br_a <- rep(0, epid_winds_n)
  wind_br_z <- rep(0.9, epid_winds_n)
  wind_br_a[seq_len(epid_winds_n) %% 2 == 1] <- 1.1
  wind_br_z[seq_len(epid_winds_n) %% 2 == 1] <- 2
  winds_sn <- split(plt_df$sn, pl_winds)

  # Random `y` coordinates within each window's boundary
  cord_y <- lapply(seq_len(epid_winds_n), function(i){
    if(length(winds_sn[[i]]) > 1){
      sample(seq(wind_br_a[i], wind_br_z[i], length.out = length(winds_sn[[i]])),
             length(winds_sn[[i]]))
    }else{
      wind_br_a[i]
    }
  })
  winds_sn <- unlist(winds_sn, use.names = FALSE)
  cord_y <- unlist(cord_y, use.names = FALSE)
  plt_df$y <- cord_y[match(plt_df$sn, winds_sn)]


  plt_df$mid_x <- (plt_df$start + plt_df$end)/2
  link_sn <- plt_df[plt_df$sn %in% plt_df$pane_id, c("sn", "mid_x", "y")]

  plt_df$x_lead <- link_sn$mid_x[match(plt_df$pane_id, link_sn$sn)]
  plt_df$y_lead <- link_sn$y[match(plt_df$pane_id, link_sn$sn)]

  if(isTRUE(show_labels)){
    show_labels <- c("sn", "epid", "date", "case_nm")
  }

  if(!isFALSE(show_labels)){
    plt_df$event_type <- ""
    plt_df$event_nm <- ""
    if("epid" %in%  show_labels){
      plt_df$event_type <- paste0("E.", plt_df$epid)
    }
    if("case_nm" %in%  show_labels){
      plt_df$event_type <- paste0(plt_df$event_type, "\n",
                                  plt_df$case_nm, "\n",
                                  ifelse(plt_df$sn %in% plt_df$pane_id & plt_df$case_nm != "Skipped",
                                         "(reference)\n",""),
                                  "event")
    }
    if("date" %in%  show_labels){
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$end)
      plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                                format(left_point(plt_df$event_nm)),
                                format(plt_df$event_nm))
    }
    if("sn" %in%  show_labels){
      plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ",
                                plt_df$event_nm)
    }
  }


  plt_df <- plt_df[plt_df$finite,]
  plot_pts <- nrow(plt_df)
  min_x <- min(c(plt_df$start, border$start))
  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = y, yend = y, colour = pane_n), size = scale_size(c(.1,1), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = start, y = y, color = pane_n), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = end, y = y, color = pane_n), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_segment(ggplot2::aes(x = start, y= y, colour = pane_n, xend = end, yend = y)) +
    ggplot2::geom_segment(ggplot2::aes(x = mid_x, y= y, colour = pane_n, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = start, xmax = end, ymin = y1, ymax =y2, fill = pane_n), data = border, alpha = .2) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = start, y = y1, yend = y2, color = pane_n), data = border, alpha = .7) +
    ggplot2::geom_segment(ggplot2::aes(x = end, xend = end, y = y1, yend = y2, color = pane_n), data = border, alpha = .7) +
    ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start + end))/2, y= y2, colour = pane_n, label = win_l), data = border, nudge_y = .05, size = 5)

  if(!isFALSE(show_labels)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = pane_n, label = event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "bottom", alpha= .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = pane_n, label = event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "top", alpha= .7)
    }

  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y= 2.2, label = title), colour = "white", size = 5)
    }
   f <- f +
     ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
   f
}

plot_epids <- function(epids, date, episode_unit, case_length,
                       recurrence_length = case_length, episode_type,
                       from_last, title = NULL, show_labels = FALSE,
                       show_skipped = TRUE, show_non_finite = FALSE){
  # Validations
  errs <- err_episodes_checks_0(date = date, case_length = case_length,
                                recurrence_length = recurrence_length,
                                episode_unit = episode_unit,
                                episode_type = episode_type, from_last = from_last)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "epid", "date", "case_nm"))
  if(!isFALSE(err)) stop(err, call. = FALSE)
  # Standardise inputs
    # `date`
  int <- as.number_line(date)
  is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  if(isTRUE(is_dt)){
    int <- number_line(
      l = as.POSIXct(int@start),
      r = as.POSIXct(right_point(int))
    )
  }
    # `episode_unit`
  episode_unit[!is_dt] <- "seconds"
    # `case_length`
  ep_l <- length_to_range(lengths = case_length,
                          int = int,
                          from_last = from_last,
                          ep_units = episode_unit)
  any_rolling <- any(episode_type == "rolling")
  if(any_rolling){
    # `recurrence_length`
    rc_l <- length_to_range(lengths = recurrence_length,
                            int = int,
                            from_last = from_last,
                            ep_units = episode_unit)
  }

  # Plot data
  plt_df <- to_df(epids)
  if(isFALSE(show_skipped)){
    lgk <- plt_df$case_nm != "Skipped"
    int <- int[lgk]
    plt_df <- plt_df[plt_df$case_nm != "Skipped",]
    ep_l <- lapply(ep_l, function(x) x[lgk])
    if(any_rolling){
      rc_l <- lapply(rc_l, function(x) x[lgk])
    }

  }
  plt_df$start <- left_point(int)
  plt_df$end <- right_point(int)
  plt_df$epid <- as.character(plt_df$epid)

  windows_dy <- sort(plt_df$wind_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$wind_id, windows_dy$values)]

  # Data points without finite coordinates.
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  # Alternating ranges (y-axis) for each window
  pl_winds <- plt_df$wind_id
  pl_winds[plt_df$wind_total == 1] <- sample(max(plt_df$wind_id) + 1:2,
                                             length(pl_winds[plt_df$wind_total == 1]),
                                             replace = TRUE)
  epid_winds_n <- length(pl_winds[!duplicated(pl_winds)])

  wind_br_a <- rep(0, epid_winds_n)
  wind_br_z <- rep(0.9, epid_winds_n)
  wind_br_a[seq_len(epid_winds_n) %% 2 == 1] <- 1.1
  wind_br_z[seq_len(epid_winds_n) %% 2 == 1] <- 2
  winds_sn <- split(plt_df$sn, pl_winds)

  # Random `y` coordinates within each window's boundary
  cord_y <- lapply(seq_len(epid_winds_n), function(i){
    if(length(winds_sn[[i]]) > 1){
      sample(seq(wind_br_a[i], wind_br_z[i], length.out = length(winds_sn[[i]])),
             length(winds_sn[[i]]))
    }else{
      wind_br_a[i]
    }
  })
  winds_sn <- unlist(winds_sn, use.names = FALSE)
  cord_y <- unlist(cord_y, use.names = FALSE)
  plt_df$y <- cord_y[match(plt_df$sn, winds_sn)]

  winds_cord_y <- split(plt_df$y, pl_winds)
  mid_y <- lapply(winds_cord_y, function(x){
    rep(mean(x), length(x))
  })
  # Midpoint of each window's boundaries (y-axis)
  mid_y <- unlist(mid_y, use.names = FALSE)
  plt_df$mid_y <- mid_y[match(plt_df$sn, winds_sn)]

  plt_df$start <- as.numeric(plt_df$start)
  plt_df$end <- as.numeric(plt_df$end)

  plt_df$episode_unit <- episode_unit
  # Case length arrows
  case_l_ar <- l_ar(ep_l, plt_df, "Case", is_dt)

  if(any_rolling == T){
    # Recurrence length arrows
    rc_l_ar <- l_ar(rc_l, plt_df, "Recurrence", is_dt)
    case_l_ar <- c(case_l_ar, rc_l_ar)
  }
  case_l_ar <- do.call(rbind, case_l_ar)

  any_finite <- length(plt_df$start[plt_df$finite]) > 0
  if(any_finite){
    breaks <- seq(min(as.numeric(plt_df$start[plt_df$finite])), max(as.numeric(plt_df$start[plt_df$finite])), length.out = 10)
    labels <- seq(min(plt_df$start[plt_df$finite]), max(plt_df$start[plt_df$finite]), length.out = 10)

    if(is_dt == TRUE){
      # Sensible labels for time points. Based on `episode_unit`
      if(min(which(names(diyar::episode_unit) == episode_unit)) >= 4){
        labels <- as.Date(as.POSIXct(labels, "GMT", origin = as.POSIXct("1970-01-01", "GMT")))
        plt_df$event_nm <- number_line(as.Date(as.POSIXct(plt_df$start, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))),
                                       as.Date(as.POSIXct(plt_df$end, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))))
      }else{
        labels <- as.POSIXct(labels, "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
        plt_df$event_nm <- number_line(as.POSIXct(plt_df$start, "GMT", origin = as.POSIXct("1970-01-01", "GMT")),
                                       as.POSIXct(plt_df$end, "GMT", origin = as.POSIXct("1970-01-01", "GMT")))
      }
    }else{
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$start)
    }
  }else{
    breaks <- 0
    labels <- "Unknown"
  }

  if(length(!duplicated(breaks)) == 1){
    int <- 0
  }else{
    int <- (breaks[2]-breaks[1])
  }

  # Labels to plot
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "epid", "date", "case_nm")
  }

  if(!isFALSE(show_labels)){
    plt_df$event_type <- ""
    plt_df$event_nm <- ""
    if("epid" %in%  show_labels){
      plt_df$event_type <- paste0("E.", plt_df$epid)
    }
    if("case_nm" %in%  show_labels){
      plt_df$event_type <- paste0(plt_df$event_type, "\n",
                                  plt_df$case_nm, "\n",
                                  ifelse(plt_df$sn %in% plt_df$wind_id & plt_df$case_nm != "Skipped",
                                         "(reference)\n",""),
                                  "event")
    }
    if("date" %in%  show_labels){
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$end)
      plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                                format(left_point(plt_df$event_nm)),
                                format(plt_df$event_nm))
    }
    if("sn" %in%  show_labels){
      plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ",
                                plt_df$event_nm)
    }
  }

  if(isTRUE(show_non_finite)){
    # Add non-finite `dates` to plot.
    plt_df$start[!plt_df$finite] <- sample(seq(max(breaks) + (int * 1),
                                               max(breaks) + (int * 3),
                                               length.out = length(plt_df$start[!plt_df$finite])),
                                           length(plt_df$start[!plt_df$finite]))

    plt_df$end[!plt_df$finite] <- plt_df$start[!plt_df$finite]

    breaks2 <- seq(max(breaks), int, length.out = 3)
    labels2 <- rep("", 3)
    labels2[median(seq_len(length(labels2)))] <- "Unknown"
  }else{
    breaks2 <- breaks[0]
    labels2 <- labels[0]
    plt_df <- plt_df[plt_df$finite,]
  }

  # Link between `dates`
  plt_df$mid_x <- (as.numeric(plt_df$start) + as.numeric( plt_df$end))/2
  link_sn <- plt_df[plt_df$sn %in% plt_df$wind_id, c("sn", "mid_x", "y")]
  plt_df$x_lead <- link_sn$mid_x[match(plt_df$wind_id, link_sn$sn)]
  plt_df$y_lead <- link_sn$y[match(plt_df$wind_id, link_sn$sn)]

  case_l_ar$start <- as.numeric(case_l_ar$start)
  case_l_ar$end <- as.numeric(case_l_ar$end)
  if(nrow(case_l_ar) > 0){
    case_l_ar$end[case_l_ar$end > max(plt_df$end)] <- max(plt_df$end)
    case_l_ar$start[case_l_ar$end < min(plt_df$start)] <- min(plt_df$start)
    case_l_ar$no_ar <- as.numeric(case_l_ar$nl_s) == 0 & as.numeric(case_l_ar$nl_e) == 0
    case_l_ar$lab_y[case_l_ar$no_ar] <- case_l_ar$y[case_l_ar$no_ar]
    case_l_ar$nl_l <- paste0(case_l_ar$wind_nm_l, "\n",
                             format(number_line(case_l_ar$nl_s, case_l_ar$nl_e)),
                             " ", ifelse(is_dt, gsub("s$", "-", episode_unit), "unit-"),
                             "difference.")
  }else{
    case_l_ar$no_ar <- logical()
    case_l_ar$nl_l <- character()
  }

  plot_pts <- nrow(plt_df)
  min_x <- min(plt_df$start)
  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = y, yend = y, colour = epid), size = scale_size(c(.1,1), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = start, y = y, colour = epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = end, y = y, colour = epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_segment(ggplot2::aes(x = mid_x, y= y, colour = epid, xend = x_lead, yend = y_lead), alpha = .4) +
    ggplot2::geom_segment(ggplot2::aes(x = start, y= y, xend = end, yend = mid_y_lead, linetype = wind_nm_l), color = "white", alpha= .9, data = case_l_ar[case_l_ar$nl_nm == "len" & !case_l_ar$no_ar & case_l_ar$wind_total > 1,], arrow = ggplot2::arrow(length = ggplot2::unit(scale_size(c(.5,.2), 500, plot_pts),"cm"), ends="last", type = "open")) +
    ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= lab_y, label = nl_l), data = case_l_ar[case_l_ar$nl_nm == "len",], nudge_y = scale_size(c(.02, .06), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), color = "white", alpha= .9, vjust = "bottom")
  if(!isFALSE(show_labels)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = epid, label = event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "bottom", alpha= .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(start) + as.numeric(end))/2, y= y, colour = epid, label = event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,5), 500, plot_pts), vjust = "top", alpha= .7)
  }
  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y= 2.15, label = title), colour = "white", size = 5)
  }
  if(isTRUE(show_non_finite)){
    t1 <- plt_df[!plt_df$finite,]
    x1 <- min(t1$x); x2 <- max(t1$x)
    f <- f +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x1), alpha = 1, color = "white", linetype = 3) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(x1) + as.numeric(x2))/2, y = 1), color = "white", label = "Non-finite\nevents)", size = scale_size(c(2,5), 500, plot_pts), alpha= .9, hjust = "middle")
  }
  f <- f + ggplot2::theme(
    legend.position = "none",
    legend.background = ggplot2::element_rect(fill = "black"),
    legend.text = ggplot2::element_text(colour = "white"),
    plot.background = ggplot2::element_rect(fill = "black"),
    panel.background = ggplot2::element_rect(fill = "black"),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )
  f
}
