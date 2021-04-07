#' @name schema
#' @aliases schema
#' @title Schema diagram for linked records in \code{diyar}
#'
#' @description Create schema diagrams for \code{\link[=number_line-class]{number_line}}, \code{\link[=epid-class]{epid}}, \code{\link[=pid-class]{pid}} and \code{\link[=pane-class]{pane}} objects.
#'
#' @param x \code{[\link[=number_line-class]{number_line}|\link[=epid-class]{epid}|\link[=pid-class]{pid}|\link[=pane-class]{pane}]}
#' @param title \code{[character]}. Plot title.
#' @param show_skipped \code{[logical]}. Show/hide \code{"Skipped"} records.
#' @param show_non_finite \code{[logical]}. Show/hide records with non-finite \code{date} values.
#' @param show_labels \code{[logical|character]}. Show/hide certain parts of the schema. See \code{Details}.
#' @param theme \code{[character]}. Options are \code{"dark"} or \code{"light"}.
#' @param orientation \code{[character]}. Show each record of a \code{pid} object within its group id (\code{"by_pid"}) or its \code{pid_cri} (\code{"by_pid_cri"})
#' @param seed \code{[integer]}. See \code{set.seed}. Used to get a consistent arrangement of items in the plot.
#' @param custom_label \code{[character]}. Custom label for each record of the identifier.
#' @param ... Other arguments.
#'
#' @return \code{ggplot} objects
#' @details
#' A visual aid to describing the data linkage (\code{\link{links}}), episode tracking (\code{\link{episodes}}) or partitioning process (\code{\link{partitions}}).
#'
#' @examples
#' schema(number_line(c(1, 2), c(2, 1)))
#'
#' schema(episodes(1:10, 2))
#'
#' schema(partitions(1:10, by = 2, separate = TRUE))
#'
#' schema(links(list(c(1, 1, NA, NA), c(NA, 1, 1, NA))))
#'
#' @export
schema <- function(x, ...) UseMethod("schema")

#' @rdname schema
#' @importFrom rlang .data
#' @export
schema.number_line <- function(x, show_labels = c("date", "case_overlap_methods"), ...){
  if("case_overlap_methods" %in% show_labels){
    x <- episodes(date = x,
                  case_length = index_window(x))
  }else{
    x <- episodes(date = x,
                  case_length = index_window(x),
                  strata = seq_len(length(x)))
  }

  f <- schema.epid(x, show_labels = c("date", "case_overlap_methods"), ...)
  return(f)
}

#' @rdname schema
#' @importFrom rlang .data
#' @export
schema.epid <- function(x, title = NULL, show_labels = c("length_arrow"),
                        show_skipped = TRUE, show_non_finite = FALSE,
                        theme = "dark", seed = NULL, custom_label = NULL, ...){
  . <- NULL
  # Validations
  errs <- err_schema_epid_0(x = x,
                            date = x@options$date,
                            case_length = x@options$case_length,
                            recurrence_length = x@options$recurrence_length,
                            episode_unit = as.vector(decode(x@options$episode_unit)),
                            from_last = x@options$from_last,
                            title = title,
                            show_labels = show_labels,
                            show_skipped = show_skipped,
                            show_non_finite = show_non_finite,
                            theme = theme)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(!is.null(seed)) set.seed(seed)
  # Standardise inputs
  # `date`
  int <- as.number_line(x@options$date)
  is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  if(isTRUE(is_dt)){
    int <- number_line(
      l = as.POSIXct(int@start),
      r = as.POSIXct(right_point(int))
    )
  }
  # `episode_unit`
  episode_unit <- x@options$episode_unit
  episode_unit[!is_dt] <- 1
  # `case_length`
  case_length <- x@options$case_length
  case_length <- lapply(case_length, function(x){
    if(length(x) == 1){
      rep(x, length(int))
    }else{
      x
    }
  })
  ep_l <- length_to_range(lengths = case_length,
                          date = int,
                          from_last = x@options$from_last,
                          episode_unit = as.vector(episode_unit))
  any_rolling <- any(x@wind_nm == 1)
  if(any_rolling){
    recurrence_length <- x@options$recurrence_length
    # `recurrence_length`
    recurrence_length <- lapply(recurrence_length, function(x){
      if(length(x) == 1){
        rep(x, length(int))
      }else{
        x
      }
    })
    rc_l <- length_to_range(lengths = recurrence_length,
                            date = int,
                            from_last = x@options$from_last,
                            episode_unit = as.vector(episode_unit))
  }else{
    rc_l <- NULL
  }

  # `epid` data
  plt_df <- as.data.frame(x)
  if(!is.null(custom_label)){
    plt_df$custom_label <- custom_label
  }
  plt_df$wind_id <- x@wind_id[[1]]

  # Show skipped records
  if(isFALSE(show_skipped)){
    lgk <- plt_df$case_nm != -1
    int <- int[lgk]
    plt_df <- plt_df[plt_df$case_nm != -1,]
    ep_l <- lapply(ep_l, function(x) x[lgk])
    if(any_rolling){
      rc_l <- lapply(rc_l, function(x) x[lgk])
    }

  }
  plt_df$start <- left_point(int)
  plt_df$end <- right_point(int)
  plt_df$start_l <- left_point(as.number_line(x@options$date))
  plt_df$end_l <- right_point(as.number_line(x@options$date))
  plt_df$epid <- as.character(plt_df$epid)

  # Data points without finite coordinates.
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  windows_dy <- sort(plt_df$wind_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$wind_id, windows_dy$values)]

  lgk <- which(plt_df$epid_total != 1)
  dts_a <- lapply(split(as.numeric(plt_df$start[lgk]), plt_df$epid[lgk]), min)
  dts_z <- lapply(split(as.numeric(plt_df$end[lgk]), plt_df$epid[lgk]), max)

  plt_df$epid_dts_a <- as.numeric(plt_df$start)
  plt_df$epid_dts_z <- as.numeric(plt_df$end)
  plt_df$epid_dts_a[lgk] <- as.numeric(dts_a)[match(plt_df$epid[lgk], names(dts_a))]
  plt_df$epid_dts_z[lgk] <- as.numeric(dts_z)[match(plt_df$epid[lgk], names(dts_z))]

  plt_df$di_pid <- episodes(date = number_line(plt_df$epid_dts_a, plt_df$epid_dts_z),
                            case_length = index_window(number_line(plt_df$epid_dts_a, plt_df$epid_dts_z)),
                            display = "none")
  bd_id <- split(plt_df$epid, as.numeric(plt_df$di_pid))
  bd_id_sn <- split(plt_df$sn, as.numeric(plt_df$di_pid))
  bd_id <- lapply(bd_id, function(x){
    match(x, x[!duplicated(x)])
  })
  bd_id_sn <- unlist(bd_id_sn, use.names = FALSE)
  bd_id <- unlist(bd_id, use.names = FALSE)
  plt_df$bd_id <- bd_id[match(plt_df$sn, bd_id_sn)]

  # Alternating boundaries for separate `windows`
  unq_bd_id <- plt_df$bd_id[!duplicated(plt_df$bd_id)]
  bds <- number_line_sequence(number_line(0, 2), length.out = length(unq_bd_id))
  # Alternating boundaries (y-axis) for each window

  wind_br_a <- bds@start + (bds@.Data/32)
  wind_br_z <- right_point(bds) - (bds@.Data/32)
  winds_sn <- split(plt_df$sn, plt_df$bd_id)

  # Random `y` coordinates within each window's boundary (above)
  cord_y <- lapply(seq_len(length(wind_br_a)), function(i){
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
  winds_cord_y <- split(plt_df$y, plt_df$bd_id)

  # Midpoint of each window's boundaries (y-axis)
  mid_y <- lapply(winds_cord_y, function(x){
    rep(mean(x), length(x))
  })
  mid_y <- unlist(mid_y, use.names = FALSE)
  plt_df$mid_y <- mid_y[match(plt_df$sn, winds_sn)]

  # `dates`
  plt_df$start <- as.numeric(plt_df$start)
  plt_df$end <- as.numeric(plt_df$end)

  # `episode_unit` used
  plt_df$episode_unit <- episode_unit

  # Information to show in the plot
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "epid", "date", "case_nm", "length_label",
                     "length_arrow", "case_overlap_methods","recurrece_overlap_methods")
  }

  # Case length arrows
  case_l_ar <- lapply(x@wind_id, function(x){
    sw <- which(plt_df$wind_id != x & !is.na(x))
    plt_df$wind_id[sw] <- x[sw]
    plt_df
    l_ar(ep_l, plt_df, "Case", is_dt)
  })
  case_l_ar <- unlist(case_l_ar, recursive = FALSE)
  case_l_ar <- case_l_ar[!duplicated(case_l_ar)]

  if(any_rolling == T){
    # Recurrence length arrows
    rc_l_ar <- lapply(x@wind_id, function(x){
      sw <- which(plt_df$wind_id != x & !is.na(x))
      plt_df$wind_id[sw] <- x[sw]
      plt_df
      l_ar(rc_l, plt_df, "Recurrence", is_dt)
    })
    rc_l_ar <- unlist(rc_l_ar, recursive = FALSE)
    rc_l_ar <- rc_l_ar[!duplicated(rc_l_ar)]
    case_l_ar <- c(case_l_ar, rc_l_ar)
  }
  case_l_ar <- do.call("rbind", case_l_ar)

  any_finite <- length(plt_df$start[plt_df$finite]) > 0
  if(any_finite){
    breaks <- seq(min(as.numeric(plt_df$start[plt_df$finite])), max(as.numeric(plt_df$start[plt_df$finite])), length.out = 10)
    labels <- seq(min(plt_df$start[plt_df$finite]), max(plt_df$start[plt_df$finite]), length.out = 10)

    if(is_dt == TRUE){
      # Sensible labels for time points. Based on `episode_unit`
      if(min(episode_unit[[1]]) >= 4){
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
  plt_df$event_type <- ""
  plt_df$event_nm <- ""
  # Show `epid_id` if requested
  if("epid" %in% show_labels){
    plt_df$event_type <- paste0("E.", plt_df$epid)
  }
  # Show `case_nm` if requested
  if("case_nm" %in% show_labels){
    plt_df$event_type <- paste0(plt_df$event_type, ifelse(plt_df$event_type == "", "", "\n"),
                                decode(plt_df$case_nm),
                                ifelse(plt_df$sn == plt_df$wind_id & plt_df$case_nm != -1,
                                       "\n(reference)",""))
  }
  # Show `date` if requested
  if("date" %in% show_labels){
    plt_df$event_nm <- number_line(plt_df$start_l, plt_df$end_l)
    plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                              format(left_point(plt_df$event_nm)),
                              format(plt_df$event_nm))
  }
  # Show record `custom_label` if requested
  if(!is.null(custom_label)){
    plt_df$event_nm <- paste0(plt_df$custom_label, " ", plt_df$event_nm)
  }
  # Show record `sn` if requested
  if("sn" %in% show_labels){
    plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ", plt_df$event_nm)
  }

  # Show non-finite dates if requested
  if(isTRUE(show_non_finite)){
    # Add non-finite `dates` to plot.
    plt_df$start[!plt_df$finite] <- sample(seq(max(breaks) + (int * 1),
                                               max(breaks) + (int * 3),
                                               length.out = length(plt_df$start[!plt_df$finite])),
                                           length(plt_df$start[!plt_df$finite]))

    plt_df$end[!plt_df$finite] <- plt_df$start[!plt_df$finite]
    breaks2 <- seq(max(breaks), int, length.out = 3)
    labels2 <- rep("", 3)
    labels2[floor(mean(seq_len(length(labels2))))] <- "Unknown"
  }else{
    breaks2 <- breaks[0]
    labels2 <- labels[0]
    plt_df <- plt_df[plt_df$finite,]
  }

  # Mid point of `date` to show links
  plt_df$mid_x <- (as.numeric(plt_df$start) + as.numeric( plt_df$end))/2
  # Link between records and their index
  plt_df <- lapply(1:length(x@wind_id), function(i){
    sw <- which(plt_df$wind_id != x@wind_id[[i]] & !is.na(x@wind_id[[i]]))
    plt_df$wind_id[sw] <- x@wind_id[[i]][sw]
    link_sn <- plt_df[plt_df$sn %in% plt_df$wind_id, c("sn", "mid_x", "y")]
    plt_df$x_lead <- link_sn$mid_x[match(plt_df$wind_id, link_sn$sn)]
    plt_df$y_lead <- link_sn$y[match(plt_df$wind_id, link_sn$sn)]
    plt_df$sn_lead <- link_sn$sn[match(plt_df$wind_id, link_sn$sn)]
    df_cols <- c("sn", "start", "end", "y", "epid", "y_lead", "x_lead", "mid_x","sn_lead", "finite")
    if(!isFALSE(show_labels) | !is.null(custom_label)) {
      df_cols <- c(df_cols, "event_nm", "event_type")
    }
    plt_df <- plt_df[df_cols]
    if(i > 1){
      plt_df <- plt_df[sw,]
    }
    plt_df
  })

  plt_df <- do.call("rbind", plt_df)

  if(nrow(case_l_ar) > 0){
    case_l_ar$start <- as.numeric(case_l_ar$start)
    case_l_ar$end <- as.numeric(case_l_ar$end)

    case_l_ar$pl_x_e <- max(plt_df$end, plt_df$start)
    case_l_ar$pl_x_s <- min(plt_df$start, plt_df$end)

    lgk <- ((case_l_ar$end > case_l_ar$pl_x_e) | is.infinite(case_l_ar$end))
    case_l_ar$end[lgk] <- case_l_ar$pl_x_e[lgk]
    lgk <- ((case_l_ar$start < case_l_ar$pl_x_s) | is.infinite(case_l_ar$start))
    case_l_ar$start[lgk] <- case_l_ar$pl_x_s[lgk]

    case_l_ar$bck_dir <- abs(case_l_ar$nl_s) > abs(case_l_ar$nl_e)
    rev_len <- reverse_number_line(number_line(case_l_ar$start[case_l_ar$bck_dir], case_l_ar$end[case_l_ar$bck_dir]), direction = "both")
    case_l_ar$start[case_l_ar$bck_dir] <- rev_len@start
    case_l_ar$end[case_l_ar$bck_dir] <- right_point(rev_len)

    case_l_ar$lab_y[case_l_ar$no_ar] <- case_l_ar$y[case_l_ar$no_ar]
    case_l_ar$nl_l <- paste0(case_l_ar$wind_nm_l, "\n",
                             format(number_line(case_l_ar$nl_s, case_l_ar$nl_e)),
                             " ", ifelse(is_dt, gsub("s$", "-", names(diyar::episode_unit)[case_l_ar$episode_unit]), "unit-"),
                             "difference.")
  }else{
    case_l_ar$no_ar <- logical()
    case_l_ar$nl_l <- character()
  }

  plot_pts <- nrow(plt_df)
  min_x <- min(c(plt_df$start, plt_df$end, case_l_ar$start, case_l_ar$end))

  if(theme == "dark"){
    bg_col <- "black"
    txt_col <- "white"
  }else{
    bg_col <- "white"
    txt_col <- "black"
  }

  plt_df$overlap_method <- ""
  if(nrow(case_l_ar) > 0 & ("case_overlap_methods" %in% show_labels | "recurrence_overlap_methods" %in% show_labels)){
    rep_lgk <- match(plt_df$sn_lead, case_l_ar$pt_sn)
    plt_df$lead_dt_a <- plt_df$start_rl[rep_lgk]
    plt_df$lead_dt_z <- plt_df$end_rl[rep_lgk]
    if("case_overlap_methods" %in% show_labels){
      rep_lgk <- which(plt_df$sn != plt_df$wind_id$wind_id1 & plt_df$wind_nm_l == "Case length")
      if(length(rep_lgk) > 0){
        plt_df$overlap_method[rep_lgk] <- overlap_method(number_line(plt_df$start[rep_lgk],
                                                                     plt_df$end[rep_lgk]),
                                                         number_line(plt_df$lead_dt_a[rep_lgk],
                                                                     plt_df$lead_dt_z[rep_lgk]))
      }
    }
    if("recurrence_overlap_methods" %in% show_labels){
      rep_lgk <- which(plt_df$sn != plt_df$wind_id$wind_id1 & plt_df$wind_nm_l == "Recurrence length")
      if(length(rep_lgk) > 0){
        plt_df$overlap_method[rep_lgk] <- overlap_method(number_line(plt_df$start[rep_lgk],
                                                                     plt_df$end[rep_lgk]),
                                                         number_line(plt_df$lead_dt_a[rep_lgk],
                                                                     plt_df$lead_dt_z[rep_lgk]))
      }
    }
  }


  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$start, xend = .data$end, y = .data$y, yend = .data$y, colour = .data$epid), size = scale_size(c(.1,1), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = .data$start, y = .data$y, colour = .data$epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_point(ggplot2::aes(x = .data$end, y = .data$y, colour = .data$epid), size = scale_size(c(1,3), 500, plot_pts), alpha= .7) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$x_lead, y = .data$y_lead, colour = .data$epid, xend = .data$mid_x, yend = .data$y), alpha = .4)
  if(!isFALSE(show_labels) | !is.null(custom_label)){
    if(("case_overlap_methods" %in% show_labels | "recurrence_overlap_methods" %in% show_labels)){
      f <- f + ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$x_lead) + as.numeric(.data$mid_x))/2, y = (as.numeric(.data$y) + as.numeric(.data$y_lead))/2, label = .data$overlap_method, colour = .data$epid), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), vjust = "bottom", alpha= .7)
    }
    if("length_arrow" %in% show_labels){
      f <- f + ggplot2::geom_segment(ggplot2::aes(x = .data$start, y = .data$y, xend = .data$end, yend = .data$y), linetype = "solid", color = txt_col, alpha= .9, data = case_l_ar[case_l_ar$wind_nm_l == "Case length" & case_l_ar$epid_total > 1 & case_l_ar$nl_s != case_l_ar$nl_e,], arrow = ggplot2::arrow(length = ggplot2::unit(scale_size(c(.5,.2), 500, plot_pts),"cm"), ends = "last", type = "open")) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$start, y = .data$y, xend = .data$end, yend = .data$y), linetype = "dashed", color = txt_col, alpha= .9, data = case_l_ar[case_l_ar$wind_nm_l == "Recurrence length" & case_l_ar$epid_total > 1 & case_l_ar$nl_s != case_l_ar$nl_e,], arrow = ggplot2::arrow(length = ggplot2::unit(scale_size(c(.5,.2), 500, plot_pts),"cm"), ends = "last", type = "open")) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$pt_end, y = .data$y, xend = .data$start, yend = .data$y), linetype = "dotted", color = txt_col, alpha= .9, data = case_l_ar[case_l_ar$epid_total > 1 & case_l_ar$start != 0 & case_l_ar$end != 0,])
    }
    if("length_label" %in% show_labels){
      f <- f + ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y= .data$y, label = .data$nl_l), data = case_l_ar[case_l_ar$nl_nm == "len" & case_l_ar$epid_total > 1,], nudge_y = scale_size(c(.02, .06), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), color = txt_col, alpha= .9, vjust = "bottom")
    }
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y = .data$y, colour = .data$epid, label = .data$event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), vjust = "bottom", alpha= .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y = .data$y, colour = .data$epid, label = .data$event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), vjust = "top", alpha= .7)
  }
  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y= 2.15, label = title), colour = txt_col, size = 5)
  }
  if(isTRUE(show_non_finite)){
    t1 <- plt_df[!plt_df$finite,]
    x1 <- min(t1$x); x2 <- max(t1$x)
    f <- f +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x1), alpha = 1, color = txt_col, linetype = 3) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(x1) + as.numeric(x2))/2, y = 1), color = txt_col, label = "Non-finite\nevents)", size = scale_size(c(2,5), 500, plot_pts), alpha= .9, hjust = "middle")
  }
  f <- f + ggplot2::theme(
    legend.position = "none",
    legend.background = ggplot2::element_rect(fill = bg_col),
    legend.text = ggplot2::element_text(colour = txt_col),
    plot.background = ggplot2::element_rect(fill = bg_col),
    panel.background = ggplot2::element_rect(fill = bg_col),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank()
  )
  return(f)
}

#' @rdname schema
#' @importFrom rlang .data
#' @export
schema.pane <- function(x, title = NULL, show_labels = c("window_label"),
                        theme = "dark", seed = NULL, custom_label = NULL, ...) {
  . <- NULL

  # Validations
  errs <- err_schema_pane_0(x = x,
                            date = x@options$date,
                            title = title,
                            show_labels = show_labels,
                            theme = theme)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  if(!is.null(seed)) set.seed(seed)

  # `Pane` data
  panes <- x
  if(!is.null(custom_label)){
    plt_df$custom_label <- custom_label
  }
  plt_df <- as.data.frame(panes)
  plt_df$start <- as.number_line(x@options$date)@start
  plt_df$end <- right_point(as.number_line(x@options$date))
  plt_df$epid <- as.character(panes@.Data)
  plt_df$pane_id <- panes@.Data
  plt_df$case_nm <- decode(panes@case_nm)

  # Data points without finite coordinates.
  plt_df$finite <- !is.na(plt_df$start) & !is.na(plt_df$end)

  windows_dy <- sort(plt_df$pane_id)
  windows_dy <- rle(windows_dy)
  plt_df$wind_total <- windows_dy$length[match(plt_df$pane_id, windows_dy$values)]

  # Colour code for each window
  if(isTRUE(x@options$separate)){
    plt_df$pane_n <- as.character(plt_df$window_matched)
  }else{
    plt_df$pane_n <- "1"
  }

  # Same colour for non-matches
  plt_df$pane_n[plt_df$window_matched == 0] <- -seq_len(length(plt_df$pane_n[plt_df$window_matched == 0]))
  plt_df$pane_n <- as.character(plt_df$pane_n)

  # `windows`
  splits_windows <- x@window_list
  splits_windows <- splits_windows[!duplicated(splits_windows)]

  border <- do.call(rbind, lapply(splits_windows, function(x){
    x <- as.data.frame(x)
    x$pane_n <- as.character(seq_len(nrow(x)))
    x
  }))

  if(isFALSE(x@options$separate)){
    border$pane_n <- "1"
  }

  border$y2 <- 2.05
  border$y1 <- 0.00

  # Information to show in the plot
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "pane", "date", "case_nm", "window_label")
  }
  # If show `window_label` is requested
  if("window_label" %in% show_labels){
    border$win_l <- format(number_line(border$start, border$end))
  }else{
    border$win_l <- ""
  }

  # Identify points that will overlap on the plot
  lgk <- which(plt_df$pane_total != 1)
  dts_a <- lapply(split(as.numeric(plt_df$start[lgk]), plt_df$epid[lgk]), min)
  dts_z <- lapply(split(as.numeric(plt_df$end[lgk]), plt_df$epid[lgk]), max)

  plt_df$epid_dts_a <- as.numeric(plt_df$start)
  plt_df$epid_dts_z <- as.numeric(plt_df$end)
  plt_df$epid_dts_a[lgk] <- as.numeric(dts_a)[match(plt_df$epid[lgk], names(dts_a))]
  plt_df$epid_dts_z[lgk] <- as.numeric(dts_z)[match(plt_df$epid[lgk], names(dts_z))]

  plt_df$di_pid <- episodes(date = number_line(plt_df$epid_dts_a, plt_df$epid_dts_z),
                            strata = plt_df$pane_n,
                            case_length = index_window(number_line(plt_df$epid_dts_a, plt_df$epid_dts_z)),
                            display = "none")
  bd_id <- split(plt_df$epid, as.numeric(plt_df$di_pid))
  bd_id_sn <- split(plt_df$sn, as.numeric(plt_df$di_pid))
  bd_id <- lapply(bd_id, function(x){
    match(x, x[!duplicated(x)])
  })
  bd_id_sn <- unlist(bd_id_sn, use.names = FALSE)
  bd_id <- unlist(bd_id, use.names = FALSE)
  plt_df$bd_id <- bd_id[match(plt_df$sn, bd_id_sn)]


  # Set alternating boundaries for separate `windows`
  unq_bd_id <- plt_df$bd_id[!duplicated(plt_df$bd_id)]
  bds <- number_line_sequence(number_line(0, 2), length.out = length(unq_bd_id))

  wind_br_a <- bds@start + (bds@.Data/32)
  wind_br_z <- right_point(bds) - (bds@.Data/32)
  winds_sn <- split(plt_df$sn, plt_df$bd_id)

  # Random `y` coordinates within each window's boundary (above)
  cord_y <- lapply(seq_len(length(wind_br_a)), function(i){
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

  # Mid point of date` to show links
  plt_df$mid_x <- (as.numeric(plt_df$start) + as.numeric(plt_df$end))/2
  # Link between records and their index
  link_sn <- plt_df[plt_df$sn %in% plt_df$pane_id, c("sn", "mid_x", "y")]
  plt_df$x_lead <- link_sn$mid_x[match(plt_df$pane_id, link_sn$sn)]
  plt_df$y_lead <- link_sn$y[match(plt_df$pane_id, link_sn$sn)]

    plt_df$event_type <- ""
    plt_df$event_nm <- ""
    # Show `pane_id` if requested
    if("pane" %in% show_labels){
      plt_df$event_type <- paste0("PN.", plt_df$epid)
    }
    # Show `case_nm` if requested
    if("case_nm" %in% show_labels){
      plt_df$event_type <- paste0(plt_df$event_type, ifelse(plt_df$event_type == "", "", "\n"),
                                  plt_df$case_nm,
                                  ifelse(plt_df$sn %in% plt_df$pane_id & plt_df$case_nm != -1,
                                         "\n(reference)",""))
    }
    # Show record `date` if requested
    if("date" %in%  show_labels){
      plt_df$event_nm <- number_line(plt_df$start,
                                     plt_df$end)
      plt_df$event_nm <- ifelse(left_point(plt_df$event_nm) == right_point(plt_df$event_nm),
                                format(left_point(plt_df$event_nm)),
                                format(plt_df$event_nm))
    }
    # Show record `custom_label` if requested
    if(!is.null(custom_label)){
      plt_df$event_nm <- paste0(plt_df$custom_label, " ",
                                    plt_df$event_nm)
    }
    # Show record `sn` if requested
    if("sn" %in%  show_labels){
      plt_df$event_nm <- paste0("SN ", plt_df$sn, "; ",
                                plt_df$event_nm)
    }

  # Can't plot `Inf'/`-Inf`, so close infinite `window`
  r_lim <- c(border$end, plt_df$end)
  r_lim <- r_lim[!is.infinite(r_lim)]
  l_lim <- c(border$start, plt_df$start)
  l_lim <- l_lim[!is.infinite(l_lim)]

  border$end[is.infinite(border$end)] <- max(r_lim)
  border$start[is.infinite(border$start)] <- min(l_lim)

  # Can't plot records with missing `dates`
  plt_df <- plt_df[plt_df$finite,]
  plt_df$start <- as.numeric(plt_df$start)
  plt_df$end <- as.numeric(plt_df$end)
  plot_pts <- nrow(plt_df)
  min_x <- min(c(plt_df$start, border$start))

  if(theme == "dark"){
    bg_col <- "black"
    txt_col <- "white"
  }else{
    bg_col <- "white"
    txt_col <- "black"
  }

  plt_df$epid <- match(plt_df$epid, plt_df$epid[!duplicated(plt_df$epid)])
  plt_df$epid <- formatC(plt_df$epid, width = nchar(max(plt_df$epid)), flag = 0, format = "fg")

  q <- border$pane_n[!duplicated(border$pane_n)]
  border$pane_n <- match(border$pane_n, sample(q, length(q)))
  border$pane_n <- formatC(border$pane_n, width = nchar(max(border$pane_n)), flag = 0, format = "fg")
  border$start <- as.numeric(border$start)
  border$end <- as.numeric(border$end)

  f <- ggplot2::ggplot(data = plt_df) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$start, xend = .data$end, y = .data$y, yend = .data$y, colour = .data$epid), size = scale_size(c(.1,1), 500, plot_pts), alpha = .7) +
    ggplot2::geom_point(ggplot2::aes(x = .data$start, y = .data$y, color = .data$epid), size = scale_size(c(1,3), 500, plot_pts), alpha = .7) +
    ggplot2::geom_point(ggplot2::aes(x = .data$end, y = .data$y, color = .data$epid), size = scale_size(c(1,3), 500, plot_pts), alpha = .7) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$mid_x, y= .data$y, colour = .data$epid, xend = .data$x_lead, yend = .data$y_lead), alpha = .4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$start, xmax = .data$end, ymin = .data$y1, ymax = .data$y2, fill = .data$pane_n), data = border, alpha = .2) +
    ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y= .data$y2, label = .data$win_l), color = txt_col, data = border, nudge_y = .05, size = 5)
  if(!isFALSE(show_labels) | !is.null(custom_label)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y= .data$y, colour = .data$epid, label = .data$event_nm), nudge_y = scale_size(c(.01, .02), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), vjust = "bottom", alpha = .7) +
      ggplot2::geom_text(ggplot2::aes(x = (as.numeric(.data$start) + as.numeric(.data$end))/2, y= .data$y, colour = .data$epid, label = .data$event_type), nudge_y = -scale_size(c(0, .01), 500, plot_pts), size = scale_size(c(2,4), 500, plot_pts), vjust = "top", alpha = .7)
  }
  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = min_x, y = 2.2, label = title), colour = txt_col, size = 5)
  }
  f <- f +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  return(f)
}

#' @rdname schema
#' @importFrom rlang .data
#' @export
schema.pid <- function(x, title = NULL, show_labels = TRUE,
                       theme = "dark", orientation = "by_pid",
                       seed = NULL, custom_label = NULL, ...){
  . <- NULL

  # Validations
  errs <- err_schema_pid_0(x = x,
                           title = title,
                           show_labels = show_labels,
                           theme = theme,
                           orientation = orientation)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  if(!is.null(seed)) set.seed(seed)
  # `Data to plot`pid` data
  pl_dt <- as.data.frame(x)
  if(!is.null(custom_label)){
    pl_dt$custom_label <- custom_label
  }
  if(orientation == "by_pid"){
    pl_dt$link_col <- paste0("P. ", pl_dt$pid)
    pl_dt$pid_box_cri <- ifelse(pl_dt$pid_cri == 0, "No Hits", ifelse(pl_dt$pid_cri == -1, "Skipped", paste0("CRI ", pl_dt$pid_cri)))
  }else if(orientation == "by_pid_cri"){
    pl_dt$pid_box_cri <- ifelse(pl_dt$pid_cri == 0, "No Hits", ifelse(pl_dt$pid_cri == -1, "Skipped", paste0("P. ", pl_dt$pid)))
    pl_dt$link_col <- ifelse(pl_dt$pid_cri %in% -1:0, "", paste0("CRI ", pl_dt$pid_cri))
  }

  cris <- pl_dt$pid_box_cri
  cris <- cris[!duplicated(cris)]
  cris <- length(cris)

  # N-cycle of pid_cri boxes
  order <- ceiling(cris / 8)

  # pid_cri boxes
  boxes_w <- 10
  border <-  lapply(seq_len(order), function(x) box_ring(order = x, boxes_w = boxes_w))
  border <- do.call(rbind, c(list(box(boxes_w)), border))
  border <- head(border, cris)
  mx <- boxes_w * order
  border$pid_box <- seq_len(nrow(border))

  pl_dt$pid_box <- match(pl_dt$pid_box_cri,  pl_dt$pid_box_cri[!duplicated(pl_dt$pid_box_cri)] )

  # Boundaries for pid_cri boxes
  pl_dt$x1 <- border$x1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$x2 <- border$x2[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y1 <- border$y1[match(pl_dt$pid_box, border$pid_box)]
  pl_dt$y2 <- border$y2[match(pl_dt$pid_box, border$pid_box)]

  # Random x & y coordinates
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

  # Link between records and their index
  link_sn <- pl_dt[pl_dt$sn %in% pl_dt$link_id, c("sn", "x", "y")]
  pl_dt$x_lead <- link_sn$x[match(pl_dt$link_id, link_sn$sn)]
  pl_dt$y_lead <- link_sn$y[match(pl_dt$link_id, link_sn$sn)]

  pl_dt$pid <- as.character(pl_dt$pid)
  if(isTRUE(show_labels)){
    show_labels <- c("sn", "pid")
  }

  # Labels to plot
    pl_dt$event_nm <- ""
    pl_dt$pid_l <- ""
    # Show record `custom_label` if requested
    if(!is.null(custom_label)){
      pl_dt$event_nm <- paste0(pl_dt$custom_label, " ", pl_dt$event_nm)
    }
    # Show record `sn` is requested
    if("sn" %in% show_labels){
      pl_dt$event_nm <- paste0("SN ", pl_dt$sn, "; ", pl_dt$event_nm)
    }
    # Show record `pid` is requested
    if("pid" %in% show_labels){
      pl_dt$pid_l <- pl_dt$link_col
    }

  border$pid_box_cri <- pl_dt$pid_box_cri[match(border$pid_box, pl_dt$pid_box)]
  tx_l <- min(pl_dt$x1)
  tx_h <- max(pl_dt$y2)
  boxes_n <- nrow(border)

  if(theme == "dark"){
    bg_col <- "black"
    txt_col <- "white"
  }else{
    bg_col <- "white"
    txt_col <- "black"
  }

  f <- ggplot2::ggplot(data = pl_dt) +
    ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, colour = .data$link_col), size = scale_size(c(1,3), 125, boxes_n), alpha = .7 + ifelse(theme != "dark", .1, 0)) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$x, y = .data$y, colour = .data$link_col, xend = .data$x_lead, yend = .data$y_lead), alpha = .4 + ifelse(theme != "dark", .1, 0)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$x1, xmax = .data$x2, ymin = .data$y1, ymax = .data$y2,  fill = .data$pid_box_cri), data = border, alpha = .1 + ifelse(theme != "dark", .1, 0)) +
    ggplot2::geom_text(ggplot2::aes(x = (.data$x1 + .data$x2)/2, y = (.data$y1 + .data$y2)/2, label = .data$pid_box_cri), size = scale_size(c(9, 30), 125, boxes_n), color = txt_col, alpha = scale_size(c(.1, .2), 125, boxes_n, decreasing = FALSE), data = border)
  if(!isFALSE(show_labels) | !is.null(custom_label)){
    f <- f +
      ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, colour = .data$link_col, label = .data$event_nm), nudge_y = scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "bottom", size = scale_size(c(2,4), 125, boxes_n), alpha = .7) +
      ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, colour = .data$link_col, label = .data$pid_l), nudge_y = -scale_size(c(.12, .3), 125, boxes_n, decreasing = FALSE), vjust = "top", size = scale_size(c(2,4), 125, boxes_n), alpha = .7)
  }

  if(!is.null(title)){
    txt_y <- max(c(border$y1, border$y2)) + 2
    txt_x <- min(c(border$x1, border$x2))

    f <- f + ggplot2::geom_text(ggplot2::aes(x = txt_x, y = txt_y), colour = txt_col, label = title, size = 5)
  }
  f <- f +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = bg_col),
      panel.background = ggplot2::element_rect(fill = bg_col),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
  return(f)
}
