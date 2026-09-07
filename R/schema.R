#' @name schema
#' @aliases schema
#' @title Schema diagram for group identifiers
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
#' A visual aid to describe the data linkage (\code{\link{links}}), episode tracking (\code{\link{episodes}}) or partitioning process (\code{\link{partitions}}).
#'
#' \bold{\code{show_labels} options (multi-select)}
#' \itemize{
#' \item schema.epid - \bold{TRUE}, \bold{FALSE}, "sn", "epid", "date", "case_nm", "wind_nm", "length", "length_arrow", "case_overlap_methods" or "recurrence_overlap_methods"
#' \item schema.pane - \bold{TRUE}, \bold{FALSE}, "sn", "pane", "date", "case_nm" or "window_label"
#' \item schema.pid - \bold{TRUE}, \bold{FALSE}, "sn" or "pid"
#' }
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
schema <- function(x, ...){
  UseMethod("schema")
}
#' @rdname schema
#' @param custom_sort \code{[atomic]}. Preferential order for selecting index events. See \code{\link{custom_sort}}.
#' @export
schema.number_line <- function(
    x, show_labels = c("date", "case_overlap_methods"),
    custom_sort = NULL, ...){
  x <- episodes(
    date = x, case_length = index_window(x),
    custom_sort = custom_sort)

  f <- schema.epid(x, show_labels = show_labels, ...)
  return(f)
}

#' @rdname schema
#' @export
schema.epid <- function(
    x, title = NULL, show_labels = c("length_arrow"),
    show_skipped = TRUE, show_non_finite = FALSE, plot_scale = 'fit.screen',
    theme = "dark", seed = NULL, custom_label = NULL, ...){
  # Validations
  errs <- err_schema_epid_0(
    x = x, date = x@options$date, case_length = x@options$case_length,
    recurrence_length = x@options$recurrence_length,
    episode_unit = as.vector(decode(x@options$episode_unit)),
    from_last = x@options$from_last, title = title, show_labels = show_labels,
    show_skipped = show_skipped, show_non_finite = show_non_finite, theme = theme)

  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(!is.null(seed)) set.seed(seed)

  theme_color <- ifelse(theme == 'light', 'white', 'black')
  contrast_color <- ifelse(theme == 'light', 'black', 'white')
  if(isTRUE(show_labels)){
    show_labels <-
      c("sn", "epid", "date", "case_nm", "wind_nm", "length",
        "length_arrow", "case_overlap_methods","recurrence_overlap_methods")
  }

  # Standardise inputs
  # `date`
  int <- as.number_line(x@options$date)
  is_dt <- any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt"))
  if(isTRUE(is_dt)){
    int <- number_line(
      l = as.POSIXct(int@start, tz = 'GMT'),
      r = as.POSIXct(right_point(int), tz = 'GMT'))
  }
  # `episode_unit`
  episode_unit <- as.vector(x@options$episode_unit)
  episode_unit[!is_dt] <- 1
  # `case_length`

  if(any(c("case_overlap_methods","recurrence_overlap_methods", "length", "length_arrow") %in% show_labels)){
    ep_l <- length_to_range(
      x = x@options$case_length, date = int, from_last = x@options$from_last,
      episode_unit = episode_unit, output = 'window', skip_if_b4_lengths = TRUE)

    any_rolling <- any(sapply(x@wind_nm, function(x) any(x == 1 & !is.na(x))))
    if(any_rolling){
      # `recurrence_length`
      rc_l <- length_to_range(
        x = x@options$recurrence_length, date = int,from_last = x@options$from_last,
        episode_unit = episode_unit, output = 'window', skip_if_b4_lengths = TRUE)
    }else{
      rc_l <- NULL
    }
  }else{
    any_rolling <- FALSE
  }

  dfr <- as.data.frame(x = int)
  dfr <- cbind(dfr, as.data.frame(x))
  dfr$from_last <- x@options$from_last
  dfr$episode_unit <- episode_unit

  dfr$event_nm <- dfr$event_type <- ''
  if("epid" %in% show_labels){
    dfr$event_type <- paste0("E.", dfr$epid)
  }

  # Show `case_nm` if requested
  if("case_nm" %in% show_labels){
    dfr$event_type <- paste0(
      dfr$event_type, ifelse(dfr$event_type == "", "", "\n"), decode(dfr$case_nm),
      ifelse(dfr$sn %in% dfr$epid & dfr$case_nm != -1, "\n(reference)",""))
  }

  # Show `date` if requested
  if("date" %in% show_labels){
    dfr$event_nm <- number_line(dfr$start, dfr$end)
    dfr$event_nm <- ifelse(
      dfr$event_nm@.Data == 0, format(left_point(dfr$event_nm)),
      format(dfr$event_nm))
  }
  # Show record `custom_label` if requested
  if(!is.null(custom_label)){
    dfr$event_nm <- paste0(custom_label, " ", dfr$event_nm)
  }
  # Show record `sn` if requested
  if("sn" %in% show_labels){
    dfr$event_nm <- paste0("SN ", dfr$sn, "; ", dfr$event_nm)
  }

  if(!show_skipped){
    dfr <- dfr[decode(dfr$case_nm) != 'Skipped', ]
  }

  epids_n <- length(dfr$epid[!duplicated(dfr$epid)])
  epids_cols <- random_colors(epids_n, alpha = .7, theme = ifelse(theme == 'light', 'dark', 'light'))


  dfr$ep_i <- match(dfr$epid, dfr$sn)
  dfr$ep_ord <- combi(dfr$ep_i, ordered = TRUE)

  tmp.lgk <- is.na(dfr$end) | is.na(dfr$start)
  if(show_non_finite){
    if(all(tmp.lgk)){
      dfr$end[tmp.lgk] <- dfr$start[tmp.lgk] <- 1
    }else{
      dfr$end[tmp.lgk] <- dfr$start[tmp.lgk] <- min(c(dfr$start, dfr$end), na.rm = TRUE)
    }
  }else{
    dfr <- dfr[!tmp.lgk,]
  }

  # Size scaling
  if(plot_scale == 'fit.screen'){
    s.fac <- prod(grDevices::dev.size())/70
  }else if(plot_scale == 'default'){
    s.fac <- 1
  }else if(inherits(plot_scale, 'numeric')){
    s.fac <- abs(1 * plot_scale)
  }

  dfr$dt_n <- bys_count(combi(dfr$start, dfr$end))
  y_scatter_fac <- max(dfr$dt_n)*10
  size.adjust <- 10/nrow(dfr) * s.fac
  nodes.size <- 4*size.adjust
  arrow.head.size <- .3*size.adjust
  text.size <- 2*size.adjust
  edges.size <- 4*size.adjust

  # yadj <- seq(0, 10, length.out = y_scatter_fac)
  # dfr$ycord <- 1 + sample(yadj, nrow(dfr), replace = nrow(dfr) > length(yadj))
  spn <- 10
  yadj <- seq(0, spn, length.out = nrow(dfr) * spn)
  dfr$ycord <- 1 + sample(yadj, nrow(dfr), replace = FALSE)

  dfr$x_gap <- sample(yadj, nrow(dfr), replace = TRUE)/(spn + 1)
  dfr$xcord_end <- dfr$end + dfr$x_gap; dfr$xcord_start <- dfr$start + dfr$x_gap
  dfr$xcord <- ((as.numeric(dfr$start) +  as.numeric(dfr$end))/2) + dfr$x_gap

  # Main plot
  wind_ids <- grep('wind_id', names(dfr), value = T)
  dfr_w <- list()
  for(i in seq_len(length(wind_ids))){
    dfr_w[[i]] <-
      dfr[c('sn', 'start', 'end','xcord', 'xcord_start',
            'xcord_end','ycord', 'ep_ord', 'x_gap', 'epid_total',
            'from_last', 'episode_unit',
            wind_ids[i], gsub('_id', '_nm', wind_ids[i]))]
    names(dfr_w[[i]])[length(dfr_w[[i]])-1] <- 'wind_id'
    names(dfr_w[[i]])[length(dfr_w[[i]])] <- 'wind_nm'
    dfr_w[[i]]$order <- i
  }

  dfr_w <- do.call('rbind', dfr_w)
  dfr_w <- dfr_w[!is.na(dfr_w$wind_id),]
  dfr_w$wi_i <- match(dfr_w$wind_id, dfr_w$sn)
  dfr_w$wi_xcord <- dfr_w$xcord[dfr_w$wi_i]; dfr_w$wi_ycord <- dfr_w$ycord[dfr_w$wi_i]

  # lengths
  # xlim <- as.list(dfr_w[c('start', 'end', 'wi_xcord','xcord', 'xcord_start', 'xcord_end')])
  xlim <- as.list(dfr_w[c('wi_xcord','xcord', 'xcord_start', 'xcord_end')])

  if(is_dt){
    xlim <- lapply(xlim, as.POSIXct)
  }

  xlim <- range(xlim, na.rm = TRUE)
  x_margin <- 0.00001; y_margin <- 0.5
  x_margin <- 0; y_margin <- 0
  xlim[1] <- xlim[1] - x_margin; xlim[2] <- xlim[2] + x_margin
  ylim <- range(as.list(dfr_w[c('wi_ycord', 'ycord')]), na.rm = TRUE)
  ylim[1] <- floor(ylim[1] - (ylim[1] * y_margin)); ylim[2] <- ceiling(ylim[2] + (ylim[2] * y_margin))

  par(bg = theme_color, mar = c(0,0,0,0))

  plot(
    x = dfr$xcord_end, y = dfr$ycord, col = epids_cols[dfr$ep_ord], pch = 16,
    cex = nodes.size, ylab = "", xlab = "", xlim = xlim, ylim = ylim,
    yaxt = "n",  xaxt = "n", bty = "n")

  if(any(c("epid", "case_nm") %in% show_labels)){
    text(dfr$xcord, dfr$ycord, labels = dfr$event_type, pos = 1,
         col = epids_cols[dfr$ep_ord], cex = text.size)
  }

  if(any(c("date", "sn") %in% show_labels | !is.null(custom_label))){
    text(dfr$xcord, dfr$ycord, labels = dfr$event_nm, pos = 3,
         col = epids_cols[dfr$ep_ord], cex = text.size)
  }

  tmp.lgk <- dfr$start != dfr$end
  points(
    x = dfr$xcord_start[tmp.lgk], y = dfr$ycord[tmp.lgk],
    col = epids_cols[dfr$ep_ord[tmp.lgk]], pch = 16, cex = nodes.size)

  segments(
    x0 = dfr_w$xcord_start[tmp.lgk], y0 = dfr_w$ycord[tmp.lgk], x1 = dfr_w$xcord_end[tmp.lgk],
    y1 = dfr_w$ycord[tmp.lgk], col = epids_cols[dfr_w$ep_ord[tmp.lgk]], lwd = edges.size)
  tmp.lgk <- dfr_w$epid_total > 1
  segments(
    x0 = dfr_w$xcord[tmp.lgk], y0 = dfr_w$ycord[tmp.lgk], x1 = dfr_w$wi_xcord[tmp.lgk],
    y1 = dfr_w$wi_ycord[tmp.lgk], col = epids_cols[dfr_w$ep_ord[tmp.lgk]], lwd = edges.size)

  if(any(c('length_arrow', 'length', 'wind_nm', 'case_overlap_methods',
           'recurrence_overlap_methods') %in% show_labels)){
    for (window in c('Case', 'Recurrence')) {
      if(window == 'Recurrence' & !any_rolling) next
      if(window == 'Case') tmp_l2 <- ep_l else tmp_l2 <- rc_l
      repeats <- length(tmp_l2$length)
      tmp_lty <- ifelse(window == 'Case',1,2)
      tmp_v.off <- ifelse(window == 'Case',1,0.995)
      tmp_len.pos <- ifelse(window == 'Case',3,1)
      tmp_l <- do.call('rbind', lapply(tmp_l2$length, as.data.frame))
      names(tmp_l) <- paste0('ep_', names(tmp_l))

      dfr_i <- dfr_w[
        dfr_w$sn %in% dfr_w$wind_id[dfr_w$wind_nm == window]
        & dfr_w$epid_total > 1  & !is.na(dfr_w$wind_nm),]
      dfr_i$ycord <- dfr_i$ycord * tmp_v.off
      mm <- as.data.frame(index_multiples(dfr_i$sn, nrow(dfr), repeats))
      dfr_i <- do.call('rbind', rep(list(dfr_i), repeats))
      dfr_i$mm <- mm$mm

      dfr_i$ep_xcord_a <- tmp_l$ep_start[dfr_i$mm] + dfr_i$x_gap
      dfr_i$ep_xcord_z <- tmp_l$ep_end[dfr_i$mm] + dfr_i$x_gap

      dfr_i$ep_xcord_a[dfr_i$ep_xcord_a < xlim[1]] <- xlim[1]
      dfr_i$ep_xcord_a[dfr_i$ep_xcord_a > xlim[2]] <- xlim[2]
      dfr_i$ep_xcord_z[dfr_i$ep_xcord_z < xlim[1]] <- xlim[1]
      dfr_i$ep_xcord_z[dfr_i$ep_xcord_z > xlim[2]] <- xlim[2]

      dfr_i$ep_xcord_m <-( (as.numeric(dfr_i$ep_xcord_z) - as.numeric(dfr_i$ep_xcord_a))/2) + dfr_i$ep_xcord_a
      dfr_i <- dfr_i[dfr_i$ep_xcord_a != dfr_i$ep_xcord_z,]

      tmp.lgk <- !dfr_i$from_last
      suppressWarnings({
        arrows(
          x0 = dfr_i$ep_xcord_a[tmp.lgk], y0 = dfr_i$ycord[tmp.lgk],
          x1 = dfr_i$ep_xcord_z[tmp.lgk], y1 = dfr_i$ycord[tmp.lgk],
          col = contrast_color,
          length = arrow.head.size, lwd = edges.size, lty = tmp_lty, code = 2)
      })

      tmp.lgk <- dfr_i$from_last
      suppressWarnings({
        arrows(
          x0 = dfr_i$ep_xcord_a[tmp.lgk], y0 = dfr_i$ycord[tmp.lgk],
          x1 = dfr_i$ep_xcord_z[tmp.lgk], y1 = dfr_i$ycord[tmp.lgk],
          col = contrast_color,
          length = arrow.head.size, lwd = edges.size, lty = tmp_lty, code = 1)
      })

      if(repeats > 1){
        tmp.lgk <- dfr_i$mm <= nrow(dfr)

        dfr_i$cv_xcord_a <-  dfr_i$cv_xcord_z <- NA
        dfr_i$cv_xcord_a[tmp.lgk] <- tmp_l2$coverage@start[dfr_i$sn[tmp.lgk]] + dfr_i$x_gap[tmp.lgk]
        dfr_i$cv_xcord_z[tmp.lgk] <- right_point(tmp_l2$coverage[dfr_i$sn[tmp.lgk]]) + dfr_i$x_gap[tmp.lgk]

        segments(
          x0 = dfr_i$cv_xcord_a[tmp.lgk], y0 = dfr_i$ycord[tmp.lgk],
          x1 = dfr_i$cv_xcord_z[tmp.lgk], y1 = dfr_i$ycord[tmp.lgk],
          col = contrast_color, lwd = edges.size, lty = 3)
      }

      dfr_i$len_nm <- ''
      if('wind_nm' %in% show_labels){
        dfr_i$len_nm <- paste0(window, '-length')
      }

      if("length" %in% show_labels){
        tmp_l2 <- length_to_range(
          x = x@options$case_length, date = int, from_last = x@options$from_last,
          episode_unit = 1, output = 'length')$length

        tmp_l2 <- do.call('rbind', lapply(tmp_l2, as.data.frame))

        dfr_i$len_nm <-
          paste0(dfr_i$len_nm, "\n",
                 format(number_line(tmp_l2$start[dfr_i$mm], tmp_l2$end[dfr_i$mm])),
                 " ", ifelse(is_dt, gsub("s$", "-", names(diyar::episode_units)[dfr_i$episode_unit]), "unit-"),
                 "difference")

        if(any(c("length", 'wind_nm') %in% show_labels)){
          text(dfr_i$ep_xcord_m, dfr_i$ycord, labels = dfr_i$len_nm,
               pos = tmp_len.pos, col = contrast_color, cex = text.size)
        }
      }



      if(tolower(paste0(window, '_overlap_methods')) %in% show_labels){
        dfr_o <- data.table::as.data.table(dfr_w[dfr_w$wind_id %in% dfr_i$sn,])
        tmp_l$mm <- seq_len(nrow(tmp_l))
        tmp_l$ord <- ceiling(tmp_l$mm/nrow(dfr))
        tmp_l$wind_id <- tmp_l$mm - (nrow(dfr) * (tmp_l$ord -1))
        tmp_l3 <- data.table::as.data.table(tmp_l[dfr_i$mm,])

        dfr_o <- merge(dfr_o, tmp_l3, all = TRUE, allow.cartesian=TRUE, by = 'wind_id')
        dfr_o$mth <- overlap_method(
          number_line(dfr_o$start, dfr_o$end),
          number_line(dfr_o$ep_start, dfr_o$ep_end))
        dfr_o <- dfr_o[dfr_o$mth != 'none',]
        text((dfr_o$xcord + dfr_o$wi_xcord)/2,
             (dfr_o$ycord + dfr_o$wi_ycord)/2,
             labels = dfr_o$mth, cex = text.size,
             col = epids_cols[dfr_o$ep_ord])
        dfr_o$mth <- dfr_o$ep_start <- dfr_o$ep_end <- NULL
      }
    }
  }
}

#' @rdname schema
#' @export
schema.pane <- function(
    x, title = NULL, show_labels = c("window_label"), plot_scale = 'fit.screen',
    theme = "dark", seed = NULL, custom_label = NULL, ...){

  # Validations
  errs <- err_schema_pane_0(
    x = x, date = x@options$date, title = '', show_labels = show_labels,
    theme = theme)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  theme_color <- ifelse(theme == 'light', 'white', 'black')
  contrast_color <- ifelse(theme == 'light', 'black', 'white')

  if(isTRUE(show_labels)){
    show_labels <- c("sn", "epid", "date", "case_nm", "window_label")
  }else{
    show_labels[show_labels == 'pane'] <- 'epid'
  }

  y <- x
  x <- as.epid(y)
  x@sn <- y@sn
  x@wind_id <- list(wind_id1 = x@.Data)
  x@wind_nm <- lapply(x@wind_id, as.character)

  names(x@wind_nm) <- gsub('wind_id', 'wind_nm',  names(x@wind_nm))
  x@epid_total <- y@pane_total
  x@case_nm <- y@case_nm
  x@options <-list(
    date = y@options$date, strata = y@.Data,
    episode_unit = encode('seconds'), from_last = FALSE,
    case_length = 1, recurrence_length = 1)

  if(length(show_labels) == 0) show_labels <- FALSE
  schema(
    x, show_labels = show_labels[!show_labels %in% 'window_label'],
    custom_label = custom_label, seed = seed,
    theme = theme, plot_scale = plot_scale)

  # Size scaling
  if(plot_scale == 'fit.screen'){
    s.fac <- prod(grDevices::dev.size())/70
  }else if(plot_scale == 'default'){
    s.fac <- 1
  }else if(inherits(plot_scale, 'numeric')){
    s.fac <- abs(1 * plot_scale)
  }

  size.adjust <- 10/length(x) * s.fac
  text.size <- 2*size.adjust

  wnds <- y@window_list[!duplicated(y@window_list)]
  wnds <- do.call('rbind', lapply(wnds, as.data.frame))
  wnds$nl <- number_line(wnds$start, wnds$end)

  ylim <- par('usr')[3:4]
  for (i in 1:nrow(wnds)) {
    rect(
      ytop = ylim[2], ybottom = ylim[1], xleft = wnds$start[i],
      xright = wnds$end[i], col = random_colors(1, alpha = .2),
      border = NA
    )

    if('window_label' %in% show_labels){
      text(wnds$start[i] + (wnds$nl[i]@.Data/2), ylim[2],
           labels = format(wnds$nl[i]), col = contrast_color, cex = text.size, pos = 1)
    }
  }
}

#' @rdname schema
#' @export
schema.pid <- function(
    x, title = NULL, show_labels = TRUE, theme = "dark", seed = NULL,
    plot_scale = 'fit.screen', custom_label = NULL, ...){

  # Validations
  errs <- err_schema_pid_0(
    x = x, title = title, show_labels = show_labels, theme = theme)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  theme_color <- ifelse(theme == 'light', 'white', 'black')
  contrast_color <- ifelse(theme == 'light', 'black', 'white')

  if(isTRUE(show_labels)){
    show_labels <- c('sn', 'epid')
  }else{
    show_labels[show_labels == 'pid'] <- 'epid'
  }

  y <- x
  x <- as.epid(y)
  x@sn <- y@sn
  x@wind_id <- y@link_id
  x@wind_nm <- lapply(x@wind_id, as.character)
  names(x@wind_id) <- gsub('link_id', 'wind_id',  names(x@wind_id))
  names(x@wind_nm) <- gsub('link_id', 'wind_nm',  names(x@wind_nm))
  x@epid_total <- y@pid_total
  x@options <-list(
    date = as.vector(y@pid_cri), strata = y@.Data,
    episode_unit = encode('seconds'), from_last = FALSE,
    case_length = 1, recurrence_length = 1)

  schema(
    x, show_labels = show_labels, custom_label = custom_label, seed = seed,
    theme = theme, plot_scale = plot_scale)

  # Size scaling
  if(plot_scale == 'fit.screen'){
    s.fac <- prod(grDevices::dev.size())/70
  }else if(plot_scale == 'default'){
    s.fac <- 1
  }else if(inherits(plot_scale, 'numeric')){
    s.fac <- abs(1 * plot_scale)
  }

  size.adjust <- 10/length(x) * s.fac
  text.size <- 3*size.adjust

  xlim2 <- xlim <- par('usr')[1:2]
  xlim2[1] <- ceiling(xlim2[1]); xlim2[2] <- floor(xlim2[2])
  xlim2 <- unique(xlim2)

  if(length(unique(range(y@pid_cri))) == 1){
    xlim <- number_line(xlim[1], xlim[2])
  }else{
    xlim <- range(y@pid_cri)
    xlim <- split_number_line(number_line(xlim[1], xlim[2]+1), by = 1, precision = .1, fill = FALSE)
    xlim@.Data <- ceiling(xlim@.Data)
    left_point(xlim[1]) <- par('usr')[1]
    right_point(xlim[length(xlim)]) <- par('usr')[2]
  }

  pid_cris <- y@pid_cri[!duplicated(y@pid_cri)]
  pid_cris <- sort(pid_cris)
  ylim <- par('usr')[3:4]

  for (i in 1:length(xlim)) {
    rect(
      ytop = ylim[2], ybottom = ylim[1], xleft = left_point(xlim[i]),
      xright = right_point(xlim[i]), col = random_colors(1, alpha = .2),
      border = NA
    )

    text(left_point(xlim[i]) + (xlim[i]@.Data/2), ylim[1] + ((ylim[2] - ylim[1])/2),
         labels = pid_cri_l(pid_cris[i]), col = contrast_color, cex = text.size)
  }
}


