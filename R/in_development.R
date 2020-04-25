# @rdname episode_group
# @param epid \code{epid} object
# @details
# \code{plot_epid()} visulaises how an episode has been created. It works backwards, using the episode (\code{epid}) and corresponding
#  \code{date}, \code{case_length} and \code{recurrence_length} to show why/how events or event periods have been grouped in each episode.
#  This is then shown on a plots (one per \code{strata}) captured in an \code{R} object (\code{list} is there are multiple plots).
#  The plots can then be saved and shared.
#  \code{date}, \code{case_length} and \code{recurrence_length} must match those used in creating \code{epid} otherwise, the plot will not reflect what actually happened.
#
plot_epid <- function(epid, date= NULL, strata = NULL, case_length = NULL, recurrence_length = NULL, from_last=FALSE){
  if(!(length(epid) == length(date) | is.null(date))) stop(paste0("lenght(epid) should be equal to length(date)"))
  if(!(length(case_length) %in% c(1, length(epid)) | (length(case_length) ==0 & is.null(case_length)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(epid)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'date'",sep=""))

  # Scale factor - Automatcially try to resize element spacing and size
  scale_fac <- 1

  # Events, window and episode color groups
  pl_cols <- grDevices::colours()
  pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,5))]

  # episodes
  dfp <- to_df(epid)

  # corresponding event dates
  if(!is.null(date)){
    dfp$date <- date
    if(is.number_line(dfp$date)){
      dfp$dt_a <- left_point(dfp$date)
      dfp$dt_z <- right_point(dfp$date)
    }else{
      dfp$dt_a <- dfp$date
      dfp$dt_z <- dfp$date
    }
  }else{
    #dfp$dt_z <- dfp$dt_a <- as.numeric(cut(1:20, length(epid)))
    dfp$date <- dfp$dt_z <- dfp$dt_a <- 1:nrow(dfp) * 3
  }

  # Sort chronologically
  dfp <- dfp[order(dfp$epid, dfp$dt_a, dfp$dt_z),]

  # event labels - Event/period date(s), case_nm and referent event per window
  # Same day periods are shown as time points to save spac
  if(is.null(date)){
    dfp$event_nm <- ifelse(dfp$case_nm!="Skipped",
                           paste0("SN-", format(dfp$sn, scientific=F), "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$wind_id), "\n(reference)", ""), "\nevent"),
                           paste0("SN-", format(dfp$sn, scientific=F), "\n", dfp$case_nm))
  }else{
    dfp$event_nm <- ifelse(dfp$case_nm!="Skipped",
                           paste0(ifelse(dfp$dt_z-dfp$dt_a==0, format(dfp$dt_a), format(dfp$date)), "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$wind_id), "\n(reference)", ""), "\nevent"),
                           paste0(ifelse(dfp$dt_z-dfp$dt_a==0, format(dfp$dt_a), format(dfp$date)), "\n", dfp$case_nm)
    )
  }


  # lengths
  if(is.null(case_length)){
    dfp$c <- 0
  }else{
    dfp$c <- case_length
  }

  if(is.null(recurrence_length)){
    dfp$r <- 0
  }else{
    dfp$r <- recurrence_length
  }

  # Separate plots per strata
  if(is.null(strata)){
    dfp$strata <- 1
  }else{
    dfp$strata <- strata
  }

  story <- function(dfp){
    #x-axis limits
    # min and dates plus & minus lengths
    #xlims <- c((min(dfp$dt_a) - max(c(max(dfp$c), max(dfp$r)))), (max(dfp$dt_z) + max(c(max(dfp$c), max(dfp$r)))))
    xlims <- c(min(dfp$dt_a)-1, max(dfp$dt_z)+1)

    # Colors groups for windows
    dfp$win_cols <- rev(pl_cols)[as.numeric(as.factor(dfp$wind_id))]
    cols <- dfp$win_cols
    names(cols) <- dfp$wind_id
    cols <- cols[!duplicated(cols)]

    # Colors groups for episodes.
    # Linked corresponding window colors
    dfp$epd_cols <- cols[as.character(dfp$epid)]

    # Y-axis center
    # Everthing else is relative to this
    e_y <- 1

    # Space out (along Y-axis) overlapping event labels
    # Order the longer periods above shorter ones
    # Event mid-pont. Where labels will be plotted

    # To save space, only one event is shown among same day duplicates
    # Below is the preference for which to show
    dfp$ord <- ifelse(dfp$case_nm=="Skipped",1,5)
    dfp$ord <- ifelse(dfp$case_nm=="Case",2,dfp$ord)
    dfp$ord <- ifelse(dfp$case_nm=="Recurrent",3,dfp$ord)
    dfp$ord <- ifelse(dfp$case_nm=="Duplicate",4,dfp$ord)
    dfp$cri <- paste0(dfp$dt_a, dfp$dt_z)
    dfp <- dfp[order(dfp$cri, dfp$ord),]
    dfp$event_nm <- ifelse(duplicated(dfp$cri), "", dfp$event_nm)

    dfp$dt_c <- (as.numeric(dfp$dt_a) + as.numeric(dfp$dt_z)) * .5
    dfp$event_length <- diyar::number_line(dfp$dt_a, dfp$dt_z)@.Data
    dfp$p_ord <- order(-dfp$event_length, -as.numeric(dfp$dt_a),  dfp$ord)

    mid_pts <- dfp$dt_c
    names(mid_pts) <- 1:length(mid_pts)
    mid_pts <- mid_pts[dfp$event_nm!=""]
    p_ord <- dfp$p_ord[dfp$event_nm!=""]

    # # Check mid-points that are too close (0.04) as this will overlap in the plot.
    chck <- diyar::compress_number_line(diyar::expand_number_line(as.number_line(mid_pts), 1), deduplicate = F, collapse = T)
    # Per overlaping group, order events based on size/length of period
    ord <- lapply(split(p_ord,  chck@gid), function(x){
      order(-x)
    })
    ord <- unsplit(ord, chck@gid)
    names(ord) <- names(mid_pts)
    ord <- ord[as.character(1:nrow(dfp))]
    ord <- ifelse(is.na(ord), 1, ord)

    # All events centered
    dfp$e_y <- e_y
    # Among overlapping events/period, space out the 2nd/more event incrementally (0.17)
    dfp$e_y[ord>1] <- max(dfp$e_y) + ((ord[ord>1]-1) * 0.25 * scale_fac)

    # dfp$e_y <- 1
    # dfp$e_y[dfp$event_nm!=""] <- max(dfp$e_y) + ((space_out_yy(diyar::expand_number_line(as.number_line(dfp$dt_c[dfp$event_nm!=""]), 1))-1) * 0.05 * scale_fac)

    # Some number_lines may overlap by chance so, space out again incrementally (0.01)
    dfp$e_y <- dfp$e_y  - (1:nrow(dfp) * (scale_fac * 0.02))

    # recurrence_length is supplied, strip out the reference events for recurrence periods
    if(!is.null(recurrence_length)){
      dfp$rec_len_y_axis <- min(dfp$e_y) - (scale_fac * 0.2)
      rl <- dfp[dfp$sn %in% unique(dfp$wind_id[dfp$wind_nm!="Case" & dfp$case_nm!="Skipped"]),]
      rl$e_dt_a <- as.numeric(lapply(split(rl$dt_a, rl$wind_id), min)[as.character(rl$wind_id)])
      rl$e_dt_z <- as.numeric(lapply(split(rl$dt_z, rl$wind_id), max)[as.character(rl$wind_id)])
      # Space out their position on Y-axis
      rl$rec_len_y_axis <- rl$rec_len_y_axis - (1:nrow(rl) * (scale_fac * 0.03))

    }else{
      dfp$rec_len_y_axis <- min(dfp$e_y)
      rl <- data.frame()
    }

    # Windows
    dfp$windows_y_axis <- rep((min(dfp$rec_len_y_axis) - (scale_fac * 0.15)), nrow(dfp))
    # Space out their position on Y-axis
    dfp$windows_y_axis <- dfp$windows_y_axis - (scale_fac * 0.04 * (as.numeric(as.factor(dfp$wind_id))-1))

    # Episodes
    dfp$episode_y_axis <- min(dfp$windows_y_axis) - (scale_fac * 0.25)
    # Space out their position on Y-axis
    dfp$episode_y_axis <- dfp$episode_y_axis - (scale_fac * 0.04 * (as.numeric(as.factor(dfp$epid))-1))

    # Case_lengths
    if(!is.null(case_length)){
      dfp$case_len_y_axis <- max(dfp$e_y) + (scale_fac * 0.35)
    }else{
      dfp$case_len_y_axis <- max(dfp$e_y)
    }

    dfp <- dfp[order(dfp$epid, dfp$dt_a, dfp$dt_z),]

    # See if there's an alternative.
    #dev.new()
    graphics::par(bg="black")
    graphics::par(mar=rep(0,4))
    graphics::plot(y=dfp$e_y, x=dfp$dt_a, ylim=c(min(dfp$episode_y_axis) - 0.1, max(dfp$case_len_y_axis) + 0.2), xlim = xlims)

    # ------------------------------
    for(i in 1:nrow(dfp)){
      # Events/periods
      graphics::points(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), bg = dfp$win_cols[i], col = dfp$win_cols[i], pch = 21)
      # left_point() tracer
      graphics::lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$windows_y_axis[i]), x = rep(dfp$dt_a[i],2), col = dfp$win_cols[i], lty = 2)

      if(dfp$c[i]<0 & dfp$wind_id[i] == dfp$sn[i]){
        if(dfp$dt_a[i] < dfp$dt_z[i] + dfp$c[i]){
          # Period "completely changed" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i] + dfp$c[i]), col = dfp$win_cols[i], lty = 1)
          # The period "cancelled out" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_z[i] + dfp$c[i], dfp$dt_z[i]), col = "white", lty = 2)
        }else{
          # Period "shortened" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i] + dfp$c[i]), col = "white", lty = 2)
        }
      }else{
        # Period "shortened" due to the negative case_length
        graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), col = dfp$win_cols[i], lty = 1)
        # right_point() tracer
        graphics::lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$windows_y_axis[i]), x = rep(dfp$dt_z[i],2), col = dfp$win_cols[i], lty = 2)
      }
      graphics::text(cex = .7 * scale_fac, y=dfp$e_y[i] + (scale_fac * 0.05), x=mean(c(dfp$dt_a[i], dfp$dt_z[i])), labels = dfp$event_nm[i], adj =c(.5,0), col = dfp$win_cols[i])
    }

    # case lengths
    cl <- dfp[dfp$sn %in% unique(dfp$wind_id[dfp$wind_nm=="Case"]),]
    if(nrow(cl)>0 & !is.null(case_length)){
      # episode start and end dates
      # re-calculated because it may not be supplied
      cl$e_dt_a <- as.numeric(lapply(split(cl$dt_a, cl$epid), min)[as.character(cl$epid)])
      cl$e_dt_z <- as.numeric(lapply(split(cl$dt_z, cl$epid), max)[as.character(cl$epid)])

      cl$dt <- as.numeric(cl$dt_z)
      cl$end_dt <- as.numeric(cl$dt + ifelse(from_last==F, cl$c, -cl$c))

      cl$dt2 <- as.numeric(cl$dt_a)
      cl$end_dt2 <- as.numeric(cl$dt2 - ifelse(from_last==F, cl$c, -cl$c))

      # spacing
      dfp$case_len_y_axis <- dfp$case_len_y_axis + (0.02 * ((1:nrow(dfp))-1))
      cl$lab <- paste0("Case length\n(",cl$c,"-day\ndifference)")

      for(i in 1:nrow(cl)){
        # Surpressed warning from 0 length arrows
        suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=cl$case_len_y_axis[i], x0 = cl$dt[i], x1 = cl$end_dt[i], col ="white"))

        if(i == nrow(cl) & i != 1 & cl$end_dt[i] > xlims[2]) x_pos <- ifelse(from_last ==T, cl$dt[i] + (scale_fac * 0.5), cl$dt[i] - (scale_fac * 0.5))
        else x_pos <- mean(c(cl$dt[i], cl$end_dt[i]))

        graphics::text(cex = .7 * scale_fac, y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=x_pos, labels = cl$lab[i], col ="white", adj =c(0.5 ,0))
        graphics::lines(y=c(cl$case_len_y_axis[i] - (scale_fac * 0.015), cl$case_len_y_axis[i] + (scale_fac * 0.015)), x = rep(cl$dt[i],2), col=cl$win_col[i])

        # Trying to guess when bi_direction has been used.
        # Doesn't capture all scenarios yet.
        # Stopped for now. Need's another approach
        if(cl$dt2[i]!= cl$e_dt_a[i]){
          # Surpressed warning from 0 length arrows
          # suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=cl$case_len_y_axis[i], x0 = cl$dt2[i], x1 = cl$end_dt2[i], col ="white"))
          # graphics::text(cex = .7 * scale_fac, y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(cl$dt2[i], cl$end_dt2[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))
        }
      }
    }



    # Recurrence lengths
    if(nrow(rl)>0 & !is.null(recurrence_length)){
      rl$dt <- as.numeric(rl$dt_z)
      rl$end_dt <- as.numeric(rl$dt + ifelse(from_last==F, rl$r, -rl$r))

      rl$dt2 <- as.numeric(rl$dt_a)
      rl$end_dt2 <- as.numeric(rl$dt2 - ifelse(from_last==F, rl$r, -rl$r))
      rl$lab <- paste0("Recurrence length\n(",rl$r,"-day\ndifference)")

      for(i in 1:nrow(rl)){
        # Surpressed warning from 0 length arrows

        if(i == nrow(rl) & i != 1 & rl$end_dt[i] > xlims[2]) x_pos <- ifelse(from_last ==T, rl$dt[i] + (scale_fac * 0.5), rl$dt[i] - (scale_fac * 0.5))
        else x_pos <- mean(c(rl$dt[i], rl$end_dt[i]))

        suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=rl$rec_len_y_axis[i], x0 = rl$dt[i], x1 = rl$end_dt[i], lty=2, col ="white"))
        graphics::text(cex = .7 * scale_fac, y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x= x_pos, labels = rl$lab[i], col ="white", adj =c(.5,0))
        graphics::lines(y=c(rl$rec_len_y_axis[i] - (scale_fac * 0.015), rl$rec_len_y_axis[i] + (scale_fac * 0.015)), x = rep(rl$dt[i],2), col=rl$win_col[i])
      }

      # Trying to guess when bi_direction has been used.
      # Doesn't capture all scenarios yet.
      # Should it apply to recurrence length?
      # Stopped for now. Need's another approach
      if(rl$dt2[i]!= rl$e_dt_a[i]){
        # Surpressed warning from 0 length arrows
        # suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=rl$rec_len_y_axis[i], x0 = rl$dt2[i], x1 = rl$end_dt2[i], col ="white"))
        # graphics::text(cex = .7 * scale_fac, y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(rl$dt2[i], rl$end_dt2[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
      }
    }

    # Windows
    win <- dfp

    win$w_dt_z <- as.numeric(lapply(split(win$dt_z, win$wind_id), ifelse(from_last==F, max, min))[as.character(win$wind_id)])
    win <- win[!duplicated(win$wind_id),]
    dt_as <- as.numeric(dfp$dt_a)
    names(dt_as) <- dfp$sn
    win$w_dt_a <- dt_as[as.character(win$wind_id)]

    # window ord as label
    win$wind_nm_l <- paste0("Window ", as.numeric(as.factor(win$wind_id)), "\n(", tolower(win$wind_nm), "\nwindow)")
    # window ID as label
    win$wind_nm_l <- paste0("Window ID: ", win$wind_id, ifelse(win$wind_nm=="Skipped","", paste0("\n(", tolower(win$wind_nm), "\nwindow)")))

    for(i in 1:nrow(win)){
      graphics::lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.02)), x = rep(win$w_dt_a[i],2), col=win$win_col[i])
      graphics::lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.02)), x = rep(win$w_dt_z[i],2), col=win$win_col[i])

      if(win$case_nm[i]!="Skipped"){
        graphics::text(cex = .7 * scale_fac, y=win$windows_y_axis[i] - (scale_fac * 0.2), x= mean(c(win$w_dt_a[i], win$w_dt_z[i])), labels = win$wind_nm_l[i], col=win$win_col[i], adj =c(.5,0))
        graphics::lines(y=rep(win$windows_y_axis[i],2), x = c(win$w_dt_a[i], win$w_dt_z[i]), col=win$win_col[i])

        if(win$w_dt_a[i]==win$w_dt_z[i]){
          graphics::lines(y=rep(win$windows_y_axis[i],2), x = c(win$w_dt_a[i]-.2, win$w_dt_z[i]+.2), col=win$win_col[i])
        }
      }

      graphics::lines(y=c(win$windows_y_axis[i] - (scale_fac * 0.23), win$episode_y_axis[i]), x = rep(mean(c(win$w_dt_a[i], win$w_dt_z[i])), 2), col=win$epd_col[i])
      graphics::lines(y=c(win$windows_y_axis[i] - (scale_fac * 0.13), win$windows_y_axis[i] - (scale_fac * (0))), x = rep(mean(c(win$w_dt_a[i], win$w_dt_z[i])), 2), col=win$epd_col[i])
    }

    # Episodes
    epd <- dfp
    epd$e_dt_a <- as.numeric(lapply(split(epd$dt_a, epd$epid), min)[as.character(epd$epid)])
    epd$e_dt_z <- as.numeric(lapply(split(epd$dt_z, epd$epid), max)[as.character(epd$epid)])
    epd <- epd[!duplicated(epd$epid),]

    # episode ord as label
    epd$wind_nm_l <- paste0("Episode ", as.numeric(as.factor(epd$epid)))
    # episode ID as label
    epd$wind_nm_l <- paste0("Episode ID: ", epd$epid)

    for(i in 1:nrow(epd)){
      graphics::lines(y=rep(epd$episode_y_axis[i],2), x = c(epd$e_dt_a[i], epd$e_dt_z[i]), col=epd$epd_cols[i])
      graphics::lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.02)), x = rep(epd$e_dt_a[i],2), col=epd$epd_cols[i])
      graphics::lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.02)), x = rep(epd$e_dt_z[i],2), col=epd$epd_cols[i])

      graphics::text(cex = .7 * scale_fac, y=epd$episode_y_axis[i] - (scale_fac * 0.08), x=mean(c(epd$e_dt_a[i], epd$e_dt_z[i])), labels = epd$wind_nm_l[i], col=epd$epd_cols[i], adj =c(.5,0))

      if(epd$e_dt_a[i]==epd$e_dt_z[i]){
        graphics::lines(y=rep(epd$episode_y_axis[i],2), x = c(epd$e_dt_a[i]-.2, epd$e_dt_z[i]+.2), col=epd$win_col[i])
      }
    }

    plt <- grDevices::recordPlot()
    # graphics.off()
    # dev.off()
    return(plt)
  }

  p <- lapply(unique(dfp$strata), function(x){
    story(dfp[dfp$strata==x,])
  })

  names(p) <- unique(dfp$strata)
  if(length(p)==1) p <- p[[1]]
  return(p)
}


# @rdname number_line
# @param x \code{number_line} object
# @details
# \code{plot_number_line()} - visulaises \code{number_line} objects and how they've overlapped with each other.
# @examples
# d <- c(number_line(1,5), number_line(1,2), number_line(2,3),
# number_line(4,7), number_line(4,5), number_line(1,5))
# diyar:::plot_number_line(d[c(1,6)])
# diyar:::plot_number_line(d[c(2,3)])
# diyar:::plot_number_line(d[c(1,3)])
# diyar:::plot_number_line(d[c(1:3)])

plot_number_line <- function(x, strata = NULL, show_overlap = FALSE){
  if(missing(x)) stop("argument 'x' is missing, with no default")
  df <- diyar::to_df(x)
  if(is.null(strata)){
    df$strata <- 1
  } else{
    df$strata <- strata
  }

  df$x <- x
  df$y <- diyar::reverse_number_line(df$x, "decreasing")

  # df <- df[order(-(df$end-df$start)), ]
  #x <- x[order(-(df$end-df$start))]

  #df$sn <- 1:nrow(df) * 1
  # df$sn <- 1
  # sn_change <- 1
  # #df <- df[nrow(df):1,]
  # while (max(sn_change) ==1) {
  #   df$c <- diyar::compress_number_line(df$y, deduplicate = F, collapse = T)
  #
  #   ord <- lapply(split(1:nrow(df), paste0(df$c@gid,"-", df$sn)), order)
  #   lk_sn <- lapply(split(df$sn, paste0(df$c@gid)), function(x){
  #     rep(max(x), length(x))
  #   })
  #
  #   df$ord <- unsplit(ord, paste0(df$c@gid,"-", df$sn))
  #   df$lk_sn <- unsplit(lk_sn, paste0(df$c@gid))
  #   new_sn <- ifelse(df$ord==2, df$lk_sn+1, df$sn)
  #
  #   sn_change <- ifelse(df$sn != new_sn,1,0)
  #   df$sn <- new_sn
  # }

  df$sn <- space_out_y(df$y)

  gid <- T

  pl_cols <- grDevices::colours()
  pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,3))]
  df$cols <- ifelse(gid==rep(T, nrow(df)), rev(pl_cols)[as.numeric(as.factor(df$gid))], rev(pl_cols)[1:nrow(df)])

  x_lim <- c(min(df$start, df$end), max(df$start, df$end))

  dec_chk <- function(x) ifelse(x-floor(as.numeric(x))==0,F,T)

  x_lim[1][dec_chk(x_lim[1])] <- floor(as.numeric(x_lim[1][dec_chk(x_lim[1])]))
  x_lim[2][dec_chk(x_lim[2])] <- ceiling(as.numeric(x_lim[2][dec_chk(x_lim[2])]))

  y_lim <- c(.8, max(df$sn) * 1)

  x_p <- length(x_lim)/12
  y_p <- length(y_lim)/12

  sf <- x_p/y_p

  graphics::par(bg="black")
  graphics::par(mar=c(2,2,2,2))
  graphics::plot(x =x_lim, y=y_lim, type="n")
  graphics::axis(1,  at=seq(x_lim[1], x_lim[2], 1), labels= format(seq(x_lim[1], x_lim[2], 1)), col="white", col.axis="white", cex.axis = .8)

  for (str in unique(df$strata)) {
    xd <- df[df$strata==str,]

    for (i in 1:nrow(xd)) {
      if (xd$start[i]!=xd$end[i]){
        graphics::arrows(length=0.1, angle=20, x0 =xd$start[i], x1= xd$end[i], y0 = xd$sn[i], y1= xd$sn[i], col = xd$cols[i])
      }
      graphics::points(y=xd$sn[i], x = xd$start[i], pch = 21, bg=xd$cols[i], col=xd$cols[i])

      if(show_overlap == T){
        for (j in 1:nrow(xd)){
          om <- overlap_method(xd$x[i], xd$x[j])
          om_l <- gsub("aligns_","aligns\n  ",om)
          if(j>i & om != "none"){
            if(om %in% c("exact", "inbetween")){
              x1 <- ifelse(xd$start[j]<xd$start[i], xd$start[i], xd$start[j])
              x2 <- ifelse(xd$end[j]<xd$end[i], xd$end[j], xd$end[i])
              y <- ifelse(xd$start[j]<xd$start[i], xd$sn[i], xd$sn[j])
              o <- ifelse(xd$start[j]<xd$start[i], -1, .6)

              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(x1, x1), lty=2, col=xd$cols[i])
              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(x2, x2), lty=2, col=xd$cols[i])
              graphics::text(y = y, x =  mean(c(x1, x2)), labels = om_l, col=xd$cols[i], cex = (sf * .8), pos = 1, offset = (sf * o))
            }else if(om=="aligns_end"){
              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(xd$end[j], xd$end[j]), lty=2, col=xd$cols[i])
              graphics::text(srt = 90, y = xd$sn[i] + (sf * .05), x =  xd$end[j] + (sf * .05), labels = om_l, col=xd$cols[i], cex = (sf * .8), pos = 4, offset = (sf *.6))
            }else if(om=="across"){
              x <- ifelse(xd$start[j] <= xd$end[i] & xd$start[j] >= xd$start[i], xd$start[j], xd$end[j])
              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(x, x), lty=2, col=xd$cols[i])
              graphics::text(srt = 90, y = xd$sn[i] + (sf * .05), x =  x + (sf * .05), labels = om_l, col=xd$cols[i], cex = (sf * .8), pos = 4, offset = (sf *.6))
            }else if(om=="chain"){
              x <- ifelse(xd$start[j] <= xd$end[j] & xd$start[j] >= xd$start[i], xd$start[j], xd$end[j])
              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(x, x), lty=2, col=xd$cols[i])
              graphics::text(srt = 90, y = xd$sn[i] + (sf * .05), x =  x + (sf * .05), labels = om_l, col=xd$cols[i], cex = (sf * .8), pos = 4, offset = (sf *.6))
            }else{
              graphics::lines(y=c(xd$sn[i], xd$sn[j]), x=c(xd$start[j], xd$start[j]), lty=2, col=xd$cols[i])
              graphics::text(srt = 90, y = xd$sn[i] + (sf * .05), x =  xd$start[j] + (sf * .05), labels = om_l, col=xd$cols[i], cex = (sf * .8), pos = 4, offset = (sf *.6))
            }
          }
        }
      }

    }
  }

  plt <- grDevices::recordPlot()
  return(plt)
}


# @rdname record_group
# @param pid \code{pid} object
# @details
# \code{plot_pid()} visulaises how a record group has been created. It works backwards, using the record group (\code{pid})
# to show which records matched have matched with which, and how they've been grouped together to form the record group.
#  This is then shown on plots (one per record group if \code{strata} is TRUE) and captured in an \code{R} object (\code{list} for multiple plots).
#  The plots can then be saved and shared.
#
plot_pid <- function(pid, strata = FALSE){
  pids_df <- to_df(pid)

  network <- function(pids_df){
    # criteria order
    pids_df$p_ord <- as.numeric(as.factor(pids_df$pid_cri))

    # position for record record boxes
    pids_df <- pids_df[order(pids_df$pid_cri), ]
    pids_df$ord <- sequence(rle(pids_df$pid_cri)$lengths)

    # boxes per row
    rec_lim <- 4

    # horizontal space between record boxes
    h_box <- 1.7
    # vertical space between record boxes
    v_box <- 2
    # horizontal space between criteria box
    h_cri <- 10
    # vertical space between criteria box
    v_cri <- 6

    # scater v1
    sct <- unique(pids_df$p_ord)
    sct <- sort(rep(sct,2)) * c(1,-1)
    sct <- c(0, sct)

    # x,y coordinates
    pids_df$x <- pids_df$y <- 0
    pids_df$y <- (-(ceiling(pids_df$ord/rec_lim) -1) * v_box) +  (sct[pids_df$p_ord] * v_cri)
    pids_df$x <- (((pids_df$ord-1) %% rec_lim) * h_box) + (pids_df$p_ord * h_cri)

    # colours per record group
    pl_cols <- grDevices::colours()
    pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,5))]
    pids_df$p_cols <- rev(pl_cols)[as.numeric(as.factor(pids_df$pid))]

    xlims <-c(min(pids_df$x-2), max(pids_df$x)+2)
    ylims <-c(min(pids_df$y)-4, max(pids_df$y)+4)

    graphics::par(bg="black")
    graphics::par(mar=rep(0,4))
    graphics::plot(x=0, y=0, type="n", xlim=xlims, ylim=ylims)

    graphics::text(x=pids_df$x, y=pids_df$y, label= paste0("SN\n", pids_df$sn), cex = .7, adj = c(.5, .5), col = pids_df$p_cols)

    # record boxes
    # half the width and length
    box_width <- .7
    box_height <- .9
    graphics::segments(x0 = pids_df$x-box_width , y0=pids_df$y-box_height, x1 = pids_df$x+box_width , y1 = pids_df$y-box_height, col = pids_df$p_cols)
    graphics::segments(x0 = pids_df$x-box_width , y0=pids_df$y+box_height, x1 = pids_df$x+box_width , y1 = pids_df$y+box_height, col = pids_df$p_cols)
    graphics::segments(x0 = pids_df$x-box_width , y0=pids_df$y-box_height, x1 = pids_df$x-box_width , y1 = pids_df$y+box_height, col = pids_df$p_cols)
    graphics::segments(x0 = pids_df$x+box_width , y0=pids_df$y-box_height, x1 = pids_df$x+box_width , y1 = pids_df$y+box_height, col = pids_df$p_cols)

    # record expansion
    pids_df2 <- merge(pids_df,
                      pids_df[c("sn","x","y")],
                      by.x = "link_id", by.y ="sn", all.x = T
    )

    pids_df2 <- pids_df2[pids_df2$link_id!=pids_df2$sn, ]
    graphics::arrows(length=0.1,angle=20, x0 =  pids_df2$x.y+.5, y0=pids_df2$y.y, x1 = pids_df2$x.x -.5, y1 = pids_df2$y.x, col = pids_df2$p_cols)

    # criteria boxes
    x_max <- lapply(split(pids_df$x, pids_df$pid_cri), max)
    x_min <- lapply(split(pids_df$x, pids_df$pid_cri), min)
    y_max <- lapply(split(pids_df$y, pids_df$pid_cri), max)
    y_min <- lapply(split(pids_df$y, pids_df$pid_cri), min)

    cri_borders <- data.frame(
      pid_cri = as.numeric(names(x_max)),
      x_max = as.numeric(x_max),
      y_max = as.numeric(y_max),
      x_min = as.numeric(x_min),
      y_min = as.numeric(y_min)
    )

    # margins (half)
    margin_l <- 1.5
    margin_r <- 1.5
    margin_t <- 1.5
    margin_b <- 1.5

    border_col <-"white"
    graphics::segments(x0 = cri_borders$x_min-margin_l , y0=cri_borders$y_max+margin_t, x1 = cri_borders$x_max+margin_r , y1 = cri_borders$y_max+margin_l, col = border_col)
    graphics::segments(x0 = cri_borders$x_min-margin_l , y0=cri_borders$y_min-margin_b, x1 = cri_borders$x_max+margin_r , y1 = cri_borders$y_min-margin_b, col = border_col)
    graphics::segments(x0 = cri_borders$x_min-margin_l , y0=cri_borders$y_max+margin_t, x1 = cri_borders$x_min-margin_l , y1 = cri_borders$y_min-margin_b, col = border_col)
    graphics::segments(x0 = cri_borders$x_max+margin_r , y0=cri_borders$y_max+margin_t, x1 = cri_borders$x_max+margin_r , y1 = cri_borders$y_min-margin_b, col = border_col)
    graphics::text(x= (cri_borders$x_max + cri_borders$x_min)/2, y= cri_borders$y_max + (margin_t * 1.7), label= ifelse(cri_borders$pid_cri==0, "No hit", paste0("Criteria ", cri_borders$pid_cri)), cex = .8, adj = c(.5, .5), col = border_col)

    plt <- grDevices::recordPlot()
    # graphics.off()
    # dev.off()
    return(plt)
  }
  if(strata ==TRUE){
    output <- lapply(unique(pids_df$pid), function(x){
      network(pids_df[pids_df$pid ==x,])
    })
  }else{
    output <- network(pids_df)
  }
  return(output)

}


space_out_y <- function(x_axis){
  rows_n <- length(x_axis)
  sn_change <- y_axis <- rep(1, rows_n)
  while (max(sn_change) ==1) {
    c <- diyar::compress_number_line(x_axis, deduplicate = F, collapse = T)

    ord <- lapply(split(1:rows_n, paste0(c@gid,"-", y_axis)), order)
    lk_sn <- lapply(split(y_axis, paste0(c@gid)), function(x){
      rep(max(x), length(x))
    })

    ord <- unsplit(ord, paste0(c@gid,"-", y_axis))
    lk_sn <- unsplit(lk_sn, paste0(c@gid))
    new_y_axis <- ifelse(ord==2, lk_sn+1, y_axis)

    sn_change <- ifelse(y_axis != new_y_axis,1,0)
    y_axis <- new_y_axis
  }

  return(y_axis)
}

space_out_yy <- function(x_axis){
  rows_n <- length(x_axis)
  sn_change <- y_axis <- rep(1, rows_n)
  while (max(sn_change) ==1) {
    x_r <- expand_number_line(as.number_line(x_axis), 2, "end")
    y_r <- expand_number_line(as.number_line(y_axis), 1, "end")

    lag <- function(x, by=1) c(rep(as.number_line(NA), by), x[1:(length(x)-by)])
    lead <- function(x, by=1) c(x[(by+1):length(x)], rep(as.number_line(NA), by))

    x_l <- overlap(x_r, lead(x_r)) & overlap(x_r, lag(x_r))
    y_l <- overlap(y_r, lead(y_r)) & overlap(y_r, lag(y_r))

    new_y_axis <- ifelse(x_l==T & y_l ==T & !duplicated(paste0(x_l, y_l)), y_axis+2, y_axis)

    sn_change <- ifelse(y_axis != new_y_axis,1,0)
    y_axis <- new_y_axis
  }

  return(y_axis)
}
