#' @title XXXX
#'
#' @description XXXX
#'
#' @aliases plot_epid
#'
#' @export
plot_epid <- function(epid, date, strata = NULL, case_length, recurrence_length = NULL, scale_fac=1){
  scale_fac <- 1

  pl_cols <- colours()
  pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,5))]

  dfp <- to_df(epid)
  dfp$date <- date

  if(is.number_line(dfp$date)){
    dfp$dt_a <- left_point(dfp$date)
    dfp$dt_z <- right_point(dfp$date)
  }else{
    dfp$dt_a <- dfp$date
    dfp$dt_z <- dfp$date
  }

  # Events
  dfp$event_nm <- ifelse(dfp$case_nm!="Skipped", paste0(format(dfp$date), "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$win_id), "\n(reference)", ""), "\nevent"))
  e_y <- 1
  dfp$alt <- ifelse(1:nrow(dfp) %% 2 ==0, -1,1)

  dfp$e_y <- e_y - (1:nrow(dfp) * (scale_fac * 0.01))
  dfp$c <- case_length
  dfp$r <- recurrence_length
  if(is.null(recurrence_length)){
    dfp$r <- 0
  }else{
    dfp$r <- recurrence_length
  }

  if(is.null(strata)){
    dfp$strata <- 1
  }else{
    dfp$strata <- strata
  }

  story <- function(dfp){
    dfp <- dfp[order(dfp$dt_a, dfp$dt_z),]
    xlims <- c((min(dfp$dt_a) - max(c(max(dfp$c), max(dfp$r)))), (max(dfp$dt_z) + max(c(max(dfp$c), max(dfp$r)))))

    # Colors for windows and episodes
    dfp$win_cols <- rev(pl_cols)[as.numeric(as.factor(dfp$win_id))]
    cols <- dfp$win_cols
    names(cols) <- dfp$win_id
    cols <- cols[!duplicated(cols)]
    dfp$epd_cols <- cols[as.character(dfp$epid)]

    par(bg="black")
    plot(y=dfp$e_y, x=dfp$dt_a, ylim=c((1 - (scale_fac * 1)), (1 + (scale_fac * 0.5))), xlim = xlims)

    dfp$windows_y_axis <- rep((e_y - (scale_fac * 0.4)), nrow(dfp))
    dfp$episode_y_axis <- rep((e_y - (scale_fac * 0.8)), nrow(dfp))
    dfp$case_len_y_axis <- rep((e_y + (scale_fac * 0.35)), nrow(dfp))
    dfp$rec_len_y_axis <- rep((e_y - (scale_fac * 0.2)), nrow(dfp))

    for(i in 1:nrow(dfp)){
      points(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), bg = rgb(47/255,117/255,181/255), col = rgb(47/255,117/255,181/255), pch = 21)
      lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), col = rgb(91/255,155/255,213/255))
      lines(y=c(dfp$case_len_y_axis[i], dfp$case_len_y_axis[i] - (scale_fac * 0.15) ), x = rep(dfp$dt_z[i],2), col = rgb(47/255,117/255,181/255), lty = 2)
      lines(y=c(dfp$case_len_y_axis[i], dfp$case_len_y_axis[i] - (scale_fac * 0.15) ), x = rep(dfp$dt_a[i],2), col = rgb(47/255,117/255,181/255), lty = 2)
      lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$episode_y_axis[i]), x = rep(dfp$dt_z[i],2), col = rgb(47/255,117/255,181/255), lty = 2)
      lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$episode_y_axis[i]), x = rep(dfp$dt_a[i],2), col = rgb(47/255,117/255,181/255), lty = 2)
      text(y=dfp$e_y[i] + (scale_fac * 0.1), x=mean(c(dfp$dt_a[i], dfp$dt_z[i])), labels = dfp$event_nm[i], adj =c(.5,.5), col = rgb(91/255,155/255,213/255))
    }

    dfp$e_dt_a <- as.numeric(lapply(split(dfp$dt_a, dfp$epid), min)[as.character(dfp$epid)])
    dfp$e_dt_z <- as.numeric(lapply(split(dfp$dt_z, dfp$epid), max)[as.character(dfp$epid)])

    # case lengths
    cl <- dfp[dfp$sn %in% unique(dfp$win_id[dfp$win_nm=="Case"]),]
    cl$dt <- as.numeric(cl$dt_z)
    cl$end_dt <- as.numeric(cl$dt + cl$c)

    cl$dt2 <- as.numeric(cl$dt_a)
    cl$end_dt2 <- as.numeric(cl$dt2 - cl$c)

    # shift each case_len arrow 1 step down
    # restart it per episode later
    cl$case_len_y_axis <- cl$case_len_y_axis - (1:nrow(cl) * (scale_fac * 0.03))
    cl$lab <- paste0("Case length\n(",cl$c,"-day\ndifference)")

    for(i in 1:nrow(cl)){
      arrows(y0=cl$case_len_y_axis[i], x0 = cl$dt[i], x1 = cl$end_dt[i], col ="white")
      text(y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(cl$dt[i], cl$end_dt[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))

      if(cl$dt[i]!= cl$e_dt_a[i]){
        arrows(y0=cl$case_len_y_axis[i], x0 = cl$dt2[i], x1 = cl$end_dt2[i], col ="white")
        text(y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(cl$dt2[i], cl$end_dt2[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))
      }
    }

    # Recurrence lengths
    rl <- dfp[dfp$sn %in% unique(dfp$win_id[dfp$win_nm!="Case"]),]
    if(nrow(rl)>0 & !is.null(recurrence_length)){
      rl$dt <- as.numeric(rl$dt_z)
      rl$end_dt <- as.numeric(rl$dt + rl$r)

      rl$dt2 <- as.numeric(rl$dt_a)
      rl$end_dt2 <- as.numeric(rl$dt2 - rl$r)

      # shift each recurrence_len arrow 1 step down
      # restart it per episode later
      rl$rec_len_y_axis <- rl$rec_len_y_axis - (1:nrow(rl) * (scale_fac * 0.03))
      rl$lab <- paste0("Recurrence length\n(",rl$c,"-day\ndifference)")

      for(i in 1:nrow(rl)){
        arrows(y0=rl$rec_len_y_axis[i], x0 = rl$dt[i], x1 = rl$end_dt[i], lty=2, col ="white")
        text(y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(rl$dt[i], rl$end_dt[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
      }

      if(rl$dt[i]!= rl$e_dt_a[i]){
        arrows(y0=rl$rec_len_y_axis[i], x0 = rl$dt2[i], x1 = rl$end_dt2[i], col ="white")
        text(y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(rl$dt2[i], rl$end_dt2[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
      }
    }

    # Windows
    win <- dfp
    win$w_dt_a <- as.numeric(lapply(split(win$dt_a, win$win_id), min)[as.character(win$win_id)])
    win$w_dt_z <- as.numeric(lapply(split(win$dt_z, win$win_id), max)[as.character(win$win_id)])
    win <- win[!duplicated(win$win_id),]

    win$win_nm_l <- paste0("Window ", as.numeric(as.factor(win$win_id)), "\n(", tolower(win$win_nm), "\nwindow)")
    for(i in 1:nrow(win)){
      lines(y=rep(win$windows_y_axis[i],2), x = c(win$w_dt_a[i], win$w_dt_z[i]), col=win$win_col[i])
      lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.03)), x = rep(win$w_dt_a[i],2), col=win$win_col[i])
      lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.03)), x = rep(win$w_dt_z[i],2), col=win$win_col[i])
      text(y=win$windows_y_axis[i] - (scale_fac * 0.13), x=mean(c(win$w_dt_a[i], win$w_dt_z[i])), labels = win$win_nm_l[i], col=win$win_col[i], adj =c(.5,0))
    }

    # Episodes
    epd <- dfp
    epd <- epd[!duplicated(epd$epid),]
    epd$win_nm_l <- paste0("Episode ", as.numeric(as.factor(epd$epid)), "")

    for(i in 1:nrow(epd)){
      lines(y=rep(epd$episode_y_axis[i],2), x = c(epd$e_dt_a[i], epd$e_dt_z[i]), col=epd$epd_cols[i])
      lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.03)), x = rep(epd$e_dt_a[i],2), col=epd$epd_cols[i])
      lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.03)), x = rep(epd$e_dt_z[i],2), col=epd$epd_cols[i])
      text(y=epd$episode_y_axis[i] - (scale_fac * 0.1), x=mean(c(epd$e_dt_a[i], epd$e_dt_z[i])), labels = epd$win_nm_l[i], col=epd$epd_cols[i], adj =c(.5,0))
    }

    plt <- recordPlot()
    plot.new()
    return(plt)
  }

  p <- lapply(unique(dfp$strata), function(x){
    story(dfp[dfp$strata==x,])
  })

  names(p) <- unique(dfp$strata)
  if(length(p)==1) p <- p[[1]]
  return(p)
}
