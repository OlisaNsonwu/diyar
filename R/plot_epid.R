#' @title XXXX
#'
#' @description XXXX
#'
#' @aliases plot_epid
#'
#' @export
plot_epid <- function(epid, date, strata = NULL, case_length, recurrence_length, scale_fac=1){
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
  dfp$event_nm <- ifelse(dfp$case_nm!="Skipped", paste0(dfp$date, "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$win_id), "\n(reference)", ""), "\nevent"))
  dfp$e_y <- rep(1, nrow(dfp))

  dfp$r <- recurrence_length
  dfp$c <- case_length

  if(is.null(strata)){
    dfp$strata <- 1
  }else{
    dfp$strata <- strata
  }

  story <- function(dfp){
    xlims <- c(min(dfp$dt_a), (max(dfp$dt_z) + max(c(max(dfp$c), max(dfp$r)))))
    #xlims <- c(min(dfp$dt_a), (max(dfp$dt_z) + 5))
    #xlims <- c(min(dfp$dt_a), max(dfp$dt_z))

    # Colors for windows and episodes
    dfp$win_cols <- rev(pl_cols)[as.numeric(as.factor(dfp$win_id))]
    cols <- dfp$win_cols
    names(cols) <- dfp$win_id
    cols <- cols[!duplicated(cols)]
    dfp$epd_cols <- cols[as.character(dfp$epid)]

    par(bg="black")
    plot(y=dfp$e_y, x=dfp$dt_a, ylim=c((1 - (scale_fac * 1)), (1 + (scale_fac * 0.5))), xlim = xlims)
    #axis(1, col = 'white', col.axis = 'white', col.ticks = 'white', family = 'serif')

    for(i in 1:nrow(dfp)){
      points(y=dfp$e_y[i], x = dfp$dt_z[i], bg = rgb(47/255,117/255,181/255), col = rgb(47/255,117/255,181/255), pch = 21)
      lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), col = rgb(91/255,155/255,213/255))
      text(y=dfp$e_y[i] + (scale_fac * 0.1), x=mean(c(dfp$dt_a[i], dfp$dt_z[i])), labels = dfp$event_nm[i], adj =c(.5,.5), col = rgb(91/255,155/255,213/255))
    }

    # Case lengths
    cl <- dfp[dfp$sn %in% unique(dfp$win_id[dfp$win_nm=="Case"]),]
    cl$dt <- as.numeric(cl$dt_z)
    cl$end_dt <- as.numeric(cl$dt + cl$c)

    cl$y <- (cl$e_y + (scale_fac * 0.35)) - (1:nrow(cl) * (scale_fac * 0.03))
    cl$lab <- paste0("Case length\n(",cl$c,"-day\ndifference)")

    for(i in 1:nrow(cl)){
      arrows(y0=cl$y[i], x0 = cl$dt[i], x1 = cl$end_dt[i], col ="white")
      text(y=cl$y[i] + (scale_fac * 0.02) , x=mean(c(cl$dt[i], cl$end_dt[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))
    }

    # Recurrence lengths
    rl <- dfp[dfp$sn %in% unique(dfp$win_id[dfp$win_nm!="Case"]),]
    if(nrow(rl)>0){
      rl$dt <- as.numeric(rl$dt_z)
      rl$end_dt <- as.numeric(rl$dt + rl$r)

      rl$y <- (rl$e_y - (scale_fac * 0.2)) - (1:nrow(rl) * (scale_fac * 0.03))
      rl$lab <- paste0("Recurrence length\n(",rl$c,"-day\ndifference)")

      for(i in 1:nrow(rl)){
        arrows(y0=rl$y[i], x0 = rl$dt[i], x1 = rl$end_dt[i], lty=2, col ="white")
        text(y=rl$y[i] + (scale_fac * 0.02) , x=mean(c(rl$dt[i], rl$end_dt[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
      }
    }

    # windows
    win <- dfp[grepl("Case", dfp$case_nm) | !duplicated(dfp$win_id, fromLast = T),]
    win$dt_a <- as.numeric(lapply(split(win$dt_a, win$win_id), min)[as.character(win$win_id)])
    win$dt_z <- as.numeric(lapply(split(win$dt_z, win$win_id), max)[as.character(win$win_id)])
    win <- win[!duplicated(win$win_id),]
    win$y <- (win$e_y - .4)
    win$win_nm_l <- paste0("Window ", as.numeric(as.factor(win$win_id)), "\n(", tolower(win$win_nm), "\nwindow)")

    for(i in 1:nrow(win)){
      lines(y=rep(win$y[i],2), x = c(win$dt_a[i], win$dt_z[i]), col=win$win_col[i])
      lines(y=c(win$y[i], win$y[i] + (scale_fac * 0.02)), x = rep(win$dt_a[i],2), col=win$win_col[i])
      lines(y=c(win$y[i], win$y[i] + (scale_fac * 0.02)), x = rep(win$dt_z[i],2), col=win$win_col[i])
      text(y=win$y[i] - (scale_fac * 0.13), x=mean(c(win$dt_a[i], win$dt_z[i])), labels = win$win_nm_l[i], col=win$win_col[i], adj =c(.5,0))
    }

    # Episodes
    epd <- dfp[grepl("Case", dfp$case_nm) | !duplicated(dfp$epid, fromLast = T),]
    epd$dt_a <- as.numeric(lapply(split(epd$dt_a, epd$epid), min)[as.character(epd$epid)])
    epd$dt_z <- as.numeric(lapply(split(epd$dt_z, epd$epid), max)[as.character(epd$epid)])
    epd <- epd[!duplicated(epd$epid),]
    epd$y <- (epd$e_y - (scale_fac * 0.8))
    epd$win_nm_l <- paste0("Episode ", as.numeric(as.factor(epd$epid)), "")

    for(i in 1:nrow(epd)){
      lines(y=rep(epd$y[i],2), x = c(epd$dt_a[i], epd$dt_z[i]), col=epd$epd_cols[i])
      lines(y=c(epd$y[i], epd$y[i] + (scale_fac * 0.02)), x = rep(epd$dt_a[i],2), col=epd$epd_cols[i])
      lines(y=c(epd$y[i], epd$y[i] + (scale_fac * 0.02)), x = rep(epd$dt_z[i],2), col=epd$epd_cols[i])
      text(y=epd$y[i] - (scale_fac * 0.1), x=mean(c(epd$dt_a[i], epd$dt_z[i])), labels = epd$win_nm_l[i], col=epd$epd_cols[i], adj =c(.5,0))
    }

    plt <- recordPlot()
    plot.new()
    return(plt)
  }

  p <- lapply(unique(dfp$strata), function(x){
    story(dfp[dfp$strata==x,])
  })

  names(p) <- unique(dfp$strata)
  return(p)
}
