#' @name plots
#' @aliases plots
#' @title Plots
#'
#' @description Plots
#' @param pids \code{pid} object
#' @param title Title of \code{ggplot} object
#' @return \code{ggplot}
#' @details
# ' text
#'
#' @examples
#' @export
plot_pids <- function(pids, title = NULL){
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
      y <- seq(left_point(y), right_point(y), length.out = length(x))
      y[1] <- y[1] + (bw * .1)
      if(length(y) > 1){
        y[length(y)] <- y[length(y)] + (bw * -.1)
        y <- sample(y, length(y))
      }
      y
    }), use.names = F)
    sn <- unlist(split(seq_len(nrow(dt)), dt$pid_box), use.names = F)
    pts <- pts[sort(sn)]
    pts
  }

  pl_dt$x <- cords("x")
  pl_dt$y <- cords("y")

  link_sn <- pl_dt[pl_dt$sn %in% pl_dt$link_id, c("sn", "x", "y")]
  pl_dt$x_lead <- pl_dt$y_lead <- NULL

  pl_dt$x_lead <- link_sn$x[match(pl_dt$link_id, link_sn$sn)]
  pl_dt$y_lead <- link_sn$y[match(pl_dt$link_id, link_sn$sn)]

  pl_dt$pid <- as.character(pl_dt$pid)

  border$pid_cri <- pl_dt$pid_cri[match(border$pid_box, pl_dt$pid_box)]
  tx_l <- min(pl_dt$x1)
  tx_h <- max(pl_dt$y2)

  f <- ggplot2::ggplot(data = pl_dt) +
    ggplot2::geom_point(ggplot2::aes(x = x, y= y, colour = pid), size = boxes_w * .15, show.legend=FALSE) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y= y, colour = pid, xend = x_lead, yend = y_lead), alpha = .25, size = boxes_w * .08, show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(x = x, y= y, colour = pid, label = sn), nudge_x = boxes_w * .025, size = boxes_w * .3, show.legend=FALSE) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x1, xmax = x2, ymin = y1, ymax =y2,  fill = pid_cri), data = border, alpha = .1) +
    ggplot2::geom_text(ggplot2::aes(x = (x1 + x2)/2, y= (y1 + y2)/2, colour = pid_cri, label = pid_cri), size = boxes_w * 3, alpha = .1, data = border, show.legend = FALSE)

  if(!is.null(title)){
    f <- f + ggplot2::geom_text(ggplot2::aes(x = tx_l - (boxes_w * .06), y= tx_h + (boxes_w * .06)), colour = "white", label = title, size = boxes_w * .6, alpha = .1, show.legend=FALSE)
  }
  f <- f +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 10)) +
    ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = "black"),
      legend.text = ggplot2::element_text(colour = "white"),
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = "black", colour = "grey"),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
  f
}
