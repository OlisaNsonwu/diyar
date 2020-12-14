# @title Convenience functions
#
# @description Convenience functions
#
# @param x x
# @aliases finite_check
#
finite_check <- function(x, lim =10){
  e <- which(!is.finite(as.numeric(x)))
  if(length(e) %in% 1:lim) {
    paste0("[",listr(format(e, scientific = F)),"]")
  }else if(length(e) > lim) {
    paste0("[",paste0(format(e[1:lim]), collapse = ", "),", ...]")
  }else if(length(e) == 0)
    TRUE
}

missing_check <- function(x, lim =10){
  e <- which(is.na(x))
  if(length(x[e]) %in% 1:lim) {
    paste0("[", listr(format(e, scientific = F)), "]")
  }else if(length(x[e]) > lim) {
    paste0("[", paste0(format(e[1:lim], scientific = F), collapse = ", "),", ...]")
  }else{
    T
  }

}

# @rdname finite_check
enq_vr <- function(x){
  x <- as.character(x)
  if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
  if(length(x)==0) x <- NULL
  x
}

# @rdname finite_check
fmt <- function(x) formatC(x, format="d", big.mark=",")

# @rdname finite_check
duplicates_check <- function(x){
  pos <- 1:length(x)
  lgk <- duplicated(x)
  dups <- x[lgk]
  dups <- dups[!duplicated(dups)]

  lgk2 <- x %in% head(dups, 3)
  y <- x[lgk2]
  y_pos <- pos[lgk2]

  y <- unlist(lapply(split(y_pos, y), function(x){
    ifelse(length(x)>3,
           paste0("[",paste0(x[1:3], collapse = ", "),",..]"),
           paste0("[",paste0(x, collapse = ", "),"]"))
  }), use.names = F)


  if(length(dups)>3){
    paste0(paste0(y, collapse=", "), " ...")
  }else if (length(y)!=0) {
    listr(y)
  }else{
    TRUE
  }
}
# @rdname finite_check
logicals_check <- function(x){
  log_vals <-  lapply(x, function(y){
    is.logical(eval(parse(text=y), envir = parent.frame(3)))
  })

  log_vals <- x[as.logical(log_vals)==F]
  if(length(log_vals) ==0){
    TRUE
  }else{
    paste0(listr(paste0("'",log_vals,"'")), " must be either TRUE or FALSE")
  }
}

# @rdname finite_check
sep_bdr_nl <- function(x){
  nl_prp <- function(i){

    x <- x[[i]]
    if(!is.number_line(x)){
      x <- as.number_line(x)
      left_point(x) <- 0
    }

    x@start <- as.numeric(x@start)
    x <- reverse_number_line(x, "decreasing")
    crx <- x@start/abs(x@start) != diyar::right_point(x)/abs(diyar::right_point(x))
    crx[is.na(crx)] <- F

    nl_a <- x
    left_point(nl_a[crx]) <- 0

    lst <- list(nl_a)
    if(any(crx)){
      nl_b <- x
      right_point(nl_b[crx]) <- 0
      lst[[2]] <- nl_b
    }

    names(lst) <- rep(i, length(lst))
    lst
  }

  b <- lapply(1:length(x), nl_prp)
  txt <- paste0("c(",paste0("b[[",1:length(b),"]]", collapse = ", " ), ")")
  b <- eval(parse(text=txt))
  return(b)
}

progress_bar <- function(prop_complete, max_width, msg){
  pct_l <- formatC(prop_complete*100, digits = 5, format = "fg")
  a <- gsub("\\s", "", pct_l)
  b <- gsub("\\S", "", pct_l)
  b <- ifelse(b==""," ", b)
  pct_l <- paste0(msg,"; ", ifelse(prop_complete == 1, "100", a),"%", b, "complete.")
  status_width <- max_width - nchar(pct_l)
  bar_width <- as.integer(prop_complete*status_width)
  space_width <- status_width - bar_width

  status <-paste0(ifelse(bar_width < 0, "", paste0(rep("-", bar_width), collapse = "")),
                  ifelse(space_width < 0, "", paste0(rep(" ", space_width), collapse = "")),
                  pct_l,
                  "\r")

  if(prop_complete==1){
    # cat(paste0("\033[0;32m",status,"\033[0m"))
    cat(status, "\r",sep="")
  }else{
    cat(status, "\r",sep="")
  }
}

datasets_xx <- function(by, val, sep = ","){
  #by_uniq <- by[!duplicated(by)]

  datasets <- split(by, val)

  datasets <- lapply(1:length(datasets), function(x){
    ifelse(by %in% datasets[[x]], names(datasets)[x], NA_character_)
  })

  ds <- rep("", length(by))
  for(i in 1:length(datasets)){
    ds <- ifelse(!is.na(datasets[[i]]),
                 ifelse(ds =="",
                        paste(ds, datasets[[i]], sep=""),
                        paste(ds, datasets[[i]], sep=",")),
                 ds)
  }
  ds
  #ds[match(by, by_uniq)]
}

datasets <- function(by, val, sep = ","){
  datasets <- split(val, by)
  func <- function(x) paste0(sort(x[!duplicated(x)]), collapse = sep)
  datasets <- lapply(datasets, func)
  #datasets <- as.character(datasets)[match(by, names(datasets))]
  datasets <- unlist(datasets[match(by, names(datasets))], use.names = F)
  datasets
}


check_links <- function(cri, data_source, data_links){
  dl_lst <- unlist(data_links, use.names = F)
  func <- function(x, y, e) {
    if(tolower(e)=="l"){
      all(y %in% x & length(x)>1)
    }else if (tolower(e)=="g"){
      any(y %in% x)
    }
  }

  lsts <- lapply(split(data_source, cri), function(x, l=data_links){
    xlst <- rep(list(a =x[!duplicated(x)]), length(l))
    r <- list(ds = paste0(sort(unique(x)), collapse=","))
    if(!all(toupper(dl_lst) == "ANY")) r["rq"] <- any(unlist(mapply(func, xlst, l, names(l), SIMPLIFY = F)))
    return(r)
  })

  dset <- lapply(lsts, function(x){x$ds})
  dset <- unlist(dset[match(cri, names(dset))], use.names = F)
  r <- list(ds=dset)
  if(!all(toupper(dl_lst) == "ANY")){
    dlks <- lapply(lsts, function(x){x$rq})
    dlks <- unlist(dlks[match(cri, names(dlks))], use.names = F)
    r$rq <- dlks
  }
  r
}

prep_lengths <- function(length, overlap_methods, int,
                         episode_unit, bi_direction,
                          #from_last,
                         include_index_period = F){
  length <- length
  if(class(length) != "list") length <- list(length)
  if(class(overlap_methods) != "list") overlap_methods <- list(overlap_methods)

  if(length(overlap_methods)==1) overlap_methods <- rep(overlap_methods, length(length))

  length <- sep_bdr_nl(length)
  r <- rle(names(length))
  overlap_methods <- rep(overlap_methods[as.numeric(r$values)], r$lengths)

  if(any(bi_direction == T)){
    is_bdr <- names(length) %in% r$values[r$lengths == 2]
    n_length  <- length[!is_bdr]
    if(length(n_length) > 0){
      n_length <- lapply(n_length, function(x){
        x[bi_direction] <- invert_number_line(x)[bi_direction]
        return(x)
        })
      length <- c(n_length, length)
      overlap_methods <- c(overlap_methods[!is_bdr], overlap_methods)
    }
  }

  overlap_methods <- lapply(overlap_methods, function(x){
    if(length(x)==1){
      x <- rep(x, length(int))
    }else{
      x
    }
  })

  list(
    lengths = length,
    method = overlap_methods
  )
}

ovr_chks <- function(tr, int, mths){
  x <- overlaps(tr, int, methods = mths)
  ifelse(is.na(x), F, x)
}

overlaps_err <- function(opts){
  opts <- tolower(opts)
  sn <- 1:length(opts)
  opts <- split(sn , opts)

  # All possible combinations
  funx <- function(v){
    v <- v[!duplicated(v)]
    lapply(seq_len(length(v)), function(i){
      shuffle <- c(0:(i-1), i+1, i, (i+2):length(v))
      shuffle <- shuffle[shuffle %in% 1:length(v)]
      shuffle <- shuffle[!duplicated(shuffle)]
      v[shuffle]
    })
  }

  m <- c("none", "exact", "across","chain","aligns_start","aligns_end","inbetween", "overlap", "reverse")
  pos <- 1:length(m)
  all_pos <- lapply(pos, function(i){
    utils::combn(pos, m =i, simplify = F, FUN = funx)
  })
  all_pos <- unlist(unlist(all_pos, recursive = F), recursive = F)
  all_pos <- sapply(all_pos, function(x){
    paste0(m[x],collapse="|")
  })
  #all_pos[duplicated(all_pos)]

  opts <- opts[!names(opts) %in% all_pos]
  opts_len <- length(opts)
  opts <- head(opts, 5)

  names(opts) <- sapply(strsplit(names(opts), split="\\|"), function(x){
    paste0(x[!x %in% m], collapse = "|")
  })

  opts <- unlist(lapply(opts, function(x){
    missing_check(ifelse(sn %in% x, NA, T), 2)
  }), use.names = T)

  if(length(opts) > 0){
    opts <- paste0("\"", names(opts),"\"", " at ", opts)
    if(opts_len >3) errs <- paste0(paste0(opts, collapse = ", "), " ...") else errs <- listr(opts)
    return(errs)
  }else{
    return(character())
  }
}

invalid_opts <- function(vals, opts){
  sn <- 1:length(vals)
  vals <- split(sn , vals)

  f1 <- function(x) paste0(x, collapse="|")
  all_pos <- combns(opts, seq_len(length(opts)), FUN = f1)

  vals <- vals[!names(vals) %in% all_pos]
  vals_len <- length(vals)
  vals <- head(vals, 5)

  names(vals) <- sapply(strsplit(names(vals), split="\\|"), function(x){
    paste0(x[!x %in% opts | is.na(x)], collapse = "|")
  })

  vals <- unlist(lapply(vals, function(x){
    missing_check(ifelse(sn %in% x, NA, T), 2)
  }), use.names = T)

  if(length(vals) > 0){
    vals <- paste0("\"", names(vals),"\"", " at ", vals)
    if(vals_len >3) errs <- paste0(paste0(vals, collapse = ", "), " ...") else errs <- listr(vals)
    return(errs)
  }else{
    return(character())
  }
}

pid_cri_l <- function(n){
  ifelse(n == 0,"No Hits",
         ifelse(n == -1, "Skipped",
                paste0("CRI ", formatC(n, width = 3, flag = 0, format = "fg"))))
}

#' @name combns
#' @aliases combns
#' @title Generate every permutation of n elements.
#' @description An extension of \code{\link{combn}} to generate permutations not ordinarily captured by \code{\link{combn}}.
#' Each argument should be used as would be used in \code{\link{combn}}.
#'
#' @param x Vector source for combination.
#' @param m Number of elements required. Multiple counts can be supplied.
#' @param FUN Function applied to each combination.
#' @param simplify Logical indicating if the result should be simplified to an array or returned as a list.
#' @param ... further arguments passed to FUN. Optional.
#'
#' @details
#' \bold{\code{combns}} - Return every possible permutation of . An extension of \code{combn}.
#'
#' @return \code{\link{number_line}}.
#'
#' @examples
#' f1 <- function(x) paste0(x, collapse = ",")
#' combn(x = 1:3, m = 3, FUN = f1, simplify = TRUE)
#' combns(x = 1:3, m = 3, FUN = f1, simplify = TRUE)
#' combns(x = 1:3, m = 1:3, FUN = f1, simplify = TRUE)
#'
#' @export
combns <- function(x, m, FUN = NULL, simplify = TRUE, ...){
  funx <- function(v){
    v <- v[!duplicated(v)]
    lapply(seq_len(length(v)), function(i){
      shuffle <- c(0:(i-1), i+1, i, (i+2):length(v))
      shuffle <- shuffle[shuffle %in% 1:length(v)]
      shuffle <- shuffle[!duplicated(shuffle)]
      v[shuffle]
    })
  }

  pos <- seq_len(length(x))
  all_pos <- lapply(m, function(i){
    utils::combn(pos, m = i, simplify = F, FUN = funx)
  })
  all_pos <- unlist(unlist(all_pos, recursive = F), recursive = F)
  all_pos <- lapply(all_pos, function(y){x[y]})

  if(all(class(FUN) == "NULL")){
    func <- function(x, ...) x
  }else{
    func <- function(x, ...) FUN(x, ...)
  }
  all_pos <- sapply(all_pos, func, simplify = simplify)
  return(all_pos)
}


#' @name listr
#' @aliases listr
#' @title Written lists.
#' @description A convenience function to format \code{atomic} vectors as a written list.
#'
#' @param x \code{atomic} vector.
#' @param sep Separator.
#' @param conj Final separator.
#' @param lim Elements to include in the list. Other elements are abbreviated to \code{" ..."}.
#'
#'
#' @return \code{character}.
#'
#' @examples
#' listr(1:5)
#' listr(1:5, sep = "; ")
#' listr(1:5, sep = "; ", conj = " and")
#' listr(1:5, sep = "; ", conj = " and", lim = 2)
#'
#' @export
listr <- function(x, sep = ", ", conj = " and", lim = Inf){
  err <- err_atomic_vectors(x, "x")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(sep, "sep", c("character"))
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(sep, "", 1, "sep")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(conj, "conj", c("character"))
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(conj, "", 1, "conj")
  if(err != F) stop(err, call. = F)
  err <- err_object_types(lim, "lim", c("integer", "numeric"))
  if(err != F) stop(err, call. = F)
  err <- err_match_ref_len(lim, "", 1, "lim")
  if(err != F) stop(err, call. = F)

  if(length(x) <= lim){
    p <- x[length(x)]
    f <- x[1:(length(x)-1)]
    f <- paste(f, collapse = sep)
    x <- ifelse(length(x) > 1, paste0(f, conj, " ", p), f)
  }else{
    x <- paste0(paste0(x[seq_len(lim)], collapse = sep), " ...")
  }

  return(x)
}

sb_h <- function(x, by) {
  x$x1 <- x$x1 + by
  x$x2 <- x$x2 + by
  x
}

sb_v <- function(x, by) {
  x$y1 <- x$y1 + by
  x$y2 <- x$y2 + by
  x
}


box_set <- function(x = box, n, by){
  mapply(sb_h, rep(x, n), (seq_len(n)-1) * by, SIMPLIFY = F)
}

box_45d <- function(y){
  x <- y
  ref_pt <- nrow(y)
  spacing <- c(0, abs(x$x2[1:(length(x$x1)-1)] - x$x1[-1]))
  y_int <- (((seq_len(nrow(x))-1) * (x$y2 - x$y1)) + ((seq_len(nrow(x))-1) * spacing))
  y$x1 <- x$x1[ref_pt]
  y$x2 <- x$x2[ref_pt]
  y$y1 <- x$y1 - y_int
  y$y2 <- x$y2 - y_int

  return(y)
}

box_45l <- function(y){
  x <- y
  ref_pt <- nrow(y)
  spacing <- c(0, abs(x$y1[1:(length(x$y1)-1)] - x$y2[-1]) )
  x_int <- (((seq_len(nrow(x))-1) * (x$x2 - x$x1)) + ((seq_len(nrow(x))-1) * spacing))
  y$y1 <- x$y1[ref_pt]
  y$y2 <- x$y2[ref_pt]
  y$x1 <- x$x1 - x_int
  y$x2 <- x$x2 - x_int
  y <- y[order(y$x1),]
  return(y)
}

box_45u <- function(y){
  x <- y
  ref_pt <- 1
  spacing <- c(0, abs(x$x2[1:(length(x$x1)-1)] - x$x1[-1]))
  y_int <- (((seq_len(nrow(x))-1) * (x$y2 - x$y1)) + ((seq_len(nrow(x))-1) * spacing))
  y$x1 <- x$x1[ref_pt]
  y$x2 <- x$x2[ref_pt]
  y$y1 <- x$y1 + y_int
  y$y2 <- x$y2 + y_int
  y <- y[order(-y$y1),]
  return(y)
}

box_45r <- function(y){
  x <- y
  ref_pt <- 1
  spacing <- c(0, abs(x$y1[1:(length(x$y1)-1)] - x$y2[-1]) )
  x_int <- (((seq_len(nrow(x))-1) * (x$x2 - x$x1)) + ((seq_len(nrow(x))-1) * spacing))
  y$y1 <- x$y1[ref_pt]
  y$y2 <- x$y2[ref_pt]
  y$x1 <- x$x1 + x_int
  y$x2 <- x$x2 + x_int
  return(y)
}

box <- function(by){
  data.frame(x1 = 1, x2 = by, y1 = 1, y2 = by)
}

box_ring <- function(boxes_w = 3, order = 1){
  boxes_n <- order * 2
  boxes <- box_set(x = list(box(boxes_w)), n = boxes_n, by = boxes_w)
  h_boxes <- do.call(rbind, boxes)
  top_box <- sb_h(sb_v(h_boxes, boxes_w * order), boxes_w * -(order-1))
  boxes <- rbind(
    top_box
    ,sb_v(box_45d(top_box), -boxes_w)
    ,sb_h(box_45l(sb_v(box_45d(top_box), -boxes_w)), -boxes_w)
    ,sb_v(box_45u(sb_h(box_45l(sb_v(box_45d(top_box), -boxes_w)), -boxes_w)), boxes_w)
  )
  boxes
}

l_ar <- function(lens, pltd, wind_nm, is_dt, epid_unit){
  winds <- pltd[!duplicated(pltd$wind_id) &  pltd$wind_nm != "Skipped" & pltd$wind_nm == wind_nm,]
  lgk <- pltd$sn %in% winds$wind_id & pltd$case_nm != "Skipped"
  lar <- pltd[lgk,]
  lens <- lapply(lens, function(x) x[lgk])
  rec_ls <- nrow(lar)
  if(rec_ls >0){
    lar$mid_y_lead <- pltd$mid_y[match(lar$sn, pltd$wind_id)]
    lar <- lapply(lens, function(x){
      y <- x
      left_point(y) <- as.numeric(left_point(x))
      right_point(y) <- as.numeric(right_point(x))

      # y <- number_line(as.numeric(left_point(x)),
      #                  as.numeric(right_point(x)))
      y <- to_df(y[match(y@id, lar$sn)])

      y$id <- NULL
      y$gid <- NULL
      y$epid <- lar$epid
      y$wind_total <- lar$wind_total
      y$y <- lar$y
      y$mid_y_lead <- lar$mid_y_lead
      y$nl_nm <- "len"
      lar$nl_nm <- "dts"
      lar$wind_nm_l <- ""
      y$wind_nm_l <- ifelse(winds$wind_nm == "Case", "Case length", "Recurrence length")

      y$nl_l <- epid_lengths(number_line(lar$start,
                                         lar$end),
                             number_line(y$start,
                                         y$end),
                             "seconds")

      if(is_dt == TRUE){
        y$nl_l <- number_line(left_point(y$nl_l)/as.numeric(diyar::episode_unit[epid_unit]),
                              right_point(y$nl_l)/as.numeric(diyar::episode_unit[epid_unit]))
      }
      y$nl_s <- left_point(y$nl_l)
      y$nl_e <- right_point(y$nl_l)

      lar$nl_s <- y$nl_s
      lar$nl_e <- y$nl_e
      y$nl_l <- NULL
      y <- rbind(y, lar[c("end", "start", "epid", "y", "mid_y_lead", "nl_s", "nl_e", "nl_nm", "wind_nm_l", "wind_total")])
      y$lab_y <- (y$mid_y_lead + y$y)/2
      y
    })
  }else{
    lar <-  pltd[0, c("end", "start", "epid", "y", "wind_total")]
    lar$wind_nm_l <- lar$nl_nm <- character()
    lar$lab_y <- lar$mid_y_lead <- lar$nl_s <- lar$nl_e <- numeric()
    lar <- list(lar)
  }
  lar
}

scale_size <- function(size_lims, count_upper_lim, pts_n ){
  unit_change <- (max(size_lims) - min(size_lims))/(count_upper_lim - 0)
  s <- max(size_lims) - (ifelse(pts_n > count_upper_lim, count_upper_lim, pts_n) * unit_change)
  s
}

