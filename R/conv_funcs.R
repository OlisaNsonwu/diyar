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

progress_bar <- function(n, d, max_width, msg){
  prop_complete <- n/d
  mx_l <- max(nchar(c(n, d)))
  d <- format(d, big.mark = ",", width = mx_l, scientific = FALSE)
  n <- format(n, big.mark = ",", width = mx_l, scientific = FALSE)
  pct_l <- paste0(msg,"; ", n, " of ", d, " record(s) completed.")

  status_width <- max_width - nchar(pct_l)
  bar_width <- floor(prop_complete*status_width)
  space_width <- status_width - bar_width

  status <-paste0(paste0(rep("-", bar_width), collapse = ""),
                  paste0(rep(" ", space_width), collapse = ""),
                  pct_l,
                  "\r")
  cat(status, "\r",sep="")
}

progress_txt <- function(n, d, msg){
  prop_complete <- n/d
  mx_l <- max(nchar(c(n, d)))
  d <- format(d, big.mark = ",", width = mx_l, scientific = FALSE)
  n <- format(n, big.mark = ",", width = mx_l, scientific = FALSE)
  pct_l <- paste0(msg,": ", n, " of ", d, " record(s) completed.")

  status <- paste0(pct_l,"\r")
  cat(status, "\r",sep="")
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
    if(tolower(e) == "l"){
      all(y %in% x & length(x)>1)
    }else if (tolower(e) == "g"){
      any(y %in% x)
    }
  }

  lsts <- lapply(split(data_source, cri), function(x, l=data_links){
    xlst <- rep(list(a =x[!duplicated(x)]), length(l))
    r <- list(ds = paste0(sort(unique(x)), collapse = ","))
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

ovr_chks <- function(date, window, mths, ord){
  x <- overlaps(date, window, methods = mths)
  ord[x %in% c(NA, FALSE)] <-  0
  rm(list = ls()[ls() != "ord"])
  as.numeric(ord)
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
  n[n == 0] <- "No hits"
  n[n == "-1"] <- "Skipped"
  lgk <- !n %in% c("Skipped", "No hits")
  n[lgk] <- paste0("CRI ", formatC(as.integer(n[lgk]), width = 3, flag = 0, format = "fg"))
  rm(lgk)
  return(n)
}

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
      y <- number_line(as.numeric(start_point(x)),
                       as.numeric(end_point(x)))
      y <- as.data.frame(y)
      y$id <- NULL
      y$gid <- NULL
      y$epid <- lar$epid
      y$wind_total <- lar$wind_total
      y$epid_total <- lar$epid_total
      y$y <- lar$y
      y$mid_y_lead <- lar$mid_y_lead
      y$pt_start <- lar$start
      y$pt_end <- lar$end
      y$pt_sn <- lar$sn
      y$ep_uni <- lar$episode_unit
      y$nl_nm <- "len"
      y$start_rl <- as.numeric(left_point(x))
      y$end_rl <- as.numeric(right_point(x))
      y$wind_nm_l <- paste0(winds$wind_nm, " length")
      y$bi_dir <- start_point(x) < lar$end & end_point(x) > lar$end
      y <- y[!is.na(y$start) & !is.na(y$end),]
      if(nrow(y) > 0){
        if(any(y$bi_dir)){
          v <- y[y$bi_dir,]
          v$start <- y$pt_end
          y$end <- y$pt_end
          y <- rbind(y, v)
          rm(v)
        }
      }else{
        y$lab_y <- y$nl_s <- y$nl_e <- y$nl_l <- numeric()
        y$episode <- character()
      }

      y$bi_dir <- NULL
      y$nl_l <- epid_lengths(number_line(y$pt_start,
                                         y$pt_end),
                             number_line(y$start,
                                         y$end),
                             "seconds")

      if(is_dt == TRUE){
        y$nl_l <- number_line(start_point(y$nl_l)/as.numeric(diyar::episode_unit[y$ep_uni]),
                              end_point(y$nl_l)/as.numeric(diyar::episode_unit[y$ep_uni]))
      }
      y$nl_s <- start_point(y$nl_l)
      y$nl_e <- end_point(y$nl_l)
      y$episode_unit <- y$ep_uni
      y$nl_l <- NULL
      y$lab_y <- (y$mid_y_lead + y$y)/2
      y
    })
  }else{
    lar <-  pltd[0, c("end", "start", "epid", "y", "wind_total", "epid_total", "episode_unit")]
    lar$wind_nm_l <- lar$nl_nm <- character()
    lar$start_rl <- lar$end_rl <- lar$pt_start <- lar$pt_end <- lar$lab_y <- lar$mid_y_lead <- lar$nl_s <- lar$nl_e <- numeric()
    lar$pt_sn <- integer()
    lar <- list(lar)
  }
  lar
}

scale_size <- function(size_lims, count_upper_lim, pts_n, decreasing = TRUE){
  unit_change <- (max(size_lims) - min(size_lims))/(count_upper_lim - 0)
  if(decreasing){
    max(size_lims) - (ifelse(pts_n > count_upper_lim, count_upper_lim, pts_n) * unit_change)
  }else{
    min(size_lims) + (ifelse(pts_n > count_upper_lim, count_upper_lim, pts_n) * unit_change)
  }
}

length_to_range <- function(lengths, date, from_last, episode_unit){
  if(!all(class(lengths) == "list")){
    len <- list(lengths)
  }else{
    len <- lengths
  }

  len <- lapply(len, function(x){
    if(class(x) != "number_line"){
      x <- number_line(0, x)
    }
    return(x)
  })

  if(any(from_last == T)) {
    len <- lapply(len, function(x){

      if(length(x) < length(from_last)){
        x <- rep(x, length(date))
      }
      x@start <- as.numeric(x@start)
      x[from_last] <- invert_number_line(x)[from_last]
      return(x)
    })
  }

  len <- lapply(len, function(x){
    if(length(x) == 1){
      x <- rep(x, length(date))
    }
    x <- epid_windows(date, x, names(diyar::episode_unit)[episode_unit])
    return(x)
  })
  return(len)
}

opt_level <- function(opt, mth, tr_mth){
  if(opt == "e") {
    tr_mth
  }else if(opt == "b"){
    ifelse(mth != tr_mth, paste0(mth, "|", tr_mth), mth)
  }else{
    mth
  }
}

pane_checks <- function(dates, windows){
  fnx <- function(x, int = dates){
    ovr_chks(rep(windows[[x]], length(int)), int, rep("overlap", length(int)), rep(x, length(int)))
  }

  checks <- as.matrix(sapply(as.numeric(seq_len(length(windows))), fnx))
  if(length(dates) == 1){
    checks <- t(checks)
  }
  ep_checks <- Rfast::rowMaxs(checks, value = TRUE)
  as.integer(ep_checks)
}

check_skips <- function(lgk, lead_skip_b4_len, cri, cr, tr_ep_int, vr, tr_int, int, case_nm){
  if(length(lgk[lgk]) > 0){
    ep_l_min_a <- Rfast::rowMinsMaxs(sapply(tr_ep_int, function(x) start_point(x[lgk])))
    ep_l_min_z <- Rfast::rowMinsMaxs(sapply(tr_ep_int, function(x) end_point(x[lgk])))
    ep_l_bounds_a <- start_point(tr_int[lgk])
    ep_l_bounds_z <- end_point(tr_int[lgk])

    ep_l_bounds_a <- ifelse(ep_l_min_a[1,] < ep_l_bounds_a, ep_l_min_a[1,], ep_l_bounds_a)
    ep_l_bounds_z <- ifelse(ep_l_min_z[2,] > ep_l_bounds_z, ep_l_min_z[2,], ep_l_bounds_z)

    epc_bnds <- suppressWarnings(
      number_line(
        l = ep_l_bounds_a,
        r = ep_l_bounds_z))

    ep_obds_checks <- suppressWarnings(overlap(int[lgk], epc_bnds))
    ep_obds_checks <- ifelse(is.na(ep_obds_checks), FALSE, ep_obds_checks)

    ref_period <- overlap(int, tr_int)
    ref_period <- ifelse(is.na(ref_period), FALSE, ref_period)
    skp_crxt <- cri[vr & !ref_period]
    skp_crxt <- skp_crxt[!duplicated(skp_crxt)]
    indx <- (ep_obds_checks &
               !cr[lgk] &
               cri[lgk] %in% skp_crxt &
               is.na(case_nm[lgk]))
    lgk <- which(lgk == TRUE)[indx == TRUE]
    return(lgk)
  }else{
    return(numeric())
  }
}

f_rbind <- function(x, y){
  if(length(x) == length(y)){
    rbind(x, y)
  }else{
    xm <- names(y)[!names(y) %in% names(x)]
    xp <- lapply(xm, function(i) rep(NA_real_, length(x[[1]])))
    names(xp) <- xm
    x2 <- as.data.frame(c(as.list(x), xp))

    ym <- names(x)[!names(x) %in% names(y)]
    yp <- lapply(ym, function(i) rep(NA_real_, length(y[[1]])))
    names(yp) <- ym
    y2 <- as.data.frame(c(as.list(y), yp))
    rbind(x2, y2)
  }
}

extract_3dot_lengths <- function(x){
  if(all(class(x) == "sub_criteria")){
    unlist(lapply(x, function(sc){
      extract_3dot_lengths(sc[[1]])
    }), use.names = FALSE)
  }else if(all(class(x) == "list")){
    unlist(lapply(x, length), use.names = FALSE)
  }else if(is.atomic(x)){
    length(x)
  }
}

dst_tab <- function(x, order_by_label = NULL, order_by_val = TRUE){
  y <- rle(as.vector(x))
  if(is.null(order_by_label) & isTRUE(order_by_val)){
    pos <- order(-y$lengths)
  }else{
    pos <- match(order_by_label, y$values)
    pos <- pos[!is.na(pos)]
  }
  y$values <- y$values[pos]
  y$lengths <- y$lengths[pos]
  y
}

date_strata_combi <- function(date, strata){
  if(class(date) == "number_line"){
    s <- list(date@start, date@.Data, strata)
  }else{
    s <- list(date, strata)
  }

  mx_n <- max(unlist(lapply(seq_len(length(s)), function(i){
    x <- s[[i]]
    x <- match(x, x[!duplicated(x)])
    floor(log10(max(x)))
  }), use.names = FALSE))

  date_strata_combi <- sapply(seq_len(length(s)), function(i){
    x <- s[[i]]
    x <- match(x, x[!duplicated(x)])
    log10(max(x))
    10^(mx_n+i) * x
  })

  date_strata_combi <- rowSums(date_strata_combi)
  return(date_strata_combi)
}
