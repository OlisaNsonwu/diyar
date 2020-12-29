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
  pct_l <- paste0(msg,"; ", n, " of ", d, " completed.")

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
  pct_l <- paste0(msg,"; ", n, " of ", d, " completed.")

  status <-paste0(pct_l,"\r")
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

ovr_chks <- function(tr, int, mths, ord){
  x <- overlaps(tr, int, methods = mths)
  ord[is.na(x)] <-  0
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
      y <- number_line(as.numeric(left_point(x)),
                       as.numeric(right_point(x)),
                       id = x@id,
                       gid = x@gid)
      # y <- x
      # left_point(y) <- as.numeric(left_point(x))
      # right_point(y) <- as.numeric(right_point(x))
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
        y$nl_l <- number_line(left_point(y$nl_l)/as.numeric(diyar::episode_unit[lar$episode_unit]),
                              right_point(y$nl_l)/as.numeric(diyar::episode_unit[lar$episode_unit]))
      }
      y$nl_s <- left_point(y$nl_l)
      y$nl_e <- right_point(y$nl_l)
      y$episode_unit <- lar$episode_unit
      lar$nl_s <- y$nl_s
      lar$nl_e <- y$nl_e
      y$nl_l <- NULL
      y <- rbind(y, lar[c("end", "start", "epid", "y", "mid_y_lead", "nl_s", "nl_e", "nl_nm", "wind_nm_l", "wind_total", "episode_unit")])
      y$lab_y <- (y$mid_y_lead + y$y)/2
      y
    })
  }else{
    lar <-  pltd[0, c("end", "start", "epid", "y", "wind_total", "episode_unit")]
    lar$wind_nm_l <- lar$nl_nm <- character()
    lar$lab_y <- lar$mid_y_lead <- lar$nl_s <- lar$nl_e <- numeric()
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

length_to_range <- function(lengths, int, from_last, ep_units){
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
      x@start <- as.numeric(x@start)
      x[from_last] <- invert_number_line(x)[from_last]
      return(x)
    })
  }

  len <- lapply(len, function(x){
    if(length(x) == 1){
      x <- rep(x, length(int))
    }
    x <- epid_windows(int, x, ep_units)
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

sub_cri_match_1 <- function(sub_criteria, cri, cr, ref_rd, pr_sn){
  curr_sub_cri <- sub_criteria
  if(length(curr_sub_cri) > 0){
    cri.2 <- cri + as.numeric(cr)/10
    sc_ord <- order(cri.2, -ref_rd, decreasing = T)
    cri.2 <- cri.2[sc_ord]
    rrr <- rle(cri.2)
    lgk <- !duplicated(cri.2, fromLast = TRUE)

    cri_match <- sapply(1:length(curr_sub_cri), function(i){
      set <- curr_sub_cri[[i]]
      set_match <- sapply(1:length(set), function(j){
        a <- set[[j]]
        x <- a[[1]]
        if(length(x) == 1){
          x <- rep(x, length(ref_rd))
        }
        x <- x[pr_sn]
        x <- x[sc_ord]
        y <- rep(x[lgk], rrr$lengths[match(cri.2[lgk], rrr$values)])
        f <- a[[2]]
        lgk <- try(f(x, y), silent = TRUE)
        if(class(lgk) == "try-error" | class(lgk) != "logical"){
          if(class(lgk) == "try-error"){
            err <- attr(lgk, "condition")$message
          }else{
            err <- "Output is not a `logical` object"
          }

          err <- paste0("Unable to evaluate `funcs-", j, "` at `sub_criteria` \"", names(curr_sub_cri[i]),"\":\n",
                        "i - Each `func` must have the following syntax and output.\n",
                        "i - Syntax ~ `func(x, y, ...)`.\n",
                        "i - Output ~ `TRUE` or `FALSE`.\n",
                        "X - Issue with `funcs - ", j, "` at \"", names(curr_sub_cri[i]),"\": ", err, ".")
          stop(err, call. = FALSE)
        }
        lgk <- as.numeric(lgk)
        x <- ifelse(is.na(lgk), 0, lgk)
        x <- ifelse(is.na(lgk), 0, lgk)
        retrieve_pos <- match(1:length(cri.2), sc_ord)
        x <- x[retrieve_pos]
        return(x)
      })

      if(length(set_match) == 1){
        set_match <- as.matrix(set_match)
      }
      ifelse(rowSums(set_match) > 0, 1, 0)
    })
    if(length(cri_match) == 1){
      cri_match <- as.matrix(cri_match)
    }
    cri_match <- rowSums(cri_match) == ncol(cri_match) | ref_rd
  }else{
    cri_match <- rep(TRUE, length(ref_rd))
  }
  return(cri_match)
}
sub_cri_match_2 <- function(sub_criteria, cri, cr = NULL, ref_rd, pr_sn, spr = FALSE){
  if(is.null(cr)){
    cr <- rep(TRUE, length(cri))
  }
  curr_sub_cri <- sub_criteria
  ds_len <- length(cri)
  if(length(curr_sub_cri) > 0){
    cri.2 <- cri + as.numeric(cr)/10
    sc_ord <- order(cri.2, -ref_rd, decreasing = T)
    cri.2 <- cri.2[sc_ord]
    rrr <- rle(cri.2)
    lgk <- !duplicated(cri.2, fromLast = TRUE)

    sub_cri_match <- sapply(1:length(curr_sub_cri), function(i){
      set <- curr_sub_cri[[i]]
      set_match <- sapply(1:length(set), function(j){
        a <- set[[j]]
        x <- a[[1]]
        if(length(x) == 1){
          x <- rep(x, ds_len)
        }
        x <- x[pr_sn]
        x <- x[sc_ord]
        y <- rep(x[lgk], rrr$lengths[match(cri.2[lgk], rrr$values)])
        f1 <- a[[2]]
        lgk <- try(f1(x, y), silent = T)
        if(class(lgk) == "try-error" | class(lgk) != "logical"){
          if(class(lgk) == "try-error"){
            err <- attr(lgk, "condition")$message
          }else{
            err <- "Output is not a `logical` object"
          }

          err <- paste0("Unable to evaluate `funcs-", j, "` at `sub_criteria` \"", names(curr_sub_cri[i]),"\":\n",
                        "i - Each `func` must have the following syntax and output.\n",
                        "i - Syntax ~ `function(x, y, ...)`.\n",
                        "i - Output ~ `TRUE` or `FALSE`.\n",
                        "X - Issue with `funcs - ", j, "` at \"", names(curr_sub_cri[i]),"\": ", err, ".")
          stop(err, call. = F)
        }
        lgk <- as.numeric(lgk)
        out1 <- ifelse(is.na(lgk), 0, lgk)
        if(length(out1) == 1) out1 <- rep(out1, length(cri))
        if(length(out1) != length(cri)){
          err <- paste0("Output length of `funcs` must be 1 or the same as `criteria`:\n",
                        "i - Unexpected length for `funcs-", j, "` at \"", names(curr_sub_cri[i]),"\":\n",
                        "i - Expecting a length of 1 of ", length(cri), ".\n",
                        "X - Length is ", length(out1), ".")
          stop(err, call. = F)
        }
        retrieve_pos <- match(1:length(cri.2), sc_ord)
        out1 <- out1[retrieve_pos]

        if(isTRUE(spr)){
          f2 <- a[[3]]
          lgk <- try(f2(x, y), silent = T)
          if(class(lgk) == "try-error" | class(lgk) != "logical"){
            if(class(lgk) == "try-error"){
              err <- attr(lgk, "condition")$message
            }else{
              err <- "Output is not a `logical` object"
            }

            err <- paste0("Unable to evaluate `equva-", j, "` at `sub_criteria` \"", names(curr_sub_cri[i]),"\":\n",
                          "i - Each `func` must have the following syntax and output.\n",
                          "i - Syntax ~ `function(x, y, ...)`.\n",
                          "i - Output ~ `TRUE` or `FALSE`.\n",
                          "X - Issue with `equva - ", j, "` at \"", names(curr_sub_cri[i]),"\": ", err, ".")
            stop(err, call. = F)
          }
          lgk <- as.numeric(lgk)
          out2 <- ifelse(is.na(lgk), 0, lgk)
          if(length(out2) == 1) out2 <- rep(out2, length(cri))
          if(length(out2) != length(cri)){
            err <- paste0("Output length of `equva` must be 1 or the same as `criteria`:\n",
                          "i - Unexpected length for `equva-", j, "` at \"", names(curr_sub_cri[i]),"\":\n",
                          "i - Expecting a length of 1 of ", length(cri), ".\n",
                          "X - Length is ", length(out2), ".")
            stop(err, call. = F)
          }
          out2 <- out2[retrieve_pos]
          out1 <- c(out1, out2)
        }
        return(out1)
      })
      if(length(set_match) == 1){
        set_match <- as.matrix(set_match)
      }
      ifelse(rowSums(set_match) > 0, 1, 0)
    })
    if(length(sub_cri_match) == 1){
      sub_cri_match <- as.matrix(sub_cri_match)
    }
    sub_cri_match <- ifelse(rowSums(sub_cri_match) == ncol(sub_cri_match) | ref_rd, 1, 0)

    if(isTRUE(spr)){
      sub_cri_match.rf <- sub_cri_match[((length(sub_cri_match)/2)+1):length(sub_cri_match)]
      sub_cri_match <- sub_cri_match[1:(length(sub_cri_match)/2)]
      return(list(sub_cri_match, sub_cri_match.rf))
    }else{
      return(list(sub_cri_match))
    }
  }else{
    sub_cri_match <- rep(1, ds_len)
    if(isTRUE(spr)){
      return(list(sub_cri_match, sub_cri_match))
    }else{
      return(list(sub_cri_match))
    }
  }
}
win_cri_checks <- function(win_criteria, wind_id_check, tr_wind_id, int_check, current_tot, pr_sn){
  if(length(win_criteria) > 0 & length(wind_id_check) > 0){
    sub_win_checks <- sapply(1:length(win_criteria), function(i){
      set <- win_criteria[[i]]
      set_match <- sapply(1:length(set), function(j){
        a <- set[[j]]
        x <- a[[1]]
        if(length(x) == 1){
          x <- rep(x, length(int_check))
        }
        x <- x[pr_sn]
        f <- a[[2]]
        a <- split(x, wind_id_checks)

        lgk <- try(lapply(a, f), silent = TRUE)
        if(class(lgk) == "try-error" | class(unlist(lgk, use.names = FALSE)) != "logical"){
          if(class(lgk) == "try-error"){
            err <- attr(lgk, "condition")$message
          }else{
            err <- "Output is not a `logical` object"
          }

          err <- paste0("Unable to evaluate `funcs-", j, "` at `win_criteria` \"", names(win_criteria[i]),"\":\n",
                        "i - Each `func` must have the following syntax and output.\n",
                        "i - Syntax ~ `func(x, ...)`.\n",
                        "i - Output ~ `TRUE` or `FALSE`.\n",
                        "X - Issue with `funcs - ", j, "` at \"", names(win_criteria[i]),"\": ", err, ".")
          stop(err, call. = F)
        }
        lgk <- as.numeric(names(lgk)[as.logical(lgk)])
        lgk <- lgk[!is.na(lgk)]

        return(wind_id_check %in% lgk)
      })
      if(length(wind_id_check) == 1){
        set_match <- as.matrix(set_match)
      }
      ifelse(rowSums(set_match) > 0, 1, 0)
    })
    if(length(wind_id_check) == 1){
      sub_win_checks <- as.matrix(sub_win_checks)
    }
    sub_win_checks <- rowSums(sub_win_checks) == ncol(sub_win_checks)
    cri_match <- rep(TRUE, current_tot)
    cri_match[tr_wind_id %in% wind_id_check[!sub_win_checks]] <- FALSE
  }else{
    cri_match <- TRUE
  }
  return(cri_match)
}

win_tot_checks <- function(wind_id_checks, int_check, tr_wind_id, wind_total){
  if(length(wind_id_checks) > 0 & any(!(wind_total@start == 1 & wind_total@.Data == Inf))){
    si_ord <- order(wind_id_checks, as.numeric(int_check@start))
    w_i <- wind_id_checks[si_ord]
    r_i <- rle(w_i)
    wind_tot <- Inf
    wind_tot[tr_wind_id %in% wind_id_checks] <- r_i$lengths[match(tr_wind_id[tr_wind_id %in% wind_id_checks], r_i$values)]
    wtm_cri <- !(wind_tot >= as.numeric(wind_total@start) & wind_tot <= as.numeric(right_point(wind_total)))
  }else{
    wtm_cri <- TRUE
  }
  return(wtm_cri)
}

links_summary <- function(pids, tms = "??", display = "stats"){
  if(display == "stats") cat("\n")
  if(display != "none"){
    r <- rle(sort(pids@.Data))
    pid_tot <- r$lengths[match(pids@.Data, r$values)]
    cri_dst <- table(pids@pid_cri)
    cri_n <- as.numeric(names(cri_dst))
    cri_dst <- c(cri_dst[cri_n > 0], cri_dst[cri_n == 0], cri_dst[cri_n == -1])
    cri_dst <- cri_dst[!is.na(cri_dst)]
    cri_n <- as.numeric(names(cri_dst))
    cri_dst <- paste0("       ", pid_cri_l(cri_n), ":   ", fmt(cri_dst), collapse = "\n")
    summ <- paste0("\nSummary.\n",
                   "Time elapsed:     ", tms, "\n",
                   "Iterations:           ", fmt(max(pids@iteration)), "\n",
                   "Records:\n",
                   "  Total:          ", fmt(length(pids)), "\n",
                   "    Stages:\n",
                   cri_dst, "\n",
                   "Groups:\n",
                   "   Total:         ", fmt(length(pids@.Data[!duplicated(pids@.Data)])), "\n",
                   "   Single-record: ", fmt(length(pids@.Data[!duplicated(pids@.Data) & pid_tot == 1])), "\n"
    )
  }else if(display == "none"){
    summ <- paste0("Data linkage completed in ", tms, "!\n")
  }
  return(summ)
}

episodes_summary <- function(epids, tms = "??", display = "stats"){
  if(display != "none"){
    r <- rle(sort(epids@.Data))
    epid_tot <- r$lengths[match(epids@.Data, r$values)]
    summ <- paste0("Summary.\n",
                   "Time elapsed:   ", tms, "\n",
                   "Iterations:     ", fmt(max(epids@iteration)), "\n",
                   "Records:\n",
                   "  Total:        ", fmt(length(epids)), "\n",
                   "  Skipped:      ", fmt(length(epids[epids@case_nm == "Skipped"])), "\n",
                   "Episodes:\n",
                   "  Total:        ", fmt(length(epids[epids@case_nm == "Case"])), "\n",
                   "  Single-event: ", fmt(length(epids[epids@case_nm == "Case" & epid_tot == 1])), "\n")
  }else if(display == "none"){
    summ <- paste0("Episode tracking completed in ", tms, "!\n")
  }
  return(summ)
}

txt_to_nl <- function(x){
  number_line(
    l = as.numeric(gsub("->.*$|==.*$|<-.*$", "", x)),
    r = as.numeric(gsub("^.*->|^.*==|^.*<-", "", x))
  )
}

pane_checks <- function(dates, windows){
  fnx <- function(x, int = dates){
    diyar:::ovr_chks(windows[[x]], int, rep("overlap", length(int)), rep(x, length(int)))
  }

  checks <- as.matrix(sapply(as.numeric(seq_len(length(windows))), fnx))
  if(length(dates) == 1){
    checks <- t(checks)
  }
  ep_checks <- Rfast::rowMaxs(checks, value = TRUE)
  ep_checks
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
               case_nm[lgk] == "")
    lgk <- which(lgk == TRUE)[indx == TRUE]
    return(lgk)
  }else{
    return(numeric())
  }
}
