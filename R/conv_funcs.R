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

fmt <- function(x, fmt = "count"){
  if(fmt == "difftime"){
    paste0(ifelse(round(x) == 0, "< 1", round(as.numeric(x), 2)), " ", attr(x, "units"))
  }else if(fmt == "MB"){
    class(x) <- "object_size"
    format(x, fmt)
  }else{
    formatC(x, format="d", big.mark=",")
  }
}

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

progress_bar <- function(n, d, max_width, msg = "",  prefix_msg = ""){
  prop_complete <- n/d
  mx_l <- max(nchar(c(n, d)))
  d <- format(d, big.mark = ",", width = mx_l, scientific = FALSE)
  n <- format(n, big.mark = ",", width = mx_l, scientific = FALSE)
  pct_l <- paste0(msg,"; ", n, " of ", d, " record(s) completed.")

  status_width <- max_width - (nchar(pct_l) + nchar(prefix_msg))
  bar_width <- floor(prop_complete*status_width)
  space_width <- status_width - bar_width

  status <-paste0(
    prefix_msg,
    paste0(rep("-", bar_width), collapse = ""),
    # ">",
    paste0(rep(" ", space_width), collapse = ""),
    pct_l,
    "\r")
  status
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

update_text <- function(tot_records = NULL,
                        current_tot = NULL,
                        current_tagged = NULL,
                        indent_txt = "",
                        time = NULL,
                        skipped = NULL,
                        iteration = NULL){
  update <- ""
  if(!is.null(iteration)){
    update <- paste0(update, indent_txt, "Iteration: ", iteration, ".\n")
  }
  if(!is.null(tot_records)){
    update <- paste0(update, indent_txt, "Total: ", tot_records, " record(s).\n")
  }
  if(!is.null(current_tot)){
    update <- paste0(update, indent_txt, "Checked: ", current_tot, " record(s).\n")
  }
  if(!is.null(skipped)){
    update <- paste0(update, indent_txt, "Skipped: ", skipped, " record(s).\n")
  }
  if(!is.null(current_tagged)){
    update <- paste0(update, indent_txt, "Linked: ", current_tagged, " record(s).\n")
  }
  if(!is.null(time)){
    update <- paste0(update, indent_txt, "Time: ", time, ".\n")
  }
  return(update)
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
  dsc <- sort(data_source[!duplicated(data_source)])
  tmp <- rep(NA_character_, length(data_source))
  datatset <- lapply(dsc, function(x){
    cri1 <- cri[x == data_source]
    cri1 <- cri1[!duplicated(cri1)]
    tmp[cri %in% cri1] <- x
    rm(cri1)
    tmp
  })

  cmb_cd <- combi(datatset)
  cmb_cd_idx <- which(!duplicated(cmb_cd))
  datatset_p <- lapply(datatset, function(x){
    x <- x[cmb_cd_idx]
  })
  names(datatset_p) <- paste0("X", seq_len(length(datatset_p)))
  datatset_p <- sapply(datatset_p, identity)
  if(isFALSE(is.matrix(datatset_p))){
    datatset_p <- t(as.matrix(datatset_p))
  }

  cmp_func <- list(l = all, g = all)
  cmp_func <- cmp_func[match(names(data_links), names(cmp_func))]

  hits <- sapply(1:length(data_links), function(i){
    x <- lapply(seq_len(nrow(datatset_p)), function(j){
      cmp_func[[i]](data_links[[i]] %in% datatset_p[j,])
    })
    unlist(x, use.names = FALSE)
  })
  if(is.vector(hits) & length(hits) > 1){
    hits <- hits > 0
  }else if (!is.vector(hits)){
    hits <- rowSums(hits) > 0
  }

  datatset_p <- lapply(seq_len(nrow(datatset_p)), function(j) {
    x <- datatset_p[j,]
    paste0(x[!is.na(x)], collapse = ",")
  })
  datatset_p <- unlist(datatset_p, use.names = FALSE)

  return(
    list(
      ds = datatset_p[cmb_cd],
      rq = hits[cmb_cd]
    )
  )
}

prep_lengths <- function(length, overlap_methods, int,
                         episode_unit, bi_direction,
                         #from_last,
                         include_index_period = F){
  length <- length
  if(!inherits(length, "list")){
    length <- list(length)
  }
  if(!inherits(overlap_methods, "list")){
    overlap_methods <- list(overlap_methods)
  }
  if(length(overlap_methods) == 1){
    overlap_methods <- rep(overlap_methods, length(length))
  }

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
  opts <- as.vector(opts)
  if(inherits(opts, "character")){
    opts <- tolower(opts)
    opts_cd <- overlap_method_codes(opts)
  }else if(inherits(opts, c("numeric", "integer"))){
    opts_cd <- match(opts, diyar::overlap_methods$options$cd)
  }

  opts <- opts[is.na(opts_cd)]
  opts <- opts[!duplicated(opts)]
  if(length(opts) > 0){
    opts <- paste0("\"", opts,"\"")
    if(length(opts) >3) errs <- paste0(paste0(opts, collapse = ", "), " ...") else errs <- listr(opts)
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

  if(inherits(FUN, "NULL")){
    func <- function(x, ...) x
  }else{
    func <- function(x, ...) FUN(x, ...)
  }
  all_pos <- sapply(all_pos, func, simplify = simplify)
  return(all_pos)
}


#' @name listr
#' @aliases listr
#' @title Grammatical lists.
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
listr <- function(x, sep = ", ", conj = " and ", lim = Inf){
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
    x <- ifelse(length(x) > 1, paste0(f, conj, p), f)
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
          v$start <- v$pt_end
          y$end[y$bi_dir] <- y$pt_end[y$bi_dir]
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

length_to_range <- function(lengths, date, from_last, episode_unit, skip_if_b4_lengths = FALSE){
  if(!inherits(lengths, "list")){
    lengths <- list(range = list(lengths))
  }else{
    lengths <- list(range = lengths)
  }

  lengths$range <- lapply(lengths$range, function(x){
    if(!inherits(x, "number_line")){
      x <- number_line(0, x)
    }
    return(x)
  })

  if(any(from_last == T)) {
    lengths$range <- lapply(lengths$range, function(x){
      if(length(x) < length(from_last)){
        x <- rep(x, length(date))
      }
      x@start <- as.numeric(x@start)
      x[from_last] <- invert_number_line(x)[from_last]
      return(x)
    })
  }

  if(any(skip_if_b4_lengths == T)) {
    lengths$coverage <- lapply(lengths$range, function(x){
      matrix(c(left_point(x), right_point(x)), ncol = 2)
    })
    lengths$coverage <- do.call("cbind", lengths$coverage)
    lengths$coverage <- number_line(
      row_wise(lengths$coverage, type = "min"),
      row_wise(lengths$coverage, type = "max")
    )
    lgk <- (lengths$coverage@start * lengths$coverage@.Data) > 0
    left_point(lengths$coverage[lgk]) <- 0
    lengths$coverage <- epid_windows(
      date = date,
      lengths = lengths$coverage,
      episode_unit = names(diyar::episode_unit)[episode_unit]
    )
  }

  lengths$range <- lapply(lengths$range, function(x){
    x <- epid_windows(
      date = date,
      lengths = x,
      episode_unit = names(diyar::episode_unit)[episode_unit]
    )
    return(x)
  })
  return(lengths)
}

opt_level <- function(opt, mth, tr_mth){
  if(opt == "g") {
    tr_mth
  }else if(opt == "b"){
    lgk <- mth != tr_mth
    mth[lgk] <- overlap_method_codes(paste0(overlap_method_names(mth[lgk]), "|", overlap_method_names(tr_mth[lgk])))
    mth
  }else{
    mth
  }
}

pane_checks <- function(dates, windows){
  fnx <- function(x, int = dates){
    ovr_chks(rep(windows[[x]], length(int)), int, rep(8, length(int)), rep(x, length(int)))
  }

  checks <- as.matrix(sapply(as.numeric(seq_len(length(windows))), fnx))
  if(length(dates) == 1){
    checks <- t(checks)
  }
  ep_checks <- row_wise(checks, type = "max", value = TRUE)
  as.integer(ep_checks)
}

check_skips <- function(lgk, cri, cr, tr_ep_l, vr, tr_date, date, case_nm){
  if(length(lgk[lgk]) > 0){
    ep_l_min_a <- rbind(
      row_wise(sapply(tr_ep_l, function(x) start_point(x[lgk])), type = "min"),
      row_wise(sapply(tr_ep_l, function(x) start_point(x[lgk])), type = "max")
    )

    ep_l_min_z <- rbind(
      row_wise(sapply(tr_ep_l, function(x) end_point(x[lgk])), type = "min"),
      row_wise(sapply(tr_ep_l, function(x) end_point(x[lgk])), type = "max")
    )
    ep_l_bounds_a <- start_point(tr_date[lgk])
    ep_l_bounds_z <- end_point(tr_date[lgk])

    ep_l_bounds_a <- ifelse(ep_l_min_a[1,] < ep_l_bounds_a, ep_l_min_a[1,], ep_l_bounds_a)
    ep_l_bounds_z <- ifelse(ep_l_min_z[2,] > ep_l_bounds_z, ep_l_min_z[2,], ep_l_bounds_z)

    epc_bnds <- suppressWarnings(
      number_line(
        l = ep_l_bounds_a,
        r = ep_l_bounds_z))

    ep_obds_checks <- suppressWarnings(overlap(date[lgk], epc_bnds))
    ep_obds_checks <- ifelse(is.na(ep_obds_checks), FALSE, ep_obds_checks)

    ref_period <- overlap(date, tr_date)
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

f_rbind <- function(..., deparse.level = 1){
  dfs <- list(...)
  dfs_nms <- unlist(lapply(dfs, names), use.names = FALSE)
  dfs_nms <- dfs_nms[!duplicated(dfs_nms)]
  dfs <- lapply(dfs, function(x){
    for (i in dfs_nms) {
      if(!i %in% names(x) & nrow(x) > 0){
        x[[i]] <- NA
      }else if (!i %in% names(x) & nrow(x) == 0){
        x[[i]] <- logical()
      }
    }
    return(x)
  })
  t.rbind <- function(..., x = deparse.level) rbind(..., deparse.level = x)
  dfs <- do.call("t.rbind", dfs)
  return(dfs)
}

#' @name attr_eval
#' @title Sub-criteria attributes.
#' @param x \code{[\link{sub_criteria}]}
#' @param func \code{[function]}
#' @param simplify If \code{TRUE} (default), coerce to a vector.
#' @description
#' Recursive evaluation of a function (\code{func}) on each attribute (vector) in a \bold{\code{\link{sub_criteria}}}.
#' @return \code{vector}; \code{list}
#' @export
#'
#' @examples
#' x <- sub_criteria(rep(1, 5), rep(5 * 10, 5))
#' attr_eval(x)
#' attr_eval(x, func = max)
#' attr_eval(x, func = max, simplify = FALSE)
#' attr_eval(sub_criteria(x, x), func = max, simplify = FALSE)
attr_eval <- function(x, func = length, simplify = TRUE){
  if(inherits(x, "sub_criteria")){
    x <- lapply(x, function(x){
      attr_eval(x[[1]], func = func, simplify = simplify)
    })
  }else if(inherits(x, "d_attribute")){
    x <- lapply(x, func)
  }else if(is.atomic(x)){
    x <- func(x)
  }

  if(simplify) unlist(x, use.names = FALSE) else x
}

sp_scri <- function(b, lgk){
  for (i in seq_len(length(b))) {
    attr <- (b[[i]][[1]])
    if(inherits(attr, "sub_criteria")){
      b[[i]][[1]] <- sp_scri(attr, lgk)
    }else if(inherits(attr, "d_attribute")){
      attr <- lapply(attr, function(x){
        x[lgk]
      })
      class(attr) <- "d_attribute"
      b[[i]][[1]] <- attr
    }else{
      b[[i]][[1]] <- attr[lgk]
    }
  }
  return(b)
}

rf_scri <- function(b, strata){
  for (i in seq_len(length(b))) {
    attr <- (b[[i]][[1]])
    if(inherits(attr, "sub_criteria")){
      b[[i]][[1]] <- sp_scri(attr, strata)
    }else if(inherits(attr, "d_attribute")){
      attr <- lapply(attr, function(x){
        x <- split(x, strata)[strata[!duplicated(strata)]]
        names(x) <- NULL
        x
      })
      class(attr) <- "d_attribute"
      b[[i]][[1]] <- attr
    }else{
      b[[i]][[1]] <- split(attr, strata)[strata[!duplicated(strata)]]
      names(b[[i]][[1]]) <- NULL
    }
  }
  return(b)
}

#' @name combi
#' @title Vector combinations
#' @description Numeric codes for unique combination of vectors.
#' @param ... \code{[atomic]}
#' @return \code{numeric}
#'
#' @examples
#' x <- c("A", "B", "A", "C", "B", "B")
#' y <- c("X", "X", "Z", "Z", "X", "Z")
#' combi(x, y)
#'
#' # The code above is equivalent to but quicker than the one below.
#' z <- paste0(y, "-", x)
#' z <- match(z, z)
#' z
#' @export

combi <- function(...){
  # ... must be vectors
  combi <- list(...)
  is_list <- unlist(lapply(combi, function(x){
    inherits(x, "list")
  }), use.names = FALSE)
  combi <- c(unlist(combi[is_list], recursive = FALSE),
             combi[!is_list])
  # Validations
  err_txt <- unlist(lapply(seq_len(length(combi)), function(i){
    x <- err_atomic_vectors(combi[[i]], paste0("vector ", i))
    x[x == FALSE] <- NA_character_
    x
  }), use.names = FALSE)
  err_txt <- err_txt[!is.na(err_txt)]
  if(length(err_txt) > 0) stop(err_txt, call. = FALSE)

  vec_lens <- unlist(lapply(combi, length), use.names = FALSE)
  dd_err <- vec_lens[!duplicated(vec_lens)]
  if(!(length(dd_err) == 1 | (length(dd_err) == 2 & 1 %in% dd_err))){
    err_txt <- paste0("Length of each vector in `...` must be the same or equal to 1:\n",
                      paste0("X - Length of vector ",
                             seq_len(length(vec_lens)),
                             " is ", vec_lens, ".",
                             collapse = "\n"))
    stop(err_txt, call. = FALSE)
  }

  combi[vec_lens == 1] <- NULL
  if(length(combi) == 0){
    return(rep(1, max(dd_err)))
  }
  combi_cd <- match(combi[[1]], (combi[[1]])[!duplicated(combi[[1]])])
  for (j in seq_len(length(combi))[-1]){
    k <- match(combi[[j]], (combi[[j]])[!duplicated(combi[[j]])])
    combi_cd <- combi_cd + (1/k)
    combi_cd <- match(combi_cd, combi_cd[!duplicated(combi_cd)])
  }
  rm(list = ls()[ls() != "combi_cd"])
  return(combi_cd)
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

di_report <- function(duration = 0L, cumm_time = 0L, iteration = NA, current_tot = 0L,
                      current_tagged = 0L, current_skipped = 0L,
                      criteria = 0L, memory_used = 0L){
  x <- list(iteration = iteration,
            duration = duration,
            cumm_time = cumm_time,
            records_checked = as.integer(current_tot),
            records_tracked = as.integer(current_tagged),
            records_skipped = as.integer(current_skipped),
            criteria = as.integer(criteria),
            memory_used = as.numeric(memory_used)/(1000 * 1024))
  return(x)
}

permute_num <- function(x, r = 2){
  (((x ^ r) - x)/r) + x
}

inherit <- function(tag, cri, pid_cri, tie_sort, sn, pr_sn, expand, pid, link_id, upd_link_id = TRUE, old_link_id = NULL, sn_ref){
  hri <- seq_len(length(tag))
  s_tag <- tag
  s_tag[s_tag != 0 & !expand] <- 1
  s_tag[!expand] <- !s_tag[!expand]
  sort_ord <- order(cri, -s_tag, pid_cri, tie_sort, sn, decreasing = TRUE)
  rm(s_tag)
  cri <- cri[sort_ord]
  sn <- sn[sort_ord]
  pr_sn <- pr_sn[sort_ord]
  pid <- pid[sort_ord]
  pid_cri <- pid_cri[sort_ord]
  tag <- tag[sort_ord]
  link_id <- link_id[sort_ord]
  if(!is.null(old_link_id)){
    old_link_id <- old_link_id[sort_ord]
  }
  lgk <- which(!duplicated(cri, fromLast = TRUE))
  rep_lgk <- match(cri, cri[lgk])
  tr_pid <- (pid[lgk])[rep_lgk]
  tr_sn <- (sn[lgk])[rep_lgk]
  tr_tag <- (tag[lgk])[rep_lgk]
  rm(rep_lgk)
  lgk <- which(cri %in% cri[!duplicated(cri)] & tr_tag == 1 & cri > 0 & tag != 1 & expand)
  pid[lgk] <- tr_pid[lgk]
  if(upd_link_id){
    link_id[lgk] <- tr_sn[lgk]
  }else{
    # ll <- which(!duplicated(cri, fromLast = TRUE))
    # link_id[link_id %in% link_id[ll]] <- tr_sn[link_id %in% link_id[ll]]
    link_id[pr_sn %in% tr_sn & tr_tag == 1] <- old_link_id[pr_sn %in% tr_sn & tr_tag == 1]
  }
  lgk <- which(cri %in% cri[!duplicated(cri)] & tr_tag == 0 & cri > 0 & tag != 1)
  pid[lgk] <- tr_sn[lgk]
  if(upd_link_id){
    link_id[lgk] <- tr_sn[lgk]
  }else{

  }

  return(list(
    pid = pid,
    link_id = link_id,
    tag = tag,
    pid_cri = pid_cri,
    pr_sn = pr_sn,
    sn = sn
  ))
}

mk_lazy_opt <- function(x){
  class(x) <- "d_lazy_opts"
  return(x)
}

prep_prob_link_args <- function(attribute,
                                probabilistic,
                                m_probability,
                                u_probability){
  # u-probabilities
  if(is.null(u_probability)){
    u_probability <- list()
    u_probability$x <- lapply(attribute, function(x){
      x_cd <- match(x, x[!duplicated(x)])
      x_cd[is.na(x)] <- NA_real_
      r <- rle(x_cd[order(x_cd)])
      n <- r$lengths[match(x_cd, r$values)]
      p <- n/length(x_cd)
      p[is.na(x_cd)] <- 0
      p
    })
  }else if(!inherits(u_probability, c("list"))){
    u_probability <- list(x = list(u_probability))
  }
  attr_nm <- names(attribute)
  rm(attribute)
  if(length(u_probability$x) == 1 & length(attr_nm) > 1){
    u_probability$x <- rep(u_probability$x, length(attr_nm))
  }

  # m-probabilities
  if(!inherits(m_probability, c("list"))){
    m_probability <- list(x = list(m_probability))
  }
  if(length(m_probability$x) == 1 & length(attr_nm) > 1){
    m_probability$x <- rep(m_probability$x, length(attr_nm))
  }

  names(m_probability$x) <- names(u_probability$x) <- attr_nm

  return(list(m_probability = m_probability,
              u_probability = u_probability))
}

prep_cmps_thresh <- function(attr_nm,
                             cmp_func,
                             attr_threshold,
                             score_threshold){
  # Threshold for agreement in each attribute
  if(is.number_line(attr_threshold)){
    attr_threshold[attr_threshold@.Data < 0] <- reverse_number_line(attr_threshold[attr_threshold@.Data < 0], "decreasing")
  }else{
    attr_threshold <- suppressWarnings(number_line(attr_threshold, Inf))
  }

  if(length(attr_threshold) == 1 & length(attr_nm) > 1){
    attr_threshold <- rep(attr_threshold, length(attr_nm))
  }

  if(is.number_line(score_threshold)){
    score_threshold[score_threshold@.Data < 0] <- reverse_number_line(score_threshold[score_threshold@.Data < 0], "decreasing")
  }else{
    score_threshold <- suppressWarnings(number_line(score_threshold, Inf))
  }

  # String comparator for each attribute
  if(!inherits(cmp_func, c("list"))){
    cmp_func <- list(cmp_func)
  }
  if(length(cmp_func) == 1 & length(attr_nm) > 1){
    cmp_func <- rep(cmp_func, length(attr_nm))
  }

  names(attr_threshold) <-
    names(cmp_func) <- attr_nm

  return(list(cmp_func = cmp_func,
              attr_threshold = attr_threshold,
              score_threshold = score_threshold))
}

make_pairs_batched <- function(
    x,
    index_record = rep(TRUE, length(x)),
    strata = index_record,
    assign_ord = order(x),
    ignore_same_source = FALSE,
    data_source = NULL,
    look_back = FALSE,
    include_repeat = FALSE){

  tmp <- list(sn = order(strata, assign_ord))
  tmp$pr_sn <- seq_len(length(tmp[[1]]))
  tmp$strata <- strata[tmp$sn]
  lgk <- which(!duplicated(tmp$strata, fromLast = FALSE))
  rrr <- rle(tmp$strata)
  class(rrr) <- NULL

  tmp$strata_tot <- rep(rrr$lengths, rrr$lengths)
  tmp$rec_ord <- sequence(rrr$lengths)
  if(isFALSE(look_back)){
    tmp$strata_a_ord <- tmp$rec_ord
    tmp$strata_a_indx <- tmp$pr_sn
  }else{
    tmp$strata_a_ord <- rep(1, length(tmp[[1]]))
    tmp$strata_a_indx <- rep(lgk, rrr$lengths)
  }
  if(isFALSE(include_repeat)){
    tmp$strata_tot <- tmp$strata_tot - 1
  }

  tmp$reference_record <- index_record[tmp$sn]
  tmp_indexes <- lapply(tmp, function(x){
    x[tmp$reference_record == 1]
  })
  tmp_indexes$strata_tot_new <- tmp_indexes$strata_tot - tmp_indexes$strata_a_ord + 1

  rrr <- rle(tmp_indexes$strata)
  tmp_indexes$index_ord <- sequence(rrr$lengths)

  tmp2 <- list()
  tmp2$x_ord <- sequence(tmp_indexes$strata_tot_new)
  if(isFALSE(include_repeat)){
    if(isTRUE(look_back)){
      tmp2$y_ord <- rep(tmp_indexes$rec_ord, tmp_indexes$strata_tot_new)
      tmp2$y_tot <- rep(tmp_indexes$strata_tot_new, tmp_indexes$strata_tot_new)
      tmp2$lgk <- tmp2$x_ord == tmp2$y_ord
      tmp2$x_ord[tmp2$lgk] <- tmp2$y_tot[tmp2$lgk] + 1
    }else{
      tmp_indexes <- lapply(tmp_indexes, function(x){
        x[!tmp_indexes$strata_tot_new == 0]
      })
      tmp_indexes$strata_a_indx <- tmp_indexes$strata_a_indx + 1
    }
  }
  tmp2$x_pos <- tmp2$x_ord + rep(tmp_indexes$strata_a_indx, tmp_indexes$strata_tot_new) - 1
  tmp2$y_pos <- rep(tmp_indexes$pr_sn, tmp_indexes$strata_tot_new)
  tmp2$index_ord <- rep(tmp_indexes$index_ord, tmp_indexes$strata_tot_new)

  tmp2 <- tmp2[c("x_pos", "y_pos", "index_ord")]
  if(!is.null(data_source) & isTRUE(ignore_same_source)){
    lgk <- which(data_source[tmp2$x_pos] != data_source[tmp2$y_pos])
    tmp2 <- lapply(tmp2, function(x) x[lgk])
  }
  tmp2$x_pos <- tmp$sn[tmp2$x_pos]
  tmp2$y_pos <- tmp$sn[tmp2$y_pos]

  tmp2$x_val <- x[tmp2$x_pos]
  tmp2$y_val <- x[tmp2$y_pos]

  rm(list = ls()[ls() != "tmp2"])
  return(tmp2)
}

fmt_sub_criteria <- function(x, depth_n = 0, show_levels = FALSE){
  left_indent <- paste0(rep(" ", 2 * depth_n), collapse = "")
  operator_txt <- toupper(attr(x, "operator"))
  operator_txt <- paste0(" ", operator_txt," ")

  attr_n <- length(x)

  # if(FALSE){
  #   if(isTRUE(show_levels)){
  #     depth_attr_n_nm <- paste0("diyar.recurs.depth.", depth_n)
  #     if(!exists(depth_attr_n_nm, 0, envir = .GlobalEnv)){
  #       assign(depth_attr_n_nm, 0, envir = .GlobalEnv)
  #     }else{
  #       assign(
  #         depth_attr_n_nm,
  #         get(depth_attr_n_nm, envir = .GlobalEnv) + attr_n, envir = .GlobalEnv)
  #     }
  #   }
  # }

  logic_txt <- lapply(x, function(vv){
    mf_nm <- names(vv[2])
    if(length(mf_nm) == 0){
      mf_nm <- "match_func"
    }else if(mf_nm == ""){
      mf_nm <- "match_func"
    }
    at_nm <- names(vv[1])
    if(length(at_nm) == 0){
      at_nm <- ""
    }
    vv <- vv[[1]]
    if(inherits(vv, c("sub_criteria"))){
      paste0("\n", fmt_sub_criteria(vv, depth_n + 1, show_levels))
    }else{
      if(at_nm != ""){

      }else if(is.atomic(vv)){
        at_nm <- listr(trimws(format(vv[1:4])), lim = 3, sep = ",")
      }else{
        if(!is.null(names(vv))){
          at_nm <- paste0(class(vv), "; ", listr(paste0("`", names(vv), "`"), lim = 2))
        }else{
          at_nm <- class(vv)
        }
      }
      paste0(mf_nm, "(", at_nm, ")")
    }
  })

  if(isTRUE(show_levels)){
    logic_txt <- lapply(1:length(logic_txt), function(i){
      if(FALSE){
        i2 <- get(depth_attr_n_nm, envir = .GlobalEnv) + i
      }else{
        i2 <- i
      }
      if(grepl("^\\n", logic_txt[[i]])){
        paste0("\n", left_indent, "Lv.", depth_n, ".", i2, "-", logic_txt[[i]])
        gsub(" \\{\\n ", paste0(" Lv.", depth_n, ".", i2, "-", "{\n "), logic_txt[[i]])
      }else{
        paste0("Lv.", depth_n, ".", i2, "-", logic_txt[[i]])
      }
    })
  }
  logic_txt <- unlist(logic_txt, use.names = FALSE)
  if(ceiling(length(logic_txt)/2) == 1){
    logic_txt_row <- rep(1, length(logic_txt))
  }else{
    logic_txt_row <- as.numeric(cut(seq_len(length(logic_txt)), ceiling(length(logic_txt)/2)))
  }

  logic_txt <- split(logic_txt, logic_txt_row)
  logic_txt <- lapply(seq_len(length(logic_txt)), function(i){
    if(i == length(logic_txt)){
      paste0(logic_txt[[i]], collapse = operator_txt)
    }else{
      paste0(logic_txt[[i]], operator_txt, collapse = "")
    }
  })
  logic_txt <- unlist(logic_txt, use.names = FALSE)
  logic_txt <- paste0(logic_txt, collapse = paste0("\n", left_indent))
  depth_l <- ifelse(F, paste0("Lv", depth_n), "")

  logic_txt <- paste0(left_indent, "{", depth_l, "\n",
                      left_indent, logic_txt, "\n",
                      left_indent, "}")
  logic_txt <- gsub("\\n *\\n", "\n", logic_txt)
  logic_txt
}

rc_dv <- function(x, func = func, depth = 1, tgt_depth = Inf){
  if(inherits(x, c("d_attribute", "list")) & depth <= tgt_depth){
    lapply(x, function(x) rc_dv(x = x, func = func,
                                depth = depth + 1,
                                tgt_depth = tgt_depth))
  }else if (is.atomic(x)){
    func(x)
  }else{

  }
}

nl_bind <- function(...){
  return(c(...))
}

index_multiples <- function(x, multiples, repeats){
  y <- seq(0, multiples * (repeats - 1), by = multiples)
  y <- rep(x, repeats) + rep(y, rep(length(x), repeats))
  rm(x, repeats, multiples)
  return(y)
}

length_hits <- function(ep_checks, strata, current_tot, range.n){
  hits <- ceiling(seq_len(length(ep_checks)) / current_tot)
  hits <- hits[ep_checks]
  tmp_strata <- rep(strata, range.n)[ep_checks]
  if(range.n <= 308){
    hits <- hits / 10 ^ (floor(hits/10) + 1)
    hits <- tmp_strata + hits
  }else{
    hits <- combi(hits, tmp_strata)
  }
  hits <- tmp_strata[!duplicated(hits)]
  hits <- rle(sort(hits))
  hits <- hits$lengths[match(strata, hits$values)]
  return(hits)
}

is_even <- function(x){
  as.logical(x %% 2)
}

row_wise <- function(x, value = TRUE, type = "max"){
  type <- ifelse(type == "max", 1, -1)
  if(is.null(ncol(x))){
    x <- t(x)
  }
  y <- ((max.col(x * type) - 1) * nrow(x)) + seq_len(nrow(x))
  if(isTRUE(value)){
    y <- x[y]
  }
  return(y)
}

group_stats <- function(strata, start_date, end_date){
  dts_a <- lapply(split(as.numeric(start_date), strata), min)
  dts_z <- lapply(split(as.numeric(end_date), strata), max)

  interval <- number_line(as.numeric(dts_a), as.numeric(dts_z))
  interval <- interval[match(strata, as.numeric(names(dts_a)))]
  return(interval)
}

make_refs <- function(x_pos, y_pos, id_length = max(x_pos, y_pos), ref_ord = NULL){
  repo <- list(x = x_pos, y = y_pos)
  if(!is.null(ref_ord)){
    repo$ref_ord <- ref_ord
  }
  ord <- order(repo$x, repo$y)
  repo <- lapply(repo, function(x) x[ord])
  if(is.null(ref_ord)){
    rrr <- rle(repo$x)
    repo$ref_ord <- sequence(rrr$lengths)
  }
  mx_ord <- max(repo$ref_ord)
  link_id <- rep(NA_real_, id_length * mx_ord)

  repo$x.mi <- ((repo$ref_ord - 1) * id_length) + repo$x
  link_id[repo$x.mi] <- repo$y
  link_id <- matrix(link_id, nrow = id_length)
  link_id
}

dv <- function(lst){
  min_ord <- order(lst$x.f, lst$y3)
  min_ord <- lapply(lst[c("x.f", "y3")], function(x) x[min_ord])
  lst$y3 <- min_ord$y3[match(lst$x.f, min_ord$x.f)]
  lst$y2a <- lst$y3[match(lst$y.f_bk, lst$x.f)]
  lst$lgk <- !is.na(lst$y2a) & lst$y2a < lst$y3
  lst$y3[lst$lgk] <- lst$y2a[lst$lgk]

  min_ord <- order(lst$y.f_bk, lst$y3)
  min_ord <- lapply(lst[c("y.f_bk", "y3")], function(x) x[min_ord])
  lst$y3 <- min_ord$y3[match(lst$y.f_bk, min_ord$y.f_bk)]
  lst$y2b <- lst$y3[match(lst$x.f, lst$y.f_bk)]
  lst$lgk <- !is.na(lst$y2b) & lst$y2b < lst$y3
  lst$y3[lst$lgk] <- lst$y2b[lst$lgk]

  rm(min_ord)
  if(all(lst$y.f == lst$y3)){
    return(lst)
  }else{
    dv(list(x.f_bk = lst$x.f_bk,
            y.f_bk = lst$y.f_bk,
            x.f = lst$x.f,
            y.f = lst$y3,
            y3 = lst$y3))
  }
}

unpack <- function(xx, depth = 1){
  classes <- unlist(lapply(xx, class))
  lgk <- classes == "list"
  xx.1 <- xx[!lgk]
  xx.2 <- unlist(xx[lgk], recursive = FALSE)
  if(length(xx.1) > 0){
    names(xx.1) <- paste0("var_", depth, ".", seq_len(length(xx.1)))
  }
  if(length(xx.2) > 0){
    names(xx.2) <- paste0("var_", depth + 1, ".", seq_len(length(xx.2)))
  }
  xx <- c(xx.1, xx.2)
  classes <- unlist(lapply(xx, class))
  lgk <- classes == "list"
  if(any(lgk)){
    # rm(list = ls()[!ls() %in% c("xx", "depth", "lgk")])
    unpack(xx, depth = depth + 1)
  }else{
    # rm(list = ls()[!ls() %in% c("xx", "depth", "lgk")])
    xx
  }
}

make_sets_v2 <- function (x, r, strata = NULL, permutations_allowed = TRUE, repeats_allowed = TRUE){
  if (!is.null(strata)) {
    strata <- match(strata, strata)
    pos <- seq_len(length(x))
    s_ord <- order(strata)
    strata <- strata[s_ord]
    pos <- pos[s_ord]
    rl <- rle(strata)
    n <- max(rl$lengths)
  }
  else {
    n <- length(x)
  }
  repo <- sets(n = n, r = r, permutations_allowed = permutations_allowed,
               repeats_allowed = repeats_allowed)
  if (!is.null(strata)) {
    sn <- seq_len(length(x))
    rl$start_sn <- sn[!duplicated(strata)]
    unq_lens <- rl$lengths[!duplicated(rl$lengths)]
    repo <- lapply(unq_lens, function(x) {
      exc_indx <- which(repo > x)
      exc_indx <- exc_indx%%nrow(repo)
      exc_indx[exc_indx == 0] <- nrow(repo)
      x.repo <- repo[!seq_len(nrow(repo)) %in% exc_indx,
      ]
      x.repo
    })
    repo <- repo[match(rl$lengths, unq_lens)]
    reps <- (unq_lens^2)
    reps <- reps[match(rl$lengths, unq_lens)]
    repo <- do.call("rbind", repo)
    start_sn <- rep(rep(rl$start_sn, reps), r)
    repo <- repo + (start_sn - 1)
    repo <- pos[repo]
    repo <- matrix(repo, ncol = r)
  }
  repo <- split(repo, rep(seq_len(r), rep(nrow(repo), r)))
  repo <- c(repo, lapply(repo, function(i) x[i]))
  names(repo) <- paste0("x", rep(1:r, 2), rep(c("_pos", "_val"),
                                              rep(r, 2)))
  rm(list = ls()[ls() != "repo"])
  return(repo)
}
