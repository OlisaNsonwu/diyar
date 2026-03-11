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
    crx <- x@start/abs(x@start) != right_point(x)/abs(right_point(x))
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

update_text <- function(
    tot_records = NULL, current_tot = NULL, current_tagged = NULL,
    indent_txt = "", time = NULL, skipped = NULL, iteration = NULL){
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

  cmb_cd <- combi(datatset, ordered = TRUE)
  cmb_cd_idx <- which(!duplicated(cmb_cd))
  datatset_p <- lapply(datatset, function(x){
    x <- x[cmb_cd_idx]
  })
  names(datatset_p) <- paste0("X", seq_len(length(datatset_p)))
  datatset_p <- sapply(datatset_p, identity)
  if(isFALSE(is.matrix(datatset_p))){
    datatset_p <- t(as.matrix(datatset_p))
  }

  cmp_func <- list(l = all, g = any)
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

ovr_chks <- function(date, window, mths, ord){
  x <- overlaps(date, window, methods = mths)
  ord[x %in% c(NA, FALSE)] <-  0
  rm(list = ls()[ls() != "ord"])
  as.numeric(ord)
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

overlaps_err <- function(opts){
  opts <- as.vector(opts)
  if(inherits(opts, "character")){
    opts <- tolower(opts)
    opts_cd <- overlap_method_codes(opts)
  }else if(inherits(opts, c("numeric", "integer"))){
    opts_cd <- match(opts, overlap_methods$options$cd)
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
  lar <- lapply(seq_len(length(lens)), function(i){
    winds <- pltd[!duplicated(pltd$wind_id) &
                    pltd$wind_nm %in% wind_nm &
                    !is.na(pltd$wind_nm),]
    lgk <- pltd$sn %in% winds$wind_id
    lar <- pltd[lgk,]
    lens[[i]] <- lens[[i]][lgk]

    rec_ls <- nrow(lar)
    if(rec_ls >0){

      # lar$mid_y_lead <- pltd$mid_y[match(lar$sn, pltd$wind_id)]
      lar$mid_y_lead <- winds$mid_y

      x <- lens[[i]]
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
      y$wind_nm_l <- paste0("`",winds$wind_nm[match(lar$sn, winds$wind_id)], "`-length")
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
        y$nl_l <- number_line(start_point(y$nl_l)/as.numeric(episode_units[y$ep_uni]),
                              end_point(y$nl_l)/as.numeric(episode_units[y$ep_uni]))
      }
      y$nl_s <- start_point(y$nl_l)
      y$nl_e <- end_point(y$nl_l)
      y$episode_unit <- y$ep_uni
      y$nl_l <- NULL
      y$lab_y <- (y$mid_y_lead + y$y)/2
      return(y)
    }else{
      lar <-  pltd[0, c("end", "start", "epid", "y", "wind_total", "epid_total", "episode_unit")]
      lar$wind_nm_l <- lar$nl_nm <- character()
      lar$start_rl <- lar$end_rl <- lar$pt_start <- lar$pt_end <- lar$lab_y <- lar$mid_y_lead <- lar$nl_s <- lar$nl_e <- numeric()
      lar$pt_sn <- integer()
      return(lar)
    }
  })
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

  if(any(from_last)) {
    lengths$range <- lapply(lengths$range, function(x){
      if(length(x) < length(from_last)){
        x <- rep(x, length(date))
      }
      x@start <- as.numeric(x@start)
      x[from_last] <- reverse_number_line(
        invert_number_line(x[from_last]),
        direction = "decreasing")
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
    lgk <- lengths$coverage@start > 0 &
      lengths$coverage@.Data > -abs(lengths$coverage@start)
    left_point(lengths$coverage[lgk]) <- 0

    lgk <- lengths$coverage@start < 0 &
      lengths$coverage@.Data < abs(lengths$coverage@start)
    right_point(lengths$coverage[lgk]) <- 0

    lengths$coverage <- epid_windows(
      date = date,
      lengths = lengths$coverage,
      episode_unit = names(episode_units)[episode_unit]
    )
  }

  lengths$range <- lapply(lengths$range, function(x){
    x <- epid_windows(
      date = date,
      lengths = x,
      episode_unit = names(episode_units)[episode_unit]
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

mk_cd <- function(x){
  match(x, x[!duplicated(x)])
}

concat.integer <- function(...){
  # ... must be vectors
  combi <- list(...)
  faC <- lapply(combi, function(x){
    faC <- as.integer(log10(max(x))) + 1L
    return(faC)
  })

  faC <- unlist(faC, use.names = FALSE)
  faC <- rev(cumsum(rev(faC)))
  faC <- c(faC, 0)

  faC <- c(faC[1:(length(faC)-1)], 0)
  faC <- 10 ^ faC

  combi <- sapply(seq_len(length(faC)-1L), function(i){
    combi[[i]] * faC[i+1]
  })

  if(inherits(combi, "matrix")){
    combi <- rowSums(combi)
  }else{
    combi <- sum(combi)
  }

  combi <- combi + faC[1]

  rm(list = ls()[ls() != "combi"])
  return(combi)
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

inherit <- function(
    tag, cri, pid_cri, tie_sort, sn, pr_sn, expand,
    pid, link_id, upd_link_id = TRUE, old_link_id = NULL, sn_ref){
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
        at_nm <- listr(trimws(format(vv[1: ifelse(length(vv) > 4, 4, length(vv))])), lim = 3, sep = ",")
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

rc_dv <- function(x, func = length, depth = 1, tgt_depth = Inf){
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
  r <- list()
  r$mi <- rep(x, repeats)
  r$mm <- r$mi + rep(y, rep(length(x), repeats))
  r$ord <- rep(seq_len(length(y)), rep(length(x), repeats))
  rm(x, repeats, multiples, y)
  return(r)
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

make_refs_V2 <- function(x_val, y_val, id_length = max(x_val, y_val), useAsPos = TRUE, na = NA_real_){
  if(length(x_val) == 0 & length(y_val) == 0){
    return(cbind(NA, NA))
  }
  repo <- list(x = x_val, y = y_val)
  repo <- cbind(x_val, y_val)

  ref_ord <- NULL
  if(!is.null(ref_ord)){
    repo$ref_ord <- ref_ord
  }
  if(isTRUE(useAsPos)){
    repo <- repo[order(repo[,1], repo[,2]),]
  }

  if(is.null(ncol(repo))){
    repo <- matrix(repo, ncol = 2)
  }
  x_val <- repo[,1][!duplicated(repo[,1])]
  if(isFALSE(useAsPos)){
    id_length <- length(x_val)
  }else{
    x_val <- seq_len(id_length)
  }
  if(is.null(ref_ord)){
    rrr <- rle(repo[,1])
    ref_ord <- sequence(rrr$lengths)
    ref_sn <- rep(seq_len(length(rrr$lengths)), rrr$lengths)
    repo <- cbind(repo, ref_sn, ref_ord)
  }
  mx_ord <- max(ref_ord)
  link_id <- rep(na, id_length * mx_ord)

  if(isTRUE(useAsPos)){
    x.mi <- ((ref_ord - 1) * id_length) + repo[,1]
    link_id[x.mi] <- y_val[repo[,2]]
  }else{
    x.mi <- ((ref_ord - 1) * id_length) + repo[,3]
    link_id[x.mi] <- repo[,2]
  }

  link_id <- matrix(c(x_val, link_id), nrow = id_length)
  colnames(link_id) <- c("x", paste0("y", 1:(ncol(link_id)-1)))
  return(link_id)
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

mix_pos <- function(cu_pos.mi, tr_pos.mi, ld_pos.mi, cu_pos.ord, opt_levels){
  rp <- list(
    cu_pos.mi = cu_pos.mi, tr_pos.mi = tr_pos.mi,
    ld_pos.mi = ld_pos.mi, cu_pos.ord = cu_pos.ord)
  # default "r"
  rp$nwPos <- rp$cu_pos.mi
  for (lv in c("e", "w")){
    if(!any(lv %in% opt_levels)){
      next
    }
    nm <- lv
    nm[nm == "e"] <- "ld_pos.mi"
    nm[nm == "w"] <- "tr_pos.mi"
    # nm[nm == "r"] <- "cu_pos.mi"
    tmp.opts.lv <- seq_len(length(opt_levels))[opt_levels %in% lv]
    indx <- which(rp$cu_pos.ord %in% tmp.opts.lv)
    rp$nwPos[indx] <-
      rp[[nm]][indx]
  }

  return(rp$nwPos)
}

S4_to_list <- function(x, decode = TRUE, .Data_type = "epid"){
  decode_opt <- decode
  rm(decode)
  if(decode_opt){
    tmp.func <- decode
  }else{
    tmp.func <- identity
  }

  if(inherits(x, "pane")){
    window_list <- lapply(x@window_list, list)
    x@window_list <- list()
  }
  slot.nm <- methods::slotNames(x)
  slot.val <- lapply(slot.nm, function(nm){
    methods::slot(x, nm)
  })
  names(slot.val) <- slot.nm
  slot.val <- slot.val[slot.nm != "options"]
  names(slot.val) -> slot.nm

  lgk <- lapply(slot.val, function(x){
    inherits(x, "list")
  })
  lgk <- as.logical(lgk)
  if(any(lgk)){
    slot.val <- c(slot.val[!lgk],
                  unlist(slot.val[lgk], recursive = FALSE))
  }

  lgk <- lapply(slot.val, length) == 0
  slot.val <- slot.val[!lgk]

  names(slot.val) <- gsub("^wind_id\\.|^wind_nm\\.|^link_id\\." , "", names(slot.val))
  names(slot.val)[grepl(".Data", names(slot.val))] <- .Data_type

  names(slot.val) -> slot.nm
  slot.val <- lapply(slot.val, function(x){
    if(inherits(x, "d_label")){
      tmp.func(x)
    }else{
      x
    }
  })

  lgk <- grepl("interval$", slot.nm)
  if(any(lgk)){
    tmp.list <- list()
    tmp.list[[paste0(.Data_type,"_start")]] <- slot.val[lgk][[1]]@start
    tmp.list[[paste0(.Data_type,"_end")]] <- right_point(slot.val[lgk][[1]])
    slot.val <- c(slot.val[!lgk], tmp.list)
  }

  if(inherits(x, "pane")){
    slot.val$window_list <- window_list[match(slot.val$pane, names(window_list))]
    names(slot.val$window_list) <- NULL
  }
  return(slot.val)
}

missing_wf.null <- function(x){
  l <- FALSE
  if(missing(x)){
    return(TRUE)
  }else{
    is.null(x)
  }
}

round_to <- function(x, to, f = round){
  r <- (as.numeric(x) %% to)
  (x - r) + (f(r/to) * to)
}

concat.integer_v1 <- function(a, z){
  faC <- as.integer(log10(max(z, na.rm = TRUE))) + 1L
  faC <- 10 ^ faC
  a <- (a * faC) + z
  a
}

id_total <- function(x){
  rr <- rle(sort(x))
  x <- rr$lengths[match(x, rr$values)]
  x
}

extract.opts <- function(..., ref.func){
  opt_lst <- list(...)
  def_list <- formals(ref.func)
  named_pos <- which(names(opt_lst) %in% names(def_list))
  unamed_pos <- seq_len(length(opt_lst))[!seq_len(length(opt_lst)) %in% named_pos]
  unnamed_args <- names(def_list)[!names(def_list) %in% names(opt_lst)]
  unnamed_args <- unnamed_args[unamed_pos]
  names(opt_lst)[unamed_pos] <- unnamed_args

  opt_lst <- c(
    opt_lst,
    def_list[names(def_list)[!names(def_list) %in% names(opt_lst)]]
  )

  opt_lst <- lapply(opt_lst, function(x){
    if(inherits(x, "call")){
      return(eval(x))
    }else if(inherits(x, "name")){
      return(opt_lst[[x]])
    }else{
      return(x)
    }
  })

  return(opt_lst)
}

mk_id <- function(id1, id2){
  dfr <- as.data.frame(reverse_number_line(number_line(id1, id2), direction = 'decreasing'))
  names(dfr) <- c('v1','v2')

  dfr$l3 <- dfr$l1 <- dfr$v2[match(dfr$v1, dfr$v1)]
  dfr$l2 <- dfr$l1[match(dfr$v2, dfr$v1)]

  lgk <- !is.na(dfr$l2)
  dfr$l1[lgk] <- dfr$l2[lgk]

  sn <- c(dfr$v1, dfr$v2)
  id <- c(dfr$l1, dfr$l1)

  indx <- which(!duplicated(combi(sn, id)))
  sn <- sn[indx]
  id <- id[indx]

  return(list(sn = sn, id = id))
}

tots <- function(x){
  n.row <- length(x)
  sn <- seq_len(n.row-1)
  sn <- c(sn[x[sn] != x[-1]], n.row)
  c(sn[1], sn[-1] - sn[seq_len(length(sn)-1)])
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

  ord <- order(strata, assign_ord)
  sn <- seq_len(length(ord))

  # if(FALSE){
  #   ordered_index <- index_record[ord]
  #   ordered_strata <- strata[ord][ordered_index]
  #   ordered_source <- data_source[ord][ordered_index]
  #   ordered_source_order <- c(1, ordered_source[seq_len(length(ordered_source) - 1)] != ordered_source[-1])
  #   ordered_source_order <- cumsum(ordered_source_order)
  #   ordered_source_order <- rep(tots(ordered_strata), tots(ordered_strata))
  # }

  #
  ordered_strata <- strata[ord]
  ordered_index <- index_record[ord]
  ordered_x <- x[ord]

  others_runs <- rle(ordered_strata)

  i.assign_pos <- which(ordered_index)
  strata_index <- cumsum(c(1L, head(others_runs$lengths, -1)))

  rep_strata_index <- data.table::rleid(ordered_strata[ordered_index])
  if(!include_repeat & look_back){
    # run_order <- seq_len(length(i.assign_pos))
    i.assign_ord <- (i.assign_pos + 1) - strata_index[rep_strata_index]
  }

  others_runs$lengths <- others_runs$lengths[rep_strata_index]
  others_runs$values <- others_runs$values[rep_strata_index]
  strata_index <- strata_index[rep_strata_index]

  if(!include_repeat){
    others_runs$lengths <- others_runs$lengths - 1L
  }
  if(!look_back){
    others_runs$lengths <- others_runs$lengths - (i.assign_pos - strata_index)
  }

  if(!include_repeat & !look_back){
    lgk <- others_runs$lengths > 0
    if(all(!lgk)){
      return(
        list( x_pos = integer(), y_pos = integer(), index_ord = integer(), x_val = integer(),
              y_val = integer(), strata = integer()))
    }else if(any(!lgk)){
      # index_runs$lengths <- index_runs$lengths[lgk]
      # index_runs$values <- index_runs$values[lgk]

      others_runs$lengths <- others_runs$lengths[lgk]
      others_runs$values <- others_runs$values[lgk]

      i.assign_pos <- i.assign_pos[lgk]
      strata_index <- strata_index[lgk]
    }
  }

  if(include_repeat & look_back){
    xpos_start <- strata_index
  }else if(include_repeat & !look_back){
    xpos_start <- i.assign_pos
  }else if(!include_repeat & look_back){
    xpos_start <- strata_index
  }else if(!include_repeat & !look_back){
    xpos_start <- i.assign_pos + 1
  }
  ypos_start <- i.assign_pos

  if(!include_repeat & look_back){
    x_pos <- sequence(others_runs$lengths + 1L) + rep(xpos_start - 1L, others_runs$lengths + 1)
    x_pos[
      cumsum(c(1L, head(others_runs$lengths + 1L, -1))) + i.assign_ord - 1L
    ] <- 0
  }else{
    x_pos <- sequence(others_runs$lengths) + rep(xpos_start - 1L, others_runs$lengths)
  }
  y_pos <- rep(ypos_start, others_runs$lengths)

  x_pos <- ord[x_pos]
  y_pos <- ord[y_pos]

  x_val <- x[x_pos]
  y_val <- x[y_pos]

  index_ord <- rep(sequence(tots(strata_index)), others_runs$lengths)

  out <- list(
    x_pos = x_pos, y_pos = y_pos,
    index_ord = index_ord,
    x_val = x_val, y_val = y_val,
    strata = strata[y_pos])
  return(out)
}

make_pairs_batched_wf_source <- function(..., data_source = NULL){
  opt_lst <- list(...)
  def_list <- formals(make_pairs_batched)
  named_pos <- which(names(opt_lst) %in% names(def_list))
  unamed_pos <- seq_len(length(opt_lst))[!seq_len(length(opt_lst)) %in% named_pos]
  unnamed_args <- names(def_list)[!names(def_list) %in% names(opt_lst)]
  unnamed_args <- unnamed_args[unamed_pos]
  names(opt_lst)[unamed_pos] <- unnamed_args

  opt_lst <- c(
    opt_lst,
    def_list[names(def_list)[!names(def_list) %in% names(opt_lst)]]
  )
  rm(def_list, unnamed_args, named_pos, unamed_pos)

  if(length(opt_lst$x) != length(data_source)){
    stop("`data_source` must be `NULL` or have the same lenght as `x`", call. = FALSE)
  }

  x <- eval(opt_lst$x)
  index_record <- eval(opt_lst$index_record)
  strata <- eval(opt_lst$strata)
  assign_ord <- eval(opt_lst$assign_ord)

  ord <- order(strata, assign_ord)
  sn <- seq_len(length(ord))

  #
  ordered_strata <- strata[ord]
  ordered_index <- index_record[ord]
  orderded_source <- data_source[ord]
  ordered_sn <- sn[ord]

  index_strata <- ordered_strata[ordered_index]
  index_source <- orderded_source[ordered_index]
  index_sn <- ordered_sn[ordered_index]

  index_frame <- bys_rank(by = index_strata)

  tgt_pairs <- list()
  for (fr in seq_len(max(index_frame))) {
    tgt_lgk <- strata %in% index_strata[index_frame == fr]
    # Strata with index records from the same source
    tmp.index_record <- sn[tgt_lgk] == index_sn[index_frame == fr]
    tmp.index_source <- bys_val(
      !tmp.index_record[tgt_lgk], val = data_source[tgt_lgk],
      by = strata[tgt_lgk])
    sub.tgt_lgk <- data_source[tgt_lgk] != tmp.index_source | tmp.index_record
    tmp.sn <- sn[tgt_lgk][sub.tgt_lgk]
    tgt_pairs[[fr]] <- data.table::as.data.table(make_pairs_batched(
      x = x[tgt_lgk][sub.tgt_lgk],
      index_record = tmp.index_record[sub.tgt_lgk],
      strata = strata[tgt_lgk][sub.tgt_lgk],
      assign_ord = assign_ord[tgt_lgk][sub.tgt_lgk],
      include_repeat = eval(opt_lst$include_repeat),
      look_back = eval(opt_lst$look_back)))
    tgt_pairs[[fr]][,index_ord := fr]
    tgt_pairs[[fr]][,index_ord := fr]
    tgt_pairs[[fr]][,x_pos := tmp.sn[x_pos]]
    tgt_pairs[[fr]][,y_pos := tmp.sn[y_pos]]
  }
  tgt_pairs <- data.table::rbindlist(tgt_pairs)
  tgt_pairs <- as.list(tgt_pairs)

  return(tgt_pairs)
}

attr_eval <- function(x, func = length, simplify = TRUE){
  if(inherits(x, 'sub_criteria')) x <- unpack_sub_criteria(x, part = 'attribute')
  x <- lapply(flatten_list(x), func)
  if(simplify){
    x <- unlist(x, use.names = FALSE)
  }
  return(x)
}

match.alt <- function(x){
  ord <- data.table:::forder(x)
  x <- data.table::rleid(x[ord])
  x <- x[data.table:::forder(data.table:::forder(ord))]
  return(x)
}

tmp.func <- function(x){
  if(inherits(x, "number_line")){
    x <- reverse_number_line(x, "decreasing")
  }else{
    x <- number_line(0, x)
  }
  return(x)
}
