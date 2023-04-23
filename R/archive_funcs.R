episode_group_archive <- function(df, sn = NULL, strata = NULL, date,
                                 case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                                 recurrence_length = NULL, rolls_max =Inf, skip_if_b4_lengths = TRUE,
                                 data_source = NULL, data_links = "ANY",
                                 custom_sort = NULL, skip_order =NULL, from_last=FALSE,
                                 overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"),
                                 overlap_methods = NULL, bi_direction = FALSE,
                                 group_stats= FALSE, display=TRUE, deduplicate=FALSE, to_s4 = TRUE,
                                 recurrence_from_last = TRUE, case_for_recurrence =FALSE, include_index_period = TRUE){
  . <- NULL
  if(missing(df)) stop("argument 'df' is missing, with no default")
  if(missing(date)) stop("argument 'date' is missing, with no default")
  if(missing(case_length)) stop("argument 'case_length' is missing, with no default")

  # Check that only logicals are passed to these arguments
  logs_check <- logicals_check(c("from_last", "bi_direction", "group_stats",
                                 "display", "deduplicate","to_s4", "recurrence_from_last",
                                 "case_for_recurrence", "skip_if_b4_lengths", "include_index_period"))
  if(logs_check!=T) stop(logs_check)

  # Suggesting the use `epid` objects - Retired since the switc
  # if(to_s4 == FALSE){
  #   # check if episode_group() was called by fixed_episodes() or rolling_episodes()
  #   wrap_func <- c("rolling_episodes","fixed_episodes")
  #   call <- deparse(sys.call(-(sys.nframe()-1)))
  #   lg <- unlist(lapply(wrap_func, function(x){
  #     grepl(paste("^",x,"\\(",sep=""), call)
  #   }))
  #
  #   # if not, display the message
  #   if(all(!lg)){
  #     if (is.null(getOption("diyar.episode_group.output"))){
  #       options("diyar.episode_group.output"= T)
  #     }
  #     if (getOption("diyar.episode_group.output")){
  #       message(paste("The default output of episode_group() will be changed to epid objects in the next release.",
  #                     "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
  #                     "",
  #                     "# Old way - merge or bind (col) results back to `df`",
  #                     "df <- cbind(df, episode_group(df, case_length= x))",
  #                     "",
  #                     "# New way - `epid` objects",
  #                     "df$epids <- episode_group(df, case_length= x, to_s4 = TRUE)",
  #                     "This message is displayed once per session.", sep = "\n"))
  #     }
  #     options("diyar.episode_group.output"= FALSE)
  #   }
  # }

  # Validations
  episodes_max <- ifelse(is.numeric(episodes_max) & !is.na(episodes_max) & !is.infinite(episodes_max), as.integer(episodes_max), episodes_max)
  rolls_max <- ifelse(is.numeric(rolls_max) & !is.na(rolls_max) & !is.infinite(rolls_max), as.integer(rolls_max), rolls_max)

  if(!is.data.frame(df)) stop(paste("A dataframe is required"))
  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(!((is.infinite(rolls_max) | is.integer(rolls_max) ) & (is.infinite(episodes_max) | is.integer(episodes_max)) & length(rolls_max)==1 & length(episodes_max)==1) ) stop(paste("'episodes_max' and 'rolls_max' must be, or can be coerced to an integer between 0 and Inf"))

  if(length(episode_type)!=1 | !is.character(episode_type)) stop(paste("'episode_type' must be a character of length 1"))
  if(length(episode_unit)!=1 | !is.character(episode_unit)) stop(paste("'episode_unit' must be a character of length 1"))

  if(!episode_unit %in% names(diyar::episode_unit)) stop(paste("'episode_unit' must be either 'seconds', 'minutes', 'hours', 'days', 'weeks', 'months' or 'years'"))
  if(!episode_type %in% c("rolling","fixed")) stop(paste("`episode_type` must be either 'rolling' or 'fixed'"))

  rd_sn <- enq_vr(substitute(sn))
  ds <- enq_vr(substitute(data_source))
  epl <- enq_vr(substitute(case_length))
  r_epl <- enq_vr(substitute(recurrence_length))
  st <- enq_vr(substitute(strata))
  ref_sort <- enq_vr(substitute(custom_sort))
  sk_od <- enq_vr(substitute(skip_order))
  dt <- enq_vr(substitute(date))
  methods <- enq_vr(substitute(overlap_methods))

  # Check that each column name supplied exists in `df`
  if(any(!unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)), !unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found in 'df'"))
  }

  if(!(any(class(df[[epl]]) %in% c("integer","double","numeric", "number_line"))))  stop(paste("'case_length' must be integer or numeric values", sep=""))
  if(!is.null(r_epl)){
    if(!(any(class(df[[r_epl]]) %in% c("integer","double","numeric", "number_line")))) stop(paste("'recurrence_length' must be integer or numeric values", sep=""))
  }
  if(!(any(class(df[[dt]]) %in% c("Date","POSIXct","POSIXt","POSIXlt","number_line","numeric","integer","Interval")))) stop("'date' must be a date, datetime, numeric or number_line object")

  if(!is.null(sk_od) & !is.null(ref_sort) ){
    if(!(any(class(df[[sk_od]]) %in% c("integer","double","numeric"))))  stop(paste("'skip_order' must be a positive integer or numeric value", sep=""))
    if(!(all(df[[sk_od]] >0)))  stop(paste("'skip_order' must be a positive integer or numeric value", sep=""))
  }

  T1 <- df[,0]

  # Record indentifier
  if(is.null(rd_sn)){
    T1$sn <- 1:nrow(df)
  }else{
    dp_check <- duplicates_check(df[[rd_sn]])
    if(dp_check!=T) stop(paste0("duplicate record indentifier ('sn') in ",dp_check))
    T1$sn <- df[[rd_sn]]
  }

  # Dataset identifier
  if(is.null(ds)){
    T1$ds <- "A"
  }else{
    T1$ds <- eval(parse(text = paste0("paste0(",paste0("df$", ds, collapse = ",'-',"),")")))
  }

  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- T1$ds[!duplicated(T1$ds)]
  ms_lst <- unique(dl_lst[!dl_lst %in% ds_lst])

  if(length(ms_lst)>0 & !all(toupper(dl_lst)=="ANY")) stop(paste("",
                                                                 paste0("Values - ", paste0("'",ms_lst,"'",collapse = ","), " not found in `datasource`."),
                                                                 "Have you used levels for `datasource`? - i.e. episode_group(... datasource = c(vr1, vr2, vr3)) ?",
                                                                 "",
                                                                 "If so, include the value for each level.",
                                                                 "",
                                                                 "Examples",
                                                                 "`data_links` <- list(l = c('ds1-ds2', 'ds3')",
                                                                 "                     g = c('ds1-ds2', 'ds3')",
                                                                 "",
                                                                 "`data_links` <- c('ds1-ds2', 'ds3')",
                                                                 "",
                                                                 "'l' - for episodes with records from 'ds1' and 'ds2' `data_sources` AND 'ds3' `data_source`",
                                                                 "'g' - for episodes with records from 'ds1' and 'ds2' `data_sources` OR  'ds3' `data_source`", sep = "\n"))

  if(!is.list(data_links)) data_links <- list(l = data_links)
  if(is.null(names(data_links)))  names(data_links) <- rep("l", length(data_links))
  if(!all(names(data_links) %in% c("g", "l"))) stop(paste("",
                                                          "`data_links` should be a `list` with every element named 'l' (links) or 'g' (groups)",
                                                          "'l' (link) is assumed for unamed elements or atomic vectors",
                                                          "",
                                                          " Examples",
                                                          "`data_links` <- list(l = c('DS1', 'DS2')",
                                                          "                     g = c('DS3', 'DS4')",
                                                          "",
                                                          "`data_links` <- c('DS1', 'DS2')",
                                                          "",
                                                          "'l' - for episodes with records from 'DS1' AND 'DS2' `data_sources`",
                                                          "'g' - for episodes with records from 'DS3' OR  'DS3' `data_sources", sep = "\n"))

  # Strata
  if(is.null(st)){
    T1$cri <- 1
  }else{
    T1$cri <- eval(parse(text = paste0("paste0(",paste0("df$", st, collapse = ",'-',"),")")))
  }

  # Date
  if(any(class(df[[dt]]) %in% c("number_line", "Interval"))){
    T1$dt_ai <- diyar::start_point(df[[dt]])
    T1$dt_zi <- diyar::end_point(df[[dt]])
  }else{
    T1$dt_ai <- df[[dt]]
    T1$dt_zi <- df[[dt]]
  }

  fn_check <- finite_check(T1$dt_zi)
  if(fn_check!=T) stop(paste0("Finite 'date' values required in ",fn_check))

  # Class of 'date'
  dt_grp <- ifelse(!any(class(T1$dt_ai) %in% c("Date","POSIXct","POSIXt","POSIXlt")) |
                     !any(class(T1$dt_zi) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)

  episode_unit <- ifelse(dt_grp==F,"seconds", episode_unit)

  if(dt_grp==T){
    T1$dt_ai <- as.numeric(as.POSIXct(format(T1$dt_ai, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S"))
    T1$dt_zi <- as.numeric(as.POSIXct(format(T1$dt_zi, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S"))
  }else{
    T1$dt_ai <- as.numeric(T1$dt_ai)
    T1$dt_zi <- as.numeric(T1$dt_zi)
  }

  # Lengths
  if(any(class(df[[epl]]) %in% c("number_line"))){
    T1$ep_a <- diyar::reverse_number_line(df[[epl]], "decreasing")
    T1$ct_rng_e <- (T1$ep_a@start >0 & right_point(T1$ep_a) > 0) | (T1$ep_a@start <0 & right_point(T1$ep_a) < 0)
    # Case level bi_direction ?
    T1$crx_e <- T1$ep_a@start/abs(T1$ep_a@start) != diyar::end_point(T1$ep_a)/abs(diyar::end_point(T1$ep_a))
    T1$crx_e[is.na(T1$crx_e)] <- F
    T1$ep_d <- T1$ep_c <- T1$ep_b <- T1$ep_a
    # Y - Split into the 'specified' and 'inverse' ranges
    left_point(T1$ep_a) <- ifelse(T1$crx_e ==T, -(T1$dt_zi-T1$dt_ai), left_point(T1$ep_a))
    right_point(T1$ep_b) <- ifelse(T1$crx_e ==T, 0 , right_point(T1$ep_b))
    # N - Get the 'inverse' of the specified range
    T1$ep_b[T1$crx_e == F] <- diyar::invert_number_line(T1$ep_b[T1$crx_e == F])

    # Max range
    T1$ep_c <- T1$ep_a; T1$ep_d <- T1$ep_b
    left_point(T1$ep_c) <- -(T1$dt_zi-T1$dt_ai)
    left_point(T1$ep_d) <- rep(0, length(T1$ep_d))
  }else{
    T1$ep_a <- diyar::as.number_line(df[[epl]])
    # Historic behaviour of case_length
    left_point(T1$ep_a) <- ifelse(T1$ep_a@start <0 & T1$ep_a@.Data ==0, 0, left_point(T1$ep_a))
    T1$ep_a[T1$ep_a@start>=0 & T1$ep_a@.Data ==0] <- diyar::number_line(-as.numeric(T1$dt_zi[T1$ep_a@start>=0 & T1$ep_a@.Data ==0] - T1$dt_ai[T1$ep_a@start>=0 & T1$ep_a@.Data ==0])/diyar::episode_unit[[episode_unit]], as.numeric(T1$ep_a@start[T1$ep_a@start>=0 & T1$ep_a@.Data ==0]))
    T1$ep_a <- diyar::reverse_number_line(T1$ep_a, "decreasing")
    # Case level bi_direction ?
    # N - Always N
    T1$crx_e <- F
    # Inverse range
    T1$ep_b <- diyar::invert_number_line(T1$ep_a)
    end_point(T1$ep_b) <- ifelse(left_point(T1$ep_b)!=0 & right_point(T1$ep_b)!=0, 0, end_point(T1$ep_b))
    # Max range
    T1$ep_c <- T1$ep_a; T1$ep_d <- T1$ep_b
    left_point(T1$ep_c) <- -(T1$dt_zi-T1$dt_ai)
    left_point(T1$ep_d) <- rep(0, length(T1$ep_d))
    T1$ct_rng_e <- F
  }

  if(is.null(r_epl) | episode_type !="rolling" ){
    T1$rc_a <- T1$ep_a
    T1$rc_b <- T1$ep_b
    T1$rc_c <- T1$ep_c
    T1$rc_d <- T1$ep_d
    T1$crx_r <- T1$crx_e
    T1$ct_rng_r <- T1$ct_rng_e
  }else{
    if(any(class(df[[r_epl]]) %in% c("number_line"))){
      T1$rc_a <- diyar::reverse_number_line(df[[r_epl]], "decreasing")
      T1$ct_rng_r <- (T1$rc_a@start >0 & right_point(T1$rc_a) > 0) | (T1$rc_a@start <0 & right_point(T1$rc_a) > 0)
      # Case level bi_direction ?
      T1$crx_r <- T1$rc_a@start/abs(T1$rc_a@start) != diyar::end_point(T1$rc_a)/abs(diyar::end_point(T1$rc_a))
      T1$crx_r[is.na(T1$crx_r)] <- F
      T1$rc_d <- T1$rc_c <- T1$rc_b <- T1$rc_a
      # Y - Split into the 'specified' and 'inverse' ranges
      left_point(T1$rc_a) <- ifelse(T1$crx_r ==T, -(T1$dt_zi-T1$dt_ai), left_point(T1$rc_a))
      right_point(T1$rc_b) <- ifelse(T1$crx_r ==T, 0 , right_point(T1$rc_b))
      # N - Get the 'inverse' of the specified range
      T1$rc_b[T1$crx_r == F] <- diyar::invert_number_line(T1$rc_b[T1$crx_r == F])

      # Max range
      T1$rc_c <- T1$rc_a; T1$rc_d <- T1$rc_b
      left_point(T1$rc_c) <- -(T1$dt_zi-T1$dt_ai)
      left_point(T1$rc_d) <- rep(0, length(T1$rc_d))
    }else{
      T1$rc_a <- diyar::as.number_line(df[[r_epl]])
      # Historic behaviour of case_length
      left_point(T1$rc_a) <- ifelse(T1$rc_a@start <0 & T1$rc_a@.Data ==0, 0, left_point(T1$rc_a))
      T1$rc_a[T1$rc_a@start>=0 & T1$rc_a@.Data ==0] <- diyar::number_line(-as.numeric(T1$dt_zi[T1$rc_a@start>=0 & T1$rc_a@.Data ==0] - T1$dt_ai[T1$rc_a@start>=0 & T1$rc_a@.Data ==0])/diyar::episode_unit[[episode_unit]], as.numeric(T1$rc_a@start[T1$rc_a@start>=0 & T1$rc_a@.Data ==0]))
      T1$rc_a <- diyar::reverse_number_line(T1$rc_a, "decreasing")
      # Case level bi_direction ?
      # N - Always N
      T1$crx_r <- F
      # Inverse range
      T1$rc_b <- diyar::invert_number_line(T1$rc_a)
      end_point(T1$rc_b) <- ifelse(left_point(T1$rc_b)!=0 & right_point(T1$rc_b)!=0, 0, end_point(T1$rc_b))
      # Max range
      T1$rc_c <- T1$rc_a; T1$rc_d <- T1$rc_b
      left_point(T1$rc_c) <- -(T1$dt_zi-T1$dt_ai)
      left_point(T1$rc_d) <- rep(0, length(T1$rc_d))
      T1$ct_rng_r <- F
    }
  }

  # Case and recurrence lengths of reference events
  efunc <- function(x){
    x@start <- x@start * diyar::episode_unit[[episode_unit]]
    x@.Data <- x@.Data * diyar::episode_unit[[episode_unit]]
    x
  }

  T1$ep_a <- lapply(list(T1$ep_a), efunc)[[1]]
  T1$ep_b <- lapply(list(T1$ep_b), efunc)[[1]]
  T1$ep_c <- lapply(list(T1$ep_c), efunc)[[1]]
  T1$ep_d <- lapply(list(T1$ep_d), efunc)[[1]]

  T1$rc_a <- lapply(list(T1$rc_a), efunc)[[1]]
  T1$rc_b <- lapply(list(T1$rc_b), efunc)[[1]]
  T1$rc_c <- lapply(list(T1$rc_c), efunc)[[1]]
  T1$rc_d <- lapply(list(T1$rc_d), efunc)[[1]]

  T1$ep_a1 <- T1$ep_a@start; T1$rc_a1 <- T1$rc_a@start;
  T1$ep_a2 <- T1$ep_a@start + T1$ep_a@.Data; T1$rc_a2 <- T1$rc_a@start + T1$rc_a@.Data

  T1$ep_b1 <- T1$ep_b@start; T1$rc_b1 <- T1$rc_b@start;
  T1$ep_b2 <- T1$ep_b@start + T1$ep_b@.Data; T1$rc_b2 <- T1$rc_b@start + T1$rc_b@.Data

  T1$ep_c1 <- T1$ep_c@start; T1$rc_c1 <- T1$rc_c@start;
  T1$ep_c2 <- T1$ep_c@start + T1$ep_c@.Data; T1$rc_c2 <- T1$rc_c@start + T1$rc_c@.Data

  T1$ep_d1 <- T1$ep_d@start; T1$rc_d1 <- T1$rc_d@start;
  T1$ep_d2 <- T1$ep_d@start + T1$ep_d@.Data; T1$rc_d2 <- T1$rc_d@start + T1$rc_d@.Data

  fn_check <- finite_check(T1$ep_a@start)
  if(fn_check!=T) stop(paste0("Finite 'case_length' values required in ",fn_check))

  fn_check <- finite_check(T1$ep_a@.Data)
  if(fn_check!=T) stop(paste0("Finite 'case_length' values required in ",fn_check))

  fn_check <- finite_check(T1$rc_a@start)
  if(fn_check!=T) stop(paste0("Finite 'recurrence_length' values required in ",fn_check))

  fn_check <- finite_check(T1$rc_a@.Data)
  if(fn_check!=T) stop(paste0("Finite 'recurrence_length' values required in ",fn_check))

  # Overlap methods
  if(missing(overlap_methods) & !missing(overlap_method)) {
    T1$methods <- paste0(overlap_method, collapse = "|")
    warning("'overlap_method' is deprecated. Please use 'overlap_methods' instead.")
  }else{
    if(is.null(methods)){
      T1$methods <- "overlap"
    }else {
      T1$methods <- df[[methods]]
    }
  }

  o <- unique(unlist(strsplit(T1$methods[!duplicated(T1$methods)], split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween", "overlap", "none")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end', 'inbetween' or 'none' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  T1$dist_from_epid <- T1$dist_from_wind <- T1$wind_id <- T1$tag <- T1$roll <- T1$episodes <- 0
  T1$wind_nm <- T1$case_nm <- ""
  T1$epid <- sn_ref <- min(T1$sn)-1
  T1$pr_sn = 1:nrow(df)

  # Chronological order
  if(from_last==T){
    T1$ord <- abs(max(T1$dt_ai) - T1$dt_ai)
    T1$ord_z <- abs(max(T1$dt_zi) - T1$dt_zi)
  }else{
    T1$ord <- abs(min(T1$dt_ai) - T1$dt_ai)
    T1$ord_z <- abs(min(T1$dt_zi) - T1$dt_zi)
  }

  # Custom sort
  if(!is.null(ref_sort)) {
    user_ord <- eval(parse(text = paste0("order(",paste0("df$", ref_sort, collapse = ", "),", T1$ord, -T1$ord_z)")))
  }else{
    user_ord <- order(T1$ord, -T1$ord_z)
  }

  names(user_ord) <- 1:length(user_ord)
  T1$user_ord <- as.numeric(names(sort(user_ord)))

  # Custom sort levels
  if(!is.null(ref_sort)){
    srd <- lapply(ref_sort, function(x){
      x <- as.numeric(as.factor(df[[x]]))
      formatC(x, width= nchar(max(x)), flag=0, format = "fg")
    })

    names(srd) <- ref_sort
    srd <- as.data.frame(srd, stringsAsFactors = F)
    srd <- eval(parse(text = paste0("paste0(",paste0("srd$", ref_sort, collapse = ",'-',"),")")))
    T1$c_sort <- as.numeric(as.factor(srd))
    rm(srd)
  }else{
    T1$c_sort <- 0
  }

  # Skip order
  if(!is.null(sk_od)){
    T1$skip_order <- df[[sk_od]]
  }else{
    T1$skip_order <- Inf
  }

  # Number of records at start
  tot <- nrow(T1)

  # Skip from episode grouping
  T1$epid[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <-
    T1$wind_id[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <-
    T1$sn[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")]

  T1$case_nm[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <-
    T1$wind_nm[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <- "Skipped"

  T1$dist_from_epid[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <-
    T1$dist_from_wind[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <- 0

  T1$tag[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <- 2

  # Number skipped because of `NA` criteria
  exa <- length(T1$case_nm[T1$case_nm=="Skipped"])

  if(!is.null(ds) & !all(toupper(dl_lst) == "ANY")){
    TH <- T1[T1$case_nm=="Skipped",]
    T1 <- T1[T1$case_nm!="Skipped",]

    # Check type of links
    links_check <- function(x, y, e) {
      if(tolower(e)=="l"){
        all(y %in% x & length(x)>1)
      }else if (tolower(e)=="g"){
        any(y %in% x)
      }
    }

    pds <- lapply(split(T1$ds, T1$cri), function(x, l=data_links){
      xlst <- rep(list(a =unique(x)), length(l))
      list(
        rq = any(unlist(mapply(links_check, xlst, l, names(l), SIMPLIFY = F)))
      )
    })

    p2 <- lapply(pds, function(x){x$rq})
    # skip if not required
    req_links <- unlist(p2[as.character(T1$cri)], use.names = F)
    T1$tag[req_links==F] <- 2
    T1$wind_nm[req_links==F] <- T1$case_nm[req_links==F] <- "Skipped"
    T1$epid[req_links==F] <- T1$wind_id[req_links==F] <- T1$sn[req_links==F]
    T1$dist_from_epid[req_links==F] <- T1$dist_from_wind[req_links==F] <- 0

    # Number skipped for not having the required `data_links`
    exa <- exa + length(req_links==F)
    T1 <- rbind(T1, TH); rm(TH)
  }

  # Making strata numeric for faster sorting
  T1$cri_l <- T1$cri
  T1$cri <- match(T1$cri, T1$cri[!duplicated(T1$cri)])

  min_tag <- min(T1$tag)
  min_episodes <- min(T1$episodes)

  rm(df)
  c <- 1
  grouped_epids <- T1[0,0]
  g_vrs <- c("sn","pr_sn","dt_ai","dt_zi", "ds","epid","wind_id","wind_nm","case_nm","skip_order", "c_sort", "user_ord", "ord","ord_z", "dist_from_wind", "dist_from_epid")
  while (min_tag != 2 & min_episodes <= episodes_max){
    # Seperate out grouped/skipped records
    grouped_epids <- rbind(grouped_epids,
                           T1[T1$tag ==2 & !is.na(T1$tag), g_vrs])
    # Exclude them from the main dataset
    T1 <- T1[T1$tag !=2 & !is.na(T1$tag),]

    # check for records to skip - `skip_order` and `episode_max`
    TR <- T1[order(T1$cri, -T1$tag, T1$user_ord, T1$sn),]
    skip_cris <- TR$cri[(TR$c_sort > TR$skip_order |
                           TR$tag==0 & TR$episodes + 1 > episodes_max) & !duplicated(TR$cri) & !is.na(TR$cri)]
    skip_cris <- skip_cris[!duplicated(skip_cris)]

    # Assign unique IDs to skipped records
    skpd <- length(T1$tag[T1$cri %in% skip_cris])
    T1$tag[T1$cri %in% skip_cris] <- 2
    T1$wind_nm[T1$cri %in% skip_cris] <- T1$case_nm[T1$cri %in% skip_cris] <- "Skipped"
    T1$epid[T1$cri %in% skip_cris] <- T1$wind_id[T1$cri %in% skip_cris] <- T1$sn[T1$cri %in% skip_cris]
    T1$dist_from_epid[T1$cri %in% skip_cris] <- T1$dist_from_wind[T1$cri %in% skip_cris] <- 0
    # Separate out skipped records
    grouped_epids <- rbind(grouped_epids,
                           T1[T1$tag ==2 & !is.na(T1$tag), g_vrs])

    # Exclude them from the main dataset
    T1 <- T1[T1$tag !=2 & !is.na(T1$tag),]

    # Reference events
    TR <- TR[!(TR$tag==0 & TR$episodes + 1 > episodes_max) &
               !(TR$c_sort > TR$skip_order) &
               !duplicated(TR$cri) &
               !is.na(TR$cri),
             c("sn", "cri", "dt_ai", "ep_a1", "ep_a2",
               "rc_a1", "rc_a2", "ep_b1", "ep_b2", "rc_b1",
               "rc_b2", "dt_zi","epid", "ep_c1", "ep_c2",
               "rc_c1", "rc_c2", "ep_d1", "ep_d2", "rc_d1", "rc_d2",
               "tag", "roll", "case_nm", "methods", "crx_e", "crx_r",
               "ct_rng_e", "ct_rng_r")]
    names(TR) <- paste0("tr_",names(TR))

    # Early break if there are no more reference events
    if(nrow(TR)==0) {
      if(skpd >0) cat(paste0(fmt(skpd), " record(s); ", fmt(skpd)," skipped\n"))
      break
    }

    if(display){cat(paste0("Episode or recurrence window ",c,".\n"))}
    if(display & exa >0 & c ==1) cat(paste0(fmt(tot), " record(s); ", fmt(exa)," excluded from episode grouping. ", fmt(tot-exa), " left to group.\n"))
    # Number of records as of current iteration
    total_1 <- nrow(T1)

    T1 <- dplyr::left_join(T1, TR, by= c("cri"="tr_cri"))

    # Reference event
    T1$lr <- ifelse(T1$tr_sn == T1$sn & !is.na(T1$tr_sn),1,0)

    # Case and recurrence lengths
    T1$int <- diyar::number_line(T1$dt_ai, T1$dt_zi)

    # Case and recurrence lengths of reference events
    T1$tr_ep_a <- suppressWarnings(diyar::number_line(T1$tr_ep_a1, T1$tr_ep_a2))
    T1$tr_rc_a <- suppressWarnings(diyar::number_line(T1$tr_rc_a1, T1$tr_rc_a2))
    T1$tr_ep_b <- suppressWarnings(diyar::number_line(T1$tr_ep_b1, T1$tr_ep_b2))
    T1$tr_rc_b <- suppressWarnings(diyar::number_line(T1$tr_rc_b1, T1$tr_rc_b2))
    T1$tr_ep_c <- suppressWarnings(diyar::number_line(T1$tr_ep_c1, T1$tr_ep_c2))
    T1$tr_rc_c <- suppressWarnings(diyar::number_line(T1$tr_rc_c1, T1$tr_rc_c2))
    T1$tr_ep_d <- suppressWarnings(diyar::number_line(T1$tr_ep_d1, T1$tr_ep_d2))
    T1$tr_rc_d <- suppressWarnings(diyar::number_line(T1$tr_rc_d1, T1$tr_rc_d2))

    # tr_*_int_a - specified range in specified direction
    # tr_*_int_b - specified range in opposite  direction
    # tr_*_int_c - maximum   range in specified direction
    # tr_*_int_d - maximum   range in opposite  direction

    T1$tr_int <- suppressWarnings(diyar::number_line(T1$tr_dt_ai, T1$tr_dt_zi))
    T1$tr_c_int_e <- T1$tr_r_int_e <- T1$tr_c_int_d <- T1$tr_c_int_c <- T1$tr_c_int_b <- T1$tr_c_int_a <- T1$tr_r_int_d <- T1$tr_r_int_b <- T1$tr_r_int_a <-  T1$tr_r_int_c <-  T1$tr_c_int_a <- T1$tr_int

    # Direction in time for episode grouping
    chr_dir <- ifelse(from_last==F, 1, -1)

    # Is check for the opposite direction required?
    bdl_e <- T1$crx_e ==T | bi_direction==T; bdl_r <- T1$crx_r ==T | bi_direction==T

    T1$tr_c_int_a <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_c_int_a) + (diyar::left_point(T1$tr_ep_a) * chr_dir), diyar::end_point(T1$tr_c_int_a) + (diyar::right_point(T1$tr_ep_a) * chr_dir)))
    T1$tr_r_int_a <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_r_int_a) + (diyar::left_point(T1$tr_rc_a) * chr_dir), diyar::end_point(T1$tr_r_int_a) + (diyar::right_point(T1$tr_rc_a) * chr_dir)))

    T1$tr_c_int_b <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_c_int_b) + (diyar::left_point(T1$tr_ep_b) * chr_dir), diyar::end_point(T1$tr_c_int_b) + (diyar::right_point(T1$tr_ep_b) * chr_dir)))
    T1$tr_r_int_b <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_r_int_b) + (diyar::left_point(T1$tr_rc_b) * chr_dir), diyar::end_point(T1$tr_r_int_b) + (diyar::right_point(T1$tr_rc_b) * chr_dir)))

    T1$tr_c_int_c <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_c_int_c) + (diyar::left_point(T1$tr_ep_c) * chr_dir), diyar::end_point(T1$tr_c_int_c) + (diyar::right_point(T1$tr_ep_c) * chr_dir)))
    T1$tr_r_int_c <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_r_int_c) + (diyar::left_point(T1$tr_rc_c) * chr_dir), diyar::end_point(T1$tr_r_int_c) + (diyar::right_point(T1$tr_rc_c) * chr_dir)))

    T1$tr_c_int_d <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_c_int_d) + (diyar::left_point(T1$tr_ep_d) * chr_dir), diyar::end_point(T1$tr_c_int_d) + (diyar::right_point(T1$tr_ep_d) * chr_dir)))
    T1$tr_r_int_d <- suppressWarnings(diyar::number_line(diyar::end_point(T1$tr_r_int_d) + (diyar::left_point(T1$tr_rc_d) * chr_dir), diyar::end_point(T1$tr_r_int_d) + (diyar::right_point(T1$tr_rc_d) * chr_dir)))

    # N:B Only check for overlaps where necessary
    T1$r1 <- T1$r2 <- T1$r4 <- T1$r5 <- T1$r6 <- T1$c1 <- T1$c2 <- T1$p1 <- T1$c4 <- T1$c5 <- T1$p2 <- F

    T1$ec_chk <- T1$tr_tag %in% c(0,1.4,1.6) & !is.na(T1$tr_tag)
    T1$rc_chk <- T1$tr_tag %in% c(0,1,1.5) & !is.na(T1$tr_tag)

    T1$c1[T1$ec_chk] <- diyar::overlaps(T1$int[T1$ec_chk], T1$tr_c_int_a[T1$ec_chk], methods = T1$methods[T1$ec_chk])
    T1$c2[bdl_e & T1$ec_chk] <- diyar::overlaps(T1$int[bdl_e & T1$ec_chk], T1$tr_c_int_b[bdl_e & T1$ec_chk], methods = T1$methods[bdl_e & T1$ec_chk])
    T1$k1 <- diyar::overlap(T1$int, T1$tr_int)
    T1$p1[include_index_period & !T1$crx_e & T1$k1] <- T

    T1$r1[T1$rc_chk] <- diyar::overlaps(T1$int[T1$rc_chk], T1$tr_r_int_a[T1$rc_chk], methods = T1$methods[T1$rc_chk])
    T1$r2[bdl_r & T1$rc_chk] <- diyar::overlaps(T1$int[bdl_r & T1$rc_chk], T1$tr_r_int_b[bdl_r & T1$rc_chk], methods = T1$methods[bdl_r & T1$rc_chk])
    T1$k2 <- diyar::overlap(T1$int, T1$tr_int)
    T1$p2[include_index_period & !T1$crx_r & T1$k2] <- T

    T1$c4[skip_if_b4_lengths & T1$ec_chk] <- diyar::overlaps(T1$int[skip_if_b4_lengths & T1$ec_chk], T1$tr_c_int_c[skip_if_b4_lengths & T1$ec_chk], methods = T1$methods[skip_if_b4_lengths & T1$ec_chk])
    T1$c5[bdl_e & skip_if_b4_lengths & T1$ec_chk] <- diyar::overlaps(T1$int[bdl_e & skip_if_b4_lengths & T1$ec_chk], T1$tr_c_int_d[bdl_e & skip_if_b4_lengths & T1$ec_chk], methods = T1$methods[bdl_e & skip_if_b4_lengths & T1$ec_chk])

    T1$r4[skip_if_b4_lengths & T1$rc_chk] <- diyar::overlaps(T1$int[skip_if_b4_lengths & T1$rc_chk], T1$tr_r_int_c[skip_if_b4_lengths & T1$rc_chk], methods = T1$methods[skip_if_b4_lengths & T1$rc_chk])
    T1$r5[bdl_r & skip_if_b4_lengths & T1$rc_chk] <- diyar::overlaps(T1$int[bdl_r & skip_if_b4_lengths & T1$rc_chk], T1$tr_r_int_d[bdl_r & skip_if_b4_lengths & T1$rc_chk], methods = T1$methods[bdl_r & skip_if_b4_lengths & T1$rc_chk])

    T1$r_rng1 <- T1$c_rng1 <- F

    T1$c_rng1 <- T1$lr==1 | T1$c1 | T1$c2 | T1$p1
    T1$r_rng1 <- T1$lr==1 | T1$r1 | T1$r2 | T1$p1

    T1$c_rng2 <- T1$lr==1 | T1$c4 | T1$c5 | T1$p2
    T1$r_rng2 <- T1$lr==1 | T1$r4 | T1$r5 | T1$p2

    skp_crxt <- T1$cri[T1$lr!=1 & ((T1$c_rng1 & T1$p1 !=T)  | (T1$r_rng1 ==T & T1$p2 !=T))]
    skp_crxt <- skp_crxt[!duplicated(skp_crxt)]
    T1$c_rng2[!T1$cri %in% skp_crxt & T1$c_rng2==T & T1$lr!=1] <- F
    T1$r_rng2[!T1$cri %in% skp_crxt & T1$r_rng2==T & T1$lr!=1] <- F

    # Distance from window's ref event
    T1$dist_from_wind <- ((as.numeric(T1$dt_ai) + as.numeric(T1$dt_zi)) *.5) - ((as.numeric(T1$tr_dt_ai) + as.numeric(T1$tr_dt_zi)) *.5)
    # Distance from episodes's ref event
    T1$dist_from_epid <- ifelse(T1$tr_case_nm=="", ((as.numeric(T1$dt_ai) + as.numeric(T1$dt_zi)) *.5) - ((as.numeric(T1$tr_dt_ai) + as.numeric(T1$tr_dt_zi)) *.5), T1$dist_from_epid)

    # Event type
    # 1 - Reference event (Case)
    # 2 - Duplicate of a case event
    # 6 - Recurrent event
    # 7 - Duplicate of a recurrent event
    # 9 - Reference event for a case window. - 1 & 9 are case windows
    # 10 - Duplicate event for a case window. - 2 & 10 are duplicates in case windows
    # Episode assignment -------
    T1$epid_type <- ifelse(
      T1$r_rng1 & T1$rc_chk,
      ifelse(T1$case_nm=="Duplicate", 7,6), 0
    )

    T1$epid_type <- ifelse(
      T1$c_rng1 & T1$ec_chk,
      ifelse(T1$tr_tag==0,
             ifelse(T1$lr==1, 1,2),
             ifelse(T1$lr==1, 9, 10)),
      T1$epid_type
    )

    T1$c_hit <- ifelse(T1$epid_type %in% c(1,2,7,6,10) | (T1$lr==1), 1, 0)

    T1$epid <- ifelse(
      T1$c_hit==1, ifelse(T1$tr_tag ==0 & !is.na(T1$tr_tag), T1$tr_sn, T1$tr_epid),
      T1$epid
    )

    # Identify events between the event itself and the lower cut-off point
    jmp_c <- !T1$c_rng1 & T1$c_rng2
    jmp_r <- !T1$r_rng1 & T1$r_rng2

    # Specific adjustment
    sadj <- jmp_r & T1$c_rng2
    sadj <- T1$epid[sadj]
    sadj <- sadj[!duplicated(sadj) & sadj !=sn_ref]
    jmp_r[T1$epid %in% sadj] <- F

    # ... Skip  these is required
    skpd <- 0
    if(skip_if_b4_lengths==T){
      # assign unique IDs to skipped records
      skpd <- skpd + length(jmp_r[jmp_r | jmp_c])
      T1$wind_nm[jmp_r | jmp_c] <- T1$case_nm[jmp_r | jmp_c] <- "Skipped"
      T1$epid[jmp_r | jmp_c] <- T1$wind_id[jmp_r | jmp_c] <- T1$sn[jmp_r | jmp_c]
      T1$dist_from_wind[jmp_r | jmp_c] <- T1$dist_from_epid[jmp_r | jmp_c] <- 0
      T1$tag[jmp_r | jmp_c] <- 2
    }

    # Seperate out skipped records
    grouped_epids <- rbind(grouped_epids,
                           T1[T1$tag ==2 & !is.na(T1$tag), g_vrs])
    # Exclude from the main dataset
    T1 <- T1[T1$tag !=2 & !is.na(T1$tag),]

    T1$wind_id <- ifelse(T1$c_hit==1 & (T1$lr !=1 | (T1$lr ==1 & T1$tr_case_nm=="")), T1$tr_sn, T1$wind_id)

    T1$wind_nm <- ifelse(T1$wind_nm=="" &T1$c_hit ==1,
                         ifelse((T1$tr_tag %in% c(1, 1.5) | (T1$tr_tag ==0 & T1$c_rng1==F & T1$r_rng2==T)), "Recurrence", "Case"),
                         T1$wind_nm)

    T1$case_nm <- ifelse(
      T1$c_hit==1 & !T1$tag %in% c(1.4,1.6),
      ifelse(T1$epid_type %in% c(1,0), "Case",
             ifelse(T1$epid_type==6 & episode_type=="rolling","Recurrent","Duplicate")),
      T1$case_nm
    )
    # ---------

    vrs <- names(T1)[!grepl("_rng1|_rng2|_int", names(T1))]
    T1 <- T1[order(T1$cri, T1$epid, T1$user_ord, T1$sn),]

    T1$episodes <- ifelse(T1$tr_tag==0 & !is.na(T1$tr_tag), T1$episodes + 1, T1$episodes)
    T1$tag <- ifelse(T1$c_hit==1 | (!T1$tag %in% c(NA,0,2) & T1$sn == T1$tr_sn), 2, T1$tag)
    T1$mrk_x <- paste(T1$cri, T1$tag, sep="-")

    T1$fg_a <- rep(rle(T1$cri)$lengths, rle(T1$cri)$lengths)
    T1$fg_x <- rep(rle(T1$mrk_x)$lengths, rle(T1$mrk_x)$lengths)
    T1$fg_c <- ifelse(T1$fg_a==T1$fg_x & T1$tag==2, 1,0)

    T1$mrk_z <- paste0(T1$case_nm, T1$epid, T1$lr)
    T1$mrk_z2 <- paste0(T1$case_nm, T1$epid)
    T1$case_nm <- ifelse(T1$lr !=1 &
                           T1$case_nm=="Recurrent" &
                           (duplicated(T1$mrk_z))
                         ,"Duplicate", T1$case_nm)

    if(min(T1$fg_c)==1) {
      vrs <- names(T1)[!grepl("^tr|^fg|^mrk", names(T1))]
      T1 <- T1[vrs]
      tagged_1 <- length(T1$epid[!T1$epid %in% c(sn_ref,NA) & T1$tag==2])

      if(display){
        cat(paste0(fmt(total_1), " record(s); ", fmt(tagged_1)," grouped to episodes", ifelse(skpd>0, paste0(", ",fmt(skpd)," skipped"), ""), " and ", fmt(total_1-tagged_1-skpd)," left to group.\n"))
      }
      break
    }

    if (episode_type=="rolling"){
      T1$d_grp <- ifelse(T1$case_nm=="Duplicate" & T1$sn != T1$tr_sn & !is.na(T1$tr_sn), 1, 0)

      pds2 <- lapply(split(T1$d_grp, T1$epid), max)
      T1$d_grp <- as.numeric(pds2[as.character(T1$epid)])

      T1$d_ord <- ifelse(T1$case_nm=="Duplicate" & T1$lr !=1
                         # & ((T1$ec_chk & !T1$p1) | (T1$rc_chk & !T1$p2))
                         # & !(T1$ec_chk & T1$p1) &
                         # & !(T1$rc_chk & T1$p2)
                         & !T1$k1 & !T1$k2
                         , T1$user_ord, NA)
      pds2 <- lapply(split(T1$d_ord, T1$epid), function(x){
        suppressWarnings(min(x, na.rm=T))
      })
      T1$d_ord <- as.numeric(pds2[as.character(T1$epid)])
    }else{
      T1$d_ord <- T1$d_grp <- 0
    }

    # Number of recurrence periods so far - Recalculate
    fx <- T1$cri[T1$epid!=sn_ref & T1$lr!=1 & T1$tr_tag==0 & T1$tr_case_nm=="" & T1$case_nm=="Recurrent" &
                   !(T1$d_grp == 1 & T1$user_ord > T1$d_ord)]
    fx <- fx[!duplicated(fx)]
    shc <- T1$cri %in% fx

    T1$roll <- ifelse(T1$tr_tag %in% c(1, 1.5, 1.4) & !is.na(T1$tr_tag) | (shc == T), T1$roll + 1, ifelse(T1$tr_tag == 0 & shc != T, 0, T1$roll))

    # Chose the next reference event
    T1$mrk_z <- paste0(T1$case_nm, T1$epid)
    T1$tag <- ifelse(episode_type == "rolling" &
                       (T1$roll < rolls_max |(case_for_recurrence==T & T1$roll < rolls_max+1 & T1$tr_tag != 1.6) )&
                       !(T1$lr==1 & T1$tr_tag %in% c(1, 1.5, 1.4, 1.6)) & T1$case_nm %in% c("Duplicate","Recurrent"),
                     ifelse(T1$case_nm == "Duplicate",
                            ifelse(T1$case_nm =="Duplicate" & !duplicated(T1$mrk_z, fromLast = T) &
                                     recurrence_from_last == T,
                                   ifelse(case_for_recurrence==T & ((T1$tr_tag==1.5) | (T1$tr_tag ==0 & T1$c_rng1==F & T1$r_rng2==T)), 1.6,1.5), 2),
                            ifelse(T1$case_nm=="Recurrent" &
                                     T1$d_grp ==1  &
                                     T1$user_ord < T1$d_ord & T1$d_ord != Inf &
                                     T1$tr_case_nm %in% c("Duplicate","") &
                                     recurrence_from_last ==T, 2,
                                   ifelse(case_for_recurrence==F, 1,1.4))),
                     T1$tag)

    tagged_1 <- length(T1$epid[!T1$epid %in% c(sn_ref,NA) & T1$tag==2])
    if(display){
      cat(paste0(fmt(total_1), " record(s); ", fmt(tagged_1)," grouped to episodes", ifelse(skpd>0, paste0(", ",fmt(skpd)," skipped"), ""), " and ", fmt(total_1-tagged_1-skpd)," left to group.\n"))
    }

    T1 <- T1[names(T1)[!grepl("^tr|^fg|^mrk", names(T1))]]
    min_tag <- min(T1$tag)
    min_episodes <- min(T1$episodes)

    c = c+1
  }
  # Append all events back together
  T1 <- rbind(T1[g_vrs], grouped_epids)
  rm(grouped_epids)

  # Assign unique IDs to ungrouped episodes
  T1$wind_nm[T1$epid==sn_ref] <- T1$case_nm[T1$epid==sn_ref] <- "Skipped"
  T1$epid[T1$epid==sn_ref] <- T1$wind_id[T1$epid==sn_ref] <- T1$sn[T1$epid==sn_ref]
  T1$dist_from_wind[T1$epid==sn_ref] <- T1$dist_from_epid[T1$epid==sn_ref] <- 0

  # Correction for window Ids with "Case" and "Recurrence" labels. These should be "Recurrence"
  tmp <- split(T1$wind_id, T1$wind_nm)
  tmp$Case <- tmp$Case[!tmp$Case %in% tmp$Recurrence]
  T1$wind_nm[T1$wind_id %in% tmp[["Case"]]] <- "Case"
  T1$wind_nm[T1$wind_id %in% tmp[["Recurrence"]]] <- "Recurrence"
  T1$wind_nm[T1$wind_id %in% tmp[["Skipped"]]] <- "Skipped"

  # Drop 'duplicate' events if required
  if(deduplicate) T1 <- T1[T1$case_nm!="Duplicate",]

  if(is.null(ds)){
    T1 <- T1[order(T1$pr_sn),]
  }else{
    # epid_dataset not needed for skipped events
    TH <- T1[T1$case_nm=="Skipped",]
    TH$epid_dataset <- TH$ds
    T1 <- T1[T1$case_nm!="Skipped",]

    # Check type of links
    links_check <- function(x, y, e) {
      if(tolower(e)=="l"){
        all(y %in% x & length(x)>1)
      }else if (tolower(e)=="g"){
        any(y %in% x)
      }
    }

    pds <- lapply(split(T1$ds, T1$epid), function(x, l=data_links){
      xlst <- rep(list(a =unique(x)), length(l))
      r <- list(ds = paste0(sort(unique(x)), collapse=","))
      if(!all(toupper(dl_lst) == "ANY")) r["rq"] <- any(unlist(mapply(links_check, xlst, l, names(l), SIMPLIFY = F)))
      return(r)
    })

    p1 <- lapply(pds, function(x){x$ds})
    T1$epid_dataset <- unlist(p1[as.character(T1$epid)], use.names = F)

    if(!all(toupper(dl_lst) == "ANY")){
      p2 <- lapply(pds, function(x){x$rq})
      # unlink if not required
      req_links <- unlist(p2[as.character(T1$epid)], use.names = F)
      T1$dist_from_wind[req_links==F] <- T1$dist_from_epid[req_links==F] <- 0
      T1$epid_dataset[req_links==F] <- T1$ds[req_links==F]
      T1$wind_nm[req_links==F] <- T1$case_nm[req_links==F] <- "Skipped"
      T1$epid[req_links==F] <- T1$wind_id[req_links==F] <- T1$sn[req_links==F]
    }

    T1 <- rbind(T1, TH); rm(TH)
    T1 <- T1[order(T1$pr_sn),]
  }


  diff_unit <- ifelse(tolower(episode_unit) %in% c("second","minutes"),
                      paste0(substr(tolower(episode_unit),1,3),"s"),
                      tolower(episode_unit))

  diff_unit <- ifelse(diff_unit %in% c("months","year"), "days", diff_unit)

  # Episode stats if required
  if(group_stats == T){
    # Group stats not needed for skipped events
    TH <- T1[T1$case_nm=="Skipped",]
    TH$a <- as.numeric(TH$dt_ai)
    TH$z <- as.numeric(TH$dt_zi)

    T1 <- T1[T1$case_nm!="Skipped",]
    T1$a <- as.numeric(lapply(split(as.numeric(T1$dt_ai), T1$epid), ifelse(from_last==F, min, max))[as.character(T1$epid)])
    T1$z <- as.numeric(lapply(split(as.numeric(T1$dt_zi), T1$epid), ifelse(from_last==F, max, min))[as.character(T1$epid)])
    T1 <- rbind(T1, TH); rm(TH)

    if(dt_grp == T){
      T1$a <- as.POSIXct(T1$a, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      T1$z <- as.POSIXct(T1$z, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      T1$epid_length <- difftime(T1$z, T1$a, units = diff_unit)
    }else{
      T1$epid_length <- T1$z - T1$a
    }
    T1 <- T1[order(T1$epid),]

    T1$epid_total <- rep(rle(T1$epid)$lengths, rle(T1$epid)$lengths)
    T1 <- T1[order(T1$pr_sn),]

    T1$epid_interval <- diyar::number_line(T1$a, T1$z, id=T1$sn, gid = T1$epid)

    vrs <- names(T1)[!grepl("^a$|^z$", names(T1))]
    T1 <- T1[vrs]
  }

  vrs <- names(T1)[!grepl("^pr_sn|^dt_|^ord|^ep_a|^user_ord|^skip_ord|^c_sort", names(T1))]
  T1 <- T1[vrs]

  if(dt_grp==T){
    T1$dist_from_wind <- T1$dist_from_wind / diyar::episode_unit[[episode_unit]]
    T1$dist_from_wind <- as.difftime(T1$dist_from_wind, units = diff_unit)

    T1$dist_from_epid <- T1$dist_from_epid / diyar::episode_unit[[episode_unit]]
    T1$dist_from_epid <- as.difftime(T1$dist_from_epid, units = diff_unit)
  }

  unique_ids <- length(T1[!duplicated(T1$epid) & !duplicated(T1$epid, fromLast = T),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste0(pd, "Episode grouping complete: " ,fmt(unique_ids)," record(s) with a unique ID. \n"))
  if(to_s4) T1 <- diyar::to_s4(T1)
  T1
}

fixed_episodes_archive <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days",
                                   episodes_max = Inf, skip_if_b4_lengths = TRUE, data_source = NULL, data_links = "ANY",
                                   custom_sort = NULL, skip_order =NULL, from_last = FALSE,
                                   overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain","overlap","none"),
                                   overlap_methods =  "overlap", bi_direction= FALSE, group_stats = FALSE,
                                   display = TRUE, deduplicate = FALSE, x, to_s4 = TRUE, include_index_period = TRUE){

  if(missing(date) & missing(x)) stop("argument 'date' is missing, with no default")
  if(missing(case_length)) stop("argument 'case_length' is missing, with no default")

  # if(to_s4 == FALSE){
  #   if (is.null(getOption("diyar.fixed_episodes.output"))){
  #     options("diyar.fixed_episodes.output"= TRUE)
  #   }
  #   if (getOption("diyar.fixed_episodes.output")){
  #     message(paste("The default output of fixed_episodes() will be changed to epid objects in the next release.",
  #                   "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
  #                   "",
  #                   "# New way - `epid` objects",
  #                   "df$epids <- fixed_episodes(case_length= df$x, to_s4 = TRUE)",
  #                   "",
  #                   "This message is displayed once per session.", sep = "\n"))
  #   }
  #   options("diyar.fixed_episodes.output"= FALSE)
  # }
  if (!missing(x)) {
    warning("'x' is deprecated; please use 'date' instead."); date <- x
  }

  logs_check <- logicals_check(c("from_last", "bi_direction", "group_stats", "display", "deduplicate", "to_s4", "skip_if_b4_lengths"))
  if(logs_check!=T) stop(logs_check)

  if(!is.character(overlap_method)) stop("'overlap_method' must be a character object")
  if(!(length(case_length) %in% c(1, length(date)))) stop("length of 'case_length' must be 1 or the same as 'date'")
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop("length of 'strata' must be 1 or the same as 'date'")
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop("length of 'data_source' must be 1 or the same as 'date'")
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop("length of 'custom_sort' must be 1 or the same as 'date'")
  if(!(length(skip_order) %in% c(1, length(date)) | (length(skip_order) ==0 & is.null(skip_order)))) stop("length of 'skip_order' must be 1 or the same as 'date'")

  if(missing(overlap_methods) & !missing(overlap_method)) {
    warning("'overlap_method' is deprecated. Please use 'overlap_methods' instead.")
    m <- paste(overlap_method,sep="", collapse = "|")
  }else{
    m <- overlap_methods
  }

  if(!(length(m) %in% c(1, length(date)))) stop("length of 'overlap_methods' must be 1 or the same as 'date'")
  o <- unique(unlist(strsplit(unique(m), split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween","overlap","none")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end', 'inbetween' or 'none' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  df <- data.frame(dts = date, stringsAsFactors = FALSE)
  df$epl <- case_length

  if(is.null(strata)){
    df$sr <- 1
  }else{
    df$sr <- strata
  }

  if(is.null(sn)){
    df$sn <- 1:nrow(df)
  }else{
    df$sn <- sn
  }

  if(is.null(data_source)){
    ds <- NULL
  }else{
    df$ds <- data_source
    ds <- "ds"
  }

  if(is.null(custom_sort)){
    df$user_srt <- 0
  }else{
    df$user_srt <- as.numeric(as.factor(custom_sort))
  }

  if(is.null(skip_order)){
    df$skip_order <- Inf
  }else{
    df$skip_order <- skip_order
  }

  df$method <- m

  if(is.null(data_source)){
    episode_group_archive(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                          bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                          from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                          display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                          include_index_period=include_index_period)
  }else{
    episode_group_archive(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                          bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                          from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                          display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                          include_index_period=include_index_period)
  }
}
rolling_episodes_archive <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days",
                                     episodes_max = Inf, rolls_max = Inf, skip_if_b4_lengths = TRUE, data_source = NULL, data_links = "ANY",
                                     custom_sort = NULL, skip_order = NULL, from_last = FALSE,
                                     overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain","overlap","none"),
                                     overlap_methods =  "overlap", bi_direction= FALSE, group_stats = FALSE,
                                     display = TRUE, deduplicate = FALSE, x, to_s4 = TRUE, recurrence_from_last = TRUE,
                                     case_for_recurrence =FALSE, include_index_period=TRUE){

  if(missing(date) & missing(x)) stop("argument 'date' is missing, with no default")
  if(missing(case_length)) stop("argument 'case_length' is missing, with no default")

  # if(to_s4 == FALSE){
  #   if (is.null(getOption("diyar.rolling_episodes.output"))){
  #     options("diyar.rolling_episodes.output"= TRUE)
  #   }
  #   if (getOption("diyar.rolling_episodes.output")){
  #     message(paste("The default output of rolling_episodes() will be changed to epid objects in the next release.",
  #                   "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
  #                   "",
  #                   "# New way - `epid` objects",
  #                   "df$epids <- rolling_episodes(case_length= df$x, to_s4 = TRUE)",
  #                   "",
  #                   "This message is displayed once per session.", sep = "\n"))
  #   }
  #   options("diyar.rolling_episodes.output"= FALSE)
  # }
  if (!missing(x)) {
    warning("'x' is deprecated; please use 'date' instead."); date <- x
  }

  logs_check <- logicals_check(c("from_last", "bi_direction", "group_stats", "display", "deduplicate", "to_s4", "recurrence_from_last", "case_for_recurrence", "skip_if_b4_lengths"))
  if(logs_check!=T) stop(logs_check)

  if(!is.character(overlap_method)) stop("'overlap_method' must be a character object")
  if(!(length(case_length) %in% c(1, length(date)))) stop("length of 'case_length' must be 1 or the same as 'date'")
  if(!(length(recurrence_length) %in% c(1, length(date)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop("length of 'recurrence_length' must be 1 or the same as 'date'")
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop("length of 'strata' must be 1 or the same as 'date'")
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop("length of 'data_source' must be 1 or the same as 'date'")
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop("length of 'custom_sort' must be 1 or the same as 'date'")
  if(!(length(skip_order) %in% c(1, length(date)) | (length(skip_order) ==0 & is.null(skip_order)))) stop("length of 'skip_order' must be 1 or the same as 'date'")

  if(missing(overlap_methods) & !missing(overlap_method)) {
    warning("'overlap_method' is deprecated. Please use 'overlap_methods' instead.")
    m <- paste(overlap_method,sep="", collapse = "|")
  }else{
    m <- overlap_methods
  }

  if(!(length(m) %in% c(1, length(date)))) stop("length of 'overlap_methods' must be 1 or the same as 'date'")
  o <- unique(unlist(strsplit(unique(m), split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween","overlap","none")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'overlap', 'exact', 'across', 'chain', 'aligns_start', 'aligns_end', 'inbetween' or 'none' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  df <- data.frame(dts = date, stringsAsFactors = FALSE)
  df$epl <- case_length

  if(is.null(strata)){
    df$sr <- 1
  }else{
    df$sr <- strata
  }

  if(is.null(sn)){
    df$sn <- 1:nrow(df)
  }else{
    df$sn <- sn
  }

  if(is.null(data_source)){
    ds <- NULL
  }else{
    df$ds <- data_source
    ds <- "ds"
  }

  if(is.null(custom_sort)){
    df$user_srt <- 0
  }else{
    df$user_srt <- as.numeric(as.factor(custom_sort))
  }

  if(is.null(skip_order)){
    df$skip_order <- Inf
  }else{
    df$skip_order <- skip_order
  }

  if(is.null(recurrence_length)){
    df$rc_epl <- df$epl
  }else{
    df$rc_epl <- recurrence_length
  }

  df$method <- m

  if(is.null(data_source)){
    episode_group_archive(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                          bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                          from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                          display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                          recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }else{
    episode_group_archive(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                          bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                          from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                          display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                          recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }

}

record_group_archive <- function(df, sn=NULL, criteria,
                                 sub_criteria=NULL, strata = NULL, data_source = NULL,
                                 group_stats=FALSE, display=TRUE, to_s4 = TRUE){

  if(missing(df)) stop("argument 'df' is missing, with no default")
  if(missing(criteria)) stop("argument 'criteria' is missing, with no default")

  if(!is.data.frame(df)) stop(paste("A dataframe is required"))
  rng_d <- as.character(substitute(df))
  . <- NULL

  # check that only logicals are passed to arguments that expect logicals.
  logs_check <- logicals_check(c("group_stats", "display", "to_s4"))
  if(logs_check!=T) stop(logs_check)

  # Suggesting the use `epid` objects
  # if(to_s4 == FALSE){
  #   if (is.null(getOption("diyar.record_group.output"))){
  #     options("diyar.record_group.output"= TRUE)
  #   }
  #   if (getOption("diyar.record_group.output")){
  #     message(paste("The default output of record_group() will be changed to pid objects in the next release.",
  #                   "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
  #                   "",
  #                   "# Old way - merge or bind (col) results back to the `df`",
  #                   "df <- cbind(df, record_group(df, criteria= x))",
  #                   "",
  #                   "# New way - `pid` objects",
  #                   "df$pids <- record_group(df, criteria= x, to_s4 = TRUE)",
  #                   "This message is displayed once per session.", sep = "\n"))
  #   }
  #   options("diyar.record_group.output"= FALSE)
  # }

  # validations
  ds <- enq_vr(substitute(data_source))
  df_vars <- names(df)
  rd_sn <- enq_vr(substitute(sn))
  st <- enq_vr(substitute(strata))

  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)
  cri_lst <- enq_vr(substitute(criteria))

  # Check that col names exist
  if(any(!unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)), !unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found"))
  }

  # Record indentifier
  if(is.null(rd_sn)){
    df$sn <- 1:nrow(df)
  }else{
    dp_check <- duplicates_check(df[[rd_sn]])
    if(dp_check!=T) stop(paste0("duplicate record indentifier ('sn') in indexes ",dp_check))
    df$sn <- df[[rd_sn]]
  }

  # Dataset identifier
  if(is.null(ds)){
    df$dsvr <- "A"
  }else{
    df$dsvr <- eval(parse(text = paste0("paste0(",paste0("df$", ds, collapse = ",'-',"),")")))
  }

  # Strata
  if(is.null(st)){
    df$strt <- "A"
  }else{
    df$strt <- eval(parse(text = paste0("paste0(",paste0("df$", st, collapse = ",'-',"),")")))
  }

  # Prep
  df <- df[unique(c("sn",cri_lst,sub_cri_lst,"dsvr", "strt"))]
  df$pr_sn <- 1:nrow(df)
  df$m_tag <- df$tag <- 0
  df$pid_cri <- Inf
  df$link_id <- df$pid <- sn_ref <- min(df$sn)-1
  cri_no <- length(cri_lst)

  # `number_line` object as subcriteria
  for(i in 1:cri_no){
    if(is.number_line(df[[cri_lst[i]]])){
      #dummy var
      rp_vr <- paste0("dmvr_d",i)
      df[[rp_vr]] <- 1
      # make `number_line` object a sub_criteria for the dummy criteria
      if(is.null(sub_criteria)) sub_criteria <- list()
      sub_criteria[paste0("s",i,"zr")] <- cri_lst[i]
      # update criteria list
      cri_lst[i] <- rp_vr
    }
  }

  # update 'sub_cri_lst'
  sub_cri_lst <- unique(unlist(sub_criteria, use.names = FALSE))

  nls <- lapply(names(df), function(x){
    is.number_line(df[[x]])
  })

  nls_nms <- names(df)[as.logical(nls)]
  if(length(nls_nms) >0){
    nls <- df[nls_nms]

    for(i in nls_nms){
      df[[i]] <- df$pr_sn
    }
  }

  # Range matching
  range_match <- function(x, tr_x) {
    if(any(!diyar::overlaps(diyar::as.number_line(x@gid), x))) {
      rng_i <- paste(which(!diyar::overlaps(diyar::as.number_line(x@gid), x)), collapse = ",", sep="")
      rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]

      stop(paste("Range matching error: Actual value (gid) is out of range in '",rng_d,"$",rng_v,"[c(",rng_i,")]'",sep=""))
    }

    diyar::overlaps(diyar::as.number_line(x@gid), tr_x)
  }

  # Exact matching
  exact_match <- function(x, tr_x) {
    x <- ifelse(is.na(x), "", as.character(x))
    tr_x <- ifelse(is.na(tr_x), "", as.character(tr_x))
    x == tr_x
  }

  for(i in 1:cri_no){
    if(display) cat(paste("\nGroup criteria ",i," - ","`",cri_lst[i],"`", sep=""))

    # Current matching criteria
    df$cri <- ifelse(df[[cri_lst[i]]] %in% c("", NA), NA, paste0(df$strt,"-", df[[cri_lst[i]]]))

    # Fetch corresponding matching criteria
    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, grepl(paste("s",i,sep=""), attr))
    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){
        ifelse(class(nls[[x]]) == "number_line",
               paste0("range_match(nls$",x,"[df2$",x, "], ", "nls$",x,"[df2$tr_",x,"])"),
               paste0("exact_match(df2$",x, ", ", "df2$tr_",x,")"))
      }

      func_1b <- function(x) unlist(lapply(x, func_1))

      func_2 <- function(x){paste(x, collapse = " | ")}
      func_3 <- function(x){paste("(",x,")", sep="")}

      sub_crx_func <- lapply(sub_criteria[attr], func_1b)
      sub_crx_func <- lapply(sub_crx_func, func_2)
      sub_crx_func <- lapply(sub_crx_func, func_3)
      sub_crx_func <- paste(sub_crx_func, collapse = " & ")

      sub_crx_func <- paste("function(df2){",sub_crx_func,"}",sep="")
      sub_crx_func <- eval(parse(text = sub_crx_func))
      curr_sub_cri_lst <- unlist(sub_criteria[attr], use.names = FALSE)
    }else{
      sub_crx_func <- function(df2){TRUE}
      curr_sub_cri_lst <- "sn"
    }

    df$force_check <- df$skip <- df$m_tag <- c <- min_m_tag <- 0
    min_pid <- sn_ref

    while (min_pid==sn_ref | min_m_tag==-1) {
      if(c+1 >1 & display ) cat(paste("\nMatching criteria ",i,": iteration ",c+1, sep=""))

      # Reference records
      TR <- df[!df$cri %in% c("",NA),]
      # TR <- TR[order(TR$cri, TR$skip, -TR$force_check, -TR$tag, TR$m_tag, TR$pid_cri, TR$sn),]
      TR <- dplyr::arrange(TR, TR$cri, TR$skip, -TR$force_check, -TR$tag, TR$m_tag, TR$pid_cri, TR$sn)
      TR <- TR[!duplicated(TR$cri),]
      TR <- TR[unique(c("pid","link_id","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst))]
      names(TR) <- paste0("tr_", names(TR))

      # df <- merge(df, TR, by.x="cri", by.y="tr_cri", all.x=T)
      df <- dplyr::left_join(df, TR, by= c("cri"="tr_cri"))

      # Matches in subcriteria
      df$sub_cri_match <- ifelse(!sub_crx_func(df) %in% c(NA, FALSE),1,0)

      df$m_tag <- ifelse(df$m_tag==1 &
                           df$sub_cri_match==1 & df$pid_cri <= df$tr_pid_cri,
                         -1, df$m_tag)
      df$m_tag <- ifelse(df$sub_cri_match==1 & df$m_tag==-1 & df$pid==df$tr_pid & !is.na(df$tr_pid),
                         1,df$m_tag)
      df$pid <- ifelse(
        (df$m_tag==-1 & df$pid!=sn_ref) | (df$sub_cri_match==1 & df$pid==sn_ref & !is.na(df$tr_pid)),
        df$tr_pid, df$pid
      )

      #inherit pid
      df$pid <- ifelse(
        df$pid==sn_ref & !df$tr_pid %in% c(sn_ref,NA) & df$sub_cri_match==1,
        df$tr_pid, df$pid
      )

      #assign new pid
      df$pid <- ifelse(
        df$pid==sn_ref & df$tr_pid == sn_ref & !is.na(df$tr_pid) & df$sub_cri_match==1,
        df$tr_sn, df$pid
      )


      df$link_id <- ifelse(
        (df$link_id==sn_ref & !is.na(df$tr_link_id) & df$sub_cri_match==1) |
          ((df$m_tag==-1 & df$pid!=sn_ref) | (df$sub_cri_match==1 & df$pid==sn_ref & !is.na(df$tr_pid))),
        df$tr_sn, df$link_id
      )

      df$m_tag <- ifelse(df$pid !=sn_ref & df$m_tag != -1,1, df$m_tag)
      df$m_tag <- ifelse(df$sn==df$tr_sn & !is.na(df$tr_sn) & df$m_tag ==-1, 1, df$m_tag )


      df$skip <- ifelse(df$m_tag ==-1 & !is.na(df$m_tag), 0, ifelse(df$sub_cri_match==1, 1, df$skip))
      min_pid <- min(df$pid[!df$cri %in% c("",NA)])
      min_m_tag <- min(df$m_tag[!df$cri %in% c("",NA)])
      df <- df[c("sn", "pr_sn", "pid", "link_id", "pid_cri", "strt", "cri", cri_lst, sub_cri_lst, "tag", "m_tag", "skip", "dsvr", "force_check")]
      c <- c+1
    }

    tagged_1 <- length(df$pid[!df$pid %in% c(sn_ref,NA) & df$tag ==0])
    total_1 <- length(df$pid[df$tag ==0])

    # Cases that have not been tagged for the print output
    df$tag <- ifelse(df$pid %in% c(sn_ref,NA),0,1)
    df$link_id <- ifelse(duplicated(df$pid) == FALSE & duplicated(df$pid, fromLast=TRUE) == FALSE,sn_ref,df$link_id)
    df$pid <- ifelse(duplicated(df$pid) == FALSE & duplicated(df$pid, fromLast=TRUE) == FALSE,sn_ref,df$pid)

    removed <- length(subset(df$pid, df$pid %in% c(sn_ref,NA) & df$tag ==1 ))

    df$tag <- ifelse(df$pid!=sn_ref,1,0)
    df$pid_cri <- ifelse(df$tag ==1 & df$pid_cri == Inf,i, df$pid_cri)

    if(display) {
      assigned <- tagged_1-removed
      cat(paste0("\n", fmt(total_1), " records(s): ",  fmt(assigned)," assigned to record groups and ", fmt(total_1-assigned)," left to group."))
    }
  }

  df$pid_cri <- ifelse(df$pid_cri==Inf, 0, df$pid_cri)

  # records not yet assigned a group ID are assigned new unique group IDs
  df$pid <- ifelse(df$pid==sn_ref, df$sn, df$pid)
  df$link_id <- ifelse(df$link_id==sn_ref, df$sn, df$link_id)
  df <- df[c("sn","pid","link_id", "pid_cri","dsvr","pr_sn")]

  if(is.null(ds)){
    df <- df[order(df$pr_sn), c("sn","pid", "link_id", "pid_cri","pr_sn")]
  }else{
    pds2 <- lapply(split(df$dsvr, df$pid), function(x){
      paste0(sort(unique(x)), collapse=",")
    })

    df$pid_dataset <- as.character(pds2[as.character(df$pid)])
    df <- df[order(df$pr_sn), c("sn","pid","link_id", "pid_cri","pid_dataset","pr_sn")]
  }

  if(group_stats){
    df <- df[order(df$pid),]
    df$pid_total <- rep(rle(df$pid)$lengths, rle(df$pid)$lengths)
    df <- df[order(df$pr_sn),]
  }

  df$pr_sn <- NULL

  pd <- ifelse(display,"\n","")
  cat(paste(pd,"Record grouping complete: ",fmt(removed + (total_1-tagged_1))," record(s) with a unique ID. \n" , sep =""))
  if(to_s4) df <- diyar::to_s4(df)
  df
}

overlaps_archive <- function(x, y, methods = "overlap", method = "overlap"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y)== 0) return(logical())
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  if(length(x) == 1 & length(y) != 1){
    x <- rep(x, length(y))
  }else if(length(y) == 1 & length(x) != 1){
    y <- rep(y, length(x))
  }else{
    err <- err_match_ref_len(x, "y", c(1, length(y)), "x")
    if(err != F) stop(err, call. = F)

    err <- err_match_ref_len(y, "x", c(1, length(x)), "y")
    if(err != F) stop(err, call. = F)
  }

  if(length(m) == 1 & length(x) != 1){
    m <- rep(m, length(x))
  }else{
    err <- err_match_ref_len(m, "x", c(1, length(x)), "method")
    if(err != F) stop(err, call. = F)
  }

  err <- err_object_types(m, "methods", "character")
  if(err != F) stop(err, call. = F)
  err <- err_overlap_methods_1(overlap_methods = m, "methods")
  if(err != F) stop(err, call. = F)

  # final check
  p <- rep(FALSE, length(x))
  sets <- split(1:length(x), m)

  # Mutually inclusive methods
  names(sets)[grepl("none", names(sets))] <- "none"
  names(sets)[grepl("overlap", names(sets))] <- "overlap"

  um1 <- names(sets)
  um1 <- um1[!duplicated(um1)]
  um1 <- unlist(strsplit(um1,split="\\|"))
  um1 <- um1[!duplicated(um1)]

  m_ab <- function(x){
    ifelse(x=="aligns_start", "as", ifelse(x=="aligns_end", "ae", substr(x,1,2)))
  }

  none <- function(x, y) rep(F, length(x))
  for (i in um1){
    assign("tp", sets)
    ab <- m_ab(i)
    names(tp) <- ifelse(grepl(i, names(sets)), i, "")
    tp <- tp[names(tp) != ""]
    tp <- unlist(tp, use.names = F)

    func <- get(i)
    tp <- tp[tp %in% which(p %in% c(FALSE, NA))]
    lgk <- func(x[tp], y[tp])
    tp <- tp[which(lgk)]
    p[tp] <- TRUE
  }
  rm(list = ls()[ls() != "p"])
  return(p)
}

overlaps_v2_archive <- function(x, y, methods = "overlap", method = "overlap"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = F)
  err <- err_object_types(x, "x", c("number_line", "numeric", "integer"))
  if(err != F) stop(err, call. = F)
  if(length(x) == 0 & length(y) == 0) return(logical())
  if(missing(methods) & !missing(method)) {
    m <- paste(method,sep="", collapse = "|")
    warning("'method' is deprecated. Please use 'methods' instead.")
  }else{
    m <- methods
  }

  if(length(x) == 1 & length(y) != 1){
    x <- rep(x, length(y))
  }else if(length(y) == 1 & length(x) != 1){
    y <- rep(y, length(x))
  }else{
    err <- err_match_ref_len(x, "y", c(1, length(y)), "x")
    if(err != F) stop(err, call. = F)

    err <- err_match_ref_len(y, "x", c(1, length(x)), "y")
    if(err != F) stop(err, call. = F)
  }

  if(length(m) == 1 & length(x) != 1){
    m <- rep(m, length(x))
  }else{
    err <- err_match_ref_len(m, "x", c(1, length(x)), "method")
    if(err != F) stop(err, call. = F)
  }

  err <- err_object_types(m, "methods", "character")
  if(err != F) stop(err, call. = F)
  err <- err_overlap_methods_1(overlap_methods = m, "methods")
  if(err != F) stop(err, call. = F)

  mths <- m
  rd_id <- seq(1:length(mths))
  mths <- split(rd_id, mths)

  # Mutually inclusive methods
  names(mths)[grepl("none", names(mths))] <- "none"
  names(mths)[grepl("overlap", names(mths))] <- "overlap"

  mths_nm <- strsplit(names(mths), "\\|")
  mths <- unlist(
    lapply(seq_len(length(mths)), function(i){
      x <- rep(mths[i], length(mths_nm[[i]]))
      names(x) <- mths_nm[[i]]
      x
    }), recursive = FALSE)
  mths <- lapply(split(mths, names(mths)), function(x) unlist(x, use.names = FALSE))

  lgk_2 <- rep(FALSE, length(x))
  mth_lgk <- which(rd_id %in% mths[["overlap"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- overlap(x[mth_lgk], y[mth_lgk])
  }
  rm(list = ls()[ls() != "lgk_2"])
  return(lgk_2)
  mth_lgk <- which(rd_id %in% mths[["across"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- across(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["exact"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- exact(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["inbetween"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- inbetween(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["aligns_start"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- aligns_start(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["aligns_end"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- aligns_end(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["chain"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- chain(x[mth_lgk], y[mth_lgk])
  }
  mth_lgk <- which(rd_id %in% mths[["reverse"]] & lgk_2 %in% c(FALSE, NA))
  if(length(mth_lgk) > 0){
    lgk_2[mth_lgk] <- reverse(x[mth_lgk], y[mth_lgk])
  }
  rm(list = ls()[ls() != "lgk_2"])
  lgk_2
}

custom_sort_archive <- function(..., decreasing = FALSE){
  ord <- order(..., decreasing = decreasing)
  ord <- match(seq_len(length(ord)), ord)

  ord_l <- list(...)
  ord_l <- eval(parse(text = paste0("paste0(",paste0("ord_l[[", seq_len(length(ord_l)), "]]", collapse = ",' ',"),")")))

  ord <- (ord[!duplicated(ord_l)])[match(ord_l, ord_l[!duplicated(ord_l)])]
  ord <- match(ord, sort(ord[!duplicated(ord)]))

  ord
}

combi_archive <- function(...){
  # ... must be vectors
  combi <- list(...)
  # Validations
  err_txt <- unlist(lapply(seq_len(length(combi)), function(i){
    x <- diyar::err_atomic_vectors(combi[[i]], paste0("vector ", i))
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

  combi_cd <- sapply(seq_len(length(combi)), function(i){
    x <- combi[[i]]
    if(length(x) == 1){
      x <- rep(1L, max(vec_lens))
    }else{
      x <- match(x, x[!duplicated(x)])
    }
    x
  })
  nrows <- nrow(combi_cd)
  ncols <- ncol(combi_cd)
  combi_cd <- as.integer(combi_cd)
  rows_pos <- rep(seq_len(nrows), ncols)
  combi_cd <- split(combi_cd, rows_pos)
  combi_cd <- match(combi_cd, combi_cd)
  return(combi_cd)
}

make_ids_archive <- function(x_pos, y_pos, id_length = max(x_pos, y_pos)){
  ord <- order(x_pos, y_pos)
  x <- x_pos <- as.integer(x_pos[ord])
  y <- y_pos <- as.integer(y_pos[ord])

  rp <- y < x
  x[rp] <- y_pos[rp]
  y[rp] <- x_pos[rp]

  # browser()
  y1 <- x[match(x, y)]
  # tmp <- y[match(y, x)]
  # y1[is.na(y1)] <- tmp[is.na(y1)]
  y1[is.na(y1)] <- x[is.na(y1)]
  y2 <- y1[match(y1, y)]
  lgk <- which(is.na(y2) & !(x != y1 | (x == y1 & y1 %in% y1[x != y1])))
  y2[lgk] <- y[lgk]
  lgk <- which(is.na(y2) & (x != y1 | (x == y1 & y1 %in% y1[x != y1])))
  y2[lgk] <- y1[lgk]

  sn <- seq_len(id_length)
  link_id <- y1[match(sn, y)]
  tmp <- y2[match(sn, x)]
  link_id[is.na(link_id)] <- tmp[is.na(link_id)]
  group_id <- y2[match(sn, y)]
  tmp <- y2[match(sn, x)]
  group_id[is.na(group_id)] <- tmp[is.na(group_id)]
  matched <- !is.na(link_id)
  group_id[!matched] <- link_id[!matched] <- sn[!matched]

  group_id <- sn[match(group_id, group_id)]
  rm(list = ls()[!ls() %in% c("sn", "link_id", "group_id", "matched")])
  return(list(sn = sn, link_id = link_id, group_id = group_id, linked = matched))
}

expand_number_line_v2_archive <- function(x, by = 1, point = "both"){
  if(missing(x)) stop("argument `x` is missing, with no default", call. = FALSE)
  err <- err_object_types(x, "x", "number_line")
  if(err != FALSE) stop(err, call. = FALSE)
  if(length(x) == 0) return(x)
  err <- err_match_ref_len(by, "x", c(1, length(x)), "by")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_match_ref_len(point, "x", c(1, length(x)), "point")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(by, "by", c("numeric", "integer"))
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_missing_check(by, "by")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_object_types(point, "point", "character")
  if(err != FALSE) stop(err, call. = FALSE)
  err <- err_invalid_opts(point, "point", c("both","start","end","left","right"))
  if(err != FALSE) stop(err, call. = FALSE)

  point <- tolower(point)
  if(length(point) == 1){
    point <- rep(point, length(x))
  }
  by[!is.finite(by)] <- NA_real_
  by <- mk_lazy_opt(by)

  if(any(point == "left")){
    indx <- which(point == "left")
    left_point(x[indx]) <- left_point(x[indx]) - by[indx]
  }
  if(any(point == "right")){
    indx <- which(point == "right")
    right_point(x[indx]) <- right_point(x[indx]) + by[indx]
  }
  if(any(point %in% c("both", "start"))){
    indx <- which(point %in% c("both", "start"))
    start_point(x[indx]) <- start_point(x[indx]) - by[indx]
  }
  if(any(point %in% c("both", "end"))){
    indx <- which(point %in% c("both", "end"))
    end_point(x[indx]) <- end_point(x[indx]) + by[indx]
  }

  return(x)
}

make_pairs_legacy <- function(x, strata = NULL, repeats_allowed = TRUE, permutations_allowed = FALSE){
  # Validations
  errs <- err_make_pairs_1(x = x, strata = strata,
                           repeats_allowed = repeats_allowed,
                           permutations_allowed = permutations_allowed)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)

  if(!is.null(strata)){
    if(length(strata[!duplicated(strata)]) == 1){
      strata <- NULL
    }
  }
  sn <- pos <- seq_len(length(x))
  repo <- list()
  if(!is.null(strata)){
    strata <- match(strata, strata[!duplicated(strata)])
    s_ord <- order(strata)
    strata <- strata[s_ord]
    sn <- sn[s_ord]
    rl <- rle(strata)
  }else{
    rl <- list(lengths = length(x), values = 1)
  }

  ord <- sequence(rl$lengths)
  repo$x_pos <- rep(ord, ord)
  repo$y_pos <- sequence(ord)

  pts <- rl$lengths[!duplicated(rl$lengths)]
  pts_val <- unlist(lapply(pts, function(x){
    if(x == 1){
      x
    }else{
      permute_num(x)
    }
  }), use.names = FALSE)
  pts_val <- as.integer(pts_val)
  if(!is.null(strata)){
    lgk <- which(!duplicated(strata, fromLast = TRUE))
  }else{
    lgk <- length(x)
  }

  if(!is.null(strata)){
    repo$y_max_pos <- rep(pos[lgk], pts_val[match(rl$lengths, pts)])
    repo$reps <- rep(pts_val[match(rl$lengths, pts)], pts_val[match(rl$lengths, pts)])
    repo$ref <- (repo$y_max_pos + 1L)
    repo$y_pos <- repo$ref - repo$y_pos
    repo$x_pos <- repo$ref - repo$x_pos
  }

  repo$x_pos <- sn[repo$x_pos]
  repo$y_pos <- sn[repo$y_pos]

  repo$x_val <- x[repo$x_pos]
  repo$y_val <- x[repo$y_pos]
  repo$ref <- repo$reps <- repo$y_max_pos <- NULL

  if(isFALSE(repeats_allowed)){
    lgk <- which(repo$x_pos != repo$y_pos)
    repo <- lapply(repo, function(x) x[lgk])
  }

  if(isTRUE(permutations_allowed)){
    lgk <- which(repo$x_pos != repo$y_pos)
    repo <- list(x_pos = c(repo$x_pos[lgk], repo$y_pos),
                 y_pos = c(repo$y_pos[lgk], repo$x_pos),
                 x_val = c(repo$x_val[lgk], repo$y_val),
                 y_val = c(repo$y_val[lgk], repo$x_val))
  }

  s_ord <- order(repo$y_pos, repo$x_pos)
  repo <- lapply(repo, function(x) x[s_ord])

  rm(list = ls()[ls() != "repo"])
  return(repo)
}


# @rdname finite_check
enq_vr <- function(x){
  x <- as.character(x)
  if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
  if(length(x)==0) x <- NULL
  x
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

check_links_retired <- function(cri, data_source, data_links){
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

overlaps_err_retired <- function(opts){
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

xx_f_rbind <- function(x, y){
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

prep_prob_link_args_xx <- function(attribute,
                                   blocking_attribute,
                                   cmp_func,
                                   attr_threshold,
                                   probabilistic,
                                   m_probability,
                                   score_threshold,
                                   u_probability,
                                   repeats_allowed = FALSE,
                                   permutations_allowed = FALSE,
                                   method = "make_pairs",
                                   ignore_same_source,
                                   data_source){
  if(inherits(attribute, c("list", "data.frame"))){
    attribute <- attrs(.obj = attribute)
  }else if(inherits(attribute, c("matrix"))){
    attribute <- attrs(.obj = as.data.frame(attribute))
  }else if(inherits(attribute, c("d_attribute"))){
  }else{
    attribute <- attrs(attribute)
  }

  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }

  # Attribute names
  attr_nm <- names(attribute)
  rd_n <- length(attribute[[1]])

  lgk <- unlist(lapply(attribute, function(x){
    if(is.number_line(x)){
      length(unique(x)) == 1
    }else{
      length(x[!duplicated(x)]) == 1
    }
  }), use.names = FALSE)
  if(any(lgk)){
    warning(paste0("Attributes with identicial values in every record are ignored:\n",
                   paste0("i - `", attr_nm[lgk], "` was ignored!", collapse = "\n")), call. = FALSE)
  }
  if(all(lgk)){
    stop("Linkage stopped since all attributes were ignored.", call. = FALSE)
  }
  attribute <- attribute[!lgk]

  # Threshold for agreement in each attribute
  if(is.number_line(attr_threshold)){
    attr_threshold[attr_threshold@.Data < 0] <- reverse_number_line(attr_threshold[attr_threshold@.Data < 0], "decreasing")
  }else{
    attr_threshold <- suppressWarnings(number_line(attr_threshold, Inf))
  }

  if(length(attr_threshold) == 1 & length(attribute) > 1){
    attr_threshold <- rep(attr_threshold, length(attribute))
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
  if(length(cmp_func) == 1 & length(attribute) > 1){
    cmp_func <- rep(cmp_func, length(attribute))
  }

  if(method == "make_pairs"){
    # Create record-pairs
    if(isTRUE(ignore_same_source)){
      r_pairs <- make_pairs_wf_source(seq_len(rd_n),
                                      strata = as.vector(blocking_attribute),
                                      repeats_allowed = repeats_allowed,
                                      permutations_allowed = permutations_allowed,
                                      data_source = data_source)
    }else{
      r_pairs <- make_pairs(seq_len(rd_n),
                            strata = as.vector(blocking_attribute),
                            repeats_allowed = repeats_allowed,
                            permutations_allowed = permutations_allowed)
    }


    x <- lapply(attribute, function(k) k[r_pairs$x_pos])
    y <- lapply(attribute, function(k) k[r_pairs$y_pos])
    rp_n <- length(x[[1]])
  }else{
    rp_n <- length(attribute[[1]])
    r_pairs <- list()
    x <- y <- integer()
  }


  if(isTRUE(probabilistic)){
    # u-probabilities
    if(is.null(u_probability)){
      u_probability <- lapply(attribute, function(x){
        x_cd <- match(x, x[!duplicated(x)])
        x_cd[is.na(x)] <- NA_real_
        r <- rle(x_cd[order(x_cd)])
        n <- r$lengths[match(x_cd, r$values)]
        p <- n/length(x_cd)
        p[is.na(x_cd)] <- 0
        p
      })

      if(method == "make_pairs"){
        u_probability <- lapply(seq_len(length(attribute)), function(i){
          u_probability[[i]][match(x[[i]], attribute[[i]])]
        })
      }
    }

    # m-probabilities
    if(!inherits(m_probability, c("list"))){
      m_probability <- list(m_probability)
    }
    if(length(m_probability) == 1 & length(attribute) > 1){
      m_probability <- rep(m_probability, length(attribute))
    }

    u_probability <- lapply(u_probability, function(x){
      if(length(x) != rp_n){
        rep(x, rp_n)
      }else{
        x
      }
    })
    m_probability <- lapply(m_probability, function(x){
      if(length(x) != rp_n){
        rep(x, rp_n)
      }else{
        x
      }
    })

  }else{
    m_probability <- u_probability <- rep(list(rep(0L, rp_n)), length(attribute))
  }

  return(list(attribute = attribute,
              x = x,
              y = y,
              blocking_attribute = blocking_attribute,
              cmp_func = cmp_func,
              attr_threshold = attr_threshold,
              probabilistic = probabilistic,
              m_probability = m_probability,
              score_threshold = score_threshold,
              u_probability = u_probability,
              r_pairs = r_pairs))
}

links_retired_1 <- function(criteria,
                  sub_criteria = NULL,
                  sn = NULL,
                  strata = NULL,
                  data_source = NULL,
                  data_links = "ANY",
                  display = "none",
                  group_stats = FALSE,
                  expand = TRUE,
                  shrink = FALSE,
                  recursive = FALSE,
                  check_duplicates = FALSE,
                  tie_sort = NULL,
                  batched = "yes",
                  repeats_allowed = FALSE,
                  permutations_allowed = FALSE,
                  ignore_same_source = FALSE){

  web <- list(
    criteria = criteria,
    sub_criteria = sub_criteria,
    sn = sn,
    strata = strata,
    data_source = data_source,
    data_links = data_links,
    display = display,
    group_stats = group_stats,
    expand = expand,
    shrink = shrink,
    recursive = recursive,
    check_duplicates = check_duplicates,
    tie_sort = tie_sort,
    batched = batched,
    repeats_allowed = repeats_allowed,
    permutations_allowed = permutations_allowed,
    ignore_same_source = ignore_same_source,
    tm_a = Sys.time(),
    export = list()
  )

  criteria <- sub_criteria <- sn <-
    strata <- data_source <- data_links <-
    display <- group_stats <- expand <-
    shrink <- recursive <- check_duplicates <-
    tie_sort <- batched <- repeats_allowed <-
    permutations_allowed <- ignore_same_source <- NULL

  if(inherits(web$sub_criteria, "sub_criteria")){
    web$sub_criteria <- list(web$sub_criteria)
  }
  # Validations
  web$err <- err_links_checks_0(web$criteria, web$sub_criteria,
                                web$sn, web$strata, web$data_source, web$data_links,
                                web$display, web$group_stats, web$expand, web$shrink,
                                web$recursive, web$check_duplicates, web$tie_sort,
                                web$repeats_allowed, web$permutations_allowed, web$ignore_same_source,
                                web$batched)

  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  if(!inherits(web$criteria, "list")){
    web$criteria <- list(web$criteria)
  }
  if(isTRUE(web$shrink)){
    web$expand <- !web$shrink
  }

  # display
  web$display <- tolower(web$display)

  # Maximum no. of records from all criteria
  web$ds_len <- as.numeric(lapply(web$criteria, length))
  if(!is.null(web$sub_criteria)){
    web$ds_len <- c(
      unlist(rc_dv(lapply(web$sub_criteria, function(x){
        attr_eval(x, func = identity, simplify = FALSE)
      }), func = length), use.names = FALSE), web$ds_len)
  }
  web$ds_len <- max(web$ds_len)
  web$err <- err_sn_1(sn = web$sn, ref_num = web$ds_len, ref_nm = "criteria")
  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  if(!web$display %in% c("none")){
    web$rp_data <- di_report(
      duration = Sys.time() - web$tm_a,
      cumm_time = Sys.time() - web$tm_a,
      "Data validation",
      current_tot = web$ds_len,
      memory_used =  utils::object.size(web))
    web$report <- list(web$rp_data)
    if(web$display %in% c("stats_with_report", "stats")){
      cat(paste0(web$rp_data[[1]], ": ",
                 fmt(web$rp_data[[2]], "difftime"), "\n"))
    }
  }
  web$tm_ia <- Sys.time()

  if(!is.null(web$data_source)) {
    class(web$data_source) <- "d_lazy_opts"
  }

  # Standardise inputs
  # strata
  if(!is.null(web$strata)) {
    class(web$strata) <- "d_lazy_opts"
  }
  # sn
  web$pr_sn <- seq_len(web$ds_len)
  if(inherits(web$sn, "NULL")){
    web$sn <- web$pr_sn
  }else{
    web$sn <- as.integer(web$sn)
  }
  # User-defined order of case-assignment
  if(!is.null(web$tie_sort)) {
    if(!inherits(web$tie_sort, c("numeric", "integer", "double"))){
      web$tie_sort <- as.integer(as.factor(web$tie_sort))
    }
    if(length(web$tie_sort) == 1){
      web$tie_sort <- rep(web$tie_sort, web$ds_len)
    }
  }else{
    web$tie_sort <- rep(0L, web$ds_len)
  }

  class(web$tie_sort) <- "d_lazy_opts"
  # data_links
  web$dl_lst <- unlist(web$data_links, use.names = FALSE)
  if(!inherits(web$data_links, "list")){
    web$data_links <- list(l = web$data_links)
  }
  if(is.null(names(web$data_links))) names(web$data_links) <- rep("l", length(web$data_links))
  names(web$data_links) <- ifelse(names(web$data_links) == "", "l", names(web$data_links))

  # batched
  if(length(web$batched) == 1 & length(web$criteria) > 1){
    web$batched <- rep(web$batched, length(web$criteria))
  }

  # Place holders for group-level options
  web$tag <- rep(0L, web$ds_len)
  web$iteration <- rep(0L, web$ds_len)
  web$m_tag <- rep(0L, web$ds_len)
  web$mxp_cri <- length(web$criteria) + 1L
  web$pid_cri <- rep(web$mxp_cri, web$ds_len)
  web$sn_ref <- min(web$sn) - 1L
  web$pid <- rep(web$sn_ref, web$ds_len)
  web$link_id <- rep(web$sn_ref, web$ds_len)
  web$n_seq <- seq_len(web$ds_len)

  web$pids_repo <- list("pid" = web$pid,
                        "tag" = web$tag,
                        "pid_cri" = web$pid_cri,
                        "link_id" = web$link_id,
                        "sn" = web$sn,
                        "pr_sn" = web$pr_sn,
                        "iteration" = web$iteration,
                        "tie_sort" = web$tie_sort,
                        "data_source" = web$data_source)

  if(!is.null(web$data_source)){
    web$pids_repo$data_source <- web$data_source
  }

  if(!web$display %in% c("none")){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_ia,
      "Data standardisation",
      current_tot = web$ds_len,
      memory_used =  utils::object.size(web))
    web$report <- c(web$report, list(web$rp_data))
    if(web$display %in% c("stats_with_report", "stats")){
      cat(paste0(web$rp_data[[1]], ": ", fmt(web$rp_data[[2]], "difftime"), "\n"))
    }
  }
  web$tm_ia <- Sys.time()

  if(web$display != "none") cat("\n")
  i <- ite <- 1L
  while(i %in% seq_len(length(web$criteria)) & (min(web$pids_repo$tag) == 0 | web$shrink)){
    if(web$display %in% c("progress", "stats", "progress_with_report", "stats_with_report")){
      cat(paste0("`Criteria ", i,"`.\n"))
    }
    if(isFALSE(tolower(web$batched[i]) == "yes")){
      web$check_duplicates <- TRUE
      web$recursive <- FALSE
    }

    # Restart iteration
    web$pids_repo$iteration[which(web$pids_repo$tag == 0 & web$pids_repo$iteration != 0)] <- 0L
    # Current stage
    web$cri_l <- web$cri <- web$criteria[[i]]
    # Standardise `criteria` input
    if (length(web$cri) == 1) web$cri <- rep(web$cri, web$ds_len)
    # Identify records to be skipped
    web$n_lgk <- is.na(web$cri)
    if(!is.null(web$strata)) {
      web$n_lgk[!web$n_lgk] <- is.na(web$strata[!web$n_lgk])
      web$cri[!web$n_lgk] <- combi(web$strata[!web$n_lgk], web$cri[!web$n_lgk])
    }
    # Nested linkage
    if(web$shrink == TRUE){
      web$tmp_pid <- web$pids_repo$pid
      if(ite > 1){
        web$tmp_pid[web$pids_repo$pid == 0] <- web$pids_repo$sn[web$pids_repo$pid == 0]
      }
      web$cri <- combi(web$cri, web$tmp_pid)
      web$tmp_pid <- NULL
    }
    # Encode current `criteria`
    if(!inherits(web$cri, c("numeric","integer"))){
      web$cri <- match(web$cri, web$cri[!duplicated(web$cri)])
    }

    web$unq_lgk <- !duplicated(web$cri, fromLast = TRUE) & !duplicated(web$cri, fromLast = FALSE)
    web$skp_lgk <- which(!web$n_lgk & !web$unq_lgk)
    web$unq_lgk <- web$n_lgk <- NULL

    if(length(web$skp_lgk) == 0 | length(web$cri) == 1) {
      if(web$display %in% c("progress", "stats")){
        cat(paste0("Skipped `criteria ", i,"`.\n\n"))
      }
      i <- i + 1L
      ite <- ite + 1L
      next
    }

    if(web$shrink == TRUE){
      # Back up identifiers
      web$pids_repo$pid[web$skp_lgk] -> web$bkp_pid
      web$pids_repo$link_id[web$skp_lgk] -> web$bkp_link_id
      web$pids_repo$tag[web$skp_lgk] -> web$bkp_tag
      web$pids_repo$pid_cri[web$skp_lgk] -> web$bkp_pid_cri
      web$pids_repo$iteration[web$skp_lgk] -> web$bkp_iteration

      # Reset identifiers
      web$pids_repo$pid[web$skp_lgk] <- web$sn_ref
      web$pids_repo$link_id[web$skp_lgk] <- web$sn_ref
      web$pids_repo$tag[web$skp_lgk] <- 0L
      web$pids_repo$iteration[web$skp_lgk] <- 0L
    }

    web$curr_sub_cri <- web$sub_criteria[which(names(web$sub_criteria) == paste0("cr", i))]

    web$cri <- web$cri[web$skp_lgk]
    web$cri_l <- web$cri_l[web$skp_lgk]
    web$pid_cri <- web$pids_repo$pid_cri[web$skp_lgk]
    web$tag <- web$pids_repo$tag[web$skp_lgk]
    web$sn <- web$pids_repo$sn[web$skp_lgk]
    web$pr_sn <- web$pids_repo$pr_sn[web$skp_lgk]
    web$link_id <- web$pids_repo$link_id[web$skp_lgk]
    web$pid <- web$pids_repo$pid[web$skp_lgk]
    web$iteration <- web$pids_repo$iteration[web$skp_lgk]
    web$tie_sort <- web$pids_repo$tie_sort[web$skp_lgk]
    if(!is.null(web$data_source)){
      web$data_source <- web$pids_repo$data_source[web$skp_lgk]
    }

    # Stages without a `sub_criteria` are compared as `exact` matches
    if(length(web$curr_sub_cri) == 0){
      web$cs_len <- length(web$cri)
      web$pp <- inherit(web$tag, web$cri, web$pid_cri,
                        web$tie_sort, web$sn, web$pr_sn,
                        web$expand, web$pid, web$link_id,
                        sn_ref = web$sn_ref)

      if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
        web$lgk <- web$data_source[web$pp$sn] != web$data_source[web$pp$pid]
        web$pp <- lapply(web$pp, function(x) x[web$lgk])
      }
      web$lgk <- !web$pp$pid %in% c(web$sn_ref, NA)
      web$pp$tag[web$lgk] <- 1L

      web$pp$pid_cri[(web$pp$pid_cri == web$mxp_cri | (web$pp$pid_cri != web$mxp_cri & web$shrink))] <- i
      web$pids_repo$pid[web$pp$pr_sn] <- web$pp$pid
      web$pids_repo$tag[web$pp$pr_sn] <- web$pp$tag
      web$pids_repo$pid_cri[web$pp$pr_sn] <- web$pp$pid_cri
      web$pids_repo$link_id[web$pp$pr_sn] <- web$pp$link_id
      web$pids_repo$iteration[web$pids_repo$pid != web$sn_ref & web$pids_repo$iteration == 0] <- ite
      ite <- ite + 1L
      web$pp <- NULL
    }else{
      # Stages with a `sub_criteria` are evaluated here
      # Only records with non-missing values are checked
      web$curr_sub_cri[[1]] <- reframe(web$curr_sub_cri[[1]],
                                       func = function(x){
                                         if(length(web$skp_lgk) <= 1){
                                           x
                                         }else{
                                           x[web$skp_lgk]
                                         }
                                       })

      # Flags
      web$cs_len <- length(web$cri)
      web$m_tag <- rep(0L, web$cs_len)
      web$min_pid <- web$sn_ref
      web$min_m_tag <- 0L

      if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("Checking `sub_criteria`\n")
      }

      while (web$min_pid == web$sn_ref | any(web$m_tag == -1)) {
        web$sort_ord <- order(web$cri, web$m_tag, web$pid_cri, web$tie_sort, web$sn, decreasing = TRUE)
        web$tag <- web$tag[web$sort_ord]
        web$cri <- web$cri[web$sort_ord]
        web$cri_l <- web$cri_l[web$sort_ord]
        web$pid <- web$pid[web$sort_ord]
        web$tag <- web$tag[web$sort_ord]
        web$m_tag <- web$m_tag[web$sort_ord]
        web$pid_cri <- web$pid_cri[web$sort_ord]
        web$sn <- web$sn[web$sort_ord]
        web$pr_sn <- web$pr_sn[web$sort_ord]
        web$link_id <- web$link_id[web$sort_ord]
        web$iteration <- web$iteration[web$sort_ord]
        web$tie_sort <- web$tie_sort[web$sort_ord]
        if(!is.null(web$data_source)){
          web$data_source <- web$data_source[web$sort_ord]
        }
        web$h_ri <- seq_len(length(web$cri))

        web$indx <- order(order(web$pr_sn))
        if(isTRUE(tolower(web$batched[i]) == "yes")){
          # Reference records
          web$lgk <- which(!duplicated(web$cri, fromLast = TRUE))
          web$rep_lgk <- match(web$cri, web$cri[web$lgk])
          web$tr_link_id <- (web$link_id[web$lgk])[web$rep_lgk]
          web$tr_pid_cri <- (web$pid_cri[web$lgk])[web$rep_lgk]
          web$tr_pid <- (web$pid[web$lgk])[web$rep_lgk]
          web$tr_sn <- (web$sn[web$lgk])[web$rep_lgk]
          web$ref_rd <- web$tr_sn == web$sn
          web$pos_repo <- make_pairs_batch_legacy(strata = web$cri,
                                                  index_record = web$ref_rd,
                                                  sn = web$indx,
                                                  ignore_same_source = web$ignore_same_source,
                                                  data_source = web$data_source)
          # temp external fix
          web$s_ord <- match(web$indx, web$pos_repo$sn)
          web$pos_repo <- lapply(web$pos_repo[1:3], function(x){
            x[web$s_ord]
          })
          names(web$pos_repo)[2] <- "x_val"
          names(web$pos_repo)[3] <- "y_val"

        }else{
          if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
            web$pos_repo <- make_pairs_wf_source(seq_len(web$ds_len),
                                                 strata = web$cri,
                                                 repeats_allowed = web$repeats_allowed,
                                                 permutations_allowed = web$permutations_allowed,
                                                 data_source = web$data_source)
          }else{
            web$pos_repo <- make_pairs(strata = web$cri,
                                       x = web$indx,
                                       repeats_allowed = web$repeats_allowed,
                                       permutations_allowed = web$permutations_allowed)
          }
        }

        # Check the `sub_criteria`
        web$sub_cri_match <- eval_sub_criteria(x = web$curr_sub_cri[[1]],
                                               x_pos = web$pos_repo$x_val,
                                               y_pos = web$pos_repo$y_val,
                                               check_duplicates = web$check_duplicates)

        web$export.nm <- names(web$sub_cri_match)
        web$export.nm <- web$export.nm[!grepl("^logical|^equal", web$export.nm)]
        if(length(web$export.nm) > 0){
          web$export[[paste0("cri.", i)]][[paste0("iteration.", ite)]] <- web$sub_cri_match[web$export.nm]
          web$sub_cri_match[web$export.nm] <- NULL
        }

        if(isTRUE(tolower(web$batched[i]) == "yes")){
          if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
            web$same_source_indx <- match(web$sn, web$pos_repo$x_val)
            web$sub_cri_match <- lapply(web$sub_cri_match, function(x){
              x <- x[web$same_source_indx]
              x[is.na(x)] <- 0L
              x
            })
            web$same_source_indx <- is.na(web$same_source_indx)
          }else{
            web$same_source_indx <- FALSE
          }

          web$sub_cri_match <- lapply(web$sub_cri_match, function(x){
            x[web$ref_rd] <- 1
            x
          })
          if(isFALSE(web$check_duplicates)){
            web$equals_ref_rd <- web$sub_cri_match[[2]] | web$ref_rd
          }
          web$sub_cri_match <- web$sub_cri_match[[1]] | web$ref_rd

        }else{
          web$lgk <- as.logical(web$sub_cri_match$logical_test)
          web$tmp_ids <- make_ids(
            x_pos = web$pos_repo$x_val[web$lgk],
            y_pos = web$pos_repo$y_val[web$lgk],
            id_length = max(web$indx)
          )

          web$tmp_ids <- lapply(web$tmp_ids, function(x){
            x[web$indx]
          })
          web$sub_cri_match <- as.logical(web$tmp_ids$linked)
          web$tmp_ids <- lapply(web$tmp_ids[c("sn", "link_id", "group_id")], function(x){
            web$pr_sn[match(x, web$indx)]
          })

          web$rep_lgk <- match(web$tmp_ids$group_id, web$pr_sn)
          web$tr_link_id <- web$link_id[web$rep_lgk]
          web$tr_pid_cri <- web$pid_cri[web$rep_lgk]
          web$tr_pid <- web$pid[web$rep_lgk]
          web$tr_sn <- web$sn[web$rep_lgk]
          web$ref_rd <- web$tr_sn == web$sn
        }
        web$rec_pairs_mem <- utils::object.size(web$pos_repo)
        web$pos_repo <- NULL

        # snapshot of pid before linking records in current criteria
        web$f_pid <- web$pid
        # Records inherit pid if they match with previously tagged records
        # If recursive, records with existing pids are overwritten if they match another tagged at the same stage (Situation A)
        web$rep_lgk <- which((web$sub_cri_match > 0 | (web$sub_cri_match == 0 & !tolower(web$batched[i]) == "yes")) &
                               (web$pid == web$sn_ref | (web$pid != web$sn_ref & web$tr_pid_cri == web$pid_cri & web$recursive)) &
                               !web$tr_pid %in% c(web$sn_ref, NA) &
                               ((web$tr_pid_cri == web$pid_cri & !web$expand) | (web$expand)))

        web$pid[web$rep_lgk] <- web$tr_pid[web$rep_lgk]
        web$lgk <- which(web$h_ri %in% web$rep_lgk & web$link_id == web$sn_ref)
        web$link_id[web$lgk] <- web$tr_sn[web$lgk]
        web$lgk <- NULL

        # Records are assigned new pids if they do not match previously tagged records
        web$rep_lgk_2 <- which((((web$pid == web$sn_ref | (web$pid != web$sn_ref & web$tr_pid_cri == web$pid_cri & web$recursive)) &
                                   web$tr_pid == web$sn_ref &
                                   !is.na(web$tr_pid))) &
                                 (web$sub_cri_match > 0 | (web$sub_cri_match == 0 & !tolower(web$batched[i]) == "yes")))
        web$pid[web$rep_lgk_2] <- web$tr_sn[web$rep_lgk_2]
        web$link_id[web$rep_lgk_2] <- web$tr_sn[web$rep_lgk_2]

        # If not recursive, all matches are closed (m_tag == 2)
        # Otherwise, new members of a group (m_tag == -1) are checked against other records
        web$rep_lgk_2 <-  which(web$h_ri %in% which(web$m_tag == 0) & web$h_ri %in% c(web$rep_lgk, web$rep_lgk_2))
        web$m_tag[which(web$h_ri %in% web$rep_lgk_2 & web$recursive)] <- -1L
        web$m_tag[which(web$h_ri %in% web$rep_lgk_2 & !web$recursive)] <- 2L
        web$m_tag[web$ref_rd] <- 2L

        # Duplicate record-sets can be closed
        if(isFALSE(web$check_duplicates)){
          web$m_tag[web$equals_ref_rd > 0] <- 2L
          web$lgk <- which(web$pid == web$sn_ref & web$equals_ref_rd > 0)
          web$pid[web$lgk] <- web$sn[web$lgk]
        }

        # Close record-pairs from the same source
        # web$lgk <- web$same_source_indx & !web$recursive
        web$lgk <- web$same_source_indx
        web$m_tag[web$lgk] <- 2L
        web$pid[web$lgk] <- web$sn[web$lgk]

        # If recursive, records with pids from "Situation A" but not part of the current matches are updated with the new pid
        if(isTRUE(web$recursive)){
          web$rec_lgk <- which(web$pid != web$sn_ref & web$f_pid != web$sn_ref & web$pid != web$f_pid & web$tr_pid_cri == web$pid_cri)
          web$rec_o <- web$f_pid[web$rec_lgk]
          web$rec_u <- web$pid[web$rec_lgk]
          web$li_u <- web$link_id[web$rec_lgk]
          web$lgk <- match(web$f_pid, web$rec_o)
          web$r_pid <- web$rec_u[web$lgk]
          web$r_lid <- web$li_u[web$lgk]
          web$r_lgk <- which(web$r_pid != web$pid & !is.na(web$r_pid) & !is.na(web$pid))
          web$pid[web$r_lgk] <- web$r_pid[web$r_lgk]
        }

        # Track when to end checks for the current criteria
        web$lgk <- !is.na(web$cri)
        if(length(web$lgk[web$lgk]) != 0){
          web$min_pid <- min(web$pid[web$lgk])
          web$min_m_tag <- min(web$m_tag[web$lgk])
        } else{
          web$min_pid <- min(web$pid)
          web$min_m_tag <- min(web$m_tag)
        }

        web$iteration[which(web$m_tag == 2 & web$iteration == 0)] <- ite
        web$pid_cri[which(web$pid_cri == web$mxp_cri | (web$pid_cri != web$mxp_cri & web$shrink))] <- i

        # If not recursive, exclude linked records.
        if(isFALSE(web$recursive)){
          web$inc_lgk <- which(web$m_tag == 2)
          web$exc_lgk <- which(web$m_tag != 2)

          web$pids_repo$cri[web$pr_sn[web$inc_lgk]] <- web$cri[web$inc_lgk]
          web$pids_repo$pid[web$pr_sn[web$inc_lgk]] <- web$pid[web$inc_lgk]
          web$pids_repo$tag[web$pr_sn[web$inc_lgk]] <- web$tag[web$inc_lgk]
          web$pids_repo$pid_cri[web$pr_sn[web$inc_lgk]] <- web$pid_cri[web$inc_lgk]
          web$pids_repo$sn[web$pr_sn[web$inc_lgk]] <- web$sn[web$inc_lgk]
          web$pids_repo$link_id[web$pr_sn[web$inc_lgk]] <- web$link_id[web$inc_lgk]
          web$pids_repo$iteration[web$pr_sn[web$inc_lgk]] <- web$iteration[web$inc_lgk]
          web$pids_repo$tie_sort[web$pr_sn[web$inc_lgk]] <- web$tie_sort[web$inc_lgk]

          if(length(web$cri[web$exc_lgk]) == 0){
            if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
              progress_bar(web$cs_len, web$cs_len, 100,
                           msg = paste0("Iteration ",
                                        fmt(ite), " (",
                                        fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                                        ")"))
            }
            ite <- ite + 1L
            break
          }

          web$cri <- web$cri[web$exc_lgk]
          web$cri_l <- web$cri_l[web$exc_lgk]
          web$pid <- web$pid[web$exc_lgk]
          web$tag <- web$tag[web$exc_lgk]
          web$pid_cri <- web$pid_cri[web$exc_lgk]
          web$sn <- web$sn[web$exc_lgk]
          web$link_id <- web$link_id[web$exc_lgk]
          web$iteration <- web$iteration[web$exc_lgk]
          web$tie_sort <- web$tie_sort[web$exc_lgk]

          web$curr_sub_cri[[1]] <- reframe(web$curr_sub_cri[[1]], func = function(x) x[sort(order(order(web$pr_sn))[web$exc_lgk])])
          web$pr_sn <- web$pr_sn[web$exc_lgk]
          web$m_tag <- web$m_tag[web$exc_lgk]
        }else{
          web$pids_repo$pid[web$pr_sn] <- web$pid
          web$pids_repo$tag[web$pr_sn] <- web$tag
          web$pids_repo$pid_cri[web$pr_sn] <- web$pid_cri
          web$pids_repo$link_id[web$pr_sn] <- web$link_id
          web$pids_repo$iteration[web$pr_sn] <- web$iteration
        }
        if(!web$display %in% c("none")){
          web$rp_data <- di_report(
            cumm_time = Sys.time() - web$tm_a,
            duration = Sys.time() - web$tm_ia,
            ite, length(web$m_tag),
            criteria = i,
            current_tagged = length(which(web$m_tag == 2 & web$iteration == ite)),
            memory_used =  utils::object.size(web) + web$rec_pairs_mem)
          web$report <- c(web$report, list(web$rp_data))
        }
        if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
          progress_bar(length(web$pids_repo$pid[web$skp_lgk][web$pids_repo$pid[web$skp_lgk] != web$sn_ref]),
                       web$cs_len, 100,
                       msg = paste0("Iteration ",
                                    fmt(ite), " (",
                                    fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                                    ")"))
        }
        web$tm_ia <- Sys.time()
        ite <- ite + 1L
      }
      if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("\n")
      }
    }

    if(web$shrink){
      web$ds_rid <- web$n_seq
      web$reset_lgk <- which(web$pids_repo$pid %in% web$pids_repo$pid[!web$ds_rid %in% web$skp_lgk] & web$pids_repo$pid %in% web$pids_repo$pid[web$ds_rid %in% web$skp_lgk] & !web$ds_rid %in% web$skp_lgk)
      if(length(web$reset_lgk) > 0){
        web$pids_repo$pid[web$reset_lgk] <- web$sn_ref
        web$pids_repo$link_id[web$reset_lgk] <- web$sn_ref
        web$pids_repo$tag[web$reset_lgk] <- 0L
        web$pids_repo$pid_cri[web$reset_lgk] <- web$mxp_cri
        web$pids_repo$iteration[web$reset_lgk] <- 0L
      }

      web$restore_lgk <- (!duplicated(web$pids_repo$pid[web$skp_lgk]) & !duplicated(web$pids_repo$pid[web$skp_lgk], fromLast = TRUE))
      web$restore_lgk <- which(!web$cri %in% web$cri[!web$restore_lgk])
      if(length(web$restore_lgk) > 0){
        web$pids_repo$pid[web$skp_lgk[web$restore_lgk]] <- web$bkp_pid[web$restore_lgk]
        web$pids_repo$link_id[web$skp_lgk[web$restore_lgk]] <- web$bkp_link_id[web$restore_lgk]
        web$pids_repo$tag[web$skp_lgk[web$restore_lgk]] <- web$bkp_tag[web$restore_lgk]
        web$pids_repo$pid_cri[web$skp_lgk[web$restore_lgk]] <- web$bkp_pid_cri[web$restore_lgk]
        web$pids_repo$iteration[web$skp_lgk[web$restore_lgk]] <- web$bkp_iteration[web$restore_lgk]
      }
      web$bkp_pid <- web$bkp_link_id <- web$bkp_tag <- web$bkp_pid_cri <- web$bkp_iteration <- NULL
    }else{
      web$restore_lgk <- integer()
    }

    # Unlink pids with a single record for another attempt in the next stage
    web$tag_h <- rep(0, length(web$pids_repo$tag))
    web$pids_repo$tag <- web$tag_h
    web$pids_repo$tag[which(!web$pids_repo$pid %in% c(web$sn_ref, NA))] <- 1L
    web$lgk <- (!duplicated(web$pids_repo$pid) & !duplicated(web$pids_repo$pid, fromLast = TRUE))
    web$pids_repo$link_id[web$lgk] <- web$sn_ref
    web$pids_repo$pid[web$lgk] <- web$sn_ref
    web$pids_repo$pid_cri[web$lgk] <- web$mxp_cri

    # Flag records linked at current stage
    web$pids_repo$tag <- web$tag_h
    web$pids_repo$tag[which(web$pids_repo$pid != web$sn_ref)] <- 1L
    if(!web$display %in% c("none")){
      web$current_tot <- length(web$skp_lgk)
      web$assigned <- length(web$pids_repo$pid[web$skp_lgk][web$pids_repo$pid[web$skp_lgk] != web$sn_ref])
      web$rp_data <- di_report(
        cumm_time = Sys.time() - web$tm_a,
        duration = Sys.time() - web$tm_ia,
        ite - 1,
        length(web$skp_lgk), criteria = i,
        current_tagged = web$assigned,
        memory_used =  utils::object.size(web))
      web$report <- c(web$report, list(web$rp_data))

      if(web$display %in% c("stats", "progress", "stats_with_report", "progress_with_report")){
        cat("\n")
        cat(paste0("Total: ", fmt(web$ds_len), " record(s).\n",
                   "Checked: ", fmt(web$current_tot), " record(s).\n",
                   "Linked: ", fmt(web$assigned)," record(s).\n\n"))
      }
    }
    web$tm_ia <- Sys.time()
    i <- i + 1L
  }

  # Skipped and unmatched records
  web$pids_repo$iteration[web$pids_repo$iteration == 0] <- ite - 1L
  if(!inherits(web$strata, "NULL")){
    web$pids_repo$pid_cri[which(web$pids_repo$pid == web$sn_ref & is.na(web$strata) & web$pids_repo$pid_cri == web$mxp_cri)] <- -1L
  }

  web$pids_repo$pid -> web$pid
  web$pids_repo$pid_cri -> web$pid_cri
  web$pids_repo$link_id ->  web$link_id
  web$pids_repo$sn -> web$sn
  web$pids_repo$pr_sn -> web$pr_sn
  web$pids_repo$iteration -> web$iteration

  web$pid_cri[web$pid == web$sn_ref & web$pid_cri == web$mxp_cri] <- 0L
  web$link_id[web$pid == web$sn_ref] <- web$sn[web$pid == web$sn_ref]
  web$pid[web$pid == web$sn_ref] <- web$sn[web$pid == web$sn_ref]

  web$pids <- methods::new("pid",
                           .Data = web$pid,
                           sn = web$sn,
                           pid_cri = web$pid_cri,
                           link_id = list(link_id1 = web$link_id),
                           iteration = web$iteration)

  web$r <- rle(sort(web$pid))
  web$pids@pid_total <- web$r$lengths[match(web$pid, web$r$values)]

  if(!is.null(web$data_source)){
    # Data links
    web$rst <- check_links(web$pids@.Data, web$pids_repo$data_source, web$data_links)

    if(!all(toupper(web$dl_lst) == "ANY")){
      web$req_links <- web$rst$rq
      web$pids@pid_total[!web$req_links] <- 1L
      web$pids@pid_cri[!web$req_links] <- -1L
      web$pids@.Data[!web$req_links] <- web$pids@web$sn[!web$req_links]
      web$pids@link_id[!web$req_links] <- web$pids@web$sn[!web$req_links]
      web$rst$ds[!web$req_links] <- web$data_source[!web$req_links]
    }
    web$pids@pid_dataset <- encode(web$rst$ds)
  }

  web$tm_z <- Sys.time()
  web$tms <- difftime(web$tm_z, web$tm_a)
  web$tms <- paste0(ifelse(round(web$tms) == 0, "< 0.01", round(as.numeric(web$tms), 2)), " ", attr(web$tms, "units"))

  if(web$display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    web$pids <- list(pid = web$pids, report = as.list(do.call("rbind", lapply(web$report, as.data.frame))))
    class(web$pids$report) <- "d_report"
  }
  if(!web$display %in% c("none", "none_with_report")) cat("Records linked in ", web$tms, "!\n", sep = "")
  if(length(web$export) > 0){
    if(inherits(web$pids, "list")){
      web$pids <- c(web$pids, web["export"])
    }else{
      web$pids <- list(pid = web$pids, export = web$export)
    }
  }
  web <- web$pids
  return(web)
}

episodes_retired_1 <- function(date, case_length = Inf, episode_type = "fixed", recurrence_length = case_length,
                     episode_unit = "days", strata = NULL, sn = NULL, episodes_max = Inf, rolls_max = Inf,
                     case_overlap_methods = 8, recurrence_overlap_methods = case_overlap_methods,
                     skip_if_b4_lengths = FALSE, data_source = NULL,
                     data_links = "ANY", custom_sort = NULL, skip_order = Inf, reference_event = "last_record",
                     case_for_recurrence = FALSE, from_last = FALSE, group_stats = FALSE,
                     display = "none", case_sub_criteria = NULL, recurrence_sub_criteria = case_sub_criteria,
                     case_length_total = 1, recurrence_length_total = case_length_total,
                     skip_unique_strata = TRUE) {
  tm_a <- Sys.time()
  # Validations
  errs <- err_episodes_checks_0(sn = sn, date = date, case_length = case_length, strata = strata,
                                display = display, episodes_max = episodes_max, from_last = from_last,
                                episode_unit = episode_unit, case_overlap_methods = case_overlap_methods,
                                recurrence_overlap_methods = recurrence_overlap_methods,
                                skip_order = skip_order, custom_sort = custom_sort, group_stats = group_stats,
                                data_source=data_source, data_links = data_links,
                                skip_if_b4_lengths = skip_if_b4_lengths,
                                rolls_max = rolls_max, case_for_recurrence = case_for_recurrence,
                                reference_event = reference_event,
                                episode_type = episode_type, recurrence_length = recurrence_length,
                                case_sub_criteria = case_sub_criteria,
                                recurrence_sub_criteria = recurrence_sub_criteria,
                                case_length_total = case_length_total,
                                recurrence_length_total = recurrence_length_total,
                                skip_unique_strata = skip_unique_strata)
  if(!isFALSE(errs)) stop(errs, call. = FALSE)
  inp_n <- length(date)
  if(!display %in% c("none")){
    rp_data <- di_report(duration = Sys.time() - tm_a,
                         cumm_time = Sys.time() - tm_a,
                         iteration = "Data validation",
                         current_tot = inp_n)
    report <- list(rp_data)
    ite_msg_repo <- character()
    if(display %in% c("stats_with_report", "stats")){
      ite_msg <- paste0(rp_data[[1]], ": ", fmt(rp_data$duration, "difftime"), "\n")
      ite_msg_repo <- c(ite_msg_repo, ite_msg)
      cat(ite_msg)
    }
  }
  tm_ia <- Sys.time()

  # `episode_unit`
  episode_unit <- tolower(episode_unit)
  episode_unit <- match(episode_unit, names(diyar::episode_unit))
  class(episode_unit) <- "d_label"
  attr(episode_unit, "value") <- as.vector(sort(episode_unit[!duplicated(episode_unit)]))
  attr(episode_unit, "label") <- names(diyar::episode_unit)[attr(episode_unit, "value")]
  attr(episode_unit, "state") <- "encoded"
  # `strata`
  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1L, inp_n)
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }

  options_lst = list(date = date,
                     strata = if(inherits(strata,"NULL")) NULL else encode(strata),
                     case_length = if(!inherits(case_length,"list")) list(case_length) else case_length,
                     recurrence_length = if(!inherits(recurrence_length,"list")) list(recurrence_length) else recurrence_length,
                     episode_unit = episode_unit,
                     from_last = from_last)

  epid_unit <- as.vector(episode_unit)
  rm(episode_unit)
  # Standardise inputs
  # `display`
  display <- tolower(display)
  # `data_links`
  dl_lst <- unlist(data_links, use.names = FALSE)
  if(!inherits(data_links, "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))

  # `episode_type`
  episode_type <- tolower(episode_type)
  episode_type <- match(episode_type, c("fixed", "rolling", "recursive"))
  any_rolling_epi <- any(episode_type %in% c(2, 3))

  # `date`
  date <- as.number_line(date)
  is_dt <- ifelse(!inherits(date@start, c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  if(isTRUE(is_dt)){
    date <- number_line(
      l = as.POSIXct(date@start),
      r = as.POSIXct(right_point(date))
    )
  }

  epid_unit[!is_dt] <- 1L
  # `case_overlap_methods`
  if(!inherits(case_overlap_methods,"list")){
    case_overlap_methods <- list(case_overlap_methods)
  }else{
    case_overlap_methods <- case_overlap_methods
  }
  # `case_length`
  ep_l <- length_to_range(lengths = case_length,
                          date = date,
                          from_last = from_last,
                          episode_unit = epid_unit)$range

  # `case_length_total`
  if(is.number_line(case_length_total)){
    case_length_total[case_length_total@.Data < 0] <- reverse_number_line(case_length_total[case_length_total@.Data < 0], "decreasing")
  }else{
    case_length_total <- number_line(case_length_total, Inf)
  }
  any_epl_min <- which(case_length_total@start != 1 & case_length_total@.Data != Inf)
  any_epl_min <- length(any_epl_min) > 0

  if(isTRUE(any_rolling_epi)){
    # `case_for_recurrence`
    any_case_for_rec <- any(case_for_recurrence == TRUE)
    # `recurrence_overlap_methods`
    if(!inherits(recurrence_overlap_methods,"list")){
      recurrence_overlap_methods <- list(recurrence_overlap_methods)
    }else{
      recurrence_overlap_methods <- recurrence_overlap_methods
    }
    # `recurrence_length`
    rc_l <- length_to_range(lengths = recurrence_length,
                            date = date,
                            from_last = from_last,
                            episode_unit = epid_unit)$range
    # `recurrence_length_total`
    if(is.number_line(recurrence_length_total)){
      recurrence_length_total[recurrence_length_total@.Data < 0] <- reverse_number_line(recurrence_length_total[recurrence_length_total@.Data < 0], "decreasing")
    }else{
      recurrence_length_total <- number_line(recurrence_length_total, Inf)
    }
    any_rcl_min <- which(recurrence_length_total@start != 1 & recurrence_length_total@.Data != Inf)
  }

  # `reference_event`
  if(inherits(reference_event, "logical")){
    reference_event[reference_event] <- "last_record"
    reference_event[reference_event != "last_record"] <- "first_record"
  }
  any_rec_from_last <- any(reference_event %in% c("first_record", "first_event"))
  # `skip_if_b4_lengths`
  any_skip_b4_len <- any(skip_if_b4_lengths == TRUE)

  # `d_lazy_opts`
  class(episodes_max) <- class(rolls_max) <- class(skip_order) <-
    class(from_last) <- class(episode_type) <- class(reference_event) <-
    attr(case_length_total, "opts") <- attr(case_length, "opts") <-
    class(epid_unit) <- class(skip_if_b4_lengths) <- "d_lazy_opts"

  case_overlap_methods <- lapply(case_overlap_methods, mk_lazy_opt)

  if(isTRUE(any_rolling_epi)){
    attr(recurrence_length_total, "opts") <- attr(recurrence_length, "opts") <-
      class(case_for_recurrence) <- "d_lazy_opts"
    recurrence_overlap_methods <- lapply(recurrence_overlap_methods, mk_lazy_opt)

    ld_case_for_recurrence <- case_for_recurrence
    ld_recurrence_length_total <- recurrence_length_total
  }

  # Use `overlap_methods` as a record-level input by default
  if(is.null(names(case_overlap_methods))){
    names(case_overlap_methods) <- rep("r", length(case_overlap_methods))
  }else{
    names(case_overlap_methods) <- ifelse(names(case_overlap_methods) %in% c("", NA), "r", names(case_overlap_methods))
  }

  if(isTRUE(any_rolling_epi)){
    if(is.null(names(recurrence_overlap_methods))){
      names(recurrence_overlap_methods) <- rep("r", length(recurrence_overlap_methods))
    }else{
      names(recurrence_overlap_methods) <- ifelse(names(recurrence_overlap_methods) %in% c("", NA), "r", names(recurrence_overlap_methods))
    }
  }

  date@id <- seq_len(inp_n)
  date@gid <- date@id
  if(!is.null(sn)) {
    date@gid <- as.integer(sn)
    ep_l[[1]]@gid <- as.integer(sn)
  }

  # Order of case-assignment
  ord_a <- abs(max(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start))
  ord_z <- abs(max(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date)))
  ord_a[!from_last] <- abs(min(as.numeric(date@start), na.rm = TRUE) - as.numeric(date@start[!from_last]))
  ord_z[!from_last] <- abs(min(as.numeric(right_point(date)), na.rm = TRUE) - as.numeric(right_point(date[!from_last])))

  assign_ord <- order(order(ord_a, -ord_z, date@gid))
  rm(ord_a); rm(ord_z)

  # User-defined order of case-assignment
  if(!is.null(custom_sort)) {
    if(!inherits(custom_sort, c("numeric", "integer", "double"))){
      custom_sort <- as.integer(as.factor(custom_sort))
    }
    if(length(custom_sort) == 1){
      custom_sort <- rep(custom_sort, inp_n)
    }
    assign_ord <- order(order(custom_sort, assign_ord))
  }else{
    custom_sort <- rep(0L, inp_n)
  }

  ld_case_length_total <- case_length_total
  ld_reference_event <- reference_event
  ld_skip_if_b4_lengths <- skip_if_b4_lengths
  ld_episode_type <- episode_type
  ld_skip_order <- skip_order
  ld_custom_sort <- custom_sort

  # Flags
  tag <- rep(0L, inp_n)
  iteration <- rep(0L, inp_n)

  e <- date@gid
  wind_id <- date@gid
  wind_id_lst <- list(wind_id1 = wind_id)
  epid_n <- rep(0L, inp_n)

  if(isTRUE(any_rolling_epi)) roll_n <- rep(0L, inp_n)
  case_nm <- rep(NA_integer_, inp_n)
  wind_nm <- case_nm

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, inp_n)
  }
  if(!display %in% c("none")){
    rp_data <- di_report(duration = Sys.time() - tm_ia,
                         cumm_time = Sys.time() - tm_a,
                         iteration = "Data standardisation",
                         current_tot = inp_n)
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      ite_msg <- paste0(rp_data[[1]], ": ", fmt(rp_data$duration, "difftime"), "\n")
      ite_msg_repo <- c(ite_msg_repo, ite_msg)
      cat(ite_msg)
    }
  }
  tm_ia <- Sys.time()

  # User-specified records to skip
  if(!is.null(strata)){
    lgk <- is.na(strata)
    tag[lgk] <- 2L
    case_nm[lgk] <- -1L
    iteration[lgk] <- 0L
  }

  # Skip events with non-finite `dates`
  lgk <- is.na(date@start) | is.na(date@.Data)
  tag[lgk] <- 2L
  case_nm[lgk] <- -1L
  iteration[lgk] <- 0L

  # Skip events from certain `data_source`
  if(!is.null(data_source) & !all(toupper(dl_lst) == "ANY")){
    req_links <- check_links(cri, data_source, data_links)$rq
    tag[!req_links] <- 2L
    case_nm[!req_links] <- -1L
    iteration[!req_links] <- 0L
  }

  # Skip events without the required `skip_order`
  if(any(is.finite(skip_order))){
    lgk <- order(cri, custom_sort)
    t_cri <- cri[lgk]
    t_csort <- custom_sort[lgk]
    t_csort <- t_csort[!duplicated(t_cri)]
    t_cri <- t_cri[!duplicated(t_cri)]

    min_custom_sort <- t_csort[match(cri, t_cri)]
    lgk <- min_custom_sort <= skip_order
    lgk <- !cri %in% cri[lgk]
    tag[lgk] <- 2L
    case_nm[lgk] <- -1L
    iteration[lgk] <- 0L
    rm(min_custom_sort, t_cri, t_csort)
  }

  # Flag a strata with only one event as a case
  lgk <- !duplicated(cri, fromLast = TRUE) & !duplicated(cri, fromLast = FALSE) & skip_unique_strata
  tag[lgk] <- 2L
  case_nm[lgk & is.na(case_nm)] <- 0L
  wind_nm[lgk & is.na(wind_nm)] <- 0L
  iteration[lgk] <- 0L

  excluded <- length(tag[tag == 2])
  pri_pos <- seq_len(inp_n)

  if(!display %in% c("none_with_report", "none")) cat("\n")
  epids_repo <- list("e" = e,
                     "tag" = tag,
                     "int" = date,
                     "case_nm" = case_nm,
                     "wind_nm" = wind_nm,
                     "wind_id" = wind_id,
                     "wind_id_lst" = list(wind_id),
                     "iteration" = iteration)

  # Subset out all linked records
  if(any(tag == 2)){
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)

    e <- e[ntag_lgk]
    tag <- tag[ntag_lgk]
    cri <- cri[ntag_lgk]
    assign_ord <- assign_ord[ntag_lgk]
    epid_n <- epid_n[ntag_lgk]
    custom_sort <- custom_sort[ntag_lgk]
    wind_nm <- wind_nm[ntag_lgk]
    wind_id <- wind_id[ntag_lgk]
    wind_id_lst <- list(wind_id)
    rolls_max <- rolls_max[ntag_lgk]
    iteration <- iteration[ntag_lgk]
    episodes_max <- episodes_max[ntag_lgk]
    skip_order <- skip_order[ntag_lgk]
    case_nm <- case_nm[ntag_lgk]
    episode_type <- episode_type[ntag_lgk]
    reference_event <- reference_event[ntag_lgk]
    skip_if_b4_lengths <- skip_if_b4_lengths[ntag_lgk]
    case_length_total <- case_length_total[ntag_lgk]
    ep_l <- lapply(ep_l, function(x){x[ntag_lgk]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[ntag_lgk]})

    ld_episode_type <- ld_episode_type[ntag_lgk]
    ld_custom_sort <- ld_custom_sort[ntag_lgk]
    ld_skip_order <- ld_skip_order[ntag_lgk]
    ld_reference_event <- ld_reference_event[ntag_lgk]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[ntag_lgk]
    ld_case_length_total <- ld_case_length_total[ntag_lgk]

    if(!is.null(case_sub_criteria) & length(ntag_lgk) > 0){
      case_sub_criteria <- reframe(case_sub_criteria, func = function(x) x[ntag_lgk])
    }

    if(isTRUE(any_rolling_epi)) {
      roll_n <- roll_n[ntag_lgk]
      case_for_recurrence <- case_for_recurrence[ntag_lgk]
      recurrence_length_total <- recurrence_length_total[ntag_lgk]

      ld_case_for_recurrence <- ld_case_for_recurrence[ntag_lgk]
      ld_recurrence_length_total <- ld_recurrence_length_total[ntag_lgk]

      rc_l <- lapply(rc_l, function(x){x[ntag_lgk]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[ntag_lgk]})

      if(!is.null(recurrence_sub_criteria) & length(ntag_lgk) > 0){
        recurrence_sub_criteria <- reframe(recurrence_sub_criteria, func = function(x) x[ntag_lgk])
      }
    }
    date <- date[ntag_lgk]
  }

  if(!display %in% c("none")){
    rp_data <- di_report(duration = Sys.time() - tm_ia,
                         cumm_time = Sys.time() - tm_a,
                         iteration = "Pre-tracking",
                         current_tot = inp_n,
                         current_skipped = excluded)
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      ite_msg <- paste0("Pre-tracking\n",
                        "Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                        "Skipped: ", fmt(rp_data$records_skipped), " record(s)","\n",
                        "Time: ", fmt(rp_data$duration, "difftime"),
                        "\n\n")
      ite_msg_repo <- c(ite_msg_repo, ite_msg)
      cat(ite_msg)
    }
  }
  tm_ia <- Sys.time()

  ite <- 1L
  while (suppressWarnings(min(tag)) != 2 & length(tag) > 0) {
    if(display %in% c("stats_with_report", "stats")){
      msg <- paste0("Iteration ", fmt(ite) ,".\n")
      cat(msg)
    }
    any_rolling_epi_curr <- any(episode_type %in% c(2, 3))
    any_epl_min_curr <- which(case_length_total@start != 1 & case_length_total@.Data != Inf)
    any_epl_min_curr <- length(any_epl_min_curr) == inp_n

    if(any_rolling_epi_curr){
      any_rcl_min_curr <- which(recurrence_length_total@start != 1 & recurrence_length_total@.Data != Inf)
      any_rcl_min_curr <- length(any_rcl_min_curr) == inp_n
    }

    # Sort dataset on order of case-assignment
    sort_ord <- order(cri, tag, assign_ord, decreasing = TRUE)
    if(any(tag == 2)){
      sort_ord <- sort_ord[tag[sort_ord] != 2]
    }

    e <- e[sort_ord]
    tag <- tag[sort_ord]
    cri <- cri[sort_ord]
    assign_ord <- assign_ord[sort_ord]
    date <- date[sort_ord]
    epid_n <- epid_n[sort_ord]
    custom_sort <- custom_sort[sort_ord]
    wind_nm <- wind_nm[sort_ord]
    wind_id <- wind_id[sort_ord]
    rolls_max <- rolls_max[sort_ord]
    iteration <- iteration[sort_ord]
    episodes_max <- episodes_max[sort_ord]
    skip_order <- skip_order[sort_ord]
    case_nm <- case_nm[sort_ord]
    episode_type <- episode_type[sort_ord]
    case_for_recurrence <- case_for_recurrence[sort_ord]
    reference_event <- reference_event[sort_ord]
    skip_if_b4_lengths <- skip_if_b4_lengths[sort_ord]
    case_length_total <- case_length_total[sort_ord]
    wind_id_lst <- lapply(wind_id_lst, function(x){x[sort_ord]})
    ep_l <- lapply(ep_l, function(x){x[sort_ord]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[sort_ord]})

    ld_episode_type <- ld_episode_type[sort_ord]
    ld_custom_sort <- ld_custom_sort[sort_ord]
    ld_skip_order <- ld_skip_order[sort_ord]
    ld_reference_event <- ld_reference_event[sort_ord]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[sort_ord]
    ld_case_length_total <- ld_case_length_total[sort_ord]

    if(isTRUE(any_rolling_epi_curr)){
      roll_n <- roll_n[sort_ord]
      recurrence_length_total <- recurrence_length_total[sort_ord]
      rc_l <- lapply(rc_l, function(x){x[sort_ord]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[sort_ord]})

      ld_recurrence_length_total <- ld_recurrence_length_total[sort_ord]
      ld_case_for_recurrence <- ld_case_for_recurrence[sort_ord]
    }

    current_tot <- length(tag)

    # Reference (index) events and options
    lgk <- !duplicated(cri, fromLast = TRUE)
    lgk_i <- which(!duplicated(cri, fromLast = TRUE))
    rep_lgk <- match(cri, cri[lgk])

    tr_date <- (date[lgk_i])[rep_lgk]
    tr_tag <- (tag[lgk_i])[rep_lgk]
    tr_rec_from_last <- (reference_event[lgk_i])[rep_lgk]
    tr_episode_type <- (episode_type[lgk_i])[rep_lgk]
    tr_skip_if_b4_lengths <- (skip_if_b4_lengths[lgk_i])[rep_lgk]
    tr_case_length_total <- (case_length_total[lgk_i])[rep_lgk]
    tr_e <- (e[lgk_i])[rep_lgk]
    tr_skip_order <- (skip_order[lgk_i])[rep_lgk]
    tr_custom_sort <- (custom_sort[lgk_i])[rep_lgk]
    if(any(names(case_overlap_methods) == "g") | any(names(case_overlap_methods) == "b")){
      tr_case_overlap_methods <- lapply(case_overlap_methods, function(x){
        (x[lgk_i])[rep_lgk]
      })
    }
    is_new_lgk <- tr_tag == 0
    is_new_idx <- which(is_new_lgk)

    ld_episode_type[is_new_idx] <- tr_episode_type[is_new_idx]
    ld_custom_sort[is_new_idx] <- tr_custom_sort[is_new_idx]
    ld_skip_order[is_new_idx] <- tr_skip_order[is_new_idx]
    ld_skip_if_b4_lengths[is_new_idx] <- tr_skip_if_b4_lengths[is_new_idx]
    ld_case_length_total[is_new_idx] <- tr_case_length_total[is_new_idx]
    ld_reference_event[is_new_idx] <- tr_rec_from_last[is_new_idx]

    ref_rd <- lgk | tag %in% c(-1, -2)
    lgk2 <- (ld_reference_event %in% c("first_event", "last_event") | ld_episode_type == 3) &
      is_new_lgk & lgk == FALSE
    ref_rd[lgk2] <- overlap(date[lgk2], tr_date[lgk2])
    rm(lgk2)
    ref_rd[is.na(ref_rd)] <- FALSE

    cri_indx <- (cri + (0.1 * match(ref_rd, ref_rd[!duplicated(ref_rd)])))
    r2 <- rle(cri_indx)
    cri_indx_ord <- sequence(r2$length)

    if(length(cri_indx_ord[ref_rd]) > 0){
      max_indx_ref <- max(cri_indx_ord[ref_rd]):1
    }else{
      max_indx_ref <- numeric()
    }

    tr_ep_l <- lapply(max_indx_ref, function(y){
      lgk2 <- (ref_rd & cri_indx_ord == y)
      lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
      lapply(ep_l, function(x){
        (x[lgk2])[rep_lgk]
      })
    })

    if(isTRUE(any_rolling_epi_curr)){
      tr_rc_l <- lapply(max_indx_ref, function(y){
        lgk2 <- (ref_rd & cri_indx_ord == y)
        lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
        lapply(rc_l, function(x){
          (x[lgk2])[rep_lgk]
        })
      })
    }

    tr_sn_list <- lapply(max_indx_ref, function(y){
      lgk2 <- (ref_rd & cri_indx_ord == y)
      lgk2[!cri %in% cri[lgk2] & ref_rd & cri_indx_ord == 1] <- TRUE
      (date@gid[lgk2])[rep_lgk]
    })

    if(isTRUE(any_rolling_epi_curr)){
      tr_case_for_recurrence <- (case_for_recurrence[lgk_i])[rep_lgk]
      tr_recurrence_length_total <- (recurrence_length_total[lgk_i])[rep_lgk]
      if(any(names(recurrence_overlap_methods) == "g") | any(names(recurrence_overlap_methods) == "b")){
        tr_recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){
          (x[lgk_i])[rep_lgk]
        })
      }

      ld_case_for_recurrence[is_new_idx] <- tr_case_for_recurrence[is_new_idx]
      ld_recurrence_length_total[is_new_idx] <- tr_recurrence_length_total[is_new_idx]

      lgk_p <- which(tr_tag == -1)
      roll_n[lgk_p] <- roll_n[lgk_p] + 1L
      roll_n[is_new_idx] <- 0L
    }
    epid_n[is_new_idx] <- epid_n[is_new_idx] + 1L

    lgk1 <- epid_n > episodes_max & tag != 2
    case_nm[lgk1] <- -1L
    tag[lgk1] <- 2L
    iteration[which(lgk1 & iteration == 0)] <- ite

    # Implement `skip_order`
    cri_skp <- cri[ld_custom_sort > ld_skip_order]
    cri_skp <- cri_skp[!duplicated(cri_skp)]
    lgk2 <- cri %in% cri_skp
    current_skipped <- length(cri[lgk1 | lgk2])
    tag[lgk2] <- 2L
    case_nm[lgk2] <- -1L
    iteration[lgk2 & iteration == 0] <- ite
    rm(cri_skp); rm(lgk2); rm(lgk1)

    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      current_tagged <- length(tag[tag == 2])
      current_skipped <- length(tag[tag == 0]) + current_skipped
      if(!display %in% c("none")){
        rp_data <- di_report(duration = Sys.time() - tm_ia,
                             cumm_time = Sys.time() - tm_a,
                             iteration = "Pre-tracking",
                             current_tot = current_tot,
                             current_tagged = current_tagged,
                             current_skipped = current_skipped)
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          ite_msg <- paste0("Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                            "Assigned: ", fmt(rp_data$records_tracked), " record(s)\n",
                            "Skipped: ", fmt(rp_data$records_skipped), " record(s)\n",
                            "Time: ", fmt(rp_data$duration, "difftime"),
                            "\n\n")
          ite_msg_repo <- c(ite_msg_repo, ite_msg)
          cat(ite_msg)
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data$duration, "difftime"), ")"))
        }
      }
      tm_ia <- Sys.time()
      break
    }

    tr_sn <- tr_date@gid
    if(any(names(case_overlap_methods) == "g") | any(names(case_overlap_methods) == "b")){
      # Change `overlap_method_c` to episode-level (g) or both (b) record and episode-level options
      ov_mth_a <- mapply(opt_level, names(case_overlap_methods), case_overlap_methods, tr_case_overlap_methods, SIMPLIFY = FALSE)
    }else{
      ov_mth_a <- case_overlap_methods
    }

    # Check `recurrence_length`s
    if(isTRUE(any_rolling_epi_curr)){
      if(any(names(recurrence_overlap_methods) == "g") | any(names(recurrence_overlap_methods) == "b")){
        # Change `overlap_method_r` to episode-level (g) or both (b) record and episode-level options
        ov_mth_b <- mapply(opt_level, names(recurrence_overlap_methods), recurrence_overlap_methods, tr_recurrence_overlap_methods, SIMPLIFY = FALSE)
      }else{
        ov_mth_b <- recurrence_overlap_methods
      }
    }
    # Check `case_length`s
    ords <- rep(list(lapply(1:length(tr_ep_l[[1]]), function(x) rep(x, current_tot))), length(tr_ep_l))
    ref_wind <- tr_sn_list[[1]]
    checks_lgk <- rep(0L, length(date))
    ep_checks <- lapply(1:length(tr_ep_l), function(i){
      if(i == 1){
        curr_check_lgk <- tr_tag %in% c(0, -2)
      }else{
        curr_check_lgk <- ref_wind != tr_sn_list[[i]] & tr_tag %in% c(0, -2)
      }
      if(length(curr_check_lgk[curr_check_lgk]) > 0){
        curr_result_lgk <- as.matrix(mapply(ovr_chks,
                                            rep(list(date[curr_check_lgk]), length(tr_ep_l[[i]])),
                                            lapply(tr_ep_l[[i]], function(x) x[curr_check_lgk]),
                                            lapply(ov_mth_a, function(x) as.vector(x[curr_check_lgk])),
                                            lapply(ords[[i]], function(x) x[curr_check_lgk])))
        if(length(date) == 1){
          curr_result_lgk <- t(curr_result_lgk)
        }
        checks_lgk[curr_check_lgk] <-  row_wise(curr_result_lgk, type = "max", value = TRUE)
      }
      as.integer(checks_lgk)
    })

    cr <- lapply(1:length(ep_checks), function(i){
      tr_tag %in% c(0) &
        (ref_rd  | (ep_checks[[i]] >= 1)) &
        tag != 2
    })
    if(length(cr) == 1){
      vr <- cr[[1]] & ep_checks[[1]]
    }else{
      vr <- as.logical(row_wise(sapply(cr, function(x) as.numeric(x)), type = "max")) &
        as.logical(row_wise(sapply(ep_checks, function(x) as.numeric(x)), type = "max"))
    }

    cr <- lapply(1:length(ep_checks), function(i){
      if(isTRUE(any_epl_min_curr)){
        cr <- cr[[i]]
        ep_checks <- ep_checks[[i]]
        dst <- rle(sort(cri[cr & !duplicated(ep_checks) & ep_checks != 0]))
        ep_phits <- rep(0L, current_tot)
        ep_phits[cr] <- dst$lengths[match(cri[cr], dst$values)]
        cr[!ref_rd & cr & !(ep_phits >= as.numeric(ld_case_length_total@start) & ep_phits <= as.numeric(right_point(ld_case_length_total)))] <- FALSE
        cr
      }else{
        cr[[i]]
      }
    })

    if(length(cr) == 1){
      cr <- cr[[1]]
    }else{
      cr <- as.logical(row_wise(sapply(cr, function(x) as.numeric(x)), type = "max"))
    }

    # Implement `case_sub_criteria`
    checks_lgk <- rep(FALSE, length(date))
    if(inherits(case_sub_criteria, "sub_criteria")){
      c_sub_cri <- lapply(1:max(cri_indx_ord[ref_rd]), function(i){
        cri_2 <- cri + (cr/10)
        cri_2 <- !duplicated(cri_2, fromLast = TRUE) & !duplicated(cri_2, fromLast = FALSE) & skip_unique_strata
        if(length(cr[cr & !cri_2]) > 0){
          ref_rd[ref_rd & cri_indx_ord != i] <- FALSE
          pos_repo <- make_pairs_batch_legacy(strata = cri[cr & !cri_2],
                                              index_record = ref_rd[cr & !cri_2],
                                              sn = order(order(date@id))[cr & !cri_2])

          # Check the `sub_criteria`
          lgk <- eval_sub_criteria(x = case_sub_criteria,
                                   x_pos = pos_repo$x_pos,
                                   y_pos = pos_repo$y_pos)[[1]]
          # temp external fix
          tmp_s_ord <- match(order(order(date@id))[cr & !cri_2], pos_repo$sn)
          lgk <- lgk[tmp_s_ord]
          checks_lgk[cr & !cri_2] <- lgk
          # checks_lgk[ref_rd] <- 1L
          checks_lgk
          rm(pos_repo)
        }
        checks_lgk
      })

      if(length(c_sub_cri) == 1){
        c_sub_cri <- as.logical(c_sub_cri[[1]]) & vr
      }else{
        c_sub_cri <- as.logical(row_wise(sapply(c_sub_cri, function(x) as.numeric(x)), type = "max")) &
          vr
      }
      cr[!c_sub_cri & cr & !ref_rd & tr_tag %in% c(0, -2)] <- FALSE
    }else{
      c_sub_cri <- FALSE
    }

    if(isTRUE(any_rolling_epi_curr)){
      lgk <- is_new_lgk & !cri %in% cri[vr & !ref_rd] & case_nm != -1L & !is.na(case_nm) & roll_n < rolls_max
      tr_tag[lgk] <- -1L
      tag[lgk & ref_rd] <- -1L
      case_nm[lgk & ref_rd] <- 0L
      wind_nm[lgk & ref_rd] <- 0L
      roll_n[lgk] <- roll_n[lgk] + 1L

      # Check `case_length`s
      ords <- rep(list(lapply(1:length(tr_rc_l[[1]]), function(x) rep(x, current_tot))), length(tr_rc_l))
      ref_wind <- tr_sn_list[[1]]
      rc_checks <- lapply(1:length(tr_rc_l), function(i){
        if(i == 1){
          curr_check_lgk <- tr_tag %in% c(-1)
        }else{
          curr_check_lgk <- ref_wind != tr_sn_list[[i]] & tr_tag %in% c(-1)
        }
        if(length(curr_check_lgk[curr_check_lgk]) > 0){
          curr_result_lgk <- as.matrix(mapply(ovr_chks,
                                              rep(list(date[curr_check_lgk]), length(tr_rc_l[[i]])),
                                              lapply(tr_rc_l[[i]], function(x) x[curr_check_lgk]),
                                              lapply(ov_mth_b, function(x) as.vector(x[curr_check_lgk])),
                                              lapply(ords[[i]], function(x) x[curr_check_lgk])))
          if(length(date) == 1){
            curr_result_lgk <- t(curr_result_lgk)
          }
          checks_lgk[curr_check_lgk] <-  row_wise(curr_result_lgk, type = "max", value = TRUE)
        }

        as.integer(checks_lgk)
      })

      cr2 <- lapply(1:length(rc_checks), function(i){
        (
          (tr_tag %in% c(-1) & (ref_rd | (rc_checks[[i]] >= 1))) |
            (tr_tag %in% c(-2) & (ref_rd | (ep_checks[[i]] >= 1)))
        ) &
          tag != 2
      })

      if(length(cr2) == 1){
        vr2 <- cr2[[1]] & rc_checks[[1]]
      }else{
        vr2 <- as.logical(row_wise(sapply(cr2, function(x) as.numeric(x)), type = "max")) &
          as.logical(row_wise(sapply(rc_checks, function(x) as.numeric(x)), type = "max"))
      }

      cr2 <- lapply(1:length(rc_checks), function(i){
        if(isTRUE(any_rcl_min_curr)){
          cr2 <- cr2[[i]]
          ep_checks <- ep_checks[[i]]
          rc_checks <- rc_checks[[i]]

          dst <- rle(sort(cri[cr2 & (
            (!duplicated(ep_checks) & ep_checks != 0 & tr_tag == -2) |
              (!duplicated(rc_checks) & rc_checks != 0 & tr_tag == -1)
          )
          ]))
          rc_phits <- rep(0, current_tot)
          rc_phits[cr2] <- dst$lengths[match(cri[cr2], dst$values)]
          cr2[!ref_rd & cr2 & !(rc_phits >= as.numeric(ld_recurrence_length_total@start) & rc_phits <= as.numeric(right_point(ld_recurrence_length_total)))] <- FALSE
          cr2
        }else{
          cr2[[i]]
        }
      })

      if(length(cr2) == 1){
        cr2 <- cr2[[1]]
      }else{
        cr2 <- as.logical(row_wise(sapply(cr2, function(x) as.numeric(x)), type = "max"))
      }

      if(inherits(recurrence_sub_criteria, "sub_criteria")){
        r_sub_cri <- lapply(1:max(cri_indx_ord[ref_rd]), function(i){
          cri_2 <- cri + (cr2/10)
          cri_2 <- !duplicated(cri_2, fromLast = TRUE) & !duplicated(cri_2, fromLast = FALSE) & skip_unique_strata
          if(length(cr2[cr2 & !cri_2]) > 0){
            ref_rd[ref_rd & cri_indx_ord != i] <- FALSE
            pos_repo <- make_pairs_batch_legacy(strata = cri[cr2 & !cri_2],
                                                index_record = ref_rd[cr2 & !cri_2],
                                                sn = order(order(date@id))[cr2 & !cri_2])
            # Check the `sub_criteria`
            lgk <- eval_sub_criteria(x = recurrence_sub_criteria,
                                     x_pos = pos_repo$x_pos,
                                     y_pos = pos_repo$y_pos)[[1]]
            # temp external fix
            tmp_s_ord <- match(order(order(date@id))[cr2 & !cri_2], pos_repo$sn)
            lgk <- lgk[tmp_s_ord]
            checks_lgk[cr2 & !cri_2] <- lgk
            # checks_lgk[ref_rd & cr2 & !cri_2] <- 1L
            checks_lgk
            rm(pos_repo)
          }
          checks_lgk
        })

        if(length(r_sub_cri) == 1){
          r_sub_cri <- as.logical(r_sub_cri[[1]]) & vr2
        }else{
          r_sub_cri <- as.logical(row_wise(sapply(r_sub_cri, function(x) as.numeric(x)), type = "max")) &
            vr2
        }
        cr2[!r_sub_cri & cr2 & !ref_rd & tr_tag %in% c(-1)] <- FALSE
      }else{
        r_sub_cri <- FALSE
      }

      cr[!cr & cr2] <- TRUE
      vr[!vr & vr2] <- TRUE
      rm(cr2); rm(vr2)
    }

    # Episode and window IDs
    e[cr & tag == 0 & tr_tag %in% c(0)] <- tr_sn[cr & tag == 0 & tr_tag %in% c(0)]
    wind_id[cr & tag == 0] <- tr_sn[cr & tag == 0]

    if(length(wind_id_lst) < length(tr_sn_list)){
      wind_id_lst <- c(wind_id_lst,
                       rep(wind_id_lst[1], (length(tr_sn_list) - length(wind_id_lst))))
    }else if(length(tr_sn_list) < length(wind_id_lst)){
      tr_sn_list <- c(tr_sn_list,
                      rep(tr_sn_list[1], (length(wind_id_lst) - length(tr_sn_list))))
    }

    wind_id_lst <- lapply(1:length(wind_id_lst), function(i){
      x <- wind_id_lst[[i]]
      x[cr & tag == 0] <- (tr_sn_list[[i]])[cr & tag == 0]
      x
    })

    e[cr & tr_tag %in% c(-1, -2)] <- tr_e[cr & tr_tag %in% c(-1, -2)]
    lgk_p <- which(cr & tr_tag %in% c(0))
    lgk_p1 <- which(cr & tr_tag %in% c(0) & !c_sub_cri)
    lgk_p2 <- which(cr & tr_tag %in% c(0) & c_sub_cri)
    case_nm[lgk_p1[ref_rd[lgk_p1]]] <- 0L
    case_nm[lgk_p2[ref_rd[lgk_p2]]] <- 4L
    case_nm[lgk_p[!ref_rd[lgk_p]]] <- 2L
    wind_nm[cr & tr_tag %in% c(0, -2) & is.na(wind_nm)] <- 0L
    new_hits <- cr & tag != 2 & !ref_rd
    tag[cr] <- 2L

    if(isTRUE(any_rolling_epi_curr)){
      case_nm[cr & tr_tag %in% c(-1, -2) & is.na(case_nm)] <- 3L
      wind_nm[cr & tr_tag %in% c(-1) & is.na(wind_nm)] <- 1L
      sort_ord <- order(cri, new_hits, -assign_ord, date@gid)
      t_cri <- cri[sort_ord]
      t_sn <- date@id[sort_ord]
      t_sn <- t_sn[!duplicated(t_cri, fromLast = TRUE)]
      case_nm[which(date@id %in% t_sn &
                      tr_tag %in% c(-1) &
                      new_hits &
                      !r_sub_cri
      )] <- 1L
      case_nm[which(date@id %in% t_sn &
                      tr_tag %in% c(-1) &
                      new_hits &
                      r_sub_cri
      )] <- 5L

      sort_ord <- order(cri, -tag)
      t_cri <- cri[sort_ord]
      t_tag <- tag[sort_ord]
      lgk <- !duplicated(t_cri, fromLast = TRUE)
      last_tag <- (t_tag[lgk])[match(t_cri, t_cri[lgk])]

      last_tag <- last_tag[match(date@id, date@id[sort_ord])]
      rm(t_tag); rm(t_cri)
      close_epi <- last_tag == 2

      roll_ref <- assign_ord
      if(isTRUE(any_rec_from_last)) roll_ref[ld_reference_event %in% c("first_record", "first_event")] <- -roll_ref[ld_reference_event %in% c("first_record", "first_event")]

      # Reference event for recurrence window
      t_cr <- cr
      t_cr[ref_rd & roll_n >= 1] <- FALSE
      sort_ord <- order(cri, t_cr, tag, roll_ref, date@gid)
      t_cri <- cri[sort_ord]
      t_sn <- date@id[sort_ord]
      t_lgk <- which(!duplicated(t_cri, fromLast = TRUE))
      t_sn <- t_sn[t_lgk]

      lgk <- rep(FALSE, length(date))
      if(any(ld_reference_event %in% c("first_event", "last_event"))){
        tr_t_date <- ((date[sort_ord])[t_lgk])[match(t_cri, t_cri[t_lgk])]
        tr_t_tag <- ((tag[sort_ord])[t_lgk])[match(t_cri, t_cri[t_lgk])]
        lgk2 <- which(ld_reference_event %in% c("first_event", "last_event") &
                        ld_episode_type != 3 &
                        tr_t_tag %in% c(-1, -2, 2))
        lgk[lgk2] <- overlap(date[lgk2], tr_t_date[lgk2])
        rm(lgk2)
      }

      if(any(ld_episode_type == 3)){
        lgk[new_hits & !lgk & ld_episode_type == 3] <- TRUE
      }

      tag[which((date@id %in% t_sn | lgk) &
                  tag != 0 &
                  !close_epi &
                  roll_n < rolls_max &
                  t_cr &
                  ld_episode_type %in% c(2, 3))] <- -1L

      if(isTRUE(any_case_for_rec)){
        # Reference event for recurrence window - `case_for_recurrence`
        tag[which((date@id %in% t_sn | lgk) &
                    !(ref_rd & roll_n >= 1) &
                    tr_tag == -1 &
                    !close_epi &
                    roll_n <= rolls_max &
                    t_cr &
                    ld_episode_type %in% c(2, 3) &
                    ld_case_for_recurrence == TRUE)] <- -2L
      }
    }

    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      current_tagged <- length(tag[tag == 2])
      current_skipped <- length(tag[tag == 0]) + current_skipped

      if(!display %in% c("none")){
        rp_data <- di_report(duration = Sys.time() - tm_ia,
                             cumm_time = Sys.time() - tm_a,
                             iteration = ite,
                             current_tot = current_tot,
                             current_tagged = current_tagged,
                             current_skipped = current_skipped)
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          ite_msg <- paste0("Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                            "Assigned: ", fmt(rp_data$records_tracked), " record(s)\n",
                            "Skipped: ", fmt(rp_data$records_skipped), " record(s)\n",
                            "Time: ", fmt(rp_data$duration, "difftime"),
                            "\n\n")
          ite_msg_repo <- c(ite_msg_repo, ite_msg)
          cat(ite_msg)
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data$duration, "difftime"), ")"))
        }
      }
      tm_ia <- Sys.time()
      iteration[iteration == 0] <- ite
      break
    }

    # Events in between `case_length`s and `recurrence_length`s
    if(isTRUE(any_skip_b4_len)){
      lgk <- ld_skip_if_b4_lengths & tag != 2 & tr_tag %in% c(0, -2)
      lgk <- check_skips(lgk = lgk,
                         cri = cri,
                         cr = cr,
                         vr = vr,
                         tr_ep_l = unlist(tr_ep_l, recursive = FALSE),
                         tr_date = tr_date,
                         date = date,
                         case_nm = case_nm)
      if(isTRUE(any_rolling_epi_curr)){
        lgk2 <- ld_skip_if_b4_lengths & tag != 2 & tr_tag %in% c(-1)
        lgk2 <- check_skips(lgk = lgk2,
                            cri = cri,
                            cr = cr,
                            vr = vr,
                            tr_ep_l = unlist(tr_rc_l, recursive = FALSE),
                            tr_date = tr_date,
                            date = date,
                            case_nm = case_nm)
        lgk <- c(lgk, lgk2)
        lgk <- lgk[!duplicated(lgk)]
      }

      case_nm[lgk] <- -1L
      tag[lgk] <- 2L
      current_skipped <- current_skipped + length(lgk[lgk])
    }
    iteration[tag != 0 & iteration == 0] <- ite
    current_tagged <- length(tag[tag == 2 & case_nm != -1 & !is.na(case_nm)])
    current_skipped <- length(tag[case_nm == -1 & !is.na(case_nm)])

    if(!display %in% c("none")){
      rp_data <- di_report(duration = Sys.time() - tm_ia,
                           cumm_time = Sys.time() - tm_a,
                           iteration = ite,
                           current_tot = current_tot,
                           current_tagged = current_tagged,
                           current_skipped = current_skipped)
      report <- c(report, list(rp_data))
      if(display %in% c("stats_with_report", "stats")){
        ite_msg <- paste0("Checked: ", fmt(rp_data$records_checked), " record(s)\n",
                          "Assigned: ", fmt(rp_data$records_tracked), " record(s)\n",
                          "Skipped: ", fmt(rp_data$records_skipped), " record(s)\n",
                          "Time: ", fmt(rp_data$duration, "difftime"),

                          "\n\n")
        ite_msg_repo <- c(ite_msg_repo, ite_msg)
        cat(ite_msg)
      }
    }
    # tm_ia <- Sys.time()

    # Subset out all linked records
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)
    rtag_lgk <- date@id[tag_lgk]

    epids_repo$e[rtag_lgk] <- e[tag_lgk]
    epids_repo$case_nm[rtag_lgk] <- case_nm[tag_lgk]
    epids_repo$wind_nm[rtag_lgk] <- wind_nm[tag_lgk]
    epids_repo$wind_id[rtag_lgk] <- wind_id[tag_lgk]
    epids_repo$iteration[rtag_lgk] <- iteration[tag_lgk]
    epids_repo$date[rtag_lgk] <- date[tag_lgk]
    epids_repo$tag[rtag_lgk] <- tag[tag_lgk]

    e <- e[ntag_lgk]
    cri <- cri[ntag_lgk]
    assign_ord <- assign_ord[ntag_lgk]
    epid_n <- epid_n[ntag_lgk]
    custom_sort <- custom_sort[ntag_lgk]
    skip_order <- skip_order[ntag_lgk]
    case_nm <- case_nm[ntag_lgk]
    wind_nm <- wind_nm[ntag_lgk]
    wind_id <- wind_id[ntag_lgk]
    rolls_max <- rolls_max[ntag_lgk]
    iteration <- iteration[ntag_lgk]
    episodes_max <- episodes_max[ntag_lgk]
    tag <- tag[ntag_lgk]
    episode_type <- episode_type[ntag_lgk]
    reference_event <- reference_event[ntag_lgk]
    skip_if_b4_lengths <- skip_if_b4_lengths[ntag_lgk]
    case_length_total <- case_length_total[ntag_lgk]
    ep_l <- lapply(ep_l, function(x){x[ntag_lgk]})
    case_overlap_methods <- lapply(case_overlap_methods, function(x){x[ntag_lgk]})

    if(!is.null(case_sub_criteria) & length(ntag_lgk) > 0){
      case_sub_criteria <- reframe(case_sub_criteria, func = function(x) x[sort(order(order(date@id))[ntag_lgk])])
    }

    ld_reference_event <- ld_reference_event[ntag_lgk]
    ld_episode_type <- ld_episode_type[ntag_lgk]
    ld_skip_if_b4_lengths <- ld_skip_if_b4_lengths[ntag_lgk]
    ld_case_length_total <- ld_case_length_total[ntag_lgk]

    if(length(epids_repo$wind_id_lst) < length(wind_id_lst)){
      w_diff <- length(wind_id_lst) - length(epids_repo$wind_id_lst)
      epids_repo$wind_id_lst <- c(epids_repo$wind_id_lst,
                                  rep(list(rep(NA_real_, inp_n)), w_diff))
    }
    epids_repo$wind_id_lst <- lapply(seq_len(length(wind_id_lst)), function(i){
      epids_repo$wind_id_lst[[i]][rtag_lgk] <- wind_id_lst[[i]][tag_lgk]
      epids_repo$wind_id_lst[[i]]
    })

    wind_id_lst <- lapply(wind_id_lst, function(x) x[ntag_lgk])

    if(isTRUE(any_rolling_epi_curr)){
      roll_n <- roll_n[ntag_lgk]
      case_for_recurrence <- case_for_recurrence[ntag_lgk]
      recurrence_length_total <- recurrence_length_total[ntag_lgk]
      rc_l <- lapply(rc_l, function(x){x[ntag_lgk]})
      recurrence_overlap_methods <- lapply(recurrence_overlap_methods, function(x){x[ntag_lgk]})
      if(!is.null(recurrence_sub_criteria) & length(ntag_lgk) > 0 & any_rolling_epi){
        recurrence_sub_criteria <- reframe(recurrence_sub_criteria, func = function(x) x[sort(order(order(date@id))[ntag_lgk])])
      }

      ld_case_for_recurrence <- ld_case_for_recurrence[ntag_lgk]
      ld_recurrence_length_total <- ld_recurrence_length_total[ntag_lgk]
    }
    date <- date[ntag_lgk]
    if (display %in% c("progress_with_report", "progress")) {
      progress_bar(length(epids_repo$tag[epids_repo$tag == 2]),
                   inp_n, 100,
                   msg = paste0("Iteration ",
                                fmt(ite), " (",
                                fmt(difftime(Sys.time(), tm_ia), "difftime"),
                                ")"))
    }
    tm_ia <- Sys.time()
    if(suppressWarnings(min(tag)) == 2 | length(tag) < 1){
      if(!display %in% c("none")){
        rp_data <- di_report(duration = Sys.time() - tm_ia,
                             cumm_time = Sys.time() - tm_a,
                             iteration = ite,
                             current_skipped = length(tag[tag == 0]))
        report <- c(report, list(rp_data))
        if(display %in% c("stats_with_report", "stats")){
          ite_msg <-paste0("Skipped: ", fmt(rp_data$records_skipped), " record(s)\n",
                           "Time: ", fmt(rp_data$duration, "difftime"),
                           "\n\n")
          ite_msg_repo <- c(ite_msg_repo, ite_msg)
          cat(ite_msg)
        }else if (display %in% c("progress_with_report", "progress")) {
          progress_bar(inp_n, inp_n, 100, msg = paste0("Iteration ", fmt(ite), " (", fmt(rp_data$duration, "difftime"), ")"))
        }
        tm_ia <- Sys.time()
      }
      break
    }
    ite <- ite + 1L
  }
  if(!display %in% c("none_with_report", "none")) cat("\n")

  if(length(e) > 0){
    tag_lgk <- which(tag == 2)
    ntag_lgk <- which(tag != 2)
    rtag_lgk <- date@id[tag_lgk]

    epids_repo$e[rtag_lgk] <- e[tag_lgk]
    epids_repo$case_nm[rtag_lgk] <- case_nm[tag_lgk]
    epids_repo$wind_nm[rtag_lgk] <- wind_nm[tag_lgk]
    epids_repo$wind_id[rtag_lgk] <- wind_id[tag_lgk]
    epids_repo$date[rtag_lgk] <- date[tag_lgk]
    epids_repo$tag[rtag_lgk] <- tag[tag_lgk]
    epids_repo$iteration[rtag_lgk] <- iteration[tag_lgk]

    e <- e[tag_lgk]
    case_nm <- case_nm[tag_lgk]
    wind_nm <- wind_nm[tag_lgk]
    wind_id <- wind_id[tag_lgk]
    date <- date[tag_lgk]
    tag <- tag[tag_lgk]

    if(length(epids_repo$wind_id_lst) < length(wind_id_lst)){
      w_diff <- length(wind_id_lst) - length(epids_repo$wind_id_lst)
      epids_repo$wind_id_lst <- c(epids_repo$wind_id_lst,
                                  rep(list(rep(NA_real_, inp_n)), w_diff))
    }
    epids_repo$wind_id_lst <- lapply(seq_len(length(wind_id_lst)), function(i){
      epids_repo$wind_id_lst[[i]][rtag_lgk] <- wind_id_lst[[i]][tag_lgk]
      epids_repo$wind_id_lst[[i]]
    })
  }

  names(epids_repo$wind_id_lst) <- paste0("wind_id", 1:length(epids_repo$wind_id_lst))

  # Collate all linked records
  e <- epids_repo$e
  case_nm <- epids_repo$case_nm
  wind_nm <- epids_repo$wind_nm
  wind_id <- epids_repo$wind_id
  iteration <- epids_repo$iteration
  date <- epids_repo$int

  wind_nm[which(case_nm == -1L & !is.na(case_nm))] <- -1L
  qfx <- wind_id + wind_nm/10
  qfx <- qfx[!duplicated(qfx) & wind_nm == 1L]
  wind_nm[wind_id %in% as.integer(qfx)] <- 1L
  rm(qfx)

  epid_unit <- epid_unit[match(date@id, seq_len(inp_n))]
  # `dist_epid_index` and `dist_wind_index`
  stat_pos <- date@id
  sort_ord <- order(e, wind_id, as.numeric(date@start))
  e <- e[sort_ord]
  date <- date[sort_ord]
  r <- rle(e)
  epid_n <- rep(r$lengths, r$lengths)
  lgk <- match(r$values, date@gid)
  dist_epid_index <- ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
    rep(((as.numeric(date@start[lgk]) + as.numeric(right_point(date[lgk]))) * .5),  r$lengths)

  if(isTRUE(any_rolling_epi)){
    wind_id <- wind_id[sort_ord]
    r <- rle(wind_id)
    lgk <- match(r$values, date@gid)
    dist_wind_index <- ((as.numeric(date@start) + as.numeric(right_point(date))) * .5) -
      rep(((as.numeric(date@start[lgk]) + as.numeric(right_point(date[lgk]))) * .5), r$lengths)
  }else{
    dist_wind_index <- dist_epid_index
    wind_id <- e
  }

  # Units for `dist_epid_index` and `dist_wind_index`
  lgk_p <- which(epid_unit %in% c(1, 2))
  diff_unit <- names(diyar::episode_unit)[epid_unit]
  diff_unit[lgk_p] <- paste0(substr(names(diyar::episode_unit)[epid_unit[lgk_p]], 1 ,3), "s")

  diff_unit[diff_unit %in% c("months","year")] <- "days"
  diff_unit <- diff_unit[!duplicated(diff_unit)]
  if(length(diff_unit) > 1) diff_unit <- "days"

  if(isTRUE(is_dt)){
    dist_epid_index <- dist_epid_index / as.numeric(diyar::episode_unit[epid_unit])
    dist_epid_index <- as.difftime(dist_epid_index, units = diff_unit)

    if (isTRUE(any_rolling_epi)){
      dist_wind_index <- dist_wind_index / as.numeric(diyar::episode_unit[epid_unit])
      dist_wind_index <- as.difftime(dist_wind_index, units = diff_unit)
    }else{
      dist_wind_index <- dist_epid_index
    }
  }

  tmp_pos <- date@id
  fd <- match(1:length(date), tmp_pos)
  f_e <- e[fd]

  retrieve_pos <- match(1:length(date), stat_pos)

  if(!inherits(case_length, "list")){
    case_length <- list(case_length)
  }
  if(!inherits(recurrence_length, "list")){
    recurrence_length <- list(recurrence_length)
  }
  # `epid` object
  td1 <- lapply(epids_repo$wind_id_lst, function(y) y[retrieve_pos])
  epids <- new("epid",
               .Data= e[fd],
               dist_epid_index = dist_epid_index[fd],
               dist_wind_index = dist_wind_index[fd],
               sn = date@gid[fd],
               iteration = iteration[retrieve_pos],
               wind_id = td1,
               epid_total = epid_n[fd],
               options = options_lst)

  epids@case_nm <- case_nm[retrieve_pos]
  class(epids@case_nm) <- "d_label"
  attr(epids@case_nm, "value") <- -1L : 5L
  attr(epids@case_nm, "label") <- c("Skipped", "Case", "Recurrent", "Duplicate_C", "Duplicate_R", "Case_CR", "Recurrent_CR")
  attr(epids@case_nm, "state") <- "encoded"

  epids@wind_nm <- wind_nm[retrieve_pos]
  class(epids@wind_nm) <- "d_label"
  attr(epids@wind_nm, "value") <- -1L : 1L
  attr(epids@wind_nm, "label") <- c("Skipped", "Case", "Recurrence")
  attr(epids@wind_nm, "state") <- "encoded"

  if(isTRUE(group_stats)){
    # `epid_interval` slot
    lgk <- which(epid_n != 1)
    dts_a <- lapply(split(as.numeric(date@start[lgk]), e[lgk]), min)
    dts_z <- lapply(split(as.numeric(right_point(date[lgk])), e[lgk]), max)

    dts_a <- as.numeric(dts_a)[match(e[lgk], names(dts_a))]
    dts_z <- as.numeric(dts_z)[match(e[lgk], names(dts_z))]

    case_nm <- case_nm[sort_ord]
    frm_last <- from_last[match(tmp_pos[fd], pri_pos)]
    epid_dt_a <- as.numeric(date@start)
    epid_dt_a[frm_last] <- as.numeric(right_point(date[frm_last]))
    epid_dt_z <- as.numeric(right_point(date))
    epid_dt_z[frm_last] <- as.numeric(date@start[frm_last])

    epid_dt_a[lgk] <- dts_a
    epid_dt_a[lgk[frm_last[lgk]]] <- dts_z[frm_last[lgk]]
    epid_dt_z[lgk] <- dts_z
    epid_dt_z[lgk[frm_last[lgk]]] <- dts_a[frm_last[lgk]]

    if(isTRUE(is_dt)){
      epid_dt_a <- as.POSIXct(epid_dt_a, tz = "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_dt_z <- as.POSIXct(epid_dt_z, tz = "GMT", origin = as.POSIXct("1970-01-01", "GMT"))
      epid_l <- difftime(epid_dt_z, epid_dt_a, units = diff_unit)
    }else{
      epid_l <- epid_dt_z - epid_dt_a
    }

    epids@epid_interval <- number_line(l = epid_dt_a[fd],
                                       r = epid_dt_z[fd],
                                       gid = f_e)
    # `epid_length` slot
    epids@epid_length <- epid_l[fd]
  }

  # `epid_dataset` slot
  if(!is.null(data_source)){
    data_source <- data_source[match(tmp_pos[fd], seq_len(inp_n))]
    # Data links
    rst <- check_links(e[fd], data_source, data_links)
    epids@epid_dataset <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epids <- suppressWarnings(delink(epids, !req_links))
      epids@epid_dataset[!req_links] <- data_source[!req_links]
    }
    epids@epid_dataset <- encode(epids@epid_dataset)
  }

  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    epids <- list(epid = epids, report = as.list(do.call("rbind", lapply(report, as.data.frame))))
    class(epids$report) <- "d_report"
  }
  tms <- difftime(Sys.time(), tm_a)
  if(!display %in% c("none_with_report", "none")) cat("Episodes tracked in ", fmt(tms, "difftime"), "!\n", sep = "")
  rm(list = ls()[ls() != "epids"])
  return(epids)
}

#' @rdname predefined_tests
#' @details
#' \bold{\code{range_match_legacy()}} - test that \code{overlap(as.number_line(x@gid), y)} is \code{TRUE}.
#' @examples
#' `range_match_legacy`
#' x_nl <- number_line(10, 16, gid = 10)
#' y_nl1 <- number_line(16, 10)
#' y_nl2 <- number_line(16, 10)
#'
#' range_match_legacy(x = x_nl, y = y_nl1)
#' range_match_legacy(x = x_nl, y = y_nl2)
#'
#' @export
range_match_legacy <- function(x, y) {
  lg <- overlap(as.number_line(x@gid), x)
  lg[is.na(lg)] <- F
  if(any(!lg)) {
    rng_i <- paste(head(which(!lg), 5), collapse = ", ", sep="")
    rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]
    stop(paste0("Range matching error: Actual value (gid) is out of range in ", "[", rng_i, "]"))
  }
  overlap(as.number_line(x@gid), y)
}
