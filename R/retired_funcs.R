episode_group_retired <- function(df, sn = NULL, strata = NULL, date,
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

fixed_episodes_retired <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days",
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
    episode_group_retired(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                         include_index_period=include_index_period)
  }else{
    episode_group_retired(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                         include_index_period=include_index_period)
  }
}
rolling_episodes_retired <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days",
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
    episode_group_retired(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }else{
    episode_group_retired(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }

}

record_group_retired <- function(df, sn=NULL, criteria,
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
