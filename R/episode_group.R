#' @name episode_group
#' @title Episode grouping for case definitions and record deduplication
#'
#' @description Group events into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique numerical record identifier. Optional.
#' @param strata Subsets of the dataset within which episode grouping will be done separately. In \code{\link{episode_group}}, you can use multiple columns supplied as column names. \code{\link{record_group}} can create useful \code{strata}.
#' @param date Date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}) of events.
#' @param case_length Duration after a \code{"case"} within which subsequent events are considred \code{duplicate} events. This period is refered to as the the \code{case wind_id}.
#' @param episodes_max Maximum number of times to group episodes within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Duration after the last or first event (see \code{recurrence_from_last}) of the previous window within which subsequent events are considered \code{"recurrent"} events. This period refered to as the \code{recurrence window}. If \code{recurrence_length} is not supplied, \code{case_length} is used as the \code{recurrence_length}.
#' @param episode_unit Time units. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years".
#' @param rolls_max Maximum number of times an event can reoccur within an episode. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset identifier. Useful when the dataset contains data from multiple sources. In \code{\link{episode_group}}, you can use multiple columns supplied as column names.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent event and proceeding to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest event and proceeding to the most recent one.
#' @param overlap_method Method of overlap considered when grouping periods of events. Each pair of periods are checked with the same set of \code{overlap_method}. Deprecated use \code{overlap_methods} instead.
#' @param overlap_methods Methods of overlap considered when grouping intervals. Different pairs of periods can be checked with different sets \code{overlap_methods}
#' @param custom_sort If \code{TRUE}, \code{"case"} assignment will be done with preference to this sort order. Useful in specifying that episode grouping begins at particular events regardless of chronological order. In \code{\link{episode_group}}, you can use multiple columns as sort levels.
#' @param bi_direction If \code{FALSE}, \code{"duplicate"} events will be those within the \code{case_length} period, before or after the \code{"case"} as determined by \code{from_last}. If \code{TRUE}, \code{"duplicate"} events will be those within the same period before and after the \code{"case"}.
#' @param group_stats If \code{TRUE}, the output will include additional information with useful stats for each episode group.
#' @param display If \code{TRUE}, status messages are printed on screen.
#' @param to_s4 if \code{TRUE}, changes the returned output to an \code{\link[=epid-class]{epid}} object.
#' @param recurrence_from_last if \code{TRUE}, the reference event for a \code{recurrence window} will be the last event from the previous \code{wind_id}. If \code{FALSE} (default), it will be from the first event. Only used if \code{episode_type} is \code{"rolling"}.
#' @param case_for_recurrence if \code{TRUE}, both case and recurrence events will have a \code{case window}. If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"}.
#'
#' @return
#'
#' @return \code{\link[=epid-class]{epid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{epid | .Data} - unique episode identifier
#' \item \code{wind_id} - unique window identifier
#' \item \code{wind_nm} - type of window i.e. "Case" or "Recurrence"
#' \item \code{case_nm} - record type in regards to case assignment
#' \item \code{epid_dataset} - data sources in each episode
#' \item \code{epid_interval} - episode start and end dates. A \code{\link{number_line}} object.
#' \item \code{epid_length} - difference between episode start and end dates (\code{difftime}). If possible, it's the same unit as \code{episode_unit} otherwise, a difference in days is returned
#' \item \code{epid_total} - number of records in each episode
#' }
#'
#'
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlap}} and \code{\link{number_line}}
#'
#' @details
#' Episode grouping begins at a reference event (\code{"case"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain events before proceeding forward or backwards in time.
#' The maximum duration of a \code{"fixed"} episode is the \code{case_length}. This period is refered to as the \code{case window}. The maximum duration of a \code{"rolling"} episode is the
#' \code{case_length} plus all recurrence periods. The recurrence periods are refered to as \code{recurrence windows}. This is a specified duration (\code{recurrence_length}) after the last or first (depending on \code{recurrence_from_last}) event from the pervious window. Events within this period are considered \code{"recurrent"} events.
#'
#' When a \code{data_source} identifier is provided,
#' \code{epid_dataset} is included in the output. This has the source of every event in each episode.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' #1. Fixed episodes
#' data(infections); infections
#' db_1 <- infections
#' # 16-day (difference of 15 days) episodes beginning from the earliest record
#' db_1$fd <- fixed_episodes(db_1$date, case_length = 15, display = FALSE)
#' # 16-hour (difference of 15 hours) episodes beginning from the earliest record
#' db_1$fh <- fixed_episodes(db_1$date, case_length = 15,
#' episode_unit = "hours", display = FALSE)
#' db_1
#'
#' #2. Rolling episodes
#' # Case length and recurrence periods of 16 days
#' db_1$rd_a <- rolling_episodes(db_1$date, case_length = 15, display = FALSE)
#' # Case length of 16 days and recurrence periods of 11 days
#' db_1$rd_b <- rolling_episodes(db_1$date, case_length = 15,
#' recurrence_length = 10, display = FALSE)
#' # Case length of 16 days and 2 recurrence periods of 11 days
#' db_1$rd_c <- rolling_episodes(db_1$date, case_length = 15,
#' recurrence_length = 10, rolls_max = 2, display = FALSE)
#' db_1
#'
#' # 3. Stratified episode grouping
#' db_3 <- infections
#'
#' db_3$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#' # One 16-day episode per patient
#' db_3$epids_p <- fixed_episodes(date=db_3$date, strata = db_3$patient_id,
#' case_length = 15, episodes_max = 1, display = FALSE)
#' db_3
#'
#' # 4. Case assignment
#' db_4 <- infections
#'
#' ## 4.1 Chronological order
#' db_4$forward_time <- fixed_episodes(db_4$date, case_length = 1,
#' episode_unit = "months", display = FALSE)
#' db_4$backward_time <- fixed_episodes(db_4$date, case_length = 1,
#' episode_unit = "months", from_last = TRUE, display = FALSE)
#' db_4
#'
#' ## 4.2 User defined order
#' db_4b <- infections
#' db_4b
#' # RTI > UTI, or RTI > BSI
#' db_4b$ord1 <- ifelse(db_4b$infection =="RTI",0,1)
#' # UTI > BSI > RTI
#' db_4b$ord2 <- factor(db_4b$infection, levels = c("UTI","BSI","RTI"))
#'
#' db_4b$epids_1 <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord1, display = FALSE)
#' db_4b$epids_2 <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord2, display = FALSE)
#' db_4b$epids_2b <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord2, bi_direction = TRUE, display = FALSE)
#' db_4b
#'
#' #5. Interval grouping
#' data(hospital_admissions)
#'
#' hospital_admissions$admin_period <- number_line(hospital_admissions$admin_dt,
#' hospital_admissions$discharge_dt)
#' admissions <- hospital_admissions[c("admin_period","epi_len")]
#' admissions
#'
#' # Episodes of overlaping periods of admission
#' admissions$epi_0 <- fixed_episodes(date=admissions$admin_period, case_length = 0,
#' group_stats = TRUE, to_s4=TRUE)
#' admissions
#'
#' # Overlaping periods of admission seperated by 1 month
#' admissions$epi_1 <- fixed_episodes(date=admissions$admin_period, case_length = 1,
#' episode_unit = "months", group_stats = TRUE, display = FALSE)
#' admissions
#'
#' # Episodes of chained admission periods, and those with aligned end periods
#' admissions$epi_0b <- fixed_episodes(date=admissions$admin_period, case_length = 0,
#' overlap_methods = c("chain|aligns_end"), group_stats = TRUE, display = FALSE)
#' admissions["epi_0b"]
#'
#'
#' # Note - episode_group() takes column names not actual values
#' db_5 <- infections
#'
#' db_5$recur <- 20
#' db_5$epids_f <- episode_group(db_5, date=date, episode_type = "fixed",
#' case_length = epi_len, display = FALSE)
#' db_5$epids_r <- episode_group(db_5, date=date, episode_type = "rolling",
#' case_length = epi_len, recurrence_length = recur, display = FALSE)
#' db_5
#'
#' @aliases episode_group
#' @export
#' @rdname episode_group
episode_group <- function(df, sn = NULL, strata = NULL, date,
                          case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL,
                          custom_sort = NULL, from_last=FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"),
                          overlap_methods = NULL, bi_direction = FALSE,
                          group_stats= FALSE, display=TRUE, deduplicate=FALSE, to_s4 = TRUE, recurrence_from_last = TRUE, case_for_recurrence =FALSE){
  . <- NULL

  if(missing(date)) stop("argument 'date' is missing, with no default")
  if(missing(case_length)) stop("argument 'case_length' is missing, with no default")

  # check that only logicals are passed to arguments that expect logicals.
  logs_check <- logicals_check(c("from_last", "bi_direction", "group_stats", "display", "deduplicate", "to_s4", "recurrence_from_last", "case_for_recurrence"))
  if(logs_check!=T) stop(logs_check)

  # Suggesting the use `epid` objects
  if(to_s4 == FALSE){
    # check if episode_group() was called by fixed_episodes() or rolling_episodes()
    wrap_func <- c("rolling_episodes","fixed_episodes")
    call <- deparse(sys.call(-(sys.nframe()-1)))
    lg <- unlist(lapply(wrap_func, function(x){
      grepl(paste("^",x,"\\(",sep=""), call)
    }))

    # if not, display the message
    if(all(!lg)){
      if (is.null(getOption("diyar.episode_group.output"))){
        options("diyar.episode_group.output"= T)
      }
      if (getOption("diyar.episode_group.output")){
        message(paste("The default output of episode_group() will be changed to epid objects in the next release.",
                      "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
                      "",
                      "# Old way - merge or bind (col) results back to `df`",
                      "df <- cbind(df, episode_group(df, case_length= x))",
                      "",
                      "# New way - `epid` objects",
                      "df$epids <- episode_group(df, case_length= x, to_s4 = TRUE)",
                      "This message is displayed once per session.", sep = "\n"))
      }
      options("diyar.episode_group.output"= FALSE)
    }
  }

  episodes_max <- ifelse(is.numeric(episodes_max) & !is.na(episodes_max) & !is.infinite(episodes_max), as.integer(episodes_max), episodes_max)
  rolls_max <- ifelse(is.numeric(rolls_max) & !is.na(rolls_max) & !is.infinite(rolls_max), as.integer(rolls_max), rolls_max)

  # validations
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
  dt <- enq_vr(substitute(date))
  methods <- enq_vr(substitute(overlap_methods))

  # Check that col names exist
  if(any(!unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods)), !unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found"))
  }

  if(!(any(class(df[[epl]]) %in% c("integer","double","numeric"))))  stop(paste("'case_length' must be integer or numeric values", sep=""))
  if(!is.null(r_epl)){
    if(!(any(class(df[[r_epl]]) %in% c("integer","double","numeric")))) stop(paste("'recurrence_length' must be integer or numeric values", sep=""))
  }
  if(!(any(class(df[[dt]]) %in% c("Date","POSIXct","POSIXt","POSIXlt","number_line","numeric","integer","Interval")))) stop("'date' must be a date, datetime, numeric or number_line object")

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

  # lengths
  df$epi_len <- df[[epl]]

  if(is.null(r_epl) | episode_type !="rolling" ){
    df$rc_len <- df$epi_len
  }else{
    df$rc_len <- df[[r_epl]]
  }

  # Strata
  if(is.null(st)){
    df$cri <- "A"
  }else{
    df$cri <- eval(parse(text = paste0("paste0(",paste0("df$", st, collapse = ",'-',"),")")))
  }

  # Date
  if(any(class(df[[dt]]) %in% c("number_line","Interval"))){
    df$rec_dt_ai <- diyar::left_point(df[[dt]])
    df$rec_dt_zi <- diyar::right_point(df[[dt]])
  }else{
    df$rec_dt_ai <- df[[dt]]
    df$rec_dt_zi <- df[[dt]]
  }

  fn_check <- finite_check(df$rec_dt_zi)
  if(fn_check!=T) stop(paste0("Finite values of 'date' required in indexes ",fn_check))

  # Class of 'date'
  dt_grp <- ifelse(!any(class(df$rec_dt_ai) %in% c("Date","POSIXct","POSIXt","POSIXlt")) |
                     !any(class(df$rec_dt_zi) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)

  episode_unit <- ifelse(dt_grp==F,"seconds", episode_unit)

  if(dt_grp==T){
    df$rec_dt_ai <- as.POSIXlt(format(df$rec_dt_ai, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
    df$rec_dt_zi <- as.POSIXlt(format(df$rec_dt_zi, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
  }else{
    df$rec_dt_ai <- as.numeric(df$rec_dt_ai)
    df$rec_dt_zi <- as.numeric(df$rec_dt_zi)
  }

  # Overlap methods
  if(missing(overlap_methods) & !missing(overlap_method)) {
    df$methods <- paste0(overlap_method, collapse = "|")
    warning("'overlap_method' is deprecated. Please use 'overlap_method' instead.")
  }else{
    if(is.null(methods)){
      df$methods <- "exact|across|chain|aligns_start|aligns_end|inbetween"
    }else {
      df$methods <- df[[methods]]
    }
  }

  o <- unique(unlist(strsplit(df$methods[!duplicated(df$methods)], split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  df <- df[c("sn","rec_dt_ai","rec_dt_zi","epi_len","rc_len","dsvr","cri",ref_sort,"methods")]

  df$dist_from_epid <- df$dist_from_wind <- df$wind_id <- df$tag <- df$roll <- df$episodes <- 0
  df$wind_nm <- ""
  df$epid <- sn_ref <- min(df$sn)-1
  df$case_nm <- ""
  df$pr_sn = 1:nrow(df)

  # Chronological order
  if(from_last==T){
    df$ord <- abs(max(df$rec_dt_ai) - df$rec_dt_ai)
    df$ord_z <- abs(max(df$rec_dt_zi) - df$rec_dt_zi)
  }else{
    df$ord <- abs(min(df$rec_dt_ai) - df$rec_dt_ai)
    df$ord_z <- abs(min(df$rec_dt_zi) - df$rec_dt_zi)
  }

  # Custom sort
  user_ord <- eval(parse(text = paste0("order(",paste0("df$", c(ref_sort, "ord"), collapse = ", "),", -df$ord_z)")))
  names(user_ord) <- 1:length(user_ord)
  df$user_ord <- as.numeric(names(sort(user_ord)))

  # Skip from episode grouping
  df$epid <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , df$sn, df$epid)
  df$wind_id <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , df$sn, df$wind_id)
  df$wind_nm <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , "Skipped", df$wind_nm)
  df$tag <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 2, df$tag)
  df$case_nm <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), "Skipped", df$case_nm)
  df$episodes <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 1, 0)
  df$roll <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), rolls_max, df$roll)

  min_tag <- min(df$tag)
  min_episodes <- min(df$episodes)

  c <- 1
  grouped_epids <- df[0,0]
  while (min_tag != 2 & min_episodes <= episodes_max){
    #vrs <- names(df)[!names(df) %in% c(c("epi_len","rc_len"))]
    g_vrs <- c("sn","pr_sn","rec_dt_ai","rec_dt_zi","dsvr","epid","wind_id","wind_nm","case_nm","user_ord","ord","ord_z", "dist_from_wind", "dist_from_epid")
    grouped_epids <- rbind(grouped_epids,
                         df[df$tag ==2 & !is.na(df$tag), g_vrs] )

    # exclude grouped episodes
    df <- df[df$tag !=2 & !is.na(df$tag),]

    # reference events
    TR <- df[order(df$cri, -df$tag, df$user_ord, df$sn),]
    TR <- TR[!(TR$tag==0 & TR$episodes + 1 > episodes_max)  &
               duplicated(TR$cri) == FALSE & !is.na(TR$cri), c("sn", "cri", "rec_dt_ai", "rec_dt_zi",
                                                               "epid", "tag", "roll", "epi_len", "rc_len", "case_nm")]
    names(TR) <- paste0("tr_",names(TR))

    # early break if there are no more reference events
    if(nrow(TR)==0) {break}

    if(display){cat(paste("Episode or recurrence window ",c,".\n", sep=""))}

    df <- merge(df, TR, by.x="cri", by.y="tr_cri", all.x=T)
    df$lr <- ifelse(df$tr_sn == df$sn & !is.na(df$tr_sn),1,0)

    # Case and recurrence lengths
    df$c_int <- diyar::number_line(df$rec_dt_ai, df$rec_dt_zi)
    df$r_int <- diyar::number_line(df$rec_dt_ai, df$rec_dt_zi)

    # Case and recurrence lengths of reference events
    df$tr_rc_len <- df$tr_rc_len * diyar::episode_unit[[episode_unit]]
    df$tr_epi_len <- df$tr_epi_len * diyar::episode_unit[[episode_unit]]

    df$tr_c_int <- suppressWarnings(diyar::number_line(df$tr_rec_dt_ai, df$tr_rec_dt_zi))
    df$tr_r_int <- suppressWarnings(diyar::number_line(df$tr_rec_dt_ai, df$tr_rec_dt_zi))

    bdir <- ifelse(bi_direction,"both","end")
    if (from_last==F){
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, df$tr_epi_len, bdir))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, df$tr_rc_len, bdir))
    }else{
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, -df$tr_epi_len, bdir))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, -df$tr_rc_len, bdir))
    }

    # Check if events overlap
    df$r_range <- df$c_range <- F

    df$c_range <- diyar::overlap(df$c_int, df$tr_c_int, methods = df$methods)
    df$r_range <- diyar::overlap(df$r_int, df$tr_r_int, methods = df$methods)

    # distance from window's ref event
    df$dist_from_wind <- ((as.numeric(df$rec_dt_ai) + as.numeric(df$rec_dt_zi)) *.5) - ((as.numeric(df$tr_rec_dt_ai) + as.numeric(df$tr_rec_dt_zi)) *.5)
    # distance from episodes's ref event
    df$dist_from_epid <- ifelse(df$tr_case_nm=="", ((as.numeric(df$rec_dt_ai) + as.numeric(df$rec_dt_zi)) *.5) - ((as.numeric(df$tr_rec_dt_ai) + as.numeric(df$tr_rec_dt_zi)) *.5), df$dist_from_epid)

    if(!bi_direction & !from_last){
      df$c_range <- ifelse(df$tr_rec_dt_ai > df$rec_dt_ai, FALSE, df$c_range)
      df$r_range <- ifelse(df$tr_rec_dt_ai > df$rec_dt_ai, FALSE, df$r_range)

    }else if(!bi_direction & from_last){
      df$c_range <- ifelse(df$tr_rec_dt_ai < df$rec_dt_ai, FALSE, df$c_range)
      df$r_range <- ifelse(df$tr_rec_dt_ai < df$rec_dt_ai, FALSE, df$r_range)
    }

    # event type
    # 1 - Reference event (Case)
    # 2 - Duplicate of a case event
    # 6 - Recurrent event
    # 7 - Duplicate of a recurrent event
    # 9 - Reference event for a case window. - 1 & 9 are case windows
    # 10 - Duplicate event for a case window. - 2 & 10 are duplicates in case windows
    # Episode assignment -------
    df$epid_type <- ifelse(
      df$r_range == T & !is.na(df$r_range) & df$tr_tag %in% c(1,1.5,0),
      ifelse(df$case_nm=="Duplicate", 7,6), 0
    )

    df$epid_type <- ifelse(
      df$c_range == T & !is.na(df$c_range) &
        ((df$tr_tag==0 & !is.na(df$tr_tag)) | (df$tr_tag %in% c(1.4,1.6) & !is.na(df$tr_tag))),
      ifelse(df$tr_tag==0,
             ifelse(df$lr==1, 1,2),
             ifelse(df$lr==1, 9, 10)),
      df$epid_type
    )

    df$c_hit <- ifelse(df$epid_type %in% c(1,2,7,6,10) | (df$lr==1), 1, 0)

    df$epid <- ifelse(
      df$c_hit==1, ifelse(df$tr_tag ==0 & !is.na(df$tr_tag), df$tr_sn, df$tr_epid),
      df$epid
    )

    df$wind_id <- ifelse(df$c_hit==1 & (df$lr !=1 | (df$lr ==1 & df$tr_case_nm=="")), df$tr_sn, df$wind_id)
    df$wind_nm <- ifelse((df$epid_type %in% c(1,2,9,10) | (df$epid_type==0 & df$lr==1))  & df$wind_nm=="", "Case", ifelse(df$epid_type!=0 & df$wind_nm=="", "Recurrence", df$wind_nm))

    df$case_nm <- ifelse(
      df$c_hit==1 & !df$tag %in% c(1.4,1.6),
      ifelse(df$epid_type %in% c(1,0), "Case",
             ifelse(df$epid_type==6 & episode_type=="rolling","Recurrent","Duplicate")),
      df$case_nm
    )
    # ---------

    vrs <- names(df)[!grepl("_range|_int", names(df))]
    df <- df[order(df$cri, df$epid, df$user_ord, df$sn), vrs]

    df$episodes <- ifelse(df$tr_tag==0 & !is.na(df$tr_tag), df$episodes + 1, df$episodes)
    df$tag <- ifelse(df$c_hit==1 | (!df$tag %in% c(NA,0,2) & df$sn == df$tr_sn), 2, df$tag)
    df$mrk_x <- paste(df$cri, df$tag, sep="-")

    df$fg_a <- rep(rle(df$cri)$lengths, rle(df$cri)$lengths)
    df$fg_x <- rep(rle(df$mrk_x)$lengths, rle(df$mrk_x)$lengths)
    df$fg_c <- ifelse(df$fg_a==df$fg_x & df$tag==2, 1,0)

    df$mrk_z <- paste0(df$case_nm, df$epid, df$lr)
    df$mrk_z2 <- paste0(df$case_nm, df$epid)
    df$case_nm <- ifelse(df$lr !=1 &
                           df$case_nm=="Recurrent" &
                           (duplicated(df$mrk_z))
                         ,"Duplicate", df$case_nm)

    if(min(df$fg_c)==1) {
      vrs <- names(df)[!grepl("^tr|^fg|^mrk", names(df))]
      df <- df[vrs]
      tagged_1 <- length(df$epid[!df$epid %in% c(sn_ref,NA) & df$tag==2])
      total_1 <- nrow(df)

      if(display){
        cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
      }
      break
    }

    if (episode_type=="rolling"){
      df$d_grp <- ifelse(df$case_nm=="Duplicate" & df$sn != df$tr_sn & !is.na(df$tr_sn), 1, 0)

      pds2 <- lapply(split(df$d_grp, df$epid), max)
      df$d_grp <- as.numeric(pds2[as.character(df$epid)])

      df$d_ord <- ifelse(df$case_nm=="Duplicate" & df$lr !=1, df$user_ord, NA)
      pds2 <- lapply(split(df$d_ord, df$epid), function(x){
        suppressWarnings(min(x, na.rm=T))
      })
      df$d_ord <- as.numeric(pds2[as.character(df$epid)])
    }else{
      df$d_ord <- df$d_grp <- 0
    }

    # Number of recurrence periods so far
    df$roll <- ifelse((df$tr_case_nm == "Recurrent" & !is.na(df$tr_case_nm)) |
                        (df$tr_tag== 1.5 & !is.na(df$tr_tag)),df$tr_roll + 1,  df$roll)

    # Chose the next reference event
    df$mrk_z <- paste0(df$case_nm, df$epid)
    df$tag <- ifelse(episode_type == "rolling" &
                       (df$roll < rolls_max |(case_for_recurrence==T & df$roll < rolls_max+1) )&
                       !(df$lr==1 & df$tr_tag %in% c(1, 1.5, 1.4, 1.6)) & df$case_nm %in% c("Duplicate","Recurrent"),
                     ifelse(grepl("^Duplicate",df$case_nm),
                            ifelse(df$case_nm =="Duplicate" & !duplicated(df$mrk_z, fromLast = T) &
                                     recurrence_from_last == T,
                                   ifelse(case_for_recurrence==T & df$tr_tag==1.5, 1.6,1.5), 2),
                            ifelse(df$case_nm=="Recurrent" &
                                     df$d_grp ==1  &
                                     df$user_ord < df$d_ord & df$d_ord != Inf &
                                     df$tr_case_nm %in% c("Duplicate","") &
                                     recurrence_from_last ==T, 2,
                                   ifelse(case_for_recurrence==F, 1,1.4))),
                     df$tag)

    if(episode_type=="rolling"){
      # Number of recurrence periods so far - Recalculate
      fx <- df[df$epid!=sn_ref & df$lr!=1 & df$tag!=2 & df$tr_case_nm=="", c("epid","case_nm","tag")]
      fx <- fx[order(-fx$tag),]
      fx <- fx[fx$case_nm=="Recurrent" & !duplicated(fx$epid),]
      fx <- fx$epid
      df$roll[df$epid %in% fx] <- df$roll[df$epid %in% fx] + 1
    }

    tagged_1 <- length(df$epid[!df$epid %in% c(sn_ref,NA) & df$tag==2])
    total_1 <- nrow(df)
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }

    df <- df[names(df)[!grepl("^tr|^fg|^mrk", names(df))]]
    min_tag <- min(df$tag)
    min_episodes <- min(df$episodes)

    c = c+1
  }

  # Combine all episodes again
  df <- rbind(df[g_vrs], grouped_epids)
  rm(grouped_epids)

  # Assign ungrouped episodes to unique IDs
  df$wind_nm[df$epid==sn_ref] <- df$case_nm[df$epid==sn_ref] <- "Skipped"
  df$epid[df$epid==sn_ref] <- df$wind_id[df$epid==sn_ref] <- df$sn[df$epid==sn_ref]

  # Drop 'duplicate' events if required
  if(deduplicate) df <- subset(df, df$case_nm!="Duplicate")

  if(is.null(ds)){
    #df <- df[c("sn","epid","wind_id","case_nm","pr_sn", "rec_dt_ai", "rec_dt_zi", "ord", "ord_z","user_ord")]
    df <- df[order(df$pr_sn),]
  }else{
    pds2 <- lapply(split(df$dsvr, df$epid), function(x){
      paste0(sort(unique(x)), collapse=",")
    })

    df$epid_dataset <- unlist(pds2[as.character(df$epid)])
    df <- df[order(df$pr_sn),]
    #df <- df[c("sn","epid","wind_id","case_nm","epid_dataset","pr_sn", "rec_dt_ai", "rec_dt_zi", "ord", "ord_z","user_ord")]
  }

  diff_unit <- ifelse(tolower(episode_unit) %in% c("second","minutes"),
                      paste0(substr(tolower(episode_unit),1,3),"s"),
                      tolower(episode_unit))

  diff_unit <- ifelse(diff_unit %in% c("months","year"), "days", diff_unit)

  # Episode stats if required
  if(group_stats){
    # epid_l <- df[c("epid", "rec_dt_ai", "rec_dt_zi", "ord", "ord_z","user_ord")]
    # epid_l <- unique(epid_l)
    #
    # epid_l <-
    #   dplyr::bind_rows(
    #     dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord), !duplicated(.data$epid)), var ="a" ),
    #     dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord_z), !duplicated(.data$epid, fromLast = T)), var ="z" )
    #   ) %>%
    #   dplyr::mutate_at(c("rec_dt_ai", "rec_dt_zi"), ~ ifelse(dt_grp==rep(T,length(.)), format(., "%d/%m/%Y %H:%M:%S"),.)) %>%
    #   dplyr::mutate(val = ifelse(.data$var=="a", .data$rec_dt_ai, .data$rec_dt_zi)) %>%
    #   dplyr::select(.data$epid, .data$var, .data$val)
    #
    # epid_l <- tidyr::spread(epid_l, "var","val")
    #
    # df <- dplyr::left_join(df, epid_l, by="epid")
    df$a <- as.numeric(lapply(split(as.numeric(df$rec_dt_ai), df$epid), ifelse(from_last==F, min, max))[as.character(df$epid)])
    df$z <- as.numeric(lapply(split(as.numeric(df$rec_dt_zi), df$epid), ifelse(from_last==F, max, min))[as.character(df$epid)])

    if(dt_grp == T){
      df$a <- as.POSIXct(df$a, "UTC", origin = as.POSIXlt("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      df$z <- as.POSIXct(df$z, "UTC", origin = as.POSIXlt("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      #df$epid_length <- lubridate::make_difftime(difftime(df$z, df$a, units = "secs"), units = diff_unit)
      df$epid_length <- difftime(df$z, df$a, units = diff_unit)
    }else{
      df$epid_length <- df$z - df$a
    }
    df <- df[order(df$epid),]

    df$epid_total <- rep(rle(df$epid)$lengths, rle(df$epid)$lengths)
    df <- df[order(df$pr_sn),]

    df$epid_interval <- diyar::number_line(df$a, df$z, id=df$sn, gid = df$epid)

    vrs <- names(df)[!grepl("^a$|^z$", names(df))]
    df <- df[vrs]
  }

  vrs <- names(df)[!grepl("^pr_sn|^rec_dt_|^ord|^epi_len|^user_ord", names(df))]
  df <- df[vrs]

  if(dt_grp==T){
    df$dist_from_wind <- df$dist_from_wind / diyar::episode_unit[[episode_unit]]
    df$dist_from_wind <- as.difftime(df$dist_from_wind, units = diff_unit)

    df$dist_from_epid <- df$dist_from_epid / diyar::episode_unit[[episode_unit]]
    df$dist_from_epid <- as.difftime(df$dist_from_epid, units = diff_unit)
  }


  unique_ids <- length(df[!duplicated(df$epid) & !duplicated(df$epid, fromLast = T),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste(pd, "Episode grouping complete - " ,fmt(unique_ids)," record(s) assinged a unique ID. \n" , sep =""))
  if(to_s4) df <- diyar::to_s4(df)
  df
}

#' @rdname episode_group
#' @param deduplicate if \code{TRUE}, \code{"dupilcate"} records are excluded from the output.
#' @param x Record date or interval. Deprecated. Please use \code{date}
#' @details
#' \code{fixed_episodes()} and \code{rolling_episodes()} are wrapper functions of \code{episode_group()}.
#' They are convenient alternatives with the same functionalities.
#'
#' @rdname episode_group
#' @export
fixed_episodes <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days", episodes_max = Inf, data_source = NULL, custom_sort = NULL,
                           from_last = FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"), overlap_methods =  "exact|across|chain|aligns_start|aligns_end|inbetween",
                           bi_direction= FALSE, group_stats = FALSE, display = TRUE, deduplicate = FALSE, x, to_s4 = TRUE){

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

  log_vals <-  lapply(list(from_last, bi_direction, group_stats, display, deduplicate, to_s4), function(x){
    is.logical(x)
  })

  log_vals <- c("from_last", "bi_direction", "group_stats", "display", "deduplicate", "to_s4")[unlist(log_vals)==F]
  if(length(log_vals)==1) {
    stop(paste0("'", log_vals[1], "' must be either TRUE or FALSE"))
  }else if (length(log_vals)>1){
    stop(paste0(paste0("'",log_vals[1:(length(log_vals)-1)],"'", collapse = ", "), " and '", log_vals[length(log_vals)], "' must be either TRUE or FALSE"))
  }

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(!all(is.finite(case_length)) ) stop(paste("'case_length' must be integer or numeric values",sep=""))
  if(!all(is.finite(date))) stop(paste("All 'date' values must be a date, datetime, numeric or number_line object",sep=""))
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))

  if(missing(overlap_methods) & !missing(overlap_method)) {
    warning("'overlap_method' is deprecated. Please use 'overlap_methods' instead.")
    m <- paste(overlap_method,sep="", collapse = "|")
  }else{
    m <- overlap_methods
  }

  if(!(length(m) %in% c(1, length(date)))) stop(paste("length of 'overlap_methods' must be 1 or the same as 'date'",sep=""))
  o <- unique(unlist(strsplit(unique(m), split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  df <- data.frame(dts = date, epl = case_length, stringsAsFactors = FALSE)

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

  df$method <- m

  if(is.null(data_source)){
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt",
                         from_last = from_last, overlap_methods = "method",
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4)
  }else{
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt",
                         from_last = from_last, overlap_methods = "method",
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4)
  }
}


#' @rdname episode_group
#' @export
rolling_episodes <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days", episodes_max = Inf, rolls_max = Inf, data_source = NULL, custom_sort = NULL,
                             from_last = FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"), overlap_methods =  "exact|across|chain|aligns_start|aligns_end|inbetween",
                             bi_direction= FALSE, group_stats = FALSE, display = TRUE, deduplicate = FALSE, x, to_s4 = TRUE, recurrence_from_last = TRUE, case_for_recurrence =FALSE){

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

  log_vals <-  lapply(list(from_last, bi_direction, group_stats, display, deduplicate, to_s4, recurrence_from_last, case_for_recurrence), function(x){
    is.logical(x)
  })

  log_vals <- c("from_last", "bi_direction", "group_stats", "display", "deduplicate", "to_s4", "recurrence_from_last", "case_for_recurrence")[unlist(log_vals)==F]
  if(length(log_vals)==1) {
    stop(paste0("'", log_vals[1], "' must be either TRUE or FALSE"))
  }else if (length(log_vals)>1){
    stop(paste0(paste0("'",log_vals[1:(length(log_vals)-1)],"'", collapse = ", "), " and '", log_vals[length(log_vals)], "' must be either TRUE or FALSE"))
  }

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(!all(is.finite(case_length)) ) stop(paste("'case_length' must be integer or numeric values",sep=""))
  if(!(all(is.finite(recurrence_length)) | is.null(recurrence_length)) ) stop(paste("'recurrence_length' must be integer or numeric values",sep=""))
  if(!all(is.finite(date))) stop(paste("All 'date' values must be a date, datetime, numeric or number_line object",sep=""))
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(date)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))

  if(missing(overlap_methods) & !missing(overlap_method)) {
    warning("'overlap_method' is deprecated. Please use 'overlap_methods' instead.")
    m <- paste(overlap_method,sep="", collapse = "|")
  }else{
    m <- overlap_methods
  }

  if(!(length(m) %in% c(1, length(date)))) stop(paste("length of 'overlap_methods' must be 1 or the same as 'date'",sep=""))
  o <- unique(unlist(strsplit(unique(m), split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste0("\n",
                              paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                              "Valid 'overlap_methods' are 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween' \n\n",
                              "Syntax ~ \"method1|method2|method3...\" \n",
                              "                 OR                   \n",
                              "Use ~ include_overlap_method() or exclude_overlap_method()"))

  df <- data.frame(dts = date, epl = case_length, stringsAsFactors = FALSE)

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

  if(is.null(recurrence_length)){
    df$rc_epl <- df$epl
  }else{
    df$rc_epl <- recurrence_length
  }

  df$method <- m

  if(is.null(data_source)){
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence)
    }else{
      diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                           bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt",
                           from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max,
                           display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                           recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence)
  }

}

#' @rdname episode_group
#' @param epid \code{epid} object
#' @details
#' \code{plot_epid()} visulaises how an episode has been created. It works backwards, using the episode (\code{epid}) and corresponding
#'  \code{date}, \code{case_length} and \code{recurrence_length} to show why/how events or event periods have been grouped in each episode.
#'  This is then shown on a plots (one per \code{strata}) captured in an \code{R} object (\code{list} is there are multiple plots).
#'  The plots can then be saved and shared.
#'  \code{date}, \code{case_length} and \code{recurrence_length} must match those used in creating \code{epid} otherwise, the plot will not reflect what actually happened.
#'
plot_epid <- function(epid, date= NULL, strata = NULL, case_length = NULL, recurrence_length = NULL, from_last=FALSE){
  if(!(length(epid) == length(date) | is.null(date))) stop(paste0("lenght(epid) should be equal to length(date)"))
  if(!(length(case_length) %in% c(1, length(epid)) | (length(case_length) ==0 & is.null(case_length)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(epid)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'date'",sep=""))

  # Scale factor - Automatcially try to resize element spacing and size
  scale_fac <- 1

  # Events, window and episode color groups
  pl_cols <- grDevices::colours()
  pl_cols <- pl_cols[!duplicated(substr(pl_cols,1,5))]

  # episodes
  dfp <- to_df(epid)

  # corresponding event dates
  if(!is.null(date)){
    dfp$date <- date
    if(is.number_line(dfp$date)){
      dfp$dt_a <- left_point(dfp$date)
      dfp$dt_z <- right_point(dfp$date)
    }else{
      dfp$dt_a <- dfp$date
      dfp$dt_z <- dfp$date
    }
  }else{
    #dfp$dt_z <- dfp$dt_a <- as.numeric(cut(1:20, length(epid)))
    dfp$date <- dfp$dt_z <- dfp$dt_a <- 1:nrow(dfp) * 3
  }

  # Sort chronologically
  dfp <- dfp[order(dfp$epid, dfp$dt_a, dfp$dt_z),]

  # event labels - Event/period date(s), case_nm and referent event per window
  # Same day periods are shown as time points to save spac
  if(is.null(date)){
    dfp$event_nm <- ifelse(dfp$case_nm!="Skipped",
                           paste0("SN-", format(dfp$sn, scientific=F), "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$wind_id), "\n(reference)", ""), "\nevent"),
                           paste0("SN-", format(dfp$sn, scientific=F), "\n", dfp$case_nm))
  }else{
    dfp$event_nm <- ifelse(dfp$case_nm!="Skipped",
                           paste0(ifelse(dfp$dt_z-dfp$dt_a==0, format(dfp$dt_a), format(dfp$date)), "\n", dfp$case_nm, ifelse(dfp$sn %in% unique(dfp$wind_id), "\n(reference)", ""), "\nevent"),
                           paste0(ifelse(dfp$dt_z-dfp$dt_a==0, format(dfp$dt_a), format(dfp$date)), "\n", dfp$case_nm)
                           )
  }


  # lengths
  if(is.null(case_length)){
    dfp$c <- 0
  }else{
    dfp$c <- case_length
  }

  if(is.null(recurrence_length)){
    dfp$r <- 0
  }else{
    dfp$r <- recurrence_length
  }

  # Separate plots per strata
  if(is.null(strata)){
    dfp$strata <- 1
  }else{
    dfp$strata <- strata
  }

  story <- function(dfp){
    #x-axis limits
    # min and dates plus & minus lengths
    xlims <- c((min(dfp$dt_a) - max(c(max(dfp$c), max(dfp$r)))), (max(dfp$dt_z) + max(c(max(dfp$c), max(dfp$r)))))

    # Colors groups for windows
    dfp$win_cols <- rev(pl_cols)[as.numeric(as.factor(dfp$wind_id))]
    cols <- dfp$win_cols
    names(cols) <- dfp$wind_id
    cols <- cols[!duplicated(cols)]

    # Colors groups for episodes.
    # Linked corresponding window colors
    dfp$epd_cols <- cols[as.character(dfp$epid)]

    # Y-axis center
    # Everthing else is relative to this
    e_y <- 1

    # Space out (along Y-axis) overlapping event labels
    # Order the longer periods above shorter ones
    # Event mid-pont. Where labels will be plotted

    # To save space, only one event is shown among same day duplicates
    # Below is the preference for which to show
    dfp$ord <- ifelse(dfp$case_nm=="Skipped",1,5)
    dfp$ord <- ifelse(dfp$case_nm=="Case",2,dfp$ord)
    dfp$ord <- ifelse(dfp$case_nm=="Recurrent",3,dfp$ord)
    dfp$ord <- ifelse(dfp$case_nm=="Duplicate",4,dfp$ord)
    dfp$cri <- paste0(dfp$dt_a, dfp$dt_z)
    dfp <- dfp[order(dfp$cri, dfp$ord),]
    dfp$event_nm <- ifelse(duplicated(dfp$cri), "", dfp$event_nm)

    dfp$dt_c <- (as.numeric(dfp$dt_a) + as.numeric(dfp$dt_z)) * .5
    dfp$event_length <- diyar::number_line(dfp$dt_a, dfp$dt_z)@.Data
    dfp$p_ord <- order(-dfp$event_length, dfp$ord)

    mid_pts <- dfp$dt_c
    names(mid_pts) <- 1:length(mid_pts)
    mid_pts <- mid_pts[dfp$event_nm!=""]
    p_ord <- dfp$p_ord[dfp$event_nm!=""]

    # Check mid-points that are too close (0.04) as this will overlap in the plot.
    chck <- diyar::compress_number_line(diyar::expand_number_line(as.number_line(mid_pts), 1), deduplicate = F, collapse = T)
    # Per overlaping group, order events based on size/length of period
    ord <- lapply(split(p_ord,  chck@gid), function(x){
      order(-x)
    })
    ord <- unsplit(ord, chck@gid)
    names(ord) <- names(mid_pts)
    ord <- ord[as.character(1:nrow(dfp))]
    ord <- ifelse(is.na(ord), 1, ord)

    # All events centered
    dfp$e_y <- e_y
    # Among overlapping events/period, space out the 2nd/more event incrementally (0.17)
    dfp$e_y[ord>1] <- max(dfp$e_y) + ((ord[ord>1]-1) * 0.25 * scale_fac)
    # Some number_lines may overlap by chance so, space out again incrementally (0.01)
    dfp$e_y <- dfp$e_y  - (1:nrow(dfp) * (scale_fac * 0.02))
    dfp$xrd <- ord

    # recurrence_length is supplied, strip out the reference events for recurrence periods
    if(!is.null(recurrence_length)){
      dfp$rec_len_y_axis <- min(dfp$e_y) - (scale_fac * 0.2)
      rl <- dfp[dfp$sn %in% unique(dfp$wind_id[dfp$wind_nm!="Case" & dfp$case_nm!="Skipped"]),]
      rl$e_dt_a <- as.numeric(lapply(split(rl$dt_a, rl$wind_id), min)[as.character(rl$wind_id)])
      rl$e_dt_z <- as.numeric(lapply(split(rl$dt_z, rl$wind_id), max)[as.character(rl$wind_id)])
      # Space out their position on Y-axis
      rl$rec_len_y_axis <- rl$rec_len_y_axis - (1:nrow(rl) * (scale_fac * 0.03))

    }else{
      dfp$rec_len_y_axis <- min(dfp$e_y)
      rl <- data.frame()
    }

    # Windows
    dfp$windows_y_axis <- rep((min(dfp$rec_len_y_axis) - (scale_fac * 0.15)), nrow(dfp))
    # Space out their position on Y-axis
    dfp$windows_y_axis <- dfp$windows_y_axis - (scale_fac * 0.04 * (as.numeric(as.factor(dfp$wind_id))-1))

    # Episodes
    dfp$episode_y_axis <- min(dfp$windows_y_axis) - (scale_fac * 0.25)
    # Space out their position on Y-axis
    dfp$episode_y_axis <- dfp$episode_y_axis - (scale_fac * 0.04 * (as.numeric(as.factor(dfp$epid))-1))

    # Case_lengths
    if(!is.null(case_length)){
      dfp$case_len_y_axis <- max(dfp$e_y) + (scale_fac * 0.35)
    }else{
      dfp$case_len_y_axis <- max(dfp$e_y)
    }

    dfp <- dfp[order(dfp$epid, dfp$dt_a, dfp$dt_z),]

    # See if there's an alternative.
    #dev.new()
    graphics::par(bg="black")
    graphics::par(mar=rep(0,4))
    graphics::plot(y=dfp$e_y, x=dfp$dt_a, ylim=c(min(dfp$episode_y_axis) - 0.1, max(dfp$case_len_y_axis) + 0.2), xlim = xlims)

    # ------------------------------
    for(i in 1:nrow(dfp)){
      # Events/periods
      graphics::points(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), bg = dfp$win_cols[i], col = dfp$win_cols[i], pch = 21)
      # left_point() tracer
      graphics::lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$windows_y_axis[i]), x = rep(dfp$dt_a[i],2), col = dfp$win_cols[i], lty = 2)

      if(dfp$c[i]<0 & dfp$wind_id[i] == dfp$sn[i]){
        if(dfp$dt_a[i] < dfp$dt_z[i] + dfp$c[i]){
          # Period "completely changed" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i] + dfp$c[i]), col = dfp$win_cols[i], lty = 1)
          # The period "cancelled out" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_z[i] + dfp$c[i], dfp$dt_z[i]), col = "white", lty = 2)
        }else{
          # Period "shortened" due to the negative case_length
          graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i] + dfp$c[i]), col = "white", lty = 2)
        }
      }else{
        # Period "shortened" due to the negative case_length
        graphics::lines(y=rep(dfp$e_y[i],2), x = c(dfp$dt_a[i], dfp$dt_z[i]), col = dfp$win_cols[i], lty = 1)
        # right_point() tracer
        graphics::lines(y=c(dfp$e_y[i] - (scale_fac * 0.06), dfp$windows_y_axis[i]), x = rep(dfp$dt_z[i],2), col = dfp$win_cols[i], lty = 2)
      }
      graphics::text(cex = .7 * scale_fac, y=dfp$e_y[i] + (scale_fac * 0.05), x=mean(c(dfp$dt_a[i], dfp$dt_z[i])), labels = dfp$event_nm[i], adj =c(.5,0), col = dfp$win_cols[i])
    }

    # case lengths
    cl <- dfp[dfp$sn %in% unique(dfp$wind_id[dfp$wind_nm=="Case"]),]
    if(nrow(cl)>0 & !is.null(case_length)){
      # episode start and end dates
      # re-calculated because it may not be supplied
      cl$e_dt_a <- as.numeric(lapply(split(cl$dt_a, cl$epid), min)[as.character(cl$epid)])
      cl$e_dt_z <- as.numeric(lapply(split(cl$dt_z, cl$epid), max)[as.character(cl$epid)])

      cl$dt <- as.numeric(cl$dt_z)
      cl$end_dt <- as.numeric(cl$dt + ifelse(from_last==F, cl$c, -cl$c))

      cl$dt2 <- as.numeric(cl$dt_a)
      cl$end_dt2 <- as.numeric(cl$dt2 - ifelse(from_last==F, cl$c, -cl$c))

      # spacing
      dfp$case_len_y_axis <- dfp$case_len_y_axis + (0.02 * ((1:nrow(dfp))-1))
      cl$lab <- paste0("Case length\n(",cl$c,"-day\ndifference)")

      for(i in 1:nrow(cl)){
        # Surpressed warning from 0 length arrows
        suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=cl$case_len_y_axis[i], x0 = cl$dt[i], x1 = cl$end_dt[i], col ="white"))
        graphics::text(cex = .7 * scale_fac, y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(cl$dt[i], cl$end_dt[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))
        graphics::lines(y=c(cl$case_len_y_axis[i] - (scale_fac * 0.015), cl$case_len_y_axis[i] + (scale_fac * 0.015)), x = rep(cl$dt[i],2), col=cl$win_col[i])

        # Trying to guess when bi_direction has been used.
        # Doesn't capture all scenarios yet.
          # Stopped for now. Need's another approach
        if(cl$dt2[i]!= cl$e_dt_a[i]){
          # Surpressed warning from 0 length arrows
          # suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=cl$case_len_y_axis[i], x0 = cl$dt2[i], x1 = cl$end_dt2[i], col ="white"))
          # graphics::text(cex = .7 * scale_fac, y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(cl$dt2[i], cl$end_dt2[i])), labels = cl$lab[i], col ="white", adj =c(.5,0))
        }
      }
    }



    # Recurrence lengths
    if(nrow(rl)>0 & !is.null(recurrence_length)){
      rl$dt <- as.numeric(rl$dt_z)
      rl$end_dt <- as.numeric(rl$dt + ifelse(from_last==F, rl$r, -rl$r))

      rl$dt2 <- as.numeric(rl$dt_a)
      rl$end_dt2 <- as.numeric(rl$dt2 - ifelse(from_last==F, rl$r, -rl$r))
      rl$lab <- paste0("Recurrence length\n(",rl$c,"-day\ndifference)")

      for(i in 1:nrow(rl)){
        # Surpressed warning from 0 length arrows
        suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=rl$rec_len_y_axis[i], x0 = rl$dt[i], x1 = rl$end_dt[i], lty=2, col ="white"))
        graphics::text(cex = .7 * scale_fac, y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(rl$dt[i], rl$end_dt[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
        graphics::lines(y=c(rl$rec_len_y_axis[i] - (scale_fac * 0.015), rl$rec_len_y_axis[i] + (scale_fac * 0.015)), x = rep(rl$dt[i],2), col=rl$win_col[i])
      }

      # Trying to guess when bi_direction has been used.
      # Doesn't capture all scenarios yet.
      # Should it apply to recurrence length?
        # Stopped for now. Need's another approach
      if(rl$dt2[i]!= rl$e_dt_a[i]){
        # Surpressed warning from 0 length arrows
        # suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=rl$rec_len_y_axis[i], x0 = rl$dt2[i], x1 = rl$end_dt2[i], col ="white"))
        # graphics::text(cex = .7 * scale_fac, y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x=mean(c(rl$dt2[i], rl$end_dt2[i])), labels = rl$lab[i], col ="white", adj =c(.5,0))
      }
    }

    # Windows
    win <- dfp

    win$w_dt_z <- as.numeric(lapply(split(win$dt_z, win$wind_id), ifelse(from_last==F, max, min))[as.character(win$wind_id)])
    win <- win[!duplicated(win$wind_id),]
    dt_as <- as.numeric(dfp$dt_a)
    names(dt_as) <- dfp$sn
    win$w_dt_a <- dt_as[as.character(win$wind_id)]

    # window ord as label
      win$wind_nm_l <- paste0("Window ", as.numeric(as.factor(win$wind_id)), "\n(", tolower(win$wind_nm), "\nwindow)")
    # window ID as label
      win$wind_nm_l <- paste0("Window ID: ", win$wind_id, ifelse(win$wind_nm=="Skipped","", paste0("\n(", tolower(win$wind_nm), "\nwindow)")))

    for(i in 1:nrow(win)){
      graphics::lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.02)), x = rep(win$w_dt_a[i],2), col=win$win_col[i])
      graphics::lines(y=c(win$windows_y_axis[i], win$windows_y_axis[i] + (scale_fac * 0.02)), x = rep(win$w_dt_z[i],2), col=win$win_col[i])

      if(win$case_nm[i]!="Skipped"){
        graphics::text(cex = .7 * scale_fac, y=win$windows_y_axis[i] - (scale_fac * 0.2), x= mean(c(win$w_dt_a[i], win$w_dt_z[i])), labels = win$wind_nm_l[i], col=win$win_col[i], adj =c(.5,0))
        graphics::lines(y=rep(win$windows_y_axis[i],2), x = c(win$w_dt_a[i], win$w_dt_z[i]), col=win$win_col[i])

        if(win$w_dt_a[i]==win$w_dt_z[i]){
          graphics::lines(y=rep(win$windows_y_axis[i],2), x = c(win$w_dt_a[i]-.2, win$w_dt_z[i]+.2), col=win$win_col[i])
        }
      }

      graphics::lines(y=c(win$windows_y_axis[i] - (scale_fac * 0.23), win$episode_y_axis[i]), x = rep(mean(c(win$w_dt_a[i], win$w_dt_z[i])), 2), col=win$epd_col[i])
      graphics::lines(y=c(win$windows_y_axis[i] - (scale_fac * 0.13), win$windows_y_axis[i] - (scale_fac * (0))), x = rep(mean(c(win$w_dt_a[i], win$w_dt_z[i])), 2), col=win$epd_col[i])
    }

    # Episodes
    epd <- dfp
    epd$e_dt_a <- as.numeric(lapply(split(epd$dt_a, epd$epid), min)[as.character(epd$epid)])
    epd$e_dt_z <- as.numeric(lapply(split(epd$dt_z, epd$epid), max)[as.character(epd$epid)])
    epd <- epd[!duplicated(epd$epid),]

      # episode ord as label
    epd$wind_nm_l <- paste0("Episode ", as.numeric(as.factor(epd$epid)))
      # episode ID as label
    epd$wind_nm_l <- paste0("Episode ID: ", epd$epid)

    for(i in 1:nrow(epd)){
      graphics::lines(y=rep(epd$episode_y_axis[i],2), x = c(epd$e_dt_a[i], epd$e_dt_z[i]), col=epd$epd_cols[i])
      graphics::lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.02)), x = rep(epd$e_dt_a[i],2), col=epd$epd_cols[i])
      graphics::lines(y=c(epd$episode_y_axis[i], epd$episode_y_axis[i] + (scale_fac * 0.02)), x = rep(epd$e_dt_z[i],2), col=epd$epd_cols[i])

      graphics::text(cex = .7 * scale_fac, y=epd$episode_y_axis[i] - (scale_fac * 0.08), x=mean(c(epd$e_dt_a[i], epd$e_dt_z[i])), labels = epd$wind_nm_l[i], col=epd$epd_cols[i], adj =c(.5,0))

      if(epd$e_dt_a[i]==epd$e_dt_z[i]){
        graphics::lines(y=rep(epd$episode_y_axis[i],2), x = c(epd$e_dt_a[i]-.2, epd$e_dt_z[i]+.2), col=epd$win_col[i])
      }
    }

    plt <- grDevices::recordPlot()
    # graphics.off()
    # dev.off()
    return(plt)
  }

  p <- lapply(unique(dfp$strata), function(x){
    story(dfp[dfp$strata==x,])
  })

  names(p) <- unique(dfp$strata)
  if(length(p)==1) p <- p[[1]]
  return(p)
}
