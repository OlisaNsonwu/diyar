#' @name episode_group
#' @title Episode grouping for case definitions and record deduplication
#'
#' @description Group events into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique numerical record identifier. Optional.
#' @param strata Subsets of the dataset. Episode grouping will be done separately within each subset of the dataset. In \code{\link{episode_group}}, you can use multiple columns. \code{\link{record_group}} can create useful \code{strata} e.g. patient identifiers.
#' @param date Date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}) of events.
#' @param case_length Duration after a \code{"case"} within which subsequent events are considered \code{"duplicate"} events. This period is referred to as the the \code{case window}.
#' @param episodes_max Maximum number of episodes to have within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Duration after the last or first event (see \code{recurrence_from_last}) of the previous window within which subsequent events are considered \code{"recurrent"} events. This period is referred to as the \code{recurrence window}. If \code{recurrence_length} is not supplied, \code{case_length} is used as the \code{recurrence_length}.
#' @param episode_unit Time units. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max Maximum number of times an episode can reoccur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset identifier. Useful when the dataset contains data from multiple sources. In \code{\link{episode_group}}, you can use multiple columns supplied as column names.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent event and proceeding to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest event and proceeding to the most recent one.
#' @param overlap_method Methods of overlap considered when grouping event periods. Each pair of periods are checked with the same set of \code{overlap_method}. Deprecated use \code{overlap_methods} instead.
#' @param overlap_methods Methods of overlap considered when grouping event periods. Different pairs of periods can be checked with different sets \code{overlap_methods}
#' @param custom_sort  Preferential order for \code{"case"} assignment. Useful in specifying that episode grouping begins at particular events regardless of chronological order. In \code{\link{episode_group}}, you can use multiple columns as sort levels.
#' @param bi_direction If \code{FALSE}, \code{"duplicate"} events will be those within the \code{case_length} before \strong{or} after the \code{"case"} as determined by \code{from_last}. If \code{TRUE}, \code{"duplicate"} events will be those within the same period before \strong{and} after the \code{"case"}.
#' @param group_stats If \code{TRUE}, the output will include additional information with useful stats for each episode group.
#' @param display If \code{TRUE}, a progress message is printed on screen.
#' @param to_s4 if \code{TRUE}, changes the returned output to an \code{\link[=epid-class]{epid}} object.
#' @param recurrence_from_last if \code{TRUE}, the reference event for a \code{recurrence window} will be the last event from the previous window. If \code{FALSE} (default), it will be from the first event. Only used if \code{episode_type} is \code{"rolling"}.
#' @param case_for_recurrence if \code{TRUE}, both case and recurrence events will have a \code{case window}. If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"}.
#' @param skip_order Skip episodes whose \code{case} events have \code{custom_sort} values that are less than or equal to the \code{"nth"} order of \code{custom_sort}. Useful in skipping episodes that are not required and so minimises the overall processing time. Ignored if \code{custom_sort} is \code{NULL}.
#' @param data_links Ungroup episodes that will not include records from certain \code{data_sources}. \code{data_links} should be a \code{list} with every element named 'l' (links) or 'g' (groups). Useful in skipping episodes that are not required and so minimises the overall processing time. Ignored if \code{data_source} is \code{NULL}.
#' @return
#'
#' @return \code{\link[=epid-class]{epid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided (or generated)
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
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlap}} and \code{\link{number_line}}
#'
#' @details
#' Episode grouping begins at a reference event (\code{"case"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain events before proceeding forward or backwards in time.
#' The maximum duration of a \code{"fixed"} episode is the \code{case_length}. This period is referred to as the \code{case window}. The maximum duration of a \code{"rolling"} episode is the
#' \code{case_length} plus all recurrence periods. The recurrence periods are referred to as \code{recurrence windows}.
#' This is a specified duration (\code{recurrence_length}) after the last or first (depending on \code{recurrence_from_last}) event from the previous window.
#' Events within this period are considered \code{"recurrent"} events.
#'
#' When a \code{data_source} identifier is provided,
#' \code{epid_dataset} is included in the output. This lists the source of every event in each episode.
#'
#' @examples
#' library(diyar)
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
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL, data_links = "ANY",
                          custom_sort = NULL, skip_order =NULL, from_last=FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"),
                          overlap_methods = NULL, bi_direction = FALSE,
                          group_stats= FALSE, display=TRUE, deduplicate=FALSE, to_s4 = TRUE, recurrence_from_last = TRUE, case_for_recurrence =FALSE){
  . <- NULL

  if(missing(df)) stop("argument 'df' is missing, with no default")
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
  sk_od <- enq_vr(substitute(skip_order))
  dt <- enq_vr(substitute(date))
  methods <- enq_vr(substitute(overlap_methods))

  # Check that col names exist
  if(any(!unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)), !unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt, methods, sk_od)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found"))
  }

  if(!(any(class(df[[epl]]) %in% c("integer","double","numeric"))))  stop(paste("'case_length' must be integer or numeric values", sep=""))
  if(!is.null(r_epl)){
    if(!(any(class(df[[r_epl]]) %in% c("integer","double","numeric")))) stop(paste("'recurrence_length' must be integer or numeric values", sep=""))
  }
  if(!(any(class(df[[dt]]) %in% c("Date","POSIXct","POSIXt","POSIXlt","number_line","numeric","integer","Interval")))) stop("'date' must be a date, datetime, numeric or number_line object")

  if(!is.null(sk_od)){
    if(!(any(class(df[[sk_od]]) %in% c("integer","double","numeric"))))  stop(paste("'skip_order' must be a positive integer or numeric value", sep=""))
    if(!(all(class(df[[sk_od]]) >0)))  stop(paste("'skip_order' must be a positive integer or numeric value", sep=""))
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
    T1$dsvr <- "A"
  }else{
    T1$dsvr <- eval(parse(text = paste0("paste0(",paste0("df$", ds, collapse = ",'-',"),")")))
  }

  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- T1$dsvr[!duplicated(T1$dsvr)]
  ms_lst <- unique(dl_lst[!dl_lst %in% ds_lst])

  if(length(ms_lst)>0 & !all(toupper(dl_lst)=="ANY")) stop(paste("",
                                                                 paste0("Values - ", paste0("'",ms_lst,"'",collapse = ","), " not found in `datasource`."),
                                                                 "Have you used levels for `datasource` - i.e. episode_group(... datasource = c(vr1, vr2, vr3)) ?",
                                                                 "",
                                                                 "If so, include the value for each level.",
                                                                 "",
                                                                 "Examples",
                                                                 "`data_links` <- list(l = c('ds1-ds2', 'ds3')",
                                                                 "                     g = c('ds1-ds2', 'ds3')",
                                                                 "",
                                                                 "`data_links` <- c('ds1-ds2', 'ds3')",
                                                                 "",
                                                                 "'l' - for groups with records from the same vr1 and vr2 `data_source(s)` ('ds1-ds2') AND vr3 `data_source` ('ds3')",
                                                                 "'g' - for groups with records from the same vr1 and vr2 `data_source(s)` ('ds1-ds2') OR  vr3 `data_source` ('ds3')", sep = "\n"))

  if(!is.list(data_links)) data_links <- list(l = data_links)
  if(is.null(names(data_links)))  names(data_links) <- rep("l", length(data_links))
  if(!all(names(data_links) %in% c("g", "l"))) stop(paste("",
                                                          "`data_links` should be a `list` with every element named 'l' (links) or 'g' (groups)",
                                                          "'l' (link) is used for unamed elements or atomic vectors",
                                                          "",
                                                          " Examples",
                                                          "`data_links` <- list(l = c('DS1', 'DS2')",
                                                          "                     g = c('DS3', 'DS4')",
                                                          "",
                                                          "`data_links` <- c('DS1', 'DS2')",
                                                          "",
                                                          "'l' - for groups with records from 'DS1' AND 'DS2' `data_source(s)`",
                                                          "'g' - for groups with records from 'DS3' OR  'DS3' `data_source(s)", sep = "\n"))

  # lengths
  T1$epi_len <- df[[epl]]

  if(is.null(r_epl) | episode_type !="rolling" ){
    T1$rc_len <- df[[epl]]
  }else{
    T1$rc_len <- df[[r_epl]]
  }

  # Strata
  if(is.null(st)){
    T1$cri <- "A"
  }else{
    T1$cri <- eval(parse(text = paste0("paste0(",paste0("df$", st, collapse = ",'-',"),")")))
  }

  # Date
  if(any(class(df[[dt]]) %in% c("number_line","Interval"))){
    T1$rec_dt_ai <- diyar::left_point(df[[dt]])
    T1$rec_dt_zi <- diyar::right_point(df[[dt]])
  }else{
    T1$rec_dt_ai <- df[[dt]]
    T1$rec_dt_zi <- df[[dt]]
  }

  fn_check <- finite_check(T1$rec_dt_zi)
  if(fn_check!=T) stop(paste0("Finite 'date' values required in ",fn_check))

  fn_check <- finite_check(T1$epi_len)
  if(fn_check!=T) stop(paste0("Finite 'case_length' values required in ",fn_check))

  fn_check <- finite_check(T1$rc_len)
  if(fn_check!=T) stop(paste0("Finite 'recurrence_length' values required in ",fn_check))

  # Class of 'date'
  dt_grp <- ifelse(!any(class(T1$rec_dt_ai) %in% c("Date","POSIXct","POSIXt","POSIXlt")) |
                     !any(class(T1$rec_dt_zi) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)

  episode_unit <- ifelse(dt_grp==F,"seconds", episode_unit)

  if(dt_grp==T){
    T1$rec_dt_ai <- as.POSIXct(format(T1$rec_dt_ai, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
    T1$rec_dt_zi <- as.POSIXct(format(T1$rec_dt_zi, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
  }else{
    T1$rec_dt_ai <- as.numeric(T1$rec_dt_ai)
    T1$rec_dt_zi <- as.numeric(T1$rec_dt_zi)
  }

  # Overlap methods
  if(missing(overlap_methods) & !missing(overlap_method)) {
    T1$methods <- paste0(overlap_method, collapse = "|")
    warning("'overlap_method' is deprecated. Please use 'overlap_method' instead.")
  }else{
    if(is.null(methods)){
      T1$methods <- "exact|across|chain|aligns_start|aligns_end|inbetween"
    }else {
      T1$methods <- df[[methods]]
    }
  }

  o <- unique(unlist(strsplit(T1$methods[!duplicated(T1$methods)], split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween")]
  if (length(o)>0) stop(paste0("\n",
                               paste0("'",o,"'", collapse = " ,"), " is not a valid overlap method \n\n",
                               "Valid 'overlap_methods' are 'exact', 'across', 'chain', 'aligns_start', 'aligns_end' or 'inbetween' \n\n",
                               "Syntax ~ \"method1|method2|method3...\" \n",
                               "                 OR                   \n",
                               "Use ~ include_overlap_method() or exclude_overlap_method()"))

  #df <- df[c("sn","rec_dt_ai","rec_dt_zi","epi_len","rc_len","dsvr","cri",ref_sort, sk_od, "methods")]

  T1$dist_from_epid <- T1$dist_from_wind <- T1$wind_id <- T1$tag <- T1$roll <- T1$episodes <- 0
  T1$wind_nm <- ""
  T1$epid <- sn_ref <- min(T1$sn)-1
  T1$case_nm <- ""
  T1$pr_sn = 1:nrow(df)

  # Chronological order
  if(from_last==T){
    T1$ord <- abs(max(T1$rec_dt_ai) - T1$rec_dt_ai)
    T1$ord_z <- abs(max(T1$rec_dt_zi) - T1$rec_dt_zi)
  }else{
    T1$ord <- abs(min(T1$rec_dt_ai) - T1$rec_dt_ai)
    T1$ord_z <- abs(min(T1$rec_dt_zi) - T1$rec_dt_zi)
  }

  # Custom sort
  if(!is.null(ref_sort)) {
    user_ord <- eval(parse(text = paste0("order(",paste0("df$", ref_sort, collapse = ", "),", T1$ord, -T1$ord_z)")))
  }else{
    user_ord <- order(T1$ord, -T1$ord_z)
  }

  names(user_ord) <- 1:length(user_ord)
  T1$user_ord <- as.numeric(names(sort(user_ord)))

  # c_sort ord
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

  # skip_order
  if(!is.null(sk_od)){
    T1$skip_order <- df[[sk_od]]
  }else{
    T1$skip_order <- Inf
  }

  # Skip from episode grouping
  T1$epid <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , T1$sn, T1$epid)
  T1$wind_id <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , T1$sn, T1$wind_id)
  T1$wind_nm <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , "Skipped", T1$wind_nm)

  T1$dist_from_epid[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <-
    T1$dist_from_wind[T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "")] <- 0

  T1$tag <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 2, T1$tag)
  T1$case_nm <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), "Skipped", T1$case_nm)
  T1$episodes <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 1, 0)
  T1$roll <- ifelse(T1$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), rolls_max, T1$roll)

  if(!is.null(ds) & !all(toupper(dl_lst) == "ANY")){
    # check type of links
    links_check <- function(x, y, e) {
      if(tolower(e)=="l"){
        all(y %in% x & length(x)>1)
      }else if (tolower(e)=="g"){
        any(y %in% x)
      }
    }

    pds <- lapply(split(T1$dsvr, T1$cri), function(x, l=data_links){
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
  }

  min_tag <- min(T1$tag)
  min_episodes <- min(T1$episodes)

  rm(df)
  c <- 1
  grouped_epids <- T1[0,0]
  g_vrs <- c("sn","pr_sn","rec_dt_ai","rec_dt_zi","dsvr","epid","wind_id","wind_nm","case_nm","skip_order", "c_sort", "user_ord", "ord","ord_z", "dist_from_wind", "dist_from_epid")
  while (min_tag != 2 & min_episodes <= episodes_max){
    # seperate out grouped records
    grouped_epids <- rbind(grouped_epids,
                         T1[T1$tag ==2 & !is.na(T1$tag), g_vrs])
      # drop them from the main dataset
    T1 <- T1[T1$tag !=2 & !is.na(T1$tag),]

    # check for records to skip - `skip_order` and `episode_max`
    TR <- dplyr::arrange(T1, T1$cri, -T1$tag, T1$user_ord, T1$sn)
    skip_cris <- unique(TR$cri[(TR$c_sort > TR$skip_order |
                                 TR$tag==0 & TR$episodes + 1 > episodes_max) & !duplicated(TR$cri) & !is.na(TR$cri)])

    # assign unique IDs to skipped records
    T1$tag[T1$cri %in% skip_cris] <- 2
    T1$wind_nm[T1$cri %in% skip_cris] <- T1$case_nm[T1$cri %in% skip_cris] <- "Skipped"
    T1$epid[T1$cri %in% skip_cris] <- T1$wind_id[T1$cri %in% skip_cris] <- T1$sn[T1$cri %in% skip_cris]
    T1$dist_from_epid[T1$cri %in% skip_cris] <- T1$dist_from_wind[T1$cri %in% skip_cris] <- 0
    # seperate out skipped records
    grouped_epids <- rbind(grouped_epids,
                           T1[T1$tag ==2 & !is.na(T1$tag), g_vrs])

      # drop them from the main dataset
    T1 <- T1[T1$tag !=2 & !is.na(T1$tag),]

    # Reference events
    TR <- TR[!(TR$tag==0 & TR$episodes + 1 > episodes_max) &
               !(TR$c_sort > TR$skip_order) &
               !duplicated(TR$cri) &
               !is.na(TR$cri),
             c("sn", "cri", "rec_dt_ai", "rec_dt_zi","epid", "tag", "roll", "epi_len", "rc_len", "case_nm")]
    names(TR) <- paste0("tr_",names(TR))

    # early break if there are no more reference events
    if(nrow(TR)==0) {break}

    if(display){cat(paste("Episode or recurrence window ",c,".\n", sep=""))}

    #T1 <- merge(T1, TR, by.x="cri", by.y="tr_cri", all.x=T)
    T1 <- dplyr::left_join(T1, TR, by= c("cri"="tr_cri"))

    T1$lr <- ifelse(T1$tr_sn == T1$sn & !is.na(T1$tr_sn),1,0)

    # Case and recurrence lengths
    T1$c_int <- diyar::number_line(T1$rec_dt_ai, T1$rec_dt_zi)
    T1$r_int <- diyar::number_line(T1$rec_dt_ai, T1$rec_dt_zi)

    # Case and recurrence lengths of reference events
    T1$tr_rc_len <- T1$tr_rc_len * diyar::episode_unit[[episode_unit]]
    T1$tr_epi_len <- T1$tr_epi_len * diyar::episode_unit[[episode_unit]]

    T1$tr_c_int <- suppressWarnings(diyar::number_line(T1$tr_rec_dt_ai, T1$tr_rec_dt_zi))
    T1$tr_r_int <- suppressWarnings(diyar::number_line(T1$tr_rec_dt_ai, T1$tr_rec_dt_zi))

    bdir <- ifelse(bi_direction,"both","end")
    if (from_last==F){
      T1$tr_c_int <-  suppressWarnings(diyar::expand_number_line(T1$tr_c_int, T1$tr_epi_len, bdir))
      T1$tr_r_int <-  suppressWarnings(diyar::expand_number_line(T1$tr_r_int, T1$tr_rc_len, bdir))
    }else{
      T1$tr_c_int <-  suppressWarnings(diyar::expand_number_line(T1$tr_c_int, -T1$tr_epi_len, bdir))
      T1$tr_r_int <-  suppressWarnings(diyar::expand_number_line(T1$tr_r_int, -T1$tr_rc_len, bdir))
    }

    # Check if events overlap
    T1$r_range <- T1$c_range <- F

    T1$c_range <- diyar::overlap(T1$c_int, T1$tr_c_int, methods = T1$methods)
    T1$r_range <- diyar::overlap(T1$r_int, T1$tr_r_int, methods = T1$methods)

    # distance from window's ref event
    T1$dist_from_wind <- ((as.numeric(T1$rec_dt_ai) + as.numeric(T1$rec_dt_zi)) *.5) - ((as.numeric(T1$tr_rec_dt_ai) + as.numeric(T1$tr_rec_dt_zi)) *.5)
    # distance from episodes's ref event
    T1$dist_from_epid <- ifelse(T1$tr_case_nm=="", ((as.numeric(T1$rec_dt_ai) + as.numeric(T1$rec_dt_zi)) *.5) - ((as.numeric(T1$tr_rec_dt_ai) + as.numeric(T1$tr_rec_dt_zi)) *.5), T1$dist_from_epid)

    if(!bi_direction & !from_last){
      T1$c_range <- ifelse(T1$tr_rec_dt_ai > T1$rec_dt_ai & T1$tr_epi_len >=0, FALSE, T1$c_range)
      T1$r_range <- ifelse(T1$tr_rec_dt_ai > T1$rec_dt_ai & T1$tr_rc_len >=0, FALSE, T1$r_range)

    }else if(!bi_direction & from_last){
      T1$c_range <- ifelse(T1$tr_rec_dt_ai < T1$rec_dt_ai & T1$tr_epi_len >=0, FALSE, T1$c_range)
      T1$r_range <- ifelse(T1$tr_rec_dt_ai < T1$rec_dt_ai & T1$tr_rc_len >=0, FALSE, T1$r_range)
    }

    # event type
    # 1 - Reference event (Case)
    # 2 - Duplicate of a case event
    # 6 - Recurrent event
    # 7 - Duplicate of a recurrent event
    # 9 - Reference event for a case window. - 1 & 9 are case windows
    # 10 - Duplicate event for a case window. - 2 & 10 are duplicates in case windows
    # Episode assignment -------
    T1$epid_type <- ifelse(
      T1$r_range == T & !is.na(T1$r_range) & T1$tr_tag %in% c(1,1.5,0),
      ifelse(T1$case_nm=="Duplicate", 7,6), 0
    )

    T1$epid_type <- ifelse(
      T1$c_range == T & !is.na(T1$c_range) &
        ((T1$tr_tag==0 & !is.na(T1$tr_tag)) | (T1$tr_tag %in% c(1.4,1.6) & !is.na(T1$tr_tag))),
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

    T1$wind_id <- ifelse(T1$c_hit==1 & (T1$lr !=1 | (T1$lr ==1 & T1$tr_case_nm=="")), T1$tr_sn, T1$wind_id)
    T1$wind_nm <- ifelse((T1$epid_type %in% c(1,2,9,10) | (T1$epid_type==0 & T1$lr==1))  & T1$wind_nm=="", "Case", ifelse(T1$epid_type!=0 & T1$wind_nm=="", "Recurrence", T1$wind_nm))

    T1$case_nm <- ifelse(
      T1$c_hit==1 & !T1$tag %in% c(1.4,1.6),
      ifelse(T1$epid_type %in% c(1,0), "Case",
             ifelse(T1$epid_type==6 & episode_type=="rolling","Recurrent","Duplicate")),
      T1$case_nm
    )
    # ---------

    vrs <- names(T1)[!grepl("_range|_int", names(T1))]
    # T1 <- T1[order(T1$cri, T1$epid, T1$user_ord, T1$sn), vrs]
    T1 <- dplyr::arrange(T1, T1$cri, T1$epid, T1$user_ord, T1$sn)

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
      total_1 <- nrow(T1)

      if(display){
        cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
      }
      break
    }

    if (episode_type=="rolling"){
      T1$d_grp <- ifelse(T1$case_nm=="Duplicate" & T1$sn != T1$tr_sn & !is.na(T1$tr_sn), 1, 0)

      pds2 <- lapply(split(T1$d_grp, T1$epid), max)
      T1$d_grp <- as.numeric(pds2[as.character(T1$epid)])

      T1$d_ord <- ifelse(T1$case_nm=="Duplicate" & T1$lr !=1, T1$user_ord, NA)
      pds2 <- lapply(split(T1$d_ord, T1$epid), function(x){
        suppressWarnings(min(x, na.rm=T))
      })
      T1$d_ord <- as.numeric(pds2[as.character(T1$epid)])
    }else{
      T1$d_ord <- T1$d_grp <- 0
    }

    # Number of recurrence periods so far
    T1$roll <- ifelse((T1$tr_case_nm == "Recurrent" & !is.na(T1$tr_case_nm)) |
                        (T1$tr_tag== 1.5 & !is.na(T1$tr_tag)),T1$tr_roll + 1,  T1$roll)

    # Chose the next reference event
    T1$mrk_z <- paste0(T1$case_nm, T1$epid)
    T1$tag <- ifelse(episode_type == "rolling" &
                       (T1$roll < rolls_max |(case_for_recurrence==T & T1$roll < rolls_max+1 & T1$tr_tag != 1.6) )&
                       !(T1$lr==1 & T1$tr_tag %in% c(1, 1.5, 1.4, 1.6)) & T1$case_nm %in% c("Duplicate","Recurrent"),
                     ifelse(grepl("^Duplicate",T1$case_nm),
                            ifelse(T1$case_nm =="Duplicate" & !duplicated(T1$mrk_z, fromLast = T) &
                                     recurrence_from_last == T,
                                   ifelse(case_for_recurrence==T & T1$tr_tag==1.5, 1.6,1.5), 2),
                            ifelse(T1$case_nm=="Recurrent" &
                                     T1$d_grp ==1  &
                                     T1$user_ord < T1$d_ord & T1$d_ord != Inf &
                                     T1$tr_case_nm %in% c("Duplicate","") &
                                     recurrence_from_last ==T, 2,
                                   ifelse(case_for_recurrence==F, 1,1.4))),
                     T1$tag)

    if(episode_type=="rolling"){
      # Number of recurrence periods so far - Recalculate
      fx <- T1[T1$epid!=sn_ref & T1$lr!=1 & T1$tag!=2 & T1$tr_case_nm=="", c("epid","case_nm","tag")]
      fx <- fx[order(-fx$tag),]
      fx <- fx[fx$case_nm=="Recurrent" & !duplicated(fx$epid),]
      fx <- fx$epid
      T1$roll[T1$epid %in% fx] <- T1$roll[T1$epid %in% fx] + 1
    }

    tagged_1 <- length(T1$epid[!T1$epid %in% c(sn_ref,NA) & T1$tag==2])
    total_1 <- nrow(T1)
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }

    T1 <- T1[names(T1)[!grepl("^tr|^fg|^mrk", names(T1))]]
    min_tag <- min(T1$tag)
    min_episodes <- min(T1$episodes)

    c = c+1
  }

  # Combine all episodes again
  T1 <- rbind(T1[g_vrs], grouped_epids)
  rm(grouped_epids)

  # Assign ungrouped episodes to unique IDs
  T1$wind_nm[T1$epid==sn_ref] <- T1$case_nm[T1$epid==sn_ref] <- "Skipped"
  T1$epid[T1$epid==sn_ref] <- T1$wind_id[T1$epid==sn_ref] <- T1$sn[T1$epid==sn_ref]
  T1$dist_from_wind[T1$epid==sn_ref] <- T1$dist_from_epid[T1$epid==sn_ref] <- 0

  # Drop 'duplicate' events if required
  if(deduplicate) T1 <- subset(T1, T1$case_nm!="Duplicate")

  if(is.null(ds)){
    T1 <- T1[order(T1$pr_sn),]
  }else{
    # check type of links
    links_check <- function(x, y, e) {
      if(tolower(e)=="l"){
        all(y %in% x & length(x)>1)
      }else if (tolower(e)=="g"){
        any(y %in% x)
      }
    }

    pds <- lapply(split(T1$dsvr, T1$epid), function(x, l=data_links){
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
      T1$epid_dataset[req_links==F] <- T1$dsvr[req_links==F]
      T1$wind_nm[req_links==F] <- T1$case_nm[req_links==F] <- "Skipped"
      T1$epid[req_links==F] <- T1$wind_id[req_links==F] <- T1$sn[req_links==F]
    }

    T1 <- T1[order(T1$pr_sn),]
  }

  diff_unit <- ifelse(tolower(episode_unit) %in% c("second","minutes"),
                      paste0(substr(tolower(episode_unit),1,3),"s"),
                      tolower(episode_unit))

  diff_unit <- ifelse(diff_unit %in% c("months","year"), "days", diff_unit)

  # Episode stats if required
  if(group_stats == T){
    # epid_l <- T1[c("epid", "rec_dt_ai", "rec_dt_zi", "ord", "ord_z","user_ord")]
    # epid_l <- unique(epid_l)
    #
    # epid_l <-
    #   rbind(
    #     dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord), !duplicated(.data$epid)), var ="a" ),
    #     dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord_z), !duplicated(.data$epid, fromLast = T)), var ="z" )
    #   ) %>%
    #   dplyr::mutate_at(c("rec_dt_ai", "rec_dt_zi"), ~ ifelse(dt_grp==rep(T,length(.)), format(., "%d/%m/%Y %H:%M:%S"),.)) %>%
    #   dplyr::mutate(val = ifelse(.data$var=="a", .data$rec_dt_ai, .data$rec_dt_zi)) %>%
    #   dplyr::select(.data$epid, .data$var, .data$val)
    #
    # epid_l <- tidyr::spread(epid_l, "var","val")
    #
    # T1 <- dplyr::left_join(T1, epid_l, by="epid")
    T1$a <- as.numeric(lapply(split(as.numeric(T1$rec_dt_ai), T1$epid), ifelse(from_last==F, min, max))[as.character(T1$epid)])
    T1$z <- as.numeric(lapply(split(as.numeric(T1$rec_dt_zi), T1$epid), ifelse(from_last==F, max, min))[as.character(T1$epid)])

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

  vrs <- names(T1)[!grepl("^pr_sn|^rec_dt_|^ord|^epi_len|^user_ord|^skip_ord|^c_sort", names(T1))]
  T1 <- T1[vrs]

  if(dt_grp==T){
    T1$dist_from_wind <- T1$dist_from_wind / diyar::episode_unit[[episode_unit]]
    T1$dist_from_wind <- as.difftime(T1$dist_from_wind, units = diff_unit)

    T1$dist_from_epid <- T1$dist_from_epid / diyar::episode_unit[[episode_unit]]
    T1$dist_from_epid <- as.difftime(T1$dist_from_epid, units = diff_unit)
  }


  unique_ids <- length(T1[!duplicated(T1$epid) & !duplicated(T1$epid, fromLast = T),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste(pd, "Episode grouping complete - " ,fmt(unique_ids)," record(s) assinged a unique ID. \n" , sep =""))
  if(to_s4) T1 <- diyar::to_s4(T1)
  T1
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
fixed_episodes <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days", episodes_max = Inf, data_source = NULL, data_links = "ANY",
                           custom_sort = NULL, skip_order =NULL, from_last = FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"),
                           overlap_methods =  "exact|across|chain|aligns_start|aligns_end|inbetween",
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
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))
  if(!(length(skip_order) %in% c(1, length(date)) | (length(skip_order) ==0 & is.null(skip_order)))) stop(paste("length of 'skip_order' must be 1 or the same as 'date'",sep=""))

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

  if(is.null(skip_order)){
    df$skip_order <- Inf
  }else{
    df$skip_order <- skip_order
  }

  df$method <- m

  if(is.null(data_source)){
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4)
  }else{
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4)
  }
}


#' @rdname episode_group
#' @export
rolling_episodes <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days", episodes_max = Inf, rolls_max = Inf, data_source = NULL, data_links = "ANY",
                             custom_sort = NULL, skip_order = NULL, from_last = FALSE, overlap_method = c("exact", "across","inbetween","aligns_start","aligns_end","chain"),
                             overlap_methods =  "exact|across|chain|aligns_start|aligns_end|inbetween", bi_direction= FALSE, group_stats = FALSE,
                             display = TRUE, deduplicate = FALSE, x, to_s4 = TRUE, recurrence_from_last = TRUE, case_for_recurrence =FALSE){

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
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(date)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))
  if(!(length(skip_order) %in% c(1, length(date)) | (length(skip_order) ==0 & is.null(skip_order)))) stop(paste("length of 'skip_order' must be 1 or the same as 'date'",sep=""))

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
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links)
    }else{
      diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                           bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                           from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max,
                           display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                           recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links)
  }

}

# @rdname episode_group
# @param epid \code{epid} object
# @details
# \code{plot_epid()} visulaises how an episode has been created. It works backwards, using the episode (\code{epid}) and corresponding
#  \code{date}, \code{case_length} and \code{recurrence_length} to show why/how events or event periods have been grouped in each episode.
#  This is then shown on a plots (one per \code{strata}) captured in an \code{R} object (\code{list} is there are multiple plots).
#  The plots can then be saved and shared.
#  \code{date}, \code{case_length} and \code{recurrence_length} must match those used in creating \code{epid} otherwise, the plot will not reflect what actually happened.
#
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
    #xlims <- c((min(dfp$dt_a) - max(c(max(dfp$c), max(dfp$r)))), (max(dfp$dt_z) + max(c(max(dfp$c), max(dfp$r)))))
    xlims <- c(min(dfp$dt_a)-1, max(dfp$dt_z)+1)

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
    dfp$p_ord <- order(-dfp$event_length, -as.numeric(dfp$dt_a),  dfp$ord)

    mid_pts <- dfp$dt_c
    names(mid_pts) <- 1:length(mid_pts)
    mid_pts <- mid_pts[dfp$event_nm!=""]
    p_ord <- dfp$p_ord[dfp$event_nm!=""]

    # # Check mid-points that are too close (0.04) as this will overlap in the plot.
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

    # dfp$e_y <- 1
    # dfp$e_y[dfp$event_nm!=""] <- max(dfp$e_y) + ((space_out_yy(diyar::expand_number_line(as.number_line(dfp$dt_c[dfp$event_nm!=""]), 1))-1) * 0.05 * scale_fac)

    # Some number_lines may overlap by chance so, space out again incrementally (0.01)
    dfp$e_y <- dfp$e_y  - (1:nrow(dfp) * (scale_fac * 0.02))

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

        if(i == nrow(cl) & i != 1 & cl$end_dt[i] > xlims[2]) x_pos <- ifelse(from_last ==T, cl$dt[i] + (scale_fac * 0.5), cl$dt[i] - (scale_fac * 0.5))
        else x_pos <- mean(c(cl$dt[i], cl$end_dt[i]))

        graphics::text(cex = .7 * scale_fac, y=cl$case_len_y_axis[i] + (scale_fac * 0.02) , x=x_pos, labels = cl$lab[i], col ="white", adj =c(0.5 ,0))
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
      rl$lab <- paste0("Recurrence length\n(",rl$r,"-day\ndifference)")

      for(i in 1:nrow(rl)){
        # Surpressed warning from 0 length arrows

        if(i == nrow(rl) & i != 1 & rl$end_dt[i] > xlims[2]) x_pos <- ifelse(from_last ==T, rl$dt[i] + (scale_fac * 0.5), rl$dt[i] - (scale_fac * 0.5))
        else x_pos <- mean(c(rl$dt[i], rl$end_dt[i]))

        suppressWarnings(graphics::arrows(length=0.1,angle=20, y0=rl$rec_len_y_axis[i], x0 = rl$dt[i], x1 = rl$end_dt[i], lty=2, col ="white"))
        graphics::text(cex = .7 * scale_fac, y=rl$rec_len_y_axis[i] + (scale_fac * 0.02) , x= x_pos, labels = rl$lab[i], col ="white", adj =c(.5,0))
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
