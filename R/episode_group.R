#' @name episode_group
#' @title Episode grouping for case definitions and record deduplication
#'
#' @description Group events into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique numerical record identifier. Optional.
#' @param strata Subsets of the dataset. Episode grouping will be done separately within each subset of the dataset.
#' In \code{\link{episode_group}}, you can use multiple columns. \code{\link{record_group}} can create useful \code{strata} e.g. patient identifiers.
#' @param date Event date (\code{date}, \code{datetime} or \code{numeric}) or period (\code{\link{number_line}}).
#' @param case_length Duration after a \code{"case"} within which subsequent events are considered \code{"duplicate"} events.
#' This period is referred to as the the \code{case window}. Can be a (\code{\link{number_line}}) range.
#' @param episodes_max Maximum number of episodes to have within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Duration after the last or first event (see \code{recurrence_from_last}) of the previous window within which subsequent events are considered \code{"recurrent"} events.
#' This period is referred to as the \code{recurrence window}. If \code{recurrence_length} is not supplied, it's assumed to be the same as \code{case_length}.
#' Can be a (\code{\link{number_line}}) range.
#' @param episode_unit Time units. Options are "seconds", "minutes", "hours", "days", "weeks", "months" or "years". See \code{diyar::episode_unit}.
#' @param rolls_max Maximum number of times an episode can reoccur. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset identifier. Useful when the dataset contains data from multiple sources.
#' In \code{\link{episode_group}}, you can use multiple columns supplied as column names.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent event and proceeding to the earliest.
#' If \code{FALSE}, it'll be forward in time - starting at the earliest event and proceeding to the most recent one.
#' @param overlap_method Methods of overlap considered when grouping event periods. Each pair of periods are checked with the same set of \code{overlap_method}.
#' Deprecated please use \code{overlap_methods} instead.
#' @param overlap_methods Methods of overlap considered when grouping event periods. Different pairs of periods can be checked with different sets of \code{overlap_methods}
#' @param custom_sort  Preferential order for \code{"case"} assignment. Useful in specifying that episode grouping begins at particular events regardless of chronological order.
#' In \code{\link{episode_group}}, you can use multiple columns as sort levels.
#' @param bi_direction If \code{FALSE} (default), \code{"duplicate"} events will be those within the \code{case_length} before \strong{or} after the \code{"case"} as determined by \code{from_last}.
#' If \code{TRUE}, \code{"duplicate"} events will be those within the same period before \strong{and} after the \code{"case"}.
#' @param group_stats If \code{TRUE}, the output will include additional information with useful stats for each episode group.
#' @param display If \code{TRUE} (default), a progress message is printed on screen.
#' @param to_s4 If \code{TRUE} (default), episodes are returned as an \code{\link[=epid-class]{epid}} object.
#' @param recurrence_from_last If \code{TRUE} (default), the reference event for a \code{recurrence window} will be the last event from the previous window.
#' If \code{FALSE} (default), it will be the first event. Only used if \code{episode_type} is \code{"rolling"}.
#' @param case_for_recurrence If \code{TRUE}, both case and recurrence events will have a \code{case window}.
#' If \code{FALSE} (default), only \code{case events} will have a \code{case window}. Only used if \code{episode_type} is \code{"rolling"}.
#' @param skip_order Skip episodes whose \code{case} events have \code{custom_sort} values that are less than or equal to the \code{"nth"} level of \code{custom_sort}.
#' Useful in skipping episodes that are not required and so minimises the overall processing time. Ignored if \code{custom_sort} is \code{NULL}.
#' @param data_links Breakup episodes that will not include records from these \code{data_sources}. \code{data_links} should be a \code{list} with every element named 'l' (links) or 'g' (groups).
#' Useful in skipping episodes that are not required to minimise processing time. Ignored if \code{data_source} is \code{NULL}.
#' @param skip_if_b4_lengths If \code{TRUE} (default), records \code{events} before the \code{case_length} or the \code{recurrence_length} range are skipped.
#' @param include_index_period If \code{TRUE}, overlaps with the index event or period are grouped together even if they are outside the cut-off range (\code{case_length} or \code{recurrence_length}).
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
#' \item \code{dist_from_wind} - duration of each event from its window's reference event
#' \item \code{dist_from_epid} - duration of each event from its episode's reference event
#' \item \code{epid_total} - number of records in each episode
#' \item \code{epid_dataset} - data sources in each episode
#' \item \code{epid_interval} - episode start and end dates. A \code{\link{number_line}} object.
#' \item \code{epid_length} - difference between episode start and end dates (\code{difftime}). If possible, it's the same unit as \code{episode_unit} otherwise, a difference in days is returned
#' \item \code{epid_total} - number of records in each episode
#'
#' }
#'
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlaps}} and \code{\link{number_line}}
#'
#' @details
#' Episode grouping begins at a reference event (\code{"case"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain events before proceeding forward or backwards in time.
#' The maximum duration of a \code{"fixed"} episode is the \code{case_length}. This period is referred to as the \code{case window}. The maximum duration of a \code{"rolling"} episode is the
#' \code{case_length} plus all periods of recurrence. The recurrence periods are referred to as \code{recurrence windows}.
#' This is a specified duration (\code{recurrence_length}) after the last or first (depending on \code{recurrence_from_last}) event in the previous window.
#' Events within this period are considered \code{"recurrent"} events.
#'
#' When a \code{data_source} identifier is provided,
#' \code{epid_dataset} is included in the output. This lists the source of every event in each episode.
#'
#' @examples
#' library(diyar)
#' data(infections)
#' data(hospital_admissions)
#'
#' db_1 <- infections
#' db_1$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#'
#' # Fixed episodes
#' # One 16-day (15-day difference) episode per patient
#' db_1$epids_p <- fixed_episodes(date=db_1$date, strata = db_1$patient_id,
#' case_length = 15, episodes_max = 1, display = FALSE)
#' # Rolling episodes
#' # Case length of 16 days and recurrence periods of 11 days
#' db_1$rd_b <- rolling_episodes(db_1$date, case_length = 15,
#' recurrence_length = 10, display = FALSE)
#'
#' # Interval grouping
#' hospital_admissions$admin_period <- number_line(hospital_admissions$admin_dt,
#' hospital_admissions$discharge_dt)
#' admissions <- hospital_admissions[c("admin_period","epi_len")]
#'
#' # Episodes of overlaping periods of admission
#' hospital_admissions$epi_0 <- fixed_episodes(date=hospital_admissions$admin_period,
#' case_length = 0, group_stats = TRUE, to_s4=TRUE)
#'
#' # Note - episode_group() takes column names not actual values
#'
#' @aliases episode_group
#' @export
#' @rdname episode_group
episode_group <- function(df, sn = NULL, strata = NULL, date,
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
    # Seperate out skipped records
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

#' @rdname episode_group
#' @param deduplicate if \code{TRUE}, \code{"dupilcate"} events are excluded from the output.
#' @param x Record date or interval. Deprecated. Please use \code{date}
#' @details
#' \code{fixed_episodes()} and \code{rolling_episodes()} are wrapper functions of \code{episode_group()}.
#' They are convenient alternatives with the same functionalities.
#'
#' @rdname episode_group
#' @export
fixed_episodes <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days",
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
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                         include_index_period=include_index_period)
  }else{
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", data_links = data_links, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4,
                         include_index_period=include_index_period)
  }
}


#' @rdname episode_group
#' @export
rolling_episodes <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days",
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
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = NULL, custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }else{
    diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                         bi_direction = bi_direction , data_source = "ds", custom_sort = "user_srt", skip_order = "skip_order",
                         from_last = from_last, overlap_methods = "method", recurrence_length = "rc_epl", rolls_max = rolls_max, skip_if_b4_lengths = skip_if_b4_lengths,
                         display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4,
                         recurrence_from_last = recurrence_from_last, case_for_recurrence = case_for_recurrence, data_links = data_links, include_index_period=include_index_period)
  }

}

episodes <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length = NULL, episode_unit = "days",
                     episodes_max = Inf, skip_if_b4_lengths = TRUE, data_source = NULL, data_links = "ANY",
                     custom_sort = NULL, skip_order = Inf, from_last = FALSE, rolls_max = Inf,
                     overlap_methods = "overlap", bi_direction = FALSE, group_stats = TRUE, recurrence_from_last = TRUE,
                     display = "progress", deduplicate = FALSE, include_index_period = TRUE, case_for_recurrence = FALSE,
                     to_s4 = TRUE, x, overlap_method = "overlap", episode_type = "fixed") {

  # Deprecated arguments and behaviour
  if(missing(overlap_methods) & !missing(overlap_method)) {
    overlap_methods <- paste0(overlap_method[!duplicated(overlap_method)], collapse = "|")
    warning(paste0("`overlap_method` is deprecated and will be removed in the next release:\n",
                   "i - Please use `overlap_methods` instead.\n",
                   "i - Your values were passed to `overlap_methods`."), call. = F)
  }

  if(missing(date) & !missing(x)) {
    date <- x
    warning(paste0("`x` is deprecated and will be removed in the next release:\n",
                   "i - Please use `date` instead.\n",
                   "i - Your values were passed to `date`."), call. = F)
  }

  errs <- err_checks_epid(sn = sn, date = date, case_length = case_length, strata = strata,
                          display=display, episodes_max = episodes_max, from_last = from_last,
                          episode_unit = episode_unit, overlap_methods = overlap_methods,
                          skip_order = skip_order, custom_sort = custom_sort, group_stats = group_stats,
                          data_source=data_source, data_links = data_links, include_index_period = include_index_period,
                          skip_if_b4_lengths = skip_if_b4_lengths, bi_direction = bi_direction, deduplicate = deduplicate,
                          rolls_max = rolls_max, case_for_recurrence = case_for_recurrence, recurrence_from_last = recurrence_from_last,
                          episode_type == episode_type)

  if(errs!=F) stop(errs, call. = F)

  dl_lst <- unlist(data_links, use.names = F)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links)=="", "l", names(data_links))

  int <- as.number_line(date)
  is_dt <- ifelse(!any(class(int@start) %in% c("Date","POSIXct","POSIXt","POSIXlt")), F, T)
  episode_unit <- ifelse(is_dt == F,"seconds", tolower(episode_unit))

  if(is_dt == T){
    int <- number_line(
      l = as.POSIXct(format(int@start, "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S"),
      r = as.POSIXct(format(right_point(int), "%d/%m/%Y %H:%M:%S"), "UTC",format="%d/%m/%Y %H:%M:%S")
    )
  }

  r <- prep_lengths(case_length, overlap_methods, int,
                    episode_unit, bi_direction, from_last)
  ep_l <- r$lengths
  mths_a <- r$method

  if(length(episode_type) == 1) episode_type <- rep(episode_type, length(int))
  any_rolling <- any(episode_type == "rolling")
  lead_epid_type <- episode_type[!duplicated(episode_type)]
  one_epid_type <- length(lead_epid_type) == 1
  if(one_epid_type != T){
    lead_epid_type <- rep("", length(int))
  }

  if(any_rolling == T){
    if(is.null(recurrence_length)){
      rc_l <- ep_l
      mths_b <- mths_a
    }else{
      r <- prep_lengths(recurrence_length, overlap_methods, int,
                        episode_unit, bi_direction, from_last,
                        include_index_period)
      rc_l <- r$lengths
      mths_b <- r$method
    }
  }

  if(length(case_for_recurrence) == 1) case_for_recurrence <- rep(case_for_recurrence, length(int))
  any_case_for_rec <- any(case_for_recurrence == T)
  lead_case_for_rec <- case_for_recurrence[!duplicated(case_for_recurrence)]
  one_case_for_rec <- length(lead_case_for_rec) == 1
  if(one_case_for_rec != T){
    lead_case_for_rec <- rep(NA, length(int))
  }

  if(length(recurrence_from_last) == 1) recurrence_from_last <- rep(recurrence_from_last, length(int))
  any_rec_from_last <- any(recurrence_from_last == F)
  lead_rec_from_last <- recurrence_from_last[!duplicated(recurrence_from_last)]
  one_rec_from_last <- length(lead_rec_from_last) == 1
  if(one_rec_from_last != T){
    lead_rec_from_last <- rep(NA, length(int))
  }

  if(length(skip_if_b4_lengths) == 1) skip_if_b4_lengths <- rep(skip_if_b4_lengths, length(int))
  any_skip_b4_len <- any(skip_if_b4_lengths == T)
  lead_skip_b4_len <- skip_if_b4_lengths[!duplicated(skip_if_b4_lengths)]
  one_skip_b4_len <- length(lead_skip_b4_len) == 1
  if(one_skip_b4_len != T){
    lead_skip_b4_len <- rep(NA, length(int))
  }

  if(length(episodes_max) == 1) episodes_max <- rep(episodes_max, length(int))
  if(length(rolls_max) == 1) rolls_max <- rep(rolls_max, length(int))
  if(length(skip_order) == 1) skip_order <- rep(skip_order, length(int))

  if(length(strata) == 1 | is.null(strata)) {
    cri <- rep(1, length(int))
  }else{
    cri <- match(strata, strata[!duplicated(strata)])
  }
  int@id <- seq_len(length(int))
  int@gid <- int@id
  if(!is.null(sn)) int@gid <- sn

  if(from_last==T){
    assign_ord <- order(abs(max(as.numeric(int@start)) - as.numeric(int@start)),
                        -abs(max(as.numeric(right_point(int))) - as.numeric(right_point(int))))
  }else{
    assign_ord <- order(abs(min(as.numeric(int@start)) - as.numeric(int@start)),
                        -abs(min(as.numeric(right_point(int))) - as.numeric(right_point(int))))
  }
  assign_ord <- match(seq_len(length(int)), assign_ord)

  if(!is.null(custom_sort)) {
    c_sort <- as.numeric(as.factor(custom_sort))
    if(length(c_sort)==1) c_sort <- rep(c_sort, length(int))
    assign_ord <- order(as.factor(c_sort), assign_ord)
    assign_ord <- match(seq_len(length(int)), assign_ord)
  }else{
    c_sort <- rep(0, length(int))
  }

  tag <- rep(0, length(int))
  names(cri) <- int@id; e <- int@gid;
  names(e) <- int@id; wind_id <- int@gid
  names(int) <- int@id
  epid_n <- rep(0, length(int))

  if(any_rolling == T) roll_n <- rep(0, length(int))
  case_nm <- rep("", length(int))
  wind_nm <- case_nm

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, length(int))
    names(data_source) <- int@id
  }

  ite <- 1
  lgk <- is.na(strata)
  tag[lgk] <- 2
  case_nm[lgk] <- "Skipped"

  if(!is.null(data_source) & !all(toupper(dl_lst) == "ANY")){
    req_links <- check_links(cri, data_source, data_links)$rq
    tag[req_links == F] <- 2
    case_nm[req_links == F] <- "Skipped"
  }

  excluded <- length(tag[tag == 2])
  tot <- length(int)
  cat("\n")
  while (min(tag) != 2) {
    if(display == "stats" & excluded >0 & ite ==1) cat(paste0(fmt(tot), " record(s); ", fmt(excluded)," excluded from episode grouping. ", fmt(tot-excluded), " left to group.\n"))
    if(display == "stats"){
      msg <- paste0("Episode or recurrence window ", fmt(ite) ,".")
      cat(msg, "\n", sep="")
    }

    current_tot <- length(tag[tag!=2])

    sort_ord <- order(cri, tag, assign_ord, int@gid, decreasing = T)
    for(i in c("e","tag","cri","assign_ord",
               "int","epid_n", "c_sort",
               "skip_order", "case_nm",
               "wind_nm", "wind_id",
               "rolls_max",
               "episodes_max")){
      assign(i, get(i)[sort_ord])
    }

    if(any_rolling == T) roll_n <- roll_n[sort_ord]
    if(one_epid_type != T){
      lead_epid_type <- lead_epid_type[sort_ord]
      episode_type <- episode_type[sort_ord]
    }

    if(one_case_for_rec != T){
      lead_case_for_rec <- lead_case_for_rec[sort_ord]
      case_for_recurrence <- case_for_recurrence[sort_ord]
    }

    if(one_rec_from_last != T){
      lead_rec_from_last <- lead_rec_from_last[sort_ord]
      recurrence_from_last <- recurrence_from_last[sort_ord]
    }

    if(one_skip_b4_len != T){
      lead_skip_b4_len <- lead_skip_b4_len[sort_ord]
      skip_if_b4_lengths <- skip_if_b4_lengths[sort_ord]
    }

    ep_l <- lapply(ep_l, function(x){x[sort_ord]})
    mths_a <- lapply(mths_a, function(x){x[sort_ord]})

    if(any_rolling == T){
      rc_l <- lapply(rc_l, function(x){x[sort_ord]})
      mths_b <- lapply(mths_b, function(x){x[sort_ord]})
    }

    r <- rle(cri)
    p <- as.numeric(names(r$values))
    cri_tot <- r$lengths
    tr_ep_int <- lapply(ep_l, function(x){
      rep(x[which(names(cri) %in% p)], r$lengths)
    })

    if(any_rolling == T){
      tr_rc_int <- lapply(rc_l, function(x){
        rep(x[which(names(cri) %in% p)], r$lengths)
      })
    }

    tr_tag <- rep(tag[which(names(cri) %in% p)], r$lengths)
    tr_e <- rep(e[which(names(cri) %in% p)], r$lengths)
    tr_int <- rep(int[which(names(cri) %in% p)], cri_tot)

    if(one_epid_type != T){
      tr_epid_type <- rep(episode_type[which(names(cri) %in% p)], r$lengths)
      lead_epid_type <- ifelse(tr_tag == 0, tr_epid_type, lead_epid_type)
    }

    if(one_case_for_rec != T){
      tr_case_for_rec <- rep(case_for_recurrence[which(names(cri) %in% p)], r$lengths)
      lead_case_for_rec <- ifelse(tr_tag == 0, tr_case_for_rec, lead_case_for_rec)
    }

    if(one_rec_from_last != T){
      tr_rec_from_last <- rep(recurrence_from_last[which(names(cri) %in% p)], r$lengths)
      lead_rec_from_last <- ifelse(tr_tag == 0, tr_rec_from_last, lead_rec_from_last)
    }

    if(one_skip_b4_len != T){
      tr_skip_b4_len <- rep(skip_if_b4_lengths[which(names(cri) %in% p)], r$lengths)
      lead_skip_b4_len <- ifelse(tr_tag == 0, tr_skip_b4_len, lead_skip_b4_len)
    }

    if (any_rolling == T) {
      roll_n <- ifelse(tr_tag == 0, 0, ifelse(tr_tag == -1, roll_n + 1, roll_n))
    }
    epid_n <- ifelse(tr_tag == 0, epid_n + 1, epid_n)

    lgk1 <- epid_n > episodes_max & tag != 2
    case_nm[lgk1] <- "Skipped"
    tag[lgk1] <- 2

    # Skip order
    cri_skp <- cri[c_sort > skip_order];
    cri_skp <- cri_skp[!duplicated(cri_skp)]
    lgk2 <- cri %in% cri_skp
    current_skipped <- length(cri[lgk1 | lgk2])
    tag[lgk2] <- 2
    case_nm[lgk2] <- "Skipped"
    rm(cri_skp); rm(lgk2); rm(lgk1)

    if(min(tag) == 2){
      if(tolower(display) == "stats"){
        msg <- paste0(fmt(current_tot), " record(s); ", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_skipped))," assigned to unique episodes.")
        cat(msg, "\n", sep="")
      }else if (tolower(display)=="progress") {
        progress_bar(length(tag[tag == 2])/tot, 100)
      }
      break
    }

    tr_sn <- tr_ep_int[[1]]@gid
    ref_rd <- int@gid %in% tr_sn

    ovr_chks <- function(tr, int, mths) diyar::overlaps(tr, int, methods =mths)
    ep_checks <- rowSums(mapply(ovr_chks, tr_ep_int, rep(list(int), length(tr_ep_int)), mths_a)) > 0
    if(any_rolling == T){
      rc_checks <- rowSums(mapply(ovr_chks, tr_rc_int, rep(list(int), length(tr_rc_int)), mths_b)) > 0
    }

    cr <- ifelse(tr_tag %in% c(0, -2) &
                   (ref_rd  | ep_checks==1) &
                   tag != 2,
                 T, F)

    if(any_rolling == T){
      cr <- ifelse(tr_tag == -1 &
                     (ref_rd  | rc_checks==1) &
                     tag != 2,
                   T, cr)
    }

    if(include_index_period == T){
      ref_period <- overlap(int, tr_int)
      cr[ref_period & !cr] <- T
    }

    e[cr & tag == 0 & tr_tag == 0] <- tr_sn[cr & tag == 0 & tr_tag ==  0]
    wind_id[cr & tag == 0] <- tr_sn[cr & tag == 0]
    e[cr & tr_tag %in% c(-1, -2)] <- tr_e[cr & tr_tag %in% c(-1, -2)]

    case_nm[cr & tr_tag == 0] <- ifelse(ref_rd[cr & tr_tag == 0], "Case", "Duplicate_C")
    wind_nm[cr & tr_tag %in% c(0, -2) & wind_nm == ""] <- "Case"
    tag[cr] <- 2

    if(include_index_period != T){
      ref_period <- overlap(int, tr_int)
    }

    if(any_rolling == T){
      case_nm[cr & tr_tag %in% c(-1, -2) & case_nm == ""] <- "Duplicate_R"
      wind_nm[cr & tr_tag == -1 & wind_nm == ""] <- "Recurrence"
      sort_ord <- order(cri, cr, -ref_period, tag, -assign_ord, int@gid)
      r <- rle(cri[sort_ord])
      case_nm[which(int@id %in% names(r$values) &
                      tr_tag %in% c(-1) &
                      !ref_period
      )] <- "Recurrent"

      t_cri <- cri[order(cri, -tag)]
      t_tag <- tag[order(cri, -tag)]
      last_tag <- rle(t_cri)
      last_tag <- rep(t_tag[which(names(t_cri) %in% names(last_tag$values))], last_tag$lengths)
      last_tag <- last_tag[match(names(cri), names(t_cri))]
      rm(t_tag); rm(t_cri)
      close_epi <- last_tag == 2

      roll_ref <- assign_ord
      if(any_rec_from_last == T) roll_ref[!lead_rec_from_last] <- -roll_ref[!lead_rec_from_last]

      t_cr <- ifelse(ref_rd & roll_n >= 1, F, cr)
      sort_ord <- order(cri, t_cr, tag, roll_ref, int@gid)
      r <- rle(cri[sort_ord])
      tag[which(int@id %in% names(r$values) & tag !=0 & !close_epi & roll_n < rolls_max & t_cr & lead_epid_type == "rolling")] <- -1

      if(any_case_for_rec == T){
        tag[which(int@id %in% names(r$values) &
                    !(ref_rd & roll_n >= 1) &
                    tr_tag == -1 &
                    !close_epi &
                    roll_n <= rolls_max &
                    t_cr  &
                    lead_epid_type == "rolling" &
                    lead_case_for_rec == T)] <- -2
      }
    }

    if(min(tag) == 2){
      if(tolower(display) == "stats"){
        msg <- paste0(fmt(current_tot), " record(s); ", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_skipped))," assigned to unique episodes.")
        cat(msg, "\n", sep="")
      }else if (tolower(display)=="progress") {
        progress_bar(length(tag[tag==2])/tot, 100)
      }
      break
    }

    if(any_skip_b4_len == T){
      ep_l_min_a <- Rfast::rowMinsMaxs(sapply(tr_ep_int[lead_skip_b4_len], start_point))
      ep_l_min_z <- Rfast::rowMinsMaxs(sapply(tr_ep_int[lead_skip_b4_len], end_point))

      ep_l_bounds_a <- start_point(tr_int[lead_skip_b4_len])
      ep_l_bounds_z <- end_point(tr_int[lead_skip_b4_len])

      ep_l_bounds_a <- ifelse(ep_l_min_a[1,] < ep_l_bounds_a, ep_l_min_a[1,], ep_l_bounds_a)
      ep_l_bounds_z <- ifelse(ep_l_min_z[2,] > ep_l_bounds_z, ep_l_min_z[2,], ep_l_bounds_z)

      epc_bnds <- suppressWarnings(
        number_line(
          l = ep_l_bounds_a,
          r = ep_l_bounds_z))

      ep_obds_checks <- suppressWarnings(overlap(int[lead_skip_b4_len], epc_bnds))

      if(any_rolling == T){
        rc_l_min_a <- Rfast::rowMinsMaxs(sapply(tr_rc_int[lead_skip_b4_len], start_point))
        rc_l_min_z <- Rfast::rowMinsMaxs(sapply(tr_rc_int[lead_skip_b4_len], end_point))

        rc_l_bounds_a <- start_point(tr_int[lead_skip_b4_len])
        rc_l_bounds_z <- end_point(tr_int[lead_skip_b4_len])

        rc_l_bounds_a <- ifelse(rc_l_min_a[1,] < rc_l_bounds_a, rc_l_min_a[1,], rc_l_bounds_a)
        rc_l_bounds_z <- ifelse(rc_l_min_z[2,] > rc_l_bounds_z, rc_l_min_z[2,], rc_l_bounds_z)

        rc_l_bnds <- suppressWarnings(
          number_line(
            l = rc_l_bounds_a,
            r = rc_l_bounds_z))

        rc_obds_checks <- suppressWarnings(overlap(int[lead_skip_b4_len], rc_l_bnds))
      }

      skp_crxt <- cri[cr & !ref_period]
      skp_crxt <- skp_crxt[!duplicated(skp_crxt)]
      indx <- (ep_obds_checks &
                 !cr &
                 cri[lead_skip_b4_len] %in% skp_crxt &
                 tr_tag[lead_skip_b4_len] %in% c(0, -2) &
                 case_nm[lead_skip_b4_len] == "")
      if(any_rolling == T){
        indx <- ifelse((rc_obds_checks &
                          !cr &
                          cri[lead_skip_b4_len] %in% skp_crxt &
                          tr_tag[lead_skip_b4_len] == -1 &
                          case_nm[lead_skip_b4_len] == ""),
                       T, indx)
      }

      case_nm[lead_skip_b4_len][indx] <- "Skipped"
      tag[lead_skip_b4_len][indx] <- 2
      rm(skp_crxt); rm(indx)
    }

    current_tagged <- length(cr[cr])
    if(tolower(display) == "stats"){
      msg <- paste0(fmt(current_tot), " record(s); ", fmt(current_tagged)," grouped to episodes", ifelse(current_skipped>0, paste0(", ",fmt(current_skipped)," skipped"), ""), " and ", fmt(current_tot - (current_tagged + current_skipped))," left to group.")
      cat(msg, "\n", sep="")
    }else if (tolower(display)=="progress") {
      progress_bar(length(tag[tag==2])/tot, 100)
    }
    ite <- ite + 1
  }
  cat("\n")

  wind_nm[which(case_nm == "Skipped")] <- "Skipped"
  diff_unit <- ifelse(tolower(episode_unit) %in% c("second","minutes"),
                      paste0(substr(tolower(episode_unit),1,3),"s"),
                      tolower(episode_unit))

  diff_unit <- ifelse(diff_unit %in% c("months","year"), "days", diff_unit)

  stat_pos <- int@id
  sort_ord <- order(e, wind_id, as.numeric(int@start))
  e <- e[sort_ord]
  int <- int[sort_ord]

  epid_n <- rle(e)
  epid_n <- rep(epid_n$lengths, epid_n$lengths)
  lgk <- !duplicated(e)
  dist_from_epid <- ((as.numeric(int@start) + as.numeric(right_point(int))) * .5) -
    rep(((as.numeric(int@start[lgk]) + as.numeric(right_point(int[lgk]))) * .5), epid_n[lgk])

  if(any_rolling == T){
    wind_id <- wind_id[sort_ord]
    wind_n <- rle(wind_id)
    wind_n <- rep(wind_n$lengths, wind_n$lengths)
    lgk <- !duplicated(wind_id)
    dist_from_wind <- ((as.numeric(int@start) + as.numeric(right_point(int)))*.5) -
      rep(((as.numeric(int@start[lgk]) + as.numeric(right_point(int[lgk])))*.5), wind_n[lgk])
  }else{
    dist_from_wind <- dist_from_epid
    wind_id <- e
  }

  if(is_dt==T){
    dist_from_epid <- dist_from_epid / diyar::episode_unit[[episode_unit]]
    dist_from_epid <- as.difftime(dist_from_epid, units = diff_unit)

    if (any_rolling == T){
      dist_from_wind <- dist_from_wind / diyar::episode_unit[[episode_unit]]
      dist_from_wind <- as.difftime(dist_from_wind, units = diff_unit)
    }else{
      dist_from_wind <- dist_from_epid
    }
  }

  tmp_pos <- names(e)
  fd <- match(1:length(int), tmp_pos)
  f_e <- e[fd]; names(e) <- NULL; names(f_e) <- NULL

  retrieve_pos <- match(1:length(int), stat_pos)
  epid <- new("epid",
              .Data= e[fd],
              dist_from_epid = dist_from_epid[fd],
              dist_from_wind = dist_from_wind[fd],
              sn = int@gid[fd],
              case_nm= case_nm[retrieve_pos],
              wind_nm = wind_nm[retrieve_pos],
              wind_id = wind_id[fd])
  names(epid@wind_id) <- NULL

  if(!is.null(data_source)){
    data_source <- data_source[match(tmp_pos[fd], names(data_source))]
    # Data links
    names(e) <- tmp_pos
    rst <- check_links(e[fd], data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      epid@dist_from_epid[req_links == F] <- 0
      epid@dist_from_wind[req_links == F] <- 0
      epid@case_nm[req_links == F] <- "Skipped"
      epid@.Data[req_links == F] <- epid@sn[req_links == F]
      epid@wind_id[req_links == F] <- epid@sn[req_links == F]
      datasets[req_links == F] <- data_source[req_links == F]
    }
    epid@epid_dataset <- datasets
  }

  epid_tot <- epid_n[fd]
  if(group_stats == T){
    epid_dt_a <- lapply(split(as.numeric(int@start), e), ifelse(from_last==F, min, max))
    epid_dt_z <- lapply(split(as.numeric(right_point(int)), e), ifelse(from_last==F, max, min))

    epid_dt_a <- as.numeric(epid_dt_a)[match(e, names(epid_dt_a))]
    epid_dt_z <- as.numeric(epid_dt_z)[match(e, names(epid_dt_z))]

    if(is_dt ==T){
      epid_dt_a <- as.POSIXct(epid_dt_a, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      epid_dt_z <- as.POSIXct(epid_dt_z, "UTC", origin = as.POSIXct("01/01/1970 00:00:00", "UTC",format="%d/%m/%Y %H:%M:%S"))
      epid_l <- difftime(epid_dt_z, epid_dt_a, units = diff_unit)
    }else{
      epid_l <- epid_dt_z - epid_dt_a
    }

    names(epid_n) <- NULL
    names(epid_l) <- NULL
    names(e) <- NULL

    epid@epid_interval <- number_line(l = epid_dt_a[fd],
                                      r = epid_dt_z[fd],
                                      gid = f_e)
    epid@epid_total <- epid_tot
    epid@epid_length <- epid_l[fd]
  }

  names(epid) <- NULL

  summ <- paste0("Records: ", fmt(length(epid)), ".\n",
                 "Skipped records: ", fmt(length(epid[epid@case_nm == "Skipped"])), ".\n",
                 "Episodes with unique events: ", fmt(length(epid[epid@case_nm == "Case" & epid_tot == 1])), ".\n",
                 "Episodes with multiple events: ", fmt(length(epid[epid@case_nm == "Case" & epid_tot > 1])), ".\n")
  cat(summ)

  if(deduplicate == T) epid <- epid[!epid@case_nm %in% c("Duplicate_C", "Duplicate_R")]
  if(to_s4 == F) epid <- to_df(epid)
  return(epid)
}
