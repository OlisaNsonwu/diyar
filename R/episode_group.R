#' @name episode_group
#' @title Episode grouping for record deduplication and case assignment
#'
#' @description Group records into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique \code{numeric} record identifier. Optional.
#' @param strata Episode grouping will be done separately for these subsets (\code{strata}) of the dataset. \code{episode_group} supports the use of multiple columns supplied as column names. \code{\link{record_group}} can be used to create \code{strata}.
#' @param date Record date (\code{date} or \code{datetime}) or period (\code{\link{number_line}}) objects.
#' @param case_length Period after a \code{"Case (C)"} within which another record from the same \code{strata} is considered a \code{"Duplicate (D)"} record.
#' @param episodes_max Maximum number of times to group episodes within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Period after the last record (\code{"Case (C)"} , \code{"Duplicate (D)"} or \code{"Recurrent (R)"}) in an episode within which another record from the same \code{strata} is considered a \code{"Recurrent (R)"} record. If \code{recurrence_length} is not supplied, \code{case_length} is used as \code{recurrence_length}.
#' @param episode_unit Time units as supported by lubridate's \code{\link[lubridate]{duration}} function.
#' @param rolls_max Maximum number of times an event an reoccur within an episode. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset identifier. Useful when the dataset contains data from multiple sources. \code{episode_group} support the use of multiple columns supplied as column names.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent record and proceeding to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest record and proceeding to the most recent one.
#' @param overlap_method A set of ways for grouped intervals to overlap. Options are; \code{"across"}, \code{"aligns_start"}, \code{"aligns_end"}, \code{"inbetween"}, \code{"chain"}. See \code{\link{overlap}} functions.
#' @param custom_sort If \code{TRUE}, \code{"Case (C)"} assignment will be in preference to this sort order. Useful in specifying that episode grouping begins at particular records regardless of chronological order. \code{episode_group} supports the use of multiple columns supplied as column names.
#' @param bi_direction If \code{FALSE}, \code{"Duplicate (D)"} records will be those within the \code{case_length} period, before or after the \code{"Case (C)"} as determined by \code{from_last}. If \code{TRUE}, \code{"Duplicate (D)"} records will be those within the same period before and after the \code{"Case (C)"}.
#' @param group_stats If \code{TRUE}, the output will include additional columns with useful stats for each episode group.
#' @param display If \code{TRUE}, status messages are printed on screen.
#' @param to_s4 if \code{TRUE}, changes the returned value to an \code{\link[=epid-class]{epid}} object.
#'
#' @return
#'
#' \code{data.frame} (\code{\link[=epid-class]{epid}} objects if \code{to_s4} is \code{TRUE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{epid} - unique episode identifier
#' \item \code{case_nm} - record type in regards to case assignment
#' \item \code{epid_dataset} - datasets contained in each episode
#' \item \code{epid_interval} - Episode start and end dates. \code{\link{number_line}} object.
#' \item \code{epid_length} - Difference between episode start and end dates (\code{difftime}). If possible, it's the same unit as \code{episode_unit} otherwise, a difference in days is returned
#' \item \code{epid_total} - number of records in each episode
#' }
#'
#' \code{epid} objects will be the default output from the next release onwards
#'
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlap}} and \code{\link{number_line}}
#'
#' @details
#' Episode grouping begins at a reference record (\code{"Case (C)"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain records before proceeding forward or backwards in time.
#' The maximum duration of a \code{"fixed"} episode is the \code{case_length} while, the maximum duration of a \code{"rolling"} episode is the
#' \code{case_length} plus all recurrence periods. A recurrence period is a fixed period (\code{recurrence_length}) after the last record in an episode.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' #1. Fixed episodes
#' data(infections); infections
#' db_1 <- infections
#' # 16-day (difference of 15 days) episodes beginning from the earliest record
#' db_1$fd <- fixed_episodes(db_1$date, case_length = 15, to_s4 = TRUE, display = FALSE)
#' # 16-hour (difference of 15 hours) episodes beginning from the earliest record
#' db_1$fh <- fixed_episodes(db_1$date, case_length = 15,
#' episode_unit = "hours", to_s4 = TRUE, display = FALSE)
#' db_1
#'
#' #2. Rolling episodes
#' # Case length and recurrence periods of 15 days
#' db_1$rd_a <- rolling_episodes(db_1$date, case_length = 15, to_s4 = TRUE, display = FALSE)
#' # Case length of 15 days and recurrence periods of 10 days
#' db_1$rd_b <- rolling_episodes(db_1$date, case_length = 15,
#' recurrence_length = 10, to_s4 = TRUE, display = FALSE)
#' # Case length of 15 days and 2 recurrence periods of 10 days
#' db_1$rd_c <- rolling_episodes(db_1$date, case_length = 15,
#' recurrence_length = 10, rolls_max = 2, to_s4 = TRUE, display = FALSE)
#' db_1
#'
#' # 3. Stratified episode grouping
#' db_3 <- infections
#'
#' db_3$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#' # One 16-day episode per patient
#' db_3$epids_p <- fixed_episodes(date=db_3$date, strata = db_3$patient_id,
#' case_length = 15, episodes_max = 1, to_s4 = TRUE, display = FALSE)
#' db_3
#'
#' # 4. Case assignment
#' db_4 <- infections
#'
#' ## 4.1 Chronological order
#' db_4$forward_time <- fixed_episodes(db_4$date, case_length = 1,
#' episode_unit = "month", to_s4 = TRUE, display = FALSE)
#' db_4$backward_time <- fixed_episodes(db_4$date, case_length = 1,
#' episode_unit = "month", from_last = TRUE, to_s4 = TRUE, display = FALSE)
#' db_4
#'
#' ## 4.2 User defined order
#' db_4b <- infections
#' # RTI > UTI, or RTI > BSI
#' db_4b$ord2 <- ifelse(db_4b$infection =="RTI",0,1)
#' # UTI > BSI > RTI
#' db_4b$ord1 <- factor(db_4b$infection, levels = c("UTI","BSI","RTI"))
#'
#' db_4b$epids_1 <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord2, to_s4 = TRUE, display = FALSE)
#' db_4b$epids_2 <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord1, to_s4 = TRUE, display = FALSE)
#' db_4b$epids_2b <- fixed_episodes(db_4b$date, case_length = 15,
#' custom_sort = db_4b$ord1, bi_direction = TRUE, to_s4 = TRUE, display = FALSE)
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
#' episode_unit = "months", group_stats = TRUE, to_s4 = TRUE, display = FALSE)
#' admissions
#'
#' # Episodes of chained admission periods, and those with aligned end periods
#' admissions$epi_0b <- fixed_episodes(date=admissions$admin_period, case_length = 0,
#' overlap_method = c("chain","aligns_end"), group_stats = TRUE, to_s4 = TRUE, display = FALSE)
#' admissions["epi_0b"]
#'
#'
#' # Note - episode_group() takes column names not actual values
#' db_5 <- infections
#'
#' db_5$recur <- 20
#' db_5$epids_f <- episode_group(db_5, date=date, episode_type = "fixed",
#' case_length = epi_len, to_s4 = TRUE, display = FALSE)
#' db_5$epids_r <- episode_group(db_5, date=date, episode_type = "rolling",
#' case_length = epi_len, recurrence_length = recur, to_s4 = TRUE, display = FALSE)
#' db_5
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @aliases episode_group
#' @export
episode_group <- function(df, sn = NULL, strata = NULL, date,
                          case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL,
                          custom_sort = NULL, from_last=FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), bi_direction = FALSE,
                          group_stats= FALSE, display=TRUE, deduplicate=FALSE, to_s4 = FALSE){
  . <- NULL
  if(!(is.logical(group_stats) & is.logical(from_last) & is.logical(display) & is.logical(to_s4))) stop(paste("'group_stats', 'from_last', 'display' and 'to_s4' must be TRUE or FALSE"))
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
        options("diyar.episode_group.output"= TRUE)
      }
      if (getOption("diyar.episode_group.output")){
        message(paste("The default output of episode_group() will be changed to epid objects in the next release.",
                      "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
                      "This message is displayed once per session.", sep = "\n"))
      }
      options("diyar.episode_group.output"= FALSE)
    }
  }

  episodes_max <- ifelse(is.numeric(episodes_max) & !is.na(episodes_max) & !is.infinite(episodes_max), as.integer(episodes_max), episodes_max)
  rolls_max <- ifelse(is.numeric(rolls_max) & !is.na(rolls_max) & !is.infinite(rolls_max), as.integer(rolls_max), rolls_max)

  if(!is.data.frame(df)) stop(paste("A dataframe is required"))
  if(!(is.logical(group_stats) & is.logical(from_last) & is.logical(display) )) stop(paste("'group_stats', 'from_last' and 'display' must be TRUE or FALSE"))
  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!((is.infinite(rolls_max) | is.integer(rolls_max) ) & (is.infinite(episodes_max) | is.integer(episodes_max)) & length(rolls_max)==1 & length(episodes_max)==1) ) stop(paste("'episodes_max' and 'rolls_max' must be, or can be coerced to an integer between 0 and Inf"))

  if(length(episode_type)!=1 | !is.character(episode_type)) stop(paste("'episode_type' must be a character of length 1"))
  if(length(episode_unit)!=1 | !is.character(episode_unit)) stop(paste("'episode_unit' must be a character of length 1"))

  if(!episode_type %in% c("rolling","fixed") ) stop(paste("`episode_type` must be either 'rolling' or 'fixed'"))

  enq_vr <- function(x){
    x <- as.character(x)
    if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
    if(length(x)==0) x <- NULL
    x
  }

  fmt <- function(g) formatC(g, format="d", big.mark=",")

  rd_sn <- enq_vr(substitute(sn))
  ds <- enq_vr(rlang::enexpr(data_source))
  epl <- enq_vr(substitute(case_length))
  r_epl <- enq_vr(substitute(recurrence_length))
  st <- enq_vr(substitute(strata))
  ref_sort <- enq_vr(substitute(custom_sort))
  dt <- enq_vr(substitute(date))

  if(any(!unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt)), !unique(c(rd_sn, ds, epl, r_epl, st, ref_sort, dt)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found"))
  }

  if(!is.null(rd_sn)){
    if(!(all(df[[rd_sn]] > 0) & is.numeric(as.numeric(df[[rd_sn]])))) stop(paste("'",rd_sn,"' as 'sn' must be > 0", sep=""))
    if(any(duplicated(df[[rd_sn]]))) stop(paste("'",rd_sn,"' as 'sn' must not have duplicate values", sep=""))
  }

  if(!( any(class(df[[epl]]) %in% c("integer","double","numeric")) & all(df[[epl]] >= -1))) stop(paste("'",epl,"' as 'case_length' must be -1 or a positive integer, numeric or double data type", sep=""))

  if(!is.null(r_epl)){
    if(!( any(class(df[[r_epl]]) %in% c("integer","double","numeric")) & all(df[[r_epl]] >= -1))) stop(paste("'",r_epl,"' as 'recurrence_length' must be -1 or a positive integer, numeric or double data type", sep=""))
  }

  if(!(any(class(df[[dt]]) %in% c("Date","POSIXct","POSIXt","POSIXlt","number_line")) & all(!is.na(df[[dt]])))) stop(paste("'",dt,"' as 'date' must be a date, datetime or number_line object, and not have missing values", sep=""))

  df_list <- names(df)

  if(is.null(rd_sn)){
    df <- dplyr::mutate(df, sn= dplyr::row_number())
  }else{
    df$sn <- dplyr::select(df, sn = !!dplyr::enquo(sn))[[1]]
  }

  if(is.null(ds)){
    df$dsvr <- "A"
  }else{
    df <- tidyr::unite(df, "dsvr", ds, remove=FALSE, sep="-")
  }

  df$epi_len <- dplyr::select(df, epi_len = !!dplyr::enquo(case_length))[[1]]

  if(is.null(r_epl) | episode_type !="rolling" ){
    df$rc_len <- df$epi_len
  }else{
    df$rc_len <- dplyr::select(df, rc_len = !!dplyr::enquo(recurrence_length))[[1]]
  }

  if(is.null(st)){
    df$cri <- "A"
  }else{
    df <- tidyr::unite(df, "cri", c(!!dplyr::enquo(strata)), remove=FALSE)
  }

  if(all(diyar::is.number_line(df[[dt]]))){
    df$rec_dt_ai <- diyar::left_point(df[[dt]])
    df$rec_dt_zi <- diyar::right_point(df[[dt]])
  }else{
    df$rec_dt_ai <- df[[dt]]
    df$rec_dt_zi <- df[[dt]]
  }

  df <- df %>%
    dplyr::select(.data$sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$epi_len, .data$rc_len, .data$dsvr, .data$cri, !!dplyr::enquo(custom_sort)) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = dplyr::row_number(), roll=0, episodes=0) %>%
    dplyr::mutate_at(c("rec_dt_ai", "rec_dt_zi"), ~ lubridate::dmy_hms(format(., "%d/%m/%Y %H:%M:%S")))

  if(from_last==TRUE){
    df$ord <- abs(max(df$rec_dt_ai) - df$rec_dt_ai)
    df$ord_z <- abs(max(df$rec_dt_zi) - df$rec_dt_zi)
  }else{
    df$ord <- abs(min(df$rec_dt_ai) - df$rec_dt_ai)
    df$ord_z <- abs(min(df$rec_dt_zi) - df$rec_dt_zi)
  }

  if(is.null(ref_sort)){
    df$user_ord <- 0
  }else{
    ord_ls <- dplyr::select(df, ref_sort) %>%
      dplyr::mutate_all(list(~as.numeric(as.factor(.)))) %>%
      dplyr::mutate_all(list(~formatC(., width = max(nchar(.)), flag=0))) %>%
      tidyr::unite("ord")

    df$user_ord <- ord_ls[[1]]
  }

  df$epid <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), "") , df$sn, df$epid)
  df$tag <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 2, df$tag)
  df$case_nm <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), "Case", df$case_nm)
  df$episodes <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), 1, 0)

  df$roll <- ifelse(df$cri %in% c(paste(rep("NA", length(st)),collapse="_"), ""), rolls_max, df$roll)

  min_tag <- min(df$tag)
  min_episodes_nm <- min(df$episodes)

  df$int_l <- diyar::number_line_width(diyar::number_line(df$rec_dt_ai, df$rec_dt_zi))
  c <- 1
  while (min_tag != 2 & min_episodes_nm <= episodes_max){
    TR <- df %>%
      # preference to exisitng episodes
      dplyr::arrange(.data$cri,  dplyr::desc(.data$tag), .data$user_ord, .data$ord, dplyr::desc(.data$int_l), .data$sn) %>%
      # exclude records that will create 1 episode more than episodes_max
      dplyr::filter(!(.data$tag==0 & .data$episodes + 1 > episodes_max )) %>%
      dplyr::filter(.data$tag !=2 & !is.na(.data$tag)) %>%
      dplyr::filter(duplicated(.data$cri) == FALSE & !is.na(.data$cri)) %>%
      dplyr::select(.data$sn, .data$cri, .data$rec_dt_ai, .data$rec_dt_zi, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len) %>%
      dplyr::rename_at(dplyr::vars(.data$sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len), list(~paste("tr_",.,sep="")))

    if(nrow(TR)==0) {break}

    if(display){cat(paste("Episode or recurrence window ",c,".\n", sep=""))}

    df <- dplyr::left_join(df, TR, by= "cri")

    df <- dplyr::mutate_at(df, c("tr_rc_len", "tr_epi_len"), ~ lubridate::duration(., units=episode_unit))

    df$c_int <- diyar::number_line(df$rec_dt_ai, df$rec_dt_zi)
    df$r_int <- diyar::number_line(df$rec_dt_ai, df$rec_dt_zi)

    df$tr_c_int <- suppressWarnings(diyar::number_line(df$tr_rec_dt_ai, df$tr_rec_dt_zi))
    df$tr_r_int <- suppressWarnings(diyar::number_line(df$tr_rec_dt_ai, df$tr_rec_dt_zi))

    bdir <- ifelse(bi_direction,"both","end")
    if (from_last==FALSE){
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, df$tr_epi_len, bdir))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, df$tr_rc_len, bdir))
    }else{
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, -df$tr_epi_len, bdir))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, -df$tr_rc_len, bdir))
    }

    df$r_range <- df$c_range <- FALSE

    df$c_range <- diyar::overlap(df$c_int, df$tr_c_int, method = overlap_method)
    df$r_range <- diyar::overlap(df$r_int, df$tr_r_int, method = overlap_method)

    if(!bi_direction & !from_last){
      df$c_range <- ifelse(df$tr_rec_dt_ai > df$rec_dt_ai, FALSE, df$c_range)
      df$r_range <- ifelse(df$tr_rec_dt_ai > df$rec_dt_ai, FALSE, df$r_range)

    }else if(!bi_direction & from_last){
      df$c_range <- ifelse(df$tr_rec_dt_ai < df$rec_dt_ai, FALSE, df$c_range)
      df$r_range <- ifelse(df$tr_rec_dt_ai < df$rec_dt_ai, FALSE, df$r_range)
    }

    df <- df %>%
      dplyr::mutate(
        epid_type = ifelse(
          .data$r_range == TRUE & !is.na(.data$r_range) & .data$tr_tag!=0 & !is.na(.data$tr_tag),
          ifelse(.data$case_nm=="Duplicate", 2,3), 0
        ),
        epid_type = ifelse(
          .data$tag==0 & .data$c_range == TRUE & !is.na(.data$c_range) & .data$tr_tag==0 & !is.na(.data$tr_tag),
          ifelse(.data$tr_sn==.data$sn & !is.na(.data$sn), 1,2), .data$epid_type
        ),
        c_hit = ifelse(.data$epid==0 & .data$epid_type %in% 1:3 | (.data$tr_sn ==.data$sn & !is.na(.data$tr_sn)), 1, 0),
        n_hit = ifelse(.data$epid==0 & .data$epid_type %in% 1:3 & .data$tr_sn !=.data$sn & !is.na(.data$tr_sn), 1, 0),
        epid = ifelse(
          .data$c_hit==1, ifelse(.data$tr_tag ==0 & !is.na(.data$tr_tag), .data$tr_sn, .data$tr_epid),
          .data$epid
        ),
        case_nm = ifelse(
          .data$c_hit==1, ifelse(.data$epid_type %in% c(1,0), "Case", ifelse(.data$epid_type==3 & episode_type=="rolling","Recurrent","Duplicate")),
          .data$case_nm
        )
      )

    tagged_1 <- length(subset(df$epid, !df$epid %in% c(0,NA) & df$tag ==0 ))
    total_1 <- length(subset(df$cri, df$tag ==0))

    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }

    df <- dplyr::select(df, -dplyr::ends_with("_range"), -dplyr::ends_with("_int"))
    df <- df %>%
      dplyr::arrange(.data$cri, .data$epid, .data$ord_z, .data$sn)

    df <- df %>%
      dplyr::mutate(
        roll = ifelse(!.data$tr_tag %in% c(NA,0,2) & !is.na(.data$tr_tag) & .data$epid!=0, .data$tr_roll + 1, .data$roll),
        episodes = ifelse(.data$tr_tag==0 & !is.na(.data$tr_tag), .data$episodes + 1, .data$episodes),
        tag = ifelse(.data$c_hit==1 | (!.data$tag %in% c(NA,0,2) & .data$sn == .data$tr_sn), 2, .data$tag),
        mrk_x = paste(.data$cri, .data$tag, sep="-"),
        mrk_y = paste(.data$epid, .data$case_nm, .data$n_hit, sep="-")
      )

    df$fg_a <- rep(rle(df$cri)$lengths, rle(df$cri)$lengths)
    df$fg_x <- rep(rle(df$mrk_x)$lengths, rle(df$mrk_x)$lengths)
    df$fg_y <- sequence(rle(df$mrk_y)$lengths)

    df$fg_c <- ifelse(df$fg_a==df$fg_x & df$tag==2, 1,0)
    df$case_nm = ifelse(df$case_nm=="Recurrent" & df$fg_y!=1 & df$n_hit==1, "Duplicate", df$case_nm)

    if(min(df$fg_c)==1) {break}

    df <- df %>%
      dplyr::mutate(
        mrk_z= ifelse(.data$c_hit==1, paste(.data$epid, .data$epid_type, sep="_"), ""),
        tag=ifelse(episode_type == "rolling" & .data$roll < rolls_max & .data$epid_type %in% 2:3 &
                     !duplicated(.data$mrk_z, fromLast=TRUE) & .data$fg_c !=1 &
                     !(.data$tr_sn == .data$sn & .data$tr_tag %in% c(1, 1.5)),
                   ifelse(.data$epid_type == 2, 1.5, 1), .data$tag)
      ) %>%
      dplyr::select( -dplyr::starts_with("tr"), -dplyr::starts_with("fg"), -dplyr::starts_with("mrk"))

    min_tag <- min(df$tag)
    min_episodes_nm <- min(df$episodes)

    c = c+1
  }
  df <- df %>%
    dplyr::mutate(
      case_nm= ifelse(.data$epid==0, "Case", .data$case_nm),
      epid= ifelse(.data$epid==0, .data$sn, .data$epid)
    )

  if(deduplicate) df <- subset(df, df$case_nm!="Duplicate")

  if(is.null(ds)){
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm, .data$pr_sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$ord, .data$ord_z, .data$epi_len)
    df <- dplyr::arrange(df, .data$pr_sn)
  }else{
    sourc_list <- as.character(sort(unique(df$dsvr)))

    df <- df %>%
      dplyr::select(.data$epid, .data$dsvr) %>%
      unique() %>%
      dplyr::mutate(val= .data$dsvr) %>%
      dplyr::arrange(.data$dsvr) %>%
      tidyr::spread(key="dsvr", value="val") %>%
      tidyr::unite("epid_dataset", sourc_list, sep=",") %>%
      dplyr::mutate(epid_dataset = gsub("NA,|,NA|^NA$","",.data$epid_dataset)) %>%
      dplyr::full_join(df, by="epid")

    df <- dplyr::arrange(df, .data$pr_sn)
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm, .data$epid_dataset, .data$pr_sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$ord, .data$ord_z, .data$epi_len)
  }

  if(group_stats){
    epid_l <- dplyr::select(df, .data$epid, .data$rec_dt_ai, .data$rec_dt_zi, .data$ord, .data$ord_z) %>%
      unique()

    epid_l <-
      dplyr::bind_rows(
        dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord), !duplicated(.data$epid)), var ="a" ),
        dplyr::mutate(dplyr::filter(dplyr::arrange(epid_l, .data$epid, .data$ord_z), !duplicated(.data$epid, fromLast = TRUE)), var ="z" )
      ) %>%
      dplyr::mutate_at(c("rec_dt_ai", "rec_dt_zi"), ~ format(., "%d/%m/%Y %H:%M:%S")) %>%
      dplyr::mutate(val = ifelse(.data$var=="a", .data$rec_dt_ai,.data$rec_dt_zi)) %>%
      dplyr::select(.data$epid, .data$var, .data$val)

    epid_l <- epid_l %>%
      tidyr::spread("var","val")

    df <- dplyr::left_join(df, epid_l, by="epid")

    diff_unit <- gsub("s$","",tolower(episode_unit))
    diff_unit <- ifelse(!diff_unit %in% c("second","minute","hour","day"), "day", diff_unit)

    df <- dplyr::mutate(df, epid_length = lubridate::make_difftime(difftime(lubridate::dmy_hms(.data$z), lubridate::dmy_hms(.data$a), units = "secs"), units = diff_unit))

    df <- dplyr::arrange(df, .data$epid)

    df$epid_total <- rep(rle(df$epid)$lengths, rle(df$epid)$lengths)
    df <- dplyr::arrange(df, .data$pr_sn)

    df <- dplyr::mutate(df, epid_interval = diyar::number_line(lubridate::dmy_hms(.data$a), lubridate::dmy_hms(.data$z), id=.data$sn, gid = .data$epid))

    df <- dplyr::select(df, -c(.data$a, .data$z))
  }

  df <- dplyr::select(df, -c(.data$pr_sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$ord, .data$ord_z, .data$epi_len))

  unique_ids <- length(df[!duplicated(df$epid) & !duplicated(df$epid, fromLast = TRUE),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste(pd, "Episode grouping complete - " ,fmt(unique_ids)," record(s) assinged a unique ID. \n" , sep =""))
  if(to_s4) df <- diyar::to_s4(df)
  df
}

#' @rdname episode_group
#' @param deduplicate if \code{TRUE}, retains only one the \code{"Case"} (C) record in an episode.
#' @param x Record date or interval. Deprecated. Please use \code{date}
#' @details
#' \code{fixed_episodes()} and \code{rolling_episodes()} are wrapper functions of \code{episode_group}.
#' They can be more convenient options and have the same functionalities as \code{episode_group()}.
#'
#' @export
fixed_episodes <- function(date, sn = NULL, strata = NULL, case_length, episode_unit = "days", episodes_max = Inf, data_source = NULL, custom_sort = NULL,
                           from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"),
                           bi_direction= FALSE, group_stats = FALSE, display = TRUE, deduplicate = FALSE, x, to_s4 = FALSE){
  if(to_s4 == FALSE){
    if (is.null(getOption("diyar.fixed_episodes.output"))){
      options("diyar.fixed_episodes.output"= TRUE)
    }
    if (getOption("diyar.fixed_episodes.output")){
      message(paste("The default output of fixed_episodes() will be changed to epid objects in the next release.",
                    "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
                    "This message is displayed once per session.", sep = "\n"))
    }
    options("diyar.fixed_episodes.output"= FALSE)
  }
  if (!missing(x)) {
    warning("'x' is deprecated; please use 'date' instead."); date <- x
    }

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(date))) stop(paste("All 'date' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))

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
    df$ds <- 1
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

  diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "fixed", episodes_max = episodes_max,
                       bi_direction = bi_direction , data_source = !!ds, custom_sort = "user_srt",
                       from_last = from_last, overlap_method = overlap_method,
                       display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate,to_s4 = to_s4)
}

#' @rdname episode_group
#' @export
rolling_episodes <- function(date, sn = NULL, strata = NULL, case_length, recurrence_length=NULL, episode_unit = "days", episodes_max = Inf, rolls_max = Inf, data_source = NULL, custom_sort = NULL,
                           from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"),
                           bi_direction= FALSE, group_stats = FALSE, display = TRUE, deduplicate = FALSE, x, to_s4 = FALSE){

  if(to_s4 == FALSE){
    if (is.null(getOption("diyar.rolling_episodes.output"))){
      options("diyar.rolling_episodes.output"= TRUE)
    }
    if (getOption("diyar.rolling_episodes.output")){
      message(paste("The default output of rolling_episodes() will be changed to epid objects in the next release.",
                    "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
                    "This message is displayed once per session.", sep = "\n"))
    }
    options("diyar.rolling_episodes.output"= FALSE)
  }
  if (!missing(x)) {
    warning("'x' is deprecated; please use 'date' instead."); date <- x
  }

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!(all(is.finite(recurrence_length) & recurrence_length >= 0) | is.null(recurrence_length)) ) stop(paste("'recurrence_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(date))) stop(paste("All 'date' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(date)))) stop(paste("length of 'case_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(date)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'date'",sep=""))
  if(!(length(strata) %in% c(1, length(date)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'date'",sep=""))
  if(!(length(data_source) %in% c(1, length(date)) | (length(data_source) ==0 & is.null(data_source)))) stop(paste("length of 'data_source' must be 1 or the same as 'date'",sep=""))
  if(!(length(custom_sort) %in% c(1, length(date)) | (length(custom_sort) ==0 & is.null(custom_sort)))) stop(paste("length of 'custom_sort' must be 1 or the same as 'date'",sep=""))

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
    df$ds <- 1
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

  diyar::episode_group(df, sn=sn, date = "dts", strata= "sr", case_length = "epl", episode_type = "rolling", episodes_max = episodes_max,
                       bi_direction = bi_direction , data_source = !!ds, custom_sort = "user_srt",
                       from_last = from_last, overlap_method = overlap_method, recurrence_length = "rc_epl", rolls_max = rolls_max,
                       display = display, episode_unit = episode_unit, group_stats = group_stats, deduplicate = deduplicate, to_s4 = to_s4)
}
