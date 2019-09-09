#' @title Episode grouping for record deduplication and case assignment
#'
#' @description This function assigns unique episode indentifiers to chronological episodes
#'
#' @param df Dataframe. One or more datasets appended together.
#' @param sn Unique \code{numeric} record indentifier.
#' @param strata Column names. Episodes will be unique to each strata. See \code{\link{record_group}}.
#' @param date Record date or interval. \code{date}, \code{datetime} or \code{\link[lubridate]{interval}} objects.
#' @param case_length Duration from the \code{"Case"} within which another record of the same \code{strata} will be considered a \code{"Duplicate"} record.
#' @param episodes_max Maximum number times to group episodes in each strata.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Duration from the last record of an episode within which another record of the same \code{strata} will be considered a \code{"Recurrent"} record.
#' @param episode_unit Time units as supported by \code{\link[lubridate]{duration}}.
#' @param rolls_max Maximum number of recurrence periods permitted within each episode. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset indentifier for the dataframe. Useful when dataframe contains multiple datsets.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent record and proceeding to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest record and proceeding to the most recent.
#' @param overlap_method A set methods for grouped intervals to overlap. Options are; \code{"across"}, \code{"aligns_start"}, \code{"aligns_end"}, \code{"inbetween"}, \code{"chain"}.
#' @param custom_sort If \code{TRUE}, \code{"Case"} assignment will be in preference to this sort order. Useful in specifying that episode grouping begins at a particular kind of record regardless of chronological order.
#' @param bi_direction If \code{FALSE}, \code{"Duplicate"} records are those within the \code{case_length} and \code{recurrence_length}, before or after the \code{"Case"} as determined by \code{from_last}. If \code{TRUE}, \code{"Duplicate"} records are those on either side of the \code{"Case"}.
#' @param group_stats If \code{TRUE}, output will include additional columns with useful stats for each episode.
#' @param display If \code{TRUE}, status messages are not printed on screen.
#'
#' @return Dataframe
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{epid} - unique episode indentifier
#' \item \code{case_nm} - record type in regards to case assignment
#' \item \code{epid_dataset} - datasets contained in each episode
#' \item \code{epid_interval} - lubridate \code{interval} object. Episode start and end dates
#' \item \code{epid_length} - \code{difftime} object. Difference between episode start and end dates. Unit will match that supplied as \code{episode_unit} if possible othewrise, a difference in days is returned
#' \item \code{epid_total} - number of records in each record group
#' }
#'
#' @seealso
#' \code{\link{record_group}}
#'
#' @details
#' Episode grouping begins at a reference record (\code{"Case"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain record before proceeding forward or backwards in time.
#' The maximun duration of a \code{"fixed"} episode is the \code{case_length} while, the maximum duration of a \code{"rolling"} episode is the
#' \code{case_length} in addition to all recurrence periods. A recurrence period is the \code{recurrence_length} from the last record in an episode
#'
#' @examples
#' library(lubridate)
#' library(dplyr)
#'
#' hospital_infections <- tibble(
#'   rd_id = c(1:11),
#'   date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="6 days"),
#'   infection = c("BSI", rep("UTI",2), "UTI", "BSI",  "UTI", rep("BSI",2), "RTI","RTI","BSI"),
#'   epi_len = 15
#' )
#'
#' hospital_infections
#'
#' # 16-day (difference of 15 days) episodes, and the earliest record defined as the "Case"
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len)
#'           ) %>% select(-sn)
#'
#' # 16-hour (difference of 15 hours) episodes, and the most recent record defined as the "Case"
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
#'                         from_last = TRUE, episode_unit = "hours", display = FALSE)
#'           ) %>% select(-sn)
#'
#' # 15-week (difference of 9072000 seconds) episodes , and the most recent record defined as the "Case"
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, sn=rd_id, date = date, case_length = epi_len,
#'                         from_last = TRUE, episode_unit = "weeks", display = FALSE)
#'           ) %>% select(-sn)
#'
#' # 16-day (difference of 15 days) rolling episodes with a periods of recurrence, each lasting 31 days (difference of 30 days)
#' hospital_infections$recur <- 30
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len, episode_type = "rolling",
#'                         recurrence_length = recur, display = FALSE)
#'           ) %>% select(-sn)
#'
#' # 16-day (difference of 15 days) rolling episodes with only one period of recurrence lasting 31 days (difference of 30 days)
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len, episode_type = "rolling",
#'                         recurrence_length = recur, rolls_max = 1, display = FALSE)
#'           ) %>% select(-sn)
#'
#' # Only one 16-day (difference of 15 days) episode
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len, episode_type = "fixed",
#'                         recurrence_length = recur, episodes_max = 1, from_last = FALSE, display = FALSE)
#'           ) %>% select(-sn)
#'
#' # User defined case assignment
#' # preference for case assignment - UTI > BSI > RTI
#' hospital_infections$infection <- factor(hospital_infections$infection, levels = c("UTI","BSI","RTI"))
#'
#' # Different case and recurrence lengths for different source of infection
#' hospital_infections <- mutate(
#'   hospital_infections,
#'   epi_len = case_when(
#'     infection == "BSI" ~ 14,
#'     infection == "UTI" ~ 30,
#'     infection == "RTI" ~ 60
#'   )
#' )
#'
#' # n-day episodes beginning with the earliest record with the specified preference; UTI > BSI > RTI
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
#'                         custom_sort = infection,  display = FALSE)
#'           ) %>% select(-sn)
#'
#' # preference for case assignment - BSI > UTI, or  BSI > RTI, or earliest record
#' hospital_infections$infection_ord <- ifelse(hospital_infections$infection =="UTI",0,1)
#'
#' # n-day episodes beginning with the earliest "BSI" record, otherwise begin at the earliest record
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
#'                         custom_sort = infection_ord, group_stats = TRUE, display = FALSE)
#'           ) %>% select(-sn)
#'
#' # preference for case assignment - RTI > UTI, or  RTI > BSI, or earliest record
#' hospital_infections$infection_ord <- ifelse(hospital_infections$infection =="RTI",0,1)
#'
#' # n-day episodes with duplicates before and after the most recent "RTI" record, otherwise begin at the most recent record
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, case_length = epi_len,
#'                         custom_sort = infection_ord, from_last = TRUE, bi_direction = TRUE, display = FALSE)
#'           ) %>% select(-sn)
#'
#' # Stratified episode grouping
#' hospital_infections$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#'
#' # Only one n-day episode per patient_id
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, strata = patient_id, case_length = epi_len,
#'                         episodes_max = 1, from_last = FALSE, display = FALSE, data_source = infection)
#'           ) %>% select(-sn)
#'
#' # Only three 9-day (difference of 8 days) rolling episode per patient and infection.
#' hospital_infections$epi_len <- 8
#' bind_cols(hospital_infections,
#'           episode_group(hospital_infections, rd_id, date=date, strata = c(patient_id, infection), case_length = epi_len,
#'                         episode_type = "rolling", recurrence_length = recur, episodes_max = 3, data_source = c(patient_id, infection),
#'                         display = FALSE)
#'           ) %>% select(-sn)
#'
#' hospital_admissions <- tibble(
#'   rd_id = 1:9,
#'   admin_dt = c(dmy("01/01/2010"), dmy("01/01/2010"), dmy("10/01/2010"), dmy("05/01/2010"),
#'                dmy("05/01/2010"), dmy("07/01/2010"), dmy("04/01/2010"),
#'                dmy("20/01/2010"), dmy("26/01/2010")),
#'   discharge_dt = c(dmy("01/01/2010"), dmy("10/01/2010"), dmy("13/01/2010"), dmy("06/01/2010"),
#'                    dmy("15/01/2010"), dmy("15/01/2010"), dmy("13/01/2010"),
#'                    dmy("30/01/2010"), dmy("31/01/2010"))
#' )
#'
#' hospital_admissions$epi_len <- 0
#' hospital_admissions$admin_period <- interval(hospital_admissions$admin_dt, hospital_admissions$discharge_dt)
#' hospital_admissions
#'
#' # 1-day (difference of 0 days) episodes of hospital admissions
#' bind_cols(
#'   hospital_admissions,
#'   episode_group(hospital_admissions, date=admin_dt, sn=rd_id, case_length = epi_len, group_stats = TRUE)
#'   ) %>% select(-admin_period, sn)
#'
#' # episodes of overlaping intervals of admission
#' bind_cols(
#'   hospital_admissions,
#'   episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, group_stats = TRUE)
#'   ) %>% select(-c(admin_dt, discharge_dt, sn))
#'
#' # rolling episodes of overlaping intervals of admission, and those within 10 days of the last interval
#' hospital_admissions$epi_len <- 0
#' hospital_admissions$recur <- 1
#' bind_cols(
#'   hospital_admissions,
#'   episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len,
#'                 episode_type = "rolling", recurrence_length = recur, episode_unit = "months")
#'   ) %>% select(-c(admin_dt, discharge_dt, sn))
#'
#' # fixed episodes of overlaping intervals of admission seperated by 1 month
#' hospital_admissions$epi_len <- 1
#' bind_cols(hospital_admissions,
#'           episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, episode_unit = "months")
#'           ) %>% select(-c(admin_dt, discharge_dt, sn))
#'
#' # Episodes of intervals within other intervals
#' hospital_admissions$epi_len <- 0
#' bind_cols(
#'   hospital_admissions,
#'   episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, overlap_method = "inbetween")
#'   ) %>% select(-c(admin_dt, discharge_dt, sn))
#'
#' #Episodes of chained intervals episodes, and those with aligned end periods
#' hospital_admissions$epi_len <- 0
#' bind_cols(
#'   hospital_admissions,
#'   episode_group(hospital_admissions, date=admin_period, sn=rd_id, case_length = epi_len, overlap_method = c("chain","aligns_end"))
#'   ) %>% select(-c(admin_dt, discharge_dt, sn))
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#' @export
#'

episode_group <- function(df, sn = NULL, strata = NULL, date,
                          case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL,
                          custom_sort = NULL, from_last=FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), bi_direction = FALSE, group_stats= FALSE, display=TRUE){

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

  enq_vr <- function(x, vr){
    x <- names(dplyr::select(x, !!vr))

    if(length(x)==0){
      x <- NULL
    }else{
      x
    }
    return(x)
  }

  fmt <- function(g) formatC(g, format="d", big.mark=",")

  rd_sn <- enq_vr(df, dplyr::enquo(sn))
  ds <- enq_vr(df, dplyr::enquo(data_source))
  epl <- enq_vr(df, dplyr::enquo(case_length))
  r_epl <- enq_vr(df, dplyr::enquo(recurrence_length))
  st <- enq_vr(df, dplyr::enquo(strata))
  ref_sort <- enq_vr(df, dplyr::enquo(custom_sort))
  dt <- enq_vr(df, dplyr::enquo(date))

  if(!is.null(rd_sn)){
    if(!(all(df[[rd_sn]] > 0) & is.numeric(as.numeric(df[[rd_sn]])))) stop(paste("'",rd_sn,"' as 'sn' must be > 0", sep=""))
    if(any(duplicated(df[[rd_sn]]))) stop(paste("'",rd_sn,"' as 'sn' must not have duplicate values", sep=""))
  }

  if(!( any(class(df[[epl]]) %in% c("integer","double","numeric")) & all(df[[epl]] >= -1))) stop(paste("'",epl,"' as 'case_length' must be -1 or a positive integer, numeric or double data type", sep=""))

  if(!is.null(r_epl)){
    if(!( any(class(df[[r_epl]]) %in% c("integer","double","numeric")) & all(df[[r_epl]] >= -1))) stop(paste("'",r_epl,"' as 'recurrence_length' must be -1 or a positive integer, numeric or double data type", sep=""))
    }

  if(!(any(class(df[[dt]]) %in% c("Date","POSIXct","POSIXt","POSIXlt","Interval")) & all(!is.na(df[[dt]])))) stop(paste("'",dt,"' as 'date' must be a date, datetime or lubridate interval, and not have missing values", sep=""))

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

  if(lubridate::is.interval(df[[dt]])){
  df$rec_dt_ai <- lubridate::int_start(df[[dt]])
  df$rec_dt_zi <- lubridate::int_end(df[[dt]])
  df$interval <- df[[dt]]
  }else{
    df$rec_dt_ai <- df[[dt]]
    df$rec_dt_zi <- df[[dt]]
  }

  df <- df %>%
    dplyr::select(.data$sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$epi_len, .data$rc_len, .data$dsvr, .data$cri, !!dplyr::enquo(custom_sort)) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = dplyr::row_number(), roll=0, episodes=0)

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
      dplyr::mutate_all(dplyr::funs(as.numeric(as.factor(.)))) %>%
      dplyr::mutate_all(dplyr::funs(stringr::str_pad(., width = max(stringr::str_length(.)), pad=0) )) %>%
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

  df$int_l <- lubridate::int_length(lubridate::interval(df$rec_dt_ai, df$rec_dt_zi))
  c <- 1
  while (min_tag != 2 & min_episodes_nm <= episodes_max){
      TR <- df %>%
        # preference to those tagged already i.e. exisitng episodes
        dplyr::arrange(.data$cri,  dplyr::desc(.data$tag), .data$user_ord, .data$ord, dplyr::desc(.data$int_l), .data$sn) %>%
        #exclude records that will create 1 episode more than episodes_max
        dplyr::filter(!(.data$tag==0 & .data$episodes + 1 > episodes_max )) %>%
        dplyr::filter(.data$tag !=2 & !is.na(.data$tag)) %>%
        dplyr::filter(duplicated(.data$cri) == FALSE & !is.na(.data$cri)) %>%
        dplyr::select(.data$sn, .data$cri, .data$rec_dt_ai, .data$rec_dt_zi, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len) %>%
        dplyr::rename_at(dplyr::vars(.data$sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len), dplyr::funs(paste("tr_",.,sep="")))

    if(nrow(TR)==0) {break}

    if(display){cat(paste("Episode or recurrence window",c,".\n"), sep=" ")}

    df <- dplyr::left_join(df, TR, by= "cri")

    df <- dplyr::mutate_at(df, c("tr_rc_len", "tr_epi_len"), ~ lubridate::duration(., units=episode_unit))

    df$c_int <- lubridate::interval(df$rec_dt_ai, df$rec_dt_zi)
    df$r_int <- lubridate::interval(df$rec_dt_ai, df$rec_dt_zi)

    if (from_last==FALSE){
      df$tr_c_int <- lubridate::interval(df$tr_rec_dt_ai, df$tr_rec_dt_zi + df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_rec_dt_ai, df$tr_rec_dt_zi + df$tr_rc_len)

    }else{
      df$tr_c_int <- lubridate::interval(df$tr_rec_dt_ai, df$tr_rec_dt_zi - df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_rec_dt_ai, df$tr_rec_dt_zi - df$tr_rc_len)
    }

    if (bi_direction==TRUE){
      df$tr_c_int <- lubridate::interval(df$tr_rec_dt_ai - lubridate::duration(df$tr_epi_len, units=episode_unit), df$tr_rec_dt_zi + df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_rec_dt_ai - lubridate::duration(df$tr_rc_len, units=episode_unit), df$tr_rec_dt_zi + df$tr_rc_len)
    }

    df$r_range <- df$c_range <- FALSE

    df$c_range <- diyar::overlap(diyar::number_line(lubridate::int_start(df$c_int), lubridate::int_end(df$c_int)),
                   diyar::number_line(lubridate::int_start(df$tr_c_int), lubridate::int_end(df$tr_c_int)), method = overlap_method )

    df$r_range <- diyar::overlap(diyar::number_line(lubridate::int_start(df$r_int), lubridate::int_end(df$r_int)),
                              diyar::number_line(lubridate::int_start(df$tr_r_int), lubridate::int_end(df$tr_r_int)), method = overlap_method )

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
      dplyr::mutate(epid_dataset = stringr::str_replace_all(.data$epid_dataset,"NA,|,NA|^NA$","")) %>%
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

    diff_unit <- stringr::str_replace(tolower(episode_unit), "s$","")
    diff_unit <- ifelse(!diff_unit %in% c("second","minute","hour","day"), "day", diff_unit)

    df <- dplyr::mutate(df, epid_interval = lubridate::interval(lubridate::dmy_hms(.data$a), lubridate::dmy_hms(.data$z) ))
    df <- dplyr::mutate(df, epid_length = lubridate::make_difftime(difftime(lubridate::dmy_hms(.data$z), lubridate::dmy_hms(.data$a), units = "secs"), units = diff_unit))

    df <- dplyr::arrange(df, .data$epid)

    df$epid_total <- rep(rle(df$epid)$lengths, rle(df$epid)$lengths)
    df <- dplyr::arrange(df, .data$pr_sn)

    df <- dplyr::select(df, -c(.data$a, .data$z))
    }

  df <- dplyr::select(df, -c(.data$pr_sn, .data$rec_dt_ai, .data$rec_dt_zi, .data$ord, .data$ord_z, .data$epi_len))

  unique_ids <- length(df[!duplicated(df$epid) & !duplicated(df$epid, fromLast = TRUE),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste(pd, "Episode grouping complete - " ,fmt(unique_ids)," record(s) assinged a unique ID. \n" , sep =""))
  df
}

