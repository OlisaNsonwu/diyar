#' @name episode_group
#' @title Episode grouping for record deduplication and case assignment
#'
#' @description Group records into chronological episodes
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique \code{numeric} record indentifier. Optional.
#' @param strata Column names. Episodes will be unique to each \code{strata}. \code{\link{record_group}} can be used to create \code{strata} within datasets.
#' @param date Record date or interval. \code{date}, \code{datetime} or \code{\link{number_line}} objects.
#' @param case_length Period from a \code{"Case"} within which another record of the same \code{strata} is considered a \code{"Duplicate"} record.
#' @param episodes_max Maximum number of times to group episodes within each \code{strata}.
#' @param episode_type \code{"fixed"} or \code{"rolling"}.
#' @param recurrence_length Period from the last record of an episode within which another record of the same \code{strata} is considered a \code{"Recurrent"} record. If a \code{recurrence_length} is not supplied, the \code{case_length} is used.
#' @param episode_unit Time units as supported by lubridate's \code{\link[lubridate]{duration}} function.
#' @param rolls_max Maximum number of recurrence permitted within each episode. Only used if \code{episode_type} is \code{"rolling"}.
#' @param data_source Unique dataset indentifier for the \code{data.frame}. Useful when \code{data.frame} contains multiple datsets.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent record and proceeding to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest record and proceeding to the most recent one.
#' @param overlap_method A set of methods for grouped intervals to overlap. Options are; \code{"across"}, \code{"aligns_start"}, \code{"aligns_end"}, \code{"inbetween"}, \code{"chain"}. See \code{\link{overlap}} functions.
#' @param custom_sort If \code{TRUE}, \code{"Case"} assignment will be in preference to this sort order. Useful in specifying that episode grouping begins at a particular kind of record regardless of chronological order.
#' @param bi_direction If \code{FALSE}, \code{"Duplicate"} records will be those within the \code{case_length} and \code{recurrence_length}, before or after the \code{"Case"} as determined by \code{from_last}. If \code{TRUE}, \code{"Duplicate"} records will be those on both sides of the \code{"Case"}.
#' @param group_stats If \code{TRUE}, the output will include additional columns with useful stats for each episode group.
#' @param display If \code{TRUE}, status messages are printed on screen.
#'
#' @return \code{record_group()} - \code{data.frame}
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{epid} - unique episode indentifier
#' \item \code{case_nm} - record type in regards to case assignment
#' \item \code{epid_dataset} - datasets contained in each episode
#' \item \code{epid_interval} - Episode start and end dates. Lubridate's \code{\link{number_line}} object.
#' \item \code{epid_length} - Difference between episode start and end dates. \code{difftime} object. If possible, the same unit supplied to \code{episode_unit} is used otherwise, a difference in days is returned
#' \item \code{epid_total} - number of records in each record group
#' }
#'
#' @seealso
#' \code{\link{record_group}}, \code{\link{overlap}} and \code{\link{number_line}}
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
#' library(diyar)
#'
#' data(infections); infections
#'
#' # 16-hour (difference of 15 hours) episodes beginning from the earliest record
#' epids <- episode_group(infections, sn = rd_id, date = date, case_length = epi_len,
#' from_last = TRUE, episode_unit = "hours", group_stats = TRUE)
#' left_join(infections, epids, by=c("rd_id"="sn"))
#'
#' # One rolling episode per strata. Initial case_length of 16 days (difference of 15 days) and
#' # one recurrence period of 31 days (difference of 30 days)
#' infections$recur <- 30
#' epids <- episode_group(infections, date=date, case_length = epi_len, episode_type = "rolling",
#' recurrence_length = recur, episodes_max = 1, rolls_max = 1, display = FALSE, group_stats = TRUE)
#' bind_cols(infections, epids)
#'
#' # User defined case assignment
#' # Preference for case assignment - UTI > BSI > RTI
#' infections$infx <- factor(infections$infection, levels = c("UTI","BSI","RTI"))
#'
#' # Different case and recurrence lengths for different sources of infection
#' infections <- mutate(infections,
#'                      epi_len = case_when(
#'                        infection == "BSI" ~ 14,
#'                        infection == "UTI" ~ 30,
#'                        infection == "RTI" ~ 60
#'                      )
#' )
#'
#' # n-day episodes beginning with the earliest record with the specified preference; UTI > BSI > RTI
#' epids <- episode_group(infections, rd_id, date=date, case_length = epi_len,
#'                        custom_sort = infx, group_stats = TRUE,  display = FALSE)
#' bind_cols(infections, epids)
#'
#' # Another preference - RTI > UTI, or  RTI > BSI, or earliest record
#' infections$infx <- ifelse(infections$infection =="RTI",0,1)
#' epids <- episode_group(infections, rd_id, date=date, case_length = epi_len,
#' custom_sort = infx, from_last = TRUE, bi_direction = TRUE, display = FALSE, group_stats = TRUE)
#' bind_cols(infections, epids)
#'
#' # Stratified episode grouping
#' infections$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#'
#' # Only three 9-day (difference of 8 days) rolling episodes per patient and infection.
#' infections$epi_len <- 8
#' epids <- episode_group(infections, rd_id, date=date, strata = c(patient_id, infection),
#' case_length = epi_len, episode_type = "rolling", recurrence_length = recur, episodes_max = 3,
#' data_source = c(patient_id, infection), display = FALSE)
#'
#' bind_cols(infections, epids)
#'
#' # Interval grouping
#'
#' data(hospital_admissions); hospital_admissions
#'
#' hospital_admissions$admin_period <- number_line(hospital_admissions$admin_dt,
#' hospital_admissions$discharge_dt)
#' hospital_admissions <- select(hospital_admissions, -c(discharge_dt, admin_dt))
#'
#' # Episodes of overlaping intervals of admission
#' epids <- episode_group(hospital_admissions, date=admin_period,
#' sn=rd_id, case_length = epi_len, group_stats = TRUE)
#' bind_cols(hospital_admissions, epids)
#'
#' # Overlaping intervals of admission seperated by 1 month
#' hospital_admissions$epi_len <- 1
#' epids <- episode_group(hospital_admissions, date=admin_period, sn=rd_id,
#' case_length = epi_len, episode_unit = "months")
#' bind_cols(hospital_admissions, epids)
#'
#' # Episodes of chained intervals, and those with aligned end periods
#' hospital_admissions$epi_len <- 0
#' epids <- episode_group(hospital_admissions, date=admin_period, sn=rd_id,
#' case_length = epi_len, overlap_method = c("chain","aligns_end"))
#' bind_cols(hospital_admissions, epids)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data

#' @aliases episode_group
#' @export
episode_group <- function(df, sn = NULL, strata = NULL, date,
                          case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL,
                          custom_sort = NULL, from_last=FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), bi_direction = FALSE, group_stats= FALSE, display=TRUE){
  . <- NULL
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
      dplyr::mutate_all(list(~as.numeric(as.factor(.)))) %>%
      dplyr::mutate_all(list(~stringr::str_pad(., width = max(stringr::str_length(.)), pad=0) )) %>%
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

    if (from_last==FALSE){
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, df$tr_epi_len, "end"))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, df$tr_rc_len, "end"))
    }else{
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, -df$tr_epi_len, "end"))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, -df$tr_rc_len, "end"))
    }

    if (bi_direction==TRUE){
      df$tr_c_int <-  suppressWarnings(diyar::expand_number_line(df$tr_c_int, df$tr_epi_len, "both"))
      df$tr_r_int <-  suppressWarnings(diyar::expand_number_line(df$tr_r_int, df$tr_rc_len, "both"))
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
  df
}


#' @rdname episode_group
#' @param x Record date or interval. \code{date}, \code{datetime}, \code{number_line} objects or other \code{numeric} based objects.
#' @param deduplicate if \code{TRUE}, retains only one the \code{"Case"} from an episode group.
#'
#' @details
#' \code{fixed_episodes()} and \code{rolling_episodes()} are more convenient implementations of \code{episode_group}.
#' However, these are less efficient in deaing with large datasets, and lack the following features;
#' \code{"custom_sort", "rolls_max", "episodes_max", "data_source", "episode_unit", "bi_direction" and "group_stats"}
#'
#' @return \code{fixed_episodes() and rolling_episodes()} - \code{number_line}.
#' \itemize{
#' \item \code{id} - unique record identifier as provided
#' \item \code{gid} - unique episode indentifier
#' \item \code{start} - Episode start dates
#' \item \code{.Data} - Difference between episode start and end dates. \code{numeric} object
#' }
#'
#' Use \code{\link{number_line_width}} to extract the \code{epid_interval}
#'
#' Use \code{\link{right_point}} to extact the episode end date
#'
#' @examples
#' # Convenient versions of episode_group()
#' # Episodes from time points
#' dts <- c("13/04/2019", "01/04/2019", "05/05/2019", "10/04/2019", "01/05/2019")
#' dts <- as.Date(dts, "%d/%m/%Y")
#' dts
#'
#' epids <- fixed_episodes(dts, case_length = 5, display = FALSE)
#' epids; str(epids); unique(epids)
#'
#' # Episodes from time periods
#' pds <- as.number_line(dts)
#' pds <- expand_number_line(pds, 1, "end")
#' pds
#'
#' epids <- rolling_episodes(pds, case_length = 5, recurrence_length =11,
#' deduplicate = TRUE, display = FALSE)
#' epids; str(epids)
#'
#' db_a <- infections
#' db_b <- mutate(db_a, epid_interval= fixed_episodes(x = date, case_length = epi_len,
#' strata = infection, from_last = FALSE, display = FALSE, deduplicate = FALSE))
#'
#' db_b
#' str(db_b$epid_interval)
#' db_b$epid <- db_b$epid_interval$gid
#' db_b
#'
#' @export
fixed_episodes <- function(x, strata = NULL, case_length, episodes_max = Inf, from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), deduplicate = FALSE, display = TRUE){

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(x))) stop(paste("All 'x' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(x)))) stop(paste("length of 'case_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(strata) %in% c(1, length(x)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'x'",sep=""))

  fmt <- function(g) formatC(g, format="d", big.mark=",")
  if(any(!diyar::is.number_line(x))){
    x  <- diyar::as.number_line(x)
  }

  x@gid <- 1:length(x)

  if(is.null(strata)){
    x <- sort.number_line(x, decreasing = from_last)
    s <- rep(1, length(x))
  }else{
    x <- data.frame(id= x@id, s = strata, l = x@start, nl = x)
    db <- x[order(x$s, x$l, x$id, decreasing = from_last),]
    x <- db$nl
    s <- as.numeric(as.factor(db$s))
    rm(db)
  }

  pr_sn <- diyar::number_line(0,0, gid =x@gid, id =x@id)

  if(any(duplicated(x@id) | is.na(x@id))) stop(paste("'id' slot of the number_line object must be contain unique finite numbers",sep=""))

  x@gid <- x@id
  x <- diyar::reverse_number_line(x, "decreasing")

  j <- 0
  t <- rep(0, length(x))
  pt <- ifelse(from_last,"start","end")
  if(length(case_length)==1) case_length <- rep(case_length, length(x@id))
  while (min(t) ==0 & j!=length(x)){
    total_1 <- length(t[t==0])
    if(display){cat(paste("Episode window ",j+1,".\n", sep=""))}
    l <- x[t==0][1]
    l_l <- case_length[t==0][1]
    l_s <- s[t==0][1]
    h <- (x@id == l@id | (diyar::overlap(diyar::expand_number_line(l, l_l, pt), x, method = overlap_method) & s ==l_s)) & t != 1
    x[which(h)]@.Data <- as.numeric(max(diyar::right_point(x[which(h),]))) - as.numeric(min(x[which(h),]@start))
    x[which(h)]@start <- min(x[which(h),]@start)
    x[which(h)]@gid <- l@gid

    tagged_1 <- length(h[h])
    t[which(h)] <- 1
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }
    if(min(t)==1) break
    j <- j + 1
  }

  x@id <- pr_sn@id
  db <- data.frame(nl=x, sn=pr_sn@gid)
  x <- db[order(db$sn),]$nl
  rm(db)

  if(deduplicate) x <- unique.number_line(x)
  if(from_last) x <- diyar::reverse_number_line(x)
  return(x)
}

#' @rdname episode_group
#' @export
rolling_episodes <- function(x,  strata=NULL, case_length, recurrence_length=NULL, from_last = FALSE, overlap_method = c("across","inbetween","aligns_start","aligns_end","chain"), deduplicate = FALSE, display = TRUE){

  if(!is.character(overlap_method)) stop(paste("'overlap_method' must be a character object"))
  if(all(!tolower(overlap_method) %in% c("across","chain","aligns_start","aligns_end","inbetween"))) stop(paste("`overlap_method` must be either 'across','chain','aligns_start','aligns_end' or 'inbetween'"))
  if(!(is.logical(from_last) & is.logical(display) & is.logical(deduplicate) )) stop(paste("'from_last', 'deduplicate' and 'display' must be TRUE or FALSE"))
  if(!all(is.finite(case_length) & case_length >= 0) ) stop(paste("'case_length' must be a numeric based object of length 1",sep=""))
  if(!(all(is.finite(recurrence_length) & recurrence_length >= 0) | is.null(recurrence_length)) ) stop(paste("'recurrence_length' must be a numeric based object of length 1",sep=""))
  if(!all(is.finite(x))) stop(paste("All 'x' values must be a date, datetime, number_line object or a numeric based object",sep=""))
  if(!(length(case_length) %in% c(1, length(x)))) stop(paste("length of 'case_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(recurrence_length) %in% c(1, length(x)) | (length(recurrence_length) ==0 & is.null(recurrence_length)))) stop(paste("length of 'recurrence_length' must be 1 or the same as 'x'",sep=""))
  if(!(length(strata) %in% c(1, length(x)) | (length(strata) ==0 & is.null(strata)))) stop(paste("length of 'strata' must be 1 or the same as 'x'",sep=""))

  fmt <- function(g) formatC(g, format="d", big.mark=",")
  if(is.null(recurrence_length)){
    recurrence_length <- case_length
  }

  if(any(!diyar::is.number_line(x))){
    x  <- diyar::as.number_line(x)
  }

  x@gid <- 1:length(x)
  if(is.null(strata)){
    x <- sort.number_line(x, decreasing = from_last)
    s <- rep(1, length(x))
  }else{
    x <- data.frame(id= x@id, s = strata, l = x@start, nl = x)
    if(from_last) x$l <- -as.numeric(x$l)
    db <- x[order(x$s, x$l, x$id),]
    x <- db$nl
    s <- as.numeric(as.factor(db$s))
    rm(db)
  }

  pr_sn <- diyar::number_line(0,0, gid =x@gid, id =x@id)
  if(any(duplicated(x@id) | is.na(x@id))) stop(paste("'id' slot of the number_line object must be contain unique finite numbers",sep=""))
  x@gid <- x@id

  x <- diyar::reverse_number_line(x, "decreasing")

  x@id <- 1:length(x@id)

  j <- 0
  l <- NULL
  t <- rep(0, length(x))
  pt <- ifelse(from_last,"start","end")
  while (min(t) ==0 & j!=length(x)){
    total_1 <- length(t[t==0])
    if(display){cat(paste("Episode window ",j+1,".\n", sep=""))}

    if(length(l)==0 | is.null(l)){
      l <- x[t==0][1]
      l_s <- s[t==0][1]
      e <- case_length
    }else{
      e <- recurrence_length
    }

    h <- (x@id == l@id | (diyar::overlap(diyar::expand_number_line(l, by=e, pt), x, method = overlap_method) & s==l_s))
    x[which(h)]@.Data <- as.numeric(max(diyar::right_point(x[which(h),]))) - as.numeric(min(x[which(h),]@start))
    x[which(h)]@start <- min(x[which(h),]@start)
    x[which(h)]@gid <- l@gid

    if(min(t[which(h)])==1){
      l <- NULL
    }else{
      l <- x[which(h)]
      l <- l[length(l)]

      l_s <- s[which(h)]
      l_s <- l_s[length(l_s)]
    }

    tagged_1 <- length(h[h & t==0])
    t[which(h)] <- 1
    if(display){
      cat(paste(fmt(tagged_1)," of ", fmt(total_1)," record(s) grouped into episodes. ", fmt(total_1-tagged_1)," records not yet grouped.\n", sep =""))
    }
    if(min(t)==1) break
    j <- j + 1
  }

  x@id <- pr_sn@id
  db <- data.frame(nl=x, sn=pr_sn@gid)
  db <- db[order(db$sn),]
  x <- db$nl
  rm(db)
  if(deduplicate) x <- unique.number_line(x)

  if(from_last) x <- diyar::reverse_number_line(x)
  return(x)
}
