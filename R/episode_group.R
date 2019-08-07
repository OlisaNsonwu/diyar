#' @title Episode grouping for case definitions
#'
#' @description This function assigns records into unique chronological episodes
#'
#' @param df Dataframe. One or more datasets appened together.
#' @param sn \code{numeric} unique record indentifier for the dataframe.
#' @param strata Column names. Episodes will be unique to each strata. \code{\link{record_group}}.
#' @param date Record date. \code{date} or \code{datetime}.
#' @param case_length Duration from the \code{"Case"} within which another record of the same \code{strata} will be considered a \code{"Duplicate"}.
#' @param episodes_max Maximum number of episodes permitted in each strata.
#' @param episode_type \emph{"fixed"} or \emph{"rolling"}.
#' @param recurrence_length Duration from the last record of an episode within which another record of the same \code{strata} will be considered a \code{"Recurrent"} record.
#' @param episode_unit Time units not less than "seconds" and supported by \code{\link[lubridate]{duration}}
#' @param rolls_max Maximum number of recurrence periods permitted within each episode. Only used if \code{episode_type} is \emph{"rolling"}.
#' @param data_source Unique dataset indentifier for the dataframe. Useful when dataframe contains multiple datsets.
#' @param from_last If \code{TRUE}, episode grouping will be backwards in time - starting at the most recent record to the earliest. If \code{FALSE}, it'll be forward in time - starting at the earliest record to most recent.
#' @param custom_sort If \code{TRUE}, epiosde grouping assignment will begin in preference to this sort order. Useful in specifying that episode grouping begins at a particular kind of record regardless of chronological order.
#' @param bi_direction If \code{FALSE}, \code{"Duplicate"} records are those within the \code{case_length} and \code{recurrence_length} before or after the \code{"Case"} as determined by \code{from_last}. If \code{TRUE}, \code{"Duplicate"} records are those on either side of the \code{"Case"}.
#' @param group_stats If \code{TRUE}, output will include two additional columns (\code{epid_total} and \code{epid_length}).
#' @param display If \code{TRUE}, status messages are not printed on screen.
#'
#' @return Dataframe
#'
#' \itemize{
#' \item \code{sn} - the unique record identifier provided
#' \item \code{epid} - unique episode indentifier
#' \item \code{case_nm} - type of record in each epsiode
#' \item \code{epid_dataset} - list of datasets in each episode
#' \item \code{epid_total} - number of records in each record group
#' \item \code{epid_length} - \code{difftime} object. Date/time difference between earliest and most recent record. \code{episode_unit} is used a \code{difftime} unit.
#' \item \code{epid_window} - lubridate \code{interval} object. Earliest and most recent dates in the episode group.
#' }
#'
#' @seealso
#' \code{\link{record_group}}
#'
#' @details
#' Episode grouping begins at a reference record (\emph{"Case"}) and proceeds forward or backward in time depending on \code{from_last}.
#' If \code{custom_sort} is used, episode grouping can be forced to begin at certain record before proceeding forward or backwards in time.
#' The maximun duration of a \emph{"fixed"} episode is the \code{case_length} while, the maximum duration of a \emph{"rolling"} episode is the
#' \code{case_length} in addition to all recurrence periods. A recurrence period is the \code{recurrence_length} from the last record in an episode
#'
#' @examples
#'
#' library(lubridate)
#' library(dplyr)
#'
#' data <- data.frame(date = seq.Date(dmy("01/04/2018"), dmy("31/05/2018"), by="6 days"))
#' data$epi_len <- 15
#' data <- mutate(data, rd_id = row_number())
#'
#' # Begin episode grouping at the earliest record thereby making it the "Case"
#' cbind(data,
#'       episode_group(data, sn=rd_id, date = date, case_length = epi_len)
#'       )
#'
#' # Begin episode grouping at the most recent record thereby making it the "Case"
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, from_last = TRUE, display = FALSE)
#'       )
#'
#' # Create rolling episodes
#' data$recur <- 30
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episode_type = "rolling",
#'                     recurrence_length = recur, display = FALSE)
#'       )
#'
#' # Limit the amount of possible recurrence period
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episode_type = "rolling",
#'                     recurrence_length = recur, rolls_max = 1, display = FALSE)
#'       )
#'
#' # Limit the number of times episode grouping occurs
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episode_type = "fixed",
#'                     recurrence_length = recur, episodes_max = 1, from_last = FALSE, display = FALSE)
#'       )
#'
#' # User defined case assignment
#' data$infection <- c("BSI", rep("UTI",2), "UTI", "BSI",  "UTI", rep("BSI",2), "RTI","RTI","BSI")
#'
#'   # When there is a combination with UTI, BSI and RTI records within the same case or recurrence length, preference for case assignment is UTI > BSI > RTI
#' data$infection <- factor(data$infection, levels = c("UTI","BSI","RTI"))
#'
#'   # Different type of records can have different case or recurrence lengths
#' data <- mutate(data,
#'                epi_len = case_when(
#'                  infection == "BSI" ~ 14,
#'                  infection == "UTI" ~ 30,
#'                  infection == "RTI" ~ 60
#'                  )
#'                )
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len,
#'                     recurrence_length = recur, custom_sort = infection, group_stats = TRUE, from_last = FALSE, display = FALSE)
#'       )
#'
#'   # Can make it such that, only when the combination includes BSI records should preference go to the BSI record
#'   # When it's a combination without a "BSI" record, preference for case assignment reverts back to the earliest or most recent record depending on from_last
#' data$infection_ord <- ifelse(data$infection =="BTI",0,1)
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len,
#'                     custom_sort = infection_ord, group_stats = TRUE, from_last = FALSE, bi_direction = TRUE, display = FALSE)
#' )
#'
#'   #Can make it such that, duplicates on either side of the case are grouped into the same episode
#' data$infection_ord <- ifelse(data$infection =="RTI",0,1)
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len,
#'                    custom_sort = infection_ord, group_stats = TRUE, from_last = FALSE, bi_direction = TRUE, display = FALSE)
#'       )
#'
#' data$infection_ord <- ifelse(data$infection =="UTI",0,1)
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episodes_max = 1,
#'                     custom_sort = infection_ord, group_stats = TRUE, from_last = FALSE, bi_direction = TRUE, display = FALSE)
#'       )
#'
#' # Stratified episode grouping
#' data$patient_id <- c(rep("PID 1",8), rep("PID 2",3))
#' cbind(data,
#'       episode_group(data, rd_id, date=date, strata = patient_id, case_length = epi_len,
#'                     recurrence_length = recur, episodes_max = 1, from_last = FALSE, display = FALSE)
#'       )
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, strata = c(patient_id, infection), case_length = epi_len,
#'                     recurrence_length = recur, episodes_max = 1, from_last = FALSE, display = FALSE)
#'       )
#'
#' # data_source is useful when grouping episodes across multiple datasets or as diagnostic option
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episode_type = "fixed",
#'                     recurrence_length = recur, episodes_max = 1, data_source = infection, from_last = FALSE, display = FALSE)
#' )
#'
#' cbind(data,
#'       episode_group(data, rd_id, date=date, case_length = epi_len, episode_type = "fixed",
#'                     recurrence_length = recur, episodes_max = 1, data_source = c(patient_id, infection), display = FALSE)
#' )
#'
#' @export

episode_group <- function(df, sn = NULL, strata = NULL, date,
                          case_length, episode_type="fixed", episode_unit = "days", episodes_max = Inf,
                          recurrence_length = NULL, rolls_max =Inf, data_source = NULL,
                          custom_sort = NULL, from_last=FALSE, coverage = "overlap", bi_direction = FALSE, group_stats= FALSE, display=TRUE){

  #Later, add data validations for arguments - assert that
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

  df <- df %>%
    dplyr::select(sn, spec_dt=!!dplyr::enquo(date), .data$epi_len, .data$rc_len, .data$dsvr, .data$cri, !!dplyr::enquo(custom_sort)) %>%
    dplyr::mutate(tag = 0, epid = 0, case_nm="", pr_sn = dplyr::row_number(), roll=0, episodes=0)

  if(from_last==TRUE){
    df$ord <- abs(max(df$spec_dt) - df$spec_dt)
  }else{
    df$ord <- abs(min(df$spec_dt) - df$spec_dt)
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

  c <- 1
  min_episodes_nm <- min_tag <- 0

  df <- dplyr::arrange(df, .data$cri, .data$epi_len)
  df$mrk_a <- paste(df$cri, df$epi_len, sep= "_")
  df$mrk_b <- paste(df$cri, df$rc_len, sep= "_")

  df$fg_x <- rep(rle(df$cri)$lengths, rle(df$cri)$lengths)
  df$fg_a <- rep(rle(df$mrk_a)$lengths, rle(df$mrk_a)$lengths)
  df$fg_b <- rep(rle(df$mrk_b)$lengths, rle(df$mrk_b)$lengths)

  df$mx_case_len <- ifelse(df$fg_a == df$fg_x & df$epi_len ==0, 0, 1)
  df$mx_recur_len <- ifelse(df$fg_a == df$fg_b & df$rc_len ==0, 0, 1)

  # df <- dplyr::select(df, .data$cri, mx_case_len = .data$epi_len) %>%
  #   unique() %>%
  #   dplyr::arrange(.data$cri, .data$mx_case_len) %>%
  #   dplyr::filter(!duplicated(.data$cri, fromLast = TRUE)) %>%
  #   dplyr::full_join(df, by="cri")

  df$epid <- ifelse(df$mx_case_len ==0, df$sn, df$epid)
  df$tag <- ifelse(df$epid !=0, 2, df$tag)
  df$case_nm <- ifelse(df$epid !=0, "Case", df$case_nm)
  df$episodes <- ifelse(df$epid !=0, 1, 0)

  df$roll <- ifelse(df$mx_recur_len ==0, rolls_max, df$roll)

  if(episode_unit=="days"){
    df$epi_len <- df$epi_len-1
    df$rc_len <- df$rc_len-1
  }else{
    df$epi_len <- df$epi_len
    df$rc_len <- df$rc_len
  }

  while (min_tag != 2 & min_episodes_nm <= episodes_max){
      TR <- df %>%
        # preference to those tagged already i.e. exisitng episodes
        dplyr::arrange(.data$cri,  dplyr::desc(.data$tag), .data$user_ord, .data$ord, dplyr::desc(.data$rc_len), dplyr::desc(.data$epi_len), .data$sn) %>%
        #exclude records that will create 1 episode more than episodes_max
        dplyr::filter(!(.data$tag==0 & .data$episodes + 1 > episodes_max )) %>%
        dplyr::filter(.data$tag !=2 & !is.na(.data$tag)) %>%
        dplyr::filter(duplicated(.data$cri) == FALSE) %>%
        dplyr::select(.data$sn, .data$cri, .data$spec_dt, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len) %>%
        dplyr::rename_at(dplyr::vars(.data$sn, .data$spec_dt, .data$epid, .data$tag, .data$roll, .data$epi_len, .data$rc_len), dplyr::funs(paste("tr_",.,sep="")))

    if(nrow(TR)==0) {break}

    if(display){cat(paste("Episode or recurrence window",c,".\n"), sep=" ")}


    df <- dplyr::left_join(df, TR, by= "cri")

    df <- dplyr::mutate_at(df, c("tr_rc_len", "tr_epi_len"), ~ lubridate::duration(., units=episode_unit))

    if (from_last==FALSE){
      df$c_int <- lubridate::interval(df$spec_dt, df$spec_dt + df$epi_len)
      df$r_int <- lubridate::interval(df$spec_dt, df$spec_dt + df$rc_len)

      df$tr_c_int <- lubridate::interval(df$tr_spec_dt, df$tr_spec_dt + df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_spec_dt, df$tr_spec_dt + df$tr_rc_len)

    }else{
      df$c_int <- lubridate::interval(df$spec_dt, df$spec_dt - df$epi_len)
      df$r_int <- lubridate::interval(df$spec_dt, df$spec_dt - df$rc_len)

      df$tr_c_int <- lubridate::interval(df$tr_spec_dt, df$tr_spec_dt - df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_spec_dt, df$tr_spec_dt - df$tr_rc_len)
    }

    if (bi_direction==TRUE){
      df$tr_c_int <- lubridate::interval(df$tr_spec_dt - df$epi_len, df$tr_spec_dt + df$tr_epi_len)
      df$tr_r_int <- lubridate::interval(df$tr_spec_dt - df$rc_len, df$tr_spec_dt + df$tr_rc_len)
    }

    df$r_range <- df$c_range <- FALSE

    if("overlap" %in% tolower(coverage)){
      df$r_range <- ifelse(lubridate::int_overlaps(df$r_int, df$tr_r_int), TRUE, df$r_range)
      df$c_range <- ifelse(lubridate::int_overlaps(df$c_int,df$tr_c_int), TRUE, df$c_range)
    }
    if("within" %in% tolower(coverage)){
      df$r_range <- ifelse(df$r_int %within% df$tr_r_int, TRUE, df$r_range)
      df$c_range <- ifelse(df$c_int %within% df$tr_c_int, TRUE, df$c_range)
    }
    if("aligns_start" %in% tolower(coverage)){
      df$r_range <- ifelse(lubridate::int_aligns(df$tr_r_int, df$r_int) & lubridate::int_start(df$tr_r_int) ==  lubridate::int_start(df$r_int), TRUE, df$r_range)
      df$c_range <- ifelse(lubridate::int_aligns(df$tr_c_int, df$c_int) & lubridate::int_start(df$tr_c_int) ==  lubridate::int_start(df$c_int), TRUE, df$c_range)
    }
    if("aligns_end" %in% tolower(coverage)){
      df$r_range <- ifelse(lubridate::int_aligns(df$tr_r_int, df$r_int) & lubridate::int_end(df$tr_r_int) ==  lubridate::int_end(df$r_int), TRUE, df$r_range)
      df$c_range <- ifelse(lubridate::int_aligns(df$tr_c_int, df$c_int) & lubridate::int_end(df$tr_c_int) ==  lubridate::int_end(df$c_int), TRUE, df$c_range)
    }
    if("chain" %in% tolower(coverage)){
      df$r_range <- ifelse(lubridate::int_end(df$tr_r_int) ==  lubridate::int_start(df$r_int), TRUE, df$r_range)
      df$c_range <- ifelse(lubridate::int_end(df$tr_c_int) ==  lubridate::int_start(df$c_int), TRUE, df$c_range)
    }

    if(!bi_direction & !from_last){
      df$c_range <- ifelse(df$tr_spec_dt > df$spec_dt, FALSE, df$c_range)
      df$r_range <- ifelse(df$tr_spec_dt > df$spec_dt, FALSE, df$r_range)

      }else if(!bi_direction & from_last){
        df$c_range <- ifelse(df$tr_spec_dt < df$spec_dt, FALSE, df$c_range)
        df$r_range <- ifelse(df$tr_spec_dt < df$spec_dt, FALSE, df$r_range)
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
      dplyr::arrange(.data$cri, .data$user_ord, .data$ord, dplyr::desc(.data$rc_len), dplyr::desc(.data$epi_len), .data$sn)

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
    min_roll <- min(df$roll)
    min_episodes_nm <- min(df$episodes)

    c = c+1
  }

  df <- df %>%
    dplyr::mutate(
      case_nm= ifelse(.data$epid==0, "Case", .data$case_nm),
      epid= ifelse(.data$epid==0, .data$sn, .data$epid)
    )

  if(is.null(ds)){
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm, .data$pr_sn, .data$spec_dt, .data$ord)
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
    df <- dplyr::select(df, .data$sn, .data$epid, .data$case_nm, .data$epid_dataset, .data$pr_sn, .data$spec_dt, .data$ord)
  }

  if(group_stats){

    epid_l <- dplyr::select(df, .data$epid, .data$spec_dt, .data$ord) %>%
      unique() %>%
      dplyr::arrange(.data$epid, .data$ord) %>%
      dplyr::filter(!duplicated(.data$epid) | !duplicated(.data$epid, fromLast = TRUE))

    epid_l$ord <- paste("o_",sequence(rle(epid_l$epid)$lengths), sep="")

    epid_l <- epid_l %>%
      dplyr::mutate(spec_dt = format(.data$spec_dt, "%d/%m/%Y %H:%M:%S")) %>%
      tidyr::spread("ord","spec_dt") %>%
      dplyr::mutate(
        o_2 = ifelse(is.na(.data$o_2), .data$o_1, .data$o_2),
        epid_length = lubridate::make_difftime(difftime(lubridate::dmy_hms(o_2), lubridate::dmy_hms(o_1), units = "secs"), units = episode_unit)
        )

    df <- dplyr::left_join(df, epid_l, by="epid")

    df <- dplyr::arrange(df, .data$epid)

    df$epid_total <- rep(rle(df$epid)$lengths, rle(df$epid)$lengths)

    df <- dplyr::arrange(df, .data$pr_sn)
    df <- dplyr::mutate(df, epid_window = lubridate::interval(lubridate::dmy_hms(o_1), lubridate::dmy_hms(o_2)))
    df <- dplyr::select(df, -c(.data$o_2, .data$o_1))
  }

  df <- dplyr::select(df, -c(.data$pr_sn, .data$spec_dt, .data$ord))

  unique_ids <- length(df[!duplicated(df$epid) & !duplicated(df$epid, fromLast = TRUE),]$epid)

  pd <- ifelse(display,"\n","")
  cat(paste(pd, "Episode grouping complete - " ,fmt(unique_ids)," record(s) assinged a unique ID." , sep =""))
  cat("\n ---------------------------------------------------------------------------------------------------- \n \n ")
  df
}
