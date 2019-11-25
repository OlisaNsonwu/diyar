#' @name record_group
#' @title Multistage deterministic record linkage
#'
#' @description Group matching records from one or more datasets.
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique \code{numeric} record identifier. Optional.
#' @param criteria Column names of attributes to match. Records with matching values in these columns are grouped together.
#' @param sub_criteria Matching sub-criteria. Additional matching conditions for each stage (\code{criteria}).
#' @param data_source Unique dataset identifier. Useful when \code{df} contains data from multiple sources.
#' @param group_stats If \code{TRUE}, output will include additional columns with useful stats for each record group.
#' @param display If \code{TRUE}, status messages are printed on screen.
#' @param to_s4 if \code{TRUE}, changes the returned value to a \code{\link[=pid-class]{pid}} object.
#'
#' @return \code{data.frame} (\code{\link[=pid-class]{pid}} objects if \code{to_s4} is \code{TRUE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{pid} - unique group identifier
#' \item \code{pid_cri} - matched criteria for each record in the group
#' \item \code{pid_dataset} - data sources in each group
#' \item \code{pid_total} - number of records in each group
#' }
#'
#' \code{pid} objects will be the default output from the next release onwards
#'
#' @seealso \code{\link{episode_group}} and \code{\link{number_line}}
#'
#' @details
#' Record grouping occurs in stages of matching \code{criteria}.
#'
#' Records are matched in two ways; an exact match - the equivalent of \code{(==)}, or matching a range of numeric values.
#' An example of range matching is matching a date give or take 5 days, or matching an age give or take 2 years.
#' To do this, create a \code{\link{number_line}} object based on the range of values, and assign the actual value assigned to \code{gid}.
#' Then use the \code{\link{number_line}} as a \code{sub_criteria}.
#'
#' A match at each stage is considered more relevant than those at subsequent stages.
#' Therefore, \code{criteria} should be listed in order of decreasing relevance or certainty.
#'
#' \code{sub_criteria} can be used to force additional matching conditions at each stage.
#' If \code{sub_criteria} is not \code{NULL}, only records with matching \code{criteria} and \code{sub_criteria} values are grouped together.
#' If a record has missing values for any \code{criteria}, it's skipped at that stage, and another attempt is made at the next stage.
#' If all \code{criteria} values are missing, that record is assigned a unique group ID.
#'
#' When a \code{data_source} identifier is included,
#' \code{pid_dataset} is included in the output. This shows the data sources included in each group.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' three_people <- data.frame(forename=c("Obinna","James","Ojay","James","Obinna"),
#'                            stringsAsFactors = FALSE)
#'
#' # Old way - merging or binding results back to the data.frame
#' output <-  bind_cols(three_people, record_group(three_people, criteria= forename))
#' output
#'
#' # New way - pid_object
#' three_people$pids_a <- output$pids <- record_group(three_people, criteria= forename, to_s4 = TRUE)
#' output
#'
#' # To handle missing or unknown data, recode missing or unknown values to NA or "".
#' three_people$forename[c(1,4)] <- NA
#' three_people$pids_b <- record_group(three_people, criteria= forename, to_s4 =TRUE)
#' three_people
#'
#' data(staff_records); staff_records
#'
#' # Range matching
#' dob <- staff_records["sex"]
#' dob$age <- c(10,8,20,5,5,9,7)
#'
#' # age range - age + 20 years
#' dob$range <- number_line(dob$age, dob$age+20, gid=dob$age)
#' dob$pids_a <- record_group(dob, criteria = sex, sub_criteria = list(s1a="range"), to_s4 = TRUE)
#'
#' # age range - age +- 20 years
#' dob$range <- number_line(dob$age-20, dob$age+20, gid=dob$age)
#' dob$pids_b <- record_group(dob, criteria = sex, sub_criteria = list(s1a="range"), to_s4 = TRUE)
#'
#' dob$pids_c <- record_group(dob, criteria = range, to_s4 = TRUE)
#' dob
#'
#' # Two or more stages of record grouping
#' staff_records$pids_a <- record_group(staff_records, sn = r_id, criteria = c(forename, surname),
#'                                      data_source = sex, display = FALSE, to_s4 = TRUE)
#' staff_records
#'
#' # Add sex to the second stage to be more certain
#' staff_records <- unite(staff_records, cri_2, c(surname, sex), sep ="-")
#' staff_records$pids_b <- record_group(staff_records, r_id, c(forename, cri_2),
#'                                      data_source = dataset, display = FALSE, to_s4 = TRUE)
#' staff_records
#'
#' # Using sub-criteria
#' data(missing_staff_id); missing_staff_id
#'
#' missing_staff_id$pids <- record_group(missing_staff_id, r_id, c(staff_id, age),
#' list(s2a=c("initials","hair_colour","branch_office")), data_source = source_1, to_s4 = TRUE)
#'
#' missing_staff_id
#'
#' @export

record_group <- function(df, sn=NULL, criteria, sub_criteria=NULL, data_source = NULL, group_stats=FALSE, display=TRUE, to_s4 = FALSE){
  if(!is.data.frame(df)) stop(paste("A dataframe is required"))
  if(!(is.logical(group_stats) & is.logical(display) & is.logical(to_s4))) stop(paste("'group_stats', 'display' and 'to_s4' must be TRUE or FALSE"))

  . <- NULL
  if(to_s4 == FALSE){
    if (is.null(getOption("diyar.record_group.output"))){
      options("diyar.record_group.output"= TRUE)
    }
    if (getOption("diyar.record_group.output")){
      message(paste("The default output of record_group() will be changed to pid objects in the next release.",
                    "Please consider switching earlier by using 'to_s4=TRUE' or to_s4()",
                    "This message is displayed once per session.", sep = "\n"))
    }
    options("diyar.record_group.output"= FALSE)
  }
  enq_vr <- function(x){
    x <- as.character(x)
    if(x[1]=="c" & length(x)>1) x <- x[2:length(x)]
    if(length(x)==0) x <- NULL
    x
  }

  fmt <- function(g) formatC(g, format="d", big.mark=",")

  ds <- enq_vr(substitute(data_source))

  df_vars <- names(df)

  rd_sn <- enq_vr(substitute(sn))

  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)
  cri_lst <- enq_vr(substitute(criteria))

  if(!is.null(rd_sn)){
    if(!(all(df[[rd_sn]] > 0) & is.numeric(as.numeric(df[[rd_sn]])))) stop(paste("'",rd_sn,"' as 'sn' must be > 0", sep=""))
    if(any(duplicated(df[[rd_sn]]))) stop(paste("'",rd_sn,"' as 'sn' must not have duplicate values", sep=""))
  }

  if(any(!unique(c(rd_sn, ds, sub_cri_lst, cri_lst)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, sub_cri_lst, cri_lst)), !unique(c(rd_sn, ds, sub_cri_lst, cri_lst)) %in% names(df))
    missing_cols <- paste(paste("'",missing_cols,"'",sep=""), collapse = "," )
    stop(paste(missing_cols, "not found"))
  }

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

  df <- df %>%
    dplyr::select(sn, !!dplyr::enquo(criteria), sub_cri_lst, .data$dsvr) %>%
    dplyr::mutate(pr_sn = dplyr::row_number(), m_tag=0, tag = 0, pid = 0, pid_cri = Inf)

  cri_no <- length(cri_lst)

  for(i in 1:cri_no){
    if(is.number_line(df[[cri_lst[i]]])){
      #dummy criteria
      rp_vr <- paste("dmvr_d",i,sep="")
      df[[rp_vr]] <- 1
      # make nl a sub_criteria for the dummy criteria
      if(is.null(sub_criteria)) sub_criteria <- list()
      sub_criteria[paste("s",i,"zr",sep="")] <- cri_lst[i]
      # update criteria list
      cri_lst[i] <- rp_vr
    }
  }
  #update 'sub_cri_lst'
  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)

  range_match <- function(x, tr_x) {
    if(any(!diyar::overlap(diyar::as.number_line(x@gid), x))) stop("Actual value (gid) is outside the range created in a number_line object")
    if(utils::packageVersion("dplyr") < package_version("0.8.0.1")) stop("dplyr >= v0.8.0.1 is required for range matching")
    diyar::overlap(diyar::as.number_line(x@gid), tr_x)
  }

  exact_match <- function(x, tr_x) {
    x <- ifelse(is.na(x), "", as.character(x))
    tr_x <- ifelse(is.na(tr_x), "", as.character(tr_x))
    x == tr_x
  }

  for(i in 1:cri_no){
    if(display) cat(paste("\nGroup criteria ",i," - ","`",cri_lst[i],"`", sep=""))

    df$cri <- df[[cri_lst[i]]]

    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, grepl(paste("s",i,sep=""), attr))

    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){
        ifelse(class(df[[x]]) == "number_line",
               paste("range_match(df2$",x, ", ", "df2$tr_",x,")",sep=""),
               paste("exact_match(df2$",x, ", ", "df2$tr_",x,")",sep=""))
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

    df$force_check <- df$skip <- df$m_tag <- c <- min_m_tag <- min_pid <- 0

    while (min_pid==0 | min_m_tag==-1) {
      #df$force_check <- ifelse(df$m_tag==-1,2,df$tag)
      if(c+1 >1 & display ) cat(paste("\nMatching criteria ",i,": iteration ",c+1, sep=""))

      TR <- df %>%
        dplyr::filter(!.data$cri %in% c("",NA))  %>%
        dplyr::arrange(.data$cri, .data$skip, dplyr::desc(.data$force_check), dplyr::desc(.data$tag), .data$m_tag, .data$pid_cri, .data$sn) %>%
        dplyr::filter(duplicated(.data$cri) == FALSE) %>%
        dplyr::select_at(c("pid","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst)) %>%
        dplyr::rename_at(c("pid","m_tag", "tag", "sn","pid_cri",curr_sub_cri_lst), list(~paste("tr_",.,sep="")))

      df <- dplyr::left_join(df,TR, by="cri")

      df$sub_cri_match <- ifelse(!sub_crx_func(df) %in% c(NA, FALSE),1,0)
      # no need to check again
      # df$m_tag <- ifelse(df$sub_cri_match==1 & df$m_tag==-1 & df$pid==df$tr_pid & !is.na(df$tr_pid),1,df$m_tag)

      df <- df %>%
        dplyr::mutate(
          m_tag = ifelse(.data$m_tag==1 &
                           #.data$tr_pid ==0 &
                           .data$sub_cri_match==1 & .data$pid_cri <= .data$tr_pid_cri, -1, .data$m_tag),
          m_tag = ifelse(.data$sub_cri_match==1 & .data$m_tag==-1 & .data$pid==.data$tr_pid & !is.na(.data$tr_pid),1,.data$m_tag),
          pid = ifelse(
            (.data$m_tag==-1 & .data$pid!=0) | (.data$sub_cri_match==1 & .data$pid==0 & !is.na(.data$tr_pid)),
            .data$tr_pid, .data$pid
          ),
          #inherit pid
          pid = ifelse(
            .data$pid==0 & !.data$tr_pid %in% c(0,NA) & .data$sub_cri_match==1,
            .data$tr_pid, .data$pid
          ),
          #assign new pid
          pid = ifelse(
            .data$pid==0 & .data$tr_pid == 0 & !is.na(.data$tr_pid) & .data$sub_cri_match==1,
            .data$tr_sn, .data$pid
          ),
          m_tag = ifelse(.data$pid !=0 & .data$m_tag != -1,1, .data$m_tag),
          m_tag = ifelse(.data$sn==.data$tr_sn & !is.na(.data$tr_sn) & .data$m_tag ==-1, 1, .data$m_tag )
        )

      df$skip <- ifelse(df$m_tag ==-1 & !is.na(df$m_tag), 0, ifelse(df$sub_cri_match==1, 1, df$skip))

      min_pid <- df %>%
        dplyr::filter(!.data$cri %in% c("",NA)) %>%
        dplyr::select(.data$pid) %>% min()

      min_m_tag <- df %>%
        dplyr::filter(!.data$cri %in% c("",NA)) %>%
        dplyr::select(.data$m_tag) %>% min()

      df <- dplyr::select(df, .data$sn, .data$pr_sn, .data$pid, .data$pid_cri, .data$cri, cri_lst, sub_cri_lst,
                          .data$ tag, .data$m_tag, .data$skip, .data$dsvr, .data$force_check)

      c <- c+1
    }

    tagged_1 <- length(subset(df$pid, !df$pid %in% c(0,NA) & df$tag ==0 ))
    total_1 <- length(subset(df$pid, df$tag ==0 ))

    if(display) {
      cat(paste("\n",fmt(tagged_1)," of ", fmt(total_1)," record(s) have been assigned a group ID. ", fmt(total_1-tagged_1)," record(s) not yet grouped.", sep =""))
    }

    # untag record groups with only one record to attempt matching in the next criteria
    df <- df %>%
      dplyr::mutate(
        # keeping track of cases that have not been tagged for the print output
        tag = ifelse(.data$pid %in% c(0,NA),0,1),
        pid = ifelse(duplicated(.data$pid) == FALSE & duplicated(.data$pid, fromLast=TRUE) == FALSE,0,.data$pid))

    removed <- length(subset(df$pid, df$pid %in% c(0,NA) & df$tag ==1 ))

    df <- df %>%
      dplyr::mutate(
        tag = ifelse(.data$pid!=0,1,0),
        pid_cri = ifelse(.data$tag ==1 & .data$pid_cri == Inf,i, .data$pid_cri)
      )


    if(display) {
      cat(paste("\n",fmt(removed), " record(s) with unique group IDs untagged for possible matching in the next stage. The number of records not yet grouped is now ", fmt(removed + (total_1-tagged_1)),".\n", sep =""))
    }
  }

  df$pid_cri <- ifelse(df$pid_cri==Inf, 0, df$pid_cri)

  df <- df %>%
    # records not yet assigned a group ID are assigned new unique group IDs
    dplyr::mutate(pid = ifelse(.data$pid==0, .data$sn, .data$pid)) %>%
    dplyr::select(.data$sn, .data$pid, .data$pid_cri, .data$dsvr, .data$pr_sn)

   if(is.null(ds)){
     df <- dplyr::arrange(df, .data$pr_sn) %>%
       dplyr::select(.data$sn, .data$pid, .data$pid_cri, .data$pr_sn)
   }else{
     sourc_list <- as.character(sort(unique(df$dsvr)))

     df <- df %>%
       dplyr::select(.data$pid, .data$dsvr) %>%
       unique() %>%
       dplyr::mutate(val= .data$dsvr) %>%
       dplyr::arrange(.data$dsvr) %>%
       tidyr::spread(key= "dsvr", value= "val") %>%
       tidyr::unite("pid_dataset", sourc_list, sep=",") %>%
       dplyr::mutate(pid_dataset = gsub("NA,|,NA|^NA$","",.data$pid_dataset)) %>%
       dplyr::full_join(df, by="pid")

      df <- dplyr::select(df, .data$sn, .data$pid, .data$pid_cri, .data$pid_dataset, .data$pr_sn)
   }

   if(group_stats){
     df <- dplyr::arrange(df, .data$pid)
     df$pid_total <- rep(rle(df$pid)$lengths, rle(df$pid)$lengths)
   }

    df <- dplyr::arrange(df, .data$pr_sn)
    df <- dplyr::select(df, -.data$pr_sn)

   pd <- ifelse(display,"\n","")
   cat(paste(pd,"Record grouping complete - ",fmt(removed + (total_1-tagged_1))," record(s) assigned a group unique ID. \n" , sep =""))
   if(to_s4) df <- diyar::to_s4(df)
   df
}
