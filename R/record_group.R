#' @name record_group
#' @title Multistage deterministic record linkage
#'
#' @description Group matching records from one or more datasets.
#'
#' @param df \code{data.frame}. One or more datasets appended together.
#' @param sn Unique numerical record identifier. Optional.
#' @param criteria Column names of attributes to match. Each column is one stage of the process and the the order in which they are listed determines the relevance of matches. Matching records are assigned to a record group.
#' @param sub_criteria Matching sub-criteria. Additional matching conditions for each stage (\code{criteria}).
#' @param data_source Unique dataset identifier. Useful when \code{df} contains data from multiple sources.
#' @param group_stats If \code{TRUE}, output will include additional columns with useful stats for each record group.
#' @param display If \code{TRUE}, status messages are printed on screen.
#' @param to_s4 if \code{TRUE}, changes the returned output to a \code{\link[=pid-class]{pid}} object.
#'
#' @return \code{\link[=pid-class]{pid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided
#' \item \code{pid | .Data} - unique group identifier
#' \item \code{pid_cri} - matched criteria for each record in the group
#' \item \code{pid_dataset} - data sources in each group
#' \item \code{pid_total} - number of records in each group
#' }
#'
#' \code{pid} objects will be the default output from the next release.
#'
#' @seealso \code{\link{episode_group}} and \code{\link{number_line}}
#'
#' @details
#' Record grouping occurs in stages of matching \code{criteria}.
#'
#' Records are matched in two ways; an exact match - the equivalent of \code{(==)}, or matching a range of numeric values.
#' An example of range matching is matching a date give or take 5 days, or matching an age give or take 2 years.
#' To do this, create a \code{\link{number_line}} object to represent the range and supply this to \code{criteria} or \code{sub_criteria}.
#' The actual value within each range must be assigned to the \code{gid} slot of the \code{number_line} object.
#'
#' A match at each stage is considered more relevant than those at subsequent stages.
#' Therefore, \code{criteria} should be listed in order of decreasing relevance or certainty.
#'
#' \code{sub_criteria} can be used to force additional matching conditions at each stage.
#' If \code{sub_criteria} is not \code{NULL}, only records with matching \code{criteria} and \code{sub_criteria} values are grouped together.
#' If a record has missing values for any \code{criteria}, that record skipped at that stage, and another attempt is made at the next stage.
#' If there are no matches for a record at every stage, that record is assigned a unique group ID.
#'
#' When a \code{data_source} identifier is provided,
#' \code{pid_dataset} is included in the output. This has the source of every record in each record group.
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' three_people <- data.frame(forename=c("Obinna","James","Ojay","James","Obinna"),
#'                            stringsAsFactors = FALSE)
#'
#' three_people$pids_a <- record_group(three_people, criteria= forename, to_s4 = TRUE)
#' three_people
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
#' dob$age <- c(30,28,40,25,25,29,27)
#'
#' # age range: age + 20 years
#' dob$range_a <- number_line(dob$age, dob$age+20, gid=dob$age)
#' dob$pids_a <- record_group(dob, criteria = sex, sub_criteria = list(s1a="range_a"), to_s4 = TRUE)
#' dob[c("sex","age","range_a","pids_a")]
#'
#' # age range: age +- 20 years
#' dob$range_b <- number_line(dob$age-20, dob$age+20, gid=dob$age)
#' dob$pids_b <- record_group(dob, criteria = sex, sub_criteria = list(s1a="range_b"), to_s4 = TRUE)
#' dob[c("sex","age","range_b","pids_b")]
#'
#' dob$pids_c <- record_group(dob, criteria = range_b, to_s4 = TRUE)
#' dob[c("age","range_b","pids_c")]
#'
#'
#' # Multistage record grouping
#' staff_records$pids_a <- record_group(staff_records, sn = r_id, criteria = c(forename, surname),
#'                                      data_source = sex, display = FALSE, to_s4 = TRUE)
#' staff_records
#'
#' # Add `sex` to the second stage (`cri`) to be more certain
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
#' @aliases record_group
#' @export
record_group <- function(df, sn=NULL, criteria, sub_criteria=NULL, data_source = NULL, group_stats=FALSE, display=TRUE, to_s4 = TRUE){
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

  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)
  cri_lst <- enq_vr(substitute(criteria))

  # Check that col names exist
  if(any(!unique(c(rd_sn, ds, sub_cri_lst, cri_lst)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, sub_cri_lst, cri_lst)), !unique(c(rd_sn, ds, sub_cri_lst, cri_lst)) %in% names(df))
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

  # Prep
  df <- df[c("sn",cri_lst,sub_cri_lst,"dsvr")]
  df$pr_sn <- 1:nrow(df)
  df$m_tag <- df$tag <- 0
  df$pid_cri <- Inf
  df$pid <- sn_ref <- min(df$sn)-1
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
  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)

  # Range matching
  range_match <- function(x, tr_x) {
    if(any(!diyar::overlap(diyar::as.number_line(x@gid), x))) {
      rng_i <- paste(which(!diyar::overlap(diyar::as.number_line(x@gid), x)), collapse = ",", sep="")
      rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]

      stop(paste("Range matching error: Actual value (gid) is out of range in '",rng_d,"$",rng_v,"[c(",rng_i,")]'",sep=""))
    }
    #if(utils::packageVersion("dplyr") < package_version("0.8.0.1")) stop("dplyr >= v0.8.0.1 is required for range matching")
    diyar::overlap(diyar::as.number_line(x@gid), tr_x)
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
    df$cri <- df[[cri_lst[i]]]

    # Fetch corresponding matching criteria
    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, grepl(paste("s",i,sep=""), attr))
    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){
        ifelse(class(df[[x]]) == "number_line",
               paste0("range_match(df2$",x, ", ", "df2$tr_",x,")"),
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
      TR <- TR[order(TR$cri, TR$skip, -TR$force_check, -TR$tag, TR$m_tag, TR$pid_cri, TR$sn),]
      TR <- TR[!duplicated(TR$cri),]
      TR <- TR[c("pid","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst)]
      names(TR) <- paste0("tr_", names(TR))

      df <- merge(df, TR, by.x="cri", by.y="tr_cri", all.x=T)

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
      df$m_tag <- ifelse(df$pid !=sn_ref & df$m_tag != -1,1, df$m_tag)
      df$m_tag <- ifelse(df$sn==df$tr_sn & !is.na(df$tr_sn) & df$m_tag ==-1, 1, df$m_tag )


      df$skip <- ifelse(df$m_tag ==-1 & !is.na(df$m_tag), 0, ifelse(df$sub_cri_match==1, 1, df$skip))
      min_pid <- min(df$pid[!df$cri %in% c("",NA)])
      min_m_tag <- min(df$m_tag[!df$cri %in% c("",NA)])
      df <- df[c("sn", "pr_sn", "pid", "pid_cri", "cri", cri_lst, sub_cri_lst, "tag", "m_tag", "skip", "dsvr", "force_check")]
      c <- c+1
    }

    tagged_1 <- length(df$pid[!df$pid %in% c(sn_ref,NA) & df$tag ==0])
    total_1 <- length(df$pid[df$tag ==0])

    if(display) {
      cat(paste("\n",fmt(tagged_1)," of ", fmt(total_1)," record(s) have been assigned a group ID. ", fmt(total_1-tagged_1)," record(s) not yet grouped.", sep =""))
    }

    # Cases that have not been tagged for the print output
    df$tag <- ifelse(df$pid %in% c(sn_ref,NA),0,1)
    df$pid <- ifelse(duplicated(df$pid) == FALSE & duplicated(df$pid, fromLast=TRUE) == FALSE,sn_ref,df$pid)

    removed <- length(subset(df$pid, df$pid %in% c(sn_ref,NA) & df$tag ==1 ))

    df$tag <- ifelse(df$pid!=sn_ref,1,0)
    df$pid_cri <- ifelse(df$tag ==1 & df$pid_cri == Inf,i, df$pid_cri)

    if(display) {
      cat(paste("\n",fmt(removed), " record(s) with unique group IDs untagged for possible matching in the next stage. The number of records not yet grouped is now ", fmt(removed + (total_1-tagged_1)),".\n", sep =""))
    }
  }

  df$pid_cri <- ifelse(df$pid_cri==Inf, 0, df$pid_cri)

  # records not yet assigned a group ID are assigned new unique group IDs
  df$pid <- ifelse(df$pid==sn_ref, df$sn, df$pid)
  df <- df[c("sn","pid","pid_cri","dsvr","pr_sn")]

  if(is.null(ds)){
    df <- df[order(df$pr_sn), c("sn","pid","pid_cri","pr_sn")]
  }else{
    pds2 <- lapply(split(df$dsvr, df$pid), function(x){
      paste0(sort(unique(x)), collapse=",")
    })

    df$pid_dataset <- as.character(pds2[as.character(df$pid)])
    df <- df[order(df$pr_sn), c("sn","pid","pid_cri","pid_dataset","pr_sn")]
  }

  if(group_stats){
    df <- df[order(df$pid),]
    df$pid_total <- rep(rle(df$pid)$lengths, rle(df$pid)$lengths)
    df <- df[order(df$pr_sn),]
  }

  df$pr_sn <- NULL

  pd <- ifelse(display,"\n","")
  cat(paste(pd,"Record grouping complete - ",fmt(removed + (total_1-tagged_1))," record(s) assigned a group unique ID. \n" , sep =""))
  if(to_s4) df <- diyar::to_s4(df)
  df
}

