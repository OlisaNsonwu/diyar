#' @title Deterministic linkage for epidemiological analysis
#'
#' @description This function links records from one or more datasets into unique group idenfiers.
#'
#' @param df Dataframe. One or more datasets appened together.
#' @param sn \code{numeric} unique record indentifier for the dataframe.
#' @param criteria Matching criteria. Records with matching values in these columns are grouped together.
#' @param sub_criteria Matching sub-criteria. Additional conditions for a match at each stage (\code{criteria}).
#' @param data_source Unique dataset indentifier. Useful when dataframe contains data from multiple datasets.
#' @param group_stats If \code{TRUE}, output will include one additional column (\code{pid_total}) with the number of records in each record group.
#' @param display If \code{TRUE}, status messages are not printed on screen.
#'
#' @return Dataframe
#'
#' @seealso \code{\link{episode_group}}
#'
#' @details
#' Record linkage occurs in stages as listed in \code{criteria}. Linkage at each stage is considered more certain than subsequent stages.
#' Therefore, \code{criteria} should be listed in order of decreasing certainty. \code{sub_criteria} can be used to force additonal conditions for record
#' linkage at each stage. If \code{sub_criteria} is not \code{NULL}, only records with matching \code{criteria} and \code{sub_criteria} values are grouped together.
#' If a \code{criteria} value is missing, that record is skipped for that stage of record linkage, and another attempt is made at the next stage.
#' If all \code{criteria} values are missing, that record is assigned a unique group ID. When a \code{data_source} identifier is included,
#' \code{pid_dataset} is included in the output. This shows the datasets included in each record group.
#'
#' @examples
#'
#'  library(dplyr)
#'  library(tidyr)
#'
#' # One stage record grouping
#'  df <- data.frame(
#'    r_id = c(1:5),
#'    forename = c("Obinna","James","Ojay","James","Obinna"),
#'    stringsAsFactors = FALSE
#'  )
#'
#'  cbind(df, record_group(df, r_id, forename))
#'
#'  # Handling missing or unknown data - Recode missing or unknown values to NA or "".
#'  # These are excluded from record grouping at the relevant stage
#'
#'  df %>%
#'    mutate(forename = ifelse(r_id %in% c(1,4), NA, forename)) %>%
#'    cbind(record_group(., r_id, forename))
#'
#'  # Two or more stages of record grouping
#'  # Criteria should be listed in decreasing order of certainty
#' data_3 <- data.frame(
#'   forename = c("James",NA,"Jamey","","Derrick","Darrack","Christie"),
#'   surname = c("Green","Anderson","Green",NA,"Anderson","Anderson","Green"),
#'   sex = c("M","M","M","F","M","M","F"),
#'   dataset = c("Staff list","Staff list", "Pay slips","Pay slips", "Staff list","Pay slips","Staff list"),
#'   r_id = c(1:7),
#'   stringsAsFactors = FALSE
#' )
#'
#' cbind(data_3, record_group(data_3,r_id, c(forename, surname), data_source = sex, display = FALSE))
#'
#' # Add sex to the second stage to be more certain
#' data_3 %>%
#'   unite(cri_2, c(surname, sex)) %>%
#'   cbind(record_group(., r_id, c(forename, cri_2), data_source = dataset, display = FALSE))
#'
#' # Using sub-criteria
#' data_4 <- data.frame(
#'   staff_id = c(NA,NA,NA,NA,NA,2,2),
#'   age = rep(30,7),
#'   initials = c("G.D.","B.G.","X.P.","X.P.",NA,"G.D.","G.D."),
#'   hair_colour = c("Brown","Teal",NA,"Green","Green","Dark brown","Brown"),
#'   branch_office = c("Republic of Ghana","France",NA,NA,"France","Ghana","Republic of Ghana"),
#'   ds_1 = c("A","A","A","B","A","A","B"),
#'   ds_2 = c(3,1,1,1,1,1,2),
#'   r_id = c(1:7),
#'   stringsAsFactors = FALSE
#' )
#'
#' cbind(data_4, record_group(data_4,r_id, c(staff_id, age), list(s2a=c("initials","hair_colour","branch_office")), data_source = ds_1))
#'
#' cbind(data_4, record_group(data_4,r_id, c(staff_id, age), list(s2a=c("initials","hair_colour","branch_office")), data_source = c(ds_1, ds_2)))
#'
#' @export

record_group <- function(df, sn, criteria, sub_criteria=NULL, data_source = NULL, group_stats=FALSE, display=TRUE){
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

  ds <- enq_vr(df[1,], dplyr::enquo(data_source))

  df_vars <- names(df)

  cri_lst <- enq_vr(df[1,], dplyr::enquo(criteria))
  sub_cri_lst <- subset(unlist(sub_criteria, use.names = FALSE),unlist(sub_criteria, use.names = FALSE) %in% names(df))

  if(is.null(ds)){
    df$dsvr <- "A"
  }else{
    df <- tidyr::unite(df, "dsvr", ds, remove=FALSE, sep="-")
  }

  df <- df %>%
    dplyr::select(sn=!!dplyr::enquo(sn), !!dplyr::enquo(criteria), sub_cri_lst, .data$dsvr) %>%
    dplyr::mutate_at(dplyr::vars(!!dplyr::enquo(criteria), sub_cri_lst), as.character) %>%
    dplyr::mutate_at(dplyr::vars(!!dplyr::enquo(criteria), sub_cri_lst), dplyr::funs(ifelse(is.na(.),"",.))) %>%
    dplyr::mutate(pr_sn = dplyr::row_number(), m_tag=0, tag = 0, pid = 0, pid_cri = Inf)

  cri_no <- length(cri_lst)

  for(i in 1:cri_no){
    if(display) cat(paste("\nGroup criteria ",i," - ","`",cri_lst[i],"`", sep=""))

    df$cri <- df[[cri_lst[i]]]

    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, stringr::str_detect(attr,paste("s",i,sep="")))

    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){paste("df2$",x, "==", "df2$tr_",x, sep="")}
      func_2 <- function(x){paste(x, collapse = " | ")}
      func_3 <- function(x){paste("(",x,")", sep="")}

      sub_crx_func <- lapply(sub_criteria[attr], func_1)
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

    df$skip <- df$m_tag <- c <- min_m_tag <- min_pid <- 0

    while (min_pid==0 | min_m_tag==-1) {
      df$force_check <- ifelse(df$m_tag==-1,2,df$tag)
      if(c+1 >1 & display ) cat(paste("\nMatching criteria ",i,": iteration ",c+1, sep=""))

      TR <- df %>%
        dplyr::filter(!.data$cri %in% c("",NA))  %>%
        dplyr::arrange(.data$cri, .data$skip, dplyr::desc(.data$force_check), dplyr::desc(.data$tag), .data$m_tag, .data$pid_cri, .data$sn) %>%
        dplyr::filter(duplicated(.data$cri) == FALSE) %>%
        dplyr::select_at(c("pid","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst)) %>%
        dplyr::rename_at(c("pid","m_tag", "tag", "sn","pid_cri",curr_sub_cri_lst), dplyr::funs(paste("tr_",.,sep="")))

      df <- dplyr::left_join(df,TR, by="cri")

      df$sub_cri_match <- ifelse(!sub_crx_func(df) %in% c(NA, FALSE),1,0)
      df$skip <- ifelse(df$sub_cri_match==1,1,df$skip)

      df <- df %>%
        dplyr::mutate(
          m_tag = ifelse(.data$m_tag==1 & .data$tr_pid ==0 & .data$sub_cri_match==1 & .data$pid_cri <= .data$tr_pid_cri, -1, .data$m_tag),
          pid = ifelse(
            .data$tr_m_tag==-1 & .data$pid!=0 & !.data$tr_pid %in% c(0,NA) & .data$pid_cri >= .data$tr_pid_cri &  .data$sub_cri_match==1,
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
          m_tag = ifelse(.data$sn==.data$tr_sn & .data$m_tag ==-1, 1, .data$m_tag )
        )

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
      cat(paste("\n",fmt(tagged_1)," of ", fmt(total_1)," record(s) have been assigned a group ID. ", fmt(total_1-tagged_1)," records not yet grouped.", sep =""))
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
      cat(paste("\n",fmt(removed), " record(s) with unique IDs untagged for possible matching in the next stage. The number of records not yet grouped is now ", fmt(removed + (total_1-tagged_1)),".\n", sep =""))
    }
  }

  df$pid_cri <- ifelse(df$pid_cri==Inf, "None", paste("Criteria",df$pid_cri))

  df <- df %>%
    # records not yet assigned a group ID are assigned new unique group IDs
    dplyr::mutate(pid = ifelse(.data$pid==0, .data$sn, .data$pid)) %>%
    dplyr::select(.data$sn, .data$pid, .data$pid_cri, .data$dsvr, .data$pr_sn)

   sourc_list <- as.character(sort(unique(df$dsvr)))

   df <- df %>%
     dplyr::select(.data$pid, .data$dsvr) %>%
     unique() %>%
     dplyr::mutate(val= .data$dsvr) %>%
     dplyr::arrange(.data$dsvr) %>%
     tidyr::spread(key= "dsvr", value= "val") %>%
     tidyr::unite("pid_dataset", sourc_list, sep=",") %>%
     dplyr::mutate(pid_dataset = stringr::str_replace_all(.data$pid_dataset,"NA,|,NA|^NA$","")) %>%
     dplyr::full_join(df, by="pid") %>%
     dplyr::arrange(.data$pr_sn)

   if(is.null(ds)){
     df <- dplyr::select(df, .data$sn, .data$pid, .data$pid_cri)
   }else{
     df <- dplyr::select(df, .data$sn, .data$pid, .data$pid_cri, .data$pid_dataset)
   }

   pd <- ifelse(display,"\n","")
   cat(paste(pd,"Record grouping complete - ",fmt(removed + (total_1-tagged_1))," record(s) assigned a unique ID." , sep =""))
   cat("\n ----------------------------------------------------------------------------------------------------")
  df
}
