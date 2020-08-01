#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Link records with matching criteria with different levels of relevance.
#'
#' @param df \code{data.frame}. One or more datasets appended together. See \code{Details}.
#' @param sn Unique numerical record identifier. Useful for creating familiar episode identifier. Optional.
#' @param strata Subsets of the dataset. Links are determined separately within each subset.
#' @param criteria \code{list} of attributes to compare. Comparison is done as an exact match i.e. (\code{==}). See \code{Details}.
#' @param sub_criteria \code{list} of additional attributes to compare. Comparison is done as an exact match or a user defined \code{function}. See \code{Details}..
#' @param data_source Unique data source identifier. Useful when the dataset contains data from multiple sources.
#' @param group_stats If \code{TRUE} (default), group specific information like record counts. See \code{Value}.
#' @param display Message printed on screen. Options are; \code{"none"} (default) or, \code{"progress"} and \code{"stats"} for a progress update or a more detailed breakdown of the linkage process.
#' @param to_s4 If \code{TRUE} (default), record groups are returned as a \code{\link[=pid-class]{pid}} object.
#'
#' @return \code{\link[=pid-class]{pid}} objects or \code{data.frame} if \code{to_s4} is \code{FALSE})
#'
#' \itemize{
#' \item \code{sn} - unique record identifier as provided (or generated)
#' \item \code{pid | .Data} - unique group identifier
#' \item \code{link_id} - unique record identifier of matching records
#' \item \code{pid_cri} - matching criteria
#' \item \code{pid_dataset} - data sources in each group
#' \item \code{pid_total} - number of records in each group
#' }
#'
#'
#' @seealso \code{\link{episodes}} and \code{\link{number_line}}
#'
#' @details
#' \bold{\code{links()}} performs an ordered multi-staged deterministic linkage.
#' The order (relevance/priority) is determined by the order of in which each \code{criteria} is listed.
#'
#' \code{sub_criteria} specifies additional matching conditions at each stage (\code{criteria}) of the process.
#' If \code{sub_criteria} is not \code{NULL}, only records with matching \code{criteria} and \code{sub_criteria} are grouped together.
#' If a record has missing values for any \code{criteria}, that record is skipped at that stage, and another attempt is made at the next stage.
#' If there are no matches for a record at every stage, that record is assigned a unique group ID.
#'
#' By default, records are compared as an exact match i.e. the equivalent of \code{(==)} however, user defined functions are also permitted.
#' The function must be able to compare two atomic vectors,
#' return only \code{TRUE} or \code{FALSE},
#' and have two arguments - \code{x} for the attribute and \code{tr_x} for what it'll be compared against.
#'
#' A match at each stage is considered more relevant than a match at the next stage. Therefore, \code{criteria} should always be listed in order of decreasing relevance.
#'
#' \code{data_source} - including this returns \code{pid_dataset}. This lists the source of every event in each record group.
#'
#' \bold{\code{record_group()}} as it existed before \code{v0.2.0} has been retired.
#' Its current implementation only exists to support existing code with minimal disruption. Please use \bold{\code{links()}} moving forward.
#'
#'
#' @examples
#' library(diyar)
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
#' staff_records$cri_2 <- paste0(staff_records$surname,"-", staff_records$sex)
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
#' @aliases links
#' @export
links <- function(criteria,
                  sub_criteria = NULL,
                  sn = NULL,
                  strata = NULL,
                  to_s4 = TRUE,
                  data_source = NULL,
                  data_links = "ANY",
                  display = "progress"){

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != F) stop(err, call. = F)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != F) stop(err, call. = F)

  if(class(criteria) != "list") criteria <- list(criteria)

  err <- err_criteria_1(criteria)
  if(err != F) stop(err, call. = F)

  err <- err_criteria_2(criteria)
  if(err != F) stop(err, call. = F)

  err <- err_sn_1(sn =sn, ref_num = length(criteria[[1]]), ref_nm = "criteria")
  if(err != F) stop(err, call. = F)

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, length(criteria[[1]]))
  }

  if(!is.null(strata)) {
    if(length(strata) == 1) strata <- rep(strata, length(criteria[[1]]))
  }

  cri_no <- length(criteria)
  pr_sn <- 1:length(criteria[[1]])

  if(class(sn) == "NULL"){
    sn <- 1:length(criteria[[1]])
  }
  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])
  tag <- rep(0, length(criteria[[1]]))
  m_tag <- tag
  pid_cri <- rep(Inf, length(criteria[[1]]))
  sn_ref <- min(sn) - 1
  pid <- rep(sn_ref, length(criteria[[1]]))
  link_id <- pid

  # lgk <- is.na(strata)
  # skipped <- pr_sn[is.na(strata)]
  if(display != "none") cat("\n")
  for(i in 1:cri_no){

    # lgk <- is.na(cri)
    # cri <- match(cri, cri[!duplicated(cri)]) + max_strata
    cri <- criteria[[i]]
    cri <- cri[match(1:length(cri), pr_sn)]
    lgk <- is.na(cri)
    if(!is.null(strata)) {
      lgk[!lgk] <- is.na(strata[!lgk])
      cri[!lgk] <- paste0(strata[!lgk], " ", cri[!lgk])
    }
    cri <- match(cri, cri[!duplicated(cri)])
    cri[lgk] <- NA_real_

    names(cri) <- 1:length(cri)
    force_check <- rep(0, length(cri))
    skip <- force_check
    m_tag <- force_check
    min_pid <- sn_ref
    min_m_tag <- 0
    c <- 0
    tot <- length(cri)

    while (min_pid==sn_ref | min_m_tag==-1) {
      #if(c+1 >1 & display ) cat(paste("\nMatching criteria ",i,": iteration ",c+1, sep=""))

      sort_ord <- order(cri, skip, -force_check, -tag, m_tag, pid_cri, sn, decreasing = T)
      for(vr in c("force_check","tag","cri",
                  "skip", "pid", "tag","m_tag",
                  "pid_cri", "sn", "pr_sn")){
        assign(vr, get(vr)[sort_ord])
      }

      if(!is.null(strata)) {
        strata <- strata[sort_ord]
      }
      # Reference records
      r <- rle(cri)
      p <- as.numeric(names(r$values))
      #cri_tot <- r$lengths
      tr_link_id <- rep(link_id[which(names(cri) %in% p)], r$lengths)
      tr_pid_cri <- rep(pid_cri[which(names(cri) %in% p)], r$lengths)
      tr_cri <- rep(cri[which(names(cri) %in% p)], r$lengths)
      tr_tag <- rep(tag[which(names(cri) %in% p)], r$lengths)
      tr_pid <- rep(pid[which(names(cri) %in% p)], r$lengths)
      tr_sn <- rep(sn[which(names(cri) %in% p)], r$lengths)

      curr_sub_cri <- sub_criteria[which(names(sub_criteria) == paste0("s",i))]
      if(length(curr_sub_cri) > 0){
        sub_cri_match <- sapply(curr_sub_cri, function(set){
          set_match <- sapply(set, function(a){
            x <- a[[1]][match(pr_sn, 1:length(cri))]
            tr_x <- rep(x[which(names(cri) %in% p)], r$lengths)
            f <- a[[2]]
            lgk <- as.numeric(f(x, tr_x))
            ifelse(is.na(lgk), 0, lgk)
          })
          ifelse(rowSums(set_match) > 0, 1, 0)
        })
        sub_cri_match <- ifelse(rowSums(sub_cri_match) == ncol(sub_cri_match), 1, 0)
      }else{
        sub_cri_match <- rep(1, length(cri))
      }

      # Matches in subcriteria
      #sub_cri_match <- ifelse(!sub_crx_func(df) %in% c(NA, FALSE),1,0)
      #sub_cri_match <- 1

      m_tag <- ifelse(m_tag==1 &
                        sub_cri_match > 0 &
                        pid_cri <= tr_pid_cri,
                      -1, m_tag)
      m_tag <- ifelse(sub_cri_match > 0 &
                        m_tag ==-1 &
                        pid == tr_pid & !is.na(tr_pid),
                      1, m_tag)
      pid <- ifelse(
        (m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid == sn_ref & !is.na(tr_pid)),
        tr_pid, pid
      )

      #inherit pid
      pid <- ifelse(
        pid == sn_ref & !tr_pid %in% c(sn_ref, NA) & sub_cri_match > 0,
        tr_pid, pid
      )

      #assign new pid
      pid <- ifelse(
        pid == sn_ref & tr_pid == sn_ref & !is.na(tr_pid) & sub_cri_match > 0,
        tr_sn, pid
      )


      link_id <- ifelse(
        (link_id == sn_ref & !is.na(tr_link_id) & sub_cri_match > 0) |
          ((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid==sn_ref & !is.na(tr_pid))),
        tr_sn, link_id
      )

      m_tag <- ifelse(pid != sn_ref & m_tag != -1,1, m_tag)
      m_tag <- ifelse(sn == tr_sn & !is.na(tr_sn) & m_tag ==-1, 1, m_tag)


      skip <- ifelse(m_tag ==-1 & !is.na(m_tag), 0, ifelse(sub_cri_match > 0, 1, skip))
      lgk <- !is.na(cri)
      if(length(lgk[lgk]) != 0){
        min_pid <- min(pid[lgk])
        min_m_tag <- min(m_tag[lgk])
      } else{
        min_pid <- min(pid)
        min_m_tag <- min(m_tag)
      }

      if(tolower(display) == "progress" & length(curr_sub_cri) > 0) {
        msg <- paste0("Checking `sub_criteria` in `criteria ", i, "`")
        progress_bar(length(m_tag[m_tag == 1])/tot, nchar(msg), msg = msg)
      }
      c <- c+1
    }
    if(display != "none" & length(curr_sub_cri) > 0) cat("\n")
    # rst <- rle(sort(pid))
    # rst <- rst$values[rst$lengths == 1]
    # pid[which(pid %in% rst)] <- sn_ref

    if(display %in% c("progress", "stats")){
      cat(paste0("Checked `criteria ", i,"`.\n"))
    }
    if(display == "stats"){
      assigned <- length(tag[tag == 0 & !pid %in% c(sn_ref, NA)])
      removed <- length(tag[!duplicated(pid) & !duplicated(pid, fromLast=TRUE)])
      current_tot <- length(tag[tag == 0 & !pid %in% c(sn_ref, NA)])
      cat(paste0(fmt(current_tot), " records(s): ",  fmt(current_tot-removed)," linked to record groups and ", fmt(removed)," with no link."))
    }
    tag <- ifelse(pid %in% c(sn_ref, NA), 0, 1)
    link_id[!duplicated(pid) & !duplicated(pid, fromLast=TRUE)] <- sn_ref
    pid[!duplicated(pid) & !duplicated(pid, fromLast=TRUE)] <- sn_ref
    tag <- ifelse(pid != sn_ref, 1, 0)
    pid_cri[tag ==1 & pid_cri == Inf] <- i
  }

  if(class(strata) != "NULL"){
    pid_cri[pid == sn_ref & is.na(strata) & pid_cri == Inf] <- -1
  }
  pid_cri[pid == sn_ref & pid_cri == Inf] <- 0
  link_id[pid == sn_ref] <- sn[pid == sn_ref]
  pid[pid == sn_ref] <- sn[pid == sn_ref]
  #pid[match(1:length(cri), pr_sn)]

  tmp_pos <- pr_sn
  fd <- match(1:length(pid), tmp_pos)

  r <- rle(sort(pid))
  pid_f <- pid[fd]
  pids <- methods::new("pid",
               .Data = pid_f,
               sn = sn[fd],
               pid_cri = pid_cri[fd],
               link_id = link_id[fd],
               pid_total = r$lengths[match(pid_f, r$values)])

  if(!is.null(data_source)){
    data_source <- data_source[match(pr_sn[fd], 1:length(pids))]
    # Data links
    #names(e) <- tmp_pos
    rst <- check_links(pids@.Data, data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      pids@pid_total[req_links == F] <- 1
      pids@pid_cri[req_links == F] <- 0
      pids@.Data[req_links == F] <- pids@sn[req_links == F]
      pids@link_id[req_links == F] <- pids@sn[req_links == F]
      datasets[req_links == F] <- data_source[req_links == F]
    }
    pids@pid_dataset <- datasets
  }

  if(display == "stats") cat("\n")
  if(display != "none"){
    cri_dst <- table(pids@pid_cri)
    cri_n <- as.numeric(names(cri_dst))
    cri_dst <- c(cri_dst[cri_n > 0], cri_dst[cri_n == 0], cri_dst[cri_n == -1])
    cri_dst <- cri_dst[!is.na(cri_dst)]
    cri_n <- as.numeric(names(cri_dst))
    cri_dst <- paste0("   ", pid_cri_l(cri_n), ": ", fmt(cri_dst), collapse = "\n")
    summ <- paste0("\nSummary.\n",
                   "Groups:     ", fmt(length(pids@.Data[!duplicated(pids@.Data)])), "\n",
                   "Records:    ", fmt(tot), "\n",
                   cri_dst, "\n"
                   )
    cat(summ)
  }

  if(display == "none") cat("Data linkage complete!\n")
  pids
}
#' @export
#' @rdname links
sub_criteria <- function(..., funcs = NULL){
  err <- err_sub_criteria_1(...)
  if(err != F) stop(err, call. = F)

  err <- err_sub_criteria_3dot_1(...)
  if(err != F) stop(err, call. = F)

  if(class(funcs) == "NULL"){
    funcs <- list(function(x, y) x == y & !is.na(x) & !is.na(y))
  }else{
    if(class(funcs) != "list") funcs <- list(funcs)

    err <- err_sub_criteria_2(funcs)
    if(err != F) stop(err, call. = F)

    err <- err_sub_criteria_3(funcs)
    if(err != F) stop(err, call. = F)

    err <- err_sub_criteria_4(..., funcs = funcs)
    if(err != F) stop(err, call. = F)
  }

  if(length(funcs) == 1){
    funcs <- rep(list(funcs), length(list(...)))
    funcs <- unlist(funcs)
    funcs_l <- 1
  }else{
    funcs_l <- length(funcs)
  }

  x <- function(x, y) list(x, y)
  sub_cris <- mapply(x, list(...), funcs, SIMPLIFY = F)

  err <- err_sub_criteria_5(sub_cris, funcs_l)
  if(err != F) stop(err, call. = F)

  err <- err_sub_criteria_6(sub_cris, funcs_l)
  if(err != F) stop(err, call. = F)

  attr(sub_cris, "sub_criteria") <- T
  sub_cris
}

#' @export
#' @rdname links
range_matching <- function(x, tr_x){
  overlap(as.number_line(x@gid), tr_x)
}

record_group_legacy <- function(df, sn=NULL, criteria,
                                sub_criteria=NULL, strata = NULL, data_source = NULL,
                                group_stats=FALSE, display=TRUE, to_s4 = TRUE){

  if(missing(df)) stop("argument 'df' is missing, with no default")
  if(missing(criteria)) stop("argument 'criteria' is missing, with no default")

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
  st <- enq_vr(substitute(strata))

  sub_cri_lst <- unlist(sub_criteria, use.names = FALSE)
  cri_lst <- enq_vr(substitute(criteria))

  # Check that col names exist
  if(any(!unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)) %in% names(df))){
    missing_cols <- subset(unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)), !unique(c(rd_sn, ds, st, sub_cri_lst, cri_lst)) %in% names(df))
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

  # Strata
  if(is.null(st)){
    df$strt <- "A"
  }else{
    df$strt <- eval(parse(text = paste0("paste0(",paste0("df$", st, collapse = ",'-',"),")")))
  }

  # Prep
  df <- df[unique(c("sn",cri_lst,sub_cri_lst,"dsvr", "strt"))]
  df$pr_sn <- 1:nrow(df)
  df$m_tag <- df$tag <- 0
  df$pid_cri <- Inf
  df$link_id <- df$pid <- sn_ref <- min(df$sn)-1
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
  sub_cri_lst <- unique(unlist(sub_criteria, use.names = FALSE))

  nls <- lapply(names(df), function(x){
    is.number_line(df[[x]])
  })

  nls_nms <- names(df)[as.logical(nls)]
  if(length(nls_nms) >0){
    nls <- df[nls_nms]

    for(i in nls_nms){
      df[[i]] <- df$pr_sn
    }
  }

  # Range matching
  range_match <- function(x, tr_x) {
    if(any(!diyar::overlaps(diyar::as.number_line(x@gid), x))) {
      rng_i <- paste(which(!diyar::overlaps(diyar::as.number_line(x@gid), x)), collapse = ",", sep="")
      rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]

      stop(paste("Range matching error: Actual value (gid) is out of range in '",rng_d,"$",rng_v,"[c(",rng_i,")]'",sep=""))
    }

    diyar::overlaps(diyar::as.number_line(x@gid), tr_x)
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
    df$cri <- ifelse(df[[cri_lst[i]]] %in% c("", NA), NA, paste0(df$strt,"-", df[[cri_lst[i]]]))

    # Fetch corresponding matching criteria
    attr <- attributes(sub_criteria)[["names"]]
    attr <- subset(attr, grepl(paste("s",i,sep=""), attr))
    curr_attr <- ifelse(length(attr)==0, FALSE, TRUE)

    if(curr_attr){
      func_1 <- function(x){
        ifelse(class(nls[[x]]) == "number_line",
               paste0("range_match(nls$",x,"[df2$",x, "], ", "nls$",x,"[df2$tr_",x,"])"),
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
      # TR <- TR[order(TR$cri, TR$skip, -TR$force_check, -TR$tag, TR$m_tag, TR$pid_cri, TR$sn),]
      TR <- dplyr::arrange(TR, TR$cri, TR$skip, -TR$force_check, -TR$tag, TR$m_tag, TR$pid_cri, TR$sn)
      TR <- TR[!duplicated(TR$cri),]
      TR <- TR[unique(c("pid","link_id","m_tag","tag", "sn","pid_cri","cri",curr_sub_cri_lst))]
      names(TR) <- paste0("tr_", names(TR))

      # df <- merge(df, TR, by.x="cri", by.y="tr_cri", all.x=T)
      df <- dplyr::left_join(df, TR, by= c("cri"="tr_cri"))

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


      df$link_id <- ifelse(
        (df$link_id==sn_ref & !is.na(df$tr_link_id) & df$sub_cri_match==1) |
          ((df$m_tag==-1 & df$pid!=sn_ref) | (df$sub_cri_match==1 & df$pid==sn_ref & !is.na(df$tr_pid))),
        df$tr_sn, df$link_id
      )

      df$m_tag <- ifelse(df$pid !=sn_ref & df$m_tag != -1,1, df$m_tag)
      df$m_tag <- ifelse(df$sn==df$tr_sn & !is.na(df$tr_sn) & df$m_tag ==-1, 1, df$m_tag )


      df$skip <- ifelse(df$m_tag ==-1 & !is.na(df$m_tag), 0, ifelse(df$sub_cri_match==1, 1, df$skip))
      min_pid <- min(df$pid[!df$cri %in% c("",NA)])
      min_m_tag <- min(df$m_tag[!df$cri %in% c("",NA)])
      df <- df[c("sn", "pr_sn", "pid", "link_id", "pid_cri", "strt", "cri", cri_lst, sub_cri_lst, "tag", "m_tag", "skip", "dsvr", "force_check")]
      c <- c+1
    }

    tagged_1 <- length(df$pid[!df$pid %in% c(sn_ref,NA) & df$tag ==0])
    total_1 <- length(df$pid[df$tag ==0])

    # Cases that have not been tagged for the print output
    df$tag <- ifelse(df$pid %in% c(sn_ref,NA),0,1)
    df$link_id <- ifelse(duplicated(df$pid) == FALSE & duplicated(df$pid, fromLast=TRUE) == FALSE,sn_ref,df$link_id)
    df$pid <- ifelse(duplicated(df$pid) == FALSE & duplicated(df$pid, fromLast=TRUE) == FALSE,sn_ref,df$pid)

    removed <- length(subset(df$pid, df$pid %in% c(sn_ref,NA) & df$tag ==1 ))

    df$tag <- ifelse(df$pid!=sn_ref,1,0)
    df$pid_cri <- ifelse(df$tag ==1 & df$pid_cri == Inf,i, df$pid_cri)

    if(display) {
      assigned <- tagged_1-removed
      cat(paste0("\n", fmt(total_1), " records(s): ",  fmt(assigned)," assigned to record groups and ", fmt(total_1-assigned)," left to group."))
    }
  }

  df$pid_cri <- ifelse(df$pid_cri==Inf, 0, df$pid_cri)

  # records not yet assigned a group ID are assigned new unique group IDs
  df$pid <- ifelse(df$pid==sn_ref, df$sn, df$pid)
  df$link_id <- ifelse(df$link_id==sn_ref, df$sn, df$link_id)
  df <- df[c("sn","pid","link_id", "pid_cri","dsvr","pr_sn")]

  if(is.null(ds)){
    df <- df[order(df$pr_sn), c("sn","pid", "link_id", "pid_cri","pr_sn")]
  }else{
    pds2 <- lapply(split(df$dsvr, df$pid), function(x){
      paste0(sort(unique(x)), collapse=",")
    })

    df$pid_dataset <- as.character(pds2[as.character(df$pid)])
    df <- df[order(df$pr_sn), c("sn","pid","link_id", "pid_cri","pid_dataset","pr_sn")]
  }

  if(group_stats){
    df <- df[order(df$pid),]
    df$pid_total <- rep(rle(df$pid)$lengths, rle(df$pid)$lengths)
    df <- df[order(df$pr_sn),]
  }

  df$pr_sn <- NULL

  pd <- ifelse(display,"\n","")
  cat(paste(pd,"Record grouping complete: ",fmt(removed + (total_1-tagged_1))," record(s) with a unique ID. \n" , sep =""))
  if(to_s4) df <- diyar::to_s4(df)
  df
}
