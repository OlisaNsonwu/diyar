#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Link records with matching criteria with different levels of relevance.
#'
#' @param df \code{data.frame}. One or more datasets appended together. See \code{Details}.
#' @param sn Unique numerical record identifier. Useful for creating familiar episode identifier. Optional.
#' @param strata Subsets of the dataset. Links are determined separately within each subset.
#' @param criteria \code{list} of attributes to compare. Comparison is done as an exact match i.e. (\code{==}). See \code{Details}.
#' @param sub_criteria \code{list} of additional attributes to compare. Comparison is done as an exact match or a user defined \code{function}. See \code{\link{sub_criteria}}
#' @param data_source Unique data source identifier. Useful when the dataset contains data from multiple sources.
#' @param group_stats If \code{TRUE} (default), group specific information like record counts. See \code{Value}.
#' @param data_links A set of \code{data_sources} required in each episode. \code{stratas} without these will be skipped, and episodes without these will be unlinked. See \code{Details}.
#' @param expand If \code{TRUE}, allows the increases in size of a record group at subsequent stages of the linkage process.
#' @param shrink If \code{TRUE}, allows the reduction in size of a record group at subsequent stages of the linkage process.
#' @param display Message printed on screen. Options are; \code{"none"} (default) or, \code{"progress"} and \code{"stats"} for a progress update or a more detailed breakdown of the linkage process.
#' @param to_s4 If \code{TRUE}, record groups are returned as a \code{\link[=pid-class]{pid}} object.
#' @param ... Arguments passed to \bold{\code{links}}
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
#' The order (relevance/priority) of this is determined by the order of in which each \code{criteria} is listed.
#'
#' \code{sub_criteria} specifies additional matching conditions for each stage (\code{criteria}) of the process.
#' If \code{sub_criteria} is not \code{NULL}, only records with a matching \code{criteria} and \code{sub_criteria} are grouped together.
#' If a record has missing values for any \code{criteria}, that record is skipped at that stage, and another attempt is made at the next stage.
#' If there are no matches for a record at every stage, that record is assigned a unique group ID.
#'
#' By default, records are compared as an exact match i.e. the equivalent of \code{(==)} however, user defined functions are also permitted.
#' The function must be able to compare two atomic vectors,
#' return only \code{TRUE} or \code{FALSE},
#' and have two arguments - \code{x} for the attribute and \code{y} for what it'll be compared against.
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
#' # Exact match
#' links(criteria = c("Obinna","James","Ojay","James","Obinna"))
#'
#' # Use defined tests using `sub_criteria()`
#' # Matching `sex` an + 20-year age gaps
#' age <- c(30, 28, 40, 25, 25, 29, 27)
#' sex <- c("M", "M", "M", "F", "M", "M", "F")
#' f1 <- function(x, y) (y - x) %in% 0:20
#' links(criteria = sex,
#'       sub_criteria = list(s1 = sub_criteria(age, funcs = f1)))
#'
#' # Matching `sex` an +/- 20-year age gaps
#' f2 <- function(x, y) abs(y - x) %in% abs(0:20)
#' links(criteria = sex,
#'       sub_criteria = list(s1 = sub_criteria(age, funcs = f2)))
#'
#' # Multistage linkage
#' # Relevance of matches: `forename` > `surname`
#' data(staff_records); staff_records
#' links(staff_records,
#'       criteria = list(staff_records$forename, staff_records$surname),
#'       data_source = staff_records$sex)
#'
#' # Relevance of matches:
#' # `staff_id` > `age` AND (`initials`, `hair_colour` or `branch_office`)
#' data(missing_staff_id); missing_staff_id
#' links(criteria = list(missing_staff_id$staff_id, missing_staff_id$age),
#'       sub_criteria = list(s2 = sub_criteria(missing_staff_id$initials,
#'                                           missing_staff_id$hair_colour,
#'                                           missing_staff_id$branch_office)),
#'       data_source = missing_staff_id$source_1)
#' @aliases links
#' @export
links <- function(criteria,
                  sub_criteria = NULL,
                  sn = NULL,
                  strata = NULL,
                  data_source = NULL,
                  data_links = "ANY",
                  display = "progress",
                  group_stats = F,
                  expand = TRUE,
                  shrink = FALSE){

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != F) stop(err, call. = F)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != F) stop(err, call. = F)

  if(class(criteria) != "list") criteria <- list(criteria)

  err <- err_criteria_1(criteria)
  if(err != F) stop(err, call. = F)

  err <- err_criteria_2(criteria)
  if(err != F) stop(err, call. = F)

  err <- err_criteria_3(criteria, sub_criteria)
  if(err != F) stop(err, call. = F)

  ds_len <- as.numeric(lapply(criteria, length))
  ds_len_b <- sapply(sub_criteria, function(x){
    sapply(x, function(x){
      length(x[[1]]) })
  })
  ds_len <- max(c(ds_len, unlist(ds_len_b, use.names = F)))
  rm(ds_len_b)

  err <- err_sn_1(sn =sn, ref_num = ds_len, ref_nm = "criteria")
  if(err != F) stop(err, call. = F)

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, ds_len)
  }

  if(!is.null(strata)) {
    if(length(strata) == 1) strata <- rep(strata, ds_len)
  }

  pr_sn <- 1:ds_len

  if(class(sn) == "NULL"){
    sn <- 1:ds_len
  }
  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])
  tag <- rep(0, ds_len)
  m_tag <- tag
  pid_cri <- rep(Inf, ds_len)
  sn_ref <- min(sn) - 1
  pid <- rep(sn_ref, ds_len)
  link_id <- pid

  # lgk <- is.na(strata)
  # skipped <- pr_sn[is.na(strata)]
  if(display != "none") cat("\n")
  for(i in 1:length(criteria)){
    cri <- criteria[[i]]
    if (length(cri) == 1) cri <- rep(cri, ds_len)
    #cri <- cri[match(1:ds_len, pr_sn)]
    cri <- cri[match(pr_sn, seq_len(ds_len))]

    lgk <- is.na(cri)
    if(!is.null(strata)) {
      lgk[!lgk] <- is.na(strata[!lgk])
      cri[!lgk] <- paste0(strata[!lgk], " ", cri[!lgk])
    }

    if(shrink == T){
      cri <- paste0(cri, " ", pid)
      pid[T==T] <- sn_ref
      link_id[T==T] <- sn_ref
    }

    cri <- match(cri, cri[!duplicated(cri)])
    cri[lgk] <- NA_real_

    names(cri) <- 1:ds_len
    force_check <- rep(0, ds_len)
    skip <- force_check
    m_tag <- force_check
    min_pid <- sn_ref
    min_m_tag <- 0
    c <- 0
    tot <- ds_len

    while (min_pid == sn_ref | min_m_tag == -1) {
      sort_ord <- order(cri, skip, -force_check, -tag, m_tag, pid_cri, sn, decreasing = T)
      for(vr in c("force_check","tag","cri",
                  "skip", "pid", "tag","m_tag",
                  "pid_cri", "sn", "pr_sn", "link_id")){
        assign(vr, get(vr)[sort_ord])
      }

      if(!is.null(strata)) {
        strata <- strata[sort_ord]
      }
      # Reference records
      r <- rle(cri)
      p <- as.numeric(names(r$values))
      tr_link_id <- rep(link_id[which(names(cri) %in% p)], r$lengths)
      tr_pid_cri <- rep(pid_cri[which(names(cri) %in% p)], r$lengths)
      #tr_cri <- rep(cri[which(names(cri) %in% p)], r$lengths)
      tr_tag <- rep(tag[which(names(cri) %in% p)], r$lengths)
      tr_pid <- rep(pid[which(names(cri) %in% p)], r$lengths)
      tr_sn <- rep(sn[which(names(cri) %in% p)], r$lengths)

      curr_sub_cri <- sub_criteria[which(names(sub_criteria) == paste0("cr", i))]
      if(length(curr_sub_cri) > 0){
        sub_cri_match <- sapply(1:length(curr_sub_cri), function(i){
          set <- curr_sub_cri[[i]]
          set_match <- sapply(1:length(set), function(j){
            a <- set[[j]]
            x <- a[[1]]
            if(length(x) == 1){
              x <- rep(x, ds_len)
            }
            x <- x[match(pr_sn, seq_len(ds_len))]
            y <- rep(x[which(names(cri) %in% p)], r$lengths)
            f <- a[[2]]
            #lgk <- try(f(x, y), silent = T)
            lgk <- f(x, y)
            if(class(lgk) == "try-error" | class(lgk) != "logical"){
              if(class(lgk) == "try-error"){
                err <- attr(lgk, "condition")$message
              }else{
                err <- "Output is not a `logical` object"
              }

              err <- paste0("Unable to evaluate `funcs-", j, "` at `sub_criteria` \"", names(curr_sub_cri[i]),"\":\n",
                            "i - Each `func` must have the following syntax and output.\n",
                            "i - Syntax ~ `func(x, y, ...)`.\n",
                            "i - Output ~ `TRUE` or `FALSE`.\n",
                            "X - Issue with `funcs - ", j, "` at \"", names(curr_sub_cri[i]),"\": ", err, ".")
              stop(err, call. = F)
            }
            lgk <- as.numeric(lgk)
            x <- ifelse(is.na(lgk), 0, lgk)
            if(length(x) == 1) x <- rep(x, length(cri))
            if(length(x) != length(cri)){
              err <- paste0("Output length of `funcs` must be 1 or the same as `criteria`:\n",
                            "i - Unexpected length for `funcs-", j, "` at \"", names(curr_sub_cri[i]),"\":\n",
                            "i - Expecting a length of 1 of ", length(cri), ".\n",
                            "X - Length is ", length(x), ".")
              stop(err, call. = F)
            }
            return(x)
          })
          ifelse(rowSums(set_match) > 0, 1, 0)
        })
        sub_cri_match <- ifelse(rowSums(sub_cri_match) == ncol(sub_cri_match) | tr_sn == sn, 1, 0)
      }else{
        sub_cri_match <- rep(1, ds_len)
      }

      m_tag <- ifelse(m_tag == 1 &
                        sub_cri_match > 0 &
                        pid_cri <= tr_pid_cri,
                      -1, m_tag)
      m_tag <- ifelse(m_tag ==-1 &
                        sub_cri_match > 0 &
                        pid == tr_pid &
                        !is.na(tr_pid),
                      1, m_tag)

      # pid <- ifelse(((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid == sn_ref & !is.na(tr_pid))),
      #               tr_pid, pid)

      pid <- ifelse(((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid == sn_ref & !is.na(tr_pid))) &
                      ((tr_pid_cri == pid_cri & !expand) | (expand)),
                    tr_pid, pid)


      #inherit pid
      # pid <- ifelse(sub_cri_match > 0 &
      #                 pid == sn_ref &
      #                 !tr_pid %in% c(sn_ref, NA),
      #               tr_pid, pid)
      pid <- ifelse(sub_cri_match > 0 &
                      pid == sn_ref &
                      !tr_pid %in% c(sn_ref, NA) &
                      ((tr_pid_cri == pid_cri & !expand) | (expand)),
                    tr_pid, pid)

      #assign new pid
      pid <- ifelse(((pid == sn_ref &
                      tr_pid == sn_ref &
                      !is.na(tr_pid))) &
                      sub_cri_match > 0,
                    tr_sn, pid)

      # link_id <- ifelse((link_id == sn_ref & !is.na(tr_link_id) & sub_cri_match > 0) |
      #                     ((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid==sn_ref & !is.na(tr_pid))),
      #                   tr_sn, link_id)
      link_id <- ifelse(((link_id == sn_ref & !is.na(tr_link_id) & sub_cri_match > 0) |
                          ((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid==sn_ref & !is.na(tr_pid)))) &
                          ((tr_pid_cri == pid_cri & !expand) | (expand)),
                        tr_sn, link_id)

      m_tag <- ifelse(pid != sn_ref & m_tag != -1, 1, m_tag)
      m_tag <- ifelse(sn == tr_sn & !is.na(tr_sn) & m_tag == -1, 1, m_tag)

      skip <- ifelse(m_tag ==-1 &
                       !is.na(m_tag), 0,
                     ifelse(sub_cri_match > 0,
                            1, skip))
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
    link_id[!duplicated(pid) & !duplicated(pid, fromLast = TRUE)] <- sn_ref
    pid[!duplicated(pid) & !duplicated(pid, fromLast = TRUE)] <- sn_ref
    tag <- ifelse(pid != sn_ref, 1, 0)
    pid_cri[tag ==1 & (shrink | (pid_cri == Inf & !shrink))] <- i
  }

  if(class(strata) != "NULL"){
    pid_cri[pid == sn_ref & is.na(strata) & pid_cri == Inf] <- -1
  }
  pid_cri[pid == sn_ref & pid_cri == Inf] <- 0
  link_id[pid == sn_ref] <- sn[pid == sn_ref]
  pid[pid == sn_ref] <- sn[pid == sn_ref]

  tmp_pos <- pr_sn
  fd <- match(1:length(pid), tmp_pos)

  pid_f <- pid[fd]
  names(link_id) <- NULL
  names(pid_f) <- NULL
  names(sn) <- NULL
  names(pid_cri) <- NULL
  pids <- methods::new("pid",
               .Data = pid_f,
               sn = sn[fd],
               pid_cri = pid_cri[fd],
               link_id = link_id[fd])

  if(group_stats == T){
    r <- rle(sort(pid))
    pids@pid_total = r$lengths[match(pid_f, r$values)]
    names(pids@pid_total) <- NULL
  }

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

#' @rdname links
#' @export
record_group <- function(df, ..., to_s4 = TRUE){
  args <- as.list(substitute(...()))
  if (length(names(args)[names(args) == ""] > 0)){
    err <- paste0("Every argument must be specified:\n",
                  "i- `record_group()` has been retired!\n",
                  "i - Your values will be passed to `links()`.\n",
                  "i - Please specify any argument you've use.")
    stop(err, call. = F)
  }

  out <- bridge_record_group(df = df, args = args)
  if(out$err_cd == F) {
    stop(out$err_nm, call. = F)
    }
  warning(paste0("`record_group()` has been retired!:\n",
                 "i - Please use `links()` instead.\n",
                 "i - Your values were passed to `links()`."), call. = F)
  if(to_s4 != T){
    return(to_df(out$err_nm))
  }else{
    return(out$err_nm)
  }
}
#' @name sub_criteria
#' @aliases sub_criteria
#' @title Subcriteria for \bold{\code{\link{links}}}
#'
#' @description Additional \code{sub_criteria} for each matching \code{criteria} in \code{\link{links}}.
#' @param ... Additional attributes to compare.
#' @param funcs User defined logical test.
#' @return \code{function} or \code{list} of \code{functions}
#' @details
#' \bold{\code{sub_criteria()}} is the mechanism for providing a subcriteria to an instance of \bold{\code{links()}}.
#'
#' Each attribute (\code{atomic}) is compared as an exact match or with a user defined logical test.
#' The test must be supplied as a funciton with at least two arguments named \code{`x`} and \code{`y`},
#' where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
#'
#' Each attribute must have the same length.
#'
#' \code{funcs} must be a \code{function} or \code{list} of \code{functions} to use on each attribute.
#'
#' Each \code{funcs} must evaluate to \code{TRUE} or \code{FALSE}.
#'
#' Each \code{sub_criteria} must be linked to a \code{criteria} in \bold{\code{\link{links}}}.
#' You can link mutliple \code{sub_criteria} to one \code{criteria}.
#' It's not neccessary to link a \code{sub_criteria} if there's only one \code{criteria}.
#' Any unlinked \code{sub_criteria} will be ignored.
#'
#' @examples
#' library(diyar)
#'
#' sub_criteria(c(30, 28, 40, 25, 25, 29, 27), funcs = range_match)
#'
#' # Link each `sub_critera` a `criteria`.
#' list(
#' c1 = sub_criteria(c(30, 28, 40, 25, 25, 29, 27), funcs = range_match),
#' c2 = sub_criteria(c(30, 28, 40, 25, 25, 29, 27), funcs = range_match))
#' @export
sub_criteria <- function(..., funcs = NULL){
  err <- err_sub_criteria_1(...)
  if(err != F) stop(err, call. = F)

  err <- err_sub_criteria_3dot_1(...)
  if(err != F) stop(err, call. = F)

  if(class(funcs) == "NULL"){
    funcs <- list(exact_match)
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

  err <- err_sub_criteria_6(sub_cris, funcs_l)
  if(err != F) stop(err, call. = F)

  sub_cris
}

#' @name predefined_tests
#' @aliases predefined_tests
#' @title User defined tests
#'
#' @param x Every value of an attribute to compare in \bold{\code{\link{links}}}.
#' @param y One value to which is compared against all other values of the same attribute (\code{x})
#'
#' @description A collection of predefined logical tests for \bold{\code{\link{sub_criteria}}}
#' @examples
#' library(diyar)
#'
#' # `exact_match` - test that `x` is equal to `y`
#' exact_match(x = 1, y = "1")
#' exact_match(x = 1, y = 1)
#'
#' @export
exact_match <- function(x, y) {
  x==y & !is.na(x) & !is.na(y)
}

#' @rdname predefined_tests
#' @param range Difference between \code{y} and \code{x}.
#' @examples
#' # `range_match` - test that `y` is between `x` and `x + range`
#' range_match(x = 10, y = 16, range = 6)
#' range_match(x = 16, y = 10, range = 6)
#'
#' @export
range_match <- function(x, y, range = 10){
  x <- as.numeric(x); y <- as.numeric(y)
  if(!class(range) %in% c("numeric","integer")) stop(paste("Invalid object type for `range`:\n",
                                                          "i - Valid object types are: `numeric` or `integer`.\n",
                                                          "X - You've supplied a `", paste0(class(range), collapse = ", "), "`` object."), call. = F)

  if(length(range) != 1) stop(paste("`range` must have a length of 1:\n",
                                    "X - Length is ", length(range), "."), call. = F)

  (y - x <= range) & (y - x >= 0) & !is.na(x) & !is.na(y)
}

#' @rdname predefined_tests
#' @examples
#' # `range_match_legacy` - test that `x` (10) is within `y` (16 - 10)
#' x_nl <- number_line(10, 16, gid = 10)
#' y_nl <- number_line(16, 10)
#'
#' range_match_legacy(x = x_nl, y = y_nl)
#'
#' @export
range_match_legacy <- function(x, y) {
  if(any(!overlaps(as.number_line(x@gid), x))) {
    rng_i <- paste(head(which(!overlaps(as.number_line(x@gid), x)), 5), collapse = ", ", sep="")
    rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]
    stop(paste0("Range matching error: Actual value (gid) is out of range in ", "[", rng_i, "]"))
    }
  overlaps(as.number_line(x@gid), y)
}
