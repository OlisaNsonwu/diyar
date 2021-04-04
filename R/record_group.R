#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Link  records with matching criteria in ordered stages of relevance.
#' Each set of linked records are assigned a unique identifier with relevant group-level information.
#'
#' @param df \code{[data.frame]}. Deprecated. One or more datasets appended together. See \code{Details}.
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=pid-class]{pid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Record groups are created separately for each \code{strata}. \emph{\code{NA} values in \code{strata} excludes records from the entire linkage process}.
#' @param criteria \code{[list|atomic]}. Attributes to compare. Each element of the list is a stage in the linkage process. \emph{\code{NA} values in \code{criteria} excludes records from the corresponding stage of the linkage process}. See \code{Details}.
#' @param sub_criteria \code{[list|\link{sub_criteria}]}. Additional matching condition for each stage of the linkage process. See \code{\link{sub_criteria}}
#' @param data_source \code{[character]}. Unique data source identifier. Adds the list of data sources in each record group to the \code{\link[=pid-class]{pid}}. Useful when the dataset has data from multiple sources.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), returns group-specific information like record counts for each \code{\link[=pid-class]{pid}}.
#' @param data_links \code{[list|character]}. A set of \code{data_sources} required in each \code{\link[=pid-class]{pid}}. A \code{\link[=pid-class]{pid}} without records from these \code{data_sources} will be unlinked. See \code{Details}.
#' @param expand \code{[logical]}. If \code{TRUE}, allows a record group to expand with subsequent stages of the linkage process. \emph{Not interchangeable with \code{shrink}}.
#' @param shrink \code{[logical]}. If \code{TRUE}, forces a record group to shrink with each subsequent stage of the linkage process. \emph{Not interchangeable with \code{expand}}.
#' @param recursive \code{[logical]}. If \code{TRUE}, within each iteration, a match can spawn new matches. See \code{vignette("links")}.
#' @param check_duplicates \code{[logical]}. If \code{TRUE}, within each iteration, duplicates values of an attributes are not checked. The outcome of the logical test on on the first instance of the value will be recycled for the duplicate values. See \code{vignette("links")}.
#' @param display \code{[character]}. Progress messages printed on screen. Options are; \code{"none"} (default) or, \code{"progress"} and \code{"stats"} for a progress update or a more detailed breakdown of the linkage process.
#' @param to_s4 \code{[logical]}. Deprecated. Output type - \code{\link[=pid-class]{pid}} (\code{TRUE}) or \code{data.frame} (\code{FALSE}).
#'
#' @return \code{\link[=pid-class]{pid}}
#'
#' @seealso \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}; \code{\link{schema}}
#'
#' @details
#' Match priority decreases with each subsequent stage of linkage
#' i.e. earlier stages (\code{criteria}) are considered superior.
#' Therefore, it's important for each \code{criteria} to be listed in an order of decreasing relevance.
#'
#' Records with missing \code{criteria} (\code{NA}) are skipped at each stage of the linkage process, while
#' records with missing \code{strata} (\code{NA}) are skipped from the entire linkage process.
#'
#' If a record is skipped, another attempt will be made to match the record at the next stage.
#' If a record does not match any other record by the end of the linkage process (or it has a missing \code{strata}),
#' it is assigned a unique \code{\link[=pid-class]{pid}}.
#'
#' A \code{\link{sub_criteria}} can be used to request additional matching conditions for each stage of the linkage process.
#' When used, only records with matching \code{criteria} and \code{sub_criteria} are linked.
#'
#' In \bold{\code{\link{links}}}, each \code{\link{sub_criteria}} must be linked to a \code{criteria}.
#' This is done by naming creating a \code{list} of the \code{\link{sub_criteria}}.
#' Each name must correspond to a stage. See below for an example of 3 \code{sub_criteria} linked to
#' \code{criteria} \code{1}, \code{5} and \code{13}.
#'
#' For example;
#'
#' \deqn{list("cr1" = sub_criteria, "cr5" = sub_criteria, "cr13" = sub_criteria).}
#'
#' Multiple matching criteria can be supplied as a nested \code{\link{sub_criteria}}.
#' The same \code{\link{sub_criteria}} or nested \code{\link{sub_criteria}} can be linked to different \code{criteria}.
#' Any unlinked \code{\link{sub_criteria}} will be ignored.
#'
#' By default, attributes in a \code{\link{sub_criteria}} are compared for an \code{\link{exact_match}}.
#' However, user-defined logical tests (\code{function}) are also permitted. Such tests must meet 3 requirements:
#' \enumerate{
#' \item It must be able to compare two \code{atomic} vectors
#' \item It must have two arguments named \code{`x`} and \code{`y`}, where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
#' \item It must return either \code{TRUE} or \code{FALSE}
#' }
#'
#' Every element in \code{data_links} must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements of \code{data_links} will be assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, only groups with records from every listed \code{data_source} will be retained.
#' \item If named \code{"g"}, only groups with records from any listed \code{data_source} will be retained.
#' }
#'
#' \bold{\code{record_group()}} as it existed before \code{v0.2.0} has been retired.
#' It now exists to support previous code and arguments with minimal input from users.
#' Moving forward, please use \bold{\code{links()}}.
#'
#' See \code{vignette("links")} for more information.
#'
#' @examples
#' # Exact match
#' links(criteria = c("Obinna","James","Ojay","James","Obinna"))
#'
#' # User-defined tests using `sub_criteria()`
#' # Matching `sex` and a 20-year age range
#' age <- c(30, 28, 40, 25, 25, 29, 27)
#' sex <- c("M", "M", "M", "F", "M", "M", "F")
#' f1 <- function(x, y) (y - x) %in% 0:20
#' links(criteria = sex,
#'       sub_criteria = list(cr1 = sub_criteria(age, funcs = f1)))
#'
#' # Multistage linkage
#' # Relevance of matches: `forename` > `surname`
#' data(staff_records); staff_records
#' links(criteria = list(staff_records$forename, staff_records$surname),
#'       data_source = staff_records$sex)
#'
#' # Relevance of matches:
#' # `staff_id` > `age` (AND (`initials`, `hair_colour` OR `branch_office`))
#' data(missing_staff_id); missing_staff_id
#' links(criteria = list(missing_staff_id$staff_id, missing_staff_id$age),
#'       sub_criteria = list(cr2 = sub_criteria(missing_staff_id$initials,
#'                                           missing_staff_id$hair_colour,
#'                                           missing_staff_id$branch_office)),
#'       data_source = missing_staff_id$source_1)
#'
#' # Group expansion or shrinkage
#' match_cri <- list(c(1,NA,NA,1,NA,NA),
#'                   c(1,1,1,2,2,2),
#'                   c(3,3,3,2,2,2))
#' links(criteria = match_cri, expand = TRUE)
#' links(criteria = match_cri, expand = FALSE)
#' links(criteria = match_cri, shrink = TRUE)
#'
#' @aliases links
#' @export
links <- function(criteria,
                  sub_criteria = NULL,
                  sn = NULL,
                  strata = NULL,
                  data_source = NULL,
                  data_links = "ANY",
                  display = "none",
                  group_stats = FALSE,
                  expand = TRUE,
                  shrink = FALSE,
                  recursive = TRUE,
                  check_duplicates = FALSE){
  tm_a <- Sys.time()

  if(class(sub_criteria) == "sub_criteria"){
    sub_criteria <- list(sub_criteria)
  }
  # Validations
  err <- err_links_checks_0(criteria,
                            sub_criteria,
                            sn,
                            strata,
                            data_source,
                            data_links,
                            display,
                            group_stats,
                            expand,
                            shrink)

  if(!isFALSE(err)) stop(err, call. = FALSE)
  if(class(criteria) != "list") criteria <- list(criteria)

  if(isTRUE(shrink)) expand <- !shrink

  # Maximum no. of records from all criteria
  ds_len <- c(as.numeric(lapply(criteria, length)),
              as.numeric(unlist(lapply(sub_criteria, extract_3dot_lengths), use.names = FALSE)))
  ds_len <- max(ds_len)
  err <- err_sn_1(sn = sn, ref_num = ds_len, ref_nm = "criteria")
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(!is.null(data_source)) {
    if(length(data_source) == 1) data_source <- rep(data_source, ds_len)
  }

  # Standardise inputs
  # `strata`
  if(!is.null(strata)) {
    if(length(strata) == 1) strata <- rep(strata, ds_len)
  }
  # `sn`
  pr_sn <- seq_len(ds_len)
  if(class(sn) == "NULL"){
    sn <- pr_sn
  }else{
    sn <- as.integer(sn)
  }

  # `data_links`
  dl_lst <- unlist(data_links, use.names = FALSE)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))

  # `display`
  display <- tolower(display)
  # Place holders for group-level options
  tag <- rep(0L, ds_len)
  # tag_h <- tag
  iteration <- tag
  m_tag <- tag
  mxp_cri <- length(criteria) + 1L
  pid_cri <- rep(mxp_cri, ds_len)
  sn_ref <- min(sn) - 1L
  pid <- rep(sn_ref, ds_len)
  link_id <- pid
  n_seq <- seq_len(ds_len)

  pids_repo <- list("pid" = pid,
                    "tag" = tag,
                    "pid_cri" = pid_cri,
                    "link_id" = link_id,
                    "sn" = sn,
                    "pr_sn" = pr_sn,
                    "iteration" = iteration)

  if(display != "none") cat("\n")
  i <- ite <- 1L
  while(i %in% seq_len(length(criteria)) & (min(pids_repo$tag) == 0 | shrink)){
    if(display %in% c("progress", "stats")){
      cat(paste0("`Criteria ", i,"`.\n"))
    }
    # Current stage
    cri <- criteria[[i]]
    # Standardise `criteria` input
    if (length(cri) == 1) cri <- rep(cri, ds_len)
    # Identify records to be skipped
    n_lgk <- is.na(cri)
    if(!is.null(strata)) {
      n_lgk[!n_lgk] <- is.na(strata[!n_lgk])
      cri[!n_lgk] <- paste0(strata[!n_lgk], " ", cri[!n_lgk])
    }
    # Nested linkage
    if(shrink == TRUE){
      cri <- paste0(cri, " ", pids_repo$pid)
    }
    # Encode current `criteria`
    cri <- match(cri, cri[!duplicated(cri)])
    unq_lgk <- !duplicated(cri, fromLast = TRUE) & !duplicated(cri, fromLast = FALSE)
    skp_lgk <- which(!n_lgk & !unq_lgk)
    if(shrink == TRUE){
      # Back up identifiers
      pids_repo$pid[skp_lgk] -> bkp_pid
      pids_repo$link_id[skp_lgk] -> bkp_link_id
      pids_repo$tag[skp_lgk] -> bkp_tag
      pids_repo$pid_cri[skp_lgk] -> bkp_pid_cri
      pids_repo$iteration[skp_lgk] -> bkp_iteration
      # Reset identifiers
      pids_repo$pid[skp_lgk] <- sn_ref
      pids_repo$link_id[skp_lgk] <- sn_ref
      pids_repo$tag[skp_lgk] <- 0L
      pids_repo$iteration[skp_lgk] <- 0L
    }
    if(length(skp_lgk) == 0) {
      if(display %in% c("progress", "stats")){
        cat(paste0("Skipped `criteria ", i,"`.\n\n"))
      }
      i <- i + 1L
      ite <- ite + 1L
      next
    }
    curr_sub_cri <- sub_criteria[which(names(sub_criteria) == paste0("cr", i))]
    # Stages without a `sub_criteria` are compared as `exact` matches
    if(length(curr_sub_cri) == 0){
      cri <- cri[skp_lgk]
      pid_cri <- pids_repo$pid_cri[skp_lgk]
      tag <- pids_repo$tag[skp_lgk]
      sn <- pids_repo$sn[skp_lgk]
      pr_sn <- pids_repo$pr_sn[skp_lgk]
      link_id <- pids_repo$link_id[skp_lgk]
      pid <- pids_repo$pid[skp_lgk]
      iteration <- pids_repo$iteration[skp_lgk]

      s_tag <- tag
      s_tag[s_tag != 0 & !expand] <- 1
      s_tag[!expand] <- !s_tag[!expand]
      sort_ord <- order(cri, -s_tag, pid_cri, sn, decreasing = TRUE)
      rm(s_tag)
      cri <- cri[sort_ord]
      sn <- sn[sort_ord]
      pr_sn <- pr_sn[sort_ord]
      pid <- pid[sort_ord]
      tag <- tag[sort_ord]
      link_id <- link_id[sort_ord]

      lgk <- which(!duplicated(cri, fromLast = TRUE))
      tr_pid <- (pid[lgk])[match(cri, cri[lgk])]
      tr_sn <- (sn[lgk])[match(cri, cri[lgk])]
      tr_tag <- (tag[lgk])[match(cri, cri[lgk])]

      lgk <- which(cri %in% cri[!duplicated(cri)] & tr_tag == 1 & cri > 0 & tag != 1 & expand)
      pid[lgk] <- tr_pid[lgk]
      link_id[lgk] <- tr_sn[lgk]
      lgk <- which(cri %in% cri[!duplicated(cri)] & tr_tag == 0 & cri > 0 & tag != 1)
      pid[lgk] <- tr_sn[lgk]
      link_id[lgk] <- tr_sn[lgk]
      pid[tag != 1] <- pid[tag != 1]
      link_id[tag != 1] <- link_id[tag != 1]
      tag[!pid %in% c(sn_ref, NA)] <- 1L
      iteration[pid != sn_ref & iteration == 0] <- ite

      pids_repo$pid[pr_sn] <- pid
      pids_repo$tag[pr_sn] <- tag
      pids_repo$pid_cri[pr_sn][pids_repo$pid_cri[pr_sn] == mxp_cri | (pids_repo$pid_cri[pr_sn] != mxp_cri & shrink)] <- i
      pids_repo$link_id[pr_sn] <- link_id
      pids_repo$iteration[pr_sn] <- iteration
      ite <- ite + 1L
    }else{
      # Stages with a `sub_criteria` are evaluated here
      # Only records with non-missing values are checked
      cri <- cri[skp_lgk]
      pid_cri <- pids_repo$pid_cri[skp_lgk]
      tag <- pids_repo$tag[skp_lgk]
      sn <- pids_repo$sn[skp_lgk]
      pr_sn <- pids_repo$pr_sn[skp_lgk]
      pid <- pids_repo$pid[skp_lgk]
      link_id <- pids_repo$link_id[skp_lgk]
      iteration <- pids_repo$iteration[skp_lgk]

      # Flags
      cs_len <- length(cri)
      h_ri <- seq_len(cs_len)
      m_tag <- rep(0L, cs_len)
      min_pid <- sn_ref
      min_m_tag <- 0L
      while (min_pid == sn_ref) {
        sort_ord <- order(cri, m_tag, pid_cri, sn, decreasing = TRUE)
        for(vr in c("tag","cri", "pid", "tag","m_tag",
                    "pid_cri", "sn", "pr_sn",
                    "link_id", "iteration")){
          assign(vr, get(vr)[sort_ord])
        }

        if(!is.null(strata)) {
          strata <- strata[sort_ord]
        }
        # Reference records
        lgk <- which(!duplicated(cri, fromLast = TRUE))
        rep_lgk <- match(cri, cri[lgk])
        tr_link_id <- (link_id[lgk])[rep_lgk]
        tr_pid_cri <- (pid_cri[lgk])[rep_lgk]
        tr_pid <- (pid[lgk])[rep_lgk]
        tr_sn <- (sn[lgk])[rep_lgk]
        ref_rd <- tr_sn == sn
        # Check the `sub_criteria`
        sub_cri_match <- eval_sub_criteria(x = curr_sub_cri[[1]],
                                           strata = cri,
                                           index_record = ref_rd,
                                           sn = pr_sn,
                                           check_duplicates = check_duplicates)
        if(isFALSE(check_duplicates)){
          equals_ref_rd <- sub_cri_match[[2]] | ref_rd
        }
        sub_cri_match <- sub_cri_match[[1]] | ref_rd

        # Snapshot of pid before linking records in current criteria
        f_pid <- pid
        # Records inherit pid if they match with previously tagged records
        # If recursive, records with existing pids are overwritten if they match another tagged at the same stage (Situation A)
        rep_lgk <- which(sub_cri_match > 0 &
                           (pid == sn_ref | (pid != sn_ref & tr_pid_cri == pid_cri & recursive)) &
                           !tr_pid %in% c(sn_ref, NA) &
                           ((tr_pid_cri == pid_cri & !expand) | (expand)))

        pid[rep_lgk] <- tr_pid[rep_lgk]
        link_id[rep_lgk] <- tr_sn[rep_lgk]

        # Records are assigned new pids if they do not match previously tagged records
        rep_lgk_2 <- which((((pid == sn_ref | (pid != sn_ref & tr_pid_cri == pid_cri & recursive)) &
                               tr_pid == sn_ref &
                               !is.na(tr_pid))) &
                             sub_cri_match > 0)
        pid[rep_lgk_2] <- tr_sn[rep_lgk_2]
        link_id[rep_lgk_2] <- tr_sn[rep_lgk_2]

        # If not recursive, all matches are closed (m_tag == 2)
        # Otherwise, new members of a group (m_tag == -1) are checked against other records
        rep_lgk_2 <-  which(h_ri %in% which(m_tag == 0) & h_ri %in% c(rep_lgk, rep_lgk_2))
        m_tag[which(h_ri %in% rep_lgk_2 & recursive)] <- -1L
        m_tag[which(h_ri %in% rep_lgk_2 & !recursive)] <- 2L
        m_tag[ref_rd] <- 2L

        # Duplicate records sets can be closed
        if(isFALSE(check_duplicates)){
          m_tag[equals_ref_rd > 0] <- 2L
          pid[pid == sn_ref & equals_ref_rd > 0] <- sn[pid == sn_ref & equals_ref_rd > 0]
        }

        # If recursive, records with pids from "Situation A" but not part of the current matches are updated with the new pid
        if(isTRUE(recursive)){
          rec_lgk <- which(pid != sn_ref & f_pid != sn_ref & pid != f_pid & tr_pid_cri == pid_cri)
          rec_o <- f_pid[rec_lgk]
          rec_u <- pid[rec_lgk]
          li_u <- link_id[rec_lgk]
          r_pid <- rec_u[match(f_pid, rec_o)]
          r_lid <- li_u[match(f_pid, rec_o)]
          r_lgk <- which(r_pid != pid & !is.na(r_pid) & !is.na(pid))
          pid[r_lgk] <- r_pid[r_lgk]
          link_id[r_lgk] <- r_pid[r_lgk]
        }

        # Track when to end checks for the current criteria
        lgk <- !is.na(cri)
        if(length(lgk[lgk]) != 0){
          min_pid <- min(pid[lgk])
          min_m_tag <- min(m_tag[lgk])
        } else{
          min_pid <- min(pid)
          min_m_tag <- min(m_tag)
        }

        iteration[pid != sn_ref & iteration == 0] <- ite
        pid_cri[pid_cri == mxp_cri | (pid_cri != mxp_cri & shrink)] <- i
        # If not recursive, exclude linked records.
        if(isFALSE(recursive)){
          inc_lgk <- which(m_tag == 2)
          exc_lgk <- which(m_tag != 2)
          for(vr in c("cri", "pid", "tag",
                      "pid_cri", "sn",
                      "link_id", "iteration")){
            pids_repo[[vr]][pr_sn[inc_lgk]] <- get(vr)[inc_lgk]
            assign(vr, get(vr)[exc_lgk])
          }
          pr_sn <- pr_sn[exc_lgk]
          m_tag <- m_tag[exc_lgk]
        }else{
          pids_repo$pid[pr_sn] <- pid
          pids_repo$tag[pr_sn] <- tag
          pids_repo$pid_cri[pr_sn] <- pid_cri
          pids_repo$link_id[pr_sn] <- link_id
          pids_repo$iteration[pr_sn] <- iteration
        }
        ite <- ite + 1L
        if(tolower(display) %in% c("progress", "stats") & length(curr_sub_cri) > 0) {
          msg <- paste0("Checking `sub_criteria`")
          progress_txt(length(pids_repo$pid[skp_lgk][pids_repo$pid[skp_lgk] != sn_ref]), cs_len, msg = msg)
        }
      }
    }

    if(shrink){
      ds_rid <- seq_len(ds_len)
      reset_lgk <- which(pids_repo$pid %in% pids_repo$pid[!ds_rid %in% skp_lgk] & pids_repo$pid %in% pids_repo$pid[ds_rid %in% skp_lgk] & !ds_rid %in% skp_lgk)
      if(length(reset_lgk) > 0){
        pids_repo$pid[reset_lgk] <- sn_ref
        pids_repo$link_id[reset_lgk] <- sn_ref
        pids_repo$tag[reset_lgk] <- 0L
        pids_repo$pid_cri[reset_lgk] <- mxp_cri
        pids_repo$iteration[reset_lgk] <- 0L
      }

      restore_lgk <- (!duplicated(pids_repo$pid[skp_lgk]) & !duplicated(pids_repo$pid[skp_lgk], fromLast = TRUE))
      restore_lgk <- which(!cri %in% cri[!restore_lgk])
      if(length(restore_lgk) > 0){
        pids_repo$pid[skp_lgk[restore_lgk]] <- bkp_pid[restore_lgk]
        pids_repo$link_id[skp_lgk[restore_lgk]] <- bkp_link_id[restore_lgk]
        pids_repo$tag[skp_lgk[restore_lgk]] <- bkp_tag[restore_lgk]
        pids_repo$pid_cri[skp_lgk[restore_lgk]] <- bkp_pid_cri[restore_lgk]
        pids_repo$iteration[skp_lgk[restore_lgk]] <- bkp_iteration[restore_lgk]
      }


      rm(bkp_pid, bkp_link_id, bkp_tag, bkp_pid_cri, bkp_iteration)
    }else{
      restore_lgk <- integer()
    }
    # Unlink pids with a single record for another attempt in the next stage
    tag_h <- rep(0, length(pids_repo$tag))
    pids_repo$tag <- tag_h
    pids_repo$tag[which(!pids_repo$pid %in% c(sn_ref, NA))] <- 1L
    lgk <- (!duplicated(pids_repo$pid) & !duplicated(pids_repo$pid, fromLast = TRUE))
    pids_repo$link_id[lgk] <- sn_ref
    pids_repo$pid[lgk] <- sn_ref
    pids_repo$pid_cri[lgk] <- mxp_cri

    # Flag records linked at current stage
    pids_repo$tag <- tag_h
    pids_repo$tag[which(pids_repo$pid != sn_ref)] <- 1L
    pids_repo$iteration[pids_repo$tag == 0 & pids_repo$iteration != 0] <- 0L
    if(display != "none" & length(curr_sub_cri) > 0) cat("\n")
    if(display %in% c("stats", "progress")){
      current_tot <- length(skp_lgk)
      assigned <- length(pids_repo$pid[skp_lgk][pids_repo$pid[skp_lgk] != sn_ref])
      cat(paste0("Checked: ", fmt(current_tot), " record(s).\nLinked: ", fmt(assigned)," record(s).\n\n"))
    }
    i <- i + 1L
  }

  # Skipped and unmatched records
  pids_repo$iteration[pids_repo$iteration == 0] <- ite - 1L
  if(class(strata) != "NULL"){
    pids_repo$pid_cri[pids_repo$pid == sn_ref & is.na(strata) & pids_repo$pid_cri == mxp_cri] <- -1L
  }

  pids_repo$pid -> pid
  pids_repo$pid_cri -> pid_cri
  pids_repo$link_id ->  link_id
  pids_repo$sn -> sn
  pids_repo$pr_sn -> pr_sn
  pids_repo$iteration -> iteration

  pid_cri[pid == sn_ref & pid_cri == mxp_cri] <- 0L
  link_id[pid == sn_ref] <- sn[pid == sn_ref]
  pid[pid == sn_ref] <- sn[pid == sn_ref]

  tmp_pos <- pr_sn
  fd <- match(seq_len(length(pid)), tmp_pos)

  pid_f <- pid[fd]
  pids <- methods::new("pid",
                       .Data = pid_f,
                       sn = sn[fd],
                       pid_cri = pid_cri[fd],
                       link_id = link_id[fd],
                       iteration = iteration[fd])

  r <- rle(sort(pid))
  pid_tot <- r$lengths[match(pid_f, r$values)]
  pids@pid_total = pid_tot

  if(!is.null(data_source)){
    data_source <- data_source[match(pr_sn[fd], n_seq)]
    # Data links
    rst <- check_links(pids@.Data, data_source, data_links)
    datasets <- rst$ds

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      pids@pid_total[!req_links] <- 1L
      pids@pid_cri[!req_links] <- -1L
      pids@.Data[!req_links] <- pids@sn[!req_links]
      pids@link_id[!req_links] <- pids@sn[!req_links]
      datasets[!req_links] <- data_source[!req_links]
    }

    pids@pid_dataset <- encode(datasets)
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))

  if(tolower(display) != "none") cat("Records linked in ", tms, "!\n", sep = "")
  rm(list = ls()[ls() != "pids"])
  return(pids)
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
    stop(err, call. = FALSE)
  }

  out <- bridge_record_group(df = df, args = args)
  if(out$err_cd == FALSE) {
    stop(out$err_nm, call. = FALSE)
  }
  warning(paste0("`record_group()` has been retired!:\n",
                 "i - Please use `links()` instead.\n",
                 "i - Your values were passed to `links()`."), call. = FALSE)
  if(to_s4 != TRUE){
    out <- to_df(out$err_nm)
  }else{
    out <- out$err_nm
  }
  rm(list = ls()[ls() != "out"])
  return(out)
}


#' @name links_wf_probabilistic
#' @title Probabilistic record linkage
#'
#' @description A specific use case of \code{links} for probabilistic record linkage.
#'
#' @param blocking_attribute \code{[atomic]} Subsets of the dataset.
#' @param attribute \code{[list]} Attributes to compare.
#' @param cmp_func \code{[list|function]} String comparators for each \code{attribute}. See \code{Details}.
#' @param cmp_threshold \code{[list|numeric|\link{number_line}]} Weight-thresholds for each \code{cmp_func}. See \code{Details}.
#' @param probabilistic \code{[logical]} If \code{TRUE}, scores are assigned base on Fellegi-Sunter model for probabilistic record linkage. See \code{Details}.
#' @param m_probability \code{[list|numeric]} The probability that a match from the string comparator is from the same entity.
#' @param score_threshold \code{[numeric|\link{number_line}]}. Score-threshold for linked records. See \code{Details}.
#' @param ... Arguments passed to \bold{\code{links}}
#'
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{links}}; \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}
#'
#' @details
#' \code{links_wf_probabilistic} is a wrapper function of \code{\link{links}} for probabilistic record linkage.
#' Its implementation is based on Fellegi and Sunter (1969) model for deciding if two records belong to the same entity.
#'
#' In summary, each record pairs are created and categorised as matches and non-matches (\code{cmp_func}).
#' Two probabilities (\code{m} and \code{u}) are then estimated for each record pair to score matches and non-matches.
#' The \code{m}-probability is the probability that matched records are from the same entity,
#' while \code{u}-probability is the probability that matched records are not from the same entity.
#' \code{m}-probabilities must be supplied but \code{u}-probabilities are calculated for each value of an \code{attribute}.
#' This is calculated as the frequency of each value in the dataset.
#' Record pairs whose total score are above a certain threshold (\code{score_threshold}) are assumed to belong to the same entity.
#'
#' Agreement and disagreement scores are calculated as described by Asher et al. (2020).
#'
#' For each record pair, an agreement or match score for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}(m_{i}/u_{i})}{log_2 (m_i / u_i)}
#'
#' For each record pair, a disagreement or non-match score for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}((1-m_{i})/(1-u_{i}))}{log_2 ((1-m_i) / (1-u_i))}
#'
#' where \eqn{m_{i}}{m_i} and \eqn{u_{i}}{u_i} are the \code{m} and \code{u}-probabilities for each value of attribute \eqn{i}.
#'
#' Missing data (\code{NA}) are categorised as non-matches and assigned a \code{u}-probability of \code{0}.
#'
#' By default, matches and non-matches for each \code{attribute} are determined as an \code{\link{exact_match}} with a binary outcome.
#' String comparators can also be used with thresholds (\code{cmp_threshold}) for each similarity score.
#' If \code{probabilistic} is \code{FALSE},
#' the sum of all similarity scores is used as the final score instead of deriving one from the \code{m} and \code{u}-probabilities.
#'
#' \code{links_wf_probabilistic} requires a \code{score_threshold} in advance of the linkage process.
#' This differs from the typical approach where a \code{score_threshold} is selected after the linkage process,
#' following a review of all calculated scores.
#' To help with this, \code{prob_score_range} will return the range of scores attainable for a given set of attributes.
#' Additionally, \code{id_1} and \code{id_2} can be used to link specific records pairs using, aiding the review of potential scores.
#'
#' A \code{blocking_attribute} can be used to reduce processing time by restricting comparisons to \code{strata} of the dataset.
#'
#' @references
#' Fellegi, I. P., & Sunter, A. B. (1969). A Theory for Record Linkage. \emph{Journal of the Statistical Association}, 64(328), 1183–1210. https://doi.org/10.1080/01621459.1969.10501049
#'
#' Asher, J., Resnick, D., Brite, J., Brackbill, R., & Cone, J. (2020). An Introduction to Probabilistic Record Linkage with a Focus on Linkage Processing for WTC Registries. \emph{International journal of environmental research and public health}, 17(18), 6937. https://doi.org/10.3390/ijerph17186937.
#'
#' @aliases links_wf_probabilistic
#'
#' @examples
#' # Exact matches
#' dfr <- missing_staff_id[c("staff_id",  "initials",
#'                           "hair_colour", "branch_office")]
#' score_ramge <- prob_score_range(attribute = as.list(dfr))
#' prob_pids1 <- links_wf_probabilistic(attribute = as.list(dfr),
#'                                      score_threshold = score_range$minimum_score)
#' prob_pids1
#'
#' # Other logical tests e.g. string comparators
#' # For example, matching last word in `hair_colour` and `branch_office`
#' last_word_wf <- function(x) tolower(gsub("^.* ", "", x))
#' last_word_cmp <- function(x, y) last_word_wf(x) == last_word_wf(y)
#' prob_pids2 <- links_wf_probabilistic(attribute = as.list(dfr),
#'                                      cmp_func = c(diyar::exact_match,
#'                                                   diyar::exact_match,
#'                                                   last_word_cmp,
#'                                                   last_word_cmp),
#'                                      score_threshold = score_range$mid_scorce)
#' prob_pids2
#'
#' # Results for specific record pairs
#' prob_pids3 <- links_wf_probabilistic(attribute = as.list(dfr),
#'                                      cmp_func = c(diyar::exact_match,
#'                                                   diyar::exact_match,
#'                                                   last_word_cmp,
#'                                                   last_word_cmp),
#'                                      score_threshold = score_range$mid_scorce,
#'                                      id_1 = c(1, 1, 1),
#'                                      id_2 = c(6, 7, 4))
#' prob_pids3
#' @export
links_wf_probabilistic <- function(attribute,
                                   blocking_attribute = NULL,
                                   cmp_func = diyar::exact_match,
                                   cmp_threshold = .95,
                                   probabilistic = TRUE,
                                   m_probability = .95,
                                   score_threshold = 1,
                                   id_1 = NULL, id_2 = NULL,
                                   ...
){

  err <- err_links_wf_probablistic_0(attribute = attribute,
                                     blocking_attribute = blocking_attribute,
                                     cmp_func = cmp_func,
                                     cmp_threshold = cmp_threshold,
                                     probabilistic = probabilistic,
                                     m_probability = m_probability,
                                     score_threshold = score_threshold,
                                     id_1 = id_1, id_2 = id_2)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(class(attribute) != "list"){
    attribute <- list(attribute)
  }

  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }

  if(isTRUE(probabilistic)){
    # u-probabilities
    u_probs <- lapply(attribute, function(x){
      x_cd <- match(x, x[!duplicated(x)])
      x_cd[is.na(x)] <- NA_real_
      r <- rle(x_cd[order(x_cd)])
      n <- r$lengths[match(x_cd, r$values)]
      p <- n/length(x_cd)
      p[is.na(x_cd)] <- 0
      p
    })

    lgk <- unlist(lapply(u_probs, function(x){
      any(x == 1)
    }), use.names = FALSE)
    if(any(lgk[lgk])){
      warning(paste0("Attributes with identicial values in every record are ignored:\n",
                     paste0("i - `", names(attribute)[lgk], "` was ignored!", collapse = "\n")), call. = FALSE)
    }
    attribute <- attribute[!lgk]

    # m-probabilities
    if(class(m_probability) != "list"){
      m_probability <- list(m_probability)
    }
    if(length(m_probability) == 1 & length(attribute) > 1){
      m_probability <- rep(m_probability, length(attribute))
    }
  }else{
    u_probs <- lapply(attribute, function(x){
      rep(0, length(x))
    })
  }

  # Attribute names
  attr_nm <- names(attribute)

  # Threshold for agreement in each attribute
  if(is.number_line(cmp_threshold)){
    cmp_threshold[cmp_threshold@.Data < 0] <- reverse_number_line(cmp_threshold[cmp_threshold@.Data < 0], "decreasing")
  }else{
    cmp_threshold <- number_line(cmp_threshold, Inf)
  }

  if(length(cmp_threshold) == 1 & length(attribute) > 1){
    cmp_threshold <- rep(cmp_threshold, length(attribute))
  }

  if(is.number_line(score_threshold)){
    score_threshold[score_threshold@.Data < 0] <- reverse_number_line(score_threshold[score_threshold@.Data < 0], "decreasing")
  }else{
    score_threshold <- number_line(score_threshold, Inf)
  }

  # String comparator for each attribute
  if(class(cmp_func) != "list"){
    cmp_func <- list(cmp_func)
  }
  if(length(cmp_func) == 1 & length(attribute) > 1){
    cmp_func <- rep(cmp_func, length(attribute))
  }

  # Weight or probabilistic matching
  prob_link_2 <- function(x, y){
    prob_link(x, y,
              cmp_threshold = cmp_threshold,
              m_probability = m_probability,
              score_threshold = score_threshold,
              return_weights = FALSE,
              probabistic = probabilistic,
              cmp_func = cmp_func)

  }

  # Identify identical records to skip repeat checks
  same_rec_func <- function(x, y){
    attr_n <- length(x)
    lgk <- sapply(seq_len(attr_n), function(i){
      lgk <- x[[i]] == y[[i]] | (is.na(x[[i]]) & is.na(y[[i]]))
      lgk[is.na(lgk)] <- FALSE
      lgk
    })
    if(is.null(nrow(lgk))){
      sum(lgk) == attr_n
    }else{
      rowSums(lgk) == attr_n
    }
  }

  # Re-calculate weights for linked records
  if(!is.null(id_1) & !is.null(id_2)){
    pids <- NULL
    x <- c(attribute, u_probs)
    y <- lapply(x, function(k) k[id_2])
    x <- lapply(x, function(k) k[id_1])
    thresh_lgk <- integer()
  }else{
    pids <- links(criteria = "place_holder",
                  strata = blocking_attribute,
                  sub_criteria = list("cr1" = sub_criteria(c(attribute, u_probs),
                                                           match_funcs = prob_link_2,
                                                           equal_funcs = same_rec_func)),
                  ...)
    x <- c(attribute, u_probs)
    y <- lapply(x, function(k)k[match(pids@link_id, pids@sn)])
    id_1 <- pids@sn
    id_2 <- pids@link_id
    thresh_lgk <- which(pids@pid_cri %in% -1:0)
  }

  pid_weights <- prob_link(x, y,
                           cmp_threshold = cmp_threshold,
                           m_probability = m_probability,
                           score_threshold = score_threshold,
                           return_weights = TRUE,
                           cmp_func = cmp_func,
                           probabistic = probabilistic)
  # Mask unlinked records
  pid_weights[thresh_lgk,] <- NA_real_
  pid_weights <- cbind(id_1, id_2, pid_weights)
  colnames(pid_weights)[1:2] <- c("sn_x","sn_y")
  # Output
  pids <- list(pids = pids,
               pid_weights = pid_weights)
  rm(list = ls()[ls() != "pids"])
  return(pids)
}

#' @rdname links_wf_probabilistic
#' @export
prob_score_range <- function(attribute, m_probability = .95){
  if(class(attribute) != "list"){
    attribute <- list(attribute)
  }
  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
  }
  u_probs <- lapply(attribute, function(x){
    x_cd <- match(x, x[!duplicated(x)])
    x_cd[is.na(x)] <- NA_real_
    r <- rle(x_cd[order(x_cd)])
    n <- r$lengths[match(x_cd, r$values)]
    p <- n/length(x_cd)
    p[is.na(x_cd)] <- 0
    p
  })

  lgk <- unlist(lapply(u_probs, function(x){
    any(x == 1)
  }), use.names = FALSE)
  if(any(lgk[lgk])){
    warning(paste0("Attributes with identicial values in every record will be ignored:\n",
                   paste0("i - `", names(attribute)[lgk], "` will be ignored!", collapse = "\n")), call. = FALSE)

    attribute <- attribute[!lgk]
    u_probs <- u_probs[!lgk]
  }

  if(class(m_probability) != "list"){
    m_probability <- list(m_probability)
  }
  if(length(m_probability) != 1 & any(lgk[lgk])){
    m_probability <- m_probability[!lgk]
  }
  if(length(m_probability) == 1 & length(attribute) > 1){
    m_probability <- rep(m_probability, length(attribute))
  }

  max_thresh <- sapply(seq_len(length(u_probs)), function(i){
    curr_uprob <- u_probs[[i]]
    # Exclude u-probability of '0' from agreements
    curr_uprob[curr_uprob == 0] <- 1
    curr_mprob <- m_probability[[i]]
    log2(curr_mprob/curr_uprob)
  })
  if(is.null(nrow(max_thresh))){
    max_thresh <- max(sum(max_thresh))
  }else{
    max_thresh <- max(rowSums(max_thresh))
  }
  min_thresh <- sapply(seq_len(length(u_probs)), function(i){
    curr_uprob <- u_probs[[i]]
    curr_mprob <- m_probability[[i]]
    log2((1 - curr_mprob)/(1 - curr_uprob))
  })
  if(is.null(nrow(min_thresh))){
    min_thresh <- min(sum(min_thresh))
  }else{
    min_thresh <- min(rowSums(min_thresh))
  }
  list(minimum_score = min_thresh,
       mid_scorce = (min_thresh + max_thresh)/2,
       maximum_score = max_thresh)

}


