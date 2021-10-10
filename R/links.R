#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Match records in consecutive stages of different matching criteria.
#' Each set of linked records are assigned a unique identifier with relevant group-level information.
#'
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=pid-class]{pid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Record-groups are created separately for each \code{strata}. See \code{Details}.
#' @param criteria \code{[list|atomic]}. Attributes to compare. Each element of the list is a stage in the linkage process. See \code{Details}.
#' @param sub_criteria \code{[list|\link{sub_criteria}]}. Additional matching criteria for each stage of the linkage process. See \code{\link{sub_criteria}}
#' @param data_source \code{[character]}. Data source identifier. Adds the list of data sources in each record-group to the \code{\link[=pid-class]{pid}}. Useful when the data is from multiple sources.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), return group specific information like record counts for each \code{\link[=pid-class]{pid}}.
#' @param data_links \code{[list|character]}. A set of \code{data_sources} required in each \code{\link[=pid-class]{pid}}. A record-group without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param expand \code{[logical]}. If \code{TRUE}, allows a record-group to expand with each subsequent stage of the linkage process. \emph{Not interchangeable with \code{shrink}}.
#' @param shrink \code{[logical]}. If \code{TRUE}, forces a record-group to shrink with each subsequent stage of the linkage process. \emph{Not interchangeable with \code{expand}}.
#' @param recursive \code{[logical]}. If \code{TRUE}, within each iteration of the process, a match can spawn new matches. See \code{vignette("links")}.
#' @param check_duplicates \code{[logical]}. If \code{TRUE}, within each iteration of the process, duplicates values of an attributes are not checked. The outcome of the logical test on the first instance of the value will be recycled for the duplicate values. See \code{vignette("links")}.
#' @param display \code{[character]}. Display or produce a status update. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param tie_sort \code{[atomic]}. Preferential order for breaking tied matches within a stage.
#'
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{link_records}}; \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}; \code{\link{schema}}
#'
#' @details
#' Match priority decreases with each subsequent stage of the linkage process
#' i.e. earlier stages (\code{criteria}) are considered superior.
#' Therefore, it's important for each \code{criteria} to be listed in an order of decreasing relevance.
#'
#' Records with missing \code{criteria} (\code{NA}) are skipped at each stage, while
#' records with missing \code{strata} (\code{NA}) are skipped from the entire linkage process.
#'
#' If a record is skipped, another attempt will be made to match the record at the next stage.
#' If a record does not match any other record by the end of the linkage process (or it has a missing \code{strata}),
#' it is assigned to a unique record-group.
#'
#' A \code{\link{sub_criteria}} can be used to request additional matching conditions for each stage of the linkage process.
#' When used, only records with a matching \code{criteria} and \code{sub_criteria} are linked.
#'
#' In \bold{\code{\link{links}}}, each \code{\link{sub_criteria}} must be linked to a \code{criteria}.
#' This is done by adding a \code{\link{sub_criteria}} to a named element of a \code{list}.
#' Each element's name must correspond to a stage. See below for an example of 3 \code{sub_criteria} linked to
#' \code{criteria} \code{1}, \code{5} and \code{13}.
#'
#' For example;
#'
#' \deqn{list("cr1" = sub_criteria(...), "cr5" = sub_criteria(...), "cr13" = sub_criteria(...)).}
#'
#' \code{\link{sub_criteria}} can be nested to achieve nested conditions.
#'
#' A \code{\link{sub_criteria}} can be linked to different \code{criteria}.
#'
#' Any unlinked \code{\link{sub_criteria}} will be ignored.
#'
#' By default, attributes in a \code{\link{sub_criteria}} are compared for an \code{\link{exact_match}}.
#' However, user-defined funcitons are also permitted. Such functions must meet two requirements:
#' \enumerate{
#' \item It must have two arguments named \code{`x`} and \code{`y`}, where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
#' \item It must return a \code{logical} object i.e.\code{TRUE} or \code{FALSE}.
#' }
#'
#' Every element in \code{data_links} must be named \code{"l"} (links) or \code{"g"} (groups).
#' Unnamed elements of \code{data_links} will be assumed to be \code{"l"}.
#' \itemize{
#' \item If named \code{"l"}, only groups with records from every listed \code{data_source} will remain linked.
#' \item If named \code{"g"}, only groups with records from any listed \code{data_source} will remain linked.
#' }
#'
#' See \code{vignette("links")} for more information.
#'
#' @examples
#' # Exact match
#' attr_1 <- c(1, 1, 1, NA, NA, NA, NA, NA)
#' attr_2 <- c(NA, NA, 2, 2, 2, NA, NA, NA)
#' links(criteria = list(attr_1, attr_2))
#'
#' # User-defined tests using `sub_criteria()`
#' # Matching `sex` and a 20-year age range
#' age <- c(30, 28, 40, 25, 25, 29, 27)
#' sex <- c("M", "M", "M", "F", "M", "M", "F")
#' f1 <- function(x, y) abs(y - x) %in% 0:20
#' links(criteria = sex,
#'       sub_criteria = list(cr1 = sub_criteria(age, match_funcs = f1)))
#'
#' # Multistage matches
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
#' # Group expansion
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
                  recursive = FALSE,
                  check_duplicates = FALSE,
                  tie_sort = NULL){
  tm_a <- Sys.time()

  if(class(sub_criteria) == "sub_criteria"){
    sub_criteria <- list(sub_criteria)
  }
  # Validations
  err <- err_links_checks_0(criteria, sub_criteria,
                            sn, strata, data_source, data_links,
                            display,group_stats, expand, shrink,
                            recursive, check_duplicates, tie_sort)

  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(class(criteria) != "list") criteria <- list(criteria)

  if(isTRUE(shrink)) expand <- !shrink

  # `display`
  display <- tolower(display)

  # Maximum no. of records from all criteria
  ds_len <- c(as.numeric(lapply(criteria, length)),
              as.numeric(unlist(lapply(sub_criteria, attr_eval), use.names = FALSE)))
  ds_len <- max(ds_len)
  err <- err_sn_1(sn = sn, ref_num = ds_len, ref_nm = "criteria")
  if(!isFALSE(err)) stop(err, call. = FALSE)
  if(!display %in% c("none")){
    rp_data <- di_report(tm_a, "Data validation", current_tot = ds_len)
    report <- list(rp_data)
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", rp_data[[3]], "\n"))
    }
  }
  tm_ia <- Sys.time()

  if(!is.null(data_source)) {
    class(data_source) <- "d_lazy_opts"
  }

  # Standardise inputs
  # `strata`
  if(!is.null(strata)) {
    class(strata) <- "d_lazy_opts"
  }
  # `sn`
  pr_sn <- seq_len(ds_len)
  if(class(sn) == "NULL"){
    sn <- pr_sn
  }else{
    sn <- as.integer(sn)
  }
  # User-defined order of case-assignment
  if(!is.null(tie_sort)) {
    if(any(!class(tie_sort) %in% c("numeric", "integer", "double"))){
      tie_sort <- as.integer(as.factor(tie_sort))
    }
    if(length(tie_sort) == 1) tie_sort <- rep(tie_sort, ds_len)
  }else{
    tie_sort <- rep(0L, ds_len)
  }

  class(tie_sort) <- "d_lazy_opts"
  # `data_links`
  dl_lst <- unlist(data_links, use.names = FALSE)
  if(!all(class(data_links) == "list")){
    data_links <- list(l = data_links)
  }
  if(is.null(names(data_links))) names(data_links) <- rep("l", length(data_links))
  names(data_links) <- ifelse(names(data_links) == "", "l", names(data_links))

  # Place holders for group-level options
  tag <- rep(0L, ds_len)
  iteration <- rep(0L, ds_len)
  m_tag <- rep(0L, ds_len)
  mxp_cri <- length(criteria) + 1L
  pid_cri <- rep(mxp_cri, ds_len)
  sn_ref <- min(sn) - 1L
  pid <- rep(sn_ref, ds_len)
  link_id <- rep(sn_ref, ds_len)
  n_seq <- seq_len(ds_len)

  pids_repo <- list("pid" = pid,
                    "tag" = tag,
                    "pid_cri" = pid_cri,
                    "link_id" = link_id,
                    "sn" = sn,
                    "pr_sn" = pr_sn,
                    "iteration" = iteration,
                    "tie_sort" = tie_sort)

  if(!display %in% c("none")){
    rp_data <- di_report(tm_ia, "Data standardisation", current_tot = ds_len)
    report <- c(report, list(rp_data))
    if(display %in% c("stats_with_report", "stats")){
      cat(paste0(rp_data[[1]], ": ", rp_data[[3]], "\n"))
    }
  }
  tm_ia <- Sys.time()

  if(display != "none") cat("\n")
  i <- ite <- 1L
  while(i %in% seq_len(length(criteria)) & (min(pids_repo$tag) == 0 | shrink)){
    if(display %in% c("progress", "stats", "progress_with_report", "stats_with_report")){
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
      cri[!n_lgk] <- combi(strata[!n_lgk], cri[!n_lgk])
    }
    # Nested linkage
    if(shrink == TRUE){
      cri <- combi(cri, pids_repo$pid)
    }
    # Encode current `criteria`
    if(all(!class(cri) %in% c("numeric","integer"))){
      cri <- match(cri, cri[!duplicated(cri)])
    }

    unq_lgk <- !duplicated(cri, fromLast = TRUE) & !duplicated(cri, fromLast = FALSE)
    skp_lgk <- which(!n_lgk & !unq_lgk)
    rm(unq_lgk, n_lgk)

    if(length(skp_lgk) == 0 | length(cri) == 1) {
      if(display %in% c("progress", "stats")){
        cat(paste0("Skipped `criteria ", i,"`.\n\n"))
      }
      i <- i + 1L
      ite <- ite + 1L
      next
    }

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
      tie_sort <- pids_repo$tie_sort[skp_lgk]

      pp <- inherit(tag, cri, pid_cri, tie_sort, sn, pr_sn, expand, pid, link_id, sn_ref = sn_ref)
      pp$tag[!pp$pid %in% c(sn_ref, NA)] <- 1L
      iteration[which(pp$pid != sn_ref & iteration == 0)] <- ite

      pids_repo$pid[pp$pr_sn] <- pp$pid
      pids_repo$tag[pp$pr_sn] <- pp$tag
      pids_repo$pid_cri[pp$pr_sn][which(pids_repo$pid_cri[pp$pr_sn] == mxp_cri | (pids_repo$pid_cri[pp$pr_sn] != mxp_cri & shrink))] <- i
      pids_repo$link_id[pp$pr_sn] <- pp$link_id
      pids_repo$iteration[pp$pr_sn] <- iteration
      ite <- ite + 1L
      rm(pp)
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
      tie_sort <- pids_repo$tie_sort[skp_lgk]
      curr_sub_cri[[1]] <- reframe(curr_sub_cri[[1]], func = function(x) x[skp_lgk])

      # Flags
      cs_len <- length(cri)
      m_tag <- rep(0L, cs_len)
      min_pid <- sn_ref
      min_m_tag <- 0L

      if(display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("Checking `sub_criteria`\n")
      }
      while (min_pid == sn_ref) {
        sort_ord <- order(cri, m_tag, pid_cri, tie_sort, sn, decreasing = TRUE)
        tag <- tag[sort_ord]
        cri <- cri[sort_ord]
        pid <- pid[sort_ord]
        tag <- tag[sort_ord]
        m_tag <- m_tag[sort_ord]
        pid_cri <- pid_cri[sort_ord]
        sn <- sn[sort_ord]
        pr_sn <- pr_sn[sort_ord]
        link_id <- link_id[sort_ord]
        iteration <- iteration[sort_ord]
        tie_sort <- tie_sort[sort_ord]
        h_ri <- seq_len(length(cri))

        # Reference records
        lgk <- which(!duplicated(cri, fromLast = TRUE))
        rep_lgk <- match(cri, cri[lgk])
        tr_link_id <- (link_id[lgk])[rep_lgk]
        tr_pid_cri <- (pid_cri[lgk])[rep_lgk]
        tr_pid <- (pid[lgk])[rep_lgk]
        tr_sn <- (sn[lgk])[rep_lgk]
        ref_rd <- tr_sn == sn

        pos_repo <- make_batch_pairs(strata = cri, index_record = ref_rd, sn = order(order(pr_sn)))
        # Check the `sub_criteria`
        sub_cri_match <- eval_sub_criteria(x = curr_sub_cri[[1]],
                                           x_pos = pos_repo$x_pos,
                                           y_pos = pos_repo$y_pos,
                                           check_duplicates = check_duplicates)
        sub_cri_match <- lapply(sub_cri_match, function(x){
         x[ref_rd] <- 1
         x
        })
        rm(pos_repo)
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
        lgk <- which(h_ri %in% rep_lgk & link_id == sn_ref)
        link_id[lgk] <- tr_sn[lgk]
        rm(lgk)

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

        # Duplicate record-sets can be closed
        if(isFALSE(check_duplicates)){
          m_tag[equals_ref_rd > 0] <- 2L
          lgk <- which(pid == sn_ref & equals_ref_rd > 0)
          pid[lgk] <- sn[lgk]
        }

        # If recursive, records with pids from "Situation A" but not part of the current matches are updated with the new pid
        if(isTRUE(recursive)){
          rec_lgk <- which(pid != sn_ref & f_pid != sn_ref & pid != f_pid & tr_pid_cri == pid_cri)
          rec_o <- f_pid[rec_lgk]
          rec_u <- pid[rec_lgk]
          li_u <- link_id[rec_lgk]
          lgk <- match(f_pid, rec_o)
          r_pid <- rec_u[lgk]
          r_lid <- li_u[lgk]
          r_lgk <- which(r_pid != pid & !is.na(r_pid) & !is.na(pid))
          pid[r_lgk] <- r_pid[r_lgk]
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

        iteration[which(pid != sn_ref & iteration == 0)] <- ite
        pid_cri[which(pid_cri == mxp_cri | (pid_cri != mxp_cri & shrink))] <- i

        if(!display %in% c("none")){
          rp_data <- di_report(tm_ia, ite, length(m_tag), criteria = i, current_tagged = length(which(m_tag == 2 & iteration == ite)))
          report <- c(report, list(rp_data))
        }
        tm_ia <- Sys.time()

        # If not recursive, exclude linked records.
        if(isFALSE(recursive)){
          inc_lgk <- which(m_tag == 2)
          exc_lgk <- which(m_tag != 2)

          pids_repo$cri[pr_sn[inc_lgk]] <- cri[inc_lgk]
          pids_repo$pid[pr_sn[inc_lgk]] <- pid[inc_lgk]
          pids_repo$tag[pr_sn[inc_lgk]] <- tag[inc_lgk]
          pids_repo$pid_cri[pr_sn[inc_lgk]] <- pid_cri[inc_lgk]
          pids_repo$sn[pr_sn[inc_lgk]] <- sn[inc_lgk]
          pids_repo$link_id[pr_sn[inc_lgk]] <- link_id[inc_lgk]
          pids_repo$iteration[pr_sn[inc_lgk]] <- iteration[inc_lgk]
          pids_repo$tie_sort[pr_sn[inc_lgk]] <- tie_sort[inc_lgk]

          if(length(cri[exc_lgk]) == 0){
            next
          }
          cri <- cri[exc_lgk]
          pid <- pid[exc_lgk]
          tag <- tag[exc_lgk]
          pid_cri <- pid_cri[exc_lgk]
          sn <- sn[exc_lgk]
          link_id <- link_id[exc_lgk]
          iteration <- iteration[exc_lgk]
          tie_sort <- tie_sort[exc_lgk]

          curr_sub_cri[[1]] <- reframe(curr_sub_cri[[1]], func = function(x) x[sort(order(order(pr_sn))[exc_lgk])])
          pr_sn <- pr_sn[exc_lgk]
          m_tag <- m_tag[exc_lgk]
        }else{
          pids_repo$pid[pr_sn] <- pid
          pids_repo$tag[pr_sn] <- tag
          pids_repo$pid_cri[pr_sn] <- pid_cri
          pids_repo$link_id[pr_sn] <- link_id
          pids_repo$iteration[pr_sn] <- iteration
        }
        if(display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
          progress_bar(length(pids_repo$pid[skp_lgk][pids_repo$pid[skp_lgk] != sn_ref]),
                       cs_len, 100,
                       msg = paste0("Iteration ",
                                    fmt(ite), " (",
                                    fmt(difftime(Sys.time(), tm_ia), "difftime"),
                                    ")"))
        }
        tm_ia <- Sys.time()
        ite <- ite + 1L
      }
      if(display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("\n")
      }
    }

    if(shrink){
      ds_rid <- n_seq
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
    pids_repo$iteration[which(pids_repo$tag == 0 & pids_repo$iteration != 0)] <- 0L

    if(!display %in% c("none")){
      current_tot <- length(skp_lgk)
      assigned <- length(pids_repo$pid[skp_lgk][pids_repo$pid[skp_lgk] != sn_ref])
      if(length(curr_sub_cri) == 0){
        cat("\n")
        rp_data <- di_report(tm_ia, ite - 1, length(skp_lgk), criteria = i, current_tagged = assigned)
        report <- c(report, list(rp_data))
      }
      if(display %in% c("stats", "progress", "stats_with_report", "progress_with_report")){
        cat(paste0("Total: ", fmt(cs_len), " record(s).\n",
                   "Checked: ", fmt(current_tot), " record(s).\n",
                   "Linked: ", fmt(assigned)," record(s).\n\n"))
      }
    }
    tm_ia <- Sys.time()
    i <- i + 1L
  }

  # Skipped and unmatched records
  pids_repo$iteration[pids_repo$iteration == 0] <- ite - 1L
  if(class(strata) != "NULL"){
    pids_repo$pid_cri[which(pids_repo$pid == sn_ref & is.na(strata) & pids_repo$pid_cri == mxp_cri)] <- -1L
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

  pids <- methods::new("pid",
                       .Data = pid,
                       sn = sn,
                       pid_cri = pid_cri,
                       link_id = link_id,
                       iteration = iteration)

  r <- rle(sort(pid))
  pids@pid_total <- r$lengths[match(pid, r$values)]

  if(!is.null(data_source)){
    # Data links
    rst <- check_links(pids@.Data, data_source, data_links)

    if(!all(toupper(dl_lst) == "ANY")){
      req_links <- rst$rq
      pids@pid_total[!req_links] <- 1L
      pids@pid_cri[!req_links] <- -1L
      pids@.Data[!req_links] <- pids@sn[!req_links]
      pids@link_id[!req_links] <- pids@sn[!req_links]
      rst$ds[!req_links] <- data_source[!req_links]
    }
    pids@pid_dataset <- encode(rst$ds)
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))

  if(display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    pids <- list(pid = pids, report = as.list(do.call("rbind", lapply(report, as.data.frame))))
    class(pids$report) <- "d_report"
  }
  if(display != "none") cat("Records linked in ", tms, "!\n", sep = "")
  rm(list = ls()[ls() != "pids"])
  return(pids)
}
