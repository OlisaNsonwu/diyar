#' @name links
#' @title Multistage and nested record linkage
#'
#' @description Assign unique identifiers to records based on multiple stages of different match criteria.
#'
#' @param sn \code{[integer]}. Unique record identifier. Useful for creating familiar \code{\link[=pid-class]{pid}} identifiers.
#' @param strata \code{[atomic]}. Subsets of the dataset. Record-groups are created separately for each \code{strata}. See \code{Details}.
#' @param criteria \code{[list|atomic]}. Attributes to be compared. Each element of the list is a stage in the linkage process. See \code{Details}.
#' @param sub_criteria \code{[list|\link{sub_criteria}]}. Match criteria. Must be paired to a stage of the linkage process (\code{criteria}). See \code{\link{sub_criteria}}
#' @param data_source \code{[character]}. Data source identifier. Adds the list of data sources in each record-group to the \code{\link[=pid-class]{pid}}. Useful when the data is from multiple sources.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), return group specific information like record counts for each \code{\link[=pid-class]{pid}}.
#' @param data_links \code{[list|character]}. \code{data_source} required in each \code{\link[=pid-class]{pid}}. A record-group without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param expand \code{[logical]}. If \code{TRUE}, a record-group gains new records if a match is found at the next stage of the linkage process. \emph{Not interchangeable with \code{shrink}}.
#' @param shrink \code{[logical]}. If \code{TRUE}, a record-group loses existing records if no match is found at the next stage of the linkage process. \emph{Not interchangeable with \code{expand}}.
#' @param recursive \code{[logical]}. If \code{TRUE}, within each iteration of the process, a match can spawn new matches. Ignored when \code{batched} is \code{FALSE}.
#' @param check_duplicates \code{[logical]}. If \code{TRUE}, within each iteration of the process, duplicates values of an attributes are not checked. The outcome of the logical test on the first instance of the value will be recycled for the duplicate values. Ignored when \code{batched} is \code{FALSE}.
#' @param display \code{[character]}. display a status updated or generate a status report. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param tie_sort \code{[atomic]}. Preferential order for breaking ties within a iteration.
#' @param repeats_allowed \code{[logical]} If \code{TRUE}, record-pairs with repeat values are created and compared. Ignored when \code{batched} is \code{TRUE}.
#' @param permutations_allowed \code{[logical]} If \code{TRUE}, permutations of record-pairs are created and compared. Ignored when \code{batched} is \code{TRUE}.
#' @param ignore_same_source \code{[logical]} If \code{TRUE}, only records-pairs with a different \bold{\code{data_source}} are created and compared.
#' @param batched \code{[logical]} Determines if record-pairs are created and compared in batches. Options are \code{"yes"} or \code{"no"}.
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{links_sv_probabilistic}}; \code{\link{episodes}}; \code{\link{partitions}}; \code{\link{predefined_tests}}; \code{\link{sub_criteria}}; \code{\link{schema}}
#'
#' @details
#' The priority of matches decreases with each subsequent stage of the linkage process
#' i.e. earlier stages (\code{criteria}) are considered superior.
#' Therefore, it's important that each \code{criteria} is listed in an order of decreasing relevance.
#'
#' Records with missing \code{criteria} (\code{NA} values) are skipped at their respective stage, while
#' records with missing \code{strata} (\code{NA}) are skipped at every stage.
#'
#' If a record is skipped, another attempt will be made to match the record at the next stage.
#' If a record does not match any other record by the end of the linkage process (or it has a missing \code{strata}),
#' it is assigned to a unique record-group.
#'
#' A \code{\link{sub_criteria}} can be used to introduce additional and/or nested matching conditions at each stage of the linkage process.
#' This results in only records with a matching \code{criteria} and \code{sub_criteria} being linked.
#'
#' In \bold{\code{\link{links}}}, each \code{\link{sub_criteria}} must be linked to a \code{criteria}.
#' This is done by adding a \code{\link{sub_criteria}} to a named element of a \code{list}.
#' Each element's name must correspond to a stage. For example, the list for 3 \code{sub_criteria} linked to
#' \code{criteria} \code{1}, \code{5} and \code{13} will be;
#'
#' \deqn{list(cr1 = sub\_criteria(...), cr5 = sub\_criteria(...), cr13 = sub\_criteria(...))}
#'
#' Any unlinked \code{\link{sub_criteria}} will be ignored.
#'
#' \code{\link{sub_criteria}} objects themselves can be nested.
#'
#' By default, attributes in a \code{\link{sub_criteria}} are compared for an \code{\link{exact_match}}.
#' However, user-defined functions are also permitted.
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
#' data(patient_records)
#' # An exact match on surname followed by an exact match on forename
#' stages <- as.list(patient_records[c("surname", "forename")])
#' pids_1 <- links(criteria = stages)
#'
#' # An exact match on forename followed by an exact match on surname
#' pids_2 <- links(criteria = rev(stages))
#'
#' # Nested matches
#' # Same sex OR year of birth
#' multi_cond1 <- sub_criteria(format(patient_records$dateofbirth, "%Y"),
#'                            patient_records$sex,
#'                            operator = "or")
#'
#' # Same middle name AND a 10 year age difference
#' age_diff <- function(x, y){
#'   diff <- abs(as.numeric(x) - as.numeric(y))
#'   wgt <-  diff %in% 0:(365 * 10) & !is.na(diff)
#'   wgt
#' }
#' multi_cond2 <- sub_criteria(patient_records$dateofbirth,
#'                            patient_records$middlename,
#'                            operator = "and",
#'                            match_funcs = c(age_diff, exact_match))
#'
#' # 'multi_cond1' OR 'multi_cond2'
#' nested_cond1 <- sub_criteria(multi_cond1,
#'                              multi_cond2,
#'                              operator = "or")
#'
#' # Record linkage with nested conditions
#' pids_3 <- links(criteria = stages,
#'                 sub_criteria = list(cr1 = multi_cond1,
#'                                     cr2 = multi_cond2))
#'
#' # Record linkage with multiple (two) layers of nested conditions
#' pids_4 <- links(criteria = stages,
#'                 sub_criteria = list(cr1 = nested_cond1,
#'                                     cr2 = nested_cond1))
#'
#' # Record linkage without group expansion
#' pids_5 <- links(criteria = stages,
#'                 sub_criteria = list(cr1 = multi_cond1,
#'                                     cr2 = multi_cond2),
#'                 expand = FALSE)
#'
#' # Record linkage with shrinking record groups
#' pids_6 <- links(criteria = stages,
#'                 sub_criteria = list(cr1 = multi_cond1,
#'                                     cr2 = multi_cond2),
#'                 shrink = TRUE)
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
                  tie_sort = NULL,
                  batched = "yes",
                  repeats_allowed = FALSE,
                  permutations_allowed = FALSE,
                  ignore_same_source = FALSE){

  web <- list(
    criteria = criteria,
    sub_criteria = sub_criteria,
    sn = sn,
    strata = strata,
    data_source = data_source,
    data_links = data_links,
    display = display,
    group_stats = group_stats,
    expand = expand,
    shrink = shrink,
    recursive = recursive,
    check_duplicates = check_duplicates,
    tie_sort = tie_sort,
    batched = batched,
    repeats_allowed = repeats_allowed,
    permutations_allowed = permutations_allowed,
    ignore_same_source = ignore_same_source,
    tm_a = Sys.time(),
    export = list()
  )

  criteria <- sub_criteria <- sn <-
    strata <- data_source <- data_links <-
    display <- group_stats <- expand <-
    shrink <- recursive <- check_duplicates <-
    tie_sort <- batched <- repeats_allowed <-
    permutations_allowed <- ignore_same_source <- NULL

  if(inherits(web$sub_criteria, "sub_criteria")){
    web$sub_criteria <- list(web$sub_criteria)
  }
  # Validations
  web$err <- err_links_checks_0(web$criteria, web$sub_criteria,
                                web$sn, web$strata, web$data_source, web$data_links,
                                web$display, web$group_stats, web$expand, web$shrink,
                                web$recursive, web$check_duplicates, web$tie_sort,
                                web$repeats_allowed, web$permutations_allowed, web$ignore_same_source,
                                web$batched)

  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  if(!inherits(web$criteria, "list")){
    web$criteria <- list(web$criteria)
  }
  if(isTRUE(web$shrink)){
    web$expand <- !web$shrink
  }

  # display
  web$display <- tolower(web$display)

  # Maximum no. of records from all criteria
  web$ds_len <- as.numeric(lapply(web$criteria, length))
  if(!is.null(web$sub_criteria)){
    web$ds_len <- c(
      unlist(rc_dv(lapply(web$sub_criteria, function(x){
        attr_eval(x, func = identity, simplify = FALSE)
      }), func = length), use.names = FALSE), web$ds_len)
  }
  web$ds_len <- max(web$ds_len)
  web$err <- err_sn_1(sn = web$sn, ref_num = web$ds_len, ref_nm = "criteria")
  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  if(!web$display %in% c("none")){
    web$rp_data <- di_report(
      duration = Sys.time() - web$tm_a,
      cumm_time = Sys.time() - web$tm_a,
      "Data validation",
      current_tot = web$ds_len,
      memory_used =  utils::object.size(web))
    web$report <- list(web$rp_data)
    if(web$display %in% c("stats_with_report", "stats")){
      cat(paste0(web$rp_data[[1]], ": ",
                 fmt(web$rp_data[[2]], "difftime"), "\n"))
    }
  }
  web$tm_ia <- Sys.time()

  if(!is.null(web$data_source)) {
    class(web$data_source) <- "d_lazy_opts"
  }

  # Standardise inputs
  # strata
  if(!is.null(web$strata)) {
    class(web$strata) <- "d_lazy_opts"
  }
  # sn
  web$pr_sn <- seq_len(web$ds_len)
  if(inherits(web$sn, "NULL")){
    web$sn <- web$pr_sn
  }else{
    web$sn <- as.integer(web$sn)
  }
  # User-defined order of case-assignment
  if(!is.null(web$tie_sort)) {
    if(!inherits(web$tie_sort, c("numeric", "integer", "double"))){
      web$tie_sort <- as.integer(as.factor(web$tie_sort))
    }
    if(length(web$tie_sort) == 1){
      web$tie_sort <- rep(web$tie_sort, web$ds_len)
    }
  }else{
    web$tie_sort <- rep(0L, web$ds_len)
  }

  class(web$tie_sort) <- "d_lazy_opts"
  # data_links
  web$dl_lst <- unlist(web$data_links, use.names = FALSE)
  if(!inherits(web$data_links, "list")){
    web$data_links <- list(l = web$data_links)
  }
  if(is.null(names(web$data_links))) names(web$data_links) <- rep("l", length(web$data_links))
  names(web$data_links) <- ifelse(names(web$data_links) == "", "l", names(web$data_links))

  # batched
  if(length(web$batched) == 1 & length(web$criteria) > 1){
    web$batched <- rep(web$batched, length(web$criteria))
  }

  # Place holders for group-level options
  web$tag <- rep(0L, web$ds_len)
  web$iteration <- rep(0L, web$ds_len)
  web$m_tag <- rep(0L, web$ds_len)
  web$mxp_cri <- length(web$criteria) + 1L
  web$pid_cri <- rep(web$mxp_cri, web$ds_len)
  web$sn_ref <- min(web$sn) - 1L
  web$pid <- rep(web$sn_ref, web$ds_len)
  web$link_id <- rep(web$sn_ref, web$ds_len)
  web$n_seq <- seq_len(web$ds_len)

  web$pids_repo <- list("pid" = web$pid,
                        "tag" = web$tag,
                        "pid_cri" = web$pid_cri,
                        "link_id" = web$link_id,
                        "sn" = web$sn,
                        "pr_sn" = web$pr_sn,
                        "iteration" = web$iteration,
                        "tie_sort" = web$tie_sort,
                        "data_source" = web$data_source)

  if(!is.null(web$data_source)){
    web$pids_repo$data_source <- web$data_source
  }

  if(!web$display %in% c("none")){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_ia,
      "Data standardisation",
      current_tot = web$ds_len,
      memory_used =  utils::object.size(web))
    web$report <- c(web$report, list(web$rp_data))
    if(web$display %in% c("stats_with_report", "stats")){
      cat(paste0(web$rp_data[[1]], ": ", fmt(web$rp_data[[2]], "difftime"), "\n"))
    }
  }
  web$tm_ia <- Sys.time()

  if(web$display != "none") cat("\n")
  i <- ite <- 1L
  while(i %in% seq_len(length(web$criteria)) & (min(web$pids_repo$tag) == 0 | web$shrink)){
    if(web$display %in% c("progress", "stats", "progress_with_report", "stats_with_report")){
      cat(paste0("`Criteria ", i,"`.\n"))
    }
    if(isFALSE(tolower(web$batched[i]) == "yes")){
      web$check_duplicates <- TRUE
      web$recursive <- FALSE
    }

    # Restart iteration
    web$pids_repo$iteration[which(web$pids_repo$tag == 0 & web$pids_repo$iteration != 0)] <- 0L
    # Current stage
    web$cri_l <- web$cri <- web$criteria[[i]]
    # Standardise `criteria` input
    if (length(web$cri) == 1) web$cri <- rep(web$cri, web$ds_len)
    # Identify records to be skipped
    web$n_lgk <- is.na(web$cri)
    if(!is.null(web$strata)) {
      web$n_lgk[!web$n_lgk] <- is.na(web$strata[!web$n_lgk])
      web$cri[!web$n_lgk] <- combi(web$strata[!web$n_lgk], web$cri[!web$n_lgk])
    }
    # Nested linkage
    if(web$shrink == TRUE){
      web$tmp_pid <- web$pids_repo$pid
      if(ite > 1){
        web$tmp_pid[web$pids_repo$pid == 0] <- web$pids_repo$sn[web$pids_repo$pid == 0]
      }
      web$cri <- combi(web$cri, web$tmp_pid)
      web$tmp_pid <- NULL
    }
    # Encode current `criteria`
    if(!inherits(web$cri, c("numeric","integer"))){
      web$cri <- match(web$cri, web$cri[!duplicated(web$cri)])
    }

    web$unq_lgk <- !duplicated(web$cri, fromLast = TRUE) & !duplicated(web$cri, fromLast = FALSE)
    web$skp_lgk <- which(!web$n_lgk & !web$unq_lgk)
    web$unq_lgk <- web$n_lgk <- NULL

    if(length(web$skp_lgk) == 0 | length(web$cri) == 1) {
      if(web$display %in% c("progress", "stats")){
        cat(paste0("Skipped `criteria ", i,"`.\n\n"))
      }
      i <- i + 1L
      ite <- ite + 1L
      next
    }

    if(web$shrink == TRUE){
      # Back up identifiers
      web$pids_repo$pid[web$skp_lgk] -> web$bkp_pid
      web$pids_repo$link_id[web$skp_lgk] -> web$bkp_link_id
      web$pids_repo$tag[web$skp_lgk] -> web$bkp_tag
      web$pids_repo$pid_cri[web$skp_lgk] -> web$bkp_pid_cri
      web$pids_repo$iteration[web$skp_lgk] -> web$bkp_iteration

      # Reset identifiers
      web$pids_repo$pid[web$skp_lgk] <- web$sn_ref
      web$pids_repo$link_id[web$skp_lgk] <- web$sn_ref
      web$pids_repo$tag[web$skp_lgk] <- 0L
      web$pids_repo$iteration[web$skp_lgk] <- 0L
    }

    web$curr_sub_cri <- web$sub_criteria[which(names(web$sub_criteria) == paste0("cr", i))]

    web$cri <- web$cri[web$skp_lgk]
    web$cri_l <- web$cri_l[web$skp_lgk]
    web$pid_cri <- web$pids_repo$pid_cri[web$skp_lgk]
    web$tag <- web$pids_repo$tag[web$skp_lgk]
    web$sn <- web$pids_repo$sn[web$skp_lgk]
    web$pr_sn <- web$pids_repo$pr_sn[web$skp_lgk]
    web$link_id <- web$pids_repo$link_id[web$skp_lgk]
    web$pid <- web$pids_repo$pid[web$skp_lgk]
    web$iteration <- web$pids_repo$iteration[web$skp_lgk]
    web$tie_sort <- web$pids_repo$tie_sort[web$skp_lgk]
    if(!is.null(web$data_source)){
      web$data_source <- web$pids_repo$data_source[web$skp_lgk]
    }

    # Stages without a `sub_criteria` are compared as `exact` matches
    if(length(web$curr_sub_cri) == 0){
      web$cs_len <- length(web$cri)
      web$pp <- inherit(web$tag, web$cri, web$pid_cri,
                        web$tie_sort, web$sn, web$pr_sn,
                        web$expand, web$pid, web$link_id,
                        sn_ref = web$sn_ref)

      if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
        web$lgk <- web$data_source[web$pp$sn] != web$data_source[web$pp$pid]
        web$pp <- lapply(web$pp, function(x) x[web$lgk])
      }
      web$lgk <- !web$pp$pid %in% c(web$sn_ref, NA)
      web$pp$tag[web$lgk] <- 1L

      web$pp$pid_cri[(web$pp$pid_cri == web$mxp_cri | (web$pp$pid_cri != web$mxp_cri & web$shrink))] <- i
      web$pids_repo$pid[web$pp$pr_sn] <- web$pp$pid
      web$pids_repo$tag[web$pp$pr_sn] <- web$pp$tag
      web$pids_repo$pid_cri[web$pp$pr_sn] <- web$pp$pid_cri
      web$pids_repo$link_id[web$pp$pr_sn] <- web$pp$link_id
      web$pids_repo$iteration[web$pids_repo$pid != web$sn_ref & web$pids_repo$iteration == 0] <- ite
      ite <- ite + 1L
      web$pp <- NULL
    }else{
      # Stages with a `sub_criteria` are evaluated here
      # Only records with non-missing values are checked
      web$curr_sub_cri[[1]] <- reframe(web$curr_sub_cri[[1]],
                                       func = function(x){
                                         if(length(web$skp_lgk) <= 1){
                                           x
                                         }else{
                                           x[web$skp_lgk]
                                         }
                                       })

      # Flags
      web$cs_len <- length(web$cri)
      web$m_tag <- rep(0L, web$cs_len)
      web$min_pid <- web$sn_ref
      web$min_m_tag <- 0L

      if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("Checking `sub_criteria`\n")
      }

      while (web$min_pid == web$sn_ref | any(web$m_tag == -1)) {
        web$sort_ord <- order(web$cri, web$m_tag, web$pid_cri, web$tie_sort, web$sn, decreasing = TRUE)
        web$tag <- web$tag[web$sort_ord]
        web$cri <- web$cri[web$sort_ord]
        web$cri_l <- web$cri_l[web$sort_ord]
        web$pid <- web$pid[web$sort_ord]
        web$tag <- web$tag[web$sort_ord]
        web$m_tag <- web$m_tag[web$sort_ord]
        web$pid_cri <- web$pid_cri[web$sort_ord]
        web$sn <- web$sn[web$sort_ord]
        web$pr_sn <- web$pr_sn[web$sort_ord]
        web$link_id <- web$link_id[web$sort_ord]
        web$iteration <- web$iteration[web$sort_ord]
        web$tie_sort <- web$tie_sort[web$sort_ord]
        if(!is.null(web$data_source)){
          web$data_source <- web$data_source[web$sort_ord]
        }
        web$h_ri <- seq_len(length(web$cri))

        web$indx <- order(order(web$pr_sn))
        if(isTRUE(tolower(web$batched[i]) == "yes")){
          # Reference records
          web$lgk <- which(!duplicated(web$cri, fromLast = TRUE))
          web$rep_lgk <- match(web$cri, web$cri[web$lgk])
          web$tr_link_id <- (web$link_id[web$lgk])[web$rep_lgk]
          web$tr_pid_cri <- (web$pid_cri[web$lgk])[web$rep_lgk]
          web$tr_pid <- (web$pid[web$lgk])[web$rep_lgk]
          web$tr_sn <- (web$sn[web$lgk])[web$rep_lgk]
          web$ref_rd <- web$tr_sn == web$sn
          web$pos_repo <- make_pairs_batch_legacy(strata = web$cri,
                                             index_record = web$ref_rd,
                                             sn = web$indx,
                                             ignore_same_source = web$ignore_same_source,
                                             data_source = web$data_source)
          # temp external fix
          web$s_ord <- match(web$indx, web$pos_repo$sn)
          web$pos_repo <- lapply(web$pos_repo[1:3], function(x){
            x[web$s_ord]
          })
          names(web$pos_repo)[2] <- "x_val"
          names(web$pos_repo)[3] <- "y_val"

        }else{
          if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
            web$pos_repo <- make_pairs_wf_source(seq_len(web$ds_len),
                                                 strata = web$cri,
                                                 repeats_allowed = web$repeats_allowed,
                                                 permutations_allowed = web$permutations_allowed,
                                                 data_source = web$data_source)
          }else{
            web$pos_repo <- make_pairs(strata = web$cri,
                                       x = web$indx,
                                       repeats_allowed = web$repeats_allowed,
                                       permutations_allowed = web$permutations_allowed)
          }
        }

        # Check the `sub_criteria`
        web$sub_cri_match <- eval_sub_criteria(x = web$curr_sub_cri[[1]],
                                               x_pos = web$pos_repo$x_val,
                                               y_pos = web$pos_repo$y_val,
                                               check_duplicates = web$check_duplicates)

        web$export.nm <- names(web$sub_cri_match)
        web$export.nm <- web$export.nm[!grepl("^logical|^equal", web$export.nm)]
        if(length(web$export.nm) > 0){
          web$export[[paste0("cri.", i)]][[paste0("iteration.", ite)]] <- web$sub_cri_match[web$export.nm]
          web$sub_cri_match[web$export.nm] <- NULL
        }

        if(isTRUE(tolower(web$batched[i]) == "yes")){
          if(isTRUE(web$ignore_same_source) & !is.null(web$data_source)){
            web$same_source_indx <- match(web$sn, web$pos_repo$x_val)
            web$sub_cri_match <- lapply(web$sub_cri_match, function(x){
              x <- x[web$same_source_indx]
              x[is.na(x)] <- 0L
              x
            })
            web$same_source_indx <- is.na(web$same_source_indx)
          }else{
            web$same_source_indx <- FALSE
          }

          web$sub_cri_match <- lapply(web$sub_cri_match, function(x){
            x[web$ref_rd] <- 1
            x
          })
          if(isFALSE(web$check_duplicates)){
            web$equals_ref_rd <- web$sub_cri_match[[2]] | web$ref_rd
          }
          web$sub_cri_match <- web$sub_cri_match[[1]] | web$ref_rd

        }else{
          web$lgk <- as.logical(web$sub_cri_match$logical_test)
          web$tmp_ids <- make_ids(
            x_pos = web$pos_repo$x_val[web$lgk],
            y_pos = web$pos_repo$y_val[web$lgk],
            id_length = max(web$indx)
          )

          web$tmp_ids <- lapply(web$tmp_ids, function(x){
            x[web$indx]
          })
          web$sub_cri_match <- as.logical(web$tmp_ids$linked)
          web$tmp_ids <- lapply(web$tmp_ids[c("sn", "link_id", "group_id")], function(x){
            web$pr_sn[match(x, web$indx)]
          })

          web$rep_lgk <- match(web$tmp_ids$group_id, web$pr_sn)
          web$tr_link_id <- web$link_id[web$rep_lgk]
          web$tr_pid_cri <- web$pid_cri[web$rep_lgk]
          web$tr_pid <- web$pid[web$rep_lgk]
          web$tr_sn <- web$sn[web$rep_lgk]
          web$ref_rd <- web$tr_sn == web$sn
        }
        web$rec_pairs_mem <- utils::object.size(web$pos_repo)
        web$pos_repo <- NULL

        # snapshot of pid before linking records in current criteria
        web$f_pid <- web$pid
        # Records inherit pid if they match with previously tagged records
        # If recursive, records with existing pids are overwritten if they match another tagged at the same stage (Situation A)
        web$rep_lgk <- which((web$sub_cri_match > 0 | (web$sub_cri_match == 0 & !tolower(web$batched[i]) == "yes")) &
                               (web$pid == web$sn_ref | (web$pid != web$sn_ref & web$tr_pid_cri == web$pid_cri & web$recursive)) &
                               !web$tr_pid %in% c(web$sn_ref, NA) &
                               ((web$tr_pid_cri == web$pid_cri & !web$expand) | (web$expand)))

        web$pid[web$rep_lgk] <- web$tr_pid[web$rep_lgk]
        web$lgk <- which(web$h_ri %in% web$rep_lgk & web$link_id == web$sn_ref)
        web$link_id[web$lgk] <- web$tr_sn[web$lgk]
        web$lgk <- NULL

        # Records are assigned new pids if they do not match previously tagged records
        web$rep_lgk_2 <- which((((web$pid == web$sn_ref | (web$pid != web$sn_ref & web$tr_pid_cri == web$pid_cri & web$recursive)) &
                                   web$tr_pid == web$sn_ref &
                                   !is.na(web$tr_pid))) &
                                 (web$sub_cri_match > 0 | (web$sub_cri_match == 0 & !tolower(web$batched[i]) == "yes")))
        web$pid[web$rep_lgk_2] <- web$tr_sn[web$rep_lgk_2]
        web$link_id[web$rep_lgk_2] <- web$tr_sn[web$rep_lgk_2]

        # If not recursive, all matches are closed (m_tag == 2)
        # Otherwise, new members of a group (m_tag == -1) are checked against other records
        web$rep_lgk_2 <-  which(web$h_ri %in% which(web$m_tag == 0) & web$h_ri %in% c(web$rep_lgk, web$rep_lgk_2))
        web$m_tag[which(web$h_ri %in% web$rep_lgk_2 & web$recursive)] <- -1L
        web$m_tag[which(web$h_ri %in% web$rep_lgk_2 & !web$recursive)] <- 2L
        web$m_tag[web$ref_rd] <- 2L

        # Duplicate record-sets can be closed
        if(isFALSE(web$check_duplicates)){
          web$m_tag[web$equals_ref_rd > 0] <- 2L
          web$lgk <- which(web$pid == web$sn_ref & web$equals_ref_rd > 0)
          web$pid[web$lgk] <- web$sn[web$lgk]
        }

        # Close record-pairs from the same source
        # web$lgk <- web$same_source_indx & !web$recursive
        web$lgk <- web$same_source_indx
        web$m_tag[web$lgk] <- 2L
        web$pid[web$lgk] <- web$sn[web$lgk]

        # If recursive, records with pids from "Situation A" but not part of the current matches are updated with the new pid
        if(isTRUE(web$recursive)){
          web$rec_lgk <- which(web$pid != web$sn_ref & web$f_pid != web$sn_ref & web$pid != web$f_pid & web$tr_pid_cri == web$pid_cri)
          web$rec_o <- web$f_pid[web$rec_lgk]
          web$rec_u <- web$pid[web$rec_lgk]
          web$li_u <- web$link_id[web$rec_lgk]
          web$lgk <- match(web$f_pid, web$rec_o)
          web$r_pid <- web$rec_u[web$lgk]
          web$r_lid <- web$li_u[web$lgk]
          web$r_lgk <- which(web$r_pid != web$pid & !is.na(web$r_pid) & !is.na(web$pid))
          web$pid[web$r_lgk] <- web$r_pid[web$r_lgk]
        }

        # Track when to end checks for the current criteria
        web$lgk <- !is.na(web$cri)
        if(length(web$lgk[web$lgk]) != 0){
          web$min_pid <- min(web$pid[web$lgk])
          web$min_m_tag <- min(web$m_tag[web$lgk])
        } else{
          web$min_pid <- min(web$pid)
          web$min_m_tag <- min(web$m_tag)
        }

        web$iteration[which(web$m_tag == 2 & web$iteration == 0)] <- ite
        web$pid_cri[which(web$pid_cri == web$mxp_cri | (web$pid_cri != web$mxp_cri & web$shrink))] <- i

        # If not recursive, exclude linked records.
        if(isFALSE(web$recursive)){
          web$inc_lgk <- which(web$m_tag == 2)
          web$exc_lgk <- which(web$m_tag != 2)

          web$pids_repo$cri[web$pr_sn[web$inc_lgk]] <- web$cri[web$inc_lgk]
          web$pids_repo$pid[web$pr_sn[web$inc_lgk]] <- web$pid[web$inc_lgk]
          web$pids_repo$tag[web$pr_sn[web$inc_lgk]] <- web$tag[web$inc_lgk]
          web$pids_repo$pid_cri[web$pr_sn[web$inc_lgk]] <- web$pid_cri[web$inc_lgk]
          web$pids_repo$sn[web$pr_sn[web$inc_lgk]] <- web$sn[web$inc_lgk]
          web$pids_repo$link_id[web$pr_sn[web$inc_lgk]] <- web$link_id[web$inc_lgk]
          web$pids_repo$iteration[web$pr_sn[web$inc_lgk]] <- web$iteration[web$inc_lgk]
          web$pids_repo$tie_sort[web$pr_sn[web$inc_lgk]] <- web$tie_sort[web$inc_lgk]

          if(length(web$cri[web$exc_lgk]) == 0){
            if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
              progress_bar(web$cs_len, web$cs_len, 100,
                           msg = paste0("Iteration ",
                                        fmt(ite), " (",
                                        fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                                        ")"))
            }
            ite <- ite + 1L
            break
          }

          web$cri <- web$cri[web$exc_lgk]
          web$cri_l <- web$cri_l[web$exc_lgk]
          web$pid <- web$pid[web$exc_lgk]
          web$tag <- web$tag[web$exc_lgk]
          web$pid_cri <- web$pid_cri[web$exc_lgk]
          web$sn <- web$sn[web$exc_lgk]
          web$link_id <- web$link_id[web$exc_lgk]
          web$iteration <- web$iteration[web$exc_lgk]
          web$tie_sort <- web$tie_sort[web$exc_lgk]

          web$curr_sub_cri[[1]] <- reframe(web$curr_sub_cri[[1]], func = function(x) x[sort(order(order(web$pr_sn))[web$exc_lgk])])
          web$pr_sn <- web$pr_sn[web$exc_lgk]
          web$m_tag <- web$m_tag[web$exc_lgk]
        }else{
          web$pids_repo$pid[web$pr_sn] <- web$pid
          web$pids_repo$tag[web$pr_sn] <- web$tag
          web$pids_repo$pid_cri[web$pr_sn] <- web$pid_cri
          web$pids_repo$link_id[web$pr_sn] <- web$link_id
          web$pids_repo$iteration[web$pr_sn] <- web$iteration
        }
        if(!web$display %in% c("none")){
          web$rp_data <- di_report(
            cumm_time = Sys.time() - web$tm_a,
            duration = Sys.time() - web$tm_ia,
            ite, length(web$m_tag),
            criteria = i,
            current_tagged = length(which(web$m_tag == 2 & web$iteration == ite)),
            memory_used =  utils::object.size(web) + web$rec_pairs_mem)
          web$report <- c(web$report, list(web$rp_data))
        }
        if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
          progress_bar(length(web$pids_repo$pid[web$skp_lgk][web$pids_repo$pid[web$skp_lgk] != web$sn_ref]),
                       web$cs_len, 100,
                       msg = paste0("Iteration ",
                                    fmt(ite), " (",
                                    fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                                    ")"))
        }
        web$tm_ia <- Sys.time()
        ite <- ite + 1L
      }
      if(web$display %in% c("stats_with_report", "stats", "progress_with_report", "progress")){
        cat("\n")
      }
    }

    if(web$shrink){
      web$ds_rid <- web$n_seq
      web$reset_lgk <- which(web$pids_repo$pid %in% web$pids_repo$pid[!web$ds_rid %in% web$skp_lgk] & web$pids_repo$pid %in% web$pids_repo$pid[web$ds_rid %in% web$skp_lgk] & !web$ds_rid %in% web$skp_lgk)
      if(length(web$reset_lgk) > 0){
        web$pids_repo$pid[web$reset_lgk] <- web$sn_ref
        web$pids_repo$link_id[web$reset_lgk] <- web$sn_ref
        web$pids_repo$tag[web$reset_lgk] <- 0L
        web$pids_repo$pid_cri[web$reset_lgk] <- web$mxp_cri
        web$pids_repo$iteration[web$reset_lgk] <- 0L
      }

      web$restore_lgk <- (!duplicated(web$pids_repo$pid[web$skp_lgk]) & !duplicated(web$pids_repo$pid[web$skp_lgk], fromLast = TRUE))
      web$restore_lgk <- which(!web$cri %in% web$cri[!web$restore_lgk])
      if(length(web$restore_lgk) > 0){
        web$pids_repo$pid[web$skp_lgk[web$restore_lgk]] <- web$bkp_pid[web$restore_lgk]
        web$pids_repo$link_id[web$skp_lgk[web$restore_lgk]] <- web$bkp_link_id[web$restore_lgk]
        web$pids_repo$tag[web$skp_lgk[web$restore_lgk]] <- web$bkp_tag[web$restore_lgk]
        web$pids_repo$pid_cri[web$skp_lgk[web$restore_lgk]] <- web$bkp_pid_cri[web$restore_lgk]
        web$pids_repo$iteration[web$skp_lgk[web$restore_lgk]] <- web$bkp_iteration[web$restore_lgk]
      }
      web$bkp_pid <- web$bkp_link_id <- web$bkp_tag <- web$bkp_pid_cri <- web$bkp_iteration <- NULL
    }else{
      web$restore_lgk <- integer()
    }

    # Unlink pids with a single record for another attempt in the next stage
    web$tag_h <- rep(0, length(web$pids_repo$tag))
    web$pids_repo$tag <- web$tag_h
    web$pids_repo$tag[which(!web$pids_repo$pid %in% c(web$sn_ref, NA))] <- 1L
    web$lgk <- (!duplicated(web$pids_repo$pid) & !duplicated(web$pids_repo$pid, fromLast = TRUE))
    web$pids_repo$link_id[web$lgk] <- web$sn_ref
    web$pids_repo$pid[web$lgk] <- web$sn_ref
    web$pids_repo$pid_cri[web$lgk] <- web$mxp_cri

    # Flag records linked at current stage
    web$pids_repo$tag <- web$tag_h
    web$pids_repo$tag[which(web$pids_repo$pid != web$sn_ref)] <- 1L
    if(!web$display %in% c("none")){
      web$current_tot <- length(web$skp_lgk)
      web$assigned <- length(web$pids_repo$pid[web$skp_lgk][web$pids_repo$pid[web$skp_lgk] != web$sn_ref])
      web$rp_data <- di_report(
        cumm_time = Sys.time() - web$tm_a,
        duration = Sys.time() - web$tm_ia,
        ite - 1,
        length(web$skp_lgk), criteria = i,
        current_tagged = web$assigned,
        memory_used =  utils::object.size(web))
      web$report <- c(web$report, list(web$rp_data))

      if(web$display %in% c("stats", "progress", "stats_with_report", "progress_with_report")){
        cat("\n")
        cat(paste0("Total: ", fmt(web$ds_len), " record(s).\n",
                   "Checked: ", fmt(web$current_tot), " record(s).\n",
                   "Linked: ", fmt(web$assigned)," record(s).\n\n"))
      }
    }
    web$tm_ia <- Sys.time()
    i <- i + 1L
  }

  # Skipped and unmatched records
  web$pids_repo$iteration[web$pids_repo$iteration == 0] <- ite - 1L
  if(!inherits(web$strata, "NULL")){
    web$pids_repo$pid_cri[which(web$pids_repo$pid == web$sn_ref & is.na(web$strata) & web$pids_repo$pid_cri == web$mxp_cri)] <- -1L
  }

  web$pids_repo$pid -> web$pid
  web$pids_repo$pid_cri -> web$pid_cri
  web$pids_repo$link_id ->  web$link_id
  web$pids_repo$sn -> web$sn
  web$pids_repo$pr_sn -> web$pr_sn
  web$pids_repo$iteration -> web$iteration

  web$pid_cri[web$pid == web$sn_ref & web$pid_cri == web$mxp_cri] <- 0L
  web$link_id[web$pid == web$sn_ref] <- web$sn[web$pid == web$sn_ref]
  web$pid[web$pid == web$sn_ref] <- web$sn[web$pid == web$sn_ref]

  web$pids <- methods::new("pid",
                           .Data = web$pid,
                           sn = web$sn,
                           pid_cri = web$pid_cri,
                           link_id = web$link_id,
                           iteration = web$iteration)

  web$r <- rle(sort(web$pid))
  web$pids@pid_total <- web$r$lengths[match(web$pid, web$r$values)]

  if(!is.null(web$data_source)){
    # Data links
    web$rst <- check_links(web$pids@.Data, web$pids_repo$data_source, web$data_links)

    if(!all(toupper(web$dl_lst) == "ANY")){
      web$req_links <- web$rst$rq
      web$pids@pid_total[!web$req_links] <- 1L
      web$pids@pid_cri[!web$req_links] <- -1L
      web$pids@.Data[!web$req_links] <- web$pids@web$sn[!web$req_links]
      web$pids@link_id[!web$req_links] <- web$pids@web$sn[!web$req_links]
      web$rst$ds[!web$req_links] <- web$data_source[!web$req_links]
    }
    web$pids@pid_dataset <- encode(web$rst$ds)
  }

  web$tm_z <- Sys.time()
  web$tms <- difftime(web$tm_z, web$tm_a)
  web$tms <- paste0(ifelse(round(web$tms) == 0, "< 0.01", round(as.numeric(web$tms), 2)), " ", attr(web$tms, "units"))

  if(web$display %in% c("none_with_report", "progress_with_report", "stats_with_report")){
    web$pids <- list(pid = web$pids, report = as.list(do.call("rbind", lapply(web$report, as.data.frame))))
    class(web$pids$report) <- "d_report"
  }
  if(!web$display %in% c("none", "none_with_report")) cat("Records linked in ", web$tms, "!\n", sep = "")
  if(length(web$export) > 0){
    if(inherits(web$pids, "list")){
      web$pids <- c(web$pids, web["export"])
    }else{
      web$pids <- list(pid = web$pids, export = web$export)
    }
  }
  web <- web$pids
  return(web)
}


links_dev <- function(criteria,
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
                      tie_sort = NULL,
                      batched = "yes",
                      repeats_allowed = FALSE,
                      permutations_allowed = FALSE,
                      ignore_same_source = FALSE){
  tm_a <- Sys.time()
  #
  web <- list(repo = list(
    sn = sn,
    strata = strata,
    data_source = data_source,
    tie_sort = tie_sort
  ))
  web$options <- list(
    display = display,
    group_stats = group_stats,
    expand = expand,
    shrink = shrink,
    recursive = recursive,
    check_duplicates = check_duplicates,
    batched = batched,
    repeats_allowed = repeats_allowed,
    permutations_allowed = permutations_allowed,
    ignore_same_source = ignore_same_source
  )
  web$match.cri <- list(
    criteria = criteria,
    sub_criteria = sub_criteria,
    data_links = data_links
  )
  web$export <- list()
  web$tm_a <- tm_a

  criteria <- sub_criteria <- sn <-
    strata <- data_source <- data_links <-
    display <- group_stats <- expand <-
    shrink <- recursive <- check_duplicates <-
    tie_sort <- batched <- repeats_allowed <-
    permutations_allowed <- ignore_same_source <- NULL
  #
  web$err <- err_links_checks_0(
    web$match.cri$criteria, web$match.cri$sub_criteria,
    web$repo$sn, web$repo$strata, web$repo$data_source, web$data_links,
    web$options$display, web$options$group_stats, web$options$expand, web$options$shrink,
    web$options$recursive, web$options$check_duplicates, web$repo$tie_sort,
    web$options$repeats_allowed, web$options$permutations_allowed, web$options$ignore_same_source,
    web$options$batched)
  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  #
  if(isTRUE(web$options$shrink)){
    web$options$expand <- !web$options$shrink
  }
  web$options$display <- tolower(web$options$display)

  if(!inherits(web$match.cri$criteria, "list")){
    web$match.cri$criteria <- list(web$match.cri$criteria)
  }
  #
  web$n.row <- as.numeric(lapply(web$match.cri$criteria, length))
  if(!is.null(web$match.cri$sub_criteria)){
    web$n.row <- c(
      unlist(rc_dv(lapply(web$match.cri$sub_criteria, function(x){
        attr_eval(x, func = identity, simplify = FALSE)
      }), func = length), use.names = FALSE), web$n.row)
  }
  web$n.row <- max(web$n.row)
  #
  web$err <- err_sn_1(
    sn = web$repo$sn,
    ref_num = web$n.row,
    ref_nm = "criteria")
  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  #
  web$repo$pr_sn <-
    web$repo$pid <- seq_len(web$n.row)
  web$repo$wind_id <- rep(NA_real_, web$n.row)
  #
  if(!is.null(web$repo$tie_sort)) {
    if(!inherits(web$repo$tie_sort, c("numeric", "integer", "double"))){
      web$repo$tie_sort <- as.integer(as.factor(web$repo$tie_sort))
    }
    if(length(web$repo$tie_sort) == 1){
      web$repo$tie_sort <- rep(web$repo$tie_sort, web$n.row)
    }
  }else{
    web$repo$tie_sort <- rep(0L, web$n.row)
  }
  #
  if(!inherits(web$data_links, "list")){
    web$data_links <- list(l = web$data_links)
  }
  if(is.null(names(web$data_links))){
    names(web$data_links) <- rep("l", length(web$data_links))
  }
  names(web$data_links) <- ifelse(names(web$data_links) == "", "l", names(web$data_links))
  #
  if(length(web$options$batched) == 1 & length(web$match.cri$criteria) > 1){
    web$options$batched <- rep(
      web$options$batched,
      length(web$match.cri$criteria))
  }
  #
  web$repo$tag <-
    web$repo$iteration <- rep(0L, web$n.row)
  web$repo$linked <- rep(FALSE, web$n.row)
  web$mxp_cri <- length(web$match.cri$criteria) + 1L
  web$repo$pid_cri <- rep(web$mxp_cri, web$n.row)
  #
  web$report <- list()
  if(grepl("report$", web$options$display)){
    web$rp_data <- di_report(
      cumm_time = Sys.time() - web$tm_a,
      duration = Sys.time() - web$tm_a,
      "Data Prep.",
      current_tot = web$n.row,
      memory_used =  utils::object.size(web[names(web)[names(web) != "report"]]))
    # web$rp_data <- matrix(unlist(web$rp_data), nrow = 1)
    web$report <- c(web$report, list(web$rp_data))
  }
  web$tm_ia <- Sys.time()
  if(!grepl("^none", web$options$display)){
    cat("\n")
  }
  web$i <- web$ite <- web$itx <- web$counts$max_indexes <- 1L
  while(web$i %in% seq_len(length(web$match.cri$criteria))){
    #
    web$i_nm <- ifelse(!is.null(names(web$match.cri$criteria[web$i])),
                       paste0(web$i, ": ", names(web$match.cri$criteria[web$i])),
                       web$i)
    if(grepl("^progress|^stats", web$options$display)){
      cat(paste0("`Criteria ", web$i_nm,"`.\n"))
    }
    #
    web$tmp$sub.cri <- web$match.cri$sub_criteria[which(names(web$match.cri$sub_criteria) == paste0("cr", web$i))]
    web$options$is_nested <- length(web$tmp$sub.cri) > 0
    web$options$recursive <- ifelse(
      tolower(web$options$batched[web$i]) %in% "no" |
        isFALSE(web$options$is_nested),
      FALSE, web$options$recursive
    )
    web$options$check_duplicates <- ifelse(
      tolower(web$options$batched[web$i]) %in% "no" |
        isFALSE(web$options$is_nested),
      TRUE, web$options$check_duplicates
    )
    # Restart iteration
    web$repo$iteration[
      which(web$repo$linked & web$repo$iteration != 0)
    ] <- 0L
    # Attribute for current stage
    web$repo$cri_l <-
      web$repo$cri_level <-
      web$match.cri$criteria[[web$i]]
    # Reuse any place holder in `criteria`
    if(length(web$repo$cri_level) == 1){
      web$repo$cri_level <- rep(
        web$repo$cri_level,
        web$n.row)
    }
    # Records included/excluded from current stage
    web$tmp$inc_lgk <-
      # missing values for current stage/attribute
      !is.na(web$repo$cri_level)
    # unique values for current stage/attribute
    web$tmp$inc_lgk[web$tmp$inc_lgk] <-
      !(!duplicated(web$repo$cri_level[web$tmp$inc_lgk], fromLast = TRUE) &
          !duplicated(web$repo$cri_level[web$tmp$inc_lgk], fromLast = FALSE))

    web$repo$cri_level <- list(current = web$repo$cri_level)
    if(!is.null(web$repo$strata)) {
      web$tmp$inc_lgk[web$tmp$inc_lgk][is.na(web$repo$strata[web$tmp$inc_lgk])] <- FALSE
      web$repo$cri_level[["strata"]] <- web$repo$strata
    }
    #
    if(web$options$shrink == TRUE){
      web$repo$cri_level[["record_group"]] <- web$repo$linked
      web$repo$cri_level$record_group[web$repo$cri_level$record_group] <-
        web$repo$pid[web$repo$cri_level$record_group]
    }
    web$repo$cri <- rep(NA, web$n.row)
    web$repo$cri[which(web$tmp$inc_lgk)] <- combi(
      lapply(web$repo$cri_level, function(x) x[web$tmp$inc_lgk])
    )
    web$tmp$inc_lgk[
      (!duplicated(web$repo$cri[web$tmp$inc_lgk], fromLast = TRUE) &
         !duplicated(web$repo$cri[web$tmp$inc_lgk], fromLast = FALSE))
    ] <- FALSE
    web$tmp$inc_indx <- which(web$tmp$inc_lgk)
    #
    if(length(web$tmp$inc_indx) %in% 0:1 | (length(web$repo$cri_l) == 1 & isFALSE(web$options$is_nested))) {
      if(grepl("^progress|^stats", web$options$display)){
        cat(paste0("  -> Skipped.\n\n"))
      }
      web$i <- web$i + 1L
      web$ite <- web$ite + 1L
      next
    }

    if(web$options$shrink == TRUE){
      # Back up identifiers
      web$repo$pid[web$tmp$inc_indx] -> web$tmp$bkp_pid
      web$repo$tag[web$tmp$inc_indx] -> web$tmp$bkp_tag
      web$repo$linked[web$tmp$inc_indx] -> web$tmp$bkp_linked
      web$repo$pid_cri[web$tmp$inc_indx] -> web$tmp$bkp_pid_cri
      web$repo$iteration[web$tmp$inc_indx] -> web$tmp$bkp_iteration
      web$tmp$inc_indx.mm <- index_multiples(
        x = web$tmp$inc_indx,
        multiples = web$n.row,
        repeats = web$counts$max_indexes)
      web$repo$wind_id[web$tmp$inc_indx.mm] -> web$tmp$bkp_wind_id
      # Reset identifiers
      web$repo$pid[web$tmp$inc_indx] <-
        web$repo$pr_sn[web$tmp$inc_indx]
      web$repo$tag[web$tmp$inc_indx] <-
        web$repo$iteration[web$tmp$inc_indx] <- 0L
      web$repo$pid_cri[web$tmp$inc_indx] <- web$mxp_cri
      web$repo$wind_id[
        index_multiples(
          x = web$tmp$inc_indx,
          multiples = web$n.row,
          repeats = web$counts$max_indexes)
      ] <- NA_real_
    }

    if(isFALSE(web$options$is_nested)){
      web$tmp$sub.cri <- list(sub_criteria(rep(TRUE, web$n.row)))
    }

    web$repo$tag <- rep(0L, web$n.row)
    web$repo$iteration.linked <- as.logical(web$repo$tag)
    web$repo$bkp_pid <- web$repo$pid
    web$itx <- 1L

    if(grepl("^progress|^stats", web$options$display) &
       isTRUE(web$options$is_nested)){
      cat("  -> Checking `sub_criteria`\n")
    }
    while(min(web$repo$tag[web$tmp$inc_indx]) != 2) {
      #
      web$sort_ord <- order(
        web$repo$cri[web$tmp$inc_indx],
        web$repo$tag[web$tmp$inc_indx],
        web$repo$pid_cri[web$tmp$inc_indx],
        web$repo$tie_sort[web$tmp$inc_indx],
        web$repo$pr_sn[web$tmp$inc_indx])

      web$tmp$cu_set_indx <- web$tmp$inc_indx[web$sort_ord]
      web$tmp$index_cd <- !duplicated(web$repo$cri[web$tmp$cu_set_indx], fromLast = FALSE)
      if(web$options$batched[web$i] == "semi" & isTRUE(web$options$is_nested)){
        web$tmp$index_cd[web$repo$tag[web$tmp$cu_set_indx] == -1] <- TRUE
      }else if(web$options$batched[web$i] == "no" & isTRUE(web$options$is_nested)){
        web$tmp$index_cd[TRUE] <- TRUE
      }

      #
      web$tmp$batch_strata <- list(web$repo$cri[web$tmp$cu_set_indx])
      if(isFALSE(web$options$expand)){
        web$tmp$batch_strata[["pid_cri"]] <- web$repo$pid_cri[web$tmp$cu_set_indx]
      }
      if(length(web$tmp$batch_strata) > 1){
        web$tmp$batch_strata <- combi(web$tmp$batch_strata)
      }else{
        web$tmp$batch_strata <- web$tmp$batch_strata[[1]]
      }
      # Create record-pairs
      web$rec.pairs <- make_pairs_batched(
        strata = web$tmp$batch_strata,
        x = web$tmp$cu_set_indx,
        index_record = web$tmp$index_cd,
        assign_ord = seq_len(length(web$tmp$cu_set_indx)),
        look_back = web$options$permutations_allowed,
        include_repeat = web$options$repeats_allowed)

      # Maximum number of index records per episode (`max_indexes`).
      web$tmp$max_indexes <- suppressWarnings(max(web$rec.pairs$index_ord))
      # Increase the number of `wind_id` by multiples of `max_indexes`
      if(web$tmp$max_indexes > web$counts$max_indexes){
        web$repo$wind_id <- c(
          web$repo$wind_id,
          rep(rep(NA_real_, web$n.row), (web$tmp$max_indexes - web$counts$max_indexes))
        )
        web$counts$max_indexes <- web$tmp$max_indexes
      }
      #
      names(web$rec.pairs)[which(names(web$rec.pairs) == "x_val")] <- "cu_pos"
      names(web$rec.pairs)[which(names(web$rec.pairs) == "y_val")] <- "tr_pos"
      web$rec.pairs$cu_pid <- web$repo$pid[web$rec.pairs$cu_pos]
      web$rec.pairs$cu_linked <- web$repo$iteration.linked[web$rec.pairs$cu_pos]
      #
      if(isTRUE(web$options$is_nested)){
        web$rec.pairs[["rec.match"]] <- eval_sub_criteria(
          x = web$tmp$sub.cri[[1]],
          x_pos = web$rec.pairs$cu_pos,
          y_pos = web$rec.pairs$tr_pos,
          check_duplicates = web$options$check_duplicates)
      }else{
        web$rec.pairs[["rec.match"]] <- list(logical_test = rep(1, length(web$rec.pairs$cu_pos)))
      }
      web$rec.pairs$rec.match <- lapply(web$rec.pairs$rec.match, as.logical)
      #
      if(isFALSE(web$options$check_duplicates)){
        web$rec.pairs$rec.match$logical_test <- as.logical(web$rec.pairs$rec.match$logical_test) |
          as.logical(web$rec.pairs$rec.match$equal_test)
      }
      #
      web$rec.pairs$cu_pos.mi <- ((web$rec.pairs$index_ord - 1) * web$n.row) + web$rec.pairs$cu_pos
      # Flag the reference record
      web$rec.pairs$ref_rd <- web$rec.pairs$cu_pos == web$rec.pairs$tr_pos
      # Update window ids for matched or reference records
      web$rec.pairs$w.match <-
        ((web$rec.pairs$rec.match$logical_test) | web$rec.pairs$ref_rd) &
        (is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi]) |
           (!is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi]) & web$rec.pairs$cu_linked)
        )
      # & is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi])
      web$repo$wind_id[web$rec.pairs$cu_pos.mi[web$rec.pairs$w.match]] <- web$rec.pairs$tr_pos[web$rec.pairs$w.match]
      #
      web$rec.pairs$e.match <- web$rec.pairs$cu_pos %in%
        web$rec.pairs$cu_pos[
          web$rec.pairs$rec.match$logical_test &
            !web$rec.pairs$ref_rd
        ]
      web$rec.pairs$e.match <- web$rec.pairs$index_ord == 1 & web$rec.pairs$e.match
      web$rec.pairs$index_rd <- web$rec.pairs$cu_pos %in% web$rec.pairs$cu_pos[web$rec.pairs$ref_rd]
      web$rec.pairs$index_rd <- (web$rec.pairs$index_ord == 1 & web$rec.pairs$index_rd) | isFALSE(web$options$is_nested)
      #
      if(web$options$batched[web$i] == "no"){
        browser()
        web$tmp$batched_pids <- make_ids(web$rec.pairs$x_pos[web$rec.pairs$rec.match$logical_test],
                                         web$rec.pairs$y_pos[web$rec.pairs$rec.match$logical_test],
                                         id_length = max(web$rec.pairs$x_pos))

        web$tmp$batched_pids$group_id2 <- web$tmp$cu_set_indx[web$tmp$batched_pids$group_id]
        web$repo$pid[
          web$tmp$batched_pids$sn[web$tmp$batched_pids$linked]
        ] <- web$tmp$batched_pids$group_id2[web$tmp$batched_pids$linked]
      }else{
        web$repo$pid[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] <- web$repo$pid[web$rec.pairs$tr_pos[web$rec.pairs$e.match]]
      }

      if(isTRUE(web$option$recursive)){
        web$repo$ovr_lgk <- web$repo$pr_sn %in% web$rec.pairs$cu_pos[web$rec.pairs$e.match] &
          web$repo$iteration.linked
        # web$tmp$tgt_pid <- web$repo$bkp_pid
        web$tmp$tgt_pid <- web$repo$pid
        web$tmp$ovr_grp_indx <- which(web$tmp$tgt_pid %in% web$tmp$tgt_pid[web$repo$ovr_lgk])
        web$repo$pid[web$tmp$ovr_grp_indx] <- web$repo$bkp_pid[web$repo$ovr_lgk][
          match(
            web$tmp$tgt_pid[web$tmp$ovr_grp_indx],
            web$tmp$tgt_pid[web$repo$ovr_lgk])]
      }
      web$tmp$linked_lgk <- which(web$rec.pairs$e.match | web$rec.pairs$index_rd)
      web$repo$iteration.linked[web$rec.pairs$cu_pos[web$tmp$linked_lgk]] <- TRUE
      web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$index_rd]] <- 2L
      if(isFALSE(web$options$check_duplicates)){
        web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$rec.match$equal_test]] <- 2L
      }

      web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$e.match]][
        web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] != 2
      ] <- ifelse(isTRUE(web$option$recursive), -1L, 2L)

      web$repo$iteration[web$rec.pairs$cu_pos[web$tmp$linked_lgk]][
        web$repo$tag[web$rec.pairs$cu_pos[web$tmp$linked_lgk]] == 2 &
          web$repo$iteration[web$rec.pairs$cu_pos[web$tmp$linked_lgk]] == 0
      ] <- web$ite

      web$repo$pid_cri[web$rec.pairs$cu_pos[web$tmp$linked_lgk]][
        web$repo$tag[web$rec.pairs$cu_pos[web$tmp$linked_lgk]] == 2 &
          web$repo$pid_cri[web$rec.pairs$cu_pos[web$tmp$linked_lgk]] == web$mxp_cri
      ] <- web$i
      #
      web$repo$bkp_pid <- web$repo$pid
      #
      web$tmp$ite.row.n <- length(web$tmp$inc_indx)
      web$tmp$ite.linked.n <- length(which(web$repo$tag[web$tmp$inc_indx] == 2))
      #
      if(grepl("^progress|^stats", web$options$display)){
        web$msg <- progress_bar(
          n = web$tmp$ite.linked.n,
          d = web$tmp$ite.row.n,
          max_width = 100,
          msg = paste0("Iteration ",
                       fmt(web$ite), " (",
                       fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                       ")"),
          prefix_msg = "  ")
        cat(
          web$msg,
          "\r",sep="")
      }
      #
      if(grepl("report$", web$options$display)){
        web$rp_data <- di_report(
          cumm_time = Sys.time() - web$tm_a,
          duration = Sys.time() - web$tm_ia,
          criteria = web$i,
          iteration = web$ite,
          current_tagged = web$tmp$ite.linked.n,
          current_tot = web$n.row,
          memory_used =  utils::object.size(web[names(web)[names(web) != "report"]]))
        web$report <- c(web$report, list(web$rp_data))
      }
      web$tm_ia <- Sys.time()
      web$ite <- web$ite + 1L
      web$itx <- web$itx + 1L
    }
    if(web$options$shrink){
      # If sub-record-group has lost it's index
      web$reset_lgk <- which(
        # Records not in the current set checked ...
        !web$repo$pr_sn %in% web$tmp$inc_indx &
          # Previously linked to an index now associated with another record-group ...
          web$repo$pid %in% web$repo$pr_sn[web$repo$pr_sn %in% web$repo$pid[!web$repo$pr_sn %in% web$tmp$inc_indx]]
      )
      if(length(web$reset_lgk) > 0){
        web$repo$pid[web$reset_lgk] <-
          web$repo$pr_sn[web$reset_lgk]
        web$repo$tag[web$reset_lgk] <- 0L
        web$repo$linked[web$reset_lgk] <- FALSE
        web$repo$pid_cri[web$reset_lgk] <- web$mxp_cri
        web$repo$iteration[web$reset_lgk] <- 0L
        web$repo$wind_id[
          index_multiples(
            x = web$reset_lgk,
            multiples = web$n.row,
            repeats = web$counts$max_indexes)
        ] <- NA_real_
      }

      web$restore_lgk <- (!duplicated(web$repo$pid[web$tmp$inc_indx]) &
                            !duplicated(web$repo$pid[web$tmp$inc_indx], fromLast = TRUE))
      web$restore_lgk <- which(!web$repo$cri %in% web$repo$cri[!web$restore_lgk])
      if(length(web$restore_lgk) > 0){
        web$repo$pid[web$tmp$inc_indx[web$restore_lgk]] <- web$tmp$bkp_pid[web$restore_lgk]
        web$repo$tag[web$tmp$inc_indx[web$restore_lgk]] <- web$tmp$bkp_tag[web$restore_lgk]
        web$repo$linked[web$tmp$inc_indx[web$restore_lgk]] <- web$tmp$bkp_linked[web$restore_lgk]
        web$repo$pid_cri[web$tmp$inc_indx[web$restore_lgk]] <- web$tmp$bkp_pid_cri[web$restore_lgk]
        web$repo$iteration[web$tmp$inc_indx[web$restore_lgk]] <- web$tmp$bkp_iteration[web$restore_lgk]
        web$restore_indx.mm <- index_multiples(
          x = web$restore_lgk,
          multiples = length(web$tmp$bkp_pid),
          repeats = length(web$tmp$inc_indx.mm)/length(web$tmp$inc_indx))
        web$repo$wind_id[web$tmp$inc_indx.mm[web$restore_indx.mm]] <- web$tmp$bkp_wind_id[web$restore_indx.mm]
      }
      web$tmp$bkp_pid <- web$tmp$bkp_link_id <-
        web$tmp$bkp_tag <- web$tmp$bkp_pid_cri <-
        web$tmp$bkp_iteration <- web$tmp$bkp_wind_id <-  NULL
    }
    # Unlink pids with a single record for another attempt in the next stage
    web$tmp$lgk <- (!duplicated(web$repo$pid[web$tmp$inc_indx]) &
                      !duplicated(web$repo$pid[web$tmp$inc_indx], fromLast = TRUE))
    web$repo$linked[web$tmp$inc_indx][
      !web$repo$pid[web$tmp$inc_indx] %in% web$repo$pid[web$tmp$inc_indx][web$tmp$lgk]
    ] <- TRUE
    web$repo$linked[web$tmp$inc_indx][!web$tmp$lgk] <- TRUE
    #
    web$repo$pid[web$tmp$inc_indx[web$tmp$lgk]] <-
      web$repo$pr_sn[web$tmp$inc_indx[web$tmp$lgk]]
    web$repo$pid_cri[web$tmp$inc_indx[web$tmp$lgk]] <- web$mxp_cri
    web$repo$iteration[web$tmp$inc_indx[web$tmp$lgk]] <- 0L
    # Flag records linked at current stage
    #
    web$current_tot <- length(web$tmp$inc_indx)
    web$assigned <- length(which(!web$tmp$lgk))
    if(grepl("^progress|^stats", web$options$display)){
      web$msg <- update_text(
        tot_records = fmt(web$n.row),
        current_tot = fmt(web$current_tot),
        current_tagged = fmt(web$assigned),
        indent_txt = "  "
      )
      cat(
        "\n",
        web$msg,
        "\n")
    }
    web$i <- web$i + 1L
  }
  #
  web$repo$iteration[web$repo$iteration == 0] <- web$ite - 1L
  web$tmp$lgk <- (!duplicated(web$repo$pid) & !duplicated(web$repo$pid, fromLast = TRUE))
  # Skipped records
  if(!inherits(web$repo$strata, "NULL")){
    web$repo$pid_cri[
      web$tmp$lgk & is.na(web$repo$strata)
    ] <- -1L
  }
  # Unmatched records
  web$repo$pid_cri[
    web$tmp$lgk & web$repo$pid_cri != -1
  ] <- 0L
  #
  web$repo$pid[web$tmp$lgk] <-
    web$repo$pr_sn[web$tmp$lgk]
  web$repo$wind_id[
    index_multiples(
      x = which(web$tmp$lgk),
      multiples = web$n.row,
      repeats = web$counts$max_indexes)
  ] <- NA_real_
  #
  if(!is.null(web$repo$sn)){
    web$repo$pid <- web$repo$sn[web$repo$pid]
    web$repo$wind_id <- web$repo$sn[web$repo$wind_id]
    web$repo$pr_sn <- web$repo$sn[web$repo$pr_sn]
  }
  #
  web$pids <- methods::new("pid",
                           .Data = web$repo$pid,
                           sn = web$repo$pr_sn,
                           pid_cri = web$repo$pid_cri,
                           link_id = 0L,
                           iteration = web$repo$iteration)
  #
  web$r <- rle(sort(web$pids@.Data))
  web$pids@pid_total <- web$r$lengths[match(web$pids@.Data, web$r$values)]
  #
  if(!is.null(web$repo$data_source)){
    # Data links
    web$rst <- check_links(
      web$pids@.Data,
      web$repo$data_source,
      web$data_links)
    if(!all(toupper(unlist(web$data_links, use.names = FALSE)) == "ANY")){
      web$req_links <- web$rst$rq
      web$pids@pid_total[!web$req_links] <- 1L
      web$pids@pid_cri[!web$req_links] <- -1L
      web$pids@.Data[!web$req_links] <- web$pids@web$repo$sn[!web$req_links]
      web$pids@link_id[!web$req_links] <- web$pids@web$repo$sn[!web$req_links]
      web$rst$ds[!web$req_links] <- web$repo$data_source[!web$req_links]
    }
    web$pids@pid_dataset <- encode(web$rst$ds)
  }
  #
  web$tm_z <- Sys.time()
  web$tms <- fmt(difftime(web$tm_z, web$tm_a), "difftime")
  #
  if(grepl("report$", web$options$display)){
    web$rp_data <- di_report(
      cumm_time = web$tm_z - web$tm_a,
      duration = web$tm_z - web$tm_ia,
      "End",
      current_tot = web$n.row,
      memory_used =  utils::object.size(web[names(web)[names(web) != "report"]]))
    web$report <- c(web$report, list(web$rp_data))
  }
  #
  if(grepl("report$", web$options$display)){
    web$pids <- list(pid = web$pids,
                     report = as.list(do.call("rbind", lapply(web$report, as.data.frame))))
    class(web$pids$report) <- "d_report"
  }
  if(!grepl("^none", web$options$display)){
    cat("Records linked in ", web$tms, "!\n", sep = "")
  }
  if(length(web$export) > 0){
    if(inherits(web$pids, "list")){
      web$pids <- c(web$pids, web["export"])
    }else{
      web$pids <- list(pid = web$pids, export = web$export)
    }
  }
  web <- web$pids
  return(web)
}

