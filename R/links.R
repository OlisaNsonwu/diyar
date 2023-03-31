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
#' @param batched \code{[character]} Determines if record-pairs are created and compared in batches. Options are \code{"yes"}, \code{"no"} or \code{"semi"}.
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
#' \item If named \code{"l"}, groups without records from every listed \code{data_source} will be unlinked.
#' \item If named \code{"g"}, groups without records from any listed \code{data_source} will be unlinked.
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
                  recursive = "none",
                  check_duplicates = FALSE,
                  tie_sort = NULL,
                  batched = "yes",
                  repeats_allowed = FALSE,
                  permutations_allowed = FALSE,
                  ignore_same_source = FALSE){
  tm_a <- Sys.time()

  # recursive <- recursive != "none"
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
  web$options$batched <- lapply(web$options$batched, tolower)
  web$options$display <- tolower(web$options$display)
  #
  if(inherits(web$options$recursive, "logical")){
    web$options$is_recursive <- web$options$recursive
    if(isTRUE(web$options$recursive)){
      web$options$recursive <- c("linked.false", "linked.true", "unlinked")
    }else{
      web$options$recursive <- "none"
    }
  }else{
    web$options$is_recursive <- any(c("linked.false", "linked.true", "unlinked") %in% web$options$recursive) & !"none" %in% web$options$recursive
  }
  #
  if(!inherits(web$match.cri$criteria, "list")){
    web$match.cri$criteria <- list(web$match.cri$criteria)
  }
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
  web$repo$cur_refs <- web$repo$max_refs <- rep(0L, web$n.row)
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
    web$report[length(web$report) + 1] <- list(web$rp_data)
  }
  web$tm_ia <- Sys.time()
  if(!grepl("^none", web$options$display)){
    cat("\n")
  }
  web$i <- web$ite <- web$itx <- web$counts$max_indexes <- 1L
  # print(paste0("Before Loop: ", difftime(Sys.time(), tm_a)))
  while(web$i %in% seq_len(length(web$match.cri$criteria))){
    #
    web$i_nm <- ifelse(!is.null(names(web$match.cri$criteria[web$i])),
                       paste0(web$i, ": ", names(web$match.cri$criteria[web$i])),
                       web$i)
    if(grepl("^progress|^stats", web$options$display)){
      cat(paste0("`Criteria ", web$i_nm,"`.\n"), sep = "")
    }
    #
    web$tmp$sub.cri <- web$match.cri$sub_criteria[
      which(names(web$match.cri$sub_criteria) == paste0("cr", web$i))
      ]
    web$options$is_nested <- length(web$tmp$sub.cri) > 0
    web$options$is_recursive <- ifelse(
      web$options$batched[web$i] %in% "no" |
        isFALSE(web$options$is_nested),
      FALSE, web$options$is_recursive
    )
    web$options$check_duplicates <- ifelse(
      web$options$batched[web$i] %in% "no" |
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
    if(isTRUE(web$options$shrink) & web$ite != 1){
      web$repo$cri_level[["record_group"]] <- web$repo$linked
      if(FALSE){
        web$tmp$lgk <- web$repo$cri_level$record_group
      }else{
        web$tmp$lgk <- TRUE
      }
      web$repo$cri_level$record_group[web$tmp$lgk] <-
        web$repo$pid[web$tmp$lgk]
    }
    web$repo$cri <- rep(NA, web$n.row)
    web$repo$cri[which(web$tmp$inc_lgk)] <- combi(
      lapply(web$repo$cri_level, function(x) x[web$tmp$inc_lgk])
    )
    web$tmp$inc_lgk[
      (!duplicated(web$repo$cri[web$tmp$inc_lgk], fromLast = TRUE) &
         !duplicated(web$repo$cri[web$tmp$inc_lgk], fromLast = FALSE))
    ] <- FALSE
    web$tmp$cri_inc_indx <- which(web$tmp$inc_lgk)
    #
    if(isFALSE(web$options$shrink)){
      if(isFALSE(web$options$expand)){
        web$tmp$cri_inc_indx <- web$tmp$cri_inc_indx[web$repo$pid_cri[web$tmp$cri_inc_indx] >= web$i]
      }else{
        if(length(web$repo$linked) == length(which(web$repo$linked[web$tmp$cri_inc_indx]))){
          web$tmp$cri_inc_indx <- numeric()
        }
      }
    }else{
      if(web$i > 1){
        web$tmp$cri_inc_indx <- web$tmp$cri_inc_indx[web$repo$pid_cri[web$tmp$cri_inc_indx] == web$i - 1]
      }
    }

    #
    if(isTRUE(web$options$is_nested)){
      mVal.indx <- function(x){
        exc_indx <- which(is.na(x))
        if(length(x) == 1){
          exc_indx <- Inf
        }
        return(exc_indx)
      }
      mVal.indx <- attr_eval(web$tmp$sub.cri[[1]], simplify = TRUE, func = mVal.indx)
      if(Inf %in% mVal.indx){
        web$tmp$cri_inc_indx <- numeric()
      }else{
        web$tmp$cri_inc_indx <- web$tmp$cri_inc_indx[!web$tmp$cri_inc_indx %in% mVal.indx]
      }
    }

    if(length(web$tmp$cri_inc_indx) %in% 0:1 | (length(web$repo$cri_l) == 1 & isFALSE(web$options$is_nested))) {
      if(grepl("^progress|^stats", web$options$display)){
        cat(paste0("  -> Skipped.\n\n"))
      }
      web$i <- web$i + 1L
      web$ite <- web$ite + 1L
      next
    }

    if(isTRUE(web$options$shrink)){
      # Back up identifiers
      web$repo$pid[web$tmp$cri_inc_indx] -> web$tmp$bkp_pid
      web$repo$tag[web$tmp$cri_inc_indx] -> web$tmp$bkp_tag
      web$repo$linked[web$tmp$cri_inc_indx] -> web$tmp$bkp_linked
      web$repo$pid_cri[web$tmp$cri_inc_indx] -> web$tmp$bkp_pid_cri
      web$repo$iteration[web$tmp$cri_inc_indx] -> web$tmp$bkp_iteration
      web$tmp$cri_inc_indx.mm <- index_multiples(
        x = web$tmp$cri_inc_indx,
        multiples = web$n.row,
        repeats = web$counts$max_indexes)$mm
      web$repo$wind_id[web$tmp$cri_inc_indx.mm] -> web$tmp$bkp_wind_id
      # Reset identifiers
      web$repo$pid[web$tmp$cri_inc_indx] <-
        web$repo$pr_sn[web$tmp$cri_inc_indx]
      web$repo$tag[web$tmp$cri_inc_indx] <-
        web$repo$iteration[web$tmp$cri_inc_indx] <- 0L
      web$repo$pid_cri[web$tmp$cri_inc_indx] <- web$mxp_cri
      web$repo$wind_id[
        index_multiples(
          x = web$tmp$cri_inc_indx,
          multiples = web$n.row,
          repeats = web$counts$max_indexes)$mm
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
    web$tmp$ite_inc_indx <- web$tmp$cri_inc_indx
    while(suppressWarnings(min(web$repo$tag[web$tmp$ite_inc_indx])) != 2 & length(web$tmp$ite_inc_indx) > 0) {
      #
      web$tmp$ite_inc_indx <- web$tmp$ite_inc_indx[
        web$repo$tag[web$tmp$ite_inc_indx] != 2 |
          (web$repo$tag[web$tmp$ite_inc_indx] == 2 & web$repo$iteration.linked[web$tmp$ite_inc_indx] & "linked.true" %in% web$options$recursive) |
          (web$repo$tag[web$tmp$ite_inc_indx] == 2 & !web$repo$iteration.linked[web$tmp$ite_inc_indx] & "linked.false" %in% web$options$recursive)
      ]
      web$sort_ord <- order(
        web$repo$cri[web$tmp$ite_inc_indx],
        web$repo$tag[web$tmp$ite_inc_indx],
        web$repo$pid_cri[web$tmp$ite_inc_indx],
        web$repo$tie_sort[web$tmp$ite_inc_indx],
        web$repo$pr_sn[web$tmp$ite_inc_indx])

      web$tmp$ite_inc_indx <- web$tmp$ite_inc_indx[web$sort_ord]
      if(length(web$tmp$ite_inc_indx) <= 1){
        web$ite <- web$ite + 1L
        break
      }
      web$tmp$index_cd <- !duplicated(web$repo$cri[web$tmp$ite_inc_indx], fromLast = FALSE)
      if(web$options$batched[web$i] == "semi" # & isTRUE(web$options$is_nested)
         ){
        web$tmp$index_cd[web$repo$tag[web$tmp$ite_inc_indx] == -1] <- TRUE
      }else if(web$options$batched[web$i] == "no" # & isTRUE(web$options$is_nested)
               ){
        web$tmp$index_cd[TRUE] <- TRUE
      }
      #
      # web$tmp$batch_strata <- list(web$repo$cri[web$tmp$ite_inc_indx])
      # if(isFALSE(web$options$expand)){
      #   web$tmp$batch_strata[["pid_cri"]] <- web$repo$pid_cri[web$tmp$ite_inc_indx]
      # }
      # if(length(web$tmp$batch_strata) > 1){
      #   web$tmp$batch_strata <- combi(web$tmp$batch_strata)
      # }else{
      #   web$tmp$batch_strata <- web$tmp$batch_strata[[1]]
      # }

      web$tmp$batch_strata <- web$repo$cri[web$tmp$ite_inc_indx]
      if(web$options$batched[[web$i]] != "no"){
        web$tmp$index_cd[!duplicated(web$tmp$batch_strata) & !web$tmp$index_cd] <- TRUE
      }
      # Create record-pairs
      web$rec.pairs <- make_pairs_batched(
        strata = web$tmp$batch_strata,
        x = web$tmp$ite_inc_indx,
        index_record = web$tmp$index_cd,
        assign_ord = seq_len(length(web$tmp$ite_inc_indx)),
        look_back = web$options$permutations_allowed,
        include_repeat = web$options$repeats_allowed,
        ignore_same_source = web$options$ignore_same_source,
        data_source = web$data_source)

      if(isFALSE(web$options$repeats_allowed) & length(web$rec.pairs$x_pos) == 0){
        # Possible when `repeats_allowed` FALSE
        #     and `permutations_allowed = TRUE`
        #     and current iteration has only 1 record
        # Recreate `web$rec.pairs` as if `repeats_allowed` = TRUE so that `web$tmp$ite_inc_indx` is tagged
        web$rec.pairs <- list(
          x_pos = seq_len(length(web$tmp$ite_inc_indx)),
          y_pos = seq_len(length(web$tmp$ite_inc_indx)),
          index_ord = rep(1L, length(web$tmp$ite_inc_indx)),
          x_val = web$tmp$ite_inc_indx,
          y_val = web$tmp$ite_inc_indx)
      }

      names(web$rec.pairs)[which(names(web$rec.pairs) == "x_val")] <- "cu_pos"
      names(web$rec.pairs)[which(names(web$rec.pairs) == "y_val")] <- "tr_pos"
      if(FALSE){
        web$rec.pairs$cu_pid <- web$repo$pid[web$rec.pairs$cu_pos]
        web$rec.pairs$cu_linked <- web$repo$iteration.linked[web$rec.pairs$cu_pos]
        web$rec.pairs$cu_linked <- rep(FALSE, length(web$rec.pairs$cu_linked))
      }
      #
      if(isTRUE(web$options$is_nested)){
        web$rec.pairs[["rec.match"]] <- eval_sub_criteria(
          x = web$tmp$sub.cri[[1]],
          x_pos = web$rec.pairs$cu_pos,
          y_pos = web$rec.pairs$tr_pos,
          check_duplicates = web$options$check_duplicates)

        web$export.nm <- names(web$rec.pairs$rec.match)
        web$export.nm <- web$export.nm[!grepl("^logical|^equal", web$export.nm)]
        if(length(web$export.nm) > 0){
          web$export[[paste0("cri.", web$i)]][[paste0("iteration.", web$ite)]] <- web$rec.pairs$rec.match[web$export.nm]
          web$rec.pairs$rec.match[web$export.nm] <- NULL
        }
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
      # Flag the reference record
      web$rec.pairs$ref_rd <- web$rec.pairs$cu_pos == web$rec.pairs$tr_pos
      # Update window ids for matched or reference records
      web$rec.pairs$w.match <-
        ((web$rec.pairs$rec.match$logical_test) | web$rec.pairs$ref_rd)# &
        # (is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi]) |
        #    (!is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi]) & web$rec.pairs$cu_linked)
        # )
      # & is.na(web$repo$wind_id[web$rec.pairs$cu_pos.mi])

      if(TRUE){
        web$tmp$s_ord <- order(web$rec.pairs$cu_pos[web$rec.pairs$w.match])
        web$tmp$nw_index_ord <- rle(web$rec.pairs$cu_pos[web$rec.pairs$w.match][web$tmp$s_ord])
        web$tmp$nw_index_ord <- sequence(web$tmp$nw_index_ord$lengths)
        web$tmp$nw_index_ord <- web$tmp$nw_index_ord[order(web$tmp$s_ord)]

        web$tmp$lgk <- !duplicated(web$rec.pairs$cu_pos[web$rec.pairs$w.match], fromLast = TRUE)
        web$repo$max_refs[web$rec.pairs$cu_pos[web$rec.pairs$w.match][web$tmp$lgk]] <- web$repo$cur_refs[web$rec.pairs$cu_pos[web$rec.pairs$w.match][web$tmp$lgk]] + web$tmp$nw_index_ord[web$tmp$lgk]

        # Maximum number of index records per episode (`max_indexes`).
        web$tmp$max_indexes <- suppressWarnings(max(web$repo$max_refs))
        # Increase the number of `wind_id` by multiples of `max_indexes`
        if(web$tmp$max_indexes > web$counts$max_indexes){
          web$repo$wind_id <- c(
            web$repo$wind_id,
            rep(rep(NA_real_, web$n.row), (web$tmp$max_indexes - web$counts$max_indexes))
          )
          web$counts$max_indexes <- web$tmp$max_indexes
        }

        web$rec.pairs$cu_pos.mi <- ((web$tmp$nw_index_ord + web$repo$cur_refs[web$rec.pairs$cu_pos[web$rec.pairs$w.match]] - 1L) * web$n.row) + web$rec.pairs$cu_pos[web$rec.pairs$w.match]
        web$repo$cur_refs <- web$repo$max_refs
      }
      web$repo$wind_id[web$rec.pairs$cu_pos.mi] <- web$rec.pairs$tr_pos[web$rec.pairs$w.match]

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
        web$tmp$batched_pids <- make_ids(web$rec.pairs$x_pos[web$rec.pairs$rec.match$logical_test],
                                         web$rec.pairs$y_pos[web$rec.pairs$rec.match$logical_test],
                                         id_length = max(web$rec.pairs$x_pos))
        web$repo$pid[
          web$tmp$ite_inc_indx[web$tmp$batched_pids$sn[web$tmp$batched_pids$linked]]
        ] <- web$tmp$ite_inc_indx[web$tmp$batched_pids$group_id[web$tmp$batched_pids$linked]]
      }else{
        web$repo$pid[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] <- web$repo$pid[web$rec.pairs$tr_pos[web$rec.pairs$e.match]]
        if(isTRUE(web$options$is_recursive)){
          web$tmp$tr_refs <- list(cu_pos = web$rec.pairs$cu_pos[web$rec.pairs$index_ord == 1], tr_pos = web$rec.pairs$tr_pos[web$rec.pairs$index_ord == 1])
          web$tmp$tr_refs$inherit_lgk <- web$repo$tag[web$tmp$tr_refs$cu_pos] == 2 & web$repo$pid[web$tmp$tr_refs$cu_pos] != web$repo$bkp_pid[web$tmp$tr_refs$cu_pos]
          web$tmp$tr_refs$inherit_lgk <- web$repo$bkp_pid[web$tmp$tr_refs$cu_pos] %in% web$repo$bkp_pid[web$tmp$tr_refs$cu_pos[web$tmp$tr_refs$inherit_lgk]]
          web$repo$pid[web$tmp$tr_refs$cu_pos[web$tmp$tr_refs$inherit_lgk]] <- web$repo$pid[web$tmp$tr_refs$tr_pos[web$tmp$tr_refs$inherit_lgk]]
        }
      }

      if(isTRUE(web$options$is_recursive)){
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
      web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$index_rd]] <- 2L
      if(isFALSE(web$options$repeats_allowed)){
        web$repo$tag[web$rec.pairs$tr_pos] <- 2L
      }
      if(isFALSE(web$options$check_duplicates)){
        web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$rec.match$equal_test]] <- 2L
      }
      web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$e.match]][
        web$repo$tag[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] != 2
      ] <- ifelse(isTRUE(web$options$is_recursive), -1L, 2L)

      if(isTRUE(web$options$permutations_allowed)){
        web$tmp$tgt_indx <- web$rec.pairs$cu_pos[web$rec.pairs$rec.match$logical_test]
      }else{
        web$tmp$tgt_indx <- c(web$rec.pairs$cu_pos[web$rec.pairs$rec.match$logical_test],
                              web$rec.pairs$tr_pos[web$rec.pairs$rec.match$logical_test])
      }
      web$repo$iteration[web$tmp$tgt_indx][
        web$repo$tag[web$tmp$tgt_indx] == 2 &
          web$repo$iteration[web$tmp$tgt_indx] == 0
      ] <- web$ite

      web$repo$pid_cri[web$tmp$tgt_indx][
        web$repo$tag[web$tmp$tgt_indx] == 2 &
          web$repo$pid_cri[web$tmp$tgt_indx] == web$mxp_cri
      ] <- web$i

      web$repo$iteration.linked[web$tmp$tgt_indx] <- TRUE
      #
      web$repo$bkp_pid <- web$repo$pid
      #
      web$tmp$ite.row.n <- length(web$tmp$ite_inc_indx)
      web$tmp$cri.row.n <- length(web$tmp$cri_inc_indx)
      web$tmp$ite.linked.n <- length(which(web$repo$tag[web$tmp$ite_inc_indx] == 2))
      web$tmp$cri.linked.n <- length(which(web$repo$tag[web$tmp$cri_inc_indx] == 2))
      #
      if(isTRUE(web$options$is_nested)){
        if(grepl("^progress", web$options$display)){
          web$msg <- progress_bar(
            n = web$tmp$cri.linked.n,
            d = web$tmp$cri.row.n,
            max_width = 100,
            msg = paste0("Iteration ",
                         fmt(web$ite), " (",
                         fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                         ")"),
            prefix_msg = "  ")
          cat(web$msg, "\r", sep = "")
        }else if (grepl("^stats", web$options$display)){
          web$msg <- update_text(
            tot_records = fmt(web$tmp$cri.row.n),
            current_tot = fmt(web$tmp$ite.row.n),
            current_tagged = fmt(web$tmp$ite.row.n),
            time = fmt(Sys.time() - web$tm_ia, "difftime"),
            iteration = web$ite,
            indent_txt = "  "
          )
          cat(web$msg, "\n", sep = "")
        }
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
        web$report[length(web$report) + 1] <- list(web$rp_data)
      }
      web$tm_ia <- Sys.time()
      web$ite <- web$ite + 1L
      web$itx <- web$itx + 1L
    }
    ta <- Sys.time()
    if(web$options$shrink){
      if(FALSE){
        # If sub-record-group has lost it's index
        web$reset_lgk <- which(
          # Records not in the current set checked ...
          !web$repo$pr_sn %in% web$tmp$cri_inc_indx &
            # Previously linked to an index now associated with another record-group ...
            # web$repo$pid %in% web$repo$pr_sn[web$repo$pr_sn %in% web$repo$pid[!web$repo$pr_sn %in% web$tmp$cri_inc_indx]]
            web$repo$pid %in% web$tmp$cri_inc_indx[web$repo$iteration.linked[web$tmp$cri_inc_indx]]
        )
      }else{
        # alt approach
        # Records which are not part of the current iteration
        web$tmp$cri_exc_indx <- which(!seq_len(web$n.row) %in% web$tmp$cri_inc_indx)
        # ... but belong to a group created by a record that has now changed groups
        web$tmp$tgt_indx <- web$tmp$cri_exc_indx[web$repo$pid[web$tmp$cri_exc_indx] %in% web$tmp$cri_inc_indx]
        if(length(web$tmp$tgt_indx) > 0){
          # ... this includes the same records from the POV of other references (index_cd)
          web$tmp$tgt_indx.mm <- index_multiples(
            x = web$tmp$tgt_indx,
            multiples = web$n.row,
            repeats = web$counts$max_indexes)$mm
          # ... flag those with links to records that did not change groups in the current since the last iteration
          web$l1 <- rep(web$tmp$tgt_indx, web$n.row)
          web$l2 <- web$repo$wind_id[web$tmp$tgt_indx.mm]
          web$indx <- which(web$l2 %in% web$tmp$tgt_indx)
          web$indx <- c(web$l2[web$indx], web$l1[web$indx])
          # ... records not flagged have no links to allow it remain part of the group and so need will be reset
          web$reset_lgk <- web$tmp$tgt_indx[!web$tmp$tgt_indx %in% web$indx]
        }else{
          web$reset_lgk <- numeric()
        }
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
              repeats = web$counts$max_indexes)$mm
          ] <- NA_real_
        }
        if(length(web$tmp$tgt_indx) > 0){
          # ... flagged records can still remain in the same group but need a new ID to differentiate them from any other group.
          web$newId_indx <- web$tmp$tgt_indx[!web$tmp$tgt_indx %in% web$reset_lgk]
          web$repo$pid[web$newId_indx] <- web$n.row + web$repo$pid[web$newId_indx]
        }
        # ... links to records that have changed grouped are erased.
        web$tmp$cri_exc_indx.mm <- index_multiples(
          x = web$tmp$cri_exc_indx,
          multiples = web$n.row,
          repeats = web$counts$max_indexes)$mm
        web$repo$wind_id[web$tmp$cri_exc_indx.mm][
          web$repo$wind_id[web$tmp$cri_exc_indx.mm] %in% web$tmp$cri_inc_indx
        ] <- NA
      }
      web$restore_lgk <- (!duplicated(web$repo$pid[web$tmp$cri_inc_indx]) &
                            !duplicated(web$repo$pid[web$tmp$cri_inc_indx], fromLast = TRUE))
      web$restore_lgk <- which(!web$repo$cri %in% web$repo$cri[!web$restore_lgk])
      if(length(web$restore_lgk) > 0){
        web$repo$pid[web$tmp$cri_inc_indx[web$restore_lgk]] <- web$tmp$bkp_pid[web$restore_lgk]
        web$repo$tag[web$tmp$cri_inc_indx[web$restore_lgk]] <- web$tmp$bkp_tag[web$restore_lgk]
        web$repo$linked[web$tmp$cri_inc_indx[web$restore_lgk]] <- web$tmp$bkp_linked[web$restore_lgk]
        web$repo$pid_cri[web$tmp$cri_inc_indx[web$restore_lgk]] <- web$tmp$bkp_pid_cri[web$restore_lgk]
        web$repo$iteration[web$tmp$cri_inc_indx[web$restore_lgk]] <- web$tmp$bkp_iteration[web$restore_lgk]
        web$restore_indx.mm <- index_multiples(
          x = web$restore_lgk,
          multiples = length(web$tmp$bkp_pid),
          repeats = length(web$tmp$cri_inc_indx.mm)/length(web$tmp$cri_inc_indx))$mm
        web$repo$wind_id[web$tmp$cri_inc_indx.mm[web$restore_indx.mm]] <- web$tmp$bkp_wind_id[web$restore_indx.mm]
      }
      web$tmp$bkp_pid <- web$tmp$bkp_link_id <-
        web$tmp$bkp_tag <- web$tmp$bkp_pid_cri <-
        web$tmp$bkp_iteration <- web$tmp$bkp_wind_id <-  NULL
    }
    # Unlink pids with a single record for another attempt in the next stage
    web$tmp$lgk <- (!duplicated(web$repo$pid[web$tmp$cri_inc_indx]) &
                      !duplicated(web$repo$pid[web$tmp$cri_inc_indx], fromLast = TRUE))
    web$repo$linked[web$tmp$cri_inc_indx][
      !web$repo$pid[web$tmp$cri_inc_indx] %in% web$repo$pid[web$tmp$cri_inc_indx][web$tmp$lgk]
    ] <- TRUE
    web$repo$linked[web$tmp$cri_inc_indx][!web$tmp$lgk] <- TRUE
    #
    web$repo$pid[web$tmp$cri_inc_indx[web$tmp$lgk]] <-
      web$repo$pr_sn[web$tmp$cri_inc_indx[web$tmp$lgk]]
    web$repo$pid_cri[web$tmp$cri_inc_indx[web$tmp$lgk]] <- web$mxp_cri
    web$repo$iteration[web$tmp$cri_inc_indx[web$tmp$lgk]] <- 0L
    # Flag records linked at current stage
    #
    web$current_tot <- length(web$tmp$cri_inc_indx)
    web$assigned <- length(which(!web$tmp$lgk))
    if(grepl("^progress", web$options$display)){
      web$msg <- update_text(
        tot_records = fmt(web$n.row),
        current_tot = fmt(web$current_tot),
        current_tagged = fmt(web$assigned),
        indent_txt = "  "
      )
      cat(
        ifelse(isTRUE(web$options$is_nested), "\n", ""),
        web$msg,
        "\n", sep = "")
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
      repeats = web$counts$max_indexes)$mm
  ] <- NA_real_
  #
  if(!is.null(web$repo$sn)){
    web$repo$pid <- web$repo$sn[web$repo$pid]
    web$repo$wind_id <- web$repo$sn[web$repo$wind_id]
    web$repo$pr_sn <- web$repo$sn[web$repo$pr_sn]
  }
  #
  web$repo$wind_id <- matrix(web$repo$wind_id, ncol = web$counts$max_indexes)
  web$repo$wind_id <- as.list(as.data.frame(web$repo$wind_id))
  if(TRUE & length(web$repo$wind_id) > 1){
    web$tmp$lgk <- lapply(web$repo$wind_id, function(x) all(is.na(x)))
    web$tmp$lgk <- as.logical(web$tmp$lgk)
    web$repo$wind_id <- web$repo$wind_id[!web$tmp$lgk]
  }
  names(web$repo$wind_id)<- paste0("link_id", seq_len(length(web$repo$wind_id)))


  web$pids <- methods::new("pid",
                           .Data = web$repo$pid,
                           sn = web$repo$pr_sn,
                           pid_cri = web$repo$pid_cri,
                           link_id = web$repo$wind_id,
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
    web$report[length(web$report) + 1] <- list(web$rp_data)
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

