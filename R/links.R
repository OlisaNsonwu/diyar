#' @name links
#' @title Multistage record linkage
#'
#' @description Assign records to unique groups based on an ordered set of match criteria.
#'
#' @param sn \code{[integer]}. Unique record ID.
#' @param strata \code{[atomic]}. Subsets of the dataset. Record-groups are created separately for each \code{strata}. See \code{Details}.
#' @param criteria \code{[list|atomic]}. Ordered list of attributes to be compared. Each element of the list is a stage in the linkage process. See \code{Details}.
#' @param sub_criteria \code{[list|\link{sub_criteria}]}. Nested match criteria. This must be paired to a stage of the linkage process (\code{criteria}). See \code{\link{sub_criteria}}
#' @param data_source \code{[character]}. Source ID for each record. If provided, a list of all sources in each record-group is returned. See \code{\link[=pid-class]{pid_dataset slot}}.
#' @param group_stats \code{[logical]}. If \code{TRUE} (default), return group specific information like record counts for each \code{\link[=pid-class]{pid}}.
#' @param data_links \code{[list|character]}. \code{data_source} required in each \code{\link[=pid-class]{pid}}. A record-group without records from these \code{data_sources} will be \code{\link[=delink]{unlinked}}. See \code{Details}.
#' @param expand \code{[logical]}. If \code{TRUE}, a record-group gains new records if a match is found at the next stage of the linkage process. \emph{Not interchangeable with \code{shrink}}.
#' @param shrink \code{[logical]}. If \code{TRUE}, a record-group loses existing records if no match is found at the next stage of the linkage process. \emph{Not interchangeable with \code{expand}}.
#' @param recursive \code{[logical]}. If \code{TRUE}, within each iteration of the process, a match can spawn new matches. Ignored when \code{batched} is \code{"no"}.
#' @param check_duplicates \code{[logical]}. If \code{TRUE}, within each iteration of the process, duplicates values of an attributes are not checked. The outcome of the logical test on the first instance of the value will be recycled for the duplicate values. Ignored when \code{batched} is \code{"no"}.
#' @param display \code{[character]}. Display progress update and/or generate a linkage report for the analysis. Options are; \code{"none"} (default), \code{"progress"}, \code{"stats"}, \code{"none_with_report"}, \code{"progress_with_report"} or \code{"stats_with_report"}.
#' @param tie_sort \code{[atomic]}. Preferential order for breaking match ties within an iteration of record linkage.
#' @param repeats_allowed \code{[logical]} If \code{TRUE}, pairs made up of repeat records are not created and compared. Only used when \code{batched} is \code{"no"}.
#' @param permutations_allowed \code{[logical]} If \code{TRUE}, permutations of record-pairs are created and compared. Only used when \code{batched} is \code{"no"}.
#' @param ignore_same_source \code{[logical]} If \code{TRUE}, only records-pairs from a different \code{data_source} are created and compared.
#' @param batched \code{[character]} Determines if record-pairs are created and compared in batches. Options are \code{"yes"}, \code{"no"} or \code{"semi"}.
#' @return \code{\link[=pid-class]{pid}}; \code{list}
#'
#' @seealso \code{\link{links_af_probabilistic}}; \code{\link{episodes}};
#' \code{\link{predefined_tests}}; \code{\link{sub_criteria}}
#'
#' @details
#' The priority of matches decreases with each subsequent stage of the linkage process.
#' Therefore, the attributes in \code{criteria} should be in an order of decreasing relevance.
#'
#' Records with missing data (\code{NA}) for each \code{criteria} are
#' skipped at the respective stage, while records with
#' missing data \code{strata} are skipped from every stage.
#'
#' If a record is skipped from a stage, another attempt will be made to
#' match the record at the next stage. If a record is still unmatched
#' by the last stage, it is assigned a unique group ID.
#'
#' A \code{\link{sub_criteria}} adds nested match criteria
#' to each stage of the linkage process. If used, only
#' records with a matching \code{criteria} and \code{sub_criteria} are linked.
#'
#' In \bold{\code{\link{links}}}, each \code{\link{sub_criteria}} must
#' be linked to a \code{criteria}. This is done by adding each \code{\link{sub_criteria}}
#' to a named element of a list - "cr" concatenated with
#' the corresponding stage's number.
#' For example, 3 \code{sub_criteria} linked to
#' \code{criteria} 1, 5 and 13 will be;
#'
#' \deqn{list(cr1 = sub_criteria(...), cr5 = sub_criteria(...), cr13 = sub_criteria(...))}
#'
#' Any unlinked \code{\link{sub_criteria}} will be ignored.
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
#' dfr <- patient_records
#' # An exact match on surname followed by an exact match on forename
#' stages <- as.list(dfr[c("surname", "forename")])
#' p1 <- links(criteria = stages)
#'
#' # An exact match on forename followed by an exact match on surname
#' p2 <- links(criteria = rev(stages))
#'
#' # Nested matches
#' # Same sex OR birth year
#' m.cri.1 <- sub_criteria(
#'   format(dfr$dateofbirth, "%Y"), dfr$sex,
#'   operator = "or")
#'
#' # Same middle name AND a 10 year age difference
#' age_diff <- function(x, y){
#'   diff <- abs(as.numeric(x) - as.numeric(y))
#'   wgt <-  diff %in% 0:10 & !is.na(diff)
#'   wgt
#' }
#' m.cri.2 <- sub_criteria(
#'   format(dfr$dateofbirth, "%Y"), dfr$middlename,
#'   operator = "and",
#'   match_funcs = c(age_diff, exact_match))
#'
#' # Nested match criteria 'm.cri.1' OR 'm.cri.2'
#' n.cri <- sub_criteria(
#'   m.cri.1, m.cri.2,
#'   operator = "or")
#'
#' # Record linkage with additional match criteria
#' p3 <- links(
#'   criteria = stages,
#'   sub_criteria = list(cr1 = m.cri.1,
#'                       cr2 = m.cri.2))
#'
#' # Record linkage with additonal nested match criteria
#' p4 <- links(
#'   criteria = stages,
#'   sub_criteria = list(cr1 = n.cri,
#'                       cr2 = n.cri))
#'
#' dfr$p1 <- p1; dfr$p2 <- p2
#' dfr$p3 <- p3; dfr$p4 <- p4
#'
#' head(dfr)
#'
#' @aliases links
#' @export
links <- function(
    criteria,
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
    ignore_same_source = FALSE,
    stepwise_method = 'expand_with_priority'){
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
  #
  rm(criteria, sub_criteria, sn,
     strata, data_source, data_links,
     display, group_stats, expand,
     shrink, recursive, check_duplicates,
     tie_sort, batched, repeats_allowed,
     permutations_allowed, ignore_same_source)
  #

  if(inherits(web$match.cri$sub_criteria, 'sub_criteria')){
    web$match.cri$sub_criteria <-
      rep(list(web$match.cri$sub_criteria), length(web$match.cri$sub_criteria))
    names(web$match.cri$sub_criteria) <-
      paste0('cr', seq_len(length(web$match.cri$sub_criteria)))
  }
  web$err <- err_links_checks_0(
    web$match.cri$criteria, web$match.cri$sub_criteria,
    web$repo$sn, web$repo$strata, web$repo$data_source, web$match.cri$data_links,
    web$options$display, web$options$group_stats, web$options$expand,
    web$options$shrink, web$options$recursive, web$options$check_duplicates,
    web$repo$tie_sort,web$options$repeats_allowed,
    web$options$permutations_allowed, web$options$ignore_same_source,
    web$options$batched)
  if(!isFALSE(web$err)){
    stop(web$err, call. = FALSE)
  }
  #
  web$options$batched <- lapply(web$options$batched, tolower)
  web$options$display <- tolower(web$options$display)

  if(isTRUE(web$options$shrink)){
    stepwise_method <- 'shrink_to_last_match'
  }
  if(isTRUE(web$options$expand) & isFALSE(web$options$shrink)){
    stepwise_method <- 'expand_with_priority'
  }
  if(isFALSE(web$options$expand) & isFALSE(web$options$shrink)){
    stepwise_method <- 'ordered_only'
  }
  show_link_id = TRUE

  #
  if(inherits(web$options$recursive, "logical")){
    web$options$is_recursive <- web$options$recursive
    if(isTRUE(web$options$recursive)){
      web$options$recursive <- c("linked", "unlinked")
    }else{
      web$options$recursive <- "none"
    }
  }else{
    web$options$is_recursive <-
      any(c("linked", "unlinked") %in%
            web$options$recursive) &
      !"none" %in% web$options$recursive
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
  web$repo$wind_id <- integer()
  web$repo$cur_refs <- rep(0L, web$n.row)
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
  if(!inherits(web$match.cri$data_links, "list")){
    web$match.cri$data_links <- list(l = web$match.cri$data_links)
  }
  if(is.null(names(web$match.cri$data_links))){
    names(web$match.cri$data_links) <- rep("l", length(web$match.cri$data_links))
  }
  names(web$match.cri$data_links) <- ifelse(
    names(web$match.cri$data_links) == "", "l", names(web$match.cri$data_links))
  #
  if(length(web$options$batched) == 1 & length(web$match.cri$criteria) > 1){
    web$options$batched <- rep(
      web$options$batched,
      length(web$match.cri$criteria))
  }
  #
  web$repo$tag <-
    web$repo$iteration <- rep(0L, web$n.row)
  web$repo$sys.linked <-
    web$repo$cri.linked <- rep(FALSE, web$n.row)
  web$mxp_cri <- length(web$match.cri$criteria) + 1L
  web$repo$pid_cri <- rep(web$mxp_cri, web$n.row)
  #
  if(!is.null(web$repo$strata)) {
    web$ite.tmp$exc_strata_lgk <- is.na(web$repo$strata)
  }else{
    web$ite.tmp$exc_strata_lgk <- FALSE
  }
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
  while(web$i %in% seq_len(length(web$match.cri$criteria))){
    web$i_nm <- ifelse(
      !is.null(names(web$match.cri$criteria[web$i])),
      paste0(web$i, ": ", names(web$match.cri$criteria[web$i])),
      web$i)
    if(grepl("^progress", web$options$display)){
      cat(paste0("`Criteria ", web$i_nm,"`.\n"), sep = "")
    }
    #
    web$cri.tmp$sub.cri <- web$match.cri$sub_criteria[
      which(names(web$match.cri$sub_criteria) == paste0("cr", web$i))
    ]
    web$options$is_nested <- length(web$cri.tmp$sub.cri) > 0
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

    web$repo$cri.linked <- rep(FALSE, web$n.row)

    # Restart iteration
    web$repo$iteration[
      which(!web$repo$sys.linked)
    ] <- 0L

    # Attribute for current stage
    web$repo$cri_l <-
      web$repo$cri_level <-
      web$match.cri$criteria[[web$i]]
    # Reuse place holders in `criteria`
    if(length(web$repo$cri_level) == 1){
      web$repo$cri_level <- rep(web$repo$cri_level, web$n.row)
    }
    # Records considered for linkage in the current stage
    web$ite.tmp$inc_lgk <-
      # missing values for current stage/attribute
      !is.na(web$repo$cri_level) &
      # skipped strata
      !web$ite.tmp$exc_strata_lgk
    # unique values for current stage/attribute
    web$ite.tmp$inc_lgk[web$ite.tmp$inc_lgk] <-
      !(!duplicated(web$repo$cri_level[web$ite.tmp$inc_lgk], fromLast = TRUE) &
          !duplicated(web$repo$cri_level[web$ite.tmp$inc_lgk], fromLast = FALSE))

    web$repo$cri_level <- list(current = web$repo$cri_level)
    if(!is.null(web$repo$strata)) {
      web$repo$cri_level[["strata"]] <- web$repo$strata
    }
    #
    if(stepwise_method == 'shrink_to_last_match' & web$ite != 1){
      web$repo$cri_level[["previous_pid"]] <- web$repo$pid
      # web$repo$cri_level$previous_pid[!web$repo$sys.linked] <-
      #   web$repo$pr_sn[!web$repo$sys.linked]
      # web$repo$sys.linked <- rep(FALSE, web$n.row)
    }
    # Current stage
    web$repo$cri <- rep(NA, web$n.row)
    if(any(c('strata', 'previous_pid') %in% names(web$repo$cri_level))){
      if(length(which(web$ite.tmp$inc_lgk)) > 0){
        web$repo$cri[which(web$ite.tmp$inc_lgk)] <- combi(
          lapply(web$repo$cri_level, function(x) x[web$ite.tmp$inc_lgk]))
        # Exclude records which are unique combinations by this point
        web$ite.tmp$inc_lgk[web$ite.tmp$inc_lgk][
          (!duplicated(web$repo$cri[web$ite.tmp$inc_lgk], fromLast = TRUE) &
             !duplicated(web$repo$cri[web$ite.tmp$inc_lgk], fromLast = FALSE))
        ] <- FALSE
      }
    }else{
      web$repo$cri[which(web$ite.tmp$inc_lgk)] <-
        web$repo$cri_level$current[which(web$ite.tmp$inc_lgk)]
    }
    #
    web$ite.tmp$cri_inc_indx <- which(web$ite.tmp$inc_lgk)
    #
    if(stepwise_method == 'shrink_to_last_match' & web$i > 1){
      # Only records matched at the previous stage are included
      web$ite.tmp$cri_inc_indx <-
        web$ite.tmp$cri_inc_indx[web$repo$pid_cri[
          web$ite.tmp$cri_inc_indx] >= web$i - 1]
    }

    if(!stepwise_method %in% c('shrink_to_last_match', 'expand_without_priority')){
      # Remove entire strata if all records in have been linked
      web$ite.tmp$cri_inc_indx <- web$ite.tmp$cri_inc_indx[
        web$repo$cri[web$ite.tmp$cri_inc_indx] %in%
          web$repo$cri[
            web$ite.tmp$cri_inc_indx[
              !web$repo$sys.linked[web$ite.tmp$cri_inc_indx]
            ]
          ]
      ]
    }

    if(stepwise_method == 'ordered_only'){
      # Remove records matched in previous stages
      web$ite.tmp$cri_inc_indx <- web$ite.tmp$cri_inc_indx[
        web$repo$pid_cri[web$ite.tmp$cri_inc_indx] >= web$i]
    }else if (stepwise_method %in% c('expand_with_priority', 'expand_without_priority')) {
      # if(length(which(!web$repo$sys.linked[web$ite.tmp$cri_inc_indx])) == 0){
      #   web$ite.tmp$cri_inc_indx <- numeric()
      # }
    }

    #
    if(isTRUE(web$options$is_nested)){
      tmp.func.1 <- function(x){
        if(length(x) == 1){
          exc_indx <- Inf
        }else{
          exc_indx <- which(is.na(x))
        }
        return(exc_indx)
      }
      tmp.func.2 <- function(x){
        rc_dv(x = x, func = tmp.func.1)
      }
      mVal.indx <- attr_eval(web$cri.tmp$sub.cri[[1]],
                             simplify = TRUE, func = tmp.func.2)
      if(Inf %in% mVal.indx){
        web$ite.tmp$cri_inc_indx <- numeric()
      }else{
        web$ite.tmp$cri_inc_indx <-
          web$ite.tmp$cri_inc_indx[!web$ite.tmp$cri_inc_indx %in% mVal.indx]
      }
    }

    if(length(web$ite.tmp$cri_inc_indx) %in% 0:1 |
       (length(web$repo$cri_l) == 1 & isFALSE(web$options$is_nested))) {
      if(grepl("^progress|^stats", web$options$display)){
        cat(paste0("  -> Skipped.\n\n"))
      }
      web$i <- web$i + 1L
      web$ite <- web$ite + 1L
      next
    }

    if(isFALSE(web$options$is_nested)){
      web$cri.tmp$sub.cri <- list(sub_criteria(rep(TRUE, web$n.row)))
    }

    web$repo$tag <- rep(0L, web$n.row)
    # web$repo$ite.linked <- as.logical(web$repo$tag)
    web$repo$bkp_pid <- web$repo$pid
    web$itx <- 1L

    if(grepl("^progress|^stats", web$options$display) &
       isTRUE(web$options$is_nested)){
      cat("  -> Checking `sub_criteria`\n")
    }
    web$ite.tmp$ite_inc_indx <- web$ite.tmp$cri_inc_indx
    while(suppressWarnings(min(web$repo$tag[web$ite.tmp$ite_inc_indx])) != 2 &
          length(web$ite.tmp$ite_inc_indx) > 0) {
      #
      web$tmp$lgk <- web$repo$tag[web$ite.tmp$ite_inc_indx] != 2
      if(web$itx > 1 & web$options$is_recursive){
        web$tmp$lgk[
          (web$repo$cri.linked[web$ite.tmp$ite_inc_indx] & "linked" %in% web$options$recursive) |
            (!web$repo$cri.linked[web$ite.tmp$ite_inc_indx] & "unlinked" %in% web$options$recursive)
        ] <- TRUE
      }
      web$ite.tmp$ite_inc_indx <- web$ite.tmp$ite_inc_indx[web$tmp$lgk]

      web$sort_ord <- order(
        web$repo$cri[web$ite.tmp$ite_inc_indx],
        web$repo$tag[web$ite.tmp$ite_inc_indx],
        web$repo$pid_cri[web$ite.tmp$ite_inc_indx],
        web$repo$tie_sort[web$ite.tmp$ite_inc_indx],
        web$repo$pr_sn[web$ite.tmp$ite_inc_indx])

      web$ite.tmp$ite_inc_indx <- web$ite.tmp$ite_inc_indx[web$sort_ord]
      if(length(web$ite.tmp$ite_inc_indx) <= 1){
        web$repo$pid_cri[web$ite.tmp$ite_inc_indx[
          web$repo$pid_cri[web$ite.tmp$ite_inc_indx] ==
            web$mxp_cri
        ]] <- -1L
        web$ite <- web$ite + 1L
        if(grepl("^progress", web$options$display)){
          web$msg <- progress_bar(
            n = 1, d = 1, max_width = 100,
            msg = paste0("Iteration ",
                         fmt(web$ite), " (",
                         fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                         ")"),
            prefix_msg = "  ")
          cat(web$msg, "\r", sep = "")
        }
        break
      }
      web$ite.tmp$index_cd <- !duplicated(web$repo$cri[web$ite.tmp$ite_inc_indx], fromLast = FALSE)
      if(web$options$batched[web$i] == "semi" # & isTRUE(web$options$is_nested)
      ){
        web$ite.tmp$index_cd[web$repo$tag[web$ite.tmp$ite_inc_indx] == -1] <- TRUE
      }else if(web$options$batched[web$i] == "no" # & isTRUE(web$options$is_nested)
      ){
        web$ite.tmp$index_cd[TRUE] <- TRUE
      }
      #
      web$ite.tmp$batch_strata <- web$repo$cri[web$ite.tmp$ite_inc_indx]
      if(web$options$batched[[web$i]] != "no"){
        web$ite.tmp$index_cd[
          !duplicated(web$ite.tmp$batch_strata) & !web$ite.tmp$index_cd] <- TRUE
      }
      # Create record-pairs
      web$rec.pairs <- make_pairs_batched(
        strata = web$ite.tmp$batch_strata,
        x = web$ite.tmp$ite_inc_indx,
        index_record = web$ite.tmp$index_cd,
        assign_ord = seq_len(length(web$ite.tmp$ite_inc_indx)),
        look_back = web$options$permutations_allowed,
        # include_repeat = web$options$repeats_allowed,
        include_repeat = TRUE,
        ignore_same_source = web$options$ignore_same_source,
        data_source = web$data_source)

      if(isFALSE(web$options$repeats_allowed) & length(web$rec.pairs$x_pos) == 0){
        # Possible when `repeats_allowed` FALSE
        #     and `permutations_allowed = TRUE`
        #     and current iteration has only 1 record
        # Recreate `web$rec.pairs` as if `repeats_allowed` = TRUE so that `web$ite.tmp$ite_inc_indx` is tagged
        web$rec.pairs <- list(
          x_pos = seq_len(length(web$ite.tmp$ite_inc_indx)),
          y_pos = seq_len(length(web$ite.tmp$ite_inc_indx)),
          index_ord = rep(1L, length(web$ite.tmp$ite_inc_indx)),
          x_val = web$ite.tmp$ite_inc_indx,
          y_val = web$ite.tmp$ite_inc_indx)
      }

      names(web$rec.pairs)[which(names(web$rec.pairs) == "x_val")] <- "cu_pos"
      names(web$rec.pairs)[which(names(web$rec.pairs) == "y_val")] <- "tr_pos"
      #
      if(isTRUE(web$options$is_nested)){
        web$rec.pairs[["rec.match"]] <- eval_sub_criteria(
          x = web$cri.tmp$sub.cri[[1]],
          x_pos = web$rec.pairs$cu_pos,
          y_pos = web$rec.pairs$tr_pos,
          check_duplicates = web$options$check_duplicates)

        web$export.nm <- names(web$rec.pairs$rec.match)
        web$export.nm <- web$export.nm[!grepl("^logical|^equal", web$export.nm)]
        if(length(web$export.nm) > 0){
          web$export[[paste0("cri.", web$i)]][[paste0("iteration.", web$ite)]] <-
            web$rec.pairs$rec.match[web$export.nm]
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

      # Flag the reference record
      web$rec.pairs$ref_rd <- web$rec.pairs$cu_pos == web$rec.pairs$tr_pos
      # Update window ids for matched or reference records
      web$rec.pairs$w.hits <-
        (web$rec.pairs$rec.match$logical_test | web$rec.pairs$ref_rd)
      web$rec.pairs$w.match <- web$rec.pairs$w.hits
      if(stepwise_method %in% c('expand_with_priority')){
        web$rec.pairs$w.match <- web$rec.pairs$w.match &
          !web$repo$sys.linked[web$rec.pairs$cu_pos]
      }

      if(show_link_id & any(web$rec.pairs$w.match)){
        web$rec.pairs$cu_pos.mi <- ((web$rec.pairs$index_ord[web$rec.pairs$w.match] + web$repo$cur_refs[web$rec.pairs$cu_pos[web$rec.pairs$w.match]]-1L) * web$n.row) + web$rec.pairs$cu_pos[web$rec.pairs$w.match]

        new_max <- ceiling(max(web$rec.pairs$cu_pos.mi) / web$n.row)
        old_max <- max(web$repo$cur_refs)

        if(old_max < new_max){
          web$repo$wind_id <- c(web$repo$wind_id, rep(rep(NA, web$n.row), (new_max - old_max)))
        }
        web$repo$wind_id[web$rec.pairs$cu_pos.mi] <- web$rec.pairs$tr_pos[web$rec.pairs$w.match]

        tgt_pos <- rle(sort(web$rec.pairs$cu_pos[web$rec.pairs$w.match]))
        web$repo$cur_refs[tgt_pos$values] <- web$repo$cur_refs[tgt_pos$values] + tgt_pos$lengths

        rm(tgt_pos)
      }

      #
      e.match_lgk <- # web$rec.pairs$rec.match$logical_test &
        web$rec.pairs$w.match
      if(stepwise_method != 'shrink_to_last_match'){
        e.match_lgk <- e.match_lgk &
          !web$rec.pairs$ref_rd
      }

      web$rec.pairs$e.match <- web$rec.pairs$cu_pos %in% web$rec.pairs$cu_pos[e.match_lgk]
      web$rec.pairs$e.match <- web$rec.pairs$index_ord == 1 & web$rec.pairs$e.match
      web$rec.pairs$index_rd <- web$rec.pairs$cu_pos %in% web$rec.pairs$cu_pos[web$rec.pairs$ref_rd]
      web$rec.pairs$index_rd <- (web$rec.pairs$index_ord == 1 & web$rec.pairs$index_rd) | isFALSE(web$options$is_nested)
      #
      if(web$options$batched[web$i] == "no"){
        web$ite.tmp$batched_pids <- make_ids(web$rec.pairs$x_pos[web$rec.pairs$rec.match$logical_test],
                                             web$rec.pairs$y_pos[web$rec.pairs$rec.match$logical_test],
                                             id_length = max(web$rec.pairs$x_pos))
        web$repo$pid[
          web$ite.tmp$ite_inc_indx[web$ite.tmp$batched_pids$sn[web$ite.tmp$batched_pids$linked]]
        ] <- web$ite.tmp$ite_inc_indx[web$ite.tmp$batched_pids$group_id[web$ite.tmp$batched_pids$linked]]
      }else{
        if(stepwise_method == 'shrink_to_last_match'){
          web$repo$pid[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] <- web$repo$pr_sn[web$rec.pairs$tr_pos[web$rec.pairs$e.match]]
        }else{
          web$repo$pid[web$rec.pairs$cu_pos[web$rec.pairs$e.match]] <- web$repo$pid[web$rec.pairs$tr_pos[web$rec.pairs$e.match]]
        }

        if(isTRUE(web$options$is_recursive)){
          web$ite.tmp$tr_refs <- list(cu_pos = web$rec.pairs$cu_pos[web$rec.pairs$index_ord == 1], tr_pos = web$rec.pairs$tr_pos[web$rec.pairs$index_ord == 1])
          web$ite.tmp$tr_refs$inherit_lgk <- web$repo$tag[web$ite.tmp$tr_refs$cu_pos] == 2 & web$repo$pid[web$ite.tmp$tr_refs$cu_pos] != web$repo$bkp_pid[web$ite.tmp$tr_refs$cu_pos]
          web$ite.tmp$tr_refs$inherit_lgk <- web$repo$bkp_pid[web$ite.tmp$tr_refs$cu_pos] %in% web$repo$bkp_pid[web$ite.tmp$tr_refs$cu_pos[web$ite.tmp$tr_refs$inherit_lgk]]
          web$repo$pid[web$ite.tmp$tr_refs$cu_pos[web$ite.tmp$tr_refs$inherit_lgk]] <- web$repo$pid[web$ite.tmp$tr_refs$tr_pos[web$ite.tmp$tr_refs$inherit_lgk]]
        }
      }

      if(isTRUE(web$options$is_recursive)){
        web$repo$ovr_lgk <- web$repo$pr_sn %in% web$rec.pairs$cu_pos[web$rec.pairs$e.match] &
          web$repo$cri.linked
        # web$ite.tmp$tgt_pid <- web$repo$bkp_pid
        web$ite.tmp$tgt_pid <- web$repo$pid
        web$ite.tmp$ovr_grp_indx <- which(web$ite.tmp$tgt_pid %in% web$ite.tmp$tgt_pid[web$repo$ovr_lgk])
        web$repo$pid[web$ite.tmp$ovr_grp_indx] <- web$repo$bkp_pid[web$repo$ovr_lgk][
          match(
            web$ite.tmp$tgt_pid[web$ite.tmp$ovr_grp_indx],
            web$ite.tmp$tgt_pid[web$repo$ovr_lgk])]
      }

      web$ite.tmp$linked_lgk <- which(web$rec.pairs$e.match | web$rec.pairs$index_rd)
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
        web$ite.tmp$tgt_indx <- web$rec.pairs$cu_pos[web$rec.pairs$rec.match$logical_test]
      }else{
        web$ite.tmp$tgt_indx <- c(web$rec.pairs$cu_pos[web$rec.pairs$rec.match$logical_test],
                                  web$rec.pairs$tr_pos[web$rec.pairs$rec.match$logical_test])
      }
      web$repo$iteration[web$ite.tmp$tgt_indx][
        web$repo$tag[web$ite.tmp$tgt_indx] == 2 &
          (web$repo$iteration[web$ite.tmp$tgt_indx] == 0 |
          stepwise_method == 'shrink_to_last_match')
      ] <- web$ite

      web$repo$pid_cri[web$ite.tmp$tgt_indx][
        web$repo$tag[web$ite.tmp$tgt_indx] == 2 &
          (web$repo$pid_cri[web$ite.tmp$tgt_indx] == web$mxp_cri |
          stepwise_method == 'shrink_to_last_match')
      ] <- web$i

      web$repo$cri.linked[web$ite.tmp$tgt_indx] <- TRUE
      #
      web$repo$bkp_pid <- web$repo$pid
      #
      web$ite.tmp$ite.row.n <- length(web$ite.tmp$ite_inc_indx)
      web$ite.tmp$cri.row.n <- length(web$ite.tmp$cri_inc_indx)
      web$ite.tmp$ite.linked.n <- length(which(web$repo$tag[web$ite.tmp$ite_inc_indx] == 2))
      web$ite.tmp$cri.linked.n <- length(which(web$repo$tag[web$ite.tmp$cri_inc_indx] == 2))
      #
      if(isTRUE(web$options$is_nested)){
        if(grepl("^progress", web$options$display)){
          web$msg <- progress_bar(
            n = web$ite.tmp$cri.linked.n,
            d = web$ite.tmp$cri.row.n,
            max_width = 100,
            msg = paste0("Iteration ",
                         fmt(web$ite), " (",
                         fmt(difftime(Sys.time(), web$tm_ia), "difftime"),
                         ")"),
            prefix_msg = "  ")
          cat(web$msg, "\r", sep = "")
        }else if (grepl("^stats", web$options$display)){
          web$msg <- update_text(
            tot_records = fmt(web$ite.tmp$cri.row.n),
            current_tot = fmt(web$ite.tmp$ite.row.n),
            current_tagged = fmt(web$ite.tmp$ite.linked.n),
            time = fmt(Sys.time() - web$tm_ia, "difftime"),
            iteration = web$ite,
            indent_txt = "  "
          )
          cat('\r', web$msg, "", sep = "")
        }
      }
      #
      if(grepl("report$", web$options$display)){
        web$rp_data <- di_report(
          cumm_time = Sys.time() - web$tm_a,
          duration = Sys.time() - web$tm_ia,
          criteria = web$i,
          iteration = web$ite,
          current_tagged = web$ite.tmp$ite.linked.n,
          current_tot = web$n.row,
          memory_used =  utils::object.size(web[names(web)[names(web) != "report"]]))
        web$report[length(web$report) + 1] <- list(web$rp_data)
      }
      web$tm_ia <- Sys.time()
      web$ite <- web$ite + 1L
      web$itx <- web$itx + 1L
    }

    ta <- Sys.time()
    # Unlink pids with a single record for another attempt in the next stage
    # web$tgt_indx <- web$ite.tmp$cri_inc_indx[!web$repo$sys.linked[web$ite.tmp$cri_inc_indx]]
    web$tgt_indx <- web$rec.pairs$cu_pos[web$rec.pairs$w.hits]
    web$ite.tmp$lgk <- (
      !duplicated(web$repo$pid[web$tgt_indx], fromLast = FALSE) &
        !duplicated(web$repo$pid[web$tgt_indx], fromLast = TRUE) &
        !web$repo$pid[web$tgt_indx] %in% web$repo$pid[web$repo$pr_sn[!web$repo$pr_sn %in% web$tgt_indx]]
    )
    web$repo$sys.linked[web$tgt_indx][
      !web$repo$pid[web$tgt_indx] %in% web$repo$pid[web$tgt_indx[web$ite.tmp$lgk]]
    ] <- TRUE

    #
    # web$repo$pid[web$tgt_indx[web$ite.tmp$lgk]] <-
    #   web$repo$pr_sn[web$tgt_indx[web$ite.tmp$lgk]]
    web$repo$pid_cri[web$tgt_indx[web$ite.tmp$lgk]] <- web$mxp_cri
    web$repo$iteration[web$tgt_indx[web$ite.tmp$lgk]] <- 0L
    # Flag records linked at current stage
    #
    web$current_tot <- length(web$ite.tmp$cri_inc_indx)
    web$assigned <- length(web$ite.tmp$cri_inc_indx[
      web$repo$pid_cri[web$ite.tmp$cri_inc_indx] == web$i])
    if(grepl("^progress|^stats", web$options$display)){
      if(grepl("^stats", web$options$display)){
        cat(paste0("`Criteria ", web$i_nm,"`.",
                   ifelse(isFALSE(web$options$is_nested), "\n", "")), sep = "")
      }

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

  if(stepwise_method == 'shrink_to_last_match'){
    web$repo$pid <- bys_val(
      by = combi(web$repo$pid, web$repo$pid_cri),
      val = web$repo$pr_sn)
  }
  #
  web$repo$iteration[web$repo$iteration == 0] <- web$ite - 1L
  web$ite.tmp$lgk <- (!duplicated(web$repo$pid) & !duplicated(web$repo$pid, fromLast = TRUE))
  # Skipped records
  if(!inherits(web$repo$strata, "NULL")){
    web$repo$pid_cri[
      web$ite.tmp$lgk & is.na(web$repo$strata)
    ] <- -1L
  }
  # Unmatched records
  web$repo$pid_cri[
    web$ite.tmp$lgk & web$repo$pid_cri != -1
  ] <- 0L
  #
  web$repo$pid[web$ite.tmp$lgk] <-
    web$repo$pr_sn[web$ite.tmp$lgk]

  if(show_link_id){
    web$repo$wind_id[
      index_multiples(
        x = which(web$ite.tmp$lgk),
        multiples = web$n.row,
        repeats = web$counts$max_indexes)$mm
    ] <- NA_real_
  }

  #
  if(!is.null(web$repo$sn)){
    web$repo$pid <- web$repo$sn[web$repo$pid]
    web$repo$wind_id <- web$repo$sn[web$repo$wind_id]
    web$repo$pr_sn <- web$repo$sn[web$repo$pr_sn]
  }
  #
  web$pids <- make_pids(
    y_pos = web$repo$pid,
    x_pos = web$repo$pr_sn,
    pid_cri = web$repo$pid_cri,
    iteration = web$repo$iteration,
    link_id = web$repo$wind_id,
    data_source = web$repo$data_source,
    data_links = web$match.cri$data_links)
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
