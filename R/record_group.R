#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Link  records with matching criteria in ordered stages of relevance.
#' Each set of linked records are assigned a unique identifier with relevant group-level data.
#'
#' @param df \code{data.frame}. One or more datasets appended together. See \code{Details}.
#' @param sn Unique numerical record identifier. Useful for creating familiar group identifiers.
#' @param strata Subsets of the dataset. Record groups are created separately for each \code{strata}. Assigning \code{NA} to \code{strata} will skip that record.
#' @param criteria \code{list} of attributes to compare. Each element of the list is a stage in the linkage process. See \code{Details}.
#' @param sub_criteria \code{list} of additional attributes to compare at each stage of the linkage process. Comparisons are done as an exact match or with user-defined logical tests. See \code{\link{sub_criteria}}
#' @param data_source Unique data source identifier. Includes a list of data sources of for each record in a \code{pid}.
#' @param group_stats If \code{TRUE} (default), returns group-specific information like record counts. See \code{Value}.
#' @param data_links A set of \code{data_sources} required in a \code{pid}. A \code{pid} without records from these \code{data_sources} are skipped or unlinked. See \code{Details}.
#' @param expand If \code{TRUE}, allows a record group to expand with each subsequent stage of the linkage process.
#' @param shrink If \code{TRUE}, forces a record group to shrink with each subsequent stage of the linkage process.
#' @param display The messages printed on screen. Options are; \code{"none"} (default) or, \code{"progress"} and \code{"stats"} for a progress update or a more detailed breakdown of the linkage process.
#' @param to_s4 Object type of the returned object. \code{\link[=pid-class]{pid}} (\code{TRUE}) or \code{data.frame} (\code{FALSE}).
#' @param schema Return a schema of the \code{\link[=pid-class]{pid}} object. Options are; \code{"none"} (default), \code{"by_pid"}, \code{"by_strata"} or \code{"by_ALL"}.
#' @param ... Arguments passed to \bold{\code{links}}
#'
#' @return \code{\link[=pid-class]{pid}} or \code{list} (\code{\link[=pid-class]{pid}} and \code{ggplot}) object
#'
#' @seealso \code{\link{episodes}}, \code{\link{partitions}}, \code{\link{predefined_tests}} and \code{\link{sub_criteria}}
#'
#' @details
#' Priority of matches in multistage linkages is given to matches that occurred at earlier stages (\code{criteria}) of the linkage process.
#' Therefore, it's important for each \code{criteria} to be listed in order of decreasing relevance.
#'
#' Records with missing \code{criteria} (\code{NA}) are skipped at each stage.
#' Another attempt will be made to match the record at the next stage.
#' If a record does not match any other record by the end of the linkage process, it is assigned a unique group ID.
#'
#' A \code{sub_criteria} is a means of specifying additional matching conditions for a stage of the linkage process.
#' When used, only records with matching \code{criteria} and \code{sub_criteria} are linked.
#'
#' Each \code{sub_criteria} must be linked to a \code{criteria} in \bold{\code{\link{links}}}.
#' You can also link multiple \code{sub_criteria} to one \code{criteria}.
#' Any unlinked \code{sub_criteria} will be ignored.
#'
#' By default, records are compared for an exact match.
#' However, user-defined logical tests (function) are also permitted. Such test must meet 3 requirements:
#' \enumerate{
#' \item It must be able to compare two atomic vectors
#' \item It must have two arguments at least two arguments named \code{`x`} and \code{`y`}, where \code{`y`} is the value for one observation being compared against all other observations (\code{`x`}).
#' \item It must return either TRUE or FALSE.
#' }
#'
#' \code{data_links} must be a \code{list} of \code{atomic} vectors, with every element named \code{"l"} (links) or \code{"g"} (groups).
#'
#' \itemize{
#' \item if named \code{"l"}, only groups with records from every listed \code{data_source} will be retained.
#' \item if named \code{"g"}, only groups with records from any listed \code{data_source} will be retained.
#' }
#'
#' \bold{\code{record_group()}} as it existed before \code{v0.2.0} has been retired.
#' Its now exists to support previous code and arguments with minimal disruption. Please use \bold{\code{links()}} instead.
#'
#' See \code{vignette("links")} for more information.
#'
#' @examples
#' library(diyar)
#' # Exact match
#' links(criteria = c("Obinna","James","Ojay","James","Obinna"))
#'
#' # User-defined tests using `sub_criteria()`
#' # Matching `sex` and + 20-year age gaps
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
#' # `staff_id` > `age` AND (`initials`, `hair_colour` OR `branch_office`)
#' data(missing_staff_id); missing_staff_id
#' links(criteria = list(missing_staff_id$staff_id, missing_staff_id$age),
#'       sub_criteria = list(s2 = sub_criteria(missing_staff_id$initials,
#'                                           missing_staff_id$hair_colour,
#'                                           missing_staff_id$branch_office)),
#'       data_source = missing_staff_id$source_1)
#'
#' # Group expansion and shrinkage
#' match_cri <- list(c(1,NA,NA,1,NA,NA),
#'                   c(1,1,1,2,2,2),
#'                   c(3,3,3,2,2,2))
#' links(criteria = match_cri, expand = TRUE)
#' links(criteria = match_cri, expand = FALSE)
#' links(criteria = match_cri, shrink =  TRUE)
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
                  shrink = FALSE){
  tm_a <- Sys.time()

  rut <- attr(sub_criteria, "diyar_sub_criteria")
  if(class(rut) != "NULL"){
    if(rut == T){
      sub_criteria <- list(sub_criteria)
    }
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

  # Maximum no. of records from all criteria
  ds_len <- as.numeric(lapply(criteria, length))
  ds_len_b <- sub_criteria[grepl("cr[0-9]", names(sub_criteria))]
  ds_len_b <- sapply(ds_len_b, function(x){
    sapply(x, function(x){
      if(class(x[[1]]) == "list"){
        length(x[[1]][[1]])
      }else{
        length(x[[1]])
      }
       })
  })
  ds_len <- max(c(ds_len, unlist(ds_len_b, use.names = FALSE)))
  rm(ds_len_b)

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
  }
  # `data_links`
  dl_lst <- unlist(data_links, use.names = FALSE)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])
  # `display`
  display <- tolower(display)
  # Place holders for group-level options
  tag <- rep(0, ds_len)
  iteration <- tag
  m_tag <- tag
  pid_cri <- rep(Inf, ds_len)
  sn_ref <- min(sn) - 1
  pid <- rep(sn_ref, ds_len)
  link_id <- pid
  n_seq <- seq_len(ds_len)

  if(display != "none") cat("\n")
  ite <- 1
  for(i in 1:length(criteria)){
    # Current stage
    cri <- criteria[[i]]
    # Standardise `criteria` input
    if (length(cri) == 1) cri <- rep(cri, ds_len)
    # Re-order to current sort order
    cri <- cri[match(pr_sn, n_seq)]
    # Identify records to be skipped
    lgk <- is.na(cri)
    if(!is.null(strata)) {
      lgk[!lgk] <- is.na(strata[!lgk])
      cri[!lgk] <- paste0(strata[!lgk], " ", cri[!lgk])
    }
    # Prevent `pid` from expanding
    if(shrink == TRUE){
      cri <- paste0(cri, " ", pid)
      pid[TRUE] <- sn_ref
      link_id[TRUE] <- sn_ref
    }
    # Encode current `criteria`
    cri <- match(cri, cri[!duplicated(cri)])
    cri[lgk] <- -10

    force_check <- rep(0, ds_len)
    skip <- force_check
    m_tag <- force_check
    min_pid <- sn_ref
    min_m_tag <- 0
    while (min_pid == sn_ref | min_m_tag == -1) {
      sort_ord <- order(cri, skip, -force_check, -tag, m_tag, pid_cri, sn, decreasing = TRUE)
      for(vr in c("force_check","tag","cri",
                  "skip", "pid", "tag","m_tag",
                  "pid_cri", "sn", "pr_sn",
                  "link_id", "iteration")){
        assign(vr, get(vr)[sort_ord])
      }

      if(!is.null(strata)) {
        strata <- strata[sort_ord]
      }
      # Reference records
      r <- rle(cri)
      lgk <- !duplicated(cri, fromLast = TRUE) | is.na(cri)
      tr_link_id <- rep(link_id[lgk], r$lengths[match(cri[lgk], r$values)])
      tr_pid_cri <- rep(pid_cri[lgk], r$lengths[match(cri[lgk], r$values)])
      tr_tag <- rep(tag[lgk], r$lengths[match(cri[lgk], r$values)])
      tr_pid <- rep(pid[lgk], r$lengths[match(cri[lgk], r$values)])
      tr_sn <- rep(sn[lgk], r$lengths[match(cri[lgk], r$values)])

      # Implement `sub_criteria`
      curr_sub_cri <- sub_criteria[which(names(sub_criteria) == paste0("cr", i))]
      sub_cri_match <- sub_cri_checks(sub_criteria = curr_sub_cri,
                                      strata = cri,
                                      index_record = tr_sn == sn,
                                      sn = pr_sn,
                                      skip_repeats = TRUE)

      equals_ref_rd <- sub_cri_match[[2]]
      sub_cri_match <- sub_cri_match[[1]]

      # Track records checked for the current `sub_criteria`
      # Records with matching `sub_criteria` from earlier stages should trigger a re-check for possible updates to the link
      m_tag <- ifelse(m_tag == 1 &
                        sub_cri_match > 0 &
                        pid_cri <= tr_pid_cri,
                      -1, m_tag)
      # A re-check that doesn't lead to a change in link should not be checked again
      m_tag <- ifelse(m_tag == -1 &
                        sub_cri_match > 0 &
                        pid == tr_pid &
                        !is.na(tr_pid),
                      1, m_tag)

      # Expand record groups
      pid <- ifelse(((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid == sn_ref & !is.na(tr_pid))) &
                      ((tr_pid_cri == pid_cri & !expand) | (expand)),
                    tr_pid, pid)

      pid <- ifelse(sub_cri_match > 0 &
                      pid == sn_ref &
                      !tr_pid %in% c(sn_ref, NA) &
                      ((tr_pid_cri == pid_cri & !expand) | (expand)),
                    tr_pid, pid)

      # Assign new IDs for newly matched records
      pid <- ifelse(((pid == sn_ref &
                        tr_pid == sn_ref &
                        !is.na(tr_pid))) &
                      sub_cri_match > 0,
                    tr_sn, pid)
      # Matching records
      link_id <- ifelse(((link_id == sn_ref & !is.na(tr_link_id) & sub_cri_match > 0) |
                           ((m_tag == -1 & pid != sn_ref) | (sub_cri_match > 0 & pid==sn_ref & !is.na(tr_pid)))) &
                          ((tr_pid_cri == pid_cri & !expand) | (expand)),
                        tr_sn, link_id)

      # Identify records that match others previously checked for a `sub_criteria`
      # Skip from another check
      m_tag <- ifelse(pid != sn_ref & m_tag != -1, 1, m_tag)
      m_tag <- ifelse(m_tag != 1 & equals_ref_rd == 1, 1, m_tag)

      m_tag <- ifelse(sn == tr_sn & !is.na(tr_sn) & m_tag == -1, 1, m_tag)
      pid <- ifelse(pid == sn_ref &
                      m_tag == 1 &
                      equals_ref_rd > 0,
                    sn, pid)

      skip <- ifelse(m_tag == -1 &
                       !is.na(m_tag), 0,
                     ifelse(sub_cri_match > 0,
                            1, skip))
      # Track checks for valid `criteria` entries
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
        progress_txt(length(m_tag[m_tag != 0]), ds_len, msg = msg)
      }
      iteration[pid != sn_ref & iteration == 0] <- ite
      ite <- ite + 1
    }
    if(display != "none" & length(curr_sub_cri) > 0) cat("\n")
    if(display %in% c("progress", "stats")){
      cat(paste0("Checked `criteria ", i,"`.\n"))
    }
    if(display == "stats"){
      assigned <- length(tag[tag == 0 & !pid %in% c(sn_ref, NA)])
      removed <- length(tag[!duplicated(pid) & !duplicated(pid, fromLast = TRUE)])
      current_tot <- length(tag[tag == 0 & !pid %in% c(sn_ref, NA)])
      cat(paste0(fmt(current_tot), " records(s): ", fmt(current_tot - removed)," linked.\n"))
    }

    tag <- ifelse(pid %in% c(sn_ref, NA), 0, 1)
    lgk <- (!duplicated(pid) & !duplicated(pid, fromLast = TRUE)) | cri < 0
    link_id[lgk] <- sn_ref
    pid[lgk] <- sn_ref
    tag <- ifelse(pid != sn_ref, 1, 0)
    iteration[tag == 0 & iteration != 0] <- 0
    pid_cri[tag ==1 & (shrink | (pid_cri == Inf & !shrink))] <- i
  }
  iteration[iteration == 0] <- ite - 1
  if(class(strata) != "NULL"){
    pid_cri[pid == sn_ref & is.na(strata) & pid_cri == Inf] <- -1
  }
  pid_cri[pid == sn_ref & pid_cri == Inf] <- 0
  link_id[pid == sn_ref] <- sn[pid == sn_ref]
  pid[pid == sn_ref] <- sn[pid == sn_ref]

  tmp_pos <- pr_sn
  fd <- match(1:length(pid), tmp_pos)

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
      pids@pid_total[!req_links] <- 1
      pids@pid_cri[!req_links] <- -1
      pids@.Data[!req_links] <- pids@sn[!req_links]
      pids@link_id[!req_links] <- pids@sn[!req_links]
      datasets[!req_links] <- data_source[!req_links]
    }
    pids@pid_dataset <- datasets
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))

  cat("Records linked in ", tms, "!\n", sep = "")
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
  if(out$err_cd == F) {
    stop(out$err_nm, call. = FALSE)
  }
  warning(paste0("`record_group()` has been retired!:\n",
                 "i - Please use `links()` instead.\n",
                 "i - Your values were passed to `links()`."), call. = FALSE)
  if(to_s4 != T){
    return(to_df(out$err_nm))
  }else{
    return(out$err_nm)
  }
}
#' @name sub_criteria
#' @aliases sub_criteria
#' @title Sub-criteria
#'
#' @description Additional matching conditions when using \bold{\code{\link{links}}} and \bold{\code{\link{episodes}}}
#' @param ... Additional attributes to compare.
#' @param funcs User defined logical test.
#' @return \code{list}
#' @details
#' \bold{\code{sub_criteria()}} is the mechanism for providing a \code{sub_criteria} to an instance of \bold{\code{links}} or \bold{\code{episodes}}.
#'
#' Each attribute (\code{atomic} vectors) is compared with a user-defined logical test.
#'
#' Each attribute must have the same length.
#'
#' \code{funcs} must be a \code{function} or \code{list} of \code{functions} to use with each attribute.
#'
#' Each \code{funcs} must evaluate to \code{TRUE} or \code{FALSE}.
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
sub_criteria <- function(...,
                         funcs = diyar::exact_match,
                         equva = diyar::exact_match){
  err <- err_sub_criteria_1(...)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  err <- err_sub_criteria_3dot_1(...)
  if(!isFALSE(err)) stop(err, call. = FALSE)

  if(class(funcs) == "NULL"){
    funcs <- list(exact_match)
  }else{
    if(class(funcs) != "list") funcs <- list(funcs)

    err <- err_sub_criteria_2(funcs)
    if(!isFALSE(err)) stop(err, call. = FALSE)

    err <- err_sub_criteria_3(funcs)
    if(!isFALSE(err)) stop(err, call. = FALSE)

    err <- err_sub_criteria_4(..., funcs = funcs)
    if(!isFALSE(err)) stop(err, call. = FALSE)
  }

  if(length(funcs) == 1){
    funcs <- rep(list(funcs), length(list(...)))
    funcs <- unlist(funcs)
    funcs_l <- 1
  }else{
    funcs_l <- length(funcs)
  }

  if(class(equva) == "NULL"){
    equva <- list(exact_match)
  }else{
    if(class(equva) != "list") equva <- list(equva)

    err <- err_sub_criteria_2(equva)
    if(!isFALSE(err)) stop(err, call. = FALSE)

    err <- err_sub_criteria_3(equva)
    if(!isFALSE(err)) stop(err, call. = FALSE)

    err <- err_sub_criteria_4(..., funcs = equva)
    if(!isFALSE(err)) stop(err, call. = FALSE)
  }

  if(length(equva) == 1){
    equva <- rep(list(equva), length(list(...)))
    equva <- unlist(equva)
    equva_l <- 1
  }else{
    equva_l <- length(equva)
  }

  x <- function(x, y, z) list(x, y, z)
  sub_cris <- mapply(x, list(...), funcs, equva, SIMPLIFY = F)

  err <- err_sub_criteria_7(list(sub_cris), cri_nm = "sub_criteria")
  if(!isFALSE(err)) stop(err, call. = FALSE)
  err <- err_sub_criteria_7(list(sub_cris), funcs_l = "equva", funcs_pos = 3, cri_nm = "sub_criteria")
  if(!isFALSE(err)) stop(err, call. = FALSE)

  attr(sub_cris, "diyar_sub_criteria") <- TRUE
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
                                                           "X - You've supplied a `", paste0(class(range), collapse = ", "), "`` object."), call. = FALSE)

  if(length(range) != 1) stop(paste("`range` must have a length of 1:\n",
                                    "X - Length is ", length(range), "."), call. = FALSE)

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
  lg <- overlaps(as.number_line(x@gid), x)
  lg[is.na(lg)] <- F
  if(any(!lg)) {
    rng_i <- paste(head(which(!lg), 5), collapse = ", ", sep="")
    rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]
    stop(paste0("Range matching error: Actual value (gid) is out of range in ", "[", rng_i, "]"))
  }
  overlaps(as.number_line(x@gid), y)
}

#' @name links_probabilistic
#' @title Probabilistic record linkage
#'
#' @description A specific use case of \code{links} to achieve probabilistic record linkage.
#'
#' @param blocking_attribute Subsets of the dataset.
#' @param attribute \code{list} of attributes to compare.
#' @param cmp_func \code{list} of string comparators for each \code{attribute}. See \code{Details}.
#' @param cmp_threshold \code{list} of similarity score-thresholds for every each \code{attribute}. See \code{Details}.
#' @param probabilistic if \code{TRUE}, scores are assigned base on Fellegi-Sunter model for probabilistic linkage. See \code{Details}.
#' @param m_probability \code{list} m-probailities. The probability that a match from the string comparator is from the same entity.
#' @param weight_threshold Minimum threshold for linked records. See \code{Details}.
#'
#' @param ... Arguments passed to \bold{\code{links}}
#'
#' @return \code{\link[=pid-class]{pid}} or \code{list} (\code{\link[=pid-class]{pid}} and \code{ggplot}) object
#'
#' @seealso \code{\link{links}}, \code{\link{episodes}}, \code{\link{partitions}}, \code{\link{predefined_tests}} and \code{\link{sub_criteria}}
#'
#' @details
#' \code{links_probabilistic} is a wrapper function of \code{\link{links}} which is used for probabilistic record linkage.
#' Its implementation is based on Fellegi-Sunter model for deciding if two records belong to the same entity.
#'
#' In summary, two probabilities (m and u) are estimated for each record pair and used to score matches and non-matches.
#' The m-probability is the probability that matched attributes are from the same entity,
#' while u-probability is the probability that matched attributes are not from the same entity.
#' m-probabilities have to be supplied but u-probabilities are calculated for each \code{attribute} as the number of occurrences of a value divided by the number of records in the dataset.
#' Record pairs whose total score are above a certain threshold (\code{weight_threshold}) are assumed to belong to the same entity.
#'
#' For each record pair, an agreement or match score for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}(m_{i}/u_{i})}{log_2 (m_i / u_i)}
#'
#' For each record pair, a disagreement or non-match score for attribute \eqn{i} is calculated as;
#'
#' \deqn{\log_{2}((1-m_{i})/(1-u_{i}))}{log_2 ((1-m_i) / (1-u_i))}
#'
#' where \eqn{m_{i}}{m_i} and \eqn{u_{i}}{u_i} are the m and u-probabilities for attribute \eqn{i}
#'
#' Matches and non-matches for each \code{attribute} are determined by string comparators.
#' By default, this is simply an \code{\link{exact_match}} with a binary outcome.
#' Alternatively, this can also be similarity score, in which case a \code{list} of thresholds (\code{cmp_threshold}) should be provided for each comparator.
#' If \code{\link{probabilistic}} is \code{FALSE}, the \code{weight_threshold} is compared against the sum of all similarity scores.
#'
#' \code{links_probabilistic} requires a \code{weight_threshold} in advance of the linkage process.
#' This differs from the typical approach where a \code{weight_threshold} is selected after the linkage process,
#' following a review of scores from every record pair.
#' To help with this, the convenience function \code{fetch_scores} will show you the minimum and maximum scores attainable for a given dataset.
#'
#' A \code{blocking_attribute} can be used to reduce processing time by restricting comparisons to \code{strata} of the dataset.
#'
#' @aliases links_probabilistic
#' @export
links_probabilistic <- function(attribute,
                                blocking_attribute = NULL,
                                cmp_func = diyar::exact_match,
                                cmp_threshold = .95,
                                probabilistic = TRUE,
                                m_probability = .95,
                                weight_threshold = 1,
                                ...
){

  if(class(attribute) != "list"){
    attribute <- list(attribute)
  }

  if(is.null(names(attribute))){
    names(attribute) <- paste0("var_", seq_len(length(attribute)))
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

  if(is.number_line(weight_threshold)){
    weight_threshold[weight_threshold@.Data < 0] <- reverse_number_line(weight_threshold[weight_threshold@.Data < 0], "decreasing")
  }else{
    weight_threshold <- number_line(weight_threshold, Inf)
  }

  # String comparator for each attribute
  if(class(cmp_func) != "list"){
    cmp_func <- list(cmp_func)
  }
  if(length(cmp_func) == 1 & length(attribute) > 1){
    cmp_func <- rep(cmp_func, length(attribute))
  }

  if(isTRUE(probabilistic)){
    # u-probabilities
    u_probs <- lapply(attribute, function(x){
      x_cd <- match(x, x[!duplicated(x)])
      r <- rle(sort(x_cd))
      n <- r$lengths[match(x_cd, r$values)]
      n/length(x_cd)
    })

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


  # Weight or probabilistic matching
  prob_link <- function(x, y,
                        agree = cmp_threshold,
                        m_pb = m_probability,
                        thresh = weight_threshold,
                        show_stats = FALSE,
                        is_prob = probabilistic){
    # Number of attributes
    attr_n <- length(x)/2

    # Weights from a string comparator
    wts <- lapply(seq_len(attr_n), function(i){
      curr_func <- cmp_func[[i]]
      wts <- curr_func(x[[i]], y[[i]])
      wts[is.na(wts)] <- 0
      wts
    })

    # Agreement/disagreement based on `agreement` for each attribute
    matches <- lapply(seq_len(attr_n), function(i){
      lgk <- (wts[[i]] >= as.numeric(agree[i]@start) & wts[[i]] <= as.numeric(right_point(agree[i])))
      lgk & !is.na(lgk)
    })

    out_2 <- sapply(wts, function(x) x)
    if(isTRUE(show_stats)){
      sum_wt <- rowSums(out_2)
      lgk <- (sum_wt >= as.numeric(thresh@start) & sum_wt <= as.numeric(right_point(thresh)))

      #out_1 <- sapply(matches, function(x) x)
      out_a <- cbind(out_2, sum_wt, lgk)
      colnames(out_a) <- c(paste0("cmp.",attr_nm),
                           "cmp.weight",
                           "cmp.threshold")
    }

    # If weight based, matches are assigned based on the string comparisons
    if(isFALSE(is_prob)){
      sum_wt <- rowSums(out_2)
      lgk <- (sum_wt >= as.numeric(thresh@start) & sum_wt <= as.numeric(right_point(thresh)))
      return(lgk)
    }

    # If probabilistic, weight is based on m- and u-probabilities
    pwts <- sapply(seq_len(attr_n), function(i){
      pwts <- rep(0, length(matches[[i]]))
      curr_match <- matches[[i]]
      curr_uprob <- x[[i + attr_n]]
      curr_mprob <- m_pb[[i]]
      # agreements
      pwts[curr_match] <- log2(curr_mprob/curr_uprob[curr_match])
      # disagreements
      pwts[!curr_match] <- log2((1 - curr_mprob)/(1 - curr_uprob[!curr_match]) )
      pwts
    })

    # Matches above the `threshold` are linked
    sum_wt <- rowSums(pwts)
    lgk <- (sum_wt >= as.numeric(thresh@start) & sum_wt <= as.numeric(right_point(thresh)))
    if(isTRUE(show_stats)){
      out_b <- cbind(pwts, sum_wt, lgk)
      colnames(out_b) <- c(paste0("prb.", attr_nm),
                           "prb.weight",
                           "prb.threshold")
      return(cbind(out_a, out_b))
    }else{
      return(lgk)
    }
  }

  # Identify identical records to skip repeat checks
  same_rec_func <- function(x, y){
    attr_n <- length(x)
    lgk <- sapply(seq_len(attr_n), function(i){
      lgk <- x[[i]] == y[[i]]
      lgk[is.na(lgk)] <- FALSE
      lgk
    })
    rowSums(lgk) == attr_n
  }

  pids <- links(criteria = "place_holder",
                strata = blocking_attribute,
                sub_criteria = list("cr1" = sub_criteria(c(attribute, u_probs),
                                                         funcs = prob_link,
                                                         equva = same_rec_func)),
                ...)

  # Re-calculate weights for linked records
  if(is.pid(pids)){
    pds <- pids
  }else{
    pds <- pids$pids
  }
  x <- c(attribute, u_probs)
  y <- lapply(x, function(k){
    k[match(pds@link_id, pds@sn)]
  })
  pid_weights <- prob_link(x, y,
                           agree = cmp_threshold,
                           m_pb = m_probability,
                           thresh = weight_threshold,
                           show_stats = TRUE)
  # Mask unlinked records
  pid_weights[pds@pid_cri %in% -1:0,] <- NA_real_
  # Output
  pids <- list(pids = pids,
               pid_weights = pid_weights)
  return(pids)
}

#' @rdname links_probabilistic
#' @export
fetch_scores <- function(attribute, m_probability = .99){
  if(class(attribute) != "list"){
    attribute <- list(attribute)
  }

  u_probs <- lapply(attribute, function(x){
    x_cd <- match(x, x[!duplicated(x)])
    r <- rle(x_cd)
    n <- r$lengths[match(x_cd, r$values)]
    n/length(x_cd)
  })

  if(class(m_probability) != "list"){
    m_probability <- list(m_probability)
  }
  if(length(m_probability) == 1 & length(attribute) > 1){
    m_probability <- rep(m_probability, length(attribute))
  }

  max_thresh <- max(rowSums(sapply(seq_len(length(u_probs)), function(i){
    curr_uprob <- u_probs[[i]]
    curr_mprob <- m_probability[[i]]
    log2(curr_mprob/curr_uprob)
  })))


  min_thresh <- min(rowSums(sapply(seq_len(length(u_probs)), function(i){
    curr_uprob <- u_probs[[i]]
    curr_mprob <- m_probability[[i]]
    log2((1 - curr_mprob)/(1 - curr_uprob))
  })))
  list(minimum_score = min_thresh,
       maximum_score = max_thresh)
}
