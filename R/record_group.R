#' @name links
#' @title Multistage deterministic record linkage
#'
#' @description Link records with matching criteria in ordered stages of relevance.
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
                  shrink = FALSE,
                  schema = "none",
                  ...){
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

  if(err != FALSE) stop(err, call. = FALSE)
  if(class(criteria) != "list") criteria <- list(criteria)

  # Maximum no. of records from all criteria
  ds_len <- as.numeric(lapply(criteria, length))
  ds_len_b <- sapply(sub_criteria, function(x){
    sapply(x, function(x){
      length(x[[1]]) })
  })
  ds_len <- max(c(ds_len, unlist(ds_len_b, use.names = FALSE)))
  rm(ds_len_b)

  err <- err_sn_1(sn = sn, ref_num = ds_len, ref_nm = "criteria")
  if(err != F) stop(err, call. = F)

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
    cri[lgk] <- NA_real_

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
      removed <- length(tag[!duplicated(pid) & !duplicated(pid, fromLast=TRUE)])
      current_tot <- length(tag[tag == 0 & !pid %in% c(sn_ref, NA)])
      cat(paste0(fmt(current_tot), " records(s): ", fmt(current_tot-removed)," linked.\n"))
    }

    tag <- ifelse(pid %in% c(sn_ref, NA), 0, 1)
    link_id[!duplicated(pid) & !duplicated(pid, fromLast = TRUE)] <- sn_ref
    pid[!duplicated(pid) & !duplicated(pid, fromLast = TRUE)] <- sn_ref
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
  if(group_stats == TRUE){
    pids@pid_total = pid_tot
  }

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

  if(schema != "none"){
    if(schema == "by_pid"){
      p_cri <- pids@.Data
      title_seq <- "Pid - P."
    }else if (schema == "by_strata" & !is.null(strata)){
      p_cri <- strata
      title_seq <- "Strata - "
    }else if (schema == "by_ALL"| (schema == "by_strata" & is.null(strata))){
      p_cri <- "ALL"
      title_seq <- ""
    }
    plot_sets <- p_cri[!duplicated(p_cri)]
    plots <- lapply(plot_sets, function(x){
      schema(x = pids[p_cri == x],
             title = paste0(title_seq, x),
             ...)
    })
    names(plots) <- plot_sets
  }

  tm_z <- Sys.time()
  tms <- difftime(tm_z, tm_a)
  tms <- paste0(ifelse(round(tms) == 0, "< 0.01", round(as.numeric(tms), 2)), " ", attr(tms, "units"))

  cat("Records linked in ", tms, "!\n", sep = "")

  if(schema == "none"){
    pids
  }else{
    list("pids" = pids, "plots" = plots)
  }
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
sub_criteria <- function(..., funcs = diyar::exact_match, equva = diyar::exact_match){
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

  if(class(equva) == "NULL"){
    equva <- list(exact_match)
  }else{
    if(class(equva) != "list") equva <- list(equva)

    err <- err_sub_criteria_2(equva)
    if(err != F) stop(err, call. = F)

    err <- err_sub_criteria_3(equva)
    if(err != F) stop(err, call. = F)

    err <- err_sub_criteria_4(..., funcs = equva)
    if(err != F) stop(err, call. = F)
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

  attr(sub_cris, "diyar_sub_criteria") <- T
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
  lg <- overlaps(as.number_line(x@gid), x)
  lg[is.na(lg)] <- F
  if(any(!lg)) {
    rng_i <- paste(head(which(!lg), 5), collapse = ", ", sep="")
    rng_v <- as.character(substitute(x))[!as.character(substitute(x)) %in% c("$","df2")]
    stop(paste0("Range matching error: Actual value (gid) is out of range in ", "[", rng_i, "]"))
  }
  overlaps(as.number_line(x@gid), y)
}
