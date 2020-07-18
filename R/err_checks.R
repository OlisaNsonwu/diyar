# @title Data validations
#
# @description Data validations
#

err_checks_epid <- function(sn, date, case_length, strata, display, episodes_max, episode_unit, data_links,
                            overlap_methods, skip_order, custom_sort, group_stats, from_last, data_source,
                            include_index_period, bi_direction, skip_if_b4_lengths, deduplicate,
                            rolls_max, case_for_recurrence, recurrence_from_last, episode_type){

  if(all(class(sn) == "NULL")) sn <- seq_len(length(date))
  if(all(class(data_source) == "NULL")) data_source <- rep("A", length(date))

  # ------- Check for missing args
  args <- c("date", "case_length")
  errs <- lapply(as.list(args), function(x){
    m <- try(get(x), silent = T)
    attr(m, "condition")$message
  })

  errs <- unlist(errs, use.names = F)
  if(length(errs) > 0) return(errs[1])

  # ------ Check object classes
  args_classes <- list(
    sn = c("numeric", "integer"),
    date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
    case_length = c("list", "numeric", "number_line"),
    episodes_max = c("numeric","integer"),
    rolls_max = c("numeric","integer"),
    display = "character",
    episode_unit = "character",
    data_source = "character",
    data_links = c("list", "character"),
    overlap_methods = c("list", "character"),
    skip_order = c("numeric","integer"),
    from_last = "logical",
    group_stats = "logical",
    include_index_period = "logical",
    bi_direction = "logical",
    skip_if_b4_lengths = "logical",
    deduplicate = "logical",
    recurrence_from_last = "logical",
    case_for_recurrence = "logical"
  )

  if(is.list(case_length)){
    for (i in 1:length(case_length)) {
      assign(paste0("case_length_",i), case_length[[i]])
      args_classes[[paste0("case_length_",i)]] <- c("numeric","number_line")
      args_classes$case_length <- NULL
    }
  }

  if(is.list(overlap_methods)){
    for (i in 1:length(case_length)) {
      assign(paste0("overlap_methods_",i), overlap_methods[[i]])
      args_classes[[paste0("overlap_methods_",i)]] <- "character"
      args_classes$overlap_methods <- NULL
    }
  }

  if(is.list(data_links)){
    for (i in 1:length(data_links)) {
      assign(paste0("data_links_",i), data_links[[i]])
      args_classes[[paste0("data_links_",i)]] <- "character"
      args_classes$data_links <- NULL
    }
  }

  check_args_classes <- function(arg, args_classes){
    err_title <- ifelse(grepl("^case_length_", arg),
                        paste0("Incorrect object type for element ", gsub("^case_length_", "", arg), " in `case_length`:\n"),
                        "")
    err_title <- ifelse(grepl("^data_links_", arg) & err_title == "",
                        paste0("Incorrect object type for element ", gsub("^data_links_", "", arg), " in `data_links`:\n"),
                        err_title)
    err_title <- ifelse(grepl("^overlap_methods_", arg) & err_title == "",
                        paste0("Incorrect object type for element ", gsub("^overlap_methods_", "", arg), " in `overlap_methods`:\n"),
                        err_title)
    err_title <- ifelse(err_title == "",
                        paste0("Incorrect object type for `", arg, "`:\n"),
                        err_title)
    hint <- ifelse(grepl("^data_links_", arg),
                   paste0("i - You'll need a ", listr(paste0("`", args_classes, "`"), "or"), " object even if `data_source` is ", class(get(arg)), ".\n"),
                   "")
    errs <-  paste0(err_title,
                   "i - Correct object ", ifelse(length(args_classes)==1, "type is", "types are" ),  ": ", listr(paste0("`", args_classes, "`"), "or"),".\n",
                   hint,
                   "X - You've supplied a ", listr(paste0("`", class(get(arg)), "`")), " object.")

    ifelse(!all(class(get(arg)) %in% args_classes),
           errs, NA_character_)
  }

  errs <- mapply(check_args_classes, names(args_classes), args_classes)
  errs <- errs[!is.na(errs)]

  if(length(errs) > 0){
    # errs <- ifelse(1:length(errs) ==1,
    #        errs,
    #        paste0("Error: ", errs))
    #
    # errs <- paste0(errs, collapse = "\n\n")
    return(errs[1])
  }

  # ------ Check argument lengths
  int <- as.number_line(date)

  lims <- c(1, length(int))
  args_lengths <- list(
    sn = c(0, lims),
    date = lims,
    case_length = lims,
    strata = c(0, lims),
    data_source = c(0, lims),
    overlap_methods = lims,
    episode_unit = lims,
    episodes_max = lims,
    rolls_max = lims,
    from_last = lims,
    group_stats = 1,
    include_index_period = lims,
    display = 1,
    bi_direction = lims,
    skip_if_b4_lengths = lims,
    deduplicate = 1,
    case_for_recurrence = lims,
    recurrence_from_last = lims,
    episode_type = lims
  )

  if(is.list(case_length)){
    for (i in 1:length(case_length)) {
      assign(paste0("case_length_",i), case_length[[i]])
      args_lengths[[paste0("case_length_",i)]] <- c(lims)
      args_lengths$case_length <- NULL
    }
  }

  if(is.list(overlap_methods)){
    for (i in 1:length(overlap_methods)) {
      assign(paste0("overlap_methods_",i), overlap_methods[[i]])
      args_lengths[[paste0("overlap_methods_",i)]] <- c(lims)
      args_lengths$overlap_methods <- NULL
    }
  }

  check_args_lengths <- function(arg, args_lengths){
    err_title <- ifelse(grepl("^case_length_", arg),
                        paste0("Length of element ", gsub("^case_length_", "", arg), " in `case_length`"),
                        "")
    err_title <- ifelse(grepl("^overlap_methods_", arg),
                        paste0("Length of element ", gsub("^overlap_methods_", "", arg), " in `overlap_methods`"),
                        err_title)
    err_title <- ifelse(err_title == "",
                        paste0("Length of `", arg, "`"),
                        err_title)
    err_title <-  paste0(err_title,
                         " must be 1", ifelse(length(args_lengths>1)," or the same as `date`", ""),
                         ":\n")
    errs <-  paste0(err_title,
                   "i - Expecting a length of ", listr(args_lengths[args_lengths!=0], "or"), ".\n",
                   "X - Length is ", length(get(arg)), ".\n")
    ifelse(!length(get(arg)) %in%  args_lengths,
           errs, NA_character_)
  }

  errs <- mapply(check_args_lengths, names(args_lengths), args_lengths)
  errs <- errs[!is.na(errs)]

  if(length(errs) > 0) return(errs[1])

  # ------ Check for argument with missing values
  args_missing_vals <- c(
    "sn", "date", "case_length" ,
    "overlap_methods", "episode_unit",
    "episodes_max", "from_last",
    "group_stats", "display",
    "bi_direction", "skip_if_b4_lengths",
    "include_index_period"
  )

  if(class(case_length) == "list"){
    args_missing_vals <- c(args_missing_vals,
                           ls()[grepl("^case_length_",ls())])
    args_missing_vals <- args_missing_vals[args_missing_vals != "case_length"]
  }

  if(class(overlap_methods) == "list"){
    args_missing_vals <- c(args_missing_vals,
                           ls()[grepl("^overlap_methods_",ls())])
    args_missing_vals <- args_missing_vals[args_missing_vals != "overlap_methods"]
  }


  check_args_missing_vals <- function(x){
    chks <- diyar:::missing_check(get(x))

    err_title <-  ifelse(class(get(x))=="logical",
                   paste0("`", x, "` must be `TRUE` or `FALSE`."),
                   "")

    err_title <-  ifelse(grepl("^case_length_", x),
                         paste0("Element ", gsub("^case_length_", "", x), " in `case_length` must not contain missing values"),
                         err_title)

    err_title <-  ifelse(grepl("^overlap_methods_", x),
                         paste0("Element ", gsub("^overlap_methods_", "", x), " in `case_length` must not contain missing values"),
                         err_title)

    err_title <-  ifelse(err_title=="",
                         paste0("`", x, "` must not contain missing values."),
                         err_title)

    errs <-  paste0(err_title,
                   ifelse(class(get(x))=="logical",
                          "",
                          paste0("\nX - Missing values in ",  chks, ".")))

    x <- ifelse(chks==T, NA, errs)
    x
  }

  errs <- unlist(lapply(args_missing_vals, check_args_missing_vals), use.names = F)
  errs <- errs[!is.na(errs)]

  if(length(errs) > 0) return(errs[1])


  # ------ Check for specific requirements
  # Episode unit - Specific options required
  if(any(!tolower(episode_unit) %in% names(diyar::episode_unit))){
    epu <- episode_unit
    sn <- 1:length(epu)

    units <- split(sn , epu)
    units <- units[!tolower(names(units)) %in% names(episode_unit)]

    units <- unlist(lapply(units, function(x){
      diyar:::missing_check(ifelse(sn %in% x, NA, T), 2)
    }), use.names = T)

    units <- paste0("\"", names(units),"\"", " at ", units)
    if(length(units) >3) err <- paste0(paste0(units[1:3], collapse = ", "), " ...") else err <- diyar:::listr(units)
    errs <-  paste0("Invalid values for `episode_unit`:\n",
                    "i - Vaild values are: \"seconds\", \"minutes\", \"hours\", \"days\", \"weeks\", \"months\" or \"years\".\n",
                    "X - You've supplied ", errs, ".")

    return(errs)
  }

  # Display - Specific options required
  if(!tolower(display) %in% c("stats","progress")){
    return(paste0("Invalid values for `display`:\n",
                  "i - Vaild values are: \"progress\" or \"stats\".\n",
                  "X - You've supplied ", paste0("\"" , display, "\"", collapse = ", ")))
  }

  # Overlap methods - Specific options required
  errs <- lapply(overlap_methods, function(mths){
    o <- unique(unlist(strsplit(mths[!duplicated(mths)], split="\\|")))
    o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween", "overlap", "none")]

    if(length(o) == 0){
      NA_character_
    } else if (length(o) > 5){
      paste0(paste0("\"", o, "\"", collapse = ", "), " ...")
    }else{
      diyar:::listr(paste0("\"",o,"\""))
    }
  })

  errs <- unlist(errs, use.names = F)
  if(class(overlap_methods)=="list") names(errs) <- 1:length(overlap_methods)
  errs <- errs[!is.na(errs)]

  if (length(errs)> 0) {
    errs <- lapply(list(errs), function(x){
      paste0("Invalid option for ", ifelse(class(overlap_methods)=="list", paste0("element ", names(x), " of "), ""), "`overlap_method`:\n",
             "i - Valid options are; \"overlap\", \"exact\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\n",
             "i - Syntax 1 ~ \"method1|method2|method3...\".\n",
             "i - Syntax 2 ~ use `include_overlap_method()` or `exclude_overlap_method()`.\n",
             "X - You've supplied ", x, ".\n")
    })
    errs <- unlist(errs, use.names = F)
    return(errs[1])
  }

  # SN - Must be unique
  if(!is.null(sn)){
    dp_check <- duplicates_check(sn)
    if(dp_check!=T) return(paste0("`sn` must be have unique values.\n",
                                "X - Duplicate values in ", dp_check ,"."))
    # fn_check <- finite_check(sn)
    # if(fn_check!=T) return(paste0("`sn` must have finite numeric values:\n",
    #                             "X - There are non-finite values in ",fn_check))
  }

  # data_links - Each element must be named g or l
  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])

  if(is.list(data_links) & !is.null(data_source)){
    nms <- names(data_links); names(nms) <- 1:length(nms)
    nms <- nms[!tolower(nms) %in% c("l","g","") ]
    nms

    invalid_opts <- ifelse(length(nms) > 2,
                           paste0(paste0("\"", head(nms, 2), "\"", " in [",head(names(nms),2),"]", collapse = ", "), " ..."),
                           paste0(listr(paste0("\"", nms, "\"", " in [", names(nms),"]")), "."))

    err_title <- "Invalid element names for `data_links`:\n"
    errs <- paste0(err_title,
                   "i - Each element in `data_link` (list) must be named \"l\" (links) or \"g\" (groups).\n",
                   "i - Syntax 1 - `data_links` <- list(l = c(\"DS1\", \"DS2\"), g = c(\"DS3\", \"DS4\"))\n",
                   "i - Syntax 2 - `data_links` <- c(\"l\" = \"DS5\")\n",
                   "i - \"l\" - Only return episodes with records from \"DS1\" AND \"DS2\" `data_source`.`\n",
                   "i - \"g\" - Only return episodes with records from \"DS3\" OR  \"DS4\" `data_source`.`\n",
                   paste0("X - You've supplied ", invalid_opts))

    if(length(nms)>0) return(errs)
  }

  # data_links - Vaues must match what's in data_sources
  if(length(ms_lst)>0 & !all(toupper(dl_lst)=="ANY") & !is.null(data_source)){
    invalid_opts <- ifelse(length(ms_lst) > 4,
                           paste0(paste0("\"", head(ms_lst,4), "\"", collapse = ", "), " ..."),
                           paste0(listr(paste0("\"", ms_lst,"\"")), "."))

    valid_opts <- ifelse(length(ds_lst) > 4,
                         paste0(paste0("\"", head(ds_lst, 4), "\"", collapse = ", "), " ..."),
                         paste0(listr(paste0("\"", ds_lst, "\"")), "."))

    err_title <- "Invalid values for `data_links`:\n"
    errs <- paste0(err_title,
                   "i - Valid values are those in `data_source`: ", valid_opts, "\n",
                   "X - You've supplied ", invalid_opts)
    return(errs)
  }

  # overlap_methods - Check if there are more overlap methods than their are case_lengths
  cl <- case_length
  om <- overlap_methods

  if(class(cl) != "list") cl <- list(cl)
  if(class(cl) != "list") om <- list(om)

  if(!length(om) %in% c(1, length(cl)) ){
    err_title <- "The number of elements in `overlap_methods` must be 1 or the same as `case_length`:\n"
    errs <- paste0(err_title,
           paste0("i - Expecting 1", ifelse(length(cl) >1, paste0(" or ", length(cl)), ""),".\n"),
           paste0("X - Found ", length(om),"."))
    return(errs)
  }

  return(F)
}
