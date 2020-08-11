# @title Data validations
#
# @description Data validations
#
err_checks_epid <- function(sn, date, case_length, strata, display, episodes_max, episode_unit, data_links,
                            overlap_methods_c, overlap_methods_r, skip_order, custom_sort, group_stats, from_last, data_source,
                            skip_if_b4_lengths, rolls_max, case_for_recurrence,
                            recurrence_from_last, episode_type, recurrence_length){

  if(all(class(sn) == "NULL")) sn <- seq_len(length(date))
  if(all(class(data_source) == "NULL")) data_source <- "`NULL`"
  if(all(class(strata) == "NULL")) strata <- "`NULL`"

  if(all(tolower(episode_type) != "rolling")){
    recurrence_length <- case_length
    recurrence_from_last <- T
    case_for_recurrence <- T
  }else{
    if(all(class(recurrence_length) == "NULL")) recurrence_length <- 0
  }

  # ------- Check for missing args ####
  args <- c("date", "case_length")
  errs <- lapply(as.list(args), function(x){
    m <- try(get(x), silent = T)
    attr(m, "condition")$message
  })

  errs <- unlist(errs, use.names = F)
  if(length(errs) > 0) return(errs[1])

  # ------ Check object classes ####
  args_classes <- list(
    sn = c("numeric", "integer"),
    date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
    case_length = c("list", "numeric", "number_line"),
    recurrence_length = c("list", "numeric", "number_line"),
    episodes_max = c("numeric","integer"),
    rolls_max = c("numeric","integer"),
    display = c("character", "logical"),
    data_source = "character",
    data_links = c("list", "character"),
    overlap_methods_c = c("list", "character"),
    overlap_methods_r = c("list", "character"),
    skip_order = c("numeric","integer"),
    from_last = "logical",
    group_stats = "logical",
  #  include_index_period = "logical",
  #  bi_direction = "logical",
    skip_if_b4_lengths = "logical",
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

  if(is.list(recurrence_length)){
    for (i in 1:length(recurrence_length)) {
      assign(paste0("recurrence_length_",i), recurrence_length[[i]])
      args_classes[[paste0("recurrence_length_",i)]] <- c("numeric","number_line")
      args_classes$recurrence_length <- NULL
    }
  }

  if(is.list(overlap_methods_c)){
    for (i in 1:length(overlap_methods_c)) {
      assign(paste0("overlap_methods_c_",i), overlap_methods_c[[i]])
      args_classes[[paste0("overlap_methods_c_",i)]] <- "character"
      args_classes$overlap_methods_c <- NULL
    }
  }

  if(is.list(overlap_methods_r)){
    for (i in 1:length(overlap_methods_r)) {
      assign(paste0("overlap_methods_r_",i), overlap_methods_r[[i]])
      args_classes[[paste0("overlap_methods_r_",i)]] <- "character"
      args_classes$overlap_methods_r <- NULL
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
                        paste0("Invalid object type for element ", gsub("^case_length_", "", arg), " in `case_length`:\n"),
                        "")
    err_title <- ifelse(grepl("^recurrence_length_", arg),
                        paste0("Invalid object type for element ", gsub("^recurrence_length_", "", arg), " in `recurrence_length`:\n"),
                        err_title)
    err_title <- ifelse(grepl("^data_links_", arg) & err_title == "",
                        paste0("Invalid object type for element ", gsub("^data_links_", "", arg), " in `data_links`:\n"),
                        err_title)
    err_title <- ifelse(grepl("^overlap_methods_c_", arg) & err_title == "",
                        paste0("Invalid object type for element ", gsub("^overlap_methods_c_", "", arg), " in `overlap_methods_c`:\n"),
                        err_title)
    err_title <- ifelse(grepl("^overlap_methods_r_", arg) & err_title == "",
                        paste0("Invalid object type for element ", gsub("^overlap_methods_r_", "", arg), " in `overlap_methods_r`:\n"),
                        err_title)
    err_title <- ifelse(err_title == "",
                        paste0("Invalid object type for `", arg, "`:\n"),
                        err_title)
    hint <- ifelse(grepl("^data_links_", arg),
                   paste0("i - You'll need a ", listr(paste0("`", args_classes, "`"), " or"), " object even if `data_source` is ", class(get(arg)), ".\n"),
                   "")
    errs <-  paste0(err_title,
                    "i - Valid object ", ifelse(length(args_classes)==1, "type is", "types are" ), listr(paste0("`", args_classes, "`"), " or"),".\n",
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

  # ------ Check argument lengths ####
  int <- as.number_line(date)

  lims <- c(1, length(int))
  args_lengths <- list(
    sn = c(0, lims),
    date = lims,
    # case_length = lims,
    recurrence_length = lims,
    strata = c(0, lims),
    data_source = c(0, lims),
    # overlap_methods = lims,
    episodes_max = lims,
    rolls_max = lims,
    from_last = lims,
    group_stats = 1,
    # include_index_period = lims,
    display = 1,
    # bi_direction = lims,
    skip_if_b4_lengths = lims,
    case_for_recurrence = lims,
    recurrence_from_last = lims,
    episode_type = lims
  )

  # if(is.list(case_length)){
  #   for (i in 1:length(case_length)) {
  #     assign(paste0("case_length_",i), case_length[[i]])
  #     args_lengths[[paste0("case_length_",i)]] <- c(lims)
  #     args_lengths$case_length <- NULL
  #   }
  # }

  if(is.list(recurrence_length)){
    for (i in 1:length(recurrence_length)) {
      assign(paste0("recurrence_length_",i), recurrence_length[[i]])
      args_lengths[[paste0("recurrence_length_",i)]] <- c(lims)
      args_lengths$recurrence_length <- NULL
    }
  }

  # if(is.list(overlap_methods)){
  #   for (i in 1:length(overlap_methods)) {
  #     assign(paste0("overlap_methods_",i), overlap_methods[[i]])
  #     args_lengths[[paste0("overlap_methods_",i)]] <- c(lims)
  #     args_lengths$overlap_methods <- NULL
  #   }
  # }

  check_args_lengths <- function(arg, args_lengths){
    err_title <- ""
    # err_title <- ifelse(grepl("^case_length_", arg),
    #                     paste0("Length of element ", gsub("^case_length_", "", arg), " in `case_length`"),
    #                     err_title)
    err_title <- ifelse(grepl("^reccurence_length_", arg),
                        paste0("Length of element ", gsub("^reccurence_length_", "", arg), " in `reccurence_length`"),
                        err_title)
    # err_title <- ifelse(grepl("^overlap_methods_", arg),
    #                     paste0("Length of element ", gsub("^overlap_methods_", "", arg), " in `overlap_methods`"),
    #                     err_title)
    err_title <- ifelse(err_title == "",
                        paste0("Length of `", arg, "`"),
                        err_title)
    err_title <-  paste0(err_title,
                         " must be 1", ifelse(length(args_lengths>1)," or the same as `date`", ""),
                         ":\n")
    errs <-  paste0(err_title,
                    "i - Expecting a length of ", listr(args_lengths[args_lengths!=0], " or"), ".\n",
                    "X - Length is ", length(get(arg)), ".\n")
    ifelse(!length(get(arg)) %in%  args_lengths,
           errs, NA_character_)
  }

  errs <- mapply(check_args_lengths, names(args_lengths), args_lengths)
  errs <- errs[!is.na(errs)]

  if(length(errs) > 0) return(errs[1])

  # ------ Check for argument with missing values ####
  args_missing_vals <- c(
    "sn", "date",
    #"case_length", "recurrence_length",
    #"overlap_methods",
    "episodes_max", "from_last",
    "group_stats", "display",
    #"bi_direction",
    "skip_if_b4_lengths",
    #"include_index_period",
    "case_for_recurrence",
    "recurrence_from_last",
    "rolls_max", "display", "group_stats",
    "custom_sort", "skip_order"
  )

  # if(class(case_length) == "list"){
  #   args_missing_vals <- c(args_missing_vals,
  #                          ls()[grepl("^case_length_",ls())])
  #   args_missing_vals <- args_missing_vals[args_missing_vals != "case_length"]
  # }
  #
  # if(class(recurrence_length) == "list"){
  #   args_missing_vals <- c(args_missing_vals,
  #                          ls()[grepl("^recurrence_length_",ls())])
  #   args_missing_vals <- args_missing_vals[args_missing_vals != "recurrence_length"]
  # }

  # if(class(overlap_methods) == "list"){
  #   args_missing_vals <- c(args_missing_vals,
  #                          ls()[grepl("^overlap_methods_",ls())])
  #   args_missing_vals <- args_missing_vals[args_missing_vals != "overlap_methods"]
  # }


  check_args_missing_vals <- function(x){
    chks <- diyar:::missing_check(get(x))

    err_title <- ""
    # err_title <-  ifelse(grepl("^case_length_", x),
    #                      paste0("Element ", gsub("^case_length_", "", x), " in `case_length` must not contain missing values"),
    #                      err_title)

    err_title <-  ifelse(grepl("^recurrence_length_", x),
                         paste0("Element ", gsub("^recurrence_length_", "", x), " in `recurrence_length` must not contain missing values"),
                         err_title)

    # err_title <-  ifelse(grepl("^overlap_methods_", x),
    #                      paste0("Element ", gsub("^overlap_methods_", "", x), " in `case_length` must not contain missing values"),
    #                      err_title)

    err_title <-  ifelse(err_title=="",
                         paste0("`", x, "` must not contain missing values."),
                         err_title)

    errs <-  paste0(err_title,
                    paste0("\nX - Missing values in ",  chks, "."))

    x <- ifelse(chks==T, NA, errs)
    x
  }

  errs <- unlist(lapply(args_missing_vals, check_args_missing_vals), use.names = F)
  errs <- errs[!is.na(errs)]

  if(length(errs) > 0) return(errs[1])


  # ------ Check for specific requirements ####
  # Episode unit ####
  err <- err_episode_unit_1(episode_unit = episode_unit)
  if(err != F) return(err)

  # Episode type ####
  err <- err_episode_type_1(episode_type = episode_type)
  if(err != F) return(err)

  # Display - ####
  err <- err_display_1(display =  display)
  if(err != F) return(err)

  # Overlap methods ####
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_c)
  if(err != F) return(err)

  err <- err_overlap_methods_1(overlap_methods = overlap_methods_r)
  if(err != F) return(err)

  # SN - ####
  err <- err_sn_1(sn = sn, ref_num = length(date), ref_nm = "date")
  if(err != F) retrun(err)

  # Data links
  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != F) return(err)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != F) return(err)

  err <- err_overlap_methods_2(overlap_methods = overlap_methods_c, lengths = case_length, overlap_methods_nm = "overlap_methods_c", lengths_nm = "case_length")
  if(err != F) return(err)

  err <- err_overlap_methods_2(overlap_methods = overlap_methods_r, lengths = recurrence_length, overlap_methods_nm = "overlap_methods_r", lengths_nm = "recurrence_length")
  if(err != F) return(err)

  # Strata-level options: episodes_max ####
  one_ep_max <- episodes_max[!duplicated(episodes_max)]
  one_ep_max <- length(one_ep_max) == 1
  if(one_ep_max != T){
    sp <- split(episodes_max, strata)
    opts <- lapply(sp, function(x){
      x <- x[!duplicated(x)]
      if(length(x) == 1){
        NA
      }else{
        listr(paste0("\"", x, "\""), lim = 3)
      }
    })

    opts <- unlist(opts, use.names = F)
    names(opts) <- names(sp)
    opts <- opts[!is.na(opts)]

    if(length(opts) > 0){
      errs <- paste0("Values of `episodes_max` must be unique within each `strata`:\n",
                     paste0("X - You've supplied ", listr(paste0(opts, " for ", "\"", names(opts), "\""), lim = 2) ,"."))
      return(errs)
    }
  }

  # Strata-level options: from_last ####
  one_val <- from_last[!duplicated(from_last)]
  one_val <- length(one_val) == 1
  if(one_val != T){
    sp <- split(from_last, strata)
    opts <- lapply(sp, function(x){
      x <- x[!duplicated(x)]
      if(length(x) == 1){
        NA
      }else{
        listr(paste0("\"", x, "\""), lim = 3)
      }
    })

    opts <- unlist(opts, use.names = F)
    names(opts) <- names(sp)
    opts <- opts[!is.na(opts)]

    if(length(opts) > 0){
      errs <- paste0("Values of `from_last` must be unique within each `strata`:\n",
                     paste0("X - You've supplied ", listr(paste0(opts, " for ", "\"", names(opts), "\""), lim = 2) ,"."))
      return(errs)
    }
  }

  return(F)
}


err_sub_criteria_1 <- function(...){
  args <- list(...)

  err <- lapply(args, function(x){
    ifelse(is.atomic(x), NA_character_, class(x))
  })
  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Invalid object type for `sub_criteria`:\n",
           "i - `sub_criteria` must be an `atomic` vector or a `list of `atomic` vectors.\n",
           paste0("X - `sub_criteria` ", names(err),"` is a ", err,".", collapse = "\n"))
  }else{
    F
  }
}

err_sub_criteria_2 <- function(funcs){
  try_txt <- try(lapply(funcs, function(x){}), silent = T)
  if(class(try_txt) == "try-error"){
    attr(try_txt, "condition")$message
  }else{
    F
  }
}

err_sub_criteria_3 <- function(funcs){
  err <- sapply(funcs, is.function)
  names(err) <- 1:length(err)
  err <- err[!err]
  if(length(err) > 0){
    err <- paste0("`funcs` must be functions:\n",
                  "X - `funcs[c(", diyar:::listr(names(err), conj = ",", lim=3), ")]` ",
                  ifelse(length(err ==1), "is not a function.", "are not functions."))
    err
  }else{
    F
  }
}

err_sub_criteria_4 <- function(..., funcs){
  if(!length(funcs) %in% c(0, 1, length(list(...)))){
    stop(paste0("Length of `funcs` must be 1 or the same as `...`:\n"),
         "i - Expecting 1", ifelse(length(list(...)) > 1, paste0(" or ",length(list(...))), ""), ".\n",
         "X - You've supplied ", length(funcs), ".")
  }else{
    F
  }
}

err_sub_criteria_5 <- function(sub_cris, funcs_l){
  err <- lapply(sub_cris, function(x){
    func <- x[[2]]
    vec <- x[[1]]
    try_txt <- try(func(vec[1], vec[1]), silent = T)
    if(class(try_txt) == "try-error"){
      attr(try_txt, "condition")$message
    }else{
      NA
    }
  })

  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]
  if(length(funcs_l) == 1) err <- err[!duplicated(err)]
  if(length(err) > 0){
    err <- paste0("Unable to evaluate `funcs` with `...`:\n",
                  "i - Each `func` must have the following syntax and output.\n",
                  "i - Syntax ~ `func(x, y, ...)`.\n",
                  "i - Output ~ `TRUE` or `FALSE`.\n",
                  paste0("X - Issue with ",
                         ifelse(length(funcs_l) == 1, "`funcs`", paste0("`funcs[", names(err), "]`") ),": ",
                         err, ".", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_6 <- function(sub_cris, funcs_l){
  err <- lapply(sub_cris, function(x){
    func <- x[[2]]
    ifelse(all(c("x", "y") %in% formalArgs(func)), NA_character_, F)
  })

  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid arguments for `funcs`:\n",
                  "i - Each `funcs` must have at lest two arguments named `x` and `y`.\n",
                  paste0("X - ",
                         ifelse(length(funcs_l) == 1, "`funcs`", paste0("`funcs[", names(err), "]`")),": Missing `args` `x` or `y`.", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_7 <- function(sub_cris, funcs_l){
  err <- lapply(sub_cris, function(x){
    func <- x[[2]]
    vec <- x[[1]]
    x <- class(func(vec[1], vec[1]))
    ifelse(x == "logical", NA_character_, x)
  })

  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]
  if(length(funcs_l) == 1) err <- err[!duplicated(err)]
  if(length(err) > 0){
    err <- paste0("Invalid ouput for `funcs`:\n",
                  "i - Each `funcs` must evaluate to `TRUE` or `FALSE`.\n",
                  paste0("X - ",
                         ifelse(length(funcs_l) == 1, "`funcs`", paste0("`funcs[", names(err), "]`")),": returns a ",
                         err, "` object.", collapse = "\n"))
    err
  }else{
    F
  }
}

err_data_links_1 <- function(data_source, data_links){
  # data_links - Each element must be named g or l ####
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

    if(length(nms)>0) errs else F
  }else{
    F
  }
}

err_data_links_2 <- function(data_source, data_links){
  dl_lst <- unlist(data_links, use.names = F)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])

  # data_links - Values must match what's in data_sources ####
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
    errs
  }else{
    F
  }
}

err_criteria_1 <- function(criteria){
  if(class(criteria) != "list") criteria <- list(criteria)

  err <- lapply(criteria, function(x){
    ifelse(is.atomic(x), NA_character_, class(x))
  })
  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Invalid object type for `criteria`:\n",
           "i - `criteria` must be an `atomic` vector or a `list of `atomic` vectors.\n",
           paste0("X - `criteria ", names(err),"` is a ", err,".", collapse = "\n"))
  }else{
    F
  }
}

err_criteria_2 <- function(criteria){
  if(class(criteria) != "list") criteria <- list(criteria)

  err <- lapply(criteria, length)
  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(!min(err) %in% c(1, max(err)) ){
    paste0("Length of each `criteria` must be the same or equal to 1:\n",
           paste0("X - `criteria ", names(err),"` is ", err,".", collapse = "\n"))
  }else{
    F
  }
}

err_criteria_3 <- function(criteria, sub_criteria){
  if(class(criteria) != "list") criteria <- list(criteria)

  err <- as.numeric(lapply(criteria, length))
  err2 <- as.numeric(sapply(sub_criteria, function(x){
    sapply(x, function(x){
      length(x[[1]]) })
    }))
  err <- c(err, unlist(err2, use.names = F))
  #err <- err[!duplicated(err)]

  if(!min(err) %in% c(1, max(err)) ){
    "Length of each `criteria` and `sub_criteria` must be the same or equal to 1"
  }else{
    F
  }
}
err_sub_criteria_3dot_1 <- function(...){
  args <- list(...)
  err <- lapply(args, length)
  err <- unlist(err, use.names = F)
  names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(length(err) != 1 | all(err == 0)){
    paste0("Length of each `sub_criteria` must be the same or equal to 1:\n",
           paste0("X - `sub_criteria ", names(err),"` is ", err,".", collapse = "\n"))
  }else{
    F
  }
}

err_sn_1 <- function(sn, ref_nm, ref_num){
  if(!any(class(sn) %in% c("numeric", "integer", "NULL"))){
    err <- paste0("Invalid object type for `sn`:\n",
                  "i - Valid object type is `numeric.\n",
                  "X - You've supplied a `", class(sn), "` object.")
    return(err)
  }

  if(class(sn) != "NULL"){
    if(length(sn) != ref_num){
      err <- paste0("Length of `sn` must be the same as `", ref_nm, "`:\n",
                    "i - Expecting a length of ", ref_num, ".\n",
                    "X - Length is ", length(sn), ".\n")
      return(err)
    }

    dp_check <- duplicates_check(sn)
    if(dp_check != T){
      err <- paste0("`sn` must be unique values.\n",
                    "X - There are duplicate values in ", dp_check ,".")
      return(err)
    }

    fn_check <- finite_check(sn)
    if(fn_check!=T){
      err <- paste0("`sn` must be finite numeric values:\n",
                    "X - There are non-finite values in ",fn_check)
      return(err)
    }
  }
  return(F)
}

err_episode_unit_1 <- function(episode_unit){
  if(any(!tolower(episode_unit) %in% names(diyar::episode_unit))){
    opts <- episode_unit
    sn <- 1:length(opts)

    opts <- split(sn , opts)
    opts <- opts[!tolower(names(opts)) %in% names(episode_unit)]

    opts <- unlist(lapply(opts, function(x){
      missing_check(ifelse(sn %in% x, NA, T), 2)
    }), use.names = T)

    opts <- paste0("\"", names(opts),"\"", " at ", opts)
    if(length(opts) >3){
      errs <- paste0(paste0(opts[1:3], collapse = ", "), " ...")
    }  else{
      errs <- listr(opts)
    }
    errs <-  paste0("Invalid values for `episode_unit`:\n",
                    "i - Vaild values are ", listr(paste0("\"", names(diyar::episode_unit), "\""), conj = " or"), ".\n",
                    "X - You've supplied ", errs, ".")

    return(errs)
  }else{
    return(F)
  }
}

err_episode_type_1 <- function(episode_type){
  if(any(!tolower(episode_type) %in% c("fixed", "rolling"))){
    opts <- episode_type
    sn <- 1:length(opts)

    opts <- split(sn , opts)
    opts <- opts[!tolower(names(opts)) %in% c("fixed", "rolling")]

    opts <- unlist(lapply(opts, function(x){
      missing_check(ifelse(sn %in% x, NA, T), 2)
    }), use.names = T)

    opts <- paste0("\"", names(opts),"\"", " at ", opts)
    if(length(opts) >3){
      errs <- paste0(paste0(opts[1:3], collapse = ", "), " ...")
      } else{
        errs <- listr(opts)
      }
    errs <-  paste0("Invalid values for `episode_type`:\n",
                    "i - Vaild values are \"fixed\" or \"rolling\".\n",
                    "X - You've supplied ", errs, ".")

    return(errs)
  }

  return(F)
}

err_display_1 <- function(display){
  if(!any(class(display) %in% c("character"))){
    err <- paste0("Invalid object type for `display`:\n",
                  "i - Valid object type is `character.\n",
                  "X - You've supplied a ", listr(paste0("`", class(display), "`"), conj = " or"), " object.")
    return(err)
  }

  if(length(display) != 1){
    err <- paste0("Length of `display` must be 1:\n",
                  "X - Length is ", length(display), ".\n")
    return(err)
  }

  if(any(!tolower(display) %in% c("progress", "stats", "none"))){
    opts <- display
    sn <- 1:length(opts)

    opts <- split(sn , opts)
    opts <- opts[!tolower(names(opts)) %in% c("progress", "stats", "none")]

    opts <- unlist(lapply(opts, function(x){
      missing_check(ifelse(sn %in% x, NA, T), 2)
    }), use.names = T)

    opts <- paste0("\"", names(opts),"\"", " at ", opts)
    if(length(opts) >3) {
      errs <- paste0(paste0(opts[1:3], collapse = ", "), " ...")
    } else {
      errs <- listr(opts)
    }
    errs <-  paste0("Invalid values for `display`:\n",
                    "i - Vaild values are \"progress\", \"stats\" or \"rolling\".\n",
                    "X - You've supplied ", errs, ".")

    return(errs)
  }

  return(F)
}

err_overlap_methods_1 <- function(overlap_methods){
  err <- unlist(lapply(overlap_methods, function(x){
    x <- overlaps_err(x)
    ifelse(length(x) == 0, NA_character_, x)
  }), use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Invalid option for `overlap_methods`\n",
                  "i - Valid options are \"overlap\", \"exact\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\n",
                  "i - Syntax 1 ~ \"aligns_end|exact...\".\n",
                  "i - Syntax 2 ~ include_overlap_method(c(\"aligns_end\", \"exact\")).\n",
                  "i - Syntax 3 ~ exclude_overlap_method(c(\"across\", \"chain\", \"aligns_start\", \"inbetween\")).\n",
                  paste0("X - `overlap_methods` ", names(err),   "`: You've supplied ", err, ".", collapse = "\n"))
    return(err)
  }else{
    return(F)
  }
}

err_overlap_methods_2 <- function(overlap_methods, lengths, overlap_methods_nm, lengths_nm){
  # overlap_methods - Check if there are more overlap methods than there are lengths

  if(class(lengths) != "list") lengths <- list(lengths)
  if(class(overlap_methods) != "list") overlap_methods <- list(overlap_methods)

  if(!length(overlap_methods) %in% c(1, length(lengths))){
    errs <- paste0("`", overlap_methods_nm, "` must have one element or the same number in `", lengths_nm ,"`:\n",
                   "X - `", overlap_methods_nm, "` has ", length(overlap_methods)," ", ifelse(length(overlap_methods) == 1, "element", "elements"), "\n",
                   "X - `", lengths_nm, "` has ", length(lengths)," ", ifelse(length(lengths) == 1, "element", "elements"),".")
    return(errs)
  }else{
    return(F)
  }
}

err_missing_check <- function(x, arg_nm, lim =10){
  err <- missing_check(x, lim = lim)
  if(err != T){
    err <- paste0("`", arg_nm, "` must not contain missing values:\n",
                  "X - Missing values in ",  err, ".")
    return(err)
  }else{
    return(F)
  }
}

err_match_ref_len <- function(arg, ref_nm, ref_len, arg_nm){
  if(!all(class(arg) == "list")){
    arg <- list(arg)
    multi_opts <- F
  }else{
    multi_opts <- T
  }
  err <- unlist(lapply(arg, function(x){
    x <- length(x)
    ifelse(!x %in% c(ref_len), x, NA_real_)
  }), use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Length of ", ifelse(multi_opts, "each element in ", ""), "`", arg_nm, "` must be", ifelse(length(ref_len) > 1, " 1 or", ""), " the same as `", ref_nm, ".`\n",
                  "i - Expecting a length of ", listr(ref_len, conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0(" `", arg_nm, " ", names(err), "`: "), ""), "Length is ", err, ".", collapse = "\n"))
    return(err)
  }

  return(F)
}

err_object_types <- function(arg, arg_nm, obj_types){
  if(!all(class(arg) == "list")){
    arg <- list(arg)
    multi_opts <- F
  }else{
    multi_opts <- T
  }
  err <- unlist(lapply(arg, function(x){
    x <- class(x)
    if(!any(x %in% obj_types)){
      x <- listr(paste0("`", x[!x %in% ob_types], "`"), conj = " or")
    }else{
      x <- NA_character_
    }
    x
  }), use.names = F)
  names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Invalid object type for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, ".`\n",
                  "i - Valid object types are ", listr(paste0("`", obj_types, "`"), conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0(" `", arg_nm, " ", names(err), "`: "), ""), "You've supplied a ", err, " object.", collapse = "\n"))
    return(err)
  }else{
    return(F)
  }
}

err_epids_checks <- function(date,
                             case_length,
                             recurrence_length,
                             episode_type,
                             episode_unit,
                             overlap_methods_c,
                             overlap_methods_r,
                             deduplicate,
                             display,
                             bi_direction,
                             #from_last,
                             include_index_period ,
                             to_s4){
  # Check for required object types
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               overlap_methods_c = overlap_methods_c,
               overlap_methods_r = overlap_methods_r,
               episode_unit = episode_unit,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
               #from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       episode_type = "character",
                       overlap_methods_c = "character",
                       overlap_methods_r = "character",
                       episode_unit = "character",
                       deduplicate = "logical",
                       display = "character",
                       bi_direction = "logical",
                      # from_last = "logical",
                       include_index_period = "logical",
                       case_length = c("integer", "numeric", "number_line"),
                       recurrence_length = c("integer", "numeric", "number_line"),
                       to_s4 = "logical")

  err <- mapply(diyar:::err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(date))
  args <- list(case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               overlap_methods_c = overlap_methods_c,
               overlap_methods_r = overlap_methods_r,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
               #from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  args_lens <- list(episode_type = len_lims,
                    overlap_methods_c = len_lims,
                    overlap_methods_r = len_lims,
                    episode_unit = len_lims,
                    deduplicate = 1,
                    display = 1,
                    bi_direction = len_lims,
                    #from_last = len_lims,
                    include_index_period = 1,
                    case_length = len_lims,
                    recurrence_length = len_lims,
                    to_s4 = 1)

  err <- mapply(diyar:::err_match_ref_len,
                args,
                rep(as.list("date"), length(args_lens)),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  # Check for missing values where they are not permitted
  args <- list(date = date,
               episode_type = episode_type,
               overlap_methods_c = overlap_methods_c,
               overlap_methods_r = overlap_methods_r,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
              # from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  err <- mapply(diyar:::err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  #err <- err_missing_check(from_last, "from_last", 2)

  err <- err_episode_unit_1(episode_unit = episode_unit)
  if(err != F) return(err[1])
  err <- err_episode_type_1(episode_type = episode_type)
  if(err != F) return(err[1])
  err <- err_display_1(display = display)
  if(err != F) return(err[1])
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_c)
  if(err != F) return(err[1])
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_r)
  if(err != F) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_c, lengths = case_length, overlap_methods_nm = "overlap_methods_c", lengths_nm = "case_length")
  if(err != F) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_r, lengths = recurrence_length, overlap_methods_nm = "overlap_methods_r", lengths_nm = "recurrence_length")
  if(err != F) return(err[1])

  return(F)
}
