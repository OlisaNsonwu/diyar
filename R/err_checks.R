# @title Data validations
#
# @description Data validations
#
err_sub_criteria_1 <- function(...){
  args <- list(...)

  err <- lapply(args, function(x){
    ifelse(is.atomic(x), NA_character_, class(x))
  })
  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
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
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!err]
  if(length(err) > 0){
    err <- paste0("`funcs` must be functions:\n",
                  "X - `funcs[c(", listr(names(err), conj = ",", lim=3), ")]` ",
                  ifelse(length(err ==1), "is not a function.", "are not functions."))
    err
  }else{
    F
  }
}

err_sub_criteria_4 <- function(..., funcs){
  if(!length(funcs) %in% c(0, 1, length(list(...)))){
    return(paste0("Length of `funcs` must be 1 or the same as `...`:\n"),
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
    }else if(class(try_txt) != "logical"){
      "Output is not a `logical` object"
      }else{
      NA
    }
  })

  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]
  if(length(funcs_l) == 1) err <- err[!duplicated(err)]
  if(length(err) > 0){
    err <- paste0("Unable to evaluate `funcs` with `...`:\n",
                  "i - Each `func` must have the following syntax and output.\n",
                  "i - Syntax ~ `func(x, y, ...)`.\n",
                  "i - Output ~ `TRUE` or `FALSE`.\n",
                  paste0("X - Issue with ",
                         ifelse(length(funcs_l) == 1, "`funcs`", paste0("`funcs - ", names(err), "`") ),": ",
                         err, ".", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_6 <- function(sub_cris, funcs_l){
  err <- lapply(sub_cris, function(x){
    func <- x[[2]]
    ifelse(all(c("x", "y") %in% methods::formalArgs(func)), NA_character_, F)
  })

  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
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
  if(length(err) > 0) names(err) <- 1:length(err)
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
  dl_lst <- as.character(dl_lst)
  ds_lst <- data_source[!duplicated(data_source)]
  ds_lst <- as.character(ds_lst)
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
                   "i - Valid values are those in `data_source` i.e. ", valid_opts, "\n",
                   "X - You've supplied ", invalid_opts)
    errs
  }else{
    F
  }
}

err_criteria_0 <- function(sub_criteria){
  rut <- attr(sub_criteria, "diyar_sub_criteria")
  if(class(rut) != "NULL"){
    if(rut == T){
      sub_criteria <- list(sub_criteria)
    }
  }

  err <- lapply(sub_criteria, function(x){
    rut <- attr(x, "diyar_sub_criteria")
    ifelse(!is.null(rut), NA_character_, class(x))
  })
  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Each `sub_criteria` must be supplied with `sub_criteria()`:\n",
           paste0("X - `sub_criteria ", names(err),"` was not supplied with `sub_criteria()`.", collapse = "\n"))
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
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Invalid object type for `criteria`:\n",
           "i - `criteria` must be an `atomic` vector or a `list of `atomic` vectors.\n",
           paste0("X - `criteria ", names(err),"` is a ", err, ".", collapse = "\n"))
  }else{
    F
  }
}

err_criteria_2 <- function(criteria){
  if(class(criteria) != "list") criteria <- list(criteria)

  err <- lapply(criteria, length)
  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(!min(err) %in% c(1, max(err)) | min(err) == 0){
    paste0("Length of each `criteria` must be the same or equal to 1:\n",
           paste0("X - `criteria ", names(err),"` is ", err, ".", collapse = "\n"))
  }else{
    F
  }
}

err_criteria_3 <- function(criteria, sub_criteria){
  if(class(criteria) != "list") criteria <- list(criteria)

  rut <- attr(sub_criteria, "diyar_sub_criteria")
  if(class(rut) != "NULL"){
    if(rut == T){
      sub_criteria <- list(sub_criteria)
    }
  }
  err <- as.numeric(lapply(criteria, length))
  err2 <- unlist(lapply(sub_criteria, function(x){
    lapply(x, function(x){
      length(x[[1]]) })
  }), use.names = F)
  err <- err[!duplicated(err)]
  err2 <- err2[!duplicated(err2)]

  if(any(!c(err, err2) %in% c(1, max(c(err, err2))))){
      return(paste0("Length of each `criteria` and `sub_criteria` must be the same or equal to 1:\n",
           "i - Expecting ", listr(unique(c(1, err)), conj = " or"), ".\n",
           "X - ", ifelse(length(err) == 1, "Length", "Lengths")," of `criteria` ", ifelse(length(err) == 1, "is", "are"), " ", listr(sort(err)), ".\n",
           "X - ", ifelse(length(err2) == 1, "Length", "Lengths")," of `sub_criteria` ", ifelse(length(err2) == 1, "is", "are"), " ", listr(sort(err2)), "."))
  }else{
    F
  }
}

err_sub_criteria_3dot_1 <- function(...){
  args <- list(...)
  err <- lapply(args, length)
  err <- unlist(err, use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
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

err_overlap_methods_1 <- function(overlap_methods, overlap_methods_nm){
  if(all(class(overlap_methods) != "list")){
    overlap_methods <- list(overlap_methods)
  }

  err <- lapply(overlap_methods, overlaps_err)
  names(err) <- seq_len(length(err))
  err <- unlist(err, use.names = T)

  if(length(err) > 0){
    err <- paste0("Invalid option for `", overlap_methods_nm, "`\n",
                  "i - Valid options are \"overlap\", \"exact\", \"reverse\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\n",
                  "i - Syntax 1 ~ \"aligns_end|exact...\".\n",
                  "i - Syntax 2 ~ include_overlap_method(c(\"aligns_end\", \"exact\")).\n",
                  "i - Syntax 3 ~ exclude_overlap_method(c(\"across\", \"chain\", \"aligns_start\", \"inbetween\")).\n",
                  paste0("X - `", overlap_methods_nm, " ", names(err),   "`: You've supplied ", err, ".", collapse = "\n"))
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
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Invalid length for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`:\n",
                  ifelse(ref_nm == "", "", paste0("i - Length must be 1 or the same as `", ref_nm, "`.\n")),
                  "i - Expecting a length of ", listr(unique(ref_len), conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0("`", arg_nm, " ", names(err), "`: "), ""), "Length is ", err, ".", collapse = "\n"))
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
      x <- listr(paste0("`", x[!x %in% obj_types], "`"), conj = " or")
    }else{
      x <- NA_character_
    }
    x
  }), use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Invalid object type for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`.\n",
                  "i - Valid object types are ", listr(paste0("`", obj_types, "`"), conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0(" `", arg_nm, " ", names(err), "`: "), ""), "You've supplied a ", err, " object.", collapse = "\n"))
    return(err)
  }else{
    return(F)
  }
}

err_episodes_checks_1 <- function(date,
                             case_length,
                             recurrence_length,
                             episode_type,
                             episode_unit,
                             overlap_methods_c,
                             overlap_methods_r,
                             deduplicate,
                             display,
                             bi_direction,
                             include_index_period ,
                             to_s4){
  # Check for non-atomic vectors
  args <- list(date = date,
               case_length = case_length,
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

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

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

  err <- mapply(err_object_types,
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

  err <- mapply(err_match_ref_len,
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

  err <- mapply(err_missing_check,
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
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_c, "overlap_methods_c")
  if(err != F) return(err[1])
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_r, "overlap_methods_r")
  if(err != F) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_c, lengths = case_length, overlap_methods_nm = "overlap_methods_c", lengths_nm = "case_length")
  if(err != F) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_r, lengths = recurrence_length, overlap_methods_nm = "overlap_methods_r", lengths_nm = "recurrence_length")
  if(err != F) return(err[1])

  return(F)
}


err_episodes_checks_0 <- function(date,
                                  case_length,
                                  recurrence_length,
                                  episode_type,
                                  episode_unit,
                                  overlap_methods_c,
                                  overlap_methods_r,
                                  display,
                                  sn,
                                  episodes_max,
                                  rolls_max,
                                  strata,
                                  skip_if_b4_lengths,
                                  data_source,
                                  data_links,
                                  custom_sort,
                                  skip_order,
                                  recurrence_from_last,
                                  case_for_recurrence,
                                  from_last,
                                  group_stats){


  # Check for non-atomic vectors
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               overlap_methods_c = overlap_methods_c,
               overlap_methods_r = overlap_methods_r,
               display = display,
               strata = strata,
               custom_sort = custom_sort,
               episodes_max = episodes_max,
               rolls_max = rolls_max,
               data_source = data_source,
               data_links = data_links,
               skip_order = skip_order,
               skip_if_b4_lengths = skip_if_b4_lengths,
               recurrence_from_last = recurrence_from_last,
               case_for_recurrence = case_for_recurrence)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               overlap_methods_c = overlap_methods_c,
               overlap_methods_r = overlap_methods_r,
               episode_unit = episode_unit,
               display = display,
               #data_source = data_source,
               episodes_max = episodes_max,
               rolls_max = rolls_max,
               skip_if_b4_lengths = skip_if_b4_lengths,
               data_links = data_links,
               skip_order = skip_order,
               recurrence_from_last = recurrence_from_last,
               case_for_recurrence = case_for_recurrence,
               from_last = from_last,
               group_stats = group_stats)


  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       episode_type = "character",
                       overlap_methods_c = "character",
                       overlap_methods_r = "character",
                       episode_unit = "character",
                       display = "character",
                       case_length = c("integer", "numeric", "number_line"),
                       recurrence_length = c("integer", "numeric", "number_line"),
                       episodes_max = c("numeric", "integer"),
                       rolls_max = c("numeric", "integer"),
                       #data_source = c("character", "NULL"),
                       data_links = c("list", "character"),
                       skip_order = c("numeric", "integer"),
                       skip_if_b4_lengths = "logical",
                       recurrence_from_last = "logical",
                       case_for_recurrence = "logical",
                       from_last = "logical",
                       group_stats = "logical")

  err <- mapply(err_object_types,
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
               display = display,
               strata = strata,
               custom_sort = custom_sort,
               episodes_max = episodes_max,
               rolls_max = rolls_max,
               data_source = data_source,
               #data_links = data_links,
               skip_order = skip_order,
               skip_if_b4_lengths = skip_if_b4_lengths,
               recurrence_from_last = recurrence_from_last,
               case_for_recurrence = case_for_recurrence)

  args_lens <- list(episode_type = len_lims,
                    overlap_methods_c = len_lims,
                    overlap_methods_r = len_lims,
                    episode_unit = len_lims,
                    display = 1,
                    case_length = len_lims,
                    strata = c(0, len_lims),
                    custom_sort = c(0, len_lims),
                    recurrence_length = len_lims,
                    episodes_max = len_lims,
                    rolls_max = len_lims,
                    data_source = c(0, len_lims),
                    #data_links = len_lims,
                    skip_order = len_lims,
                    skip_if_b4_lengths = len_lims,
                    recurrence_from_last = len_lims,
                    case_for_recurrence = len_lims)

  err <- mapply(err_match_ref_len,
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
               display = display,

               episodes_max = episodes_max,
               rolls_max = rolls_max,
               data_source = data_source,
               data_links = data_links,
               skip_order = skip_order,
               skip_if_b4_lengths = skip_if_b4_lengths,
               recurrence_from_last = recurrence_from_last,
               case_for_recurrence = case_for_recurrence)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != F) return(err)
  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != F) return(err)
  err <- err_episode_unit_1(episode_unit = episode_unit)
  if(err != F) return(err)
  err <- err_episode_type_1(episode_type = episode_type)
  if(err != F) return(err)
  err <- err_display_1(display = display)
  if(err != F) return(err)
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_c, "overlap_methods_c")
  if(err != F) return(err)
  err <- err_overlap_methods_1(overlap_methods = overlap_methods_r, "overlap_methods_r")
  if(err != F) return(err)
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_c, lengths = case_length, overlap_methods_nm = "overlap_methods_c", lengths_nm = "case_length")
  if(err != F) return(err)
  err <- err_overlap_methods_2(overlap_methods = overlap_methods_r, lengths = recurrence_length, overlap_methods_nm = "overlap_methods_r", lengths_nm = "recurrence_length")
  if(err != F) return(err)
  err <- err_sn_1(sn = sn, ref_num = length(date), ref_nm = "date")
  if(err != F) return(err)
  err <- err_strata_level_args(from_last, strata, "from_last")
  if(err != F) return(err)
  err <- err_strata_level_args(episodes_max, strata, "episodes_max")
  if(err != F) return(err)
  return(F)
}

err_links_checks_0 <- function(criteria,
                                 sub_criteria,
                                 sn,
                                 strata,
                                 data_source,
                                 data_links,
                                 display,
                                 group_stats,
                                 expand,
                                 shrink){


  # Check for non-atomic vectors
  args <- list(strata = strata,
               data_source = data_source,
               data_links = data_links,
               display = display,
               group_stats = group_stats,
               expand = expand,
               shrink = shrink,
               criteria = criteria)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(#strata = strata,
               data_source = data_source,
               display = display,
               group_stats = group_stats,
               expand = expand,
               shrink = shrink)


  args_classes <- list(display = "character",
                       data_source = c("character", "NULL"),
                       data_links = c("list", "character"),
                       shrink = "logical",
                       expand = "logical",
                       group_stats = "logical")

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  if(class(criteria) != "list") criteria <- list(criteria)
  len_lims <- c(1, length(criteria[[1]]))
  args <- list(data_source = data_source,
               display = display,
               group_stats = group_stats,
               expand = expand,
               shrink = shrink)

  args_lens <- list(data_source = c(0, len_lims),
                    display = 1,
                    group_stats = 1,
                    expand = 1,
                    shrink = 1)

  err <- mapply(err_match_ref_len,
                args,
                rep(as.list("criteria"), length(args_lens)),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = F)
  err <- err[err != F]
  if(length(err) > 0) return(err[1])

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != F) return(err)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != F) return(err)

  if(class(criteria) != "list") criteria <- list(criteria)

  err <- err_criteria_0(sub_criteria)
  if(err != F) return(err)

  err <- err_criteria_1(criteria)
  if(err != F) return(err)

  err <- err_criteria_2(criteria)
  if(err != F) return(err)

  err <- err_criteria_3(criteria, sub_criteria)
  if(err != F) return(err)
  return(F)
}

err_atomic_vectors <- function(arg, arg_nm){
  if(!all(class(arg) == "list")){
    arg <- list(arg)
    multi_opts <- F
  }else{
    multi_opts <- T
  }
  err <- unlist(lapply(arg, is.atomic), use.names = F)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!err]

  if(length(err) > 0){
    err <- paste0(ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`` must be an `atomic` vector.`\n")
    return(err)
  }else{
    return(F)
  }
}

err_invalid_opts <- function(arg_vals, arg_nm, valid_opts){
  errs <- invalid_opts(arg_vals, valid_opts)
  if(length(errs) > 0){
    errs <-  paste0("Invalid values for `", arg_nm, "`:\n",
                    "i - Vaild values are ", listr(paste0("\"", valid_opts, "\""), conj = " or"), ".\n",
                    "X - You've supplied ", errs, ".")
    return(errs)
  }else{
    return(F)
  }
}

err_strata_level_args <- function(arg, strata, arg_nm){
  one_val <- arg[!duplicated(arg)]
  one_val <- length(arg) == 1
  if(one_val != T){
    if(class(strata) == "NULL") strata <- "NULL"
    sp <- split(arg, strata)
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
      errs <- paste0("Unique values of `", arg_nm, "` required in each `strata`:\n",
                     paste0("X - You've supplied ", listr(paste0(opts, " for ", "\"", names(opts), "\""), lim = 2) ,"."))
      return(errs)
    }else{
      return(F)
    }
  }else{
    return(F)
  }
}

