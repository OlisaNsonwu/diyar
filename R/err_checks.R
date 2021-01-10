# @title Data validations
#
# @description Data validations
#
err_sub_criteria_1 <- function(..., cri_nm = "sub_criteria"){
  args <- list(...)
  args <- unlist(args, recursive = FALSE)
  err <- lapply(args, function(x){
    ifelse(is.atomic(x), NA_character_, class(x))
  })
  err <- unlist(err, use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Invalid object type for `", cri_nm, "`:\n",
           "i - `", cri_nm, "` must be an `atomic` vector or a `list of `atomic` vectors.\n",
           paste0("X - `", cri_nm, "` ", names(err),"` is a ", err,".", collapse = "\n"))
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
    err <- paste0("`funcs` must be a fuction or a `list` of functions:\n",
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

err_sub_criteria_6.0 <- function(sub_cris, funcs_l = "funcs", funcs_pos = 2, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[funcs_pos]]
      vec <- l2[[1]]
      if(class(vec) == "list"){
        vec <- lapply(vec, function(x) x[1:5])
      }else{
        vec <- vec[1:5]
      }
      try_txt <- try(func(x = vec, y = vec), silent = T)
      if(class(try_txt) == "try-error"){
        attr(try_txt, "condition")$message
      }else if(class(try_txt) != "logical"){
        "Output is not a `logical` object"
      }else{
        NA
      }
    })
    y <- unlist(y)
    names(y) <- paste0("`", funcs_l, "` ", seq_len(length(y)), " of ", toupper(names(sub_cris)[i]))
    y
  })

  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Unable to evaluate `", funcs_l, "` in `", cri_nm, "`:\n",
                  "i - Each `", funcs_l, "` must have the following syntax and output.\n",
                  "i - Syntax ~ `function(x, y, ...)`.\n",
                  "i - Output ~ `TRUE` or `FALSE`.\n",
                  paste0("X - Issues with ", names(err) ,": ",
                         err, ".", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_6.1 <- function(sub_cris, funcs_l, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[2]]
      vec <- l2[[1]]
      try_txt <- try(func(x = vec), silent = T)
      if(class(try_txt) == "try-error"){
        attr(try_txt, "condition")$message
      }else if(class(try_txt) != "logical"){
        "Output is not a `logical` object"
      }else{
        NA
      }
    })
    y <- unlist(y)
    names(y) <- paste0("`funcs` ", seq_len(length(y)), " of ", toupper(names(sub_cris)[i]))
    y
  })

  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Unable to evaluate `funcs` in `", cri_nm, "`:\n",
                  "i - Each `func` must have the following syntax and output.\n",
                  "i - Syntax ~ `function(x, ...)`.\n",
                  "i - Output ~ `TRUE` or `FALSE`.\n",
                  paste0("X - Issues with ", names(err) ,": ",
                         err, ".", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_5.0 <- function(sub_cris, funcs_l = "funcs", funcs_pos = 2, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[funcs_pos]]
      ifelse(all(c("x", "y") %in% methods::formalArgs(func)), NA_character_, F)
    })
    y <- unlist(y)
    names(y) <- paste0("`", funcs_l,"` ", seq_len(length(y)), " of ", toupper(names(sub_cris)[i]))
    y
  })


  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid arguments for `", funcs_l, "` in `", cri_nm, "`:\n",
                  "i - Each `", funcs_l, "` must have at least two arguments named `x` and `y`.\n",
                  paste0("X - ", names(err),": Missing `args` `x` or `y`.", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_5.1 <- function(sub_cris, funcs_l, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[2]]
      ifelse(all(c("x") %in% methods::formalArgs(func)), NA_character_, F)
    })
    y <- unlist(y)
    names(y) <- paste0("`funcs` ", seq_len(length(y)), " of ", toupper(names(sub_cris)[i]))
    y
  })


  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid arguments for `funcs` in `", cri_nm, "`:\n",
                  "i - Each `funcs` must have at least one argument named `x`.\n",
                  paste0("X - ", names(err),": Missing `args` `x`", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_7 <- function(sub_cris, funcs_l = "funcs", funcs_pos = 2, cri_nm = "case_sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[funcs_pos]]
      vec <- l2[[1]]
      if(class(vec) == "list"){
        vec <- lapply(vec, function(x) x[1:5])
      }else{
        vec <- vec[1:5]
      }
      e <- try(func(vec, vec),
               silent = TRUE)
      x <- class(e)
      ifelse(x == "logical" & length(e) == 5, NA_character_, paste0("Issue with ",
                                                                    "`", funcs_l, "-", j, "` of ",
                                                                    "`", cri_nm, "-", i, "`: ",
                                                                    gsub("\\n", "", e),"."))
    })
    y
  })

  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid ouput for `", funcs_l, "`:\n",
                  "i - Each `", funcs_l, "` output must have a length of 1 or the same length as `...`.\n",
                  "i - Each `", funcs_l, "` output must evaluate to `TRUE` or `FALSE`.\n",
                  paste0("X - ", err, collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_9 <- function(sub_cris, funcs_l, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[2]]
      vec <- l2[[1]]
      e <- func(vec)
      x <- class(e)
    ifelse(x == "logical" & length(e) == 1, NA_character_, x)
    })
    y <- unlist(y)
    names(y) <- paste0("`funcs` ", seq_len(length(y)), " of ", toupper(names(sub_cris)[i]))
    y
  })

  err <- unlist(err, use.names = FALSE)
  err <- err[!is.na(err)]
  if(length(err) > 0){
    err <- paste0("Invalid ouput for `funcs` in `", cri_nm, "` :\n",
                  "i - Output of each `funcs` must have a length of 1.\n",
                  "i - Output of each `funcs` must evaluate to `TRUE` or `FALSE`.\n",
                  paste0("X - ", names(err),": returns a ", err, "` object.", collapse = "\n"))
    err
  }else{
    F
  }
}

err_sub_criteria_8 <- function(x, cri_nm = "sub_criteria"){
  rut <- attr(x, "diyar_sub_criteria")
  if(class(rut) != "NULL"){
    return(FALSE)
  }else{
    if(class(x) != "list"){
      x <- list(x)
    }
    err <- lapply(x, function(x){
      rut <- attr(x, "diyar_sub_criteria")
      if(class(rut) != "NULL"){
        TRUE
      }else{
        FALSE
      }
    })
    err_1 <- as.logical(err)
    names(err_1) <- 1:length(err_1)
  }

  if(!all(err_1)){
    err_lst <- err_1[!err_1]
    err <- paste0("Invalid input for `", cri_nm, "`:\n",
                  "i - Each `", cri_nm, "` must be created by `", cri_nm, "()`.\n",
                  paste0("X - ",
                         ifelse(rep(length(err_lst) == 1, length(err_lst)), paste0("`", cri_nm, "`"), paste0("`", cri_nm,"[", names(err_lst), "]`")),": was not created with `",cri_nm,"()`.",
                         collapse = "\n"))
    err
  }else{
    F
  }
}

err_data_links_1 <- function(data_source, data_links){
  # data_links - Each element must be named g or l ####
  dl_lst <- unlist(data_links, use.names = FALSE)
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
  dl_lst <- unlist(data_links, use.names = FALSE)
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

err_sub_criteria_0 <- function(sub_criteria){
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
  err <- unlist(err, use.names = FALSE)
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
  err <- unlist(err, use.names = FALSE)
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
  err <- unlist(err, use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(!min(err) %in% c(1, max(err)) | min(err) == 0){
    paste0("Length of each `criteria` must be the same or equal to 1:\n",
           paste0("X - `criteria ", names(err),"` is ", err, ".", collapse = "\n"))
  }else{
    F
  }
}

err_sub_criteria_10 <- function(ref_arg, sub_criteria, arg_nm = "criteria", cri_nm = "sub_criteria"){
  if(class(ref_arg) != "list") ref_arg <- list(ref_arg)

  rut <- attr(sub_criteria, "diyar_sub_criteria")
  if(class(rut) != "NULL"){
    if(rut == T){
      sub_criteria <- list(sub_criteria)
    }
  }
  err <- as.numeric(lapply(ref_arg, length))
  err2 <- unlist(lapply(sub_criteria, function(x){
    lapply(x, function(x){
      length(x[[1]]) })
  }), use.names = FALSE)
  err <- err[!duplicated(err)]
  err2 <- err2[!duplicated(err2)]

  if(any(!c(err, err2) %in% c(1, max(c(err, err2))))){
      return(paste0("Length of each `", arg_nm, "` and `",cri_nm, "` must be the same or equal to 1:\n",
           "i - Expecting ", listr(unique(c(1, err)), conj = " or"), ".\n",
           "X - ", ifelse(length(err) == 1, "Length", "Lengths")," of `", arg_nm, "` ", ifelse(length(err) == 1, "is", "are"), " ", listr(sort(err)), ".\n",
           "X - ", ifelse(length(err2) == 1, "Length", "Lengths")," of `", cri_nm, "` ", ifelse(length(err2) == 1, "is", "are"), " ", listr(sort(err2)), "."))
  }else{
    F
  }
}

err_sub_criteria_3dot_1 <- function(..., cri_nm = "sub_criteria"){
  args <- list(...)
  err <- lapply(args, length)
  err <- unlist(err, use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(length(err) != 1 | all(err == 0)){
    paste0("Length of each `", cri_nm, "` must be the same or equal to 1:\n",
           paste0("X - `", cri_nm, "` ", names(err),"` is ", err,".", collapse = "\n"))
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
  return(FALSE)
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
    return(FALSE)
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

  return(FALSE)
}

err_by_1 <- function(by){
  if(any( by <= 0)){
    by[by <= 0] <- NA_real_
    err <- missing_check(by)

    if(err != FALSE){
      err <-  paste0("Invalid values for `by`:\n",
                      "i - `by' must be greater than 0.\n",
                      "X - `by' is less than 1 at ", err, ".")
      return(err)
    }

  }

  return(FALSE)
}
err_by_2 <- function(by, x){
  lgk <- (x@.Data > 0 & by < 0) | (x@.Data < 0 & by > 0)
  if(any(lgk)){
    by[lgk] <- NA_real_
    err <- missing_check(by)

    if(err != FALSE){
      err <-  paste0("Incorrect sign for `by`:\n",
                     "i - \"+\" and \"-\" integers are required for \"increasing\" and \"decreasing\" `number_lines' respectively.\n",
                     "X - Incorrect sign at ", err, ".")
      return(err)
    }

  }

  return(FALSE)
}

err_lnt_out_1 <- function(length.out){
  if(any( length.out  <= 0)){
    length.out [length.out  <= 0] <- NA_real_
    err <- missing_check(length.out )

    if(err != FALSE){
      errs <-  paste0("Invalid values for `length.out `:\n",
                      "i - `length.out' must be greater than 0.\n",
                      "X - `length.out' is less than 1 at ", errs, ".")
      return(errs)
    }

  }

  return(FALSE)
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

  return(FALSE)
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
    return(FALSE)
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
    return(FALSE)
  }
}

err_missing_check <- function(x, arg_nm, lim =10){
  err <- missing_check(x, lim = lim)
  if(err != T){
    err <- paste0("`", arg_nm, "` must not contain missing values:\n",
                  "X - Missing values in ",  err, ".")
    return(err)
  }else{
    return(FALSE)
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
  }), use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    err <- paste0("Invalid length for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`:\n",
                  ifelse(ref_nm == "", "", paste0("i - Length must be 1 or the same as `", ref_nm, "`.\n")),
                  "i - Expecting a length of ", listr(unique(ref_len), conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0("`", arg_nm, " ", names(err), "`: "), ""), "Length is ", err, ".", collapse = "\n"))
    return(err)
  }

  return(FALSE)
}

err_object_types <- function(arg, arg_nm, obj_types){
  if(!all(class(arg) == "list") | (all(class(arg) == "list") & !"list" %in% obj_types)){
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
  }), use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    obj_types <- ifelse(obj_types == "logical", "logical (`TRUE` or `FALSE`)", obj_types)
    err <- paste0("Invalid object type for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`.\n",
                  "i - Valid object types are ", listr(paste0("`", obj_types, "`"), conj = " or"), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, length(err)), paste0(" `", arg_nm, " ", names(err), "`: "), ""), "You've supplied a ", err, " object.", collapse = "\n"))
    return(err)
  }else{
    return(FALSE)
  }
}

err_episodes_checks_1 <- function(date,
                             case_length,
                             recurrence_length,
                             episode_type,
                             episode_unit,
                             case_overlap_methods,
                             recurrence_overlap_methods,
                             deduplicate,
                             display,
                             bi_direction,
                             include_index_period,
                             to_s4){
  # Check for non-atomic vectors
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
               #from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               episode_unit = episode_unit,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
               #from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       episode_type = "character",
                       case_overlap_methods = c("list", "character"),
                       recurrence_overlap_methods = c("list", "character"),
                       episode_unit = "character",
                       deduplicate = "logical",
                       display = "character",
                       bi_direction = "logical",
                      # from_last = "logical",
                       include_index_period = "logical",
                       case_length = c("list", "integer", "numeric", "number_line"),
                       recurrence_length = c("list", "integer", "numeric", "number_line"),
                       to_s4 = "logical")

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(date))
  args <- list(case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
               #from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  args_lens <- list(episode_type = len_lims,
                    case_overlap_methods = len_lims,
                    recurrence_overlap_methods = len_lims,
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
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for missing values where they are not permitted
  args <- list(episode_type = episode_type,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               deduplicate = deduplicate,
               display = display,
               bi_direction = bi_direction,
              # from_last = from_last,
               include_index_period = include_index_period,
               to_s4 = to_s4)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  #err <- err_missing_check(from_last, "from_last", 2)

  err <- err_episode_unit_1(episode_unit = episode_unit)
  if(err != FALSE) return(err[1])
  err <- err_episode_type_1(episode_type = episode_type)
  if(err != FALSE) return(err[1])
  err <- err_display_1(display = display)
  if(err != FALSE) return(err[1])
  err <- err_overlap_methods_1(overlap_methods = case_overlap_methods, "case_overlap_methods")
  if(err != FALSE) return(err[1])
  err <- err_overlap_methods_1(overlap_methods = recurrence_overlap_methods, "recurrence_overlap_methods")
  if(err != FALSE) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = case_overlap_methods, lengths = case_length, overlap_methods_nm = "case_overlap_methods", lengths_nm = "case_length")
  if(err != FALSE) return(err[1])
  err <- err_overlap_methods_2(overlap_methods = recurrence_overlap_methods, lengths = recurrence_length, overlap_methods_nm = "recurrence_overlap_methods", lengths_nm = "recurrence_length")
  if(err != FALSE) return(err[1])

  return(FALSE)
}

err_split_nl_1 <- function(x,
                           by,
                           length.out,
                           fill,
                           simplify){
  # Check for non-atomic vectors
  args <- list(x = x,
               by = by,
               length.out = length.out,
               fill = fill,
               simplify = simplify)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(x = x,
               by = by,
               length.out = length.out,
               fill = fill,
               simplify = simplify)

  args_classes <- list(x = c("number_line"),
               by = c("integer", "numeric", "NULL"),
               length.out = c("integer", "numeric", "NULL"),
               fill = "logical",
               simplify = "logical")

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(x))
  args <- list(by = by,
               length.out = length.out,
               fill = fill,
               simplify = simplify)

  args_lens <- list(by = c(len_lims, 0),
                    length.out = c(len_lims, 0),
                    fill = len_lims,
                    simplify = len_lims)

  err <- mapply(err_match_ref_len,
                args,
                rep(as.list("date"), length(args_lens)),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for missing values where they are not permitted
  args <- list(by = by,
               length.out = length.out,
               fill = fill,
               simplify = simplify)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_by_2(by, x)
  if(err != FALSE) return(err[1])
  err <- err_lnt_out_1(length.out)
  if(err != FALSE) return(err[1])

  errs_l <- finite_check(x@start)
  errs_r <- finite_check(x@.Data)

  if(errs_l != T | errs_r != T){
    errs <- paste0("`x` must have finite left and right points:\n",
                   ifelse(errs_l != T, paste0("X - There are non-finite values in `left_point(x)", errs_l, "`.\n"), ""),
                   ifelse(errs_r != T, paste0("X - There are non-finite values in `right_point(x)", errs_r, "`."), ""))
    return(errs)
  }

  fn_check <- finite_check(by)
  if(fn_check!=T) return(paste0("`by` must have finite values:\n",
                              paste0("X - There are non-finite values in `by",fn_check, "`.")))

  return(FALSE)
}

err_episodes_checks_0 <- function(date = 1,
                                  case_length = 1,
                                  recurrence_length = 1,
                                  episode_type = "fixed",
                                  episode_unit = "days",
                                  case_overlap_methods = "overlap",
                                  recurrence_overlap_methods = "overlap",
                                  display = "none",
                                  sn = seq_len(length(date)),
                                  episodes_max = 1,
                                  rolls_max = 1,
                                  strata = 1,
                                  skip_if_b4_lengths = TRUE,
                                  data_source = 1,
                                  data_links = "ANY",
                                  custom_sort = rep(1, length(date)),
                                  skip_order = 1,
                                  recurrence_from_last = TRUE,
                                  case_for_recurrence = TRUE,
                                  from_last = TRUE,
                                  group_stats = TRUE,
                                  case_sub_criteria = NULL,
                                  recurrence_sub_criteria = NULL,
                                  schema = "none",
                                  case_length_total = 1,
                                  recurrence_length_total = 1){


  # Check for non-atomic vectors
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
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
               case_for_recurrence = case_for_recurrence,
               schema = schema,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
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
               group_stats = group_stats,
               schema = schema,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total)


  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       episode_type = "character",
                       case_overlap_methods = c("list", "character"),
                       recurrence_overlap_methods = c("list", "character"),
                       episode_unit = "character",
                       display = "character",
                       case_length = c("list", "integer", "numeric", "number_line"),
                       recurrence_length = c("list", "integer", "numeric", "number_line"),
                       episodes_max = c("numeric", "integer"),
                       rolls_max = c("numeric", "integer"),
                       #data_source = c("character", "NULL"),
                       data_links = c("list", "character"),
                       skip_order = c("numeric", "integer"),
                       skip_if_b4_lengths = "logical",
                       recurrence_from_last = "logical",
                       case_for_recurrence = "logical",
                       from_last = "logical",
                       group_stats = "logical",
                       schema =  "character",
                       case_length_total = c("numeric", "integer", "number_line"),
                       recurrence_length_total = c("numeric", "integer", "number_line"))

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(date))
  args <- list(case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               episode_unit= episode_unit,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
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
               case_for_recurrence = case_for_recurrence,
               schema = schema,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total)

  args_lens <- list(episode_type = len_lims,
                    case_overlap_methods = len_lims,
                    recurrence_overlap_methods = len_lims,
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
                    case_for_recurrence = len_lims,
                    schema = 1,
                    case_length_total = len_lims,
                    recurrence_length_total = len_lims)

  err <- mapply(err_match_ref_len,
                args,
                rep(as.list("date"), length(args_lens)),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for missing values where they are not permitted
  args <- list(episode_type = episode_type,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               display = display,
               schema = schema,

               episodes_max = episodes_max,
               rolls_max = rolls_max,
               data_source = data_source,
               data_links = data_links,
               skip_order = skip_order,
               skip_if_b4_lengths = skip_if_b4_lengths,
               recurrence_from_last = recurrence_from_last,
               case_for_recurrence = case_for_recurrence,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)
  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)
  err <- err_episode_unit_1(episode_unit = episode_unit)
  if(err != FALSE) return(err)
  err <- err_episode_type_1(episode_type = episode_type)
  if(err != FALSE) return(err)
  err <- err_display_1(display = display)
  if(err != FALSE) return(err)
  err <- err_overlap_methods_1(overlap_methods = case_overlap_methods, "case_overlap_methods")
  if(err != FALSE) return(err)
  err <- err_overlap_methods_1(overlap_methods = recurrence_overlap_methods, "recurrence_overlap_methods")
  if(err != FALSE) return(err)
  err <- err_overlap_methods_2(overlap_methods = case_overlap_methods, lengths = case_length, overlap_methods_nm = "case_overlap_methods", lengths_nm = "case_length")
  if(err != FALSE) return(err)
  err <- err_overlap_methods_2(overlap_methods = recurrence_overlap_methods, lengths = recurrence_length, overlap_methods_nm = "recurrence_overlap_methods", lengths_nm = "recurrence_length")
  if(err != FALSE) return(err)
  err <- err_sn_1(sn = sn, ref_num = length(date), ref_nm = "date")
  if(err != FALSE) return(err)
  err <- err_strata_level_args(from_last, strata, "from_last")
  if(err != FALSE) return(err)
  err <- err_strata_level_args(episodes_max, strata, "episodes_max")
  if(err != FALSE) return(err)

  if(class(case_sub_criteria) != "NULL"){
    err <- err_sub_criteria_8(case_sub_criteria, cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err[1])
    err <- err_sub_criteria_10(date, case_sub_criteria, "date", cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(case_sub_criteria, cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(case_sub_criteria, funcs_l = "equva", funcs_pos = 3, cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(case_sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(case_sub_criteria, funcs_l = "equva", funcs_pos = 3, cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(case_sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(case_sub_criteria, funcs_l = "equva", funcs_pos = 3)
    if(err != FALSE) return(err)
  }

  if(class(recurrence_sub_criteria) != "NULL"){
    err <- err_sub_criteria_8(recurrence_sub_criteria, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err[1])
    err <- err_sub_criteria_10(date, recurrence_sub_criteria, "date", cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(recurrence_sub_criteria, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(recurrence_sub_criteria, funcs_l = "equva", funcs_pos = 3, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(recurrence_sub_criteria, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(recurrence_sub_criteria, funcs_l = "equva", funcs_pos = 3, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(recurrence_sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(recurrence_sub_criteria, funcs_l = "equva", funcs_pos = 3)
    if(err != FALSE) return(err)
  }

  err <- err_spec_vals(schema, "schema", c("none", "by_epid", "by_strata", "by_ALL"))
  if(err != FALSE) return(err)
  err <- err_mins_1(case_length_total, "case_length_total")
  if(err != FALSE) return(err)
  err <- err_mins_1(recurrence_length_total, "recurrence_length_total")
  if(err != FALSE) return(err)
  return(FALSE)
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
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
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
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
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
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)

  if(class(criteria) != "list") criteria <- list(criteria)

  if(class(sub_criteria) != "NULL"){
    err <- err_sub_criteria_8(sub_criteria)
    if(err != FALSE) return(err[1])
    err <- err_sub_criteria_10(criteria, sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_5.0(sub_criteria, funcs_l = "equva", funcs_pos = 3)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_6.0(sub_criteria, funcs_l = "equva", funcs_pos = 3)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_7(sub_criteria, funcs_l = "equva", funcs_pos = 3)
    if(err != FALSE) return(err)
  }
  err <- err_criteria_1(criteria)
  if(err != FALSE) return(err)

  err <- err_criteria_2(criteria)
  if(err != FALSE) return(err)

  return(FALSE)
}

err_atomic_vectors <- function(arg, arg_nm){
  if(!all(class(arg) == "list")){
    arg <- list(arg)
    multi_opts <- F
  }else{
    multi_opts <- T
  }
  err <- unlist(lapply(arg, is.atomic), use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!err]

  if(length(err) > 0){
    err <- paste0(ifelse(multi_opts, "Input for ", ""), "`", arg_nm, "` must be an `atomic` vector.`\n")
    return(err)
  }else{
    return(FALSE)
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
    return(FALSE)
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
        listr(paste0("\"", unlist(lapply(x, format), use.names = FALSE), "\""), lim = 3)
      }
    })

    opts <- unlist(opts, use.names = FALSE)
    names(opts) <- names(sp)
    opts <- opts[!is.na(opts)]

    if(length(opts) > 0){
      errs <- paste0("Unique values of `", arg_nm, "` required in each `strata`:\n",
                     paste0("X - You've supplied ", listr(paste0(opts, " for ", "\"", names(opts), "\""), lim = 2) ,"."))
      return(errs)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}

  err_partitions_checks_0 <- function(date,
                                 window,
                                 windows_total ,
                                 separate,
                                 sn,
                                 strata,
                                 data_source,
                                 data_links,
                                 custom_sort,
                                 group_stats,
                                 by,
                                 length.out,
                                 fill){


    # Check for non-atomic vectors
    args <- list(date = date,
                 window = window,
                 windows_total  = windows_total ,
                 separate = separate,
                 strata = strata,
                 custom_sort = custom_sort,
                 data_source = data_source,
                 data_links = data_links,
                 by = by,
                 length.out = length.out,
                 fill = fill,
                 group_stats = group_stats)

    err <- mapply(err_atomic_vectors,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object types
    args <- list(date = date,
                 window = window,
                 windows_total  = windows_total ,
                 separate = separate,
                 #strata = strata,
                 #custom_sort = custom_sort,
                 #data_source = data_source,
                 data_links = data_links,
                 group_stats = group_stats,
                 by = by,
                 length.out = length.out,
                 fill = fill,
                 group_stats = group_stats)

    args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                         window = c("number_line", "numeric", "list"),
                         windows_total  = c("number_line", "numeric", "integer"),
                         separate = "logical",
                         #data_source = c("character", "NULL"),
                         data_links = c("list", "character"),
                         group_stats = "logical",
                         by = c("numeric", "integer", "NULL"),
                         length.out = c("numeric", "integer", "NULL"),
                         fill = "logical",
                         group_stats = "logical")

    err <- mapply(err_object_types,
                  args,
                  as.list(names(args)),
                  args_classes[match(names(args), names(args_classes))])
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object lengths
    len_lims <- c(1, length(date))
    args <- list(separate = separate,
                 strata = strata,
                 custom_sort = custom_sort,
                 data_source = data_source,
                 by = by,
                 length.out = length.out,
                 fill = fill,
                 group_stats = group_stats)

    args_lens <- list(separate = 1,
                      strata = c(0, len_lims),
                      custom_sort = c(0, len_lims),
                      data_source = c(0, len_lims),
                      by = c(0, len_lims),
                      length.out = c(0, len_lims),
                      fill = c(0, len_lims),
                      group_stats = 1)

    err <- mapply(err_match_ref_len,
                  args,
                  rep(as.list("date"), length(args_lens)),
                  args_lens[match(names(args), names(args_lens))],
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for missing values where they are not permitted
    args <- list(window = window,
                 windows_total  = windows_total ,
                 separate = separate,
                 strata = strata,
                 custom_sort = custom_sort,
                 data_source = data_source,
                 data_links = data_links,
                 by = by,
                 length.out = length.out,
                 fill = fill,
                 group_stats = group_stats)

    err <- mapply(err_missing_check,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    err <- err_data_links_1(data_source = data_source, data_links = data_links)
    if(err != FALSE) return(err)
    err <- err_data_links_2(data_source = data_source, data_links = data_links)
    if(err != FALSE) return(err)
    err <- err_sn_1(sn = sn, ref_num = length(date), ref_nm = "date")
    if(err != FALSE) return(err)
    err <- err_strata_level_args(separate, strata, "separate")
    if(err != FALSE) return(err)

    if(!is.null(by)){
      err <- err_by_2(by, date)
      if(err != FALSE) return(err[1])

      err <- err_strata_level_args(by, strata, "by")
      if(err != FALSE) return(err)
    }

    if(!is.null(length.out)){
      err <- err_lnt_out_1(length.out)
      if(err != FALSE) return(err[1])

      err <- err_strata_level_args(length.out, strata, "length.out")
      if(err != FALSE) return(err)
    }

    err <- err_strata_level_args(fill, strata, "fill")
    if(err != FALSE) return(err)
    err <- err_strata_level_args(windows_total , strata, "windows_total")
    if(err != FALSE) return(err)
    if(is.number_line(window)){
      window <- list(window)
    }
    err <- err_strata_level_args(window , strata, "window")
    if(err != FALSE) return(err)

    return(FALSE)
  }

  err_spec_vals <- function(x, var, vals){
    if(any(!tolower(x) %in% tolower(vals))){
      opts <- x
      sn <- 1:length(opts)

      opts <- split(sn , opts)
      opts <- opts[!tolower(names(opts)) %in% tolower(vals)]

      opts <- unlist(lapply(opts, function(x){
        missing_check(ifelse(sn %in% x, NA, T), 2)
      }), use.names = T)

      opts <- paste0("\"", names(opts),"\"", " at ", opts)
      if(length(opts) >3){
        errs <- paste0(paste0(opts[1:3], collapse = ", "), " ...")
      }  else{
        errs <- listr(opts)
      }
      errs <-  paste0("Invalid values for `",var, "`:\n",
                      "i - Vaild values are ", listr(paste0("\"", vals, "\""), conj = " or"), ".\n",
                      "X - You've supplied ", errs, ".")

      return(errs)
    }else{
      return(FALSE)
    }
  }

  err_mins_1 <- function(x, arg){
    lgk <- start_point(as.number_line(x)) < 1
    if(any(lgk)){
      opts <- x
      sn <- 1:length(opts)
      opts <- split(sn[lgk] , format(opts[lgk]))
      opts <- head(opts, 5)
      opts <- unlist(lapply(opts, function(x){
        missing_check(ifelse(sn %in% x, NA, T), 2)
      }), use.names = T)

      opts <- paste0("\"", names(opts),"\"", " at ", opts)
      if(length(opts) >3){
        errs <- paste0(paste0(opts[1:3], collapse = ", "), " ...")
      }  else{
        errs <- listr(opts)
      }
      errs <-  paste0("Invalid values for `", arg, "`:\n",
                      "i - Vaild values are ", "integers > 0", ".\n",
                      "X - You've supplied ", errs, ".")
      return(errs)
    }else{
      F
    }
  }

  err_schema_epid_0 <- function(x,
                                date,
                                case_length,
                                recurrence_length,
                                episode_unit,
                                episode_type,
                                from_last,
                                title,
                                show_labels,
                                show_skipped,
                                show_non_finite,
                                dark_mode){

    err <- err_object_types(x, "x", "epid")
    if(err != FALSE) return(err)
    err <- err_match_ref_len(date, "x", length(x), "date")
    if(err != FALSE) return(err)

    errs <- err_episodes_checks_0(date = date, case_length = case_length,
                                  recurrence_length = recurrence_length,
                                  episode_unit = episode_unit,
                                  episode_type = episode_type, from_last = from_last)

    if(!isFALSE(errs)) return(errs)

    # Check for non-atomic vectors
    args <- list(title = title,
                 show_labels = show_labels,
                 show_skipped = show_skipped,
                 show_non_finite = show_non_finite,
                 dark_mode = dark_mode)

    err <- mapply(err_atomic_vectors,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object types
    args <- list(title = title,
                 show_labels = show_labels,
                 show_skipped = show_skipped,
                 show_non_finite = show_non_finite,
                 dark_mode = dark_mode)

    args_classes <- list(title = c("character", "NULL"),
                         show_labels = c("character", "logical"),
                         show_skipped = "logical",
                         show_non_finite = "logical",
                         dark_mode = "logical")

    err <- mapply(err_object_types,
                  args,
                  as.list(names(args)),
                  args_classes[match(names(args), names(args_classes))])
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object lengths
    len_lims <- 1
    args <- list(title = title,
                 show_labels = show_labels,
                 show_skipped = show_skipped,
                 show_non_finite = show_non_finite,
                 dark_mode = dark_mode)

    args_lens <- list(title = c(0, len_lims),
                      show_labels = len_lims,
                      show_skipped = len_lims,
                      show_non_finite = len_lims,
                      dark_mode = len_lims)

    err <- mapply(err_match_ref_len,
                  args,
                  rep(as.list("x"), length(args_lens)),
                  args_lens[match(names(args), names(args_lens))],
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for missing values where they are not permitted
    args <- list(title = title,
                 show_labels = show_labels,
                 show_skipped = show_skipped,
                 show_non_finite = show_non_finite,
                 dark_mode = dark_mode)

    err <- mapply(err_missing_check,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "epid", "date", "case_nm", "length_arrow", "length_label"))
    if(err != FALSE) return(err[1])

    return(FALSE)
  }

  err_schema_pane_0 <- function(x,
                                date,
                                title,
                                show_labels,
                                dark_mode){

    err <- err_object_types(x, "x", "pane")
    if(err != FALSE) return(err)
    err <- err_match_ref_len(date, "x", length(x), "date")
    if(err != FALSE) return(err)

    errs <- err_episodes_checks_0(date = date)
    if(!isFALSE(errs)) return(errs)

    # Check for non-atomic vectors
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    err <- mapply(err_atomic_vectors,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object types
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    args_classes <- list(title = c("character", "NULL"),
                         show_labels = c("character", "logical"),
                         dark_mode = "logical")

    err <- mapply(err_object_types,
                  args,
                  as.list(names(args)),
                  args_classes[match(names(args), names(args_classes))])
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object lengths
    len_lims <- 1
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    args_lens <- list(title = c(0, len_lims),
                      show_labels = len_lims,
                      dark_mode = len_lims)

    err <- mapply(err_match_ref_len,
                  args,
                  rep(as.list("x"), length(args_lens)),
                  args_lens[match(names(args), names(args_lens))],
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for missing values where they are not permitted
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    err <- mapply(err_missing_check,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "pane", "date", "case_nm", "window_label"))
    if(err != FALSE) return(err[1])

    return(FALSE)
  }

  err_schema_pid_0 <- function(x,
                               title,
                               show_labels,
                               dark_mode){

    err <- err_object_types(x, "x", "pid")
    if(err != FALSE) return(err)

    # Check for non-atomic vectors
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    err <- mapply(err_atomic_vectors,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object types
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    args_classes <- list(title = c("character", "NULL"),
                         show_labels = c("character", "logical"),
                         dark_mode = "logical")

    err <- mapply(err_object_types,
                  args,
                  as.list(names(args)),
                  args_classes[match(names(args), names(args_classes))])
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for required object lengths
    len_lims <- 1
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    args_lens <- list(title = c(0, len_lims),
                      show_labels = len_lims,
                      dark_mode = len_lims)

    err <- mapply(err_match_ref_len,
                  args,
                  rep(as.list("x"), length(args_lens)),
                  args_lens[match(names(args), names(args_lens))],
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    # Check for missing values where they are not permitted
    args <- list(title = title,
                 show_labels = show_labels,
                 dark_mode = dark_mode)

    err <- mapply(err_missing_check,
                  args,
                  as.list(names(args)))
    err <- unlist(err, use.names = FALSE)
    err <- err[err != FALSE]
    if(length(err) > 0) return(err[1])

    err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "pane", "date", "case_nm", "window_label"))
    if(err != FALSE) return(err[1])

    return(FALSE)
  }
