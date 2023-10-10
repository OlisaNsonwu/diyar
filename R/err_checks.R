# @title Data validations
#
# @description Data validations
#
err_sub_criteria_1 <- function(...){
  args <- list(...)
  err <- lapply(args, function(x){
    if(is.atomic(x) | inherits(x, c("d_attribute", "sub_criteria"))){
      NA_character_
    }else{
      class(x)
    }
  })
  err <- unlist(err, use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!is.na(err)]

  if(length(err) > 0){
    paste0("Invalid object type for `...` in `sub_criteria()`:\n",
           "i - `...` must be an `atomic` vector OR,\n",
           "i - `...` must be a `d_attribute` object OR,\n",
           "i - `...` must be a `sub_criteria` object.\n",
           paste0("X - `...` ", "contains is a `", err,"` object.", collapse = "\n"))
  }else{
    FALSE
  }
}

err_sub_criteria_2 <- function(funcs, funcs_l){
  try_txt <- try(lapply(funcs, function(x){}), silent = T)
  if(inherits(try_txt, "try-error")){
    paste0("Unable to evaluate `",funcs_l,"`.:\n",
           "Issue - \"",attr(try_txt, "condition")$message, "\"")

  }else{
    FALSE
  }
}

err_sub_criteria_3 <- function(funcs, funcs_l){
  err <- sapply(funcs, is.function)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!err]
  if(length(err) > 0){
    err <- paste0("`",funcs_l,"` must be a function or a `list` of functions:\n",
                  "X - `",funcs_l,"[c(", listr(names(err), conj = ",", lim=3), ")]` ",
                  ifelse(length(err ==1), "is not a function.", "are not functions."))
    err
  }else{
    F
  }
}

err_sub_criteria_4 <- function(..., funcs, funcs_l){
  if(!length(funcs) %in% c(0, 1, length(list(...)))){
    lens <- length(list(...))
    return(paste0("Number of logical test (`",funcs_l,"`) must be 1 or the same as the number of matching attributes (`...`):\n",
                  "i - Expecting 1", ifelse(lens > 1, paste0(" or ", lens), ""), ".\n",
                  "X - You've supplied ", length(funcs), " logical tests (`",funcs_l,"`)."))

  }else{
    FALSE
  }
}

err_sub_criteria_6.0 <- function(sub_cris, funcs_l = "match_funcs", funcs_pos = 2, cri_nm = "sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[funcs_pos]]
      vec <- l2[[1]]
      if(inherits(vec, "list")){
        vec <- lapply(vec, function(x) x[1:5])
      }else{
        vec <- vec[1:5]
      }
      try_txt <- try(func(x = vec, y = vec), silent = T)
      if(inherits(try_txt, "try-error")){
        attr(try_txt, "condition")$message
      }else if(!inherits(try_txt, "logical")){
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
      if(inherits(try_txt, "try-error")){
        attr(try_txt, "condition")$message
      }else if(!inherits(try_txt, "logical")){
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

err_sub_criteria_5.0 <- function(sub_cris, funcs_l = "match_funcs", funcs_pos = 2, cri_nm = "sub_criteria"){
  f1 <- function(l1){
    sb_attr <- l1[[1]]
    func <- l1[[funcs_pos]]
    if(inherits(sb_attr, "sub_criteria")){
      f1(sb_attr[[1]])
    }else{
      all(c("x", "y") %in% methods::formalArgs(func))
    }
  }

  err <- lapply(sub_cris, f1)
  err <- unlist(err, use.names = FALSE)

  if(length(err[!err]) > 0){
    err <- paste0("Invalid arguments for `", funcs_l, "`:\n",
                  "i - Each `", funcs_l, "` must have at least two arguments named `x` and `y`.\n")
    err
  }else{
    F
  }
}

err_sub_criteria_7 <- function(sub_cris, funcs_l = "match_funcs", funcs_pos = 2, cri_nm = "case_sub_criteria"){
  err <- lapply(seq_len(length(sub_cris)), function(i){
    l1 <- sub_cris[[i]]
    y <- lapply(seq_len(length(l1)), function(j){
      l2 <- l1[[j]]
      func <- l2[[funcs_pos]]
      vec <- l2[[1]]
      if(inherits(vec, "list")){
        vec <- lapply(vec, function(x) x[1:5])
      }else{
        vec <- vec[1:5]
      }
      e <- try(func(vec, vec),
               silent = TRUE)
      x <- class(e)
      ifelse(x == "logical" & length(e) %in% c(1,5), NA_character_, paste0("Issue with ",
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
  err <- err_object_types(x, cri_nm, c("sub_criteria", "list"))
  if(!isFALSE(err)) {
    return(err)
  }
  else{
    return(FALSE)
  }
}

err_data_links_1 <- function(data_source, data_links){
  # data_links - Each element must be named g or l ####
  dl_lst <- unlist(data_links, use.names = FALSE)
  ds_lst <- data_source[!duplicated(data_source)]
  ms_lst <- unique(dl_lst[!dl_lst %in% c(ds_lst,"ANY")])

  if(is.list(data_links) & !is.null(data_source)){
    nms <- names(data_links);
    names(nms) <- 1:length(nms)
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
                   "i - \"l\" - Only return record-groups with records from \"DS1\" AND \"DS2\" `data_source`.`\n",
                   "i - \"g\" - Only return record-groups with records from \"DS3\" OR  \"DS4\" `data_source`.`\n",
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

err_criteria_1 <- function(criteria){
  if(!inherits(criteria, "list")){
    criteria <- list(criteria)
  }

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
  if(!inherits(criteria, "list")){
    criteria <- list(criteria)
  }

  err <- lapply(criteria, length)
  err <- unlist(err, use.names = FALSE)
  if(length(err) > 0) names(err) <- 1:length(err)
  err <- err[!duplicated(err)]

  if(!min(err) %in% c(1, max(err)) | min(err) == 0){
    paste0("Length of each `criteria` must be the same and greater than 0 or equal to 1:\n",
           paste0("X - `criteria ", names(err),"` is ", fmt(err), ".", collapse = "\n"))
  }else{
    F
  }
}

err_sub_criteria_10 <- function(ref_arg, sub_criteria, arg_nm = "criteria", cri_nm = "sub_criteria"){
  if(!inherits(ref_arg, "list")){
    ref_arg <- list(ref_arg)
  }

  if(inherits(sub_criteria, "sub_criteria")){
    sub_criteria <- list(sub_criteria)
  }
  err <- as.numeric(lapply(ref_arg, length))
  err <- err[!duplicated(err)]

  err2 <- lapply(sub_criteria, function(x){
    attr_eval(x, identity, simplify = FALSE)
  })
  err2 <- unpack(err2)
  err2 <- lapply(err2, length)
  err2 <- unlist(err2, use.names = FALSE)
  err2 <- sort(err2[!duplicated(err2)])
  if(any(!c(err, err2) %in% c(1, max(c(err, err2))))){
    return(paste0("Length of each `", arg_nm, "` and each attribute of `", cri_nm, "` must be the same or equal to 1:\n",
                  # "i - Expecting ", listr(unique(c(1, err)), conj = " or "), ".\n",
                  "X - ", ifelse(length(err) == 1, "Length", "Lengths")," of `", arg_nm, "` ", ifelse(length(err) == 1, "is", "are"), " ", listr(fmt(sort(err))), ".\n",
                  "X - ", ifelse(length(err2) == 1, "Length", "Lengths")," of attribute(s) in `", cri_nm, "` ", ifelse(length(err2) == 1, "is", "are"), " ", listr(fmt(sort(err2))), "."))
  }else{
    F
  }
}

err_sub_criteria_10b <- function(sub_criteria){
  nms <- names(sub_criteria)
  nms <- nms[nms %in% nms[duplicated(nms)]]
  nms <- rle(nms)
  if(length(nms$lengths) > 0){
    return(paste0("Multiple `sub_criteria` objects assinged to a `criteria`:\n",
                  "i - Only one `sub_criteria` object can be assinged to a `criteria`.\n",
                  "i - Instead, nest all match criteria in one `sub_criteria` object.\n",
                  "i - Example ~ `sub_criteria(X, sub_criteria(Y), operator = \"and\")`.\n",
                  paste0("X - ", nms$lengths, " `sub_criteria` object(s) assinged to Criteria-", gsub("^cr", "", nms$values), " (`", nms$values, "`)", collapse = "\n")
                  )
           )
  }else{
    F
  }
}

err_sub_criteria_3dot_1 <- function(...){
  err <- lapply(list(...), function(x){
    attr_eval(x, rc_dv)
  })
  err <- unlist(err, use.names = FALSE)
  err <- sort(err[!duplicated(err)])
  err2 <- err[err != 1]

  if(length(err2) > 1){
    paste0("Different lengths for each attribute (`...`) of a `sub_criteria`:\n",
           "i - Each attribute in (`...`) must have the same length or a length of 1.\n",
           "i - This includes recursive evaluations of `d_attribute` and `sub_criteria` objects.\n",
           paste0("X - `...` ", "contains elements of lengths ", listr(err),".", collapse = "\n"))
  }else{
    FALSE
  }
}

err_sn_1 <- function(sn, ref_nm, ref_num){
  if(!inherits(sn, c("numeric", "integer", "NULL"))){
    err <- paste0("Invalid object type for `sn`:\n",
                  "i - Valid object type is `integer`.\n",
                  "i - `numeric` objects are coerced to `integer` objects.\n",
                  "i - Duplicate values (after coercion) are not permitted.\n",
                  "X - You've supplied a `", class(sn), "` object.")
    return(err)
  }

  if(!inherits(sn, "NULL")){
    if(length(sn) != ref_num){
      err <- paste0("Length of `sn` must be the same as `", ref_nm, "`:\n",
                    "i - Expecting a length of ", fmt(ref_num), ".\n",
                    "X - Length is ", fmt(length(sn)), ".\n")
      return(err)
    }

    dp_check <- duplicates_check(as.integer(sn))
    if(dp_check != T){
      err <- paste0("`sn` must be unique values.\n",
                    "i - `numeric` objects are coerced to `integer` objects.\n",
                    "i - Duplicate values (after coercion) are not permitted.\n",
                    "X - There are duplicate values in ", dp_check ,".")
      return(err)
    }

    fn_check <- finite_check(sn)
    if(fn_check!=T){
      err <- paste0("`sn` must be finite `integer` values:\n",
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
                    "i - Vaild values are ", listr(paste0("\"", names(diyar::episode_unit), "\""), conj = " or "), ".\n",
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
  x <- as.number_line(x)
  lgk <- (x@.Data > 0 & by < 0) | (x@.Data < 0 & by > 0)
  if(any(lgk)){
    by[lgk] <- NA_real_
    err <- missing_check(by)

    if(err != FALSE){
      err <-  paste0("Incorrect sign for `by`:\n",
                     "i - \"+\" and \"-\" are required for \"increasing\" and \"decreasing\" `number_lines' respectively.\n",
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

err_overlap_methods_1 <- function(overlap_methods, overlap_methods_nm){
  if(!(inherits(overlap_methods, "list"))){
    overlap_methods <- list(overlap_methods)
  }

  err <- lapply(overlap_methods, overlaps_err)
  names(err) <- seq_len(length(err))
  err <- unlist(err, use.names = T)

  if(length(err) > 0){
    err <- paste0("Invalid option for `", overlap_methods_nm, "`\n",
                  "i - Valid options can be values in `diyar::overlap_methods$options$cd`.\n",
                  "i - Valid options can be values in `names(diyar::overlap_methods$methods)`.\n",
                  "i - Syntax 1 ~ `100`.\n",
                  "i - Syntax 2 ~ `\"aligns_end|exact...\"`.\n",
                  "i - Syntax 3 ~ `include_overlap_method(c(\"aligns_end\", \"exact\"))`.\n",
                  "i - Syntax 4 ~ `exclude_overlap_method(c(\"across\", \"chain\", \"aligns_start\", \"inbetween\")`).\n",
                  paste0("X - `", overlap_methods_nm, " ", names(err),   "`: You've supplied ", err, ".", collapse = "\n"))
    return(err)
  }else{
    return(FALSE)
  }
}

err_overlap_methods_2 <- function(overlap_methods, lengths, overlap_methods_nm, lengths_nm){
  # overlap_methods - Check if there are more overlap methods than there are lengths

  if(!inherits(lengths, "list")){
    lengths <- list(lengths)
  }
  if(!inherits(overlap_methods, "list")){
    overlap_methods <- list(overlap_methods)
  }
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
  if(!inherits(arg, "list")){
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
                  ifelse(ref_nm == "", "", paste0("i - Length must be ", ifelse(1 %in% ref_len,"1 or", ""), " the same as `", ref_nm, "`.\n")),
                  "i - Expecting a length of ", listr(fmt(unique(ref_len)), conj = " or "), ".\n",
                  paste0("X - ", ifelse(rep(multi_opts, fmt(length(err))), paste0("`", arg_nm, " ", names(err), "`: "), ""), "Length is ", fmt(err), ".", collapse = "\n"))
    return(err)
  }
  return(FALSE)
}

err_match_ref_len_2 <- function(arg, ref_nm, ref_len, arg_nm, multi_opts = FALSE){
  if(inherits(arg, "list")){
    unlist(lapply(arg, function(x){
      err_match_ref_len_2(x, ref_nm, ref_len, arg_nm, multi_opts = TRUE)
    }), use.names = FALSE)
  }
  else{
    err <- length(arg)
    err <- ifelse(!err %in% c(ref_len), err, NA_real_)
    if(length(err) > 0) names(err) <- 1:length(err)
    err <- err[!is.na(err)]

    if(length(err) > 0){
      err <- paste0("Invalid length for ", ifelse(multi_opts, "elements in ", ""), "`", arg_nm, "`:\n",
                    paste0("i - Length must be ", ifelse(1 %in% ref_len,"1", ""), ref_nm, ".\n"),
                    ifelse(ref_nm == "",
                           "",
                           paste0("i - Expecting a length of ", listr(fmt(unique(ref_len)), conj = " or "), ".\n")),
                    paste0("X - ", ifelse(rep(multi_opts, fmt(length(err))), paste0("`", arg_nm, " ", names(err), "`: "), ""), "Length is ", fmt(err), ".", collapse = "\n"))
      return(err)
    }else{
      return(FALSE)
    }
  }

  if(!inherits(arg, "list")){
    arg <- list(arg)
    multi_opts <- F
  }else{
    multi_opts <- T
  }


  return(FALSE)
}

err_object_types <- function(arg, arg_nm, obj_types){

  if(inherits(arg, "list")){
    unlist(lapply(arg, function(x){
      err_object_types(x, arg_nm, obj_types)
    }), use.names = FALSE)
  }else{
    x <- class(arg)
    if(!any(x %in% obj_types)){
      err <- listr(paste0("`", x[!x %in% obj_types], "`"), conj = " or ")
    }else{
      err <- NA_character_
    }
    err <- err[!is.na(err)]

    if(length(err) > 0){
      obj_types <- ifelse(obj_types == "logical", "logical (TRUE or FALSE)", obj_types)
      obj_types <- paste0("`", obj_types, "`")
      if("`list`" %in% obj_types){
        obj_types <- obj_types[!obj_types %in% "`list`"]
        obj_types <- c(obj_types,
                       paste0("a `list` with ", listr(paste0(obj_types), conj = " or "), " elements"))
      }
      valid_opts <- listr(paste0(obj_types), conj = " or ")
      err <- paste0("Invalid object type for `", arg_nm, "`.\n",
                    "i - Valid object type", ifelse(length(obj_types) > 1, "s are", " is"),  " ", valid_opts, ".\n",
                    paste0("X - You've supplied a ", err, " object.", collapse = "\n"))
      return(err)
    }else{
      return(FALSE)
    }
  }
}

err_object_types_2 <- function(arg, arg_nm, obj_types){
  if("list" %in% obj_types & inherits(arg, "list")){
    unlist(lapply(arg, function(x){
      err_object_types_2(x, arg_nm, obj_types)
    }), use.names = FALSE)
  }else if("atomic" %in% obj_types){
    unlist(err_atomic_vectors_2(arg, arg_nm), use.names = FALSE)
  }else{
    x <- class(arg)
    if(!any(x %in% obj_types)){
      err <- listr(paste0("`", x[!x %in% obj_types], "`"), conj = " or ")
    }else{
      err <- NA_character_
    }
    err <- err[!is.na(err)]
    if(length(err) > 0){
      obj_types <- ifelse(obj_types == "logical", "logical (TRUE or FALSE)", obj_types)
      obj_types <- paste0("`", obj_types, "`")
      if("`list`" %in% obj_types){
        obj_types <- obj_types[!obj_types %in% "`list`"]
        obj_types <- c(obj_types,
                       paste0("a `list` with ", listr(paste0(obj_types), conj = " or "), " elements"))
      }
      valid_opts <- listr(paste0(obj_types), conj = " or ")

      err <- paste0("Invalid object type for `", arg_nm, "`.\n",
                    "i - Valid object type ", ifelse(length(valid_opts) > 1, "are", "is"),  ": ", valid_opts, ".\n",
                    paste0("X - You've supplied: ", err, collapse = "\n"))
      return(err)
    }else{
      return(FALSE)
    }
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
                       case_overlap_methods = c("list", "numeric", "integer", "character"),
                       recurrence_overlap_methods = c("list", "numeric", "integer", "character"),
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
  err <- err_spec_vals(episode_type, "episode_type", c("fixed", "rolling", "recursive"))
  if(err != FALSE) return(err[1])
  err <- err_spec_vals(display, "display", c("none", "progress", "stats", "none_with_report", "progress_with_report", "stats_with_report"))
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

err_episodes_checks_0 <- function(date = 1, case_length = 1, episode_type = "fixed", recurrence_length = case_length,
                                  episode_unit = "days", strata = 1, sn = seq_len(length(date)), episodes_max = 1, rolls_max = 1,
                                  case_overlap_methods = 8, recurrence_overlap_methods = case_overlap_methods,
                                  skip_if_b4_lengths = FALSE, data_source = "1",
                                  data_links = "ANY", custom_sort = 1, skip_order = 1, reference_event = "last_record",
                                  case_for_recurrence = FALSE, from_last = FALSE, group_stats = c("case_nm", "wind", "epid_interval"),
                                  display = "none", case_sub_criteria = NULL, recurrence_sub_criteria = NULL,
                                  case_length_total = 1, recurrence_length_total = case_length_total,
                                  skip_unique_strata = TRUE, skip_checks = NULL, ...){

  # Check for required object types
  args <- list(date = date,
               case_length = case_length,
               recurrence_length = recurrence_length,
               episode_type = episode_type,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               episode_unit = episode_unit,
               display = display,
               data_source = data_source,
               episodes_max = episodes_max,
               rolls_max = rolls_max,
               skip_if_b4_lengths = skip_if_b4_lengths,
               data_links = data_links,
               skip_order = skip_order,
               reference_event = reference_event,
               case_for_recurrence = case_for_recurrence,
               from_last = from_last,
               group_stats = group_stats,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total,
               case_sub_criteria = case_sub_criteria,
               recurrence_sub_criteria = recurrence_sub_criteria,
               skip_unique_strata = skip_unique_strata)

  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       episode_type = "character",
                       case_overlap_methods = c("list", "numeric", "integer", "character"),
                       recurrence_overlap_methods = c("list", "numeric", "integer", "character"),
                       episode_unit = "character",
                       display = "character",
                       case_length = c("list", "integer", "numeric", "number_line"),
                       recurrence_length = c("list", "integer", "numeric", "number_line"),
                       episodes_max = c("numeric", "integer"),
                       rolls_max = c("numeric", "integer"),
                       data_source = c("NULL", "character"),
                       data_links = c("list", "character"),
                       skip_order = c("numeric", "integer"),
                       skip_if_b4_lengths = "logical",
                       reference_event = c("character", "logical"),
                       case_for_recurrence = "logical",
                       from_last = "logical",
                       group_stats = c("logical", "character", "NULL"),
                       case_length_total = c("numeric", "integer", "number_line"),
                       recurrence_length_total = c("numeric", "integer", "number_line"),
                       case_sub_criteria = c("sub_criteria", "NULL"),
                       recurrence_sub_criteria = c("sub_criteria", "NULL"),
                       skip_unique_strata = "logical")

  err <- mapply(err_object_types_2,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  if(length(date) == 0){
    return(
      paste0("Invalid length for `date`:\n",
             "i - Length must be greater than 0.\n",
             "X - Length is ", fmt(length(date)), ".\n")
    )
  }
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
               reference_event = reference_event,
               case_for_recurrence = case_for_recurrence,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total,
               skip_unique_strata = skip_unique_strata)

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
                    reference_event = len_lims,
                    case_for_recurrence = len_lims,
                    case_length_total = len_lims,
                    recurrence_length_total = len_lims,
                    skip_unique_strata = 1)

  err <- mapply(err_match_ref_len_2,
                args,
                c(
                  rep(as.list(" or the same length as `date`"), 4),
                  as.list(""),
                  rep(as.list(" or the same length as `date`"), 13),
                  as.list("")
                )
                ,
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
               episodes_max = episodes_max,
               rolls_max = rolls_max,
               data_source = data_source,
               data_links = data_links,
               skip_order = skip_order,
               skip_if_b4_lengths = skip_if_b4_lengths,
               reference_event = reference_event,
               case_for_recurrence = case_for_recurrence,
               case_length_total = case_length_total,
               recurrence_length_total = recurrence_length_total,
               skip_unique_strata = skip_unique_strata)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(date))
  args <- list(case_length = case_length,
               recurrence_length = recurrence_length,
               case_overlap_methods = case_overlap_methods,
               recurrence_overlap_methods = recurrence_overlap_methods,
               data_links = data_links)

  args_opt_lv <- list(case_length = c("", "r", "e", "w"),
                    recurrence_length = c("", "r", "e", "w"),
                    case_overlap_methods = c("", "r", "e", "w"),
                    recurrence_overlap_methods = c("", "r", "e", "w"),
                    data_links = c("", "l", "g"))

  err <- mapply(err_option_level,
                args,
                args_opt_lv[match(names(args), names(args_opt_lv))],
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
  err <- err_spec_vals(episode_type, "episode_type", c("fixed", "rolling", "recursive"))
  if(err != FALSE) return(err)
  err <- err_spec_vals(display, "display", c("none", "progress", "stats", "none_with_report", "progress_with_report", "stats_with_report"))
  if(err != FALSE) return(err)
  err <- err_spec_vals(reference_event, "reference_event", c("first_record", "first_event", "last_record", "last_event", TRUE, FALSE))
  if(err != FALSE) return(err)
  err <- err_spec_vals(group_stats, "group_stats", c("case_nm", "wind", "epid_interval", TRUE, FALSE, NULL))
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
  if(!"strata_level" %in% skip_checks){
    err <- err_strata_level_args(from_last, strata, "from_last")
    if(err != FALSE) return(err)
    err <- err_strata_level_args(episodes_max, strata, "episodes_max")
    if(err != FALSE) return(err)
  }
  if(!inherits(case_sub_criteria, "NULL")){
    err <- err_sub_criteria_8(case_sub_criteria, cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err[1])
    err <- err_sub_criteria_10(date, case_sub_criteria, "date", cri_nm = "case_sub_criteria")
    if(err != FALSE) return(err)
  }

  if(!inherits(recurrence_sub_criteria, "NULL")){
    err <- err_sub_criteria_8(recurrence_sub_criteria, cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err[1])
    err <- err_sub_criteria_10(date, recurrence_sub_criteria, "date", cri_nm = "recurrence_sub_criteria")
    if(err != FALSE) return(err)
  }

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
                               shrink,
                               recursive,
                               check_duplicates,
                               tie_sort,
                               repeats_allowed,
                               permutations_allowed,
                               ignore_same_source,
                               batched){

  # Check for required object types
  args <- list(
    data_source = data_source,
    display = display,
    group_stats = group_stats,
    expand = expand,
    shrink = shrink,
    recursive = recursive,
    sub_criteria = sub_criteria,
    criteria = criteria,
    tie_sort = tie_sort,
    strata = strata,
    check_duplicates = check_duplicates,
    repeats_allowed = repeats_allowed,
    permutations_allowed = permutations_allowed,
    ignore_same_source = ignore_same_source,
    batched = batched)

  args_classes <- list(display = "character",
                       data_source = c("character", "NULL"),
                       data_links = c("list", "character"),
                       shrink = "logical",
                       expand = "logical",
                       group_stats = "logical",
                       recursive = c("character", "logical"),
                       sub_criteria = c("list", "sub_criteria", "NULL"),
                       criteria = c("list", "atomic"),
                       tie_sort = "atomic",
                       strata = "atomic",
                       check_duplicates = "logical",
                       repeats_allowed = "logical",
                       permutations_allowed = "logical",
                       ignore_same_source = "logical",
                       # batched = "logical",
                       batched = "character")

  err <- mapply(err_object_types_2,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  if(!inherits(criteria, "list")){
    criteria <- list(criteria)
  }
  len_lims <- c(1, length(criteria[[1]]))
  if(!is.null(sub_criteria)){
    scri.attrs.len <- lapply(sub_criteria, function(x){
      attr_eval(x, rc_dv)
    })
    scri.attrs.len <- unlist(scri.attrs.len, use.names = FALSE)
    len_lims <- max(c(len_lims, scri.attrs.len))
  }
  args <- list(data_source = data_source,
               display = display,
               group_stats = group_stats,
               expand = expand,
               shrink = shrink,
               recursive = recursive,
               check_duplicates = check_duplicates,
               tie_sort = tie_sort,
               repeats_allowed = repeats_allowed,
               permutations_allowed = permutations_allowed,
               ignore_same_source = ignore_same_source,
               batched = batched,
               strata = strata)

  args_lens <- list(data_source = c(0, len_lims),
                    display = 1,
                    group_stats = 1,
                    expand = 1,
                    shrink = 1,
                    recursive = 1:4,
                    check_duplicates = 1,
                    tie_sort = c(0, len_lims),
                    repeats_allowed = 1,
                    permutations_allowed = 1,
                    ignore_same_source = 1,
                    batched = c(1, length(criteria)),
                    strata = c(0, len_lims))

  err <- mapply(err_match_ref_len_2,
                args,
                c(as.list(" or the same length as each attribute in the match crtieria"),
                  rep(as.list(""), 6),
                  as.list(" or the same length as each attribute in the match crtieria"),
                  rep(as.list(""), 3),
                  rep(as.list(" or the same length as each attribute in the match crtieria"), 2)
                ),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_data_links_1(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)

  err <- err_data_links_2(data_source = data_source, data_links = data_links)
  if(err != FALSE) return(err)

  if(!inherits(criteria, "list")){
    criteria <- list(criteria)
  }

  if(!inherits(sub_criteria, "NULL")){
    err <- err_sub_criteria_8(sub_criteria)
    if(any(err != FALSE)) return((err[err != FALSE])[1])
    err <- err_sub_criteria_10(criteria, sub_criteria)
    if(err != FALSE) return(err)
    err <- err_sub_criteria_10b(sub_criteria)
    if(err != FALSE) return(err)
  }
  err <- err_criteria_1(criteria)
  if(err != FALSE) return(err)
  err <- err_criteria_2(criteria)
  if(err != FALSE) return(err)
  err <- err_spec_vals(display, "display", c("none", "progress", "stats",
                                             "none_with_report", "progress_with_report",
                                             "stats_with_report"))
  if(err != FALSE) return(err)
  return(FALSE)
}

err_atomic_vectors <- function(arg, arg_nm){
  if(!inherits(arg, "list")){
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

err_atomic_vectors_2 <- function(arg, arg_nm){
  err <- is.atomic(arg) | is.null(arg)
  if(isFALSE(err)){
    return(paste0("`", arg_nm, "` must be an `atomic` vector.`\n"))
  }else{
    return(FALSE)
  }
}

err_invalid_opts <- function(arg_vals, arg_nm, valid_opts){
  errs <- invalid_opts(arg_vals, valid_opts)
  if(length(errs) > 0){
    errs <-  paste0("Invalid values for `", arg_nm, "`:\n",
                    "i - Vaild values are ", listr(paste0("\"", valid_opts, "\""), conj = " or "), ".\n",
                    "X - You've supplied ", errs, ".")
    return(errs)
  }else{
    return(FALSE)
  }
}

err_strata_level_args <- function(arg, strata, arg_nm, strata_l = "`strata`"){
  one_val <- arg[!duplicated(arg)]
  one_val <- length(arg) == 1
  if(!one_val){
    if(inherits(strata, "NULL")){
      strata <- "NULL"
    }
    sp <- split(arg, strata)
    opts <- lapply(sp, function(x){
      x <- x[!duplicated(x)]
      length(x)
    })

    opts <- unlist(opts, use.names = FALSE)
    names(opts) <- names(sp)
    opts <- opts[opts > 1]

    if(length(opts) > 0){
      errs <- paste0("Unique values of `", arg_nm, "` required for every record of each ", strata_l, ":\n",
                     paste0("X - Different values in records of `strata`: ", listr(paste0("\"", names(opts), "\""), lim = 3), "."))
      return(errs)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
}

err_partitions_checks_0 <- function(
    date,
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
    fill,
    display){


  # Check for required object types
  args <- list(
    date = date,
    window = window,
    windows_total  = windows_total ,
    separate = separate,
    strata = strata,
    custom_sort = custom_sort,
    data_source = data_source,
    data_links = data_links,
    group_stats = group_stats,
    by = by,
    length.out = length.out,
    fill = fill,
    group_stats = group_stats,
    display = display )

  args_classes <- list(date = c("Date","POSIXct", "POSIXt", "POSIXlt", "number_line", "numeric", "integer"),
                       window = c("numeric", "integer", "number_line", "list", "NULL"),
                       windows_total  = c("number_line", "numeric", "integer"),
                       separate = "logical",
                       strata = "atomic",
                       custom_sort = "atomic",
                       data_source = "atomic",
                       data_links = c("list", "character"),
                       group_stats = "logical",
                       by = c("numeric", "integer", "NULL"),
                       length.out = c("numeric", "integer", "NULL"),
                       fill = "logical",
                       group_stats = "logical",
                       display = "character")

  err <- mapply(err_object_types_2,
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
               window = window,
               custom_sort = custom_sort,
               data_source = data_source,
               by = by,
               length.out = length.out,
               fill = fill,
               group_stats = group_stats,
               display = display)

  args_lens <- list(separate = 1,
                    strata = c(0, len_lims),
                    window = c(0, len_lims),
                    custom_sort = c(0, len_lims),
                    data_source = c(0, len_lims),
                    by = c(0, len_lims),
                    length.out = c(0, len_lims),
                    fill = c(0, len_lims),
                    group_stats = 1,
                    display = 1)

  err <- mapply(err_match_ref_len_2,
                args,
                c(
                  rep(as.list(""), 1),
                  " or the same length as `date`",
                  " or the same length as `date`",
                  rep(as.list(""), 7)
                ),
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
               group_stats = group_stats,
               display = display)

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

  if(!is.null(window)){
    if(is.number_line(window)){
      window <- list(window)
    }
    err <- err_strata_level_args(window , strata, "window")
  }
  if(err != FALSE) return(err)
  err <- err_spec_vals(display, "display", c("none", "progress", "stats"))
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
                    "i - Vaild values are ", listr(paste0("\"", vals, "\""), conj = " or "), ".\n",
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
                              from_last,
                              title,
                              show_labels,
                              show_skipped,
                              show_non_finite,
                              theme){

  err <- err_object_types(x, "x", "epid")
  if(err != FALSE) return(err)
  err <- err_match_ref_len(date, "x", length(x), "date")
  if(err != FALSE) return(err)

  errs <- err_episodes_checks_0(date = date, case_length = case_length,
                                recurrence_length = recurrence_length,
                                episode_unit = episode_unit,
                                from_last = from_last,
                                skip_checks = "strata_level")

  if(!isFALSE(errs)) return(errs)

  # Check for non-atomic vectors
  args <- list(title = title,
               show_labels = show_labels,
               show_skipped = show_skipped,
               show_non_finite = show_non_finite,
               theme = theme)

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
               theme = theme)

  args_classes <- list(title = c("character", "NULL"),
                       show_labels = c("character", "logical"),
                       show_skipped = "logical",
                       show_non_finite = "logical",
                       theme = "character")

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
               #show_labels = show_labels,
               show_skipped = show_skipped,
               show_non_finite = show_non_finite,
               theme = theme)

  args_lens <- list(title = c(0, len_lims),
                    #show_labels = len_lims,
                    show_skipped = len_lims,
                    show_non_finite = len_lims,
                    theme = len_lims)

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
               theme = theme)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "epid", "date", "case_nm", "wind_nm", "length",
                                                     "length_arrow", "case_overlap_methods","recurrence_overlap_methods"))
  if(err != FALSE) return(err[1])

  err <- err_spec_vals(theme, "theme", c("dark", "light"))
  if(err != FALSE) return(err[1])

  return(FALSE)
}

err_schema_pane_0 <- function(x,
                              date,
                              title,
                              show_labels,
                              theme){

  err <- err_object_types(x, "x", "pane")
  if(err != FALSE) return(err)
  err <- err_match_ref_len(date, "x", length(x), "date")
  if(err != FALSE) return(err)

  errs <- err_episodes_checks_0(date = date)
  if(!isFALSE(errs)) return(errs)

  # Check for non-atomic vectors
  args <- list(title = title,
               show_labels = show_labels,
               theme = theme)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(title = title,
               show_labels = show_labels,
               theme = theme)

  args_classes <- list(title = c("character", "NULL"),
                       show_labels = c("character", "logical"),
                       theme = "character")

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
               #show_labels = show_labels,
               theme = theme)

  args_lens <- list(title = c(0, len_lims),
                    #show_labels = len_lims,
                    theme = len_lims)

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
               theme = theme)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "pane", "date", "case_nm", "window_label"))
  if(err != FALSE) return(err[1])

  err <- err_spec_vals(theme, "theme", c("dark", "light"))
  if(err != FALSE) return(err[1])

  return(FALSE)
}

err_schema_pid_0 <- function(x,
                             title,
                             show_labels,
                             theme,
                             orientation){

  err <- err_object_types(x, "x", "pid")
  if(err != FALSE) return(err)

  # Check for non-atomic vectors
  args <- list(title = title,
               show_labels = show_labels,
               theme = theme,
               orientation = orientation)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(title = title,
               show_labels = show_labels,
               theme = theme,
               orientation = orientation)

  args_classes <- list(title = c("character", "NULL"),
                       show_labels = c("character", "logical"),
                       theme = "character",
                       orientation = "character")

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
               #show_labels = show_labels,
               theme = theme,
               orientation = orientation)

  args_lens <- list(title = c(0, len_lims),
                    #show_labels = len_lims,
                    theme = len_lims,
                    orientation = len_lims)

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
               theme = theme,
               orientation = orientation)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  err <- err_spec_vals(show_labels, "show_labels", c(TRUE, FALSE, "sn", "pane", "date", "case_nm", "window_label"))
  if(err != FALSE) return(err[1])

  err <- err_spec_vals(orientation, "orientation", c("by_pid", "by_pid_cri"))
  if(err != FALSE) return(err[1])

  err <- err_spec_vals(theme, "theme", c("dark", "light"))
  if(err != FALSE) return(err[1])

  return(FALSE)
}


err_sub_criteria_funcs <- function(..., funcs, funcs_l){
  if(!inherits(funcs, "list")){
    funcs <- list(funcs)
  }

  err <- err_sub_criteria_2(funcs = funcs, funcs_l = funcs_l)
  if(!isFALSE(err)) return(err)

  err <- err_sub_criteria_3(funcs = funcs, funcs_l = funcs_l)
  if(!isFALSE(err)) return(err)

  err <- err_sub_criteria_4(..., funcs = funcs, funcs_l = funcs_l)
  if(!isFALSE(err)) return(err)

  return(FALSE)
}

err_links_wf_probablistic_0 <- function(attribute,
                                        blocking_attribute,
                                        cmp_func,
                                        attr_threshold,
                                        probabilistic,
                                        m_probability,
                                        u_probability,
                                        score_threshold,
                                        id_1, id_2){
  # Check for non-atomic vectors
  args <- list(blocking_attribute = blocking_attribute,
               attr_threshold = attr_threshold,
               probabilistic = probabilistic,
               # m_probability = m_probability,
               # u_probability = u_probability,
               score_threshold = score_threshold,
               id_1 = id_1, id_2 = id_2)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(
    # attribute = attribute,
    attr_threshold = attr_threshold,
    probabilistic = probabilistic,
    m_probability = m_probability,
    u_probability = u_probability,
    score_threshold = score_threshold,
    cmp_func = cmp_func,
    id_1 = id_1, id_2 = id_2)

  args_classes <- list(
    # attribute = c("list", "data.frame", "matrix", "d_attribute"),
    attr_threshold = c("list", "numeric", "integer", "number_line"),
    probabilistic = "logical",
    m_probability = c("list", "numeric", "integer"),
    u_probability = c("list", "numeric", "integer", "NULL"),
    score_threshold = c("list", "numeric", "integer", "number_line"),
    cmp_func = c("list", "function"),
    id_1 = c("NULL", "numeric", "integer"),
    id_2 = c("NULL", "numeric", "integer"))

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  if(inherits(attribute, c("list", "data.frame"))){
    attribute <- attrs(.obj = attribute)
  }else if(inherits(attribute, c("matrix"))){
    attribute <- attrs(.obj = as.data.frame(attribute))
  }else if(inherits(attribute, c("d_attribute"))){

  }else{
    attribute <- attrs(attribute)
  }

  err <- unlist(lapply(attribute, is.atomic), use.names = FALSE)
  if(length(which(!err)) > 0){
    err <- paste0("`attribute` must be an `atomic` object or `d_attribute` with `atomic` elements:\n")
    return(err)
  }

  lens <- unlist(lapply(attribute, attr_eval), use.names = FALSE)
  if(!is.null(blocking_attribute)){
    lens <- c(lens, length(blocking_attribute))
  }
  lens <- lens[!duplicated(lens)]
  if(length(lens) != 1 & length(blocking_attribute) != 1){
    err <- paste0("Lengths of `blocking_attribute` and each attribute must be the same or equal to 1")
    return(err)
  }
  # lens_2 <- unlist(lapply(list(id_1, id_2), attr_eval), use.names = FALSE)
  lens_2 <- c(length(id_1), length(id_2))
  lens_2 <- lens_2[!duplicated(lens_2)]
  if(length(lens_2) != 1){
    err <- paste0("Lengths of `id_1` and `id_2` must be the same or equal to 1")
    return(err)
  }

  lens <- seq_len(max(lens))
  id_1 <- id_1[!duplicated(id_1)]
  id_2 <- id_2[!duplicated(id_2)]
  err_1 <- which(!as.integer(id_1) %in% lens)
  err_2 <- which(!as.integer(id_2) %in% lens)
  if(length(err_1) + length(err_2) != 0){
    err <- paste0("`id_1` and `id_2` must be an index of the record set:\n",
                  paste0("X - Index ", listr(c(id_1[err_1], id_2[err_2]), lim =5), " do not exist"))
    return(err)
  }

  err <- c("m_probability" = length(m_probability),
           "u_probability" = length(u_probability),
           "cmp_func" = length(cmp_func),
           "attr_threshold" = length(attr_threshold))
  if(is.null(u_probability)){
    err <- err[names(err) != "u_probability"]
  }
  ref_err <- length(attribute)
  err_lgk <- which(ref_err != err & err != 1)
  if(any(which(ref_err != err & err != 1))){
    err <- paste0("Lengths of `cmp_func`, `attr_threshold`, `m_probability` and `u_probability` must be 1 or match the number of attributes supplied:\n",
                  "i - Expecting lengths of 1 or ", ref_err, "\n",
                  paste0("X - Length of `", names(err[err_lgk]), "` is ", err[err_lgk], ".", collapse = "\n"))
    return(err)
  }


  if(!inherits(m_probability, "list")){
    m_probability <- as.list(m_probability)
  }
  err <- lapply(m_probability, function(x) !all(x >= 0 & x <= 1))
  err <- unlist(err, use.names = FALSE)
  if(length(err[err]) > 0){
    err <- paste0("m-probabilities must be between 0 and 1:\n",
                  paste0("X - `m_probability[[", which(err), "]]` is ", m_probability[[which(err)]], ".", collapse = "\n"))
    return(err)
  }

  if(!is.null(u_probability)){
    if(!inherits(u_probability, "list")){
      u_probability <- as.list(u_probability)
    }
    err <- lapply(u_probability, function(x) !all(x >= 0 & x <= 1))
    err <- unlist(err, use.names = FALSE)
    if(length(err[err]) > 0){
      err <- paste0("u-probabilities must be between 0 and 1:\n",
                    paste0("X - `u_probability[[", which(err), "]]` is ", u_probability[[which(err)]], ".", collapse = "\n"))
      return(err)
    }
  }
  return(FALSE)
}

err_sub_criteria_0 <- function(...,
                               match_funcs = match_funcs,
                               equal_funcs = equal_funcs,
                               operator = operator){
  err <- err_sub_criteria_1(...)
  if(!isFALSE(err)) return(err)

  err <- err_sub_criteria_3dot_1(...)
  if(!isFALSE(err)) return(err)

  err <- err_sub_criteria_funcs(..., funcs = match_funcs, funcs_l = "match_funcs")
  if(!isFALSE(err)) return(err)

  err <- err_sub_criteria_funcs(..., funcs = equal_funcs, funcs_l = "match_funcs")
  if(!isFALSE(err)) return(err)

  return(FALSE)
}


err_3dot_lens <- function(...){
  args <- list(...)
  err <- lapply(list(...), length)
  err <- unlist(err, use.names = FALSE)
  err <- sort(err[!duplicated(err)])

  if(length(err) != 1 | all(err == 0)){
    paste0("Different lengths for each attribute (`...`) in `attrs()`:\n",
           "i - Each attribute in (`...`) must have the same length.\n",
           paste0("X - `...` ", "contains elements of lengths ", listr(err),".", collapse = "\n"))
  }else{
    FALSE
  }
}

err_make_pairs_1 <- function(x = 1, strata = NULL,
                             repeats_allowed = TRUE,
                             permutations_allowed = TRUE){

  # Check for non-atomic vectors
  args <- list(x = x,
               strata = strata,
               repeats_allowed = repeats_allowed,
               permutations_allowed = permutations_allowed)

  err <- mapply(err_atomic_vectors,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object types
  args <- list(repeats_allowed = repeats_allowed,
               permutations_allowed = permutations_allowed)

  args_classes <- list(repeats_allowed = "logical",
                       permutations_allowed = "logical")

  err <- mapply(err_object_types,
                args,
                as.list(names(args)),
                args_classes[match(names(args), names(args_classes))])
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for required object lengths
  len_lims <- c(1, length(x))
  args <- list(strata = strata,
               repeats_allowed = repeats_allowed,
               permutations_allowed = permutations_allowed)

  args_lens <- list(strata = c(0, len_lims),
                    repeats_allowed = 1,
                    permutations_allowed = 1)

  err <- mapply(err_match_ref_len,
                args,
                rep(as.list("x"), length(args_lens)),
                args_lens[match(names(args), names(args_lens))],
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  # Check for missing values where they are not permitted
  args <- list(repeats_allowed = repeats_allowed,
               permutations_allowed = permutations_allowed)

  err <- mapply(err_missing_check,
                args,
                as.list(names(args)))
  err <- unlist(err, use.names = FALSE)
  err <- err[err != FALSE]
  if(length(err) > 0) return(err[1])

  return(FALSE)
}

err_option_level <- function(x,  valid_opts = c("", "r", "e", "w"), arg_nm = "case_length"){
  if(inherits(x, "list") & !is.null(names(x))){
    opts <- names(x)
    opts <- opts[!opts %in% valid_opts]

    if(length(opts) > 0){
      err_title <- paste0("Invalid option level for `", arg_nm, "`:\n")
      errs <- paste0(err_title,
                     "i - valid option levels are: ", listr(paste0("\"",valid_opts,"\"")), ".\n",
                     "i - Syntax ~ ", paste0("`(... ", arg_nm, " = list(\"w\" = c(\"14\",\"14\",\"28\",\"29\")))`"),"\n",
                     "i - Syntax ~ ", paste0("`(... ", arg_nm, " = list(\"w\" = c(\"14\",\"14\",\"28\",\"29\")))`"),"\n",
                     paste0("X - You've supplied ", listr(paste0("\"",opts,"\""), lim = 3)))
      return(errs)
    }else{
      FALSE
    }
  }else{
    FALSE
  }
}
