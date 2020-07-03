# @title Data validations
#
# @description Data validations
#

err_checks_epid <- function(sn, date, case_length, strata, display, episodes_max, episode_unit,
                            overlap_methods, skip_order, custom_sort){
  # Check object classes
  args_classes <- list(
    date = c("Date","POSIXct","POSIXt","POSIXlt","number_line","numeric","integer"),
    episodes_max = c("numeric","integer"),
    episode_unit = c("character"),
    overlap_methods = c("character"),
    skip_order = c("numeric","integer")
  )

  args_check <- function(arg, args_classes){
    err <-  paste0("i Valid object types for `", arg, "` are: ", listr(paste0("`", args_classes, "`"), "or"),".\n",
                   "X You've supplied a ", listr(paste0("`", class(arg), "`"), "or"), " object.")

    ifelse(all(class(get(arg)) != args_classes),
           err, NA_character_)
  }

  errs <- mapply(args_check, names(args_classes), args_classes)
  errs <- errs[!is.na(errs)]

  if(length(errs)>0){
    errs <- paste0("Incorrect object type for ", listr(paste0("`", names(errs), "`")), ":\n",
                   paste0(errs, collapse = "\n"))
    return(errs)
  }

  # Data type for case length
  eps <- as.list(case_length)
  errs <- unlist(lapply(eps, function(x){
    ifelse(
      !all(class(x) %in% c("integer","double","numeric", "number_line")),
      class(x), NA_character_)
  }), use.names = F)

  names(errs) <- 1:length(eps)

  errs <- errs[!is.na(errs)]
  if(length(errs)>0){
    errs <- paste0("Incorrect data type for `case_length`:\n",
                   "i Every `case_length` must be a `numeric` or `number_line` object.\n",
                   paste0("X `case_length-", names(errs),"` is a `", errs, "` object.", collapse = "\n"))
    return(errs)
  }

  # Check argument lengths
  int <- as.number_line(date)

  errs <- unlist(lapply(eps, length), use.names = F)
  names(errs) <- 1:length(eps)
  errs <- errs[!errs %in% c(1, length(int))]

  if(length(errs)>0){
    errs <- paste0("Argument lengths differ:\n",
                   "i Length of every `case_length` must be 1 or ", length(int), ".\n",
                   paste0("X Length of `case_length-", names(errs), "` is ", errs,".", collapse="\n"))
    return(errs)
  }

  lims <- c(1, length(int))
  args_lens <- list(
    sn = lims,
    date = lims,
    case_length = lims,
    strata = lims,
    overlap_methods = lims,
    episode_unit = 1,
    display = 1,
    episodes_max = lims
  )


  args_len_check <- function(arg_nms, args_lens){
    err <-  paste0("X Length of `", arg_nms, "` is ", length(get(arg_nms)), ".\n")
    ifelse(!length(get(arg_nms)) %in%  args_lens,
           err, NA_character_)
  }

  errs <- mapply(args_len_check, names(args_lens), args_lens)
  errs <- errs[!is.na(errs)]

  if(length(errs)>0){
    errs <- paste0("Argument lengths differ:\n",
                   "i Length of every argument must be 1 or ", length(int), ".\n",
                   paste0(errs, collapse = ""))
    return(errs)
  }

  if(length(episode_unit)!=1 |
     !episode_unit %in% names(diyar::episode_unit)
  ){
    return(paste0("Invalid values for `episode_unit`:\n",
                "i Vaild values are: \"seconds\", \"minutes\", \"hours\", \"days\", \"weeks\", \"months\" or \"years\".\n",
                "X You've supplied ", paste0("\"" , episode_unit, "\"", collapse = ", ")))
  }


  # Episode max
  emx_check <- missing_check(episodes_max)
  if(emx_check!=T) return(paste0("`episode_max` must contain non-missing values:\n",
                               "X There are missing values in ", emx_check))

  # Overlap methods
  o <- unique(unlist(strsplit(overlap_methods[!duplicated(overlap_methods)], split="\\|")))
  o <- o[!tolower(o) %in% c("exact", "across","chain","aligns_start","aligns_end","inbetween", "overlap", "none")]
  if (length(o)>0) {
    return(paste0("Invalid option for `overlap_method`:\n",
                "i Valid options are; \"overlap\", \"exact\", \"across\", \"chain\", \"aligns_start\", \"aligns_end\", \"inbetween\" or \"none\".\n",
                "i Syntax 1 ~ \"method1|method2|method3...\".\n",
                "i Syntax 2 ~ use `include_overlap_method()` or `exclude_overlap_method()`.\n",
                "X You've included ", paste0("\"",o,"\"", collapse = " ,"), ".\n"))
  }

  # Record indentifier
  if(!is.null(sn)){
    dp_check <- duplicates_check(sn)
    if(dp_check!=T) return(paste0("`sn` must be unique:\n",
                                "X Duplicate values in ",dp_check,"."))
    fn_check <- finite_check(sn)
    if(fn_check!=T) return(paste0("`sn` must have finite numeric values:\n",
                                "X There are non-finite values in ",fn_check))
  }

  return(F)
}
