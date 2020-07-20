bridge_episode_group <- function(df, args){

  if(class(df) != "data.frame"){
    stop(paste0("`df` must be a `data.frame`:\n",
                "You've supplied a ", class(df), "object."), call. = F)
  }

  # Missing args ####
  errs <- lapply(c("date", "case_length"), function(x){
    if(!x %in% names(args)){
      paste0("argument \"", x,"\" is missing, with no default")
    }else{
      NA_character_
    }
  })
  errs <- unlist(errs, use.names = F)
  errs <- errs[!is.na(errs)]
  if(length(errs) > 0) stop(errs[1], call. = F)

  # Check for cols in data.frame ####
  cols <- c("sn", "case_length", "recurrence_length",
            "skip_order", "date", "overlap_methods")
  multi_cols <- c("data_source", "strata")
  csort_col <- "custom_sort"

  errs <- args[names(args)  %in% c(cols, multi_cols, csort_col)]
  errs <- lapply(errs, function(x){
    x <- enq_vr(x)
    if(all(as.character(x) %in% c("NULL", "Inf", "TRUE", "FALSE"))){
      NA_character_
    }else{
      x
    }
  })
  errs <- unlist(errs, use.names = T)
  errs <- errs[!is.na(errs)]

  errs <- lapply(errs, function(x){
    ifelse(x %in% names(df), NA_character_, x)
  })
  errs <- unlist(errs, use.names = T)
  errs <- errs[!is.na(errs)]
  if(length(errs) > 0){
    stop(paste0("Invalid column name at `", names(errs[1]), "`:\n",
                "X - `",errs[1], "` not found in `df`."), call. = F)
  }

  # Pass args to `episodes()` ####
  # Extract atomic vectors from `df`
  cols_args <- args[names(args)  %in% cols]
  cols_args <- lapply(cols_args, function(x){
    if(all(as.character(x) %in% c("NULL", "Inf", "TRUE", "FALSE"))){
      NA_character_
    }else{
      paste0("df$",x)
    }
  })
  cols_args <- unlist(cols_args, use.names = T)
  cols_args <- cols_args[!is.na(cols_args)]

  # strata and data_source columns
  multi_args <- args[names(args)  %in% multi_cols]
  multi_args <- lapply(multi_args, function(x){
    if(length(x) ==1){
      if(as.character(x) %in% c("NULL", "Inf", "TRUE", "FALSE") ){
        x
      }else{
        paste0("df$",x)
      }
    }else if(length(x) > 1){
       paste0("paste0(", paste0("df$", x, collapse = ", "), ", sep ='-')")
    }

  })
  multi_args <- multi_args[!is.na(multi_args)]

  # Handle - custom_sort
  csort_args <- args[names(args)  %in% csort_col]
  csort_cols <- enq_vr(csort_args[[1]])
  if(is.null(csort_cols)){
    csort_txt <- "custom_sort = eval(csort_args$custom_sort)"
  }else if(length(csort_cols) > 1){
    srd <- lapply(csort_cols, function(x){
      x <- as.numeric(as.factor(df[[x]]))
      formatC(x, width= nchar(max(x)), flag=0, format = "fg")
      })

      names(srd) <- csort_cols
      srd <- as.data.frame(srd, stringsAsFactors = F)
      srd <- eval(parse(text = paste0("paste0(",paste0("srd$", csort_cols, collapse = ",'-',"),")")))
      srd <- as.numeric(as.factor(srd))
      csort_txt <- "custom_sort = eval(srd)"
  }else{
    csort_txt <- paste0("custom_sort = eval(", paste0("df$", csort_cols), ")")
  }

  # The rest can go through as they are
  remainder <- args[!names(args) %in% c(names(cols_args), names(multi_args), csort_col)]
  cols_args <- paste0(names(cols_args), " = ", cols_args, collapse = ", ")
  args <- cols_args
  if(length(remainder) > 0){
    rest_txt <- paste0(names(remainder), " = eval(remainder$", names(remainder), ")", collapse = ", ")
    args <- paste0(args, ", ", rest_txt)
  }
  if(length(multi_args) > 0){
    multi_txt <- paste0(names(multi_args), " = eval(multi_args$", names(multi_args), ")", collapse = ", ")
    args <- paste0(args, ", ", multi_txt)
  }
  if(length(csort_cols) > 0){
    args <- paste0(args, ", ", csort_txt)
  }

  # Pass to `episodes`
  args <- paste0("diyar:::episodes(", args, ")" )
  # Eval
  eval(parse(text=args))
  # Warn
  warning(paste0("`episode_group()` has been retired!:\n",
                 "i - Please use `episodes()`, `fixed_episodes()` or `rolling_episodes()` instead.\n",
                 "i - Your values were passed to `episodes()`."), call. = F)
}
