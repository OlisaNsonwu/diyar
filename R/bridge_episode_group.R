bridge_episode_group <- function(df, args, episode_type){

  if(all(class(df) != "data.frame")){
    return(paste0("`df` must be a `data.frame`:\n",
                "You've supplied a ", class(df), " object."))
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
  if(length(errs) > 0){
    out <- list(err_cd = F, err_nm = errs[1])
    return(out)
  }

  if(any(class(episode_type) != "character")){
    out <- list(err_nm = paste0("Invalid object type for `episode_type`:\n",
                                "i - Valid object type is: `character`.\n",
                                "X - You've supplied a ", class(episode_type), " object type."),
                err_cd = F)
    return(out)
  }

  if(length(episode_type) != 1){
    out <- list(err_nm = paste0("Length of `episode_type` must be 1:\n",
                                "X - Length is ", length(episode_type)),
                err_cd = F)
    return(out)
  }

  if(length(episode_type) != 1){
    err <- paste0("Length of `episode_type` must be 1:\n",
                  "X - Length is ", length(episode_type), ".")
    return(err)
  }

  if(!tolower(episode_type) %in% c("rolling","fixed")){
    out <- list(err_nm = paste0("Invalid values for `episode_type`:\n",
                                "i - Vaild values are: \"fixed\", or \"rolling\".\n",
                                "X - You've supplied ", episode_type, "."),
                err_cd = F)
    return(out)
  }

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
    out <- list(
      err_cd = F,
      err_nm = paste0("Unknown columns supplied to `", names(errs[1]), "`:\n",
                      "X - `", errs[1], "` column not found in `df`.")
    )
    return(out)
  }

  # Pass args to `episodes()` ####
  # Extract atomic vectors from `df`
  cols_args <- args[names(args)  %in% cols]

  # strata and data_source columns
  multi_args <- args[names(args)  %in% multi_cols]

  # Handle - custom_sort
  csort_args <- args[names(args)  %in% csort_col]

  # to_s4
  to_s4_arg <- args[names(args) == "to_s4"]

  # Display
  display_arg <- args[names(args) == "display"]

  # The rest can go through as they are
  remainder <- args[!names(args) %in% c(names(cols_args), names(multi_args), csort_col, "to_s4", "display")]
  args <- ""
  if(length(cols_args) > 0){
    cols_args <- lapply(cols_args, function(x){
      if(all(as.character(x) %in% c("NULL", "Inf", "TRUE", "FALSE"))){
        NA_character_
      }else{
        paste0("df$",x)
      }
    })
    cols_args <- unlist(cols_args, use.names = T)
    cols_args <- cols_args[!is.na(cols_args)]
    cols_args <- paste0(names(cols_args), " = ", cols_args, collapse = ", ")
    args <- cols_args
  }
  args <- cols_args
  if(length(remainder) > 0){
    rest_txt <- paste0(names(remainder), " = eval(remainder$", names(remainder), ")", collapse = ", ")
    args <- paste0(args, ", ", rest_txt)
  }

  if(length(multi_args) > 0){
    multi_args <- lapply(multi_args, function(x){
      if(length(enq_vr(x)) == 1){
        if(as.character(enq_vr(x)) %in% c("NULL", "Inf", "TRUE", "FALSE") ){
          x
        }else{
          paste0("df$", enq_vr(x))
        }
      }else if(length(enq_vr(x)) > 1){
        paste0("paste(", paste0("df$", unique(enq_vr(x)), collapse = ", "), ", sep ='-')")
      }

    })
    multi_args <- unlist(multi_args[!is.na(multi_args)], use.names = T)
    multi_txt <- paste0(names(multi_args), " = ", multi_args, collapse = ", ")
    args <- paste0(args, ", ", multi_txt)
  }
  if(length(csort_args) > 0){
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
    args <- paste0(args, ", ", csort_txt)
  }

  if(length(display_arg) > 0){
    if(any(class(eval(display_arg$display)) == "logical")){
      display_arg$display <- ifelse(all(eval(display_arg$display) == F), "none", "stats")
    }
    display_txt <- paste0("display = eval(display_arg$display)", collapse = ", ")
    args <- paste0(args, ", ", display_txt)
  }

  # Pass to `episodes()`
  args <- paste0(ifelse(episode_type == "rolling", "diyar::rolling_episodes", "diyar::fixed_episodes"), "(", args, ")" )
  if(length(to_s4_arg) > 0){
    to_s4 <- ifelse(all(eval(to_s4_arg$to_s4) == F), F, T)
    args <- ifelse(to_s4 == F, paste0("diyar::to_df(", args, ")"), args)
  }
  # Eval
  epids <- eval(parse(text=args))
  return(list(err_cd = args, err_nm = epids))
}
