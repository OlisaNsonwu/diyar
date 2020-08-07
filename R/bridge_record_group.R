bridge_record_group <- function(df, args){

  if(class(df) != "data.frame"){
    stop(paste0("`df` must be a `data.frame`:\n",
                "You've supplied a ", class(df), "object."), call. = F)
  }

  # Missing args ####
  errs <- lapply("criteria", function(x){
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
  cols <- c("sn")
  multi_cols <- c("data_source", "strata")

  errs <- args[names(args) %in% unique(c(cols,
                                         multi_cols,
                                         enq_vr(args[names(args) == "criteria"][[1]]),
                                         unlist(args[names(args) == "sub_criteria"])))]
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

  # Pass args to `links()` ####
  # Extract atomic vectors from `df`
  cols_args <- args[names(args)  %in% cols]

  # Criteria
  cri_args <- args[names(args) == "criteria"]

  # Sub Criteria
  sub_cri_args <- args[names(args) == "sub_criteria"]

  # Display
  display_arg <- args[names(args) == "display"]

  # strata and data_source columns
  multi_args <- args[names(args)  %in% multi_cols]

  # to_s4
  to_s4_arg <- args[names(args) == "to_s4"]

  # The rest can go through as they are
  remainder <- args[!names(args) %in% c(names(cols_args), names(multi_args), "criteria", "sub_criteria", "display", "to_s4")]

  args <- ""
  if(length(cols_args) > 0){
    cols_args <- lapply(cols_args, function(x){
      if(all(as.character(x) %in% c("NULL", "Inf", "TRUE", "FALSE"))){
        NA_character_
      }else{
        paste0("df$", x)
      }
    })
    cols_args <- unlist(cols_args, use.names = T)
    cols_args <- cols_args[!is.na(cols_args)]
    cols_args <- paste0(names(cols_args), " = ", cols_args, collapse = ", ")
    args <- cols_args
  }

  if(length(remainder) > 0){
    rest_txt <- paste0(names(remainder), " = eval(remainder$", names(remainder), ")", collapse = ", ")
    args <- paste0(args, ", ", rest_txt)
  }

  if(length(multi_args) > 0){
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
    multi_txt <- paste0(names(multi_args), " = eval(multi_args$", names(multi_args), ")", collapse = ", ")
    args <- paste0(args, ", ", multi_txt)
  }

  if(length(cri_args) > 0){
    cri_args <- enq_vr(cri_args$criteria)
    cri_args <- paste0("criteria = list(", paste0("df$", cri_args, collapse = ", "), ")")
    args <- paste0(args, ", ", cri_args)
  }
  if(length(sub_cri_args) > 0){
    sub_cri_args <- eval(sub_cri_args$sub_criteria)
    sub_cri_args <- lapply(1:length(sub_cri_args), function(x){
      paste0("sub_criteria = sub_criteria(", paste0("df$", sub_cri_args[[x]], collapse = ", "), ")")
    })
    sub_cri_args <- as.character(sub_cri_args)
    sub_cri_args <- paste0("c",gsub("[^0-9]", "", names(sub_cri_args)), " = list(", sub_cri_args,")", collapse = ",")
    args <- paste0(args, ", ", sub_cri_args)
  }
  if(length(display_arg) > 0){
    display_arg$display <- ifelse(all(eval(display_arg$display) == F), "none", "stats")
    display_txt <- paste0("display = eval(display_arg$display)", collapse = ", ")
    args <- paste0(args, ", ", display_txt)
  }

  # Pass to `episodes`
  args <- paste0("diyar::links(", gsub("^,", "", args), ")" )
  if(length(to_s4_arg) > 0){
    to_s4 <- ifelse(all(eval(to_s4_arg$to_s4) == F), F, T)
    args <- ifelse(to_s4 == F, paste0("diyar::to_df(", args, ")"), args)
  }
  # Eval
  pids <- eval(parse(text=args))
  # Warn
  warning(paste0("`record_group()` has been retired!:\n",
                 "i - Please use `links()` instead.\n",
                 "i - Your values were passed to `links()`."), call. = F)
  return(pids)
}
