bridge_record_group <- function(df, args){

  if(all(class(df) != "data.frame")){
    out <- list(
      err_cd = F,
      err_nm = paste0("`df` must be a `data.frame`:\n",
                      "You've supplied a ", class(df), "object."))
    return(out)
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
  if(length(errs) > 0){
    out <- list(err_cd = F, err_nm = errs[1])
    return(out)
  }

  # Check for cols in data.frame ####
  cols <- c("sn")
  multi_cols <- c("data_source", "strata")

  errs <- args[names(args) %in% unique(c(cols, multi_cols, "criteria"))]
  errs <- lapply(c(errs, eval(args$sub_criteria)),  function(x){
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

  # Check if expecting range matching

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

  if(length(cri_args) > 0){
    cri_args <- enq_vr(cri_args$criteria)
    lgk <- lapply(cri_args, function(xx){
      any(class(df[[xx]]) == "number_line") & attr(class(df[[xx]]), "package") == "diyar"
    })

    lgk <- unlist(lgk, use.names = F)
    if(length(lgk[lgk]) == 0){
      cris <- paste0("df$", cri_args)
      supp_cri <- as.character()
    }else{
      cris <- ifelse(lgk == T, "1",  paste0("df$", cri_args))
      supp_cri <- paste0("sub_criteria(df$", cri_args[lgk], ", match_funcs = diyar::range_match_legacy)")
      names(supp_cri) <- paste0("cr", (1:length(cri_args))[lgk])
    }

    cri_args <- paste0("criteria = list(", paste0(cris, collapse = ", "), ")")
    args <- paste0(args, ", ", cri_args)
  }

  if(length(sub_cri_args) > 0 | length(supp_cri) > 0){
    sub_cri_args <- eval(sub_cri_args$sub_criteria)
    if(length(sub_cri_args) > 0){
      nms <- names(sub_cri_args)
      sub_cri_args <- sapply(1:length(sub_cri_args), function(x){
        funcs <- lapply(sub_cri_args[[x]], function(xx){
          ifelse(any(class(df[[xx]]) == "number_line"), "diyar::range_match_legacy", "diyar::exact_match")
        })
        funcs <- paste0("list(", paste0(unlist(funcs, use.names = F), collapse = ", "), ")")
        paste0("sub_criteria(", paste0("df$", sub_cri_args[[x]], collapse = ", "), ", match_funcs = ", funcs, ", operator = \"or\")")
      })
      names(sub_cri_args) <- paste0("cr", gsub("[^0-9]", "", nms))
    }else{
      sub_cri_args <- as.character()
    }

    sub_cri_args <- sapply(split(sub_cri_args, names(sub_cri_args)), function(x){
      if(length(x) == 1){
        paste0(x, collapse = ", ")
      }else{
        paste0("sub_criteria(", paste0(x, collapse = ", "), ", operator = \"and\")")
      }
    })
    nm <- c(names(sub_cri_args), names(supp_cri))
    sub_cri_args <- c(sub_cri_args, supp_cri)
    sub_cri_args <- paste0(nm, " = ", sub_cri_args, collapse = ", ")
    sub_cri_args <- paste0("list(", sub_cri_args, ")")
    args <- paste0(args, ", sub_criteria = ", sub_cri_args)
  }

  if(length(display_arg) > 0){
    if(any(class(eval(display_arg$display)) == "logical")){
      display_arg$display <- ifelse(eval(display_arg$display) == TRUE,
                                    "stats",
                                    ifelse(eval(display_arg$display) == FALSE,
                                           "none",
                                           eval(display_arg$display))
      )
    }
    display_txt <- paste0("display = eval(display_arg$display)", collapse = ", ")
    args <- paste0(args, ", ", display_txt)
  }

  # Pass to `links()`
  args <- paste0("diyar::links(", gsub("^,", "", args), ")" )
  if(length(to_s4_arg) > 0){
    to_s4 <- ifelse(all(eval(to_s4_arg$to_s4) == F), F, T)
    args <- ifelse(to_s4 == F, paste0("as.data.frame(", args, ", stringsAsFactors = FALSE)"), args)
  }
  # Eval
  pids <- eval(parse(text = args))
  return(list(err_cd = args, err_nm = pids))
}
