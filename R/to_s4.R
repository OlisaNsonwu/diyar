# @title Change S4 objects in \code{diyar} to data frames and vice versa
#
# @description Convert \link[=pid-class]{pid}, \link[=epid-class]{epid}, \link[=pane-class]{pane} or \link[=number_line-class]{number_line} objects
# to a \code{data.frame} and vice versa.
#
# @aliases to_s4
#
# @param lst \code{[list]}
#
# @rdname to_s4
# @param s4 \code{[\link[=pid-class]{pid}|\link[=epid-class]{epid}|\link[=pane-class]{pane}|\link[=number_line-class]{number_line}]}
# @param ... Arguments passed to \code{data.frame}
# @return to_df - \code{data.frame} object
# @details \code{to_df} has been retired. Moving forward, please use \code{as.data.frame}.
to_df <- function(s4, ...) as.data.frame(s4, ...)

# @return to_s4 - \link[=pid-class]{pid}, \link[=epid-class]{epid}, \link[=pane-class]{pane} or \link[=number_line-class]{number_line} objects
 to_s4 <- function(lst){
   lst <- as.list(lst)

   if(any(names(lst) == "epid")){
     s4 <- methods::new("epid", .Data=lst$epid)
     s4@wind_id <- list(wind_id = lst[grepl("wind_id", names(lst))])
     s4@epid_interval <- number_line(l = lst$epid_start,
                                     r = lst$epid_end,
                                     id = lst$sn,
                                     gid = lst$wind_id1)
   }else if(any(names(lst) == "pane")){
     s4 <- methods::new("pane", .Data=lst$pane)
     s4@epid_interval <- number_line(l = lst$pane_start,
                                     r = lst$pane_end,
                                     id = lst$sn)
   }else if(any(names(lst) == "pid")){
     s4 <- methods::new("pid", .Data=lst$pid)
   }else if(any(names(lst) == "gid")){
     s4 <- methods::new("number_line", .Data = as.numeric(lst$end) - as.numeric(lst$start))
   }

   vrs <- subset(names(lst), names(lst) %in% methods::slotNames(s4))

   for(i in vrs){
     if(all(class(lst[[i]]) == "d_label")){
       methods::slot(s4, i) <- encode(lst[[i]])
     }else{
       methods::slot(s4, i) <- lst[[i]]
     }
   }
   s4
 }
