#' @name reframe_repeats
#' @title Reframe repeats records for \code{\link{episodes_wf_repeats()}}.
#'
#' @description Helper functions to reframe repeats records of
#' \code{\link{sub_critieria}} attributes. These are intended for use with
#' \code{\link{episodes_wf_repeats}}.
#'
#' @param x \code{[vector]} Repeat values to be reframed
#' @param repeat_id \code{[vector]} Groups. \code{x} is reframed along
#' \code{repeat_id} such that there is one element per group
#'
#' @seealso
#' \code{\link{episodes_wf_repeats}}; \code{\link{sub_criteria}};
#' \code{\link{reframe}}
#'
#' @details
#' \code{\link{episodes_wf_repeats()}} removes duplicate records that will not
#' affect a case definition before passing them to \code{\link{episodes()}}.
#' The same records must also be removed or reframed in
#' \code{\link{sub_criteria}} objects supplied to \code{case_sub_criteria} and
#' \code{recurrence_sub_criteria}. Depending on the intended behaviour of the
#' \code{\link{sub_criteria}} object's \code{match_funcs} and \code{equal_funcs},
#' duplicate records may need to be reframed in different ways.
#'
#' \code{subset_repeats()} and \code{split_repeats()} are convenience
#' implementations of a repeat reframer. Custom reframers can also be
#' supplied to \code{\link{episodes_wf_repeats}}
#' through \code{sub_criteria_reframer}. A reframer function must have the
#' following:
#' \itemize{
#' \item Two arguments - \code{x} and \code{repeat_id}.
#' \item Output with one element per group (\code{repeat_id}).
#' }
#'
#' \bold{\code{subset_repeats()}} - Keep one value per group (\code{repeat_id})
#' and include the number of repeats records per group.
#'
#' \bold{\code{split_repeats()}} - Split repeat values by the group
#' (\code{repeat_id}) return a list containing one element per group.
#' Each element contains the original values and
#' the number of repeat records in the group.
#'
#' @examples
#' # 3 repeat records for 3 events 3 repeat records for 3 events
#' dfr <- data.frame(date = rep(1:3, rep(3, 3)))
#'
#' # Additional match criteria to only create episodes with >5 events
#' five_plus_events <- function(x, y) bys_count(y) >= 5
#' sub_cri <- sub_criteria(dfr$date, match_funcs = five_plus_events)
#' # All events linked to one episode since there are 9 events (records)
#' dfr$ep1 <- episodes(
#'   dfr$date, case_length = 3, case_sub_criteria = sub_cri,
#'   group_stats = 'case_nm')
#'
#' # The same analysis with `episodes_wf_repeats` does not
#' # link the events because repeats are excluded leaving only 3 events
#' dfr$ep2 <- episodes_wf_repeats(
#'   dfr$date, sub_criteria_reframer = subset_repeats,
#'   case_length = 3, case_sub_criteria = sub_cri,
#'   group_stats = 'case_nm')
#'
#' # Resolving this require the analysis to be reframe as:
#' # Options 1: Additional match criteria to only create episodes with >3 events
#' three_plus_events <- function(x, y) bys_count(y) >= 3
#' sub_cri2 <- sub_criteria(dfr$date, match_funcs = three_plus_events)
#' dfr$ep3 <- episodes_wf_repeats(
#'   dfr$date, sub_criteria_reframer = subset_repeats,
#'   case_length = 3, case_sub_criteria = sub_cri2,
#'   group_stats = 'case_nm')
#'
#' # OR
#' # Options 1: reframing the `sub_criteria`'s attributes to include
#' # the count of repeats. The initial match criteria can then be implemented of
#' # the sum of these counts.
#' five_plus_events_b <- function(x, y) bys_sum(by = y$sn, y$repeats) >= 5
#' sub_cri3 <- sub_criteria(dfr$date, match_funcs = five_plus_events_b)
#' dfr$ep4 <- episodes_wf_repeats(
#'   dfr$date, sub_criteria_reframer = split_repeats,
#'   case_length = 3, case_sub_criteria = sub_cri3,
#'   group_stats = 'case_nm')
#'
#' # the reframe analysis (`ep3` and `ep4`) now leads to
#' # the intended outcome (`ep1`)
#' dfr
#'
#' @rdname reframe_repeats
#' @export
subset_repeats <- function(x, repeat_id){
  if(length(x) > 1) x <- x[!duplicated(repeat_id)]
  return(x)
}

#' @rdname reframe_repeats
#' @export
split_repeats <- function(x, repeat_id){
  if(length(x) > 1){
    dups_lgk <- !duplicated(repeat_id)
    x_unq <- x[dups_lgk]; x_repeat_n <- bys_count(repeat_id)[dups_lgk]
    x <- attrs(sn = seq_len(length(x_unq)), attribute = x_unq, repeats = x_repeat_n)
  }
  return(x)
}
