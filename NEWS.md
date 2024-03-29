
# Version 0.5.1.9000

## New features

## Changes

## Bug fixes

# Version 0.5.1

## New features

## Changes

## Bug fixes

- `links()` - Incorrect results in some situations. Resolved.
- `links_af_probabilistic()` - Failed in some situations. Resolved.

# Version 0.5.0

## New features

- New option (`"semi"`) for the `batched` argument in `links()`. All
  matches are compared against the record-set in the next iteration.
  Therefore, the number of record-pairs increase exponentially as new
  matches are found. This means fewer record-pairs (memory usage) but a
  longer run time compared to the `"no"` option. Conversely, it leads to
  more record-pairs (memory usage) but a shorter run time compared to
  the `"yes"` option.
- New argument (`batched`) in `episodes()`
- New argument (`split`) in `episodes()`. Split the analysis in
  `N`-splits of `strata`. This leads to fewer record-pairs (and memory
  usage) but a longer run time.
- New argument (`decode`) in `as.data.frame.pid()`,
  `as.data.frame.epid()` and `as.data.frame.pane()`
- New function - `episodes_af_shift()`. A more vectorised approach to
  `episodes()` based on `epidm::group_time()`.
- New function - `links_wf_episodes()`. Implantation of `episodes()`
  using `links()`.

## Changes

- Optimised `episodes()` and `links()`. Each iteration now uses less
  time and memory.
- `link_id` slot in `pid` objects is now a `list`.
- `links()` - records with missing values in a `sub_criteria` are now
  skipped at the corresponding iteration.
- Updated argument in `links()`- `recursive`. This now takes any of
  three options `[c("linked", "unlinked", "none")]` .
  `[c("linked", "unlinked")]` collectively were previously `[TRUE]`,
  while `["none"]` was previously `[FALSE]`.
- `as.epids()` now calls `make_episodes()`.
- The default value for the `window` argument in `partitions()` is now
  `NULL`
- `as.data.frame()` and `as.data.list()` now only creates
  elements/fields from non-empty fields
- `id` and `gid` slots in `number_line` objects are now `integer(0)` by
  default.
- `episode_group()`, `record_group()` and `range_match_legacy()` have
  been removed.
- `["recurisve"]` episodes from `episodes()` are now presented as
  `["rolling"]` episodes with `reference_event = "all_records"` i.e
  - `Old syntax ~ episodes(..., episode_type == "recursive")`
  - `New syntax ~ episodes(..., episode_type == "rolling", reference_event = "all_records")`

## Bug fixes

- When `recursive` was `TRUE`, `links()` ended prematurely and therefore
  missed some matches. Resolved.
- `recurrence_sub_criteria` in `episodes()` was not implemented
  correctly and lead to incorrect linkage result in some instances.
  Resolved.
- `overlap_method()` - logical tests recycled incorrectly. Resolved.
- `check_links` argument - Option `"g"` implemented as option `"l"`.
  Resolved.
- `make_pairs_wf_source()`. Created incorrect pairs. Resolved.
- `case_sub_criteria` and `recurrence_sub_criteria` in `episodes()` led
  to incorrect results. Resolved.

# Version 0.4.2

## New features

- New argument in `merge_ids()` - `shrink` and `expand`.
- New S3 method for class ‘d_report’ - `plot`.
- New S3 method for class ‘sub_criteria’ - `format`.
- New function - `true()`. Predefined logical test for use with
  `sub_criteria()`.
- New function - `false()`. Predefined logical test for use with
  `sub_criteria()`.
- New argument in `links()`- `batched`. Specify if all record pairs are
  created or compared at once (`"no"`) or in batches (`"yes"`).  
- New argument in `links()`- `repeats_allowed`. Specify if record-pairs
  with duplicate elements should be created.
- New argument in `links()`- `permutations_allowed`. Specify if
  permutations of the same record-pair should be created.
- New argument in `links()`- `ignore_same_source`. Specify if
  record-pairs from different datasets should be created.
  <!-- + New argument in `links_wf_probabilistic()`- `return_weights`. XXXXXX. -->
- New argument in `eval_sub_criteria()`- `depth`. First order of
  recursion.
- New function - `sets()` and `make_sets()`. Create permutations of
  record-sets.

## Changes

- `links()` - When `shrink` is `TRUE`, records in a record-group must
  meet every listed match `criteria` and `sub_criteria`. For example, if
  `pid_cri` is 3, then the record must have meet matched another on the
  the first three match criteria.
- `links()` - `pid@iteration` now tracks when a record was dealt with
  instead of when it was assigned to a record-group. For example, a
  record can be closed (matched or not matched) at iteration 1 but
  assigned to a record-group at iteration 5.
- `make_pairs()` - `x.*` and `y.*` values in the output are now swapped.
- `sub_criteria` can now export any data created by `match_func`. To do
  this, `match_func` must export a `list`, where the first element is a
  logical object. See an example below.

``` r
library(diyar)
val <- rep(month.abb[1:5], 2); val
#>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jan" "Feb" "Mar" "Apr" "May"
match_and_export <- function(x, y){
  output <- list(x == y, 
                 data.frame(x_val = x, y_val = y, is_match = x == y))
  return(output)
}
sub.cri.1 <- sub_criteria(
  val, match_funcs = list(match.export = match_and_export)
)

format(sub.cri.1, show_levels = TRUE)
#> logical_test-{
#> Lv.0.1-match.export(Jan,Feb,Mar ...)
#> }
eval_sub_criteria(sub.cri.1)
#> $logical_test
#>  [1] 1 0 0 0 0 1 0 0 0 0
#> 
#> $mf.0.1
#>    x_val y_val is_match
#> 1    Jan   Jan     TRUE
#> 2    Feb   Jan    FALSE
#> 3    Mar   Jan    FALSE
#> 4    Apr   Jan    FALSE
#> 5    May   Jan    FALSE
#> 6    Jan   Jan     TRUE
#> 7    Feb   Jan    FALSE
#> 8    Mar   Jan    FALSE
#> 9    Apr   Jan    FALSE
#> 10   May   Jan    FALSE
```

- `links` can now export any data created within a `sub_criteria`. To do
  this, the `sub_criteria` must be created as described above. See an
  example below

``` r
val <- 1:5
diff_one_and_export <- function(x, y){
  diff <- x - y
  is_match <- diff <= 1
  output <- list(is_match, 
                 data.frame(x_val = x, y_val = y, diff = diff,  is_match = is_match))
  return(output)
}
sub.cri.2 <- sub_criteria(
  val, match_funcs = list(diff.export = diff_one_and_export)
)
links(
  criteria = "place_holder", 
  sub_criteria = list("cr1" = sub.cri.2))
#> $pid
#> [1] "P.1 (CRI 001)" "P.1 (CRI 001)" "P.3 (CRI 001)" "P.3 (CRI 001)"
#> [5] "P.5 (No hits)"
#> 
#> $export
#> $export$cri.1
#> $export$cri.1$iteration.1
#> $export$cri.1$iteration.1$mf.0.1
#>   x_val y_val diff is_match
#> 1     1     1    0     TRUE
#> 2     2     1    1     TRUE
#> 3     3     1    2    FALSE
#> 4     4     1    3    FALSE
#> 5     5     1    4    FALSE
#> 
#> 
#> $export$cri.1$iteration.2
#> $export$cri.1$iteration.2$mf.0.1
#>   x_val y_val diff is_match
#> 1     3     3    0     TRUE
#> 2     4     3    1     TRUE
#> 3     5     3    2    FALSE
```

## Bug fixes

- `summary.epid()` - Incorrect count for ‘`by episode type`’. Resolved.
- `episodes()` - Incorrect results in some instances with `skip_order`.
  Resolved.
- `make_ids()` - Did not capture all records in that should be in a
  record-group when matches are recursive. Resolved.
- `make_pairs()` - Incorrect record-pairs in some instances. Resolved.
- `eval_sub_criteria()` - When output of `match_func` is length one,
  it’s not recycled. Resolved.
- `reverse_number_line()` - Incorrect results in some instances.
  Resolved.
- `links()`- Incorrect `iteration` (`pids` slot) for non-matches.
  Resolved.
- `links()` and `episodes()` - Timing for each iteration was incorrect.
  Resolved.

# Version 0.4.1

## New features

- New function - `overlap_method_names()`. Overlap methods for a
  corresponding overlap method codes.
- Memory usage added to `*with_report` options for display.

## Changes

- `"chain"` overlap method split into `"x_chain_y"` and `"y_chain_x"`.
  `"chain"` will continue to be supported as a keyword for
  `"x_chain_y" OR "y_chain_x"` method
- `"across"` overlap method split into `"x_across_y"` and
  `"y_across_x"`. `"across"` will continue to be supported as a keyword
  for `"x_across_y" OR "y_across_x"` methods
- `"inbetween"` overlap method split into `"x_inbetween_y"` and
  `"y_inbetween_x"`. `"inbetween"` will continue to be supported as a
  keyword for `"x_inbetween_y" OR "y_inbetween_x"` methods
- Optimised `overlaps()`.
- Some overlap method codes have changed. Please review any previously
  specified codes with `overlap_method_names()`.

## Bug fixes

- `make_batch_pairs()` (internal) created invalid record pairs.
  Resolved.

# Version 0.4.0

## New features

- New function - `reframe()`. Modify the attributes of a `sub_criteria`
  object.
- New function - `link_records()`. Record linkage by creating all record
  pairs as opposed to batches as with `link()`.
- New function - `make_pairs()`. Create every combination of
  records-pairs for a given dataset.
- New function - `make_pairs_wf_source()`. Create records-pairs from
  different sources only.
- New function - `make_ids()`. Convert an edge list to a group
  identifier.
- New function - `merge_ids()`. Merge two group identifiers.
- New function - `attrs()`. Pass a set of attributes to one instance of
  `match_funcs` or `equal_funcs`.

## Changes

- Optimised `episodes_wf_splits()`
- Optimised `episodes()` and `links()`. Reduced processing times.
- Three new options for the `display` argument.
  `"progress_with_report"`, `"stats_with_report"` and
  `"none_with_report"`. Creates a `d_report`; a status of the analysis
  over its run time.
- `eval_sub_criteria()`. Record-pairs are no longer created in the
  function. Therefore, `index_record` and `sn` arguments have been
  replaced with `x_pos` and `y_pos`.
- `link_records()` and `links_wf_probabilistic()`. The `cmp_threshold`
  argument has been renamed to `attr_threshold`.
- `show_labels` argument in `schema()`. Two new options - `"wind_nm"`
  and `"length"` to replace `"length_label"`.

## Bug fixes

- Incorrect `wind_id` list in `episodes(..., data_link = "XX")` in .
  Resolved.
- Incorrect `link_id` in `links(..., recursive = TRUE)`. Resolved.
- `iteration` not recorded in some situations with `episodes()`.
  Resolved.
- `skip_order` ends an open episode. Resolved.
- `NA` in `dist_wind_index` and `dist_epid_index` when `sn` is supplied.
  Resolved.
- `overlap_method_codes()` - overlap method codes not recycled properly.
  Resolved.

# Version 0.3.1

## New features

- New function - `delink()`. Unlink identifiers.
- New function - `episodes_wf_splits()`. Wrapper function of
  `episodes()`. Better optimised for handling datasets with many
  duplicate records.
- New function - `combi()`. Numeric codes for unique combination of
  vectors.
- New function - `attr_eval()`. Recursive evaluation of a function on
  each attribute of a `sub_criteria`.

## Changes

- Two new `case_nm` values - `Case_CR` and `Recurrence_CR` which are
  `Case` and `Recurrence` without a sub-criteria match.

## Bug fixes

- Corrected length arrows in `schema.epid`.
- Corrected outcome of `eval_sub_criteria` with 1 result.

# Version 0.3.0

## New features

- New function - `links_wf_probabilistic()`. Probabilistic record
  linkage.
- New function - `partitions()`. Spilt events into sections in time.
- New function - `schema()`. Plot schema diagrams for `pid`, `epid`,
  `pane` and `number_line` objects.
- New functions - `encode()` and `decode()`. Encode and decode slots
  values to minimise memory usage.
- New argument in `episodes()` - `case_sub_criteria` and
  `recurrence_sub_criteria`. Additional matching conditions for temporal
  links.
- New argument in `episodes()`- `case_length_total` and
  `recurrence_length_total`. Number of temporal links required for a
  `window`/`episode`.
- New argument in `links()` - `recursive`. Control if matches can spawn
  new matches.
- New argument in `links()` - `check_duplicates`. Control the checking
  of logical tests on duplicate values. If `FALSE`, results are recycled
  for the duplicates.
- `as.data.frame` and `as.list` S3 methods for the `pid`, `number_line`,
  `epid`, `pane` objects.
- New option for `episode_type` in `episodes()` - “recursive”. For
  recursive episodes where every linked events can be used as a
  subsequent index event.
- `recurrence_from_last` renamed to `reference_event` and given two new
  options.

## Changes

- `episodes()` and `links()`. Speed improvements.
- Default time zone for an `epid_interval` or `pane_interval` with
  `POSIXct` objects is now “GMT”.
- `number_line_sequence()` - splits number_line objects. Also available
  as a `seq` method.
- `epid_total`, `pid_total` and `pane_total` slots are populated by
  default. No need to used `group_stats` to get these.
- `to_df()` - Removed. Use `as.data.frame()` instead.
- `to_s4()` - Now an internal function. It’s no longer exported.
- `compress_number_line()` - Now an internal function. It’s no longer
  exported. Use `episodes()` instead.
- `sub_criteria()` - produces a `sub_criteria` object. Nested “AND” and
  “OR” conditions are now possible.
- `case_overlap_methods`, `recurrence_overlap_methods` and
  `overlap_methods` now take `integer` codes for different combinations
  of overlap methods. See `overlap_methods$options` for the full list.
  `character` inputs are still supported.

## Bug fixes

- `"Single-record"` was wrong in `links` summary output. Resolved.

# Version 0.2.0

## New features

- Better support for `Inf` in `number_line` objects.
- Can now use multiple `case_length` or `recurrence_length` for the same
  event.
  - Can now use multiple `overlap_methods` for the corresponding
    `case_length` and `recurrence_length`.
- New function `links()` to replace `record_group()`.
- New function `sub_criteria()`. The new way of supplying a
  `sub_criteria` in `links()`.
- New functions `exact_match()`, `range_match()` and
  `range_match_legacy()`. Predefined logical tests for use with
  `sub_criteria()`. User-defined tests can also be used. See
  `?sub_criteria`.
- New function `custom_sort()` for nested sorting.
- New function `epid_lengths()` to show the required `case_length` or
  `recurrence_length` for an analyses. Useful in confirming the required
  `case_length` or `recurrence_length` for episode tracking.
- New function `epid_windows()`. Shows the period a `date` will overlap
  with given a particular `case_length` or `recurrence_length`. Useful
  in confirming the required `case_length` or `recurrence_length` for
  episode tracking.
- New argument - `strata` in `links()`. Useful for stratified data
  linkage. As in stratified episode tracking, a record with a missing
  `strata` (`NA_character_`) is skipped from data linkage.
- New argument - `data_links` in `links()`. Unlink record groups that do
  not include records from certain data sources
- New convenience functions
  - `listr()`. Format `atomic` vectors as a written list.
  - `combns()`. An extension of `combn` to generate permutations not
    ordinarily captured by `combn`.
- New `iteration` slot for `pid` and `epid` objects
- New `overlap_method` - `reverse()`

## Changes

- `number_line()` - `l` and `r` must have the same length or be `1`.
- `episodes()` - `case_nm` differentiates between duplicates of `"Case"`
  (`"Duplicate_C"`) and `"Recurrent"` events (`"Duplicate_R"`).
- Strata and episode-level options for most arguments. This gives
  greater flexibility within the same instance of `episodes()`.
  - Episode-level - The behaviour for each episode is determined by the
    corresponding option for its index event (`"Case"`).
    - `episode_type` - simultaneously track both `"fixed"` and
      `"rolling"` episodes.
    - `skip_if_b4_lengths` - simultaneously track episodes where events
      before a cut-off range are both skipped and not skipped.
    - `episode_unit` - simultaneously track episodes by different units
      of time.
    - `case_for_recurrence` - simultaneously track `"rolling"` episodes
      with and without an additional case window for recurrent events.
    - `recurrence_from_last` - simultaneously track `"rolling"` episodes
      with reference windows calculated from the first and last event of
      the previous window.
  - Strata-level - The behaviour for each episode is determined by the
    corresponding option for its `strata`. Options must be the same in
    each strata.
    - `from_last` - simultaneously track episodes in both directions of
      time - past to present and present to past.
    - `episodes_max` - simultaneously track different number of episodes
      within the dataset.
- `include_overlap_method` - `"overlap"` and `"none"` will not be
  combined with other methods.
  - `"overlap"` - mutually inclusive with the other methods, so their
    inclusion is not necessary.
  - `"none"` - mutually exclusive and prioritised over the other methods
    (including `"none"`), so their inclusion is not necessary.
- Events can now have missing cut-off points (`NA_real_`) or periods
  (`number_line(NA_real_, NA_real_)`) `case_length` and
  `recurrence_length`. This ensures that the event does not become an
  index case however, it can still be part of different episode. For
  reference, an event with a missing `strata` (`NA_character_`) ensures
  that the event does not become an index case nor part of any episode.

## Bug fixes

- `fixed_episodes`, `rolling_episodes` and `episode_group` -
  `include_index_period` didn’t work in certain situations. Corrected.
- `fixed_episodes`, `rolling_episodes` and `episode_group` -
  `dist_from_wind` was wrong in certain situations. Corrected.

# Version 0.1.0

## New features

- New argument in `record_group()` - `strata`. Perform record linkage
  separately within subsets of a dataset.
- New argument in `overlap()`, `compress_number_line()`,
  `fixed_sepisodes()`, `rolling_episodes()` and `episode_group()` -
  `overlap_methods` and `methods`. Replaces `overlap_method` and
  `method` respectively. Use different sets of methods within the same
  dataset when grouping episodes or collapsing `number_line` objects.
  `overlap_method` and `method` only permits 1 method per per dataset.
- New slot in `epid` objects - `win_nm`. Shows the type of window each
  event belongs to i.e. case or recurrence window
- New slot in `epid` objects - `win_id`. Unique ID for each window. The
  ID is the `sn` of the reference event for each window
  - Format of `epid` objects updated to reflect this
- New slot in `epid` objects - `dist_from_wind`. Shows the duration of
  each event from its window’s reference event
- New slot in `epid` objects - `dist_from_epid`. Shows the duration of
  each event from its episode’s reference event
- New argument in `episode_group()` and `rolling_episodes()` -
  `recurrence_from_last`. Determine if reference events should be the
  first or last event from the previous window.
- New argument in `episode_group()` and `rolling_episodes()` -
  `case_for_recurrence`. Determine if recurrent events should have their
  own case windows or not.
- New argument in `episode_group()`, `fixed_episodes()` and
  `rolling_episodes()` - `data_links`. Unlink episodes that do not
  include records from certain `data_source(s)`.
- `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
  `case_length` and `recurrence_length` arguments. You can now use a
  range (`number_line` object).
- New argument in `episode_group()`, `fixed_episodes()` and
  `rolling_episodes()` - `include_index_period`. If `TRUE`, overlaps
  with the index event or period are grouped together even if they are
  outside the cut-off range (`case_length` or `recurrence_length`).
- New slot in `pid` objects - `link_id`. Shows the record (`sn` slot) to
  which every record in the dataset has matched to.
- New function - `invert_number_line()`. Invert the `left` and/or
  `right` points to the opposite end of the number line
- New accessor functions -`left_point(x)<-`, `right_point(x)<-`,
  `start_point(x)<-` and `end_point(x)<-`

## Changes

- `overlap()` renamed to `overlaps()`. `overlap()` is now a convenience
  `overlap_method` to capture ANY kind of overlap.
- `"none"` is another convenience `overlap_method` for NO kind of
  overlap
- `expand_number_line()` - new options for `point`; `"left"` and
  `"right"`
- `compress_number_line()` - compressed `number_line` object inherits
  the direction of the widest `number_line` among overlapping group of
  `number_line` objects
- `overlap_methods` - have been changed such that each pair of
  `number_line` objects can only overlap in one way. E.g.
  - `"chain"` and `"aligns_end"` used to be possible but this is now
    considered a `"chain"` overlap only
  - `"aligns_start"` and `"aligns_end"` use to be possible but this is
    now considered an `"exact"` overlap
- `number_line_sequence()` - Output is now a `list`.
- `number_line_sequence()` - now works across multiple `number_line`
  objects.
- `to_df()` - can now change `number_line` objects to data.frames.
  - `to_s4()` can do the reverse.
- `epid` objects are the default outputs for `fixed_episodes()`,
  `rolling_episodes()` and `episode_group()`
- `pid` objects are the default outputs for `record_group()`
- In episode grouping, the `case_nm` for events that were skipped due to
  `rolls_max` or `episodes_max` is now `"Skipped"`.
- In `episode_group()` and `record_group()`, `sn` can be negative
  numbers but must still be unique
- Optimised `episode_group()` and `record_group()`. Runs just a little
  bit faster …
- Relaxed the requirement for `x` and `y` to have the same lengths in
  overlap functions.
  - The behaviour of overlap functions will now be the same as that of
    standard R logical tests
- `episode_group` - `case_length` and `recurrence_length` arguments. Now
  accepts negative numbers.
  - negative “lengths” will collapse two periods into one, if the second
    one is within some days before the `end_point()` of the first
    period.
    - if the “lengths” are larger than the `number_line_width()`, both
      will be collapsed if the second one is within some days (or any
      other `episode_unit`) before the `start_point()` of the first
      period.
- cheat sheet updated

## Bug fixes

- Recurrence was not checked if the initial case event had no
  duplicates. Resolved
- `case_nm` wasn’t right for rolling episodes. Resolved

# Version 0.0.3

## Changes

- [\#7](https://github.com/OlisaNsonwu/diyar/issues/7)
  `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
  optimized to take less time when working with large datasets
- `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
  `date` argument now supports numeric values
- `compress_number_line()` - the output (`gid` slot) is now a group
  identifier just like in `epid` objects (`epid_interval`)

# Version 0.0.2

## New feature

- `pid` S4 object class for results of `record_group()`. This will
  replace the current default (`data.frame`) in the next major release
- `epid` S4 object class for results of `episode_group()`,
  `fixed_episodes()` and `rolling_episodes()`. This will replace the
  current default (`data.frame`) in the next release
- `to_s4()` and `to_s4` argument in `record_group()`, `episode_group()`,
  `fixed_episodes()` and `rolling_episodes()`. Changes their output from
  a `data.frame` (current default) to `epid` or `pid` objects
- `to_df()` changes `epid` or `pid` objects to a `data.frame`
- `deduplicate` argument from `fixed_episodes()` and
  `rolling_episodes()` added to `episode_group()`

## Changes

- `fixed_episodes()` and `rolling_episodes()` are now wrapper functions
  of `episode_group()`. Functionality remains the same but now includes
  all arguments available to `episode_group()`
- Changed the output of `fixed_episodes()` and `rolling_episodes()` from
  `number_line` to `data.frame`, pending the change to `epid` objects
- `pid_cri` column returned in `record_group` is now `numeric`. `0`
  indicates no match.
- columns can now be used as `criteria` multiple times `record_group()`
- [\#6](https://github.com/OlisaNsonwu/diyar/issues/6) `number_line`
  objects can now be used as a `criteria` in `record_group()`

## Bug fixes

- [\#3](https://github.com/OlisaNsonwu/diyar/issues/3) - Resolved a bug
  with `episode_unit` in `episode_group()`
- [\#4](https://github.com/OlisaNsonwu/diyar/issues/4) - Resolved a bug
  with `bi_direction` in `episode_group()`

# Version 0.0.1

## Features

- `fixed_episodes()` and `rolling_episodes()` - Group records into fixed
  or rolling episodes of events or period of events.
- `episode_group()` - A more comprehensive implementation of
  `fixed_episodes()` and `rolling_episodes()`, with additional features
  such as user defined case assignment.
- `record_group()` - Multistage deterministic linkage that addresses
  missing data.
- `number_line` S4 object.
  - Used to represent a range of numeric values to match using
    `record_group()`
  - Used to represent a period in time to be grouped using
    `fixed_episodes()`, `rolling_episodes()` and `episode_group()`
  - Used as the returned output of `fixed_episodes()` and
    `rolling_episodes()`
