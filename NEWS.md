
# Version 0.3.1.9000

## New features

-   New function - `reframe()`. Modify the attributes of a
    `sub_criteria` object.
-   New function - `link_records()`. Record linkage by creating all
    record pairs as opposed to batches as with `link()`.
-   New function - `make_pairs()`. Create every combination of
    records-pairs for a given dataset.
-   New function - `make_pairs_wf_source()`. Create records-pairs from
    different sources only.
-   New function - `make_ids()`. Convert an edge list to a group
    identifier.
-   New function - `merge_ids()`. Merge two group identifiers.
-   New function - `attrs()`. XXXXXXXXXX.
-   New `S3`object - `d_report`. XXXXXXXX
-   New `S3`object - `d_attribute`. XXXXXXXX

## Changes

-   Optimised `episodes()` and `links()`. Reduced processing times.
-   Three new `display` values. `"progress_with_report"`,
    `"stats_with_report"` and `"none_with_report"`.

## Bug fixes

-   Incorrect `wind_id` list in `episodes()` when `data_link` is used.
    Resolved.
-   Incorrect `link_id` `links()` when `recursive` is used. Resolved.
-   `iteration` not recorded in some situations with `episodes()`.
    Resolved.

# Version 0.3.1

## New features

-   New function - `delink()`. Unlink identifiers.
-   New function - `episodes_wf_splits()`. Wrapper function of
    `episodes()`. Better optimised for handling datasets with many
    duplicate records.
-   New function - `combi()`. Numeric codes for unique combination of
    vectors.
-   New function - `attr_eval()`. Recursive evaluation of a function on
    each attribute of a `sub_criteria`.

## Changes

-   Two new `case_nm` values - `Case_CR` and `Recurrence_CR` which are
    `Case` and `Recurrence` without a sub-criteria match.

## Bug fixes

-   Corrected length arrows in `schema.epid`.
-   Corrected outcome of `eval_sub_criteria` with 1 result.

# Version 0.3.0

## New features

-   New function - `links_wf_probabilistic()`. Probabilistic record
    linkage.
-   New function - `partitions()`. Spilt events into sections in time.
-   New function - `schema()`. Plot schema diagrams for `pid`, `epid`,
    `pane` and `number_line` objects.
-   New functions - `encode()` and `decode()`. Encode and decode slots
    values to minimise memory usage.
-   New argument in `episodes()` - `case_sub_criteria` and
    `recurrence_sub_criteria`. Additional matching conditions for
    temporal links.
-   New argument in `episodes()`- `case_length_total` and
    `recurrence_length_total`. Number of temporal links required for a
    `window`/`episode`.
-   New argument in `links()` - `recursive`. Control if matches can
    spawn new matches.
-   New argument in `links()` - `check_duplicates`. Control the checking
    of logical tests on duplicate values. If `FALSE`, results are
    recycled for the duplicates.
-   `as.data.frame` and `as.list` S3 methods for the `pid`,
    `number_line`, `epid`, `pane` objects.
-   New option for `episode_type` in `episodes()` - “recursive”. For
    recursive episodes where every linked events can be used as a
    subsequent index event.
-   `recurrence_from_last` renamed to `reference_event` and given two
    new options.

## Changes

-   `episodes()` and `links()`. Speed improvements.
-   Default time zone for an `epid_interval` or `pane_interval` with
    `POSIXct` objects is now “GMT”.
-   `number_line_sequence()` - splits number\_line objects. Also
    available as a `seq` method.
-   `epid_total`, `pid_total` and `pane_total` slots are populated by
    default. No need to used `group_stats` to get these.
-   `to_df()` - Removed. Use `as.data.frame()` instead.
-   `to_s4()` - Now an internal function. It’s no longer exported.
-   `compress_number_line()` - Now an internal function. It’s no longer
    exported. Use `episodes()` instead.
-   `sub_criteria()` - produces a `sub_criteria` object. Nested “AND”
    and “OR” conditions are now possible.
-   `case_overlap_methods`, `recurrence_overlap_methods` and
    `overlap_methods` now take `integer` codes for different
    combinations of overlap methods. See `overlap_methods$options` for
    the full list. `character` inputs are still supported.

## Bug fixes

-   `"Single-record"` was wrong in `links` summary output. Resolved.

# Version 0.2.0

## New features

-   Better support for `Inf` in `number_line` objects.
-   Can now use multiple `case_length` or `recurrence_length` for the
    same event.
    -   Can now use multiple `overlap_methods` for the corresponding
        `case_length` and `recurrence_length`.
-   New function `links()` to replace `record_group()`.
-   New function `sub_criteria()`. The new way of supplying a
    `sub_criteria` in `links()`.
-   New functions `exact_match()`, `range_match()` and
    `range_match_legacy()`. Predefined logical tests for use with
    `sub_criteria()`. User-defined tests can also be used. See
    `?sub_criteria`.
-   New function `custom_sort()` for nested sorting.
-   New function `epid_lengths()` to show the required `case_length` or
    `recurrence_length` for an analyses. Useful in confirming the
    required `case_length` or `recurrence_length` for episode tracking.
-   New function `epid_windows()`. Shows the period a `date` will
    overlap with given a particular `case_length` or
    `recurrence_length`. Useful in confirming the required `case_length`
    or `recurrence_length` for episode tracking.
-   New argument - `strata` in `links()`. Useful for stratified data
    linkage. As in stratified episode tracking, a record with a missing
    `strata` (`NA_character_`) is skipped from data linkage.
-   New argument - `data_links` in `links()`. Unlink record groups that
    do not include records from certain data sources
-   New convenience functions
    -   `listr()`. Format `atomic` vectors as a written list.
    -   `combns()`. An extension of `combn` to generate permutations not
        ordinarily captured by `combn`.
-   New `iteration` slot for `pid` and `epid` objects
-   New `overlap_method` - `reverse()`

## Changes

-   `number_line()` - `l` and `r` must have the same length or be `1`.
-   `episodes()` - `case_nm` differentiates between duplicates of
    `"Case"` (`"Duplicate_C"`) and `"Recurrent"` events
    (`"Duplicate_R"`).
-   Strata and episode-level options for most arguments. This gives
    greater flexibility within the same instance of `episodes()`.
    -   Episode-level - The behaviour for each episode is determined by
        the corresponding option for its index event (`"Case"`).
        -   `episode_type` - simultaneously track both `"fixed"` and
            `"rolling"` episodes.
        -   `skip_if_b4_lengths` - simultaneously track episodes where
            events before a cut-off range are both skipped and not
            skipped.
        -   `episode_unit` - simultaneously track episodes by different
            units of time.
        -   `case_for_recurrence` - simultaneously track `"rolling"`
            episodes with and without an additional case window for
            recurrent events.
        -   `recurrence_from_last` - simultaneously track `"rolling"`
            episodes with reference windows calculated from the first
            and last event of the previous window.
    -   Strata-level - The behaviour for each episode is determined by
        the corresponding option for its `strata`. Options must be the
        same in each strata.
        -   `from_last` - simultaneously track episodes in both
            directions of time - past to present and present to past.
        -   `episodes_max` - simultaneously track different number of
            episodes within the dataset.
-   `include_overlap_method` - `"overlap"` and `"none"` will not be
    combined with other methods.
    -   `"overlap"` - mutually inclusive with the other methods, so
        their inclusion is not necessary.
    -   `"none"` - mutually exclusive and prioritised over the other
        methods (including `"none"`), so their inclusion is not
        necessary.
-   Events can now have missing cut-off points (`NA_real_`) or periods
    (`number_line(NA_real_, NA_real_)`) `case_length` and
    `recurrence_length`. This ensures that the event does not become an
    index case however, it can still be part of different episode. For
    reference, an event with a missing `strata` (`NA_character_`)
    ensures that the event does not become an index case nor part of any
    episode.

## Bug fixes

-   `fixed_episodes`, `rolling_episodes` and `episode_group` -
    `include_index_period` didn’t work in certain situations. Corrected.
-   `fixed_episodes`, `rolling_episodes` and `episode_group` -
    `dist_from_wind` was wrong in certain situations. Corrected.

# Version 0.1.0

## New features

-   New argument in `record_group()` - `strata`. Perform record linkage
    separately within subsets of a dataset.
-   New argument in `overlap()`, `compress_number_line()`,
    `fixed_sepisodes()`, `rolling_episodes()` and `episode_group()` -
    `overlap_methods` and `methods`. Replaces `overlap_method` and
    `method` respectively. Use different sets of methods within the same
    dataset when grouping episodes or collapsing `number_line` objects.
    `overlap_method` and `method` only permits 1 method per per dataset.
-   New slot in `epid` objects - `win_nm`. Shows the type of window each
    event belongs to i.e. case or recurrence window
-   New slot in `epid` objects - `win_id`. Unique ID for each window.
    The ID is the `sn` of the reference event for each window
    -   Format of `epid` objects updated to reflect this
-   New slot in `epid` objects - `dist_from_wind`. Shows the duration of
    each event from its window’s reference event
-   New slot in `epid` objects - `dist_from_epid`. Shows the duration of
    each event from its episode’s reference event
-   New argument in `episode_group()` and `rolling_episodes()` -
    `recurrence_from_last`. Determine if reference events should be the
    first or last event from the previous window.
-   New argument in `episode_group()` and `rolling_episodes()` -
    `case_for_recurrence`. Determine if recurrent events should have
    their own case windows or not.
-   New argument in `episode_group()`, `fixed_episodes()` and
    `rolling_episodes()` - `data_links`. Unlink episodes that do not
    include records from certain `data_source(s)`.
-   `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
    `case_length` and `recurrence_length` arguments. You can now use a
    range (`number_line` object).
-   New argument in `episode_group()`, `fixed_episodes()` and
    `rolling_episodes()` - `include_index_period`. If `TRUE`, overlaps
    with the index event or period are grouped together even if they are
    outside the cut-off range (`case_length` or `recurrence_length`).
-   New slot in `pid` objects - `link_id`. Shows the record (`sn` slot)
    to which every record in the dataset has matched to.
-   New function - `invert_number_line()`. Invert the `left` and/or
    `right` points to the opposite end of the number line
-   New accessor functions -`left_point(x)<-`, `right_point(x)<-`,
    `start_point(x)<-` and `end_point(x)<-`

## Changes

-   `overlap()` renamed to `overlaps()`. `overlap()` is now a
    convenience `overlap_method` to capture ANY kind of overlap.
-   `"none"` is another convenience `overlap_method` for NO kind of
    overlap
-   `expand_number_line()` - new options for `point`; `"left"` and
    `"right"`
-   `compress_number_line()` - compressed `number_line` object inherits
    the direction of the widest `number_line` among overlapping group of
    `number_line` objects
-   `overlap_methods` - have been changed such that each pair of
    `number_line` objects can only overlap in one way. E.g.
    -   `"chain"` and `"aligns_end"` used to be possible but this is now
        considered a `"chain"` overlap only
    -   `"aligns_start"` and `"aligns_end"` use to be possible but this
        is now considered an `"exact"` overlap
-   `number_line_sequence()` - Output is now a `list`.
-   `number_line_sequence()` - now works across multiple `number_line`
    objects.
-   `to_df()` - can now change `number_line` objects to data.frames.
    -   `to_s4()` can do the reverse.
-   `epid` objects are the default outputs for `fixed_episodes()`,
    `rolling_episodes()` and `episode_group()`
-   `pid` objects are the default outputs for `record_group()`
-   In episode grouping, the `case_nm` for events that were skipped due
    to `rolls_max` or `episodes_max` is now `"Skipped"`.
-   In `episode_group()` and `record_group()`, `sn` can be negative
    numbers but must still be unique
-   Optimised `episode_group()` and `record_group()`. Runs just a little
    bit faster …
-   Relaxed the requirement for `x` and `y` to have the same lengths in
    overlap functions.
    -   The behaviour of overlap functions will now be the same as that
        of standard R logical tests
-   `episode_group` - `case_length` and `recurrence_length` arguments.
    Now accepts negative numbers.
    -   negative “lengths” will collapse two periods into one, if the
        second one is within some days before the `end_point()` of the
        first period.
        -   if the “lengths” are larger than the `number_line_width()`,
            both will be collapsed if the second one is within some days
            (or any other `episode_unit`) before the `start_point()` of
            the first period.
-   cheat sheet updated

## Bug fixes

-   Recurrence was not checked if the initial case event had no
    duplicates. Resolved
-   `case_nm` wasn’t right for rolling episodes. Resolved

# Version 0.0.3

## Changes

-   [\#7](https://github.com/OlisaNsonwu/diyar/issues/7)
    `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
    optimized to take less time when working with large datasets
-   `episode_group()`, `fixed_episodes()` and `rolling_episodes()` -
    `date` argument now supports numeric values
-   `compress_number_line()` - the output (`gid` slot) is now a group
    identifier just like in `epid` objects (`epid_interval`)

# Version 0.0.2

## New feature

-   `pid` S4 object class for results of `record_group()`. This will
    replace the current default (`data.frame`) in the next major release
-   `epid` S4 object class for results of `episode_group()`,
    `fixed_episodes()` and `rolling_episodes()`. This will replace the
    current default (`data.frame`) in the next release
-   `to_s4()` and `to_s4` argument in `record_group()`,
    `episode_group()`, `fixed_episodes()` and `rolling_episodes()`.
    Changes their output from a `data.frame` (current default) to `epid`
    or `pid` objects
-   `to_df()` changes `epid` or `pid` objects to a `data.frame`
-   `deduplicate` argument from `fixed_episodes()` and
    `rolling_episodes()` added to `episode_group()`

## Changes

-   `fixed_episodes()` and `rolling_episodes()` are now wrapper
    functions of `episode_group()`. Functionality remains the same but
    now includes all arguments available to `episode_group()`
-   Changed the output of `fixed_episodes()` and `rolling_episodes()`
    from `number_line` to `data.frame`, pending the change to `epid`
    objects
-   `pid_cri` column returned in `record_group` is now `numeric`. `0`
    indicates no match.
-   columns can now be used as `criteria` multiple times
    `record_group()`
-   [\#6](https://github.com/OlisaNsonwu/diyar/issues/6) `number_line`
    objects can now be used as a `criteria` in `record_group()`

## Bug fixes

-   [\#3](https://github.com/OlisaNsonwu/diyar/issues/3) - Resolved a
    bug with `episode_unit` in `episode_group()`
-   [\#4](https://github.com/OlisaNsonwu/diyar/issues/4) - Resolved a
    bug with `bi_direction` in `episode_group()`

# Version 0.0.1

## Features

-   `fixed_episodes()` and `rolling_episodes()` - Group records into
    fixed or rolling episodes of events or period of events.
-   `episode_group()` - A more comprehensive implementation of
    `fixed_episodes()` and `rolling_episodes()`, with additional
    features such as user defined case assignment.
-   `record_group()` - Multistage deterministic linkage that addresses
    missing data.
-   `number_line` S4 object.
    -   Used to represent a range of numeric values to match using
        `record_group()`
    -   Used to represent a period in time to be grouped using
        `fixed_episodes()`, `rolling_episodes()` and `episode_group()`
    -   Used as the returned output of `fixed_episodes()` and
        `rolling_episodes()`
