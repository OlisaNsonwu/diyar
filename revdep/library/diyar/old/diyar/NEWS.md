
Version 0.0.2
=============

New feature
-----------

-   `pid` S4 object class for results of `record_group()`. This will replace the current default (`data.frame`) in the next major release
-   `epid` S4 object class for results of `episode_group()`, `fixed_episodes()` and `rolling_episodes()`. This will replace the current default (`data.frame`) in the next release
-   `to_s4()` and `to_s4` argument in `record_group()`, `episode_group()`, `fixed_episodes()` and `rolling_episodes()`. Changes their output from a `data.frame` (current default) to `epid` or `pid` objects
-   `to_df()` changes `epid` or `pid` objects to a `data.frame`
-   `deduplicate` argument from `fixed_episodes()` and `rolling_episodes()` added to `episode_group()`

Changes
-------

-   `fixed_episodes()` and `rolling_episodes()` are now wrapper functions of `episode_group()`. Functionality remains the same but now includes all arguments available to `episode_group()`
-   Changed the output of `fixed_episodes()` and `rolling_episodes()` from `number_line` to `data.frame`, pending the change to `epid` objects
-   `pid_cri` column returned in `record_group` is now `numeric`. `0` indicates no match.
-   columns can now be used as `criteria` multiple times `record_group()`
-   [\#6](https://github.com/OlisaNsonwu/diyar/issues/6) `number_line` objects can now be used as a `criteria` in `record_group()`

Bug fixes
---------

-   [\#3](https://github.com/OlisaNsonwu/diyar/issues/3) - Resolved a bug with `episode_unit` in `episode_group()`
-   [\#4](https://github.com/OlisaNsonwu/diyar/issues/4) - Resolved a bug with `bi_direction` in `episode_group()`

Version 0.0.1
=============

Features
--------

-   `fixed_episodes()` and `rolling_episodes()` - Group records into fixed or rolling episodes of events or period of events.
-   `episode_group()` - A more comprehensive implementation of `fixed_episodes()` and `rolling_episodes()`, with additional features such as user defined case assignment.
-   `record_group()` - Multistage deterministic linkage that addresses missing data.
-   `number_line` S4 object.
    -   Used to represent a range of numeric values to match using `record_group()`
    -   Used to represent a period in time to be grouped using `fixed_episodes()`, `rolling_episodes()` and `episode_group()`
    -   Used as the returned output of `fixed_episodes()` and `rolling_episodes()`