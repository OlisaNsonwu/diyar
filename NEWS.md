
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
