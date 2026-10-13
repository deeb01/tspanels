# tspanels 0.2.0

* Three panels, fetched from their sources:
  * `ilinet`: CDC ILINet through the Delphi Epidata API, weekly visits for
    influenza-like illness by state, with patients seen as exposure.
  * `flunet`: WHO FluNet through the xmart API, weekly specimens positive for
    influenza by country, nested in WHO region, with specimens processed as
    exposure. An empty total of positives is read as 0, reports of
    different origin are summed, and a report that repeats the others counts
    once.
  * `bbs`: the North American Breeding Bird Survey, 2023 release, birds
    counted by route and species on each annual run, with route nested in
    state and country. A species not recorded on a run counts 0, and a run
    that failed the survey's criteria has no count.
* A third exposure unit, `"count"`: the number of units a value is observed
  among, such as patients seen. tspanels keeps counts as recorded.
  `check_panel()` checks a count as non-negative and now rejects an unknown
  unit.
* `panel_info()` gives each panel's `pin`, what a fetch fixes, and
  `snapshot`, the date of the copy whose checksums are recorded for a source
  served by a live API. `fetch_panel()` keeps a file from such a source that
  no longer matches, with a warning; a file from a fixed release that fails
  its checksum is still refused.

# tspanels 0.1.0

First release.

* Four panels: `favorita`, `m5` and `rossmann` (retail, daily, imported from
  the user's own Kaggle download) and `portal` (ecology, lunar-monthly,
  fetched from PortalData release 6.112.0).
* One long format with named margins and an optional `exposure` column.
  `load_panel(exposure = FALSE)` drops it and does not need the files used
  only to derive it.
* `check_panel()` validates any frame against the format, including the
  nesting of attributes in margins and positive values at zero exposure.
* Every raw file is checked against a recorded SHA-256 checksum.
