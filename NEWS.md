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
