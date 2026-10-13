# tspanels

tspanels is an R package that loads open panels of related time series into
one long format. A panel here is a collection of time series observed over a
common period and indexed by categorical margins: daily sales by store and
product family, monthly rodent captures by plot and species. Each panel
arrives with its margins as named columns and, where the source records one,
an exposure variable giving the share of each period during which the value
could be observed. Exposure is optional throughout.

It was written for a study of partial pooling across margins, which needed
several such panels in one format with their structure intact.

## Why it needed to exist

Public collections of time series give the series without that structure.
The Monash forecasting archive and the M-competition packages (`Mcomp`,
`M4comp2018`) store each dataset as a list of series, each with a few
descriptive fields and no hierarchy or exposure. Nixtla's `datasetsforecast`
(Python) ships item-level Favorita and M5 with their hierarchies as summing
matrices, and the R `m5` package loads M5 alone; neither derives an exposure
variable, and no R package we found loads Favorita or Rossmann with their
store attributes.

Turning a source's raw files into a panel also takes decisions that fail
silently when they are wrong. Four from the panels here, the first two found
wrong in code that had been in use:

1. **Favorita closures.** The source has no store calendar. Taking a store as
   shut on days with no row in `transactions.csv` marks 118 store-days
   (3,248 rows) as shut on which the store recorded sales.
2. **Portal species code "NA".** It is *Neotoma albigula*, the white-throated
   woodrat. Treating it as a missing value drops 1,593 captures.
3. **Portal censuses not taken.** A plot that was not trapped has no count.
   Recording it as zero captures is wrong, and the error stays hidden only as
   long as exposure (0 on that row) is kept beside it.
4. **M5 items not yet carried.** Each item's sales are recorded as zero
   before it was first listed. Those zeros are not demand, so a series starts
   on the item's first listed day in an open store.

Each loader states its decisions next to the code that makes them, and the
tests pin each one with a case built to break it.

## The format

`load_panel()` returns one row per series and time point:

| column | type | meaning |
|---|---|---|
| `series_id` | factor | the series |
| `ds` | Date | the time point |
| `y` | numeric | the observed value; `NA` where the source records that no observation was made |
| `exposure` | numeric | present only if the panel records exposure and `exposure = TRUE` |
| margins | factor | the key margins and nested attributes |

Rows are ordered by `series_id`, then `ds`. Factor levels are sorted in the C
locale, so row order and any sample drawn from it are the same on every
machine.

**Margins.** A *key margin* is a categorical column whose combinations
define the series: Favorita's series are the observed combinations of
`store_nbr` and `family`. An *attribute* is a categorical column constant
within a key margin or within another attribute, which places it at a level
above: `city` is constant within `store_nbr`, and `state` within `city`. Key
margins are crossed with each other; attributes are nested in them.
`panel_info(name)$nesting` records each attribute's parent, and
`check_panel()` verifies the relation on the data.

**Exposure.** For a series with exposure $e_t \in [0, 1]$, a model for the
mean takes it as a multiplier,

$$\mathrm{E}[y_t] = e_t \, \lambda_t,$$

where $\lambda_t$ is the value the series would show with full exposure. A
store shut for the day has $e_t = 0$; a plot with half its traps set has
$e_t = 0.5$. Many panels record nothing of the kind, and then the column is
absent. `load_panel(name, exposure = FALSE)` drops it, and the files used only
to derive it are then not needed. Dropping it leaves `y` unchanged: a zero
recorded while a store was shut is still a zero, and a model without exposure
sees it as one.

`check_panel()` validates any frame against the format, including your own.
Among its checks, it rejects a positive `y` where exposure is 0, which a
multiplicative model cannot fit; the first defect above fails that check.

## The panels

| name | domain | frequency | series | span | key margins | attributes | exposure | access |
|---|---|---|---|---|---|---|---|---|
| `favorita` | retail | daily | 1,782 | 2013-01-01 to 2017-08-15 | store_nbr × family | city, state, type, cluster | store open (binary) | import |
| `m5` | retail | daily | 30,490 | 2011-01-29 to 2016-05-22 | item_id × store_id | dept_id, cat_id, state_id | item available (binary) | import |
| `rossmann` | retail | daily | 1,115 | 2013-01-01 to 2015-07-31 | store | store_type, assortment | store open (binary) | import |
| `portal` | ecology | lunar month | 1,200 | 1977-07-16 to 2026-07-18 | plot × species | none | traps set (fraction) | fetch |

What each loader decides:

- **favorita.** Exposure is 1 on a day the store has a row in
  `transactions.csv` or recorded any sales, and 0 otherwise. Christmas Day is
  absent from the source in every year. Stores that opened during the span
  show zero sales and exposure 0 before they opened.
- **m5.** Exposure is 1 when the item was listed in `sell_prices.csv` that
  week and its store sold anything that day; a store-day with no sales of any
  of its 3,049 items is taken as shut, which picks out each Christmas Day. A
  series starts on its item's first listed day in an open store, with or
  without the exposure column. The full panel is 46.9 million rows and needs
  about 5 GB; `panel_series("m5")` lists the series so a subset can be loaded
  with `series =`.
- **rossmann.** Exposure is the source's `Open` flag. 180 stores have no rows
  from 2014-07-01 to 2014-12-31, and one store lacks a single day; those rows
  are absent in the source and stay absent.
- **portal.** `y` is the number of individuals of a species caught on a plot
  in a regular census (period > 0). Species code "NA" is *Neotoma albigula*.
  Records with a blank species are trapping placeholders and are not counted.
  A species not caught on a trapped plot counts 0; a plot not trapped in a
  census has `y = NA` and exposure 0. Exposure is the share of the plot's 49
  trap stakes that were set. Species codes are as recorded, including codes
  for unidentified and non-target animals. Files are pinned to PortalData
  release 6.112.0.

## Getting the data

Raw files are kept in a cache, by default
`tools::R_user_dir("tspanels", "cache")`; set the `TSPANELS_CACHE`
environment variable to put it elsewhere. Every file is checked against a
recorded SHA-256 checksum.

Panels with access `fetch` are downloaded from a pinned URL at the source. A
file is moved into the cache only after its checksum matches.

```r
fetch_panel("portal")
d <- load_panel("portal")
```

Panels with access `import` are distributed by Kaggle under each
competition's rules, which tspanels does not redistribute. Download the
competition's data from the source URL in `panels()` after accepting its
rules, then install the zip archive or the folder you extracted:

```r
import_panel("favorita", "~/Downloads/store-sales-time-series-forecasting.zip")
d <- load_panel("favorita")
```

`import_panel()` warns, and does not refuse, when a file differs from the
copy tspanels was tested on, since a source may re-issue a file with
immaterial changes; the warning names the file.

## Usage

```r
library(tspanels)

panels()                          # what is registered, and what is in the cache
panel_info("favorita")$nesting    # city = "store_nbr", state = "city", ...

d  <- load_panel("favorita")                    # with exposure
d0 <- load_panel("favorita", exposure = FALSE)  # without; transactions.csv not needed

ids <- panel_series("m5")$series_id
m5  <- load_panel("m5", series = ids[1:500])

check_panel(my_frame, key = c("store", "item"), nesting = c(region = "store"),
            exposure_unit = "fraction")
```

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("deeb01/tspanels")
```

## Scope

tspanels loads panels and checks them. It does not include covariates such as
prices, promotions, holidays or weather, though several sources record them;
it does not build aggregation or summing matrices; and it does not forecast or
score forecasts. A panel is read whole into memory before any `series`
subset is taken, except M5, which is subset before it is reshaped.

The licences differ by panel and are listed by `panels()`. Portal is CC0. The
three retail panels are under their Kaggle competitions' rules, which
tspanels has not verified beyond the competition pages; whether results from
them may be published is a question for those rules, not for this package.

## Licence

MIT, for the code. The data keep their own licences, above.
