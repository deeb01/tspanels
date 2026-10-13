# tspanels

tspanels is an R package that loads open panels of related time series into
one long format. A panel here is a collection of time series observed over a
common period and indexed by categorical margins: daily sales by store and
product family, monthly rodent captures by plot and species, weekly influenza
detections by country. Each panel arrives with its margins as named columns
and, where the source records one, an exposure variable: the share of each
period during which the value could be observed, or the number of units, such
as patients seen, it was observed among. Exposure is optional throughout.

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
store attributes. R clients exist for two of the other sources, ILINet
(`cdcfluview`, and Delphi's own `epidatr`) and the Breeding Bird Survey
(`bbsBayes2`, built around its trend models); we found none for FluNet.

Turning a source's raw files into a panel also takes decisions that fail
silently when they are wrong. Six from the panels here, the first four found
wrong in code that had been in use:

1. **Favorita closures.** The source has no store calendar. Taking a store as
   shut on days with no row in `transactions.csv` marks 118 store-days
   (3,248 rows) as shut on which the store recorded sales.
2. **Portal species code "NA".** It is *Neotoma albigula*, the white-throated
   woodrat. Treating it as a missing value drops 1,593 captures.
3. **Breeding Bird Survey species not seen.** The survey lists only the
   species seen on a run. A loader that builds series from those rows alone
   leaves out every zero, 6.3 million of the panel's 14.0 million rows, and
   each series reads as positive counts with gaps.
4. **FluNet reports by origin.** A country reports a week in up to three
   rows, by the origin of the specimens. In the 37,453 of 142,014
   country-weeks that have more than one, keeping one row per country-week
   keeps whichever the server returned first and drops the others'
   specimens.
5. **Portal censuses not taken.** A plot that was not trapped has no count.
   Recording it as zero captures is wrong, and the error stays hidden only as
   long as exposure (0 on that row) is kept beside it.
6. **M5 items not yet carried.** Each item's sales are recorded as zero
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
| `y` | numeric | the observed value; `NA` where the source records that no observation was made, or that the one made fails its own criteria |
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

**Exposure.** For a series with exposure $e_t$, a model for the mean takes
it as a multiplier,

$$\mathrm{E}[y_t] = e_t \, \lambda_t.$$

Exposure comes in two kinds, and `panels()` gives each panel's unit. A share
of the period, $e_t \in [0, 1]$ (unit `fraction`, or `binary` when it is 0 or
1), makes $\lambda_t$ the value the series would show with full exposure: a
store shut for the day has $e_t = 0$; a plot with half its traps set has
$e_t = 0.5$. A count of the units the value is observed among (unit `count`)
makes $\lambda_t$ the value per unit: with $e_t$ the patients a state's
providers saw in a week, $\lambda_t$ is the share of them with
influenza-like illness. tspanels keeps counts as the source records them, and
leaves any rescaling to $[0, 1]$ to the caller.

Many panels record nothing of the kind, and then the column is absent.
`load_panel(name, exposure = FALSE)` drops it, and the files used only to
derive it are then not needed. Dropping it leaves `y` unchanged: a zero
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
| `ilinet` | epidemiology | weekly | 53 | 2010-10-03 to 2025-09-21 | state | none | patients seen (count) | fetch |
| `flunet` | epidemiology | weekly | 189 | 1995-01-02 to 2025-12-22 | country | who_region | specimens processed (count) | fetch |
| `portal` | ecology | lunar month | 1,200 | 1977-07-16 to 2026-07-18 | plot × species | none | traps set (fraction) | fetch |
| `bbs` | ecology | annual | 523,994 | 1966-05-24 to 2022-07-15 | route × species | state, country | run acceptable (binary) | fetch |

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
- **ilinet.** `y` is the number of visits for influenza-like illness that a
  state's reporting providers saw in a week, and exposure the number of
  patients they saw for any reason, so `y / exposure` is the share CDC
  publishes. A week is dated by the Sunday that starts its MMWR week. In 41
  state-weeks no provider reported and the source records 0 visits among 0
  patients; `y` is `NA` there and exposure 0. New York is the whole state:
  Delphi also serves New York City and the rest of the state, which are left
  out so that no visit is counted twice. Puerto Rico, the US Virgin Islands,
  Louisiana and Florida start later than the other series, as they do in the
  source.
- **flunet.** `y` is the number of specimens positive for influenza in a
  country in a week, and exposure the number of specimens processed. FluNet
  leaves its total of positives, `INF_ALL`, empty rather than writing 0: the
  column is never 0 in the table, its parts for influenza A and B are never
  positive where it is empty, and where such a report gives its negatives
  they equal its specimens processed in 91% of cases. An empty total on a
  report with specimens processed is therefore 0. A country reports a week in
  up to three rows, by the origin of the specimens (sentinel, non-sentinel,
  not defined), and these are summed, except a report of undefined origin
  that repeats the week's other reports, or one of them, in specimens and
  positives: that is the same specimens reported twice, as in every such week
  in Austria, Denmark and Turkey. A report that gives no number of specimens
  processed adds nothing, so that `y` counts the positives among the
  specimens exposure counts. That leaves out 10.5% of all positives, more
  than half of them in England's non-sentinel reports and Japan's reports; a
  week with no report of specimens processed has `y` `NA` and exposure 0.
  Reports repeated exactly (40 rows for Bhutan in 2025) count once.
- **portal.** `y` is the number of individuals of a species caught on a plot
  in a regular census (period > 0). Species code "NA" is *Neotoma albigula*.
  Records with a blank species are trapping placeholders and are not counted.
  A species not caught on a trapped plot counts 0; a plot not trapped in a
  census has `y = NA` and exposure 0. Exposure is the share of the plot's 49
  trap stakes that were set. Species codes are as recorded, including codes
  for unidentified and non-target animals. Files are pinned to PortalData
  release 6.112.0.
- **bbs.** `y` is the number of birds of a species counted on a run of a
  route, over its 50 stops. Only runs under the survey's standard protocol
  count (RPID 101); the 464 of 135,529 runs under other protocols are left
  out. A series is a species recorded on a standard run of the route, and has
  a row for every standard run of the route in `weather.csv`, with 0 where
  the species was not recorded. A run that failed the survey's criteria for
  weather, date, time of day or route completion (RunType 0, 9.4% of standard
  runs) has `y` `NA` and exposure 0: its counts are in the source, but the
  survey marks them as not comparable with those of acceptable runs, and a
  positive count at exposure 0 is one a multiplicative model cannot fit. A
  row is dated by the day the route was run, at most once a year; no route
  was run in 2020. `route` is the survey's route number, state code × 1000 +
  route; `species` is the AOU code as recorded, and the survey's taxa include
  subspecies and unidentified species groups. The full panel is 14.0 million
  rows and needs about 3 GB.

## Getting the data

Raw files are kept in a cache, by default
`tools::R_user_dir("tspanels", "cache")`; set the `TSPANELS_CACHE`
environment variable to put it elsewhere. Every file is checked against a
recorded SHA-256 checksum.

Panels with access `fetch` are downloaded from a pinned URL at the source,
and each file is checked against the checksum of the copy tspanels was tested
on. What the pin fixes depends on the source:

| panel | served by | what is pinned | what is not |
|---|---|---|---|
| `portal` | GitHub | PortalData release 6.112.0, whose files cannot change | nothing |
| `bbs` | ScienceBase (USGS) | the 2023 data release, with data to 2022, whose files cannot change; they also match the MD5 checksums the release publishes | nothing; USGS has since issued newer releases, which tspanels does not follow |
| `ilinet` | Delphi Epidata API | the data as of FluView issue 202639 (the week ending 2026-10-03): each season from 2010-11 to 2024-25 as published in the issue that last revised it | the bytes the API returns, which Delphi could change; any revision CDC publishes after that issue |
| `flunet` | WHO xmart API | the query only: its columns, its weeks (starting 1995-01-02 to 2025-12-22) and its row order | the data: WHO revises FluNet in place, so a later fetch will usually differ |

A file from a fixed release that fails its checksum is refused and nothing is
kept. A file from an API is a snapshot: the checksums for `ilinet` and
`flunet` are of the copies fetched on 2026-10-12, and a later fetch that
differs is kept, with a warning that names the file, because refusing it
would leave the panel impossible to fetch once the source changes.
`panel_info(name)$pin` and `$snapshot` record the same.

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

panels()$exposure                 # binary, fraction or count, by panel
panel_info("flunet")$pin          # what a fetch fixes

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
subset is taken, except M5, which is subset before it is reshaped, and the
Breeding Bird Survey, which is subset before its zeros are filled in.

The licences differ by panel and are listed by `panels()`. Portal is CC0.
ILINet and the Breeding Bird Survey are US Government works in the public
domain; the survey's release gives its citation as Ziolkowski et al. (2023),
doi:10.5066/P9GS9K64. FluNet is under CC BY-NC-SA 3.0 IGO: non-commercial use,
with attribution to WHO. The three retail panels are under their Kaggle
competitions' rules, which tspanels has not verified beyond the competition
pages; whether results from them may be published is a question for those
rules, not for this package.

## Licence

MIT, for the code. The data keep their own licences, above.
