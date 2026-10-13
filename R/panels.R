# The registered panels. Each loader states the decisions it makes in deriving
# the panel from the raw files; those decisions are the reason the package
# exists, so they are written next to the code that makes them.

.rd <- function(path, ...) fread(path, showProgress = FALSE, ...)

# ---------------------------------------------------------------------------
# Favorita: daily sales by store and product family, Ecuador.
#
# Exposure is whether the store was open. The source has no such column. A
# store-day with no row in transactions.csv is taken as shut, which is the
# usual reading, but on 118 store-days (3,248 rows) transactions.csv has no row
# while train.csv records sales. A store that sold something was open, so open
# is: a transactions row OR any sales in the store that day.
# ---------------------------------------------------------------------------

.ld_favorita <- function(dir, series = NULL, exposure = TRUE) {
  tr <- .rd(file.path(dir, "train.csv"),
            select = c("date", "store_nbr", "family", "sales"))
  tr[, date := as.Date(date)]
  if (exposure) {
    op <- tr[, .(open = as.numeric(any(sales > 0))), by = .(store_nbr, date)]
    tx <- .rd(file.path(dir, "transactions.csv"), select = c("date", "store_nbr"))
    tx[, date := as.Date(date)]
    op[tx, on = .(store_nbr, date), open := 1]
    tr[op, on = .(store_nbr, date), e := i.open]
  }
  tr[, series_id := paste(store_nbr, family, sep = "|")]
  if (!is.null(series)) tr <- tr[series_id %in% series]
  st <- .rd(file.path(dir, "stores.csv"))
  at <- st[match(tr$store_nbr, st$store_nbr)]
  .panel_frame(tr$series_id, tr$date, tr$sales, if (exposure) tr$e,
               list(store_nbr = tr$store_nbr, family = tr$family,
                    city = at$city, state = at$state, type = at$type,
                    cluster = at$cluster))
}

.ix_favorita <- function(dir) {
  s <- unique(.rd(file.path(dir, "train.csv"), select = c("store_nbr", "family")))
  st <- .rd(file.path(dir, "stores.csv"))
  at <- st[match(s$store_nbr, st$store_nbr)]
  data.frame(series_id = paste(s$store_nbr, s$family, sep = "|"),
             store_nbr = .fct(s$store_nbr), family = .fct(s$family),
             city = .fct(at$city), state = .fct(at$state), type = .fct(at$type),
             cluster = .fct(at$cluster))
}

.register("favorita", list(
  title = "Corporacion Favorita store sales: store by product family",
  domain = "retail", freq = "daily",
  key = c("store_nbr", "family"),
  nesting = c(city = "store_nbr", state = "city", type = "store_nbr",
              cluster = "store_nbr"),
  exposure = paste("store open: 1 on a day the store has a row in",
                   "transactions.csv or recorded any sales, 0 otherwise"),
  exposure_unit = "binary",
  n_series = 1782L, start = "2013-01-01", end = "2017-08-15",
  notes = c("Christmas Day is absent from the source in every year.",
            paste("Stores that opened during the span have zero sales and",
                  "exposure 0 before they opened.")),
  access = "import",
  pin = "the competition's data files, fixed since it closed",
  snapshot = NA_character_,
  source = "https://www.kaggle.com/competitions/store-sales-time-series-forecasting/data",
  licence = "Kaggle competition rules; not redistributed",
  files = data.frame(
    file = c("train.csv", "stores.csv", "transactions.csv"),
    role = c("data", "data", "exposure"),
    sha256 = c("99a1b7f4241821df10fa71f23abdd71c634f71f901e3cab1400c0f0c583f862c",
               "af503b2bce11d7906d249f81cc0598f10e2addcc9f6c59aa2d95c9f2652c296b",
               "e116384a6981af74932832436aa2f6a43121f77ca81accc44e3a5160158ca03c"),
    url = NA_character_, stringsAsFactors = FALSE),
  loader = .ld_favorita, index = .ix_favorita))

# ---------------------------------------------------------------------------
# M5: daily unit sales by item and store, Walmart, three US states.
#
# Exposure is whether the item could be bought: listed that week (it has a row
# in sell_prices.csv) and the store open that day. The source has no store
# calendar; a store-day on which none of its 3,049 items sold is taken as shut,
# which picks out each Christmas Day. Before an item's first listed day in an
# open store it was not carried, so the series starts there; that holds with
# exposure = FALSE as well, since it decides which rows exist.
#
# Built on integer keys: the full panel is 30,490 series by 1,941 days.
# ---------------------------------------------------------------------------

.ld_m5 <- function(dir, series = NULL, exposure = TRUE) {
  sal <- .rd(file.path(dir, "sales_train_evaluation.csv"))
  dc <- grep("^d_", names(sal), value = TRUE)
  dn <- as.integer(sub("^d_", "", dc))
  at <- sal[, .(id, item_id, dept_id, cat_id, store_id, state_id)]
  at[, `:=`(sid = .I, stn = match(store_id, unique(store_id)))]

  tot <- sal[, lapply(.SD, sum), by = store_id, .SDcols = dc]
  op <- melt(tot, id.vars = "store_id", measure.vars = dc,
             variable.name = "d", value.name = "tot")
  op[, `:=`(stn = match(store_id, unique(at$store_id)), d = dn[as.integer(d)],
            open = tot > 0)]

  sal[, sid := .I]
  if (!is.null(series)) sal <- sal[id %in% series]
  long <- melt(sal[, c("sid", dc), with = FALSE], id.vars = "sid",
               measure.vars = dc, variable.name = "d", value.name = "y")
  rm(sal)
  long[, d := dn[as.integer(d)]]

  cal <- .rd(file.path(dir, "calendar.csv"), select = c("d", "date", "wm_yr_wk"))
  cal[, d := as.integer(sub("^d_", "", d))]
  k <- match(long$d, cal$d)
  long[, `:=`(ds = as.Date(cal$date)[k], wk = cal$wm_yr_wk[k])]

  pr <- .rd(file.path(dir, "sell_prices.csv"),
            select = c("store_id", "item_id", "wm_yr_wk"))
  pr[, sid := match(paste(item_id, store_id), paste(at$item_id, at$store_id))]
  pr <- unique(pr[!is.na(sid), .(sid, wk = wm_yr_wk)])
  pr[, listed := TRUE]
  long[, stn := at$stn[sid]]
  long[pr, on = .(sid, wk), listed := i.listed]
  long[op, on = .(stn, d), open := i.open]
  long[, e := as.numeric(!is.na(listed) & open)]

  # melt returns each series' days in order, so the running sum needs no sort
  long[, keep := cumsum(e > 0) > 0, by = sid]
  long <- long[keep == TRUE]
  # factor once per series, then index by series: cheap at 47 million rows
  f <- function(x) .fct(x)[long$sid]
  .panel_frame(f(at$id), long$ds, long$y, if (exposure) long$e,
               list(item_id = f(at$item_id), store_id = f(at$store_id),
                    dept_id = f(at$dept_id), cat_id = f(at$cat_id),
                    state_id = f(at$state_id)))
}

.ix_m5 <- function(dir) {
  s <- .rd(file.path(dir, "sales_train_evaluation.csv"),
           select = c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id"))
  data.frame(series_id = s$id, item_id = .fct(s$item_id),
             store_id = .fct(s$store_id), dept_id = .fct(s$dept_id),
             cat_id = .fct(s$cat_id), state_id = .fct(s$state_id))
}

.register("m5", list(
  title = "M5 forecasting competition: Walmart unit sales by item and store",
  domain = "retail", freq = "daily",
  key = c("item_id", "store_id"),
  nesting = c(dept_id = "item_id", cat_id = "dept_id", state_id = "store_id"),
  exposure = paste("item available: 1 if it was listed in sell_prices.csv",
                   "that week and its store sold anything that day, 0 otherwise"),
  exposure_unit = "binary",
  n_series = 30490L, start = "2011-01-29", end = "2016-05-22",
  notes = c("Each series starts on its item's first listed day in an open store.",
            "The full panel is about 47 million rows; panel_series() lists the series."),
  access = "import",
  pin = "the competition's data files, fixed since it closed",
  snapshot = NA_character_,
  source = "https://www.kaggle.com/competitions/m5-forecasting-accuracy/data",
  licence = "Kaggle competition rules; not redistributed",
  files = data.frame(
    file = c("sales_train_evaluation.csv", "calendar.csv", "sell_prices.csv"),
    role = c("data", "data", "data"),
    sha256 = c("4b4a47c44c38380d2a9168216fea8c9ff2f31b1ddb772f8a0995952a038b8aa0",
               "d12b5914ef03e66649adf5dd9e996e6602251c22b7a6af8f1f7e3aa12f8860f5",
               "9da3ad1f8b8ccacdbdc70612191dd375ec24a4ac6625c24b75b3bc60b0bed2ef"),
    url = NA_character_, stringsAsFactors = FALSE),
  loader = .ld_m5, index = .ix_m5))

# ---------------------------------------------------------------------------
# Rossmann: daily sales by store, Germany.
#
# Exposure is the source's Open flag. Nothing is derived. 180 stores have no
# rows from 2014-07-01 to 2014-12-31 and one store lacks a day; the rows are
# absent in the source and are left absent.
# ---------------------------------------------------------------------------

.ld_rossmann <- function(dir, series = NULL, exposure = TRUE) {
  tr <- .rd(file.path(dir, "train.csv"), select = c("Store", "Date", "Sales", "Open"))
  tr[, series_id := sprintf("store%04d", Store)]
  if (!is.null(series)) tr <- tr[series_id %in% series]
  st <- .rd(file.path(dir, "store.csv"), select = c("Store", "StoreType", "Assortment"))
  at <- st[match(tr$Store, st$Store)]
  .panel_frame(tr$series_id, tr$Date, tr$Sales, if (exposure) tr$Open,
               list(store = tr$Store, store_type = at$StoreType,
                    assortment = at$Assortment))
}

.ix_rossmann <- function(dir) {
  s <- unique(.rd(file.path(dir, "train.csv"), select = "Store"))
  st <- .rd(file.path(dir, "store.csv"), select = c("Store", "StoreType", "Assortment"))
  at <- st[match(s$Store, st$Store)]
  data.frame(series_id = sprintf("store%04d", s$Store), store = .fct(s$Store),
             store_type = .fct(at$StoreType), assortment = .fct(at$Assortment))
}

.register("rossmann", list(
  title = "Rossmann store sales: daily sales by store",
  domain = "retail", freq = "daily",
  key = "store",
  nesting = c(store_type = "store", assortment = "store"),
  exposure = "the source's Open flag",
  exposure_unit = "binary",
  n_series = 1115L, start = "2013-01-01", end = "2015-07-31",
  notes = c("180 stores have no rows from 2014-07-01 to 2014-12-31.",
            "One store lacks a single day."),
  access = "import",
  pin = "the competition's data files, fixed since it closed",
  snapshot = NA_character_,
  source = "https://www.kaggle.com/competitions/rossmann-store-sales/data",
  licence = "Kaggle competition rules; not redistributed",
  files = data.frame(
    file = c("train.csv", "store.csv"),
    role = c("data", "data"),
    sha256 = c("f6e4597c142d7d909a13d53b68a8e85c00b9a4c7b5ff40adbb37d6829cc1f4cc",
               "f56bd124a2849489e6bbb5c000f5fc9640204355e316475c918ae4d089afb344"),
    url = NA_character_, stringsAsFactors = FALSE),
  loader = .ld_rossmann, index = .ix_rossmann))

# ---------------------------------------------------------------------------
# Portal: rodent captures by plot and species, one census per lunar month,
# Chihuahuan Desert, Arizona, since 1977.
#
# The series is the number of individuals of a species caught on a plot in a
# census. Decisions:
#   * Species code "NA" is Neotoma albigula, not a missing value. It is read
#     as the string it is; a loader that treats it as missing loses 1,593
#     captures.
#   * Records with a blank species are trapping placeholders, not captures.
#   * Only regular censuses (period > 0) are counted or gridded.
#   * A species not caught on a trapped plot is a count of 0.
#   * A plot that was not trapped in a census has no count: y is NA there and
#     exposure 0.
# Exposure is the share of the plot's 49 trap stakes that were set.
# ---------------------------------------------------------------------------

.PORTAL_TAG <- "6.112.0"

.portal_grid <- function(dir) {
  ro <- .rd(file.path(dir, "Portal_rodent.csv"), select = c("period", "plot", "species"))
  # captures outside a regular census of the trapping table drop out at the join
  ro <- ro[!is.na(species) & species != ""]
  cnt <- ro[, .(y = .N), by = .(period, plot, species)]
  tp <- .rd(file.path(dir, "Portal_rodent_trapping.csv"))
  tp <- tp[period > 0]
  tp[, ds := as.Date(sprintf("%d-%02d-%02d", year, month, day))]
  g <- CJ(period = unique(tp$period), plot = unique(tp$plot),
          species = unique(cnt$species))
  g <- tp[, .(period, plot, ds, sampled, effort)][g, on = .(period, plot), nomatch = NULL]
  g[cnt, on = .(period, plot, species), y := i.y]
  g[is.na(y), y := 0L]
  g[sampled == 0, y := NA_integer_]
  g[, series_id := paste(plot, species, sep = "|")]
  g
}

.ld_portal <- function(dir, series = NULL, exposure = TRUE) {
  g <- .portal_grid(dir)
  if (!is.null(series)) g <- g[series_id %in% series]
  .panel_frame(g$series_id, g$ds, g$y, if (exposure) g$effort / 49,
               list(plot = g$plot, species = g$species))
}

.ix_portal <- function(dir) {
  g <- unique(.portal_grid(dir)[, .(series_id, plot, species)])
  data.frame(series_id = g$series_id, plot = .fct(g$plot), species = .fct(g$species))
}

.register("portal", list(
  title = "Portal Project: rodent captures by plot and species",
  domain = "ecology", freq = "monthly (lunar census)",
  key = c("plot", "species"),
  nesting = structure(character(0), names = character(0)),
  exposure = "share of the plot's 49 trap stakes set in that census",
  exposure_unit = "fraction",
  n_series = 1200L, start = "1977-07-16", end = "2026-07-18",
  notes = c("y is NA, and exposure 0, for a plot not trapped in a census.",
            paste("Species codes are as recorded, including codes for",
                  "unidentified and non-target animals.")),
  access = "fetch",
  pin = paste("PortalData release", .PORTAL_TAG),
  snapshot = NA_character_,
  source = paste0("https://github.com/weecology/PortalData/tree/", .PORTAL_TAG),
  licence = "CC0 1.0",
  files = data.frame(
    file = c("Portal_rodent.csv", "Portal_rodent_trapping.csv"),
    role = c("data", "data"),
    sha256 = c("d5659beecbc92da0696a9a7e74c88029f59c5b5b802ca2b0f485d2450b3f4f3a",
               "3c7fe42063e43311ba7777b9e941083a1ceae87233289dba3a58edfdb9ba0b2f"),
    url = paste0("https://raw.githubusercontent.com/weecology/PortalData/",
                 .PORTAL_TAG, "/Rodents/",
                 c("Portal_rodent.csv", "Portal_rodent_trapping.csv")),
    stringsAsFactors = FALSE),
  loader = .ld_portal, index = .ix_portal))

# ---------------------------------------------------------------------------
# ILINet: weekly outpatient visits for influenza-like illness (ILI) by US
# state, from CDC's ILINet, served by the Delphi Epidata API.
#
# y is the number of ILI visits the reporting providers saw that week, and
# exposure the number of patients they saw for any reason, so y / exposure is
# the ILI share CDC publishes. Decisions:
#   * A week is dated by the Sunday that starts its MMWR week.
#   * A week in which no provider reported has no count: y is NA there and
#     exposure 0. The source records it as 0 ILI visits among 0 patients.
#   * Delphi keeps every revision of a week; an issue is the FluView week that
#     published one. Each file holds the weeks one issue last revised, and a
#     week found in two files takes the later issue.
#   * New York is the whole state, Delphi's "ny". New York City and the rest
#     of the state, which Delphi also serves, are not separate series.
# ---------------------------------------------------------------------------

.ILINET_REGIONS <- c(
  "ak", "al", "ar", "az", "ca", "co", "ct", "dc", "de", "fl", "ga", "hi", "ia",
  "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms",
  "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa",
  "pr", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vi", "vt", "wa", "wi", "wv",
  "wy")

# The issue that last revised each season, up to issue 202639. CDC republishes
# a season's figures every week until the end of the following season; every
# week before the 2016-17 season was last revised in issue 201740.
.ILINET_ISSUES <- data.frame(
  from = c(201040L, 201640L, 201740L, 201840L, 201940L,
           202040L, 202140L, 202240L, 202340L, 202440L),
  to = c(201639L, 201739L, 201839L, 201939L, 202039L,
         202139L, 202239L, 202339L, 202439L, 202539L),
  issue = c(201740L, 201839L, 201939L, 202039L, 202139L,
            202239L, 202339L, 202439L, 202538L, 202639L))

.ilinet_url <- function(from, to, issue) paste0(
  "https://api.delphi.cmu.edu/epidata/fluview/?regions=",
  paste(.ILINET_REGIONS, collapse = ","), "&epiweeks=", from, "-", to,
  "&issues=", issue,
  "&format=csv&fields=region,issue,epiweek,num_ili,num_patients,num_providers")

# The Sunday that starts MMWR week `ew`, written yyyyww. Week 1 is the first
# week, Sunday to Saturday, with at least four days in January.
.mmwr_date <- function(ew) {
  jan1 <- as.Date(sprintf("%d-01-01", ew %/% 100L))
  wd <- as.POSIXlt(jan1)$wday
  jan1 - wd + ifelse(wd > 3L, 7L, 0L) + 7L * (ew %% 100L - 1L)
}

.ilinet_read <- function(dir) {
  d <- rbindlist(lapply(file.path(dir, .spec("ilinet")$files$file), .rd,
                        select = c("region", "issue", "epiweek", "num_ili", "num_patients")))
  setorderv(d, c("region", "epiweek", "issue"), c(1L, 1L, -1L))
  unique(d, by = c("region", "epiweek"))
}

.ld_ilinet <- function(dir, series = NULL, exposure = TRUE) {
  d <- .ilinet_read(dir)
  if (!is.null(series)) d <- d[region %in% series]
  d[num_patients == 0, num_ili := NA]
  .panel_frame(d$region, .mmwr_date(d$epiweek), d$num_ili,
               if (exposure) d$num_patients, list(state = d$region))
}

.ix_ilinet <- function(dir) {
  r <- unique(.ilinet_read(dir)$region)
  data.frame(series_id = r, state = .fct(r))
}

.register("ilinet", list(
  title = "CDC ILINet: outpatient visits for influenza-like illness, by state",
  domain = "epidemiology", freq = "weekly (MMWR week, from Sunday)",
  key = "state",
  nesting = structure(character(0), names = character(0)),
  exposure = "patients seen that week, for any reason, by the reporting providers",
  exposure_unit = "count",
  n_series = 53L, start = "2010-10-03", end = "2025-09-21",
  notes = c("y is NA, and exposure 0, in the 41 state-weeks in which no provider reported.",
            paste("Four series start late, as the source does: Puerto Rico in the week",
                  "of 2013-09-29, the US Virgin Islands 2015-10-04, Louisiana",
                  "2016-10-02 and Florida 2021-10-03. The District of Columbia lacks",
                  "one week.")),
  access = "fetch",
  pin = paste("the data as of issue 202639: weeks 2010w40 to 2025w39, each season",
              "as published in the issue that last revised it"),
  snapshot = "2026-10-12",
  source = "https://cmu-delphi.github.io/delphi-epidata/api/fluview.html",
  licence = "Public domain (US Government work, CDC)",
  files = data.frame(
    file = sprintf("fluview_issue%d.csv", .ILINET_ISSUES$issue),
    role = "data",
    sha256 = c("47dd7fad2657d72397e91fd33f04ba2e4f867b97ff65d08368cadaa95468718f",
               "327fe670839036d1485fe11b5ac49a948708f2dc999a664766737d404cb1d6f6",
               "b29917328d02b7c09e4eb9f07050dac3def5047c845db0b87d9f8a9d8f56b16e",
               "ead5b859b9708d4f904d33c1369a8ddab6efbf47b3bd1a5703011b4fc94814e7",
               "ce6368a898f34abc33c23fbc715f1432d7d53057e72a4e50f73f2448660625f0",
               "2425e5e89debb99eee927011bf4a56d5397362c79066e549f5e729dabc7a0368",
               "e579201dfb720150e95dcbb2ab8cdb9c9df595014d4c45890c3e366f107c1f8c",
               "9903d725489cdaea819159267cf9f5717b725fde62d4d70ef1afee1b7510dd7d",
               "fdc197067a249b4be2881c12ae4a37262dc3c284b97af36c89b9270266c5290a",
               "595b3e63c24ba9d70ad4fbf63ec7fca35de5f8066eac01300c9a1d992fe93463"),
    url = .ilinet_url(.ILINET_ISSUES$from, .ILINET_ISSUES$to, .ILINET_ISSUES$issue),
    stringsAsFactors = FALSE),
  loader = .ld_ilinet, index = .ix_ilinet))

# ---------------------------------------------------------------------------
# FluNet: weekly influenza virus detections by country, from WHO's FluNet,
# served by the xmart OData API (table VIW_FNT).
#
# y is the number of specimens positive for influenza, and exposure the number
# of specimens processed. A country reports a week in up to three rows, by the
# origin of the specimens: sentinel, non-sentinel, or not defined. Decisions:
#   * FluNet writes null rather than 0 in INF_ALL for a report with no
#     positives. INF_ALL is never 0 in the table, INF_A and INF_B are never
#     positive where it is null, and where such a report gives its negatives
#     they equal its specimens processed in 91% of cases. A null INF_ALL on a
#     report with specimens processed is a count of 0.
#   * A report that gives no number of specimens processed (null or 0) adds
#     nothing, so that y counts the positives among the specimens exposure
#     counts. A week in which no report gives that number has no count: y is
#     NA there and exposure 0.
#   * Reports of different origin are different specimens and are summed,
#     except a not-defined report that repeats the week's other reports, or
#     one of them, in specimens processed and positives: that is the same
#     specimens reported twice, as in every such week in Austria, Denmark and
#     Turkey.
#   * A report repeated in every column is one report.
# ---------------------------------------------------------------------------

.FLUNET_COLS <- c(
  "WHOREGION", "COUNTRY_CODE", "COUNTRY_AREA_TERRITORY", "ISO_WEEKSTARTDATE",
  "ISO_YEAR", "ISO_WEEK", "ORIGIN_SOURCE", "SPEC_RECEIVED_NB", "SPEC_PROCESSED_NB",
  "AH1N12009", "AH1", "AH3", "AH5", "AH7N9", "ANOTSUBTYPED", "ANOTSUBTYPABLE",
  "AOTHER_SUBTYPE", "INF_A", "BVIC_2DEL", "BVIC_3DEL", "BVIC_NODEL",
  "BVIC_DELUNK", "BYAM", "BNOTDETERMINED", "INF_B", "INF_ALL", "INF_NEGATIVE")

.FLUNET_URL <- paste0(
  "https://xmart-api-public.who.int/FLUMART/VIW_FNT?%24format=csv",
  "&%24select=", paste(.FLUNET_COLS, collapse = ","),
  "&%24filter=ISO_WEEKSTARTDATE%20lt%202025-12-29",
  "&%24orderby=COUNTRY_CODE,ISO_WEEKSTARTDATE,ORIGIN_SOURCE")

.flunet_weeks <- function(dir) {
  d <- unique(.rd(file.path(dir, "VIW_FNT.csv"), na.strings = ""))
  d[, `:=`(p = as.numeric(SPEC_PROCESSED_NB), pos = as.numeric(INF_ALL))]
  d[is.na(p), p := 0]
  d[, use := p > 0]
  d[use & is.na(pos), pos := 0]
  # a not-defined report that repeats, in specimens and positives, the week's
  # other reports or one of them is the same specimens reported twice
  d[, again := use & ORIGIN_SOURCE == "NOTDEFINED" & {
    o <- use & ORIGIN_SOURCE != "NOTDEFINED"
    (p == sum(p[o]) & pos == sum(pos[o])) | paste(p, pos) %in% paste(p, pos)[o]
  }, by = .(COUNTRY_CODE, ISO_WEEKSTARTDATE)]
  d[again == TRUE, use := FALSE]
  w <- d[, .(y = sum(pos[use]), e = sum(p[use]), obs = any(use)),
         by = .(COUNTRY_CODE, WHOREGION, ISO_WEEKSTARTDATE)]
  w[obs == FALSE, y := NA]
  w
}

.ld_flunet <- function(dir, series = NULL, exposure = TRUE) {
  w <- .flunet_weeks(dir)
  if (!is.null(series)) w <- w[COUNTRY_CODE %in% series]
  .panel_frame(w$COUNTRY_CODE, w$ISO_WEEKSTARTDATE, w$y, if (exposure) w$e,
               list(country = w$COUNTRY_CODE, who_region = w$WHOREGION))
}

.ix_flunet <- function(dir) {
  s <- unique(.rd(file.path(dir, "VIW_FNT.csv"), select = c("COUNTRY_CODE", "WHOREGION")))
  data.frame(series_id = s$COUNTRY_CODE, country = .fct(s$COUNTRY_CODE),
             who_region = .fct(s$WHOREGION))
}

.register("flunet", list(
  title = "WHO FluNet: specimens positive for influenza, by country",
  domain = "epidemiology", freq = "weekly (ISO week, from Monday)",
  key = "country",
  nesting = c(who_region = "country"),
  exposure = "specimens processed that week, in the reports that give a number",
  exposure_unit = "count",
  n_series = 189L, start = "1995-01-02", end = "2025-12-22",
  notes = c(paste("y is NA, and exposure 0, in the 22,678 country-weeks with no report",
                  "of specimens processed. Liechtenstein has none in any week."),
            paste("Positives reported without a number of specimens processed are not",
                  "in y: 10.5% of all positives, among them most of Japan's and Hong",
                  "Kong's and the non-sentinel reports of England, Switzerland and the",
                  "Netherlands."),
            "Positives exceed specimens processed in 213 country-weeks; both are as recorded.",
            "The four nations of the United Kingdom are separate series, X09 to X12."),
  access = "fetch",
  pin = paste("the query only: its columns, the weeks starting 1995-01-02 to",
              "2025-12-22 and the row order. WHO revises FluNet in place, so a",
              "later fetch can return different data"),
  snapshot = "2026-10-12",
  source = "https://www.who.int/tools/flunet",
  licence = "CC BY-NC-SA 3.0 IGO (WHO): non-commercial use, with attribution",
  files = data.frame(
    file = "VIW_FNT.csv", role = "data",
    sha256 = "9cd765c188f291530e9825dae56e695bb779a9c19758b3a6e591d3659ce2a1db",
    url = .FLUNET_URL, stringsAsFactors = FALSE),
  loader = .ld_flunet, index = .ix_flunet))

# ---------------------------------------------------------------------------
# North American Breeding Bird Survey: birds counted along roadside routes in
# the United States and Canada, once a year in the breeding season, by route
# and species, since 1966.
#
# y is the number of birds of a species counted on a run of a route, over its
# 50 stops. Decisions:
#   * Only runs under the survey's standard protocol count (RPID 101). The
#     464 of the 135,529 runs under other protocols are left out.
#   * A series is a species recorded on a standard run of the route. It has a
#     row for every standard run of the route listed in weather.csv, and a
#     species not recorded on a run counts 0 there: the source lists only the
#     species seen.
#   * A run whose conditions failed the survey's criteria (RunType 0: weather,
#     date, time of day or route completion) has y NA and exposure 0. Its
#     counts are in the source, but the survey marks them as not comparable
#     with those of acceptable runs.
#   * Every state and province file in States.zip is read.
#   * A row is dated by the day the route was run. A route is run at most
#     once a year; no route was run in 2020.
# Exposure is RunType: 1 for a run that met the criteria, 0 otherwise.
# ---------------------------------------------------------------------------

.BBS_ITEM <- "64ad9c3dd34e70357a292cee"

# Counts on standard runs, one row per run and species. `route` is the BBS
# route number, StateNum * 1000 + Route.
.bbs_counts <- function(dir) {
  zip <- file.path(dir, "States.zip")
  fs <- utils::unzip(zip, list = TRUE)$Name
  fs <- fs[grepl("\\.csv$", fs, ignore.case = TRUE)]
  td <- tempfile("tspanels-bbs-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  ct <- rbindlist(lapply(fs, function(f) {
    utils::unzip(zip, files = f, exdir = td, junkpaths = TRUE)
    p <- file.path(td, basename(f))
    x <- .rd(p, select = c("RouteDataID", "StateNum", "Route", "RPID", "AOU", "SpeciesTotal"))
    unlink(p)
    x[RPID == 101L]
  }))
  ct[, .(y = sum(SpeciesTotal)), by = .(RouteDataID, route = StateNum * 1000L + Route, AOU)]
}

.bbs_runs <- function(dir) {
  wx <- .rd(file.path(dir, "weather.csv"),
            select = c("RouteDataID", "CountryNum", "StateNum", "Route", "RPID",
                       "Year", "Month", "Day", "RunType"))
  wx <- wx[RPID == 101L]
  wx[, `:=`(route = StateNum * 1000L + Route,
            ds = as.Date(sprintf("%d-%02d-%02d", Year, Month, Day)))]
  wx
}

# One row per series, with its margins, in the order the count files give
# them; only routes with a standard run in weather.csv have series.
.bbs_series <- function(ct, runs) {
  sp <- unique(ct[, .(route, AOU)])
  at <- unique(runs[, .(route, StateNum, CountryNum)])
  sp <- at[sp, on = "route", nomatch = NULL]
  sp[, series_id := paste(route, AOU, sep = "|")]
  sp
}

.ld_bbs <- function(dir, series = NULL, exposure = TRUE) {
  ct <- .bbs_counts(dir)
  runs <- .bbs_runs(dir)
  sp <- .bbs_series(ct, runs)
  if (!is.null(series)) sp <- sp[series_id %in% series]
  sp[, sid := .I]
  g <- runs[, .(RouteDataID, route, ds, RunType)][sp[, .(route, AOU, sid)], on = "route",
                                                 allow.cartesian = TRUE]
  g[ct, on = .(RouteDataID, AOU), y := i.y]
  g[is.na(y), y := 0L]
  g[RunType == 0L, y := NA]
  # the margins are integer codes, so their levels sort numerically; the
  # series_id strings are factored once per series, then indexed
  i <- g$sid
  .panel_frame(.fct(sp$series_id)[i], g$ds, g$y, if (exposure) g$RunType,
               list(route = sp$route[i], species = sp$AOU[i],
                    state = sp$StateNum[i], country = sp$CountryNum[i]))
}

.ix_bbs <- function(dir) {
  sp <- .bbs_series(.bbs_counts(dir), .bbs_runs(dir))
  data.frame(series_id = sp$series_id, route = .fct(sp$route), species = .fct(sp$AOU),
             state = .fct(sp$StateNum), country = .fct(sp$CountryNum))
}

.register("bbs", list(
  title = "North American Breeding Bird Survey: birds counted, by route and species",
  domain = "ecology", freq = "annual (one run per route)",
  key = c("route", "species"),
  nesting = c(state = "route", country = "state"),
  exposure = "RunType: 1 if the run met the survey's criteria, 0 otherwise",
  exposure_unit = "binary",
  n_series = 523994L, start = "1966-05-24", end = "2022-07-15",
  notes = c(paste("y is NA, and exposure 0, on the 12,650 of 135,065 standard runs",
                  "(9.4%) that failed the survey's criteria."),
            "No route was run in 2020.",
            paste("Species are AOU codes as recorded; the survey's taxa include",
                  "subspecies and unidentified species groups as well as species."),
            "The full panel is 14.0 million rows; panel_series() lists the series."),
  access = "fetch",
  pin = paste0("the 2023 data release (data to 2022), ScienceBase item ", .BBS_ITEM,
               "; USGS has since superseded it"),
  snapshot = NA_character_,
  source = paste0("https://www.sciencebase.gov/catalog/item/", .BBS_ITEM),
  licence = paste("Public domain (US Government work, USGS); cite Ziolkowski et al.",
                  "(2023), doi:10.5066/P9GS9K64"),
  files = data.frame(
    file = c("States.zip", "weather.csv"),
    role = c("data", "data"),
    sha256 = c("d5f08650aa469a5b6af0c02661ee8bd93e5210430c2071bcda3287e826ede9c9",
               "c3e4d9d10a4240d7e28ae8d80880c9198a70cdb991a91b73d6d954cfb35d3c5e"),
    url = paste0("https://www.sciencebase.gov/catalog/file/get/", .BBS_ITEM, "?f=__disk__",
                 c("3c%2F04%2F38%2F3c0438bc33761eb7c5aca1b60def515dd17a4a8e",
                   "2a%2F11%2F1f%2F2a111f9305be6d61949624cfba3377bc3f35327a")),
    stringsAsFactors = FALSE),
  loader = .ld_bbs, index = .ix_bbs))
