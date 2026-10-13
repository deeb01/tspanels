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
