# Tiny raw files in each source's own schema, written into a temporary cache.
# Every case a loader decides on has a row here, so the tests pin the
# decisions rather than the happy path.

local_cache <- function(env = parent.frame()) {
  root <- tempfile("tspanels-cache-")
  dir.create(root)
  old <- Sys.getenv("TSPANELS_CACHE", NA)
  Sys.setenv(TSPANELS_CACHE = root)
  withr::defer({
    if (is.na(old)) Sys.unsetenv("TSPANELS_CACHE") else Sys.setenv(TSPANELS_CACHE = old)
    unlink(root, recursive = TRUE)
  }, envir = env)
  root
}

write_raw <- function(name, file, df) {
  dir <- file.path(Sys.getenv("TSPANELS_CACHE"), name)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, file.path(dir, file), row.names = FALSE)
}

# Favorita: store 2 has no transactions row on day 3 (shut, no sales) and none
# on day 4 although it sold (the case transactions.csv alone gets wrong).
fixture_favorita <- function() {
  days <- as.Date("2015-03-01") + 0:4
  tr <- expand.grid(date = days, store_nbr = 1:2, family = c("BREAD", "AUTOMOTIVE"),
                    stringsAsFactors = FALSE)
  tr$sales <- 5
  tr$sales[tr$store_nbr == 2 & tr$date == days[3]] <- 0
  tr$sales[tr$store_nbr == 2 & tr$date == days[4] & tr$family == "AUTOMOTIVE"] <- 0
  tr$id <- seq_len(nrow(tr)) - 1L
  tr$onpromotion <- 0L
  tr$date <- format(tr$date)
  write_raw("favorita", "train.csv", tr[c("id", "date", "store_nbr", "family", "sales", "onpromotion")])
  write_raw("favorita", "stores.csv", data.frame(
    store_nbr = 1:2, city = c("Quito", "Guayaquil"), state = c("Pichincha", "Guayas"),
    type = c("D", "A"), cluster = c(13L, 3L)))
  tx <- expand.grid(date = format(days), store_nbr = 1:2, stringsAsFactors = FALSE)
  tx <- tx[!(tx$store_nbr == 2 & tx$date %in% format(days[3:4])), ]
  tx$transactions <- 100L
  write_raw("favorita", "transactions.csv", tx)
  invisible(days)
}

# M5: two items in two stores over six days, weeks 11101 (d_1..d_3) and 11102
# (d_4..d_6). HOBBIES_1_001 is first listed at CA_1 in week 11102, so its
# series there starts on d_4. CA_2 sells nothing on d_5, so it was shut.
fixture_m5 <- function() {
  ids <- expand.grid(item_id = c("FOODS_1_001", "HOBBIES_1_001"),
                     store_id = c("CA_1", "CA_2"), stringsAsFactors = FALSE)
  ids$dept_id <- sub("_[0-9]+$", "", ids$item_id)
  ids$cat_id <- sub("_.*$", "", ids$item_id)
  ids$state_id <- "CA"
  ids$id <- paste(ids$item_id, ids$store_id, "evaluation", sep = "_")
  y <- matrix(c(1, 2, 1, 3, 1, 2,     # FOODS CA_1
                0, 0, 0, 2, 0, 1,     # HOBBIES CA_1, unlisted before d_4
                4, 4, 4, 4, 0, 4,     # FOODS CA_2, shut on d_5
                1, 0, 1, 1, 0, 1),    # HOBBIES CA_2, shut on d_5
              nrow = 4, byrow = TRUE, dimnames = list(NULL, paste0("d_", 1:6)))
  write_raw("m5", "sales_train_evaluation.csv",
            cbind(ids[c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id")], y))
  write_raw("m5", "calendar.csv", data.frame(
    date = format(as.Date("2011-01-29") + 0:5), wm_yr_wk = rep(c(11101L, 11102L), each = 3),
    weekday = "x", d = paste0("d_", 1:6)))
  pr <- rbind(
    data.frame(store_id = "CA_1", item_id = "FOODS_1_001", wm_yr_wk = c(11101L, 11102L)),
    data.frame(store_id = "CA_1", item_id = "HOBBIES_1_001", wm_yr_wk = 11102L),
    data.frame(store_id = "CA_2", item_id = c("FOODS_1_001", "HOBBIES_1_001"), wm_yr_wk = 11101L),
    data.frame(store_id = "CA_2", item_id = c("FOODS_1_001", "HOBBIES_1_001"), wm_yr_wk = 11102L))
  pr$sell_price <- 1.5
  write_raw("m5", "sell_prices.csv", pr)
}

fixture_rossmann <- function() {
  tr <- expand.grid(Store = 1:2, Date = format(as.Date("2015-07-29") + 0:2),
                    stringsAsFactors = FALSE)
  tr$DayOfWeek <- 1L
  tr$Open <- 1L
  tr$Open[tr$Store == 2 & tr$Date == "2015-07-30"] <- 0L
  tr$Sales <- ifelse(tr$Open == 1L, 1000, 0)
  tr$Customers <- 10L; tr$Promo <- 0L; tr$StateHoliday <- "0"; tr$SchoolHoliday <- 0L
  write_raw("rossmann", "train.csv", tr[c("Store", "DayOfWeek", "Date", "Sales", "Customers",
                                          "Open", "Promo", "StateHoliday", "SchoolHoliday")])
  write_raw("rossmann", "store.csv", data.frame(
    Store = 1:2, StoreType = c("c", "a"), Assortment = c("a", "c"),
    CompetitionDistance = c(1270, 570)))
}

# Portal: species "NA" is Neotoma albigula. A blank species is a placeholder.
# Period -1 is not a regular census. Plot 2 is half trapped in period 1 and
# not trapped in period 2.
fixture_portal <- function() {
  write_raw("portal", "Portal_rodent.csv", data.frame(
    recordID = 1:7,
    period = c(1L, 1L, 1L, 1L, 2L, -1L, 1L),
    plot = c(1L, 1L, 1L, 2L, 1L, 1L, 2L),
    species = c("NA", "NA", "DM", "", "DM", "DM", "DM"),
    stringsAsFactors = FALSE))
  write_raw("portal", "Portal_rodent_trapping.csv", data.frame(
    day = c(2L, 16L, 16L, 14L, 14L), month = c(7L, 7L, 7L, 8L, 8L), year = 1977L,
    period = c(-1L, 1L, 1L, 2L, 2L), plot = c(1L, 1L, 2L, 1L, 2L),
    sampled = c(1L, 1L, 1L, 1L, 0L), effort = c(49L, 49L, 24L, 49L, 0L), qcflag = 1L))
}

# ILINet: one file per Delphi issue, as fetched. "ca" runs through the 53-week
# MMWR year 2014 into 2015, and its week 201501 is revised in a later issue.
# "vi" has a week in which no provider reported, and lacks 201501.
fixture_ilinet <- function() {
  rows <- data.frame(
    region = c("ca", "ca", "ca", "vi", "vi", "ca"),
    issue = c(201740L, 201740L, 201740L, 201740L, 201740L, 201839L),
    epiweek = c(201452L, 201453L, 201501L, 201452L, 201453L, 201501L),
    num_ili = c(500L, 650L, 700L, 4L, 0L, 720L),
    num_patients = c(30000L, 31000L, 32000L, 359L, 0L, 32500L),
    num_providers = c(110L, 112L, 111L, 1L, 0L, 113L))
  for (f in panel_info("ilinet")$files$file) {
    iss <- as.integer(sub("^fluview_issue([0-9]+)\\.csv$", "\\1", f))
    write_raw("ilinet", f, rows[rows$issue == iss, ])
  }
}

# FluNet: each row is a country's report for an ISO week from one origin of
# specimens. AAA has a report with no positives, written as null. BBB reports
# sentinel and non-sentinel specimens; in its second week the non-sentinel
# report gives positives but no specimens processed, and in its third a
# not-defined report repeats the sum of the other two. CCC never gives a
# number of specimens processed. DDD's report is repeated exactly. EEE's
# not-defined report stands beside a non-sentinel one without specimens. FFF's
# not-defined report counts other specimens in one week and repeats its
# sentinel report in the next.
fixture_flunet <- function() {
  wk <- format(as.Date("2017-01-02") + 7 * 0:2)
  r <- function(cc, region, w, origin, proc, all = NA, neg = NA) data.frame(
    WHOREGION = region, COUNTRY_CODE = cc, COUNTRY_AREA_TERRITORY = paste("Land", cc),
    ISO_WEEKSTARTDATE = wk[w], ISO_YEAR = 2017L, ISO_WEEK = w, ORIGIN_SOURCE = origin,
    SPEC_RECEIVED_NB = NA, SPEC_PROCESSED_NB = proc,
    AH1N12009 = NA, AH1 = NA, AH3 = NA, AH5 = NA, AH7N9 = NA, ANOTSUBTYPED = NA,
    ANOTSUBTYPABLE = NA, AOTHER_SUBTYPE = NA, INF_A = all, BVIC_2DEL = NA,
    BVIC_3DEL = NA, BVIC_NODEL = NA, BVIC_DELUNK = NA, BYAM = NA,
    BNOTDETERMINED = NA, INF_B = NA, INF_ALL = all, INF_NEGATIVE = neg)
  d <- rbind(
    r("AAA", "AMR", 1, "NOTDEFINED", 40, 11, 25),
    r("AAA", "AMR", 2, "NOTDEFINED", 20, NA, 20),
    r("BBB", "EUR", 1, "SENTINEL", 50, 5),
    r("BBB", "EUR", 1, "NONSENTINEL", 30),
    r("BBB", "EUR", 2, "SENTINEL", 10, 2),
    r("BBB", "EUR", 2, "NONSENTINEL", NA, 40),
    r("BBB", "EUR", 3, "NOTDEFINED", 60, 6),
    r("BBB", "EUR", 3, "SENTINEL", 40, 4),
    r("BBB", "EUR", 3, "NONSENTINEL", 20, 2),
    r("CCC", "WPR", 1, "SENTINEL", NA, 30),
    r("CCC", "WPR", 2, "SENTINEL", 0),
    r("DDD", "SEAR", 1, "SENTINEL", 10, 3),
    r("DDD", "SEAR", 1, "SENTINEL", 10, 3),
    r("EEE", "AFR", 1, "NOTDEFINED", 100, 10),
    r("EEE", "AFR", 1, "NONSENTINEL", NA, 5),
    r("FFF", "WPR", 1, "NOTDEFINED", 92, 20),
    r("FFF", "WPR", 1, "NONSENTINEL", 27, 3),
    r("FFF", "WPR", 2, "NOTDEFINED", 15, 1),
    r("FFF", "WPR", 2, "SENTINEL", 15, 1),
    r("FFF", "WPR", 2, "NONSENTINEL", 8))
  dir <- file.path(Sys.getenv("TSPANELS_CACHE"), "flunet")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # the API writes a null as an empty field
  utils::write.csv(d, file.path(dir, "VIW_FNT.csv"), row.names = FALSE, na = "")
}

# BBS: lines as the source writes them, with CRLF endings, zero-padded codes
# and space-padded counts. Route 2001 (state 2, US) is run in 2017, 2018,
# 2019 and 2021; its 2019 run failed the survey's criteria (RunType 0), and a
# replicate run (RPID 102) in 2018 records a species seen nowhere else. Route
# 4001 (state 4, Canada) is in a second state file and run in 2018 and 2019.
fixture_bbs <- function() {
  dir <- file.path(Sys.getenv("TSPANELS_CACHE"), "bbs")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  crlf <- function(lines, path)
    writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), path)
  cnt <- function(id, cn, st, yr, aou, n, rpid = 101L) sprintf(
    "%d,%d,%02d,001,%d,%d,%05d,0   ,0   ,0   ,0   ,%-4d,1 ,%-4d",
    id, cn, st, rpid, yr, aou, n, n)
  hdr <- paste("RouteDataID,CountryNum,StateNum,Route,RPID,Year,AOU,Count10,Count20,",
               "Count30,Count40,Count50,StopTotal,SpeciesTotal", sep = "")
  stage <- tempfile("bbs-states-")
  dir.create(stage)
  crlf(c(hdr,
         cnt(1, 840, 2, 2017, 7360, 2),
         cnt(2, 840, 2, 2018, 6882, 1),
         cnt(3, 840, 2, 2019, 7360, 4),
         cnt(3, 840, 2, 2019, 4740, 1),
         cnt(4, 840, 2, 2021, 7360, 3),
         cnt(5, 840, 2, 2018, 9999, 6, rpid = 102L)),
       file.path(stage, "Alabama.csv"))
  crlf(c(hdr, cnt(6, 124, 4, 2018, 3250, 5)), file.path(stage, "Alberta.csv"))
  old <- setwd(stage)
  on.exit(setwd(old))
  utils::zip(file.path(dir, "States.zip"), c("Alabama.csv", "Alberta.csv"), flags = "-q")
  run <- function(id, cn, st, yr, mo, dy, runtype, rpid = 101L) sprintf(
    "%d,%d,%02d,001,%d,%d,%-4d,%-4d,       1150175,47  ,65  ,60  ,F,1,3,1,2,0450,0900,0   ,1,%d",
    id, cn, st, rpid, yr, mo, dy, runtype)
  crlf(c(paste("RouteDataID,CountryNum,StateNum,Route,RPID,Year,Month,Day,ObsN,TotalSpp,",
               "StartTemp,EndTemp,TempScale,StartWind,EndWind,StartSky,EndSky,StartTime,",
               "EndTime,Assistant,QualityCurrentID,RunType", sep = ""),
         run(1, 840, 2, 2017, 6, 4, 1),
         run(2, 840, 2, 2018, 6, 10, 1),
         run(3, 840, 2, 2019, 6, 2, 0),
         run(4, 840, 2, 2021, 6, 13, 1),
         run(5, 840, 2, 2018, 6, 11, 1, rpid = 102L),
         run(6, 124, 4, 2018, 6, 17, 1),
         run(7, 124, 4, 2019, 6, 16, 1)),
       file.path(dir, "weather.csv"))
}
