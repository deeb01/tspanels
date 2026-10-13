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
