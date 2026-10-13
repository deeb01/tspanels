row_of <- function(d, id, day) d[d$series_id == id & d$ds == as.Date(day), ]

test_that("favorita: open is a transactions row or any sales that day", {
  local_cache(); fixture_favorita()
  d <- load_panel("favorita")
  expect_equal(levels(d$series_id), c("1|AUTOMOTIVE", "1|BREAD", "2|AUTOMOTIVE", "2|BREAD"))
  expect_equal(nrow(d), 20L)
  # no transactions row and no sales: shut
  expect_equal(row_of(d, "2|BREAD", "2015-03-03")$exposure, 0)
  # no transactions row but bread sold: open, for every family in the store
  expect_equal(row_of(d, "2|BREAD", "2015-03-04")$exposure, 1)
  expect_equal(row_of(d, "2|AUTOMOTIVE", "2015-03-04")$exposure, 1)
  expect_equal(sum(d$exposure == 0), 2L)
  # attributes ride along and nest
  expect_equal(as.character(row_of(d, "2|BREAD", "2015-03-01")$state), "Guayas")
  expect_equal(levels(d$cluster), c("3", "13"))
})

test_that("favorita without exposure does not need transactions.csv", {
  local_cache(); fixture_favorita()
  unlink(file.path(panel_dir("favorita"), "transactions.csv"))
  expect_error(load_panel("favorita"), "exposure = FALSE")
  d <- load_panel("favorita", exposure = FALSE)
  expect_false("exposure" %in% names(d))
  expect_equal(nrow(d), 20L)
  expect_true(panels()$available[panels()$name == "favorita"])
})

test_that("m5: series start at first listed day in an open store; shut days are 0", {
  local_cache(); fixture_m5()
  d <- load_panel("m5")
  expect_equal(nrow(d), 21L)
  h1 <- d[d$series_id == "HOBBIES_1_001_CA_1_evaluation", ]
  expect_equal(h1$ds, as.Date("2011-01-29") + 3:5)
  expect_equal(h1$y, c(2, 0, 1))
  f2 <- d[d$series_id == "FOODS_1_001_CA_2_evaluation", ]
  expect_equal(f2$exposure, c(1, 1, 1, 1, 0, 1))
  expect_true(all(d$exposure[d$store_id == "CA_1"] == 1))
  expect_equal(as.character(unique(d$cat_id[d$item_id == "HOBBIES_1_001"])), "HOBBIES")
  # the start is a property of the series, not of the exposure column
  d0 <- load_panel("m5", exposure = FALSE)
  expect_equal(d0, d[setdiff(names(d), "exposure")])
})

test_that("m5: a series subset gives the same rows as the full panel", {
  local_cache(); fixture_m5()
  ids <- panel_series("m5")$series_id
  expect_equal(ids, c("FOODS_1_001_CA_1_evaluation", "HOBBIES_1_001_CA_1_evaluation",
                      "FOODS_1_001_CA_2_evaluation", "HOBBIES_1_001_CA_2_evaluation"))
  full <- load_panel("m5")
  sub <- load_panel("m5", series = ids[2:3])
  keep <- full[full$series_id %in% ids[2:3], ]
  keep$series_id <- droplevels(keep$series_id)
  for (m in c("item_id", "store_id", "dept_id", "cat_id", "state_id"))
    keep[[m]] <- droplevels(keep[[m]])
  rownames(keep) <- NULL
  expect_equal(sub, keep)
})

test_that("rossmann: Open is the exposure, attributes come from store.csv", {
  local_cache(); fixture_rossmann()
  d <- load_panel("rossmann")
  expect_equal(levels(d$series_id), c("store0001", "store0002"))
  expect_equal(row_of(d, "store0002", "2015-07-30")$exposure, 0)
  expect_equal(as.character(d$store_type[d$series_id == "store0001"][1]), "c")
})

test_that("portal: NA is a species, blanks and irregular censuses are not, untrapped is NA", {
  local_cache(); fixture_portal()
  d <- load_panel("portal")
  expect_equal(levels(d$series_id), c("1|DM", "1|NA", "2|DM", "2|NA"))
  expect_equal(levels(d$species), c("DM", "NA"))
  expect_equal(row_of(d, "1|NA", "1977-07-16")$y, 2)
  expect_equal(row_of(d, "1|NA", "1977-08-14")$y, 0)
  # period -1 is not a census: no row for it, and its capture is not counted
  expect_equal(nrow(d), 8L)
  expect_false(as.Date("1977-07-02") %in% d$ds)
  expect_equal(row_of(d, "1|DM", "1977-08-14")$y, 1)
  expect_equal(row_of(d, "2|DM", "1977-07-16")$exposure, 24 / 49)
  untrapped <- row_of(d, "2|NA", "1977-08-14")
  expect_true(is.na(untrapped$y))
  expect_equal(untrapped$exposure, 0)
  # dropping exposure keeps the NA: an untrapped plot is not a zero count
  d0 <- load_panel("portal", exposure = FALSE)
  expect_true(is.na(row_of(d0, "2|NA", "1977-08-14")$y))
})

test_that("a panel that records no exposure loads without the column", {
  local_cache()
  tspanels:::.register("toy", list(
    title = "toy", domain = "test", freq = "daily", key = "g",
    nesting = c(h = "g"), exposure = NA_character_, exposure_unit = NA_character_,
    n_series = 2L, start = "2020-01-01", end = "2020-01-03", notes = character(0),
    access = "fetch", source = "", licence = "",
    files = data.frame(file = "toy.csv", role = "data", sha256 = "", url = "",
                       stringsAsFactors = FALSE),
    loader = function(dir, series, exposure) {
      stopifnot(!exposure)
      x <- utils::read.csv(file.path(dir, "toy.csv"))
      tspanels:::.panel_frame(x$id, x$ds, x$y, NULL, list(g = x$g, h = x$h))
    },
    index = function(dir) unique(utils::read.csv(file.path(dir, "toy.csv"))[c("id", "g", "h")])))
  on.exit(rm("toy", envir = tspanels:::.PANELS), add = TRUE)
  write_raw("toy", "toy.csv", data.frame(
    id = rep(c("a", "b"), each = 3), ds = rep(c("2020-01-01", "2020-01-02", "2020-01-03"), 2),
    y = 1:6, g = rep(c("a", "b"), each = 3), h = "z"))
  d <- load_panel("toy")
  expect_equal(names(d), c("series_id", "ds", "y", "g", "h"))
  expect_identical(load_panel("toy", exposure = FALSE), d)
  expect_true(is.na(panels()$exposure[panels()$name == "toy"]))
})

test_that("unknown series and bad arguments are refused", {
  local_cache(); fixture_rossmann()
  expect_error(load_panel("rossmann", series = "store9999"), "None of `series`")
  expect_warning(d <- load_panel("rossmann", series = c("store0001", "store9999")),
                 "1 of `series` not in panel")
  expect_equal(levels(d$series_id), "store0001")
  expect_error(load_panel("rossmann", exposure = NA), "TRUE or FALSE")
  expect_error(load_panel("nope"), "Unknown panel")
})

test_that("factor levels sort the same on every machine", {
  f <- tspanels:::.fct(c("b", "B", "a", "b", NA))
  expect_equal(levels(f), c("B", "a", "b"))
  expect_equal(as.integer(f), c(3L, 1L, 2L, 3L, NA))
  expect_equal(levels(tspanels:::.fct(c(10L, 2L, 1L))), c("1", "2", "10"))
  g <- factor(c("x", "y", "x"), levels = c("y", "x", "unused"))
  r <- tspanels:::.fct(g)
  expect_equal(levels(r), c("x", "y"))
  expect_equal(as.character(r), c("x", "y", "x"))
})

test_that("ilinet: weeks start on the MMWR Sunday, across a 53-week year", {
  local_cache(); fixture_ilinet()
  d <- load_panel("ilinet")
  expect_equal(levels(d$series_id), c("ca", "vi"))
  expect_equal(d$ds[d$series_id == "ca"], as.Date(c("2014-12-21", "2014-12-28", "2015-01-04")))
  # vi lacks 201501 in the source: absent, not filled
  expect_equal(nrow(d), 5L)
  expect_equal(sort(panel_series("ilinet")$series_id), levels(d$series_id))
})

test_that("ilinet: a week revised in a later issue takes it; counts are not scaled", {
  local_cache(); fixture_ilinet()
  d <- load_panel("ilinet")
  expect_equal(row_of(d, "ca", "2015-01-04")$y, 720)
  expect_equal(row_of(d, "ca", "2015-01-04")$exposure, 32500)
  expect_equal(panel_info("ilinet")$exposure_unit, "count")
})

test_that("ilinet: New York is fetched whole, not as the city and the rest", {
  u <- panel_info("ilinet")$files$url
  regions <- unique(unlist(strsplit(sub("^.*regions=([^&]*)&.*$", "\\1", u), ",")))
  expect_length(regions, 53L)
  expect_true("ny" %in% regions)
  expect_false(any(c("jfk", "ny_minus_jfk") %in% regions))
})

test_that("ilinet: a week in which no provider reported has no count", {
  local_cache(); fixture_ilinet()
  d <- load_panel("ilinet")
  none <- row_of(d, "vi", "2014-12-28")
  expect_true(is.na(none$y))
  expect_equal(none$exposure, 0)
  d0 <- load_panel("ilinet", exposure = FALSE)
  expect_equal(d0, d[setdiff(names(d), "exposure")])
})

test_that("flunet: a null INF_ALL on a report with specimens processed is 0", {
  local_cache(); fixture_flunet()
  d <- load_panel("flunet")
  expect_equal(row_of(d, "AAA", "2017-01-09")$y, 0)
  expect_equal(row_of(d, "AAA", "2017-01-09")$exposure, 20)
  expect_equal(row_of(d, "AAA", "2017-01-02")$y, 11)
})

test_that("flunet: sentinel and non-sentinel reports are summed", {
  local_cache(); fixture_flunet()
  d <- load_panel("flunet")
  expect_equal(row_of(d, "BBB", "2017-01-02")[c("y", "exposure")],
               data.frame(y = 5, exposure = 80), ignore_attr = TRUE)
})

test_that("flunet: positives without specimens processed are left out", {
  local_cache(); fixture_flunet()
  d <- load_panel("flunet")
  # the non-sentinel report's 40 positives have no specimens to count them among
  expect_equal(row_of(d, "BBB", "2017-01-09")[c("y", "exposure")],
               data.frame(y = 2, exposure = 10), ignore_attr = TRUE)
  # a week without a report of specimens processed has no count
  ccc <- d[d$series_id == "CCC", ]
  expect_true(all(is.na(ccc$y)))
  expect_equal(ccc$exposure, c(0, 0))
  d0 <- load_panel("flunet", exposure = FALSE)
  expect_equal(d0, d[setdiff(names(d), "exposure")])
})

test_that("flunet: a not-defined report counts unless it repeats the others", {
  local_cache(); fixture_flunet()
  d <- load_panel("flunet")
  ye <- function(id, day) unlist(row_of(d, id, day)[c("y", "exposure")], use.names = FALSE)
  # repeats the sum of the sentinel and non-sentinel reports
  expect_equal(ye("BBB", "2017-01-16"), c(6, 60))
  # repeats the sentinel report
  expect_equal(ye("FFF", "2017-01-09"), c(1, 23))
  # other specimens: summed
  expect_equal(ye("FFF", "2017-01-02"), c(23, 119))
  # the only report with specimens processed
  expect_equal(ye("EEE", "2017-01-02"), c(10, 100))
})

test_that("flunet: a report repeated exactly counts once; regions nest countries", {
  local_cache(); fixture_flunet()
  d <- load_panel("flunet")
  expect_equal(row_of(d, "DDD", "2017-01-02")[c("y", "exposure")],
               data.frame(y = 3, exposure = 10), ignore_attr = TRUE)
  expect_equal(levels(d$series_id), c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF"))
  expect_equal(as.character(unique(d$who_region[d$country == "BBB"])), "EUR")
  expect_equal(nrow(d), 11L)
  expect_equal(sort(panel_series("flunet")$series_id), levels(d$series_id))
})

test_that("bbs: a species not recorded on a standard run counts 0", {
  local_cache(); fixture_bbs()
  d <- load_panel("bbs")
  expect_equal(row_of(d, "2001|7360", "2018-06-10")$y, 0)
  expect_equal(row_of(d, "4001|3250", "2019-06-16")$y, 0)
  expect_equal(d$y[d$series_id == "2001|7360"], c(2, 0, NA, 3))
  # four standard runs on route 2001 for three species, two on 4001 for one
  expect_equal(nrow(d), 14L)
})

test_that("bbs: a run that failed the survey's criteria has no count", {
  local_cache(); fixture_bbs()
  d <- load_panel("bbs")
  bad <- row_of(d, "2001|7360", "2019-06-02")
  expect_true(is.na(bad$y))
  expect_equal(bad$exposure, 0)
  expect_equal(sum(d$exposure == 0), 3L)
  d0 <- load_panel("bbs", exposure = FALSE)
  expect_equal(d0, d[setdiff(names(d), "exposure")])
})

test_that("bbs: series are the species recorded on a route's standard runs", {
  local_cache(); fixture_bbs()
  d <- load_panel("bbs")
  # not 2001|9999, seen only on a replicate run; not 2001|3250, never on 2001;
  # 2001|4740, seen only on the run that failed, is a series
  expect_equal(levels(d$series_id), c("2001|4740", "2001|6882", "2001|7360", "4001|3250"))
  expect_false(as.Date("2018-06-11") %in% d$ds)
  expect_equal(sort(panel_series("bbs")$series_id, method = "radix"), levels(d$series_id))
})

test_that("bbs: rows carry the run's date, and routes nest in states and countries", {
  local_cache(); fixture_bbs()
  d <- load_panel("bbs")
  expect_equal(d$ds[d$series_id == "4001|3250"], as.Date(c("2018-06-17", "2019-06-16")))
  expect_equal(levels(d$route), c("2001", "4001"))
  expect_equal(as.character(unique(d$country[d$state == "4"])), "124")
  expect_equal(as.character(unique(d$species[d$series_id == "2001|7360"])), "7360")
  sub <- load_panel("bbs", series = "2001|7360")
  expect_equal(sub$y, d$y[d$series_id == "2001|7360"])
})
