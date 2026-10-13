good <- function() data.frame(
  series_id = rep(c("a", "b"), each = 3),
  ds = rep(as.Date("2024-01-01") + 0:2, 2),
  y = c(1, 0, 2, 5, 4, 0),
  exposure = c(1, 0, 1, 1, 1, 0.5),
  store = rep(c("s1", "s2"), each = 3),
  city = rep(c("c1", "c1"), each = 3),
  stringsAsFactors = FALSE)

chk <- function(d, ...) check_panel(d, key = "store", nesting = c(city = "store"),
                                   exposure_unit = "fraction", ...)

test_that("a valid frame passes, with or without exposure", {
  expect_invisible(chk(good()))
  expect_silent(chk(good()[names(good()) != "exposure"]))
  d <- good(); d$y[2] <- NA
  expect_silent(chk(d))
})

# Each check is shown to fail on a frame built to violate it.
test_that("each violation is caught", {
  d <- good(); d$ds <- as.character(d$ds)
  expect_error(chk(d), "`ds` is not a Date")
  d <- good(); d$ds[2] <- d$ds[1]
  expect_error(chk(d), "repeat a \\(series_id, ds\\) pair")
  d <- good(); d$exposure[1] <- -0.1
  expect_error(chk(d), "negative")
  d <- good(); d$exposure[1] <- 1.2
  expect_error(chk(d), "exceed 1")
  d <- good(); d$exposure[1] <- NA
  expect_error(chk(d), "missing or non-finite")
  d <- good(); d$y[2] <- 3
  expect_error(chk(d), "positive y where exposure is 0")
  d <- good(); d$store[3] <- "s9"
  expect_error(chk(d), "more than one value of the key margins")
  d <- good(); d$store[4:6] <- "s1"
  expect_error(chk(d), "share a combination of key margins")
  d <- good(); d$city[1] <- "c2"
  expect_error(chk(d), "`city` is not constant within `store`")
  expect_error(chk(good()[names(good()) != "city"]), "Missing column")
})

test_that("binary exposure must be 0 or 1", {
  d <- good()
  expect_error(check_panel(d, exposure_unit = "binary"), "neither 0 nor 1")
  d$exposure[6] <- 1
  expect_silent(check_panel(d, exposure_unit = "binary"))
})

test_that("violations are reported together", {
  d <- good(); d$exposure[1] <- -1; d$city[1] <- "c2"
  expect_error(chk(d), "fails 2 check")
})

test_that("the old favorita exposure rule fails the check", {
  local_cache(); fixture_favorita()
  d <- load_panel("favorita")
  # transactions rows only: store 2 shut on day 4, although bread sold
  d$exposure[d$store_nbr == "2" & d$ds == as.Date("2015-03-04")] <- 0
  expect_error(check_panel(d, "favorita"), "positive y where exposure is 0")
})
