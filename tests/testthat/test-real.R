# On the real files, when asked: TSPANELS_TEST_REAL=true. The M5 panel needs
# about 5 GB and half a minute.
test_that("each available panel matches its registry entry and checksums", {
  skip_if_not(identical(Sys.getenv("TSPANELS_TEST_REAL"), "true"),
              "set TSPANELS_TEST_REAL=true to run on the real files")
  ran <- 0L
  for (n in panels()$name) {
    if (!tspanels:::.available(n)) next
    p <- panel_info(n)
    expect_true(tspanels:::.verify(n, strict = TRUE), info = n)
    d <- load_panel(n)
    expect_equal(nlevels(d$series_id), p$n_series, info = n)
    expect_equal(min(d$ds), as.Date(p$start), info = n)
    expect_equal(max(d$ds), as.Date(p$end), info = n)
    expect_equal(sort(panel_series(n)$series_id, method = "radix"), levels(d$series_id), info = n)
    ran <- ran + 1L
  }
  expect_gt(ran, 0L)
})
