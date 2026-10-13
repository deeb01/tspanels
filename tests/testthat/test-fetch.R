src_file <- function(text) {
  f <- tempfile(fileext = ".csv")
  writeLines(text, f)
  f
}

files_for <- function(path, sha) data.frame(
  file = basename(path), role = "data", sha256 = sha,
  url = paste0("file://", normalizePath(path)), stringsAsFactors = FALSE)

test_that("a download is kept only if its checksum matches", {
  root <- local_cache()
  f <- src_file(c("a,b", "1,2"))
  dest <- file.path(root, "x")
  tspanels:::.fetch_files(files_for(f, tspanels:::.sha256(f)), dest, quiet = TRUE)
  expect_true(file.exists(file.path(dest, basename(f))))

  g <- src_file(c("a,b", "1,3"))
  expect_error(tspanels:::.fetch_files(files_for(g, strrep("0", 64)), dest, quiet = TRUE),
               "Checksum mismatch")
  expect_false(file.exists(file.path(dest, basename(g))))
  expect_length(list.files(dest, pattern = "\\.part$"), 0L)
})

test_that("a file already present with the right checksum is not downloaded again", {
  root <- local_cache()
  f <- src_file("z")
  ff <- files_for(f, tspanels:::.sha256(f))
  dest <- file.path(root, "x")
  tspanels:::.fetch_files(ff, dest, quiet = TRUE)
  ff$url <- "file:///nonexistent/never"
  expect_silent(tspanels:::.fetch_files(ff, dest, quiet = TRUE))
})

test_that("every fetchable panel pins a url and a checksum", {
  for (n in panels()$name[panels()$access == "fetch"]) {
    f <- panel_info(n)$files
    expect_true(all(grepl("^https://", f$url)), info = n)
    expect_true(all(grepl("^[0-9a-f]{64}$", f$sha256)), info = n)
  }
})

test_that("an import panel cannot be fetched and says where to get it", {
  local_cache()
  expect_error(fetch_panel("rossmann"), "kaggle.com/competitions/rossmann-store-sales")
  expect_error(fetch_panel("rossmann"), "import_panel")
})

test_that("import_panel takes a zip or a folder and warns on a changed file", {
  local_cache(); fixture_rossmann()
  made <- panel_dir("rossmann")
  stage <- tempfile("dl-"); dir.create(stage)
  file.copy(file.path(made, c("train.csv", "store.csv")), stage)
  writeLines("x", file.path(stage, "test.csv"))
  unlink(made, recursive = TRUE)

  # the fixtures are not the tested copies, so the checksum warning fires
  expect_warning(import_panel("rossmann", stage), "differ from the copies")
  expect_true(all(file.exists(file.path(made, c("train.csv", "store.csv")))))
  expect_false(file.exists(file.path(made, "test.csv")))

  unlink(made, recursive = TRUE)
  zip <- tempfile(fileext = ".zip")
  old <- setwd(stage); on.exit(setwd(old), add = TRUE)
  utils::zip(zip, c("train.csv", "store.csv", "test.csv"), flags = "-q")
  setwd(old)
  expect_warning(import_panel("rossmann", zip), "differ from the copies")
  expect_equal(nrow(load_panel("rossmann")), 6L)

  # a folder without the files: named as missing, and the cached copies kept
  expect_warning(expect_warning(import_panel("rossmann", tempdir()), "Not found"),
                 "differ from the copies")
  expect_error(import_panel("rossmann", tempfile()), "directory or a .zip")
})

test_that("panel_dir follows TSPANELS_CACHE", {
  root <- local_cache()
  expect_equal(panel_dir(), root)
  expect_equal(panel_dir("portal"), file.path(root, "portal"))
  expect_error(panel_dir("nope"), "Unknown panel")
})

test_that("a file from a live API that has changed is kept, with a warning", {
  root <- local_cache()
  g <- src_file(c("a,b", "1,3"))
  dest <- file.path(root, "x")
  expect_warning(
    tspanels:::.fetch_files(files_for(g, strrep("0", 64)), dest, quiet = TRUE,
                            snapshot = "2026-10-12"),
    "differs from the snapshot of 2026-10-12")
  expect_true(file.exists(file.path(dest, basename(g))))
  expect_length(list.files(dest, pattern = "\\.part$"), 0L)
})

test_that("only the panels served by a live API are snapshots", {
  live <- vapply(panels()$name, function(n) !is.na(panel_info(n)$snapshot), logical(1))
  expect_equal(names(live)[live], c("flunet", "ilinet"))
  for (n in panels()$name) expect_true(nzchar(panel_info(n)$pin), info = n)
})

test_that("fetch_panel keeps a changed snapshot and refuses a changed fixed file", {
  root <- local_cache()
  g <- src_file(c("a,b", "1,3"))
  toy <- function(snapshot) list(access = "fetch", snapshot = snapshot,
                                 files = files_for(g, strrep("0", 64)))
  on.exit(rm("toylive", envir = tspanels:::.PANELS), add = TRUE)
  tspanels:::.register("toylive", toy(NA_character_))
  expect_error(fetch_panel("toylive", quiet = TRUE), "Checksum mismatch")
  tspanels:::.register("toylive", toy("2026-10-12"))
  expect_warning(fetch_panel("toylive", quiet = TRUE), "differs from the snapshot")
  expect_true(file.exists(file.path(panel_dir("toylive"), basename(g))))
})
