#' Download a panel's raw files from their source
#'
#' For panels whose licence permits automated download (`access == "fetch"` in
#' [panels()]), downloads each file from a pinned URL into [panel_dir()],
#' checks it against its recorded SHA-256 checksum, and only then moves it into
#' place. A file already present with the right checksum is not downloaded
#' again.
#'
#' What a mismatch means depends on the source. A file from a fixed release
#' (`snapshot` is `NA` in [panel_info()]) cannot change, so a mismatch is an
#' error and leaves nothing behind. A file served by a live API is a snapshot:
#' the query is pinned, but the source may revise what it returns, so a
#' mismatch is a warning that names the file, and the file is kept.
#' `panel_info(name)$pin` says what is pinned.
#'
#' Panels with `access == "import"` cannot be downloaded by tspanels: their
#' terms require each user to obtain them from the source. For those, this
#' function checks the files already in the cache and, if any are missing,
#' stops with the source URL and the file names to download; [import_panel()]
#' then installs them.
#'
#' @param name A panel name from [panels()].
#' @param quiet Suppress download progress.
#' @return The panel's directory, invisibly.
#' @export
fetch_panel <- function(name, quiet = FALSE) {
  p <- .spec(name)
  if (p$access != "fetch") {
    f <- p$files
    have <- file.exists(file.path(panel_dir(name), f$file))
    if (!all(have))
      stop("Panel '", name, "' cannot be downloaded by tspanels; its terms ",
           "require you to obtain it from the source. Download ",
           paste(f$file[!have], collapse = ", "), " from ", p$source,
           ", then run import_panel(\"", name, "\", <zip file or folder>).",
           call. = FALSE)
    .verify(name, strict = FALSE)
    return(invisible(panel_dir(name)))
  }
  .fetch_files(p$files, panel_dir(name), quiet = quiet, snapshot = p$snapshot)
  invisible(panel_dir(name))
}

# `snapshot`: NA for fixed files, where a mismatch is an error; otherwise the
# date the recorded checksums were taken, and a mismatch is a warning.
.fetch_files <- function(files, dir, quiet = FALSE, snapshot = NA_character_) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  for (i in seq_len(nrow(files))) {
    dest <- file.path(dir, files$file[i])
    if (file.exists(dest) && identical(.sha256(dest), files$sha256[i])) next
    part <- paste0(dest, ".part")
    on.exit(unlink(part), add = TRUE)
    status <- utils::download.file(files$url[i], part, mode = "wb", quiet = quiet)
    if (status != 0L) stop("Download of ", files$url[i], " failed.", call. = FALSE)
    got <- .sha256(part)
    if (!identical(got, files$sha256[i])) {
      if (is.na(snapshot)) {
        unlink(part)
        stop("Checksum mismatch for ", files$file[i], " from ", files$url[i],
             ": expected ", files$sha256[i], ", got ", got, ". Nothing was kept.",
             call. = FALSE)
      }
      warning(files$file[i], " differs from the snapshot of ", snapshot,
              " that tspanels was tested on: the source has changed what it ",
              "returns for the pinned query. Kept; results can differ from the ",
              "documented ones.", call. = FALSE)
    }
    if (!file.rename(part, dest)) stop("Could not move ", part, " to ", dest, call. = FALSE)
  }
  invisible(dir)
}

#' Install raw files you downloaded yourself
#'
#' For panels with `access == "import"`, whose terms require each user to
#' download them from the source. `path` is the zip archive the source serves
#' or a folder holding the files; the files the panel needs are copied (or
#' extracted) into [panel_dir()] and checked against the checksums of the copy
#' tspanels was tested on. A mismatch is a warning rather than an error,
#' because a source may re-issue a file with immaterial changes; the warning
#' names the file so a changed loader result can be traced to it.
#'
#' @param name A panel name from [panels()].
#' @param path A `.zip` file or a directory.
#' @return The panel's directory, invisibly.
#' @export
import_panel <- function(name, path) {
  p <- .spec(name)
  path <- path.expand(path)
  dir <- panel_dir(name)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  want <- p$files$file
  if (dir.exists(path)) {
    src <- file.path(path, want)
    ok <- file.exists(src)
    if (any(ok) && !all(file.copy(src[ok], dir, overwrite = TRUE)))
      stop("Could not copy files into ", dir, call. = FALSE)
    miss <- want[!ok]
  } else if (file.exists(path) && grepl("\\.zip$", path, ignore.case = TRUE)) {
    inside <- utils::unzip(path, list = TRUE)$Name
    hit <- inside[basename(inside) %in% want]
    if (length(hit)) utils::unzip(path, files = hit, exdir = dir, junkpaths = TRUE)
    miss <- setdiff(want, basename(hit))
  } else {
    stop("`path` must be a directory or a .zip file: ", path, call. = FALSE)
  }
  if (length(miss))
    warning("Not found in ", path, ": ", paste(miss, collapse = ", "), ".",
            call. = FALSE)
  .verify(name, strict = FALSE)
  invisible(dir)
}

# Compare present files with their recorded checksums. strict = TRUE is an
# error on mismatch, FALSE a warning.
.verify <- function(name, strict) {
  f <- .spec(name)$files
  path <- file.path(panel_dir(name), f$file)
  have <- file.exists(path)
  bad <- character(0)
  for (i in which(have)) if (!identical(.sha256(path[i]), f$sha256[i])) bad <- c(bad, f$file[i])
  if (length(bad)) {
    msg <- paste0("Panel '", name, "': ", paste(bad, collapse = ", "),
                  " differ from the copies tspanels was tested on. The loader ",
                  "may still work; results can differ from the documented ones.")
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  invisible(length(bad) == 0L)
}
