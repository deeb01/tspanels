#' Where a panel's raw files are kept
#'
#' The cache root is, in order of precedence, the `TSPANELS_CACHE` environment
#' variable, the `tspanels.cache` option, and
#' `tools::R_user_dir("tspanels", "cache")`. Each panel's files sit in a
#' subdirectory named after it, under the file names the source uses.
#'
#' @param name A panel name, or `NULL` for the cache root.
#' @return A path. It is not created.
#' @examples
#' panel_dir("favorita")
#' @export
panel_dir <- function(name = NULL) {
  root <- Sys.getenv("TSPANELS_CACHE", "")
  if (!nzchar(root))
    root <- getOption("tspanels.cache", tools::R_user_dir("tspanels", "cache"))
  root <- path.expand(root)
  if (is.null(name)) return(root)
  .spec(name)
  file.path(root, name)
}

.sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# Files a call needs: the data files always, the exposure files only when
# exposure is wanted.
.needed <- function(name, exposure) {
  f <- .spec(name)$files
  f[f$role == "data" | (exposure & f$role == "exposure"), , drop = FALSE]
}

.require_files <- function(name, exposure) {
  f <- .needed(name, exposure)
  miss <- f$file[!file.exists(file.path(panel_dir(name), f$file))]
  if (!length(miss)) return(invisible(TRUE))
  how <- if (.spec(name)$access == "fetch")
    sprintf("Run fetch_panel(\"%s\") to download them.", name)
  else sprintf("Download them from %s and run import_panel(\"%s\", <path>).",
               .spec(name)$source, name)
  stop("Panel '", name, "' is missing ", paste(miss, collapse = ", "), " in ",
       panel_dir(name), ". ", how,
       if (!exposure) "" else if (any(f$role[f$file %in% miss] == "exposure"))
         " Or load it with exposure = FALSE, which does not need the exposure files."
       else "",
       call. = FALSE)
}
