# The registry: what is known about each panel before it is loaded.
#
# Each entry is a list with
#   title, domain, freq      what the panel is
#   key                      the margins whose combinations are the series
#   nesting                  named character: attribute = the margin it is
#                            constant within (city = "store_nbr")
#   exposure                 how exposure is derived, or NA if none is recorded
#   exposure_unit            "binary", "fraction" or NA
#   n_series, start, end     the full panel, as load_panel() returns it
#   notes                    properties of the source a user should know
#   access                   "fetch" (fetch_panel downloads it) or "import"
#                            (the user downloads it; import_panel installs it)
#   source, licence          where it comes from and on what terms
#   files                    data.frame(file, role, sha256, url); role is
#                            "data" or "exposure" (needed only for exposure)
#   loader                   function(dir, series, exposure) -> panel frame
#   index                    function(dir) -> one row per series, source order

.PANELS <- new.env(parent = emptyenv())

.register <- function(name, spec) {
  assign(name, spec, envir = .PANELS)
  invisible(name)
}

.spec <- function(name) {
  if (!is.character(name) || length(name) != 1L || !exists(name, envir = .PANELS,
                                                            inherits = FALSE))
    stop("Unknown panel '", paste(name, collapse = ", "), "'. Registered: ",
         paste(ls(.PANELS), collapse = ", "), ".", call. = FALSE)
  get(name, envir = .PANELS, inherits = FALSE)
}

#' The registered panels
#'
#' One row per panel. `key` names the margins whose combinations are the
#' series; `attributes` names the columns nested in them (each constant within
#' a key margin or within another attribute; [panel_info()] says which).
#' `exposure` is the unit of the exposure column, or `NA` where the source
#' records none. `available` says whether the files [load_panel()] needs with
#' its default arguments are in the cache.
#'
#' @return A data frame.
#' @examples
#' panels()
#' @export
panels <- function() {
  nm <- ls(.PANELS)
  rows <- lapply(nm, function(n) {
    p <- .spec(n)
    data.frame(
      name = n, domain = p$domain, freq = p$freq, n_series = p$n_series,
      start = as.Date(p$start), end = as.Date(p$end),
      key = paste(p$key, collapse = " x "),
      attributes = paste(names(p$nesting), collapse = ", "),
      exposure = if (is.na(p$exposure)) NA_character_ else p$exposure_unit,
      access = p$access, licence = p$licence,
      available = .available(n),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Everything recorded about one panel
#'
#' @param name A panel name from [panels()].
#' @return A list: `title`, `domain`, `freq`, `key`, `nesting` (a named
#'   character vector, attribute = the column it is constant within),
#'   `exposure` (how it is derived, or `NA`), `exposure_unit`, `n_series`,
#'   `start`, `end`, `notes`, `access`, `source`, `licence`, and `files` (a
#'   data frame of file, role, sha256 and url).
#' @examples
#' panel_info("portal")$files
#' @export
panel_info <- function(name) {
  p <- .spec(name)
  p[!vapply(p, is.function, logical(1))]
}

.available <- function(name) {
  f <- .spec(name)$files
  all(file.exists(file.path(panel_dir(name), f$file[f$role == "data"])))
}
