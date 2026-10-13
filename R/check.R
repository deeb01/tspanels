#' Check a frame against the panel format
#'
#' Collects every violation and reports them together. With `name`, the key
#' margins, nested attributes and exposure unit come from the registry;
#' without it, pass them to check any frame of your own.
#'
#' The checks:
#'
#' 1. `series_id`, `ds` and `y` are present; `ds` is a Date and `y` numeric.
#' 2. No series has two rows at the same `ds`.
#' 3. If `exposure` is present: numeric, finite, non-negative; at most 1 for a
#'    `"fraction"` unit and 0 or 1 for `"binary"`, with no bound for a
#'    `"count"`; and no row has a positive `y` where exposure is 0, which a
#'    multiplicative model cannot fit.
#' 4. Each key margin is constant within a series, and no two series share a
#'    combination of key margins.
#' 5. Each nested attribute is constant within the column it is nested in.
#'
#' @param d A data frame.
#' @param name Optional panel name from [panels()].
#' @param key Key margin column names.
#' @param nesting Named character vector: attribute = the column it is
#'   constant within.
#' @param exposure_unit `"binary"`, `"fraction"`, `"count"` or `NULL`. A count
#'   and `NULL` are checked as non-negative only.
#' @return `d`, invisibly, if every check passes; otherwise an error listing
#'   each violation.
#' @examples
#' d <- data.frame(series_id = rep(c("a", "b"), each = 3),
#'                 ds = rep(as.Date("2024-01-01") + 0:2, 2),
#'                 y = c(1, 0, 2, 5, 4, 0),
#'                 exposure = c(1, 0, 1, 1, 1, 0.5),
#'                 store = rep(c("s1", "s2"), each = 3))
#' check_panel(d, key = "store", exposure_unit = "fraction")
#' @export
check_panel <- function(d, name = NULL, key = NULL, nesting = NULL,
                        exposure_unit = NULL) {
  if (!is.null(name)) {
    p <- .spec(name)
    if (is.null(key)) key <- p$key
    if (is.null(nesting)) nesting <- p$nesting
    if (is.null(exposure_unit) && !is.na(p$exposure)) exposure_unit <- p$exposure_unit
  }
  if (!is.data.frame(d)) stop("`d` must be a data frame.", call. = FALSE)
  if (!is.null(exposure_unit) &&
      !(length(exposure_unit) == 1L && exposure_unit %in% c("binary", "fraction", "count")))
    stop('`exposure_unit` must be "binary", "fraction", "count" or NULL.', call. = FALSE)
  bad <- character(0)
  add <- function(...) bad <<- c(bad, paste0(...))

  miss <- setdiff(c("series_id", "ds", "y", key, names(nesting), unname(nesting)),
                  names(d))
  if (length(miss))
    stop("Missing column(s): ", paste(unique(miss), collapse = ", "), ".",
         call. = FALSE)
  if (!inherits(d$ds, "Date")) add("`ds` is not a Date.")
  if (!is.numeric(d$y)) add("`y` is not numeric.")

  dt <- as.data.table(d)
  nd <- sum(duplicated(dt, by = c("series_id", "ds")))
  if (nd) add(nd, " row(s) repeat a (series_id, ds) pair.")

  if ("exposure" %in% names(d)) {
    e <- d$exposure
    if (!is.numeric(e)) {
      add("`exposure` is not numeric.")
    } else {
      if (any(!is.finite(e))) add(sum(!is.finite(e)), " exposure value(s) are missing or non-finite.")
      ef <- e[is.finite(e)]
      if (any(ef < 0)) add(sum(ef < 0), " exposure value(s) are negative.")
      if (identical(exposure_unit, "fraction") && any(ef > 1))
        add(sum(ef > 1), " exposure value(s) exceed 1 for a fraction.")
      if (identical(exposure_unit, "binary") && any(!ef %in% c(0, 1)))
        add(sum(!ef %in% c(0, 1)), " exposure value(s) are neither 0 nor 1.")
      if (is.numeric(d$y)) {
        z <- is.finite(e) & e == 0 & !is.na(d$y) & d$y > 0
        if (any(z))
          add(sum(z), " row(s) have positive y where exposure is 0, ",
              "which a multiplicative model cannot fit.")
      }
    }
  }

  if (length(key)) {
    u <- unique(dt[, c("series_id", key), with = FALSE])
    nv <- sum(duplicated(u, by = "series_id"))
    if (nv) add(nv, " series have more than one value of the key margins ",
                paste(key, collapse = ", "), ".")
    ns <- sum(duplicated(u, by = key))
    if (ns) add(ns, " series share a combination of key margins with another series.")
  }
  for (a in names(nesting)) {
    par <- nesting[[a]]
    u <- unique(dt[, c(par, a), with = FALSE])
    nv <- sum(duplicated(u, by = par))
    if (nv) add("`", a, "` is not constant within `", par, "` (", nv,
                " value(s) of `", par, "` have more than one).")
  }

  if (length(bad))
    stop("The panel fails ", length(bad), " check(s):\n",
         paste0("  - ", bad, collapse = "\n"), call. = FALSE)
  invisible(d)
}
