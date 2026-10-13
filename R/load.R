#' Load a panel in the standard long format
#'
#' Returns one row per series and time point:
#'
#' | column | type | meaning |
#' |---|---|---|
#' | `series_id` | factor | the series |
#' | `ds` | Date | the time point |
#' | `y` | numeric | the observed value; `NA` where the source records that no observation was made, or that the one made fails its own criteria |
#' | `exposure` | numeric | present only if the panel records exposure and `exposure = TRUE` |
#' | margins | factor | the key margins and nested attributes named in [panel_info()] |
#'
#' Rows are ordered by `series_id`, then `ds`. Factor levels are sorted in the
#' C locale, so the order does not depend on the machine.
#'
#' **Exposure** measures how much of the value could be observed. Its unit,
#' given by [panels()], is a share of the period on \[0, 1\] (`"fraction"`,
#' or `"binary"` when it is 0 or 1), or a number of units the value is
#' observed among (`"count"`), such as patients seen or specimens tested. A
#' model for the mean takes it as a multiplier, \eqn{E[y_t] = e_t \lambda_t}:
#' for a share, \eqn{\lambda_t} is the value the series would show with full
#' exposure; for a count, it is the value per unit. Where the source records
#' nothing of the kind the column is absent. `exposure = FALSE` drops it, and
#' the files used only to derive it are then not needed. Dropping it does not
#' change `y`: a zero recorded while a store was shut is still a zero, and a
#' model without exposure will see it as one.
#'
#' @param name A panel name from [panels()].
#' @param series Optional character vector of series identifiers to keep;
#'   [panel_series()] lists them. `NULL` loads every series.
#' @param exposure Keep the exposure column, where the panel records one.
#' @param check Validate the result with [check_panel()].
#' @return A data frame.
#' @examples
#' \dontrun{
#' fetch_panel("portal")
#' d <- load_panel("portal")
#' d0 <- load_panel("portal", exposure = FALSE)
#' }
#' @export
load_panel <- function(name, series = NULL, exposure = TRUE, check = TRUE) {
  p <- .spec(name)
  if (!is.logical(exposure) || length(exposure) != 1L || is.na(exposure))
    stop("`exposure` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(series)) series <- unique(as.character(series))
  use <- exposure && !is.na(p$exposure)
  .require_files(name, use)
  d <- p$loader(panel_dir(name), series = series, exposure = use)
  if (!is.null(series)) {
    unknown <- setdiff(series, levels(d$series_id))
    if (length(unknown) == length(series))
      stop("None of `series` is in panel '", name, "'; see panel_series(\"",
           name, "\").", call. = FALSE)
    if (length(unknown))
      warning(length(unknown), " of `series` not in panel '", name, "': ",
              paste(utils::head(unknown, 3L), collapse = ", "),
              if (length(unknown) > 3L) ", ..." else "", call. = FALSE)
  }
  if (check) check_panel(d, name = name)
  d
}

#' The series in a panel, without loading it
#'
#' One row per series, in the order the source lists them, with the key
#' margins and nested attributes. Use it to choose a subset for
#' [load_panel()]'s `series` argument: the full M5 panel is about 47 million
#' rows.
#'
#' @param name A panel name from [panels()].
#' @return A data frame with `series_id` (character) and one factor column per
#'   margin.
#' @export
panel_series <- function(name) {
  p <- .spec(name)
  .require_files(name, FALSE)
  s <- p$index(panel_dir(name))
  s$series_id <- as.character(s$series_id)
  rownames(s) <- NULL
  s
}

# The factor every margin and series_id is built with: levels in C-locale
# order for strings and numeric order for numbers, so the result does not
# depend on the machine. Works on integer codes, because base factor() goes
# through as.character() and the M5 panel has 47 million rows.
.fct <- function(x) {
  if (is.factor(x)) {
    lv <- levels(x)
    used <- sort(unique(as.integer(x)))
    nl <- lv[used]
    o <- nl[order(nl, method = "radix")]
    recode <- rep(NA_integer_, length(lv))
    recode[used] <- match(nl, o)
    return(structure(recode[as.integer(x)], levels = o, class = "factor"))
  }
  u <- unique(x)
  u <- u[!is.na(u)]
  lv <- u[order(u, method = "radix")]
  structure(match(x, lv), levels = as.character(lv), class = "factor")
}

# Assemble the standard frame. `margins` is a named list of equal-length
# vectors; `exposure` is NULL when the panel has none or it was not asked for.
.panel_frame <- function(series_id, ds, y, exposure = NULL, margins = list()) {
  d <- data.table(series_id = .fct(series_id), ds = as.Date(ds), y = as.numeric(y))
  if (!is.null(exposure)) set(d, j = "exposure", value = as.numeric(exposure))
  for (m in names(margins)) set(d, j = m, value = .fct(margins[[m]]))
  setorderv(d, c("series_id", "ds"))
  setDF(d)
  d
}
