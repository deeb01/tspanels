#' tspanels: open panels of related time series, with margins and exposure
#'
#' A panel here is a collection of time series indexed by categorical margins:
#' sales by store and product family, captures by plot and species. tspanels
#' loads several open panels into one long format, with the margins as named
#' columns and, where the source records it, an exposure column.
#'
#' Start with [panels()] for what is registered, [fetch_panel()] or
#' [import_panel()] to obtain the raw files, and [load_panel()] to read one.
#' [check_panel()] validates any frame against the format.
#'
#' @keywords internal
#' @import data.table
"_PACKAGE"

# data.table column names used in non-standard evaluation.
utils::globalVariables(c(
  ".", ".I", "cat_id", "d", "date", "day", "dept_id", "ds", "e", "effort",
  "family", "i.listed", "i.open", "i.y", "id", "item_id", "keep", "listed",
  "month", "open", "period", "plot", "sales", "sampled", "series_id", "sid",
  "species", "state_id", "stn", "store_id", "store_nbr", "Store", "wk",
  "wm_yr_wk", "y", "year"))
