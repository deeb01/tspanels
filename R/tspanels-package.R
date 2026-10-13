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
  ".", ".I", "again", "AOU", "cat_id", "COUNTRY_CODE", "CountryNum", "d", "date", "day",
  "Day", "dept_id", "ds", "e", "effort", "epiweek", "family", "i.listed",
  "i.open", "i.y", "id", "INF_ALL", "ISO_WEEKSTARTDATE", "item_id", "keep",
  "listed", "month", "Month", "num_ili", "num_patients", "obs", "open",
  "ORIGIN_SOURCE", "p", "period", "plot", "pos", "region", "Route", "route",
  "RouteDataID", "RPID", "RunType", "sales", "sampled", "series_id", "sid",
  "SPEC_PROCESSED_NB", "species", "SpeciesTotal", "state_id",
  "StateNum", "stn", "store_id", "store_nbr", "Store", "use", "WHOREGION",
  "wk", "wm_yr_wk", "y", "year", "Year"))
