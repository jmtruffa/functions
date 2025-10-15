#' @keywords internal
#' @importFrom utils globalVariables
NULL

utils::globalVariables(c(
  # dplyr/tidyr NSE:
  ".", "symbol", "symbol2", "symbol_local", "adjusted", "adjusted.x", "adjusted.y",
  "periodReturn", "ret", "volume", "volume.x", "volume.y", "openingPrice",
  "letras", "precios", "initialFee", "endingFee", "ticker", "vto", "dias",
  "yield", "mduration", "text", "fecha", "CCL", "mervalCCL", "series_id",
  "value", "name", "Year", "Month", "Núcleo", "PBICorriente", "PBIConstante",
  "current_friday", "prev_friday_date", "year_month", "year_week",
  "footnote_codes", "tipoSerie", "close.x", "close.y", "ratio", "ratio.x",
  "USCPI", "holidaysANBIMA", "server", "port"
))
