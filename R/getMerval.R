#' getMerval devuelve el Merval y otros valores
#'
#' @details
#' Trae merval en pesos, ccl (via api rofex más histórico), USCPI mervalCCL y mervalCCLAjustado
#'
#'
#'
#'  @param FechaInicio Fecha por defecto 2014-05-27
#'  @returns Una tibble de 6 columnas
#'
#'  @examples \dontrun{getMerval()}
#' @export
getMerval = function(fechaInicio = "2014-05-27", ...) {

  require(methodsPPI)
  require(functions)
  require(tidyquant)
  require(tidyverse)
  require(rofex)

  merval = tq_get("^MERV", from = fechaInicio, to = Sys.Date() + 1)
  ### corrijo el valor que yahoo tiene mal para julio
  merval = merval %>%
    mutate(
      adjusted = ifelse(date == "2022-07-14",  100518.41, adjusted)
    )
  # bajo ccl desde API Rofex
  #ccl = rofex::getRofexCCL(from = fechaInicio)
  # tomo CCL desde la tabla
  ccl = dbGetTable(table = "ccl", server = server, port = port) %>% distinct(date, .keep_all = T) %>% arrange(date)
  ccl = ccl %>% select(date, ccl)
  # le pego la infla de US
  ccl = left_join(ccl, functions::getUSCPI(format = "daily", ...) %>% select(-series_id, USCPI = value))
  ccl = ccl %>% fill(USCPI)
  # Calculo el valor ajustado por infla US
  df = left_join(merval %>% select(date, merval = adjusted), ccl) %>%
    mutate(
      mervalCCL = merval / ccl,
      mervalCCLAjustado = (mervalCCL) * (LAST(USCPI) / (USCPI))
    )
}


