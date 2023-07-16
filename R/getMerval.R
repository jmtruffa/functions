#' getMerval devuelve el Merval y otros valores
#'
#' @details
#' Trae merval en pesos, mep via AL y GD, CCL via GD
#' tomando LAST T+0, Canje, USCPI (tener en cuenta que toma el valor
#' que haya en la DB. Actualizar previamente), merval al CCL y
#' Merval Ajustado por infla USA
#'
#' Utiliza functiones de methodsPPI para tomar precio de CCL,
#' tidyquant (la usa para tomar el valor histórico de merval desde yahoo).
#'
#'  @param FechaInicio Fecha por defecto 2014-05-27
#'  @returns Una tibble de 9 columnas
#'
#'  @examples getMerval() -> Devuelve la tibble
#'
getMerval = function(fechaInicio = "2014-05-27", settle = "t+2") {

  require(methodsPPI)
  require(functions)
  require(tidyquant)


  #ccl = methodsPPI::getPPIDLR(from = fechaInicio)[[1]]
  ccl = functions::getDLR(from = fechaInicio, settle = settle)
  ### acá utilizo la función functions::getUSCPI pidiendole los datos daily para luego poder hacer el ajuste del CCL.
  ccl = left_join(ccl, functions::getUSCPI(format = "daily") %>% select(-series_id, USCPI = value))
  ccl = ccl %>% fill(USCPI)
  merval = tq_get("m.ba", get = "stock.prices", from = fechaInicio, Sys.Date() + 1)
  ### corrijo el valor que yahoo tiene mal para julio
  merval = merval %>%
    mutate(
      adjusted = ifelse(date == "2022-07-14",  100518.41, adjusted)
    )

  df = left_join(merval %>% select(date, merval = adjusted), ccl) %>%
    mutate(
      mervalCCL = merval / cclGD,
      mervalCCLAjustado = (mervalCCL) * (LAST(USCPI) / (USCPI))
    )
}




