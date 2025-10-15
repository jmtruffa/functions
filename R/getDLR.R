#´ getDLR
#'
#' Duplicado con otro nombre para conveniencia
#' Trae el valor de dolar mep, ccl de BYMA usando
#' métodos de PPI
#'
#' @param from Fecha Inicio. Mínima 2015-05-27
#' @param to hasta donde
#'
#' @returns Una tibble con la serie. Anula la devolución del gráfico como
#' segundo componente del valor retornado según viene de getPPIDLR
#'
#' @examples \dontrun{getDLR(from = "2020-09-14", to = Suys.Date(), settle = "t+2")}

getDLR = function (from = "2014-05-27", to = Sys.Date(), settle = "t+0") {

  require(methodsPPI)
  dlr = getPPIDLR(from, to, settle)[[1]]
  dlr = dlr %>% left_join(CCLGGAL(from = from, to = to))
  return(dlr)
}

#' CCLGGAL
#'
#' Devuelve el CCL calculado con GGAL.
#'
#' @param from Fecha desde
#' @param to Fecha hasta
#' @return Tibble con la serie (date, CCLGGAL)
#' @examples
#' \dontrun{
#' CCLGGAL(from = "2020-09-14", to = Sys.Date() + 1) # devuelve el CCL via GGAL
#' }
#' @export
CCLGGAL <- function(from = Sys.Date(), to = Sys.Date() + 1) {
  # NO uses require() dentro de paquetes
  # tidyquant::tq_get, dplyr verbs calificados o importados via roxygen

  ggal <- tidyquant::tq_get(c("ggal.ba","ggal"), from = from, to = to)

  ggal_ba <- ggal[c(1,2,8)] |>
    dplyr::filter(symbol == "ggal.ba")
  ggal_us <- ggal[c(1,2,8)] |>
    dplyr::filter(symbol == "ggal")

  out <- dplyr::left_join(ggal_us, ggal_ba, by = dplyr::join_by(date)) |>
    dplyr::mutate(CCLGGAL = adjusted.y * 10 / adjusted.x) |>
    dplyr::select(date, CCLGGAL) |>
    tidyr::drop_na()

  return(out)
}


  #### Abajo quedó la anterior que usaba alphacast y que traía también solidario, oficial y blue.
  ### Función que descarga dolar Blue, oficial mayorista, oficial minorista, solidario desde Alphacast
  ###
  # require(tidyverse)
  # require(utils)
  # source('/Users/juan/Google Drive/Mi unidad/analisis financieros/functions/alphacast.R')
  # fileDirectory = paste0('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/')
  # fileName = paste0(fileDirectory, 'dlr.csv')
  # if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
  #   dlr = getAlphacast(5288, validUntil = validUntil)
  #   dlr = dlr %>% select(Date, BLUE, Dolar.Mayorista, Dolar.Oficial, Dolar.Solidario)
  #   dlr = dlr %>% mutate(fecha = as.Date(Date)) %>% select(-Date) %>% rename(dlrMayorista = Dolar.Mayorista,
  #                                                                            dlrOficial = Dolar.Oficial,
  #                                                                            dlrSolidario = Dolar.Solidario,
  #                                                                            blue = BLUE) %>% relocate(fecha)
  #   readr::write_csv(dlr, fileName)
  # } else {
  #   dlr = readr::read_csv(fileName)
  # }
  # dlr


