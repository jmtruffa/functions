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
#' @examples getDLR("tabla-a-buscar") -> Devuelve la tibble
#'

getDLR = function (from = "2014-05-27", to = Sys.Date()) {

  dlr = getPPIDLR(from, to)[[1]]


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
}

