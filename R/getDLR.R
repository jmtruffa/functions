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
#' @examples getDLR(from = "2020-09-14", to = Suys.Date(), settle = "t+2") -> Devuelve la tibble
#'

getDLR = function (from = "2014-05-27", to = Sys.Date(), settle = "t+0") {

  require(methodsPPI)
  dlr = getPPIDLR(from, to, settle)[[1]]
  dlr = dlr %>% left_join(CCLGGAL(from = from, to = to))
  return(dlr)
}

#'
#' CCLGGAL
#'
#' Devuelve el CCL calculado con GGAL.
#'
#' @param from Fecha desde
#' @param to Fecha hasta
#' @return Devuelve un tibble con la serie en cuestión
#' @examples CCLGGAL(from = "2020-09-14", to = sys.Date() + 1) devuelve el CCL via GGAL
CCLGGAL = function(from = Sys.Date(), to = Sys.Date() + 1) {
    require(tidyquant)
    require(tidyverse)

    ggal = tq_get(c("ggal.ba","ggal"),
                  from = from,
                  to = to
    )

    ggal.ba = ggal[c(1,2,8)] %>% filter(symbol == "ggal.ba")
    ggal =  ggal[c(1,2,8)] %>% filter(symbol == "ggal")

    CCLGGAL = left_join(ggal, ggal.ba, by = join_by(date)) %>%
      mutate(
        CCLGGAL = adjusted.y * 10 / adjusted.x
      ) %>%
      select(date, CCLGGAL) %>%
      drop_na()
    return(CCLGGAL)
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


