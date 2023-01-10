### Función que descarga dolar Blue, oficial mayorista, oficial minorista, solidario desde Alphacast
###

getDLR = function (validUntil = 0) {
  require(tidyverse)
  require(utils)
  source('/Users/juan/Google Drive/Mi unidad/analisis financieros/functions/alphacast.R')
  fileDirectory = paste0('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/')
  fileName = paste0(fileDirectory, 'dlr.csv')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    dlr = getAlphacast(5288, validUntil = validUntil)
    dlr = dlr %>% select(Date, BLUE, Dolar.Mayorista, Dolar.Oficial, Dolar.Solidario)
    dlr = dlr %>% mutate(fecha = as.Date(Date)) %>% select(-Date) %>% rename(dlrMayorista = Dolar.Mayorista,
                                                                             dlrOficial = Dolar.Oficial,
                                                                             dlrSolidario = Dolar.Solidario,
                                                                             blue = BLUE) %>% relocate(fecha)
    readr::write_csv(dlr, fileName)
  } else {
    dlr = readr::read_csv(fileName)
  }
  dlr
}

