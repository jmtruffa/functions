### Función que descarga la inflación argentina
###

getInflaArgentina = function (validUntil = 0) {
  require(tidyverse)
  #source('/Users/juan/Google Drive/Mi unidad/analisis financieros/functions/alphacast.R')
  fileDirectory = paste0('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/')
  fileName = paste0(fileDirectory, 'infla.csv')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    infla = getAlphacast(5515)
    infla = infla %>% select(1, 3:20) %>% mutate(fecha = as.Date(Date)) %>% select(-Date) %>% rename(Nucleo=`Núcleo`) %>% relocate(fecha)
    readr::write_csv(infla, fileName)
  } else {
    dlr = readr::read_csv(fileName)
  }
}
