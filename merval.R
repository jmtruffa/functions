### Función que devuelve el merval en términos de CCL promedio
### Usa la función ccl que toma el promedio
### Tiene un argumento que indica desde cuando. Lo filtará antes de devolverlo
### puesto que la data se baja de RAVA y baja desde 2005

returnMervalCCL = function(inicio = '2005-01-03', validUntil = 0) {
  require(dplyr)
  source('/Volumes/GoogleDrive/Mi unidad/analisis financieros/ccl/ccl/ccl.R')
  ccl <- returnCcl(graba = FALSE, lookBack = Sys.Date() - as.Date(inicio))
  #ccl <- tidyr::drop_na(ccl)
  merval = returnMerval(inicio = inicio, validUntil = validUntil)
  # download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=','MERVAL','&csv=1', sep=''), 
  #               paste('/Volumes/GoogleDrive/Mi unidad/analisis financieros/functions/data/', 'MERVAL', '.csv', sep =''), mode = 'wb')
  # merval = readr::read_csv('/Volumes/GoogleDrive/Mi unidad/analisis financieros/functions/data/MERVAL.CSV')
  # ## filtramos el dato erroneo
  # merval = merval %>% filter(fecha <= Sys.Date())
  merval = dplyr::left_join(merval %>% dplyr::filter(fecha >= inicio) %>% dplyr::select(fecha, cierre, volumen), ccl)
  merval = merval %>% dplyr::mutate(mervalCcl = cierre / CCL) %>% tidyr::drop_na()
}

# returnMerval = function(inicio = '2005-01-03', validUntil = 0) {
#   require(dplyr)
#   fileName = paste('/Volumes/GoogleDrive/Mi unidad/analisis financieros/functions/data/', 'MERVAL', '.csv', sep ='')
#   if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
#     download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=','MERVAL','&csv=1', sep=''), 
#                 fileName, mode = 'wb')
#   }
#   merval = readr::read_csv(fileName)
#   ## filtramos el dato erroneo
#   merval = merval %>% filter(fecha <= Sys.Date())
#   merval = merval %>% dplyr::filter(fecha >= inicio) %>% dplyr::select(fecha, cierre, volumen) %>% tidyr::drop_na()
# }


returnMerval = function(inicio = '2005-01-03', validUntil = 0) {
  require(dplyr)
  require(tidyquant)
  fileName = paste('/Volumes/GoogleDrive/Mi unidad/analisis financieros/functions/data/', 'MERVAL', '.csv', sep ='')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    merval = tq_get('m.ba', get = "stock.prices", complete_cases = TRUE, from = inicio, to = TODAY())
    merval = merval %>% select(date, volume, adjusted) %>% distinct(date, .keep_all = TRUE) %>% drop_na(date)
    colnames(merval) = c("fecha", "volumen", "cierre")
    write_csv(merval, fileName)
  } else {
    merval = readr::read_csv(fileName)
  }
  merval
}

