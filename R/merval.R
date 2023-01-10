### Función que devuelve el merval en términos de CCL promedio
### Usa la función ccl que toma el promedio
### Tiene un argumento que indica desde cuando. Lo filtará antes de devolverlo
### puesto que la data se baja de RAVA y baja desde 2005

returnMervalCCL = function(inicio = '2005-01-03', validUntil = 0) {
  require(dplyr)
  source('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/ccl.R')
  ccl <- returnCcl(lookBack = Sys.Date() - as.Date(inicio), validUntil = validUntil)

  merval = returnMerval(inicio = inicio, validUntil = validUntil)
  merval = dplyr::left_join(merval %>% dplyr::filter(fecha >= inicio) %>% dplyr::select(fecha, cierre, volumen), ccl)
  merval = merval %>% dplyr::mutate(mervalCcl = cierre / CCL) #%>% tidyr::drop_na()
}



returnMerval = function(inicio = '2005-01-03', validUntil = 0) {
  require(dplyr)
  require(tidyquant)
  fileName = paste('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/', 'MERVAL', '.csv', sep ='')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    merval = tq_get('m.ba', get = "stock.prices", complete_cases = TRUE, from = inicio, to = TODAY() +1)
    merval = merval %>% select(date, volume, adjusted) %>% distinct(date, .keep_all = TRUE) %>% drop_na(date)
    colnames(merval) = c("fecha", "volumen", "cierre")
    ### corrección 14-7-2022
    merval$cierre[merval['fecha'] == '2022-07-14'] = 100518.41
    write_csv(merval, fileName)
  } else {
    merval = readr::read_csv(fileName)
  }
  merval
}

