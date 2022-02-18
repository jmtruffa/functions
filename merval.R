### Función que devuelve el merval en términos de CCL promedio
### Usa la función ccl que toma el promedio
### Tiene un argumento que indica desde cuando. Lo filtará antes de devolverlo
### puesto que la data se baja de RAVA y baja desde 2005

returnMervalCCL = function(inicio = '2005-01-03') {
  source('/Volumes/GoogleDrive/Mi unidad/analisis financieros/ccl/ccl/ccl.R')
  ccl <- returnCcl(graba = FALSE, lookBack = Sys.Date() - as.Date(inicio))
  #ccl <- tidyr::drop_na(ccl)
  download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=','MERVAL','&csv=1', sep=''), paste('/Volumes/GoogleDrive/Mi unidad/analisis financieros/merval/', 'MERVAL', '.csv', sep =''), mode = 'wb')
  merval = readr::read_csv('/Volumes/GoogleDrive/Mi unidad/analisis financieros/merval/MERVAL.CSV')
  ## filtramos el dato erroneo
  merval = merval %>% filter(fecha <= Sys.Date())
  merval = dplyr::left_join(merval %>% dplyr::filter(fecha >= inicio) %>% dplyr::select(fecha, cierre, volumen), ccl)
  merval = merval %>% dplyr::mutate(mervalCcl = cierre / CCL) %>% tidyr::drop_na()
}

returnMerval = function(inicio = '2005-01-03') {
  download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=','MERVAL','&csv=1', sep=''), paste('/Volumes/GoogleDrive/Mi unidad/analisis financieros/merval/', 'MERVAL', '.csv', sep =''), mode = 'wb')
  merval = readr::read_csv('/Volumes/GoogleDrive/Mi unidad/analisis financieros/merval/MERVAL.CSV')
  ## filtramos el dato erroneo
  merval = merval %>% filter(fecha <= Sys.Date())
  merval = merval %>% dplyr::filter(fecha >= inicio) %>% dplyr::select(fecha, cierre, volumen) %>% tidyr::drop_na()
}



