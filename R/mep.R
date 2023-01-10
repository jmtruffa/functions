returnMep = function (validUntil = 0) {
  require(tidyverse)
  #setwd('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/mep/')
  bonos_mep <- c("AL30", "AL30D", "GD30", "GD30D", "AL30C", "GD30C")
  
  fileDirectory = paste0('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/')
  fileName = paste0(fileDirectory, 'mep.csv')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    for (i in seq_along(bonos_mep)){
      download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=',bonos_mep[i],'&csv=1', sep=''), paste0(fileDirectory, bonos_mep[i], '.csv'), mode = 'wb')
      
    }
  }
  
  al30 <- read_csv(paste0(fileDirectory, 'AL30.csv'))
  al30d <- read_csv(paste0(fileDirectory, 'AL30D.csv'))
  al30c <- read_csv(paste0(fileDirectory, 'AL30C.csv'))
  gd30 <- read_csv(paste0(fileDirectory, 'GD30.csv'))
  gd30d <- read_csv(paste0(fileDirectory, 'GD30D.csv'))
  gd30c = read_csv(paste0(fileDirectory, 'GD30C.csv'))
  
  
  
    #arreglo del 4-11
    al30$cierre[al30['fecha'] == '2021-11-04'] = al30$cierre[al30['fecha'] == '2021-11-04'] * 1000
    gd30$cierre[gd30['fecha'] == '2021-11-04'] = gd30$cierre[gd30['fecha'] == '2021-11-04'] * 1000


  al = left_join(al30 , al30d, by = ("fecha"))
  al = left_join(al, al30c, by = ("fecha"))
  al = al %>% ungroup() %>% select(fecha, cierre.x, cierre.y, cierre)
  colnames(al) = c('fecha', 'pesos', 'dolar', 'cable')
  al = al %>% mutate(mepAL = pesos / dolar,
                     cclAL = pesos / cable,
                     canjeAL = (cclAL / mepAL)- 1 )
  
  gd = left_join(gd30 , gd30d, by = ("fecha"))
  gd = left_join(gd, gd30c, by = ("fecha"))
  gd = gd %>% ungroup() %>% select(fecha, cierre.x, cierre.y, cierre)
  colnames(gd) = c('fecha', 'pesos', 'dolar', 'cable')
  gd = gd %>% mutate(mepGD = pesos / dolar,
                     cclGD = pesos / cable,
                     canjeGD = (cclGD / mepGD)- 1 )
  
  
  mep = left_join(al, gd, by = c('fecha'))
  colnames(mep) = c('fecha', 'AL30', 'AL30D', 'AL30C', 'MEP-AL30', 'CCL-AL30', 'CANJE-AL30' ,
                              'GD30', 'GD30D', 'GD30C', 'MEP-GD30', 'CCL-GD30', 'CANJE-GD30')
  mep = mep %>% drop_na('MEP-GD30')
  
  write_csv(mep, paste0(fileDirectory, 'mep.csv'))
  
  mep
}
