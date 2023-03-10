# Calcula el CCL  de los últimos 90 o la cantidad de días que se determine en inicio
# calcula la fecha final del rango como la fecha de hoy ajustada si es hábil - 4 así tiene los úlitmos 5 días


returnCcl <- function(lookBack = 90, validUntil = 0)   {
  require(tidyquant)
  require(bizdays)
  require(tidyverse)
  require(ggthemes)
  #setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  cal <- create.calendar("Argentina/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
  final <- adjust.previous(Sys.Date() + 1, cal)
  inicio <- adjust.previous(final - lookBack, cal)
  file = paste("../ccl/", "ccl", str_remove_all(inicio, "-"), "-", str_remove_all(final, "-"), ".csv", sep='')
  file_prom = paste("../ccl/", "CCLProm", str_remove_all(inicio, "-"), "-", str_remove_all(final, "-"), ".csv", sep='')
  file_grafprom = paste("../ccl/", "CCLPromGraf", str_remove_all(inicio, "-"), "-", str_remove_all(final, "-"), ".jpg", sep='')
  
  # cargo los adr argentinos y los cedears. Cada uno viene con sus symbol, symbol_local y ratio.
    adr_argentinos <- read_csv("/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/ccl/ADRs_Argentinos/adr_argentinos.csv", 
                               col_types = cols(empresa = col_skip(), 
                                                ratio = col_number()))
  cedears <- read_csv("/Users/Juan/Google Drive/Mi unidad/analisis financieros/ccl/Cedear/cedears.csv", 
                      col_types = cols(Nombre = col_skip(), 
                                       Cod_Caja = col_skip(), ISIN_Cedear = col_skip(), 
                                       ISIN_Suby = col_skip(), CUSIP = col_skip(), 
                                       ratio = col_number()))
  
  # creo un df con todos los activos a analizar con su correspondiente activo local
  # luego hay que reciclarla para pedir a tq_get todos los archivos (externo y local) y no hacer varias llamadas
  activos <- bind_rows(adr_argentinos, cedears)
  lista_activos <- bind_rows(activos %>% transmute(symbol1 = symbol, symbol2 = symbol_local, ratio = ratio),
                             activos %>% transmute(symbol1 = symbol_local, symbol2 = symbol, ratio = ratio))
  colnames(lista_activos) <- c("symbol", "symbol2", "ratio")
  rm(activos) # lo borro
  
  # voy a restringir a los activos que necesito para ccl y nada mas porque está tardando mucho con la 
  # conexión que tengo
  lista_activos <- lista_activos %>% filter(symbol == "GGAL.BA" | symbol == "BMA.BA" | symbol == "YPFD.BA" | symbol == "EDN.BA" | symbol == "GGAL"| symbol == "BMA"| symbol == "EDN"| symbol == "YPF")
  
  # esto me devuelve un df con los precios en formato OHLCVA.
  
  fileName = paste('/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/functions/data/', 'cclStocksPrices', '.csv', sep ='')
  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    precios <- lista_activos$symbol %>%
      tq_get(get  = "stock.prices",
             from = inicio,
             to   = final + 1) %>%
      group_by(symbol)
    write_csv(precios, fileName)
  } else {
    precios = read_csv(fileName)
  }
  
  #ahora los separo entre local y externo
  local <- precios %>% filter(str_detect(symbol, fixed(".")))
  afuera <- precios %>% filter(!str_detect(symbol, fixed(".")))
  rm(precios) # lo descarto
  
  # ahora agregamos el simbolo correspondiente a cada uno, tomandolo de lista_activos
  local <- left_join(local, lista_activos)
  afuera <- left_join(afuera, lista_activos)
  
  # ahora que ambos tiene su correspondiente symbolo de la otra bolsa los juntamos
  df_ccl <- left_join(local, afuera, by = c("symbol2" = "symbol", "date" = "date"))
  
  
  # ahora tenemos en final los activos locales y su precio afuera
  # ahora vamos a calcularle el ccl
  # y luego borrarles los que tienen volumen 0
  # finalmente lo graba
  
  df_ccl <- df_ccl  %>% mutate(
    ccl = close.x * ratio.x / close.y) %>% 
    select(date, symbol, volume.x, close.x, adjusted.x, symbol2, ratio.x, volume.y, close.y, adjusted.y, ccl) %>% 
    filter (volume.x != 0)
  
  #write_csv(df_ccl, file, col_names = TRUE)
  
  
  ccl <- df_ccl %>% 
    group_by(date) %>%
    summarise (CCL_prom = mean(ccl)) %>% drop_na()
  colnames(ccl) <- c('fecha', 'CCL')
  
  
  # Acá calculo un CCL con Galicia, BMA, YPF y EDN como para tomar una referencia.
  # GBYE <- df_ccl %>% select(date, symbol, close.x, symbol2, ratio.x, close.y, ccl) %>% 
  #   filter(symbol == "GGAL.BA" | symbol == "BMA.BA" | symbol == "YPFD.BA" | symbol == "EDN.BA") %>% drop_na()
  # 
  # write_csv(GBYE, file_prom, col_names = TRUE)
  # 
  grafprom <- ccl %>%
      ggplot(aes(x = fecha, y = CCL)) +
      geom_line() +
      theme_economist() +
      scale_x_date(date_breaks="1 month", date_labels="%Y %m") +
      scale_color_economist() +
      labs(title = "CCL prom con GGAL BMA YPFD EDN",
           y = "CCL calculado con precios de Cierre", x = "")
  
  
  ccl

}


