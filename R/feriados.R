getFeriados = function(...) {
  dbGetTable("calendarioFeriados", server = server, port = port)$date
}


addFeriados = function(
    lista,
    ...) {
  
  feriados = dbGetTable("calendarioFeriados", server = server, port = port)
  juntos = as_tibble(rbind(feriados, tibble(date = lista)))
  juntos = juntos %>% distinct() %>% arrange(date)  #remueve duplicados y ordena
  dbWriteDF(table = "calendarioFeriados", df = juntos, server = server, port = port, overwrite = T)
}

removeFeriados = function(
    lista, ...) {
  
  feriados = dbGetTable("calendarioFeriados", server = server, port = port)
  feriados = feriados %>% filter(!date %in% as.Date(lista))
  dbWriteDF(table = "calendarioFeriados", df = feriados, server = server, port = port, overwrite = T)

}

