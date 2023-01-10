getFeriados = function(db = "") {
  require(stringr)
  require(RSQLite)
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/GoogleDrive/Mi unidad/data/test.sqlite3"
    } else {
      db = '/data/test.sqlite3'
    }
  }
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  feriados = DBI::dbReadTable(con, "calendarioFeriados")
  DBI::dbDisconnect(con)
  as.Date(feriados$date)
}


addFeriados = function(
    lista,
    db = "") {
  require(stringr)
  require(RSQLite)
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/GoogleDrive/Mi unidad/data/test.sqlite3"
    } else {
      db = '/data/test.sqlite3'
    }
  }
  con = dbConnect(RSQLite::SQLite(), dbname = db)
  feriados = dbReadTable(con, "calendarioFeriados")
  feriados$date = as.Date(feriados$date)

  juntos = as_tibble(rbind(feriados, tibble(date = lista)))
  juntos = juntos %>% distinct() %>% arrange(date)  #remueve duplicados y ordena
  juntos$date = as.character(juntos$date) #convierte a texto para mandar a db

  DBI::dbWriteTable(con, "calendarioFeriados", juntos, overwrite = TRUE)
  DBI::dbDisconnect(con)
}


removeFeriados = function(
    lista,
    db = "") {
  require(stringr)
  require(RSQLite)
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/GoogleDrive/Mi unidad/data/test.sqlite3"
    } else {
      db = '/data/test.sqlite3'
    }
  }
  con = dbConnect(RSQLite::SQLite(), dbname = db)
  feriados = dbReadTable(con, "calendarioFeriados")
  feriados$date = as.Date(feriados$date)

  feriados = feriados %>% filter(!date %in% as.Date(lista))
  feriados$date = as.character(feriados$date)

  DBI::dbWriteTable(con, "calendarioFeriados", feriados, overwrite = TRUE)
  DBI::dbDisconnect(con)

}

