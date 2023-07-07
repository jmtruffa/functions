#' Trae una tabla de las bases de datos locales
#'
#' @details
#' Simplifica el trae una tabla de las bases locales.
#' Cada tabla tiene una db asociada. En la base test
#' La función busca la tabla y abre la DB que corresponda
#'
#'  @param Tabla No tiene valor por defecto
#'  @param File permite cambiar el path y nombre de la db a buscar
#'  Sino va al default que se llama "test"
#'  @returns Una tibble igual a la tabla consultada
#'
#'  @examples getTable("tabla-a-buscar") -> Devuelve la tibble
#'


getTable = function(table, file = "~/data/test.sqlite3") {
  tabla = data.frame()
  if (!is.null(table)) {
    con = DBI::dbConnect(RSQLite::SQLite(), file)
    # trae la tabla con los nombres y dbs
    df = dplyr::as_tibble(tbl(con, "tables"))
    DBI::dbDisconnect(con)
    # busca table a ver en qué DB está
    db = df %>% filter(name == table) %>% pull(db)

    db = paste0(db, ".sqlite3")
    db = file.path('~/data', db)
    #db = df[df[1]==table][2]

    if (!is.null(db)) {

      # abre la db que encontró.
      con2 = DBI::dbConnect(RSQLite::SQLite(), db)
      tabla = dplyr::as_tibble(tbl(con2, table))
      #tabla = DBI::dbReadTable(con2, table)
      DBI::dbDisconnect(con2)

      col = which(grepl('date|Date|fecha|Fecha', names(tabla)))

      #if (col != 0 | length(col) != 0) {
      if (length(col) != 0)

        if (tabla[[col]][1] >= 1000000)
          tabla[[col]] = tabla$date = as.Date(as.POSIXct(tabla$date,origin = "1970-01-01"))
        else {
          tabla[[col]] =  as.Date(as.POSIXct.Date(tabla[[col]], origin = "1970-01-01"))
        }

        #tabla = as_tibble(tabla)
        #tabla[[col]] =  as.Date(as.POSIXct.Date(tabla[[col]], origin = "1970-01-01")) # esta anda con dolar
        #tabla$date = as.Date(as.POSIXct(tabla$date,origin = "1970-01-01")) # esta anda con depositos
      }

    }

    return(tabla)

  }


getTable(table = "prestamos")



