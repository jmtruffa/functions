#' Trae una tabla de las bases de datos locales
#'
#' @details
#' Simplifica el trae una tabla de las bases locales.
#' Cada tabla tiene una db asociada. En la base test
#' La función busca la tabla y abre la DB que corresponda.
#'
#' @param table No tiene valor por defecto
#' @param overrideDates Si TRUE, no convierte los campos Fecha
#' @param file permite cambiar el path y nombre de la db a buscar. Si no, usa "test"
#' @return Una tibble igual a la tabla consultada
#' @examples \dontrun{ getTable("tabla-a-buscar") }
#' @deprecated Usar `dbGetTable()` (ya no hay soporte para SQLite).
#' @export
getTable = function(table = NULL, overrideDates = FALSE, file = "~/data/test.sqlite3") {
  lifecycle::deprecate_soft(
    when = "1.0.0",  # Versión desde la que está deprecada
    what = "functions::getTable",
    with = "dbGetTable()",  # Alternativa recomendada
    details = "Quite el soporte de SQLlite."
  )
  require(tidyverse)
  tabla = data.frame()
  if (!is.null(table)) {
    con = DBI::dbConnect(RSQLite::SQLite(), file)
    # trae la tabla con los nombres y dbs
    df = dplyr::as_tibble(dplyr::tbl(con, "tables"))
    DBI::dbDisconnect(con)
    # busca table a ver en qué DB está
    db = df %>% filter(name == table) %>% pull(db)

    if (length(db) != 0) {

      db = paste0(db, ".sqlite3")
      db = file.path('~/data', db)

      # abre la db que encontró.
      con2 = DBI::dbConnect(RSQLite::SQLite(), db)
      tabla = dplyr::as_tibble(dplyr::tbl(con2, table))
      #tabla = DBI::dbReadTable(con2, table)

      DBI::dbDisconnect(con2)

      if (overrideDates == FALSE) {

        col = which(grepl('date|Date|fecha|Fecha', names(tabla)))

        #if (col != 0 | length(col) != 0) {
        if (length(col) != 0) {

          dateValue = tabla[[col]][1]

          if (dateValue <= 25569) { # 1899 base

            tabla[[col]] =  as.Date(tabla[[col]], origin = "1970-01-01")

          } else if (dateValue > 25569 & dateValue <= 100000) { ## excel

            tabla[[col]] =  as.Date(tabla[[col]], origin = "1899-12-30")

          } else { # Unix timeStamp

            tabla[[col]] = as.Date(as.POSIXct(tabla$date,origin = "1970-01-01"))

          }

        }

      }

      return(tabla)

    }


  }

  return(warning("No se encontró la tabla"))

}






