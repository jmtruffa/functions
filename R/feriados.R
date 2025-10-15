#' Obtiene la lista de feriados de la base de datos
#'
#' Abre la conexión (vía `dbGetTable()`) y devuelve los feriados disponibles.
#' La conexión se resuelve con los parámetros pasados en `...` (por ejemplo
#' `server`, `port`, `db`) o con defaults internos.
#'
#' @param ... Parámetros opcionales reenviados a `dbGetTable()`, por ejemplo
#'   `server`, `port`, `db`, `user`, `password`.
#'
#' @return Un vector `Date` con las fechas de feriado.
#'
#' @examples
#' \dontrun{
#' # Usando configuración por defecto
#' getFeriados()
#'
#' # Indicando servidor/puerto/base
#' getFeriados(server = "medina", port = 5432, db = "data")
#' }
#'
#' @seealso dbGetTable
#' @export
getFeriados = function(...) {
  dbGetTable("calendarioFeriados", server = server, port = port)$date
}

#' Agrega feriados a la base de datos
#'
#' Lee la tabla de feriados, agrega las fechas provistas y escribe el resultado
#' sin duplicados. La conexión se resuelve con `...` (p. ej. `server`, `port`, `db`).
#'
#' @param lista Vector de fechas (character o `Date`). No hace falta convertir.
#' @param ...   Parámetros opcionales reenviados a `dbGetTable()`/`dbWriteDF()`,
#'   por ejemplo `server`, `port`, `db`, `user`, `password`.
#'
#' @return `invisible(TRUE)` (efecto de escritura); sin valor útil.
#'
#' @examples
#' \dontrun{
#' addFeriados(lista = c("2022-11-07", "2022-11-06"),
#'             server = "medina", port = 5432, db = "data")
#' }
#'
#' @seealso dbGetTable, dbWriteDF
#' @export
addFeriados = function(
    lista,
    ...) {

  feriados = dbGetTable("calendarioFeriados", server = server, port = port)
  juntos = as_tibble(rbind(feriados, tibble(date = lista)))
  juntos = juntos %>% distinct() %>% arrange(date)  #remueve duplicados y ordena
  dbWriteDF(table = "calendarioFeriados", df = juntos, server = server, port = port, overwrite = T)
  invisible(TRUE)
}

#' Quita feriados de la base de datos
#'
#' Lee la tabla de feriados, elimina las fechas provistas y escribe el resultado.
#' La conexión se resuelve con `...` (p. ej. `server`, `port`, `db`).
#'
#' @param lista Vector de fechas (character o `Date`) a remover.
#' @param ...   Parámetros opcionales reenviados a `dbGetTable()`/`dbWriteDF()`,
#'   por ejemplo `server`, `port`, `db`, `user`, `password`.
#'
#' @return `invisible(TRUE)` (efecto de escritura); sin valor útil.
#'
#' @examples
#' \dontrun{
#' removeFeriados(lista = c("2022-11-07", "2022-11-06"),
#'                server = "medina", port = 5432, db = "data")
#' }
#'
#' @seealso dbGetTable, dbWriteDF
#' @export
removeFeriados = function(
    lista, ...) {

  feriados = dbGetTable("calendarioFeriados", server = server, port = port)
  feriados = feriados %>% filter(!date %in% as.Date(lista))
  dbWriteDF(table = "calendarioFeriados", df = feriados, server = server, port = port, overwrite = T)
  invisible(TRUE)

}

