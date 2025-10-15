#'
#' @title serieCal
#' @description Retorna un df de fechas entre dos fechas dadas, excluyendo los días feriados basado en el calendario
#' disponible en la tabla "calendarioFeriados"
#' @param from Fecha de inicio
#' @param to Fecha de fin
#' @param ... Argumentos adicionales para dbGetTable
#' @return df de fechas
#' @examples
#' \dontrun{
#' serieCal("2019-01-01", "2019-01-31")
#' }
#' @export
serieCal = function(from, to = Sys.Date(), ...) {
  require(bizdays)
  require(functions)
  cal = create.calendar('cal', dbGetTable("calendarioFeriados", ...)$date, weekdays = c('saturday','sunday'))
  dias = tibble(date = bizseq(from, to, 'cal'))
  return(dias)
}

