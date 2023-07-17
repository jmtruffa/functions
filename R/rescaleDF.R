#' RescaleDF
#'
#' Reescala una serie de tiempo en función de un valor de la serie.
#' Hacia adelante y hacia atrás.
#' También ajusta las series contiguas si las hay.
#' Es ideal para cuando hay series de tiempo juntas en un df, se le
#' pasa el df a la función y reescalará todo en función de una fecha.
#'
#' @param df El dataframe que contiene los datos
#' @param date La fecha a buscar y que será el pivot
#' @param column en qué columna está el valor que será seteado a 100
#' @param format 0 Si es wide o la columna donde están los nombres en caso que sea long format
#'
#' @return devuelve el df reescalado
#'
#' @example rescale(df, "2019-01-08", 3, 0) -> 3 es la columna donde está el valor a setear en 100. format indica que es wide.
rescaleDF = function(df, date, column, format){
  ## buscamos el valor
  valor = pull(df[df[1] == date,][column])
  factor = 100 / valor
  df = df %>% group_by_at(format) %>% mutate_at(.vars = c(-1, -format),
                                                .funs = list(~ . * factor))
  return (df)
}
