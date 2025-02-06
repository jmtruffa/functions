#' Filtra el último registro de cada mes.
#'
#' Esta función recibe un dataframe y una columna de fecha y devuelve un nuevo dataframe que contiene únicamente
#' la última fila (según la fecha) de cada mes. Se utiliza el enfoque de tidyverse y evaluación tidy para
#' especificar la columna de fecha.
#' Es ideal para filtrar datos de series temporales, donde se desea conservar únicamente el último registro
#' de la serie por tratarse de stocks.
#'
#' @param df Dataframe que se desea filtrar.
#' @param date_col Columna que contiene las fechas. Debe especificarse sin comillas.
#'
#' @return Un dataframe con únicamente la última fila de cada mes, basado en la columna de fecha proporcionada.
#'
#' @examples
#' \dontrun{
#' # Supongamos que 'df' es un dataframe que contiene una columna de fechas llamada 'fecha'.
#' resultado <- filter_last_date(df, fecha)
#' }
#'
#' @export
las_date <- function(df, date_col) {
  df %>%
    arrange({{ date_col }}) %>%  # Ordena el dataframe por la columna de fecha
    group_by(Year = lubridate::year({{ date_col }}),
             Month = lubridate::month({{ date_col }})) %>%  # Agrupa por año y mes
    slice_max(order_by = {{ date_col }}, n = 1, with_ties = FALSE) %>%  # Selecciona el último registro de cada grupo
    ungroup() %>%  # Elimina la agrupación
    select(-Year, -Month)  # Elimina las columnas auxiliares de agrupación
}
