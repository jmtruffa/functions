#' Filtra el primer registro de cada mes
#'
#' Esta función recibe un dataframe y una columna de fecha y devuelve un nuevo dataframe que contiene únicamente
#' la primera fila (según la fecha) de cada mes. Utiliza la sintaxis y funciones del tidyverse junto con evaluación tidy
#' para especificar la columna de fecha.
#'
#' @param df Dataframe que se desea filtrar.
#' @param date_col Columna que contiene las fechas. Debe especificarse sin comillas.
#'
#' @return Un dataframe con únicamente la primera fila de cada mes, basado en la columna de fecha proporcionada.
#'
#' @examples
#' \dontrun{
#' # Supongamos que 'df' es un dataframe que contiene una columna de fechas llamada 'fecha'.
#' resultado <- first_date(df, fecha)
#' }
#'
#' @export
first_date <- function(df, date_col) {
  df %>%
    arrange({{ date_col }}) %>%  # Ordena el dataframe por la columna de fecha en orden ascendente
    group_by(Year = lubridate::year({{ date_col }}),
             Month = lubridate::month({{ date_col }})) %>%  # Agrupa por año y mes
    slice_min(order_by = {{ date_col }}, n = 1, with_ties = FALSE) %>%  # Selecciona la primera fila de cada grupo
    ungroup() %>%  # Elimina la agrupación
    select(-Year, -Month)  # Elimina las columnas auxiliares de agrupación
}
