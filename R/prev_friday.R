#' @title Agrega una columna con el valor encontrado el viernes previo
#' @description Crea una columna con el valor encontrado el viernes previo (o adía anterior si es feriado) a la fecha. Recibe un dataframe, el nombre de la columna que contiene la fecha, y las columnas para las cuáles creará nuevas columnas, con el sufijo "_lagged". Utiliza la semántica de tidyverse para seleccionar las columnas. Utiliza un calendario para ajustar la fecha en caso de que el viernes sea feriado.
#' @param data Dataframe
#' @param date_col Nombre de la columna que contiene la fecha
#' @param value_cols Columnas para las cuáles se creará una columna con el valor
#' del viernes previo
#' @param calendar_name Nombre del calendario a utilizar para ajustar la fecha
#' @return Dataframe con las columnas adicionales
#'
prev_friday <- function(data, date_col, value_cols, calendar_name = "cal") {
  # Ensure date_col is a symbol for tidy evaluation
  date_col <- enquo(date_col)

  # Select columns using tidyselect syntax
  value_cols <- enquo(value_cols)
  value_col_names <- names(tidyselect::eval_select(value_cols, data))

  # Add prev_friday_date column
  data <- data %>%
    mutate(
      current_friday = !!date_col + days(5 - wday(!!date_col, week_start = 1)),
      prev_friday = current_friday - days(7),
      prev_friday_date = adjust.previous(prev_friday, calendar_name)
    )

  # Join with the original data to get lagged values for all specified columns
  lagged_data <- data %>%
    left_join(
      select(data, !!date_col, all_of(value_col_names)),
      by = c("prev_friday_date" = quo_name(date_col)),
      suffix = c("", "_lagged")
    )

  # Clean up intermediate columns
  lagged_data <- lagged_data %>%
    select(-current_friday, -prev_friday, -prev_friday_date)

  return(lagged_data)
}




