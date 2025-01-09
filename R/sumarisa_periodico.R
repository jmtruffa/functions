#' @title Sumariza data por mes
#' @description Esta functión recibe un dataframe y sumariza los datos por mes, calculando la suma, media, mínimo y mnáximo.
#' @param df Un dataframe
#' @param date_column El nombre de la columna que contiene la fecha. "date" por default
#' @param value_columns Las columnas que se desean sumarizar. Todas las columnas por default
#' @return Un dataframe con la sumarización de los datos por mes.
#' @examples
#' smonth(ccl, date_column = "date", value_columns = c("mepAL", "ccl3"))
#' @import dplyr
#' @import lubridate
#' @export
#'
smonth <- function(df, date_column = "date", value_columns = everything()) {
  require(dplyr)
  require(lubridate)
  # Ensure the date column is in Date format
  df[[date_column]] <- as.Date(df[[date_column]])

  # Extract year and month from the date column
  df$year_month <- format(df[[date_column]], "%Y-%m")

  # Summarize the data by month using multiple functions (sum, mean, min, max)
  monthly_summary <- df %>%
    group_by(year_month) %>%
    summarise(across(all_of(value_columns),
                     list(sum = ~sum(. , na.rm = TRUE),
                          mean = ~mean(. , na.rm = TRUE),
                          min = ~min(. , na.rm = TRUE),
                          max = ~max(. , na.rm = TRUE)),
                     .names = "{.col}_{.fn}"))

  return(monthly_summary)
}

#' @title Sumariza data por semana
#' @description Esta functión recibe un dataframe y sumariza los datos por semana, calculando la suma, media, mínimo y mnáximo.
#' @param df Un dataframe
#' @param date_column El nombre de la columna que contiene la fecha. "date" por default
#' @param value_columns Las columnas que se desean sumarizar. Todas las columnas por default
#' @return Un dataframe con la sumarización de los datos por semana.
#' @examples
#' sweek(ccl, date_column = "date", value_columns = c("mepAL", "ccl3"))
#' @import dplyr
#' @import lubridate
#' @export
#'
sweek <- function(df, date_column = "date", value_columns = everything()) {
  require(dplyr)
  require(lubridate)
  # Ensure the date column is in Date format
  df[[date_column]] <- as.Date(df[[date_column]])

  # Extract year and week from the date column
  df$year_week <- format(df[[date_column]], "%Y-%U")

  # Summarize the data by week using multiple functions (sum, mean, min, max)
  weekly_summary <- df %>%
    group_by(year_week) %>%
    summarise(across(all_of(value_columns),
                     list(sum = ~sum(. , na.rm = TRUE),
                          mean = ~mean(. , na.rm = TRUE),
                          min = ~min(. , na.rm = TRUE),
                          max = ~max(. , na.rm = TRUE)),
                     .names = "{.col}_{.fn}"))

  return(weekly_summary)
}
