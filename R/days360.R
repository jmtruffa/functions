#' @title days360
#' @description Calcula el número de días entre dos fechas utilizando el método 360.
#' @param start_date Fecha de inicio.
#' @param end_date Fecha de fin.
#' @param method Método de cálculo de días. Puede ser "US" o "EU". Usa US por default.
#' @return Número de días entre las dos fechas.
#' @examples
#' fecha_inicio <- "2023-01-01"
#' fecha_fin <- "2024-06-30"
#' days360(fecha_inicio, fecha_fin)
#' @export
#' @importFrom utils format
#' @importFrom base as.Date
#' @importFrom base as.numeric
#'
days360 = function(start_date, end_date, method = "US") {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  start_day <- as.numeric(format(start_date, "%d"))
  end_day <- as.numeric(format(end_date, "%d"))
  start_month <- as.numeric(format(start_date, "%m"))
  end_month <- as.numeric(format(end_date, "%m"))
  start_year <- as.numeric(format(start_date, "%Y"))
  end_year <- as.numeric(format(end_date, "%Y"))

  if (method == "US") {
    start_day <- ifelse(start_day == 31 | (start_month == 2 & start_day == 28), 30, start_day)
    end_day <- ifelse(end_day == 31 & start_day < 30, 1, end_day)
    end_month <- ifelse(end_day == 1 & end_day == 31 & start_day < 30, end_month + 1, end_month)
    end_day <- ifelse(end_day == 31, 30, end_day)
  } else if (method == "EU") {
    start_day <- ifelse(start_day == 31, 30, start_day)
    end_day <- ifelse(end_day == 31, 30, end_day)
  }

  days360 <- 360 * (end_year - start_year) + 30 * (end_month - start_month) + (end_day - start_day)
  return(days360)
}



