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
days360 <- function(start_date, end_date, method = c("US", "EU")) {

  method <- match.arg(method)

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  # --- igualamos longitudes (reciclado estilo R) -------------------
  n1 <- length(start_date); n2 <- length(end_date)
  if (n1 != n2) {
    if (n1 == 1L) start_date <- rep(start_date, n2)
    else if (n2 == 1L) end_date <- rep(end_date, n1)
    else stop("start_date y end_date deben tener la misma longitud o uno de ellos ser de longitud 1")
  }

  # --- helper vectorizado: último día de febrero -------------------
  is_last_feb <- function(d) {
    m   <- as.integer(format(d, "%m"))
    y   <- as.integer(format(d, "%Y"))
    day <- as.integer(format(d, "%d"))
    last_feb_day <- as.integer(format(as.Date(paste0(y, "-03-01")) - 1, "%d"))
    (m == 2L) & (day == last_feb_day)
  }

  # --- descomposición de fechas -----------------------------------
  s_day   <- as.integer(format(start_date, "%d"))
  s_month <- as.integer(format(start_date, "%m"))
  s_year  <- as.integer(format(start_date, "%Y"))

  e_day   <- as.integer(format(end_date, "%d"))
  e_month <- as.integer(format(end_date, "%m"))
  e_year  <- as.integer(format(end_date, "%Y"))

  # --- ajustes US / EU --------------------------------------------
  if (method == "US") {
    # Regla 1 (inicio)
    s_day <- ifelse(s_day == 31L | is_last_feb(start_date), 30L, s_day)
    # Regla 2 (fin)
    e_day <- ifelse(e_day == 31L & s_day >= 30L, 30L, e_day)
  } else {                # método EU
    s_day <- ifelse(s_day == 31L, 30L, s_day)
    e_day <- ifelse(e_day == 31L, 30L, e_day)
  }

  # --- fórmula 30/360 ---------------------------------------------
  360L * (e_year - s_year) +
    30L * (e_month - s_month) +
    (e_day - s_day)
}



