#'
#' labelSlashMeses
#'
#' Es un una función que se usa dentro de scale_x_date y similares
#' y sirve para convertir los meses del año en español,
#' y además les pone un separador. En el caso de la variante
#' slash, el separador es "-" y en el caso de la variante
#' NL el separador es un salto de línea.
#'
#' @param date Un objeto de clase Date que es pasado automáticamente
#' por ggplot2.
#' @return el vector con las etiquetas de los meses en español y el separador
labelSlashMeses <- function(date) {
  meses = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
            "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  # si date viene con días, agregar días. Sino devolver sin días
  if (lubridate::day(date) != 1) {
    paste0(lubridate::day(date), "-", meses[lubridate::month(date)], "-", lubridate::year(date))
  } else {
    paste0(meses[lubridate::month(date)], "-", lubridate::year(date))
  }
  #paste0(meses[lubridate::month(date)], "-", lubridate::year(date))
}

#'
#' labelNLMeses

#' Es un una función que se usa dentro de scale_x_date y similares
#' y sirve para convertir los meses del año en español,
#' y además les pone un separador. En el caso de la variante
#' slash, el separador es "-" y en el caso de la variante
#' NL el separador es un salto de línea.
#'
#' #' @param date Un objeto de clase Date que es pasado automáticamente
#' por ggplot2.
#' #' @return el vector con las etiquetas de los meses en español y el separador
labelNLMeses <- function(date) {
  meses = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
            "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  # si date viene con días, agregar días. Sino devolver sin días
  if (lubridate::day(date) != 1) {
    paste0(lubridate::day(date), "\n", meses[lubridate::month(date)], "-", lubridate::year(date))
  } else {
    paste0(meses[lubridate::month(date)], "\n", lubridate::year(date))
  }
  #paste0(meses[lubridate::month(date)], "\n", lubridate::year(date))
}


