#' Guarda un gráfico en un archivo
#'
#' Esta función guarda un gráfico en un archivo con un nombre que incluye
#' la fecha y el nombre de la variable (o el provisto en `name`).
#'
#' @param fecha Fecha incluida en el nombre. Default: `Sys.Date()`.
#' @param variable Objeto ggplot a guardar.
#' @param name Nombre base del archivo (sin extensión). Si `NULL`, se usa
#'   el nombre de la variable capturado con `deparse(substitute(variable))`.
#' @param path Carpeta destino.
#' @param width,height,dpi,units Parámetros de `ggplot2::ggsave()`.
#' @return Invisiblemente, la ruta del archivo escrito.
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
#' grabaGrafo(variable = p, name = "scatter_mtcars")
#' }
#' @export
grabaGrafo <- function(fecha = Sys.Date(), variable, name = NULL,
                       path = "~/OneDrive/outlier/docs/tablasGraficos",
                       width = 14.2, height = 7.61, dpi = 600, units = "in") {

  if (missing(variable))
    stop("Falta variable")

  variable_name <- if (!is.null(name)) name else deparse(substitute(variable))
  fecha <- as.Date(fecha)

  file <- file.path(path, sprintf("%s %s.png", format(fecha, "%Y%m%d"), variable_name))

  # Convertir tamaño a pixeles si viene en pulgadas (que es tu caso típico)
  if (units == "in") {
    w_px <- width  * dpi
    h_px <- height * dpi
  } else if (units == "px") {
    w_px <- width
    h_px <- height
  } else {
    stop("units debe ser 'in' o 'px' en esta implementación")
  }

  # Abrir dispositivo PNG usando cairo, sin ragg ni ggsave
  grDevices::png(
    filename = file,
    width    = w_px,
    height   = h_px,
    res      = dpi,
    type     = "cairo"
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  # Renderizar el gráfico
  print(variable)
}
