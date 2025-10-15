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
  if (missing(variable)) stop("Falta variable")

  # Use the provided name if given, otherwise use the default variable name capture
  variable_name <- if (!is.null(name)) name else deparse(substitute(variable))

  if (fecha != Sys.Date()) {
    fecha <- as.Date(fecha)
  }

  # Save the plot with fixed dimensions, dpi, and format
  ggplot2::ggsave(filename = file.path(path, paste0(format(fecha, "%Y%m%d"), " ", variable_name, ".png")),
         plot = variable,
         width = width,
         height = height,
         dpi = dpi,
         units = units)
}
