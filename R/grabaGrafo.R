#' Guarda un gráfico en un archivo
#'
#'  Esta función guarda un gráfico en un archivo con un nombre que incluye la fecha y el nombre de la variable.
#'
#'  @param fecha Fecha que se incluirá en el nombre del archivo. Por defecto es la fecha actual.
#'  @param variable Gráfico que se guardará.
#'  @param path Ruta donde se guardará el archivo. Por defecto es "~/OneDrive/outlier/docs/tablasGraficos".
#'  @param width Ancho del gráfico en pulgadas. Por defecto es 14.2.
#'  @param height Alto del gráfico en pulgadas. Por defecto es 7.61.
#'  @param dpi Resolución del gráfico. Por defecto es 300.
#'  @param units Unidades del gráfico. Por defecto es "in".
#'  @return NULL
#'  @export
#'  @examples
#'  \dontrun{
#'  library(ggplot2)
#'  library(outlier)
#'  data("datos")
#'  ggplot(datos, aes(x = fecha, y = valor)) + geom_line()
#'  grabaGrafo(variable = ggplot(datos, aes(x = fecha, y = valor)) + geom_line())
#'  }
#'  @import ggplot2
#'  @importFrom utils deparse substitute
#'  @importFrom base format
#'  @importFrom base as.Date
#'  @importFrom ggplot2 ggsave
#'  @importFrom base paste0
#'  @importFrom base file.path
#'  @importFrom base missing
grabaGrafo <- function(fecha = Sys.Date(), variable, path = "~/OneDrive/outlier/docs/tablasGraficos",
                       width = 14.2, height = 7.61, dpi = 600, units = "in") {
  if (missing(variable)) stop("Falta variable")

  # Capture the variable name as a string
  variable_name <- deparse(substitute(variable))

  if (fecha != Sys.Date()) {
    fecha <- as.Date(fecha)
  }

  # Save the plot with fixed dimensions, dpi, and format
  ggsave(filename = file.path(path, paste0(format(fecha, "%Y%m%d"), " ", variable_name, ".png")),
         plot = variable,
         width = width,
         height = height,
         dpi = dpi,
         units = units)
}
