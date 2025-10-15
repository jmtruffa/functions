#' @title grabaTabla
#' @description Graba una tabla en formato png
#' @param fecha Fecha de la tabla
#' @param variable Tabla a grabar
#' @param path Ruta donde se grabará la tabla
#' @param vwidth Ancho de la tabla
#' @param vheight Alto de la tabla
#' @return NULL
#' @examples
#' \dontrun{grabaTabla(variable = tablaMLC)
#' grabaTabla(variable = tablaMLC, vwidth = 990, vheight = 409)
#' }
#' @export
grabaTabla = function(fecha = Sys.Date(), variable, path = "~/OneDrive/outlier/docs/tablasGraficos",
                      vwidth, vheight) {
  if (missing(variable)) stop("Falta variable")
  require(kableExtra)
  require(webshot2)
  variable_name <- deparse(substitute(variable))


  if (fecha != Sys.Date()) {
    fecha <- as.Date(fecha)
  }

  # Save the plot with fixed dimensions, dpi, and format
  save_kable(variable, file = file.path(path, paste0(format(fecha, "%Y%m%d"), " ", variable_name, ".png")),
             vwidth = vwidth, vheight = vheight)
  # ggsave(filename = file.path(path, paste0(format(fecha, "%Y%m%d"), " ", variable_name, ".png")),
  #        plot = variable,
  #        width = width,
  #        height = height,
  #        dpi = dpi,
  #        units = units)
}

#' @title grabaTabla2
#' @description Graba una tabla en formato png
#' @param fecha Fecha de la tabla
#' @param variable Tabla a grabar
#' @param path Ruta donde se grabará la tabla
#' @param vwidth Ancho de la tabla
#' @param vheight Alto de la tabla
#' @return NULL
#' @examples
#' \dontrun{grabaTabla(variable = tablaMLC)
#' grabaTabla(variable = tablaMLC, vwidth = 990, vheight = 409)
#' }
#' @export
grabaTabla2 = function(variable, fecha = Sys.Date(), path = "~/OneDrive/outlier/docs/tablasGraficos",
                       vwidth, vheight) {
  if (missing(variable)) stop("Falta variable")
  require(flextable)

  variable_name <- deparse(substitute(variable))


  if (fecha != Sys.Date()) {
    fecha <- as.Date(fecha)
  }

  # Save the plot with fixed dimensions, dpi, and format
  file = file.path(path, paste0(format(fecha, "%Y%m%d"), " ", variable_name, ".png"))
  save_as_image(x = variable, path = file)
}
