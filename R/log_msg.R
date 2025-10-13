#' Loguea un mensaje en un archivo de log con un timestamp y nivel de severidad.
#' @details Esta función escribe un mensaje en un archivo de log especificado,
#' incluyendo un timestamp y un nivel de severidad (INFO, WARNING, ERROR).
#' @param msg El mensaje a loguear.
#' @param level El nivel de severidad del mensaje (por defecto es "INFO").
#' @param log_file El archivo donde se guardará el log (por defecto es "app.log").
#' @return No retorna ningún valor.
#' @examples log_msg("Script finalizado correctamente.", "INFO")
#'
log_msg <- function(msg, level = "INFO", log_file = "app.log") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  write(line, file = log_file, append = TRUE)
}
