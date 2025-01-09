#' Setup para establecer server, port y theme_usado
#'
#' Establece las variables gloables server, port y theme_usado
#' Default son server = "local", port = 5432.
#' @param server local, aws, medina (REMOTO)
#' @param port puerto de conexión
#' @return NULL
#' @export
#' @examples
#'  \dontrun{
#' setup(server = "medina", 15142)
#' }
#'
setup = function(server = "local", port = 5432) {
  server <<- server
  port <<- port
  use_outlier <<- TRUE
  theme_usado <<- if (use_outlier) outlier::theme_outlier else mpt::theme_mpt
}
