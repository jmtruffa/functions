\name{getFeriados}
\alias{getFeriados}
\title{Obtiene la lista de feriados de la base de datos}
\usage{
  getFeriados(db)
}
\description{
Abre la conexión de sqlite que se le indique en db (default es "~/GoogleDrive/Mi unidad/data/test.sqlite3") y devuelve los feriados disponibles.

Esto permite crear un calendario y agregarle los feriados de la siguiente manera:

cal = create.calendar(nombre-del-calendario-a-crear,
                       getFeriados(),
                       weekdays=c("saturday", "sunday"))

}
\arguments{
\item{db}{Ubicación del archivo sqlite}
}
\examples{
getFeriados(db = "directorio/archivo-sqlite")
}
