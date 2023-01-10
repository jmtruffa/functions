\name{removeFeriados}
\alias{removeFeriados}
\title{Quita feriados a la base de datos}
\usage{
  removeFeriados(lista, db)
}
\description{
Abre la conexión de sqlite que se le indique en db (default es "~/GoogleDrive/Mi unidad/data/test.sqlite3") y remueve los feriados detallados en lista.

Esto permite quitar feriados a un calendario.

No devuelve nada. Debería consultarse y filtrarse luego con getFeriados

}
\arguments{
\item{lista}{Vector de fechas. No hace falta convertir a fecha}
\item{db}{Ubicación del archivo sqlite}
}
\examples{
removeFeriados(lista = c("2022-11-07", "2022-11-06"))
}
