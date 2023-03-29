\name{addFeriados}
\alias{addFeriados}
\title{Agrega feriados a la base de datos}
\usage{
  addFeriados(lista, db)
}
\description{
Abre la conexión de sqlite que se le indique en db (default es "~/data/test.sqlite3") y agrega los feriados detallados en lista.

Esto permite agregar feriados a un calendario.

No devuelve nada. Debería consultarse y filtrarse luego con getFeriados

}
\arguments{
\item{lista}{Vector de fechas. No hace falta convertir a fecha}
\item{db}{Ubicación del archivo sqlite}
}
\examples{
addFeriados(lista = c("2022-11-07", "2022-11-06"))
}
