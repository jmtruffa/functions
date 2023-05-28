\name{getSeriesBCRA}
\alias{getSeriesBCRA}
\title{Devuelve las tablas del archivo Series del BCRA}
\usage{
  getSeriesBCRA()
}
\description{
Devuelve alguna de las siguientes tablas, correspondientes a las solapas del archivo series.xls del BCRA.

table:
bmBCRA -> BASE MONATARIA
reservas -> RESERVAS
depositos -> DEPOSITOS
prestamos -> PRESTAMOS
tasas -> TASAS DE MERCADO
instrBCRA -> INSTRUMENTOS DEL BCRA

format:
D -> Diario
VM -> Valores Mensuales
PM -> Promedio Mensual
VA -> Valor Anual
VFM
VPM -> Valor Promedio Mensual

download:
Si TRUE, llama a downloadSeriesBCRA para actualizar el seriese.
Default: FALSE

Value

tibble
  donde cada columna es equivalente a una columna del series.xls

}
\arguments{
\item{table}{Valores esperados: "bmBCRA", "reservas", "depositos", "prestamos", "tasas", "instrBCRA"}
\item{format}{Valores esperados: "D", "VM", "PM", "VA", "VFM", "VPM"}
\item{db}{Ubicación del archivo sqlite. Lo deduce según el host en donde esté corriendo. El archivo de base de datos que utiliza es data1.sqlite}
}
\examples{
getSeriesBCRA(format = "D") -> Devuelve bmBCRA en formato dario
getSeriesBCRA(table = "depositos", format = "D") -> Devuelve el diario de depositos
getSeriesBCRA(table = "instrBCRA") -> Devuelve el diario de datos de Instrumentos del BCRA. Solo tiene valores diarios
}
