\name{downloadData}
\alias{downloadData}
\title{Descarga series de BCRA}
\usage{
  downloadData(dataset, destPath = "~/Google Drive/Mi unidad/analisis financieros/functions/data/", lastDate = Sys.Date())
}
\description{
Permite descargar archivos desde BCRA. Los graba como excel files y luego los convierte a CSV y los ubica en el directorio indicado.

Tiene implementados:
- CER
- ITCRM
- A3500

Los nombres de los datasets son case insesitive.

}
\arguments{
\item{dataset}{El dataset a bajar. Ver los implementados.}
\item{destPath}{En qué path dejar el CSV final}
\item{lastDate}{La fecha más nueva para la que se necesita el dataset. Si es una fecha que ya tiene, no lo descarga}
}
\examples{
downloadData(dataset = "cer", lastDate = Sys.Date())
downloadData(dataset = "a3500", lastDate = Sys.Date())
}
