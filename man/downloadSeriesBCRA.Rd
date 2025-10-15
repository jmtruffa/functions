\name{downloadSeriesBCRA}
\alias{downloadSeriesBCRA}
\title{Descarga el archivo series.xlsm desde url = "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/series.xlsm"}
\usage{
  downloadSeriesBCRA()
}
\description{
Descarga el archivo en cuestión. Graba una copia en /Downloads/temp si es linux o en "~/Google Drive/Mi unidad/analisis financieros/temp" y luego parsea el archivo y lo graba en la base de datos sqlite "~/data/data1.sqlite3" o '~/data/data1.sqlite3' para el host linux.

Value

Devuelve una lista de dataframes. Cada uno corresponde a una solapa de series.xlsm
Se puede solicitar directo de la base de datos (tomando las últimas tablas descargadas) a traves de getSeriesBCRA()

}
\arguments{
\item{db}{Ubicación del archivo sqlite}
}
\examples{
\dontrun{
downloadSeriesBCRA()
downloadSeriesBCRA(db = ubicacion-de-la-db-sqlite)

}
}
