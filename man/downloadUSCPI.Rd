\name{downloadUSCPI}
\alias{downloadUSCPI}
\title{Descarga el archivo txt desde https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems}
\usage{
  downloadUSCPI()
}
\description{
Descarga el archivo en cuestión. Graba una copia en /Downloads/temp si es linux o en "~/Google Drive/Mi unidad/analisis financieros/temp" y luego parsea el archivo y lo graba en la base de datos sqlite "~/data/test.sqlite3" o '~/data/test.sqlite3' para el host linux.

Value

No devuelve valor. Para obtener los datos, utilizar getUSCPI()

}
\arguments{
\item{db}{Ubicación del archivo sqlite}
}
\examples{
downloadUSCPI()
downloadUSCPI(db = ubicacion-de-la-db-sqlite)
}
