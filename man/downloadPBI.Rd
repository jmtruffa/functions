\name{downloadPBI}
\alias{downloadPBI}
\title{Descarga el archivo desde url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_22.xls"
\usage{
  downloadPBI()
}
\description{
Descarga el archivo en cuestión. Graba una copia en /Downloads/temp si es linux o en "~/Google Drive/Mi unidad/analisis financieros/temp" y luego parsea el archivo y lo graba en la base de datos sqlite "~/GoogleDrive/Mi unidad/data/data1.sqlite3" o '/data/data1.sqlite3' para el host linux.

Value

Devuelve una lista de dataframes. Cada uno corresponde a PBI a precios constantes y corrientes, tanto anual como trimestral
Se puede solicitar directo de la base de datos (tomando las últimas tablas descargadas) a traves de getPBI()

}
\arguments{
\item{db}{Ubicación del archivo sqlite}
\item{url}{Ubicación del archivo en sitio indec}
}
\examples{
downloadPBI()
downloadPBI(db = ubicacion-de-la-db-sqlite, url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_22.xls") ## por si cambia el archivo de nombre
}
