\name{getUSCPI}
\alias{getUSCPI}
\title{Devuelve la serie de CPI de USA en modo mensual o diario}
\usage{
  getUSCPI()
}
\description{
Consulta la base  de datos sqlite "~/GoogleDrive/Mi unidad/data/test.sqlite3" o '/data/test.sqlite3' para el host linux y devuelve una tibble con los datos en formato mensual o diario.

Value

tibble
  date: fecha. Es diaria o mensual según lo solicitado
  series_id: CUSR0000SA0 que corresponde al valor indice de la CPI
  value: valor del índice

}
\arguments{
\item{format}{Valores esperados: "daily" o "monthly"}
\item{db}{Ubicación del archivo sqlite. Lo deduce según el host en donde esté corriendo}
}
\examples{
getUSCPI() -> Devuelve el mensual pero en forato raw como se encuentra en la base
getUSCPI("monthly") -> Devuelve el diario
getUSCPI("daily") -> Devuelve el diario
}
