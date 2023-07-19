\name{getPBI}
\alias{getPBI}
\title{Devuelve la serie de PBI previamente bajada.}
\usage{
  getPBI()
}
\description{
  Devuelve la serie de PBI

  table:
    pbiCorriente (a precios de 2004)
    pbiConstante

  format:
    A = Anual
    T = Trimestral

  Value

  Devuelve una serie de tiempo con los valores.

}
\arguments{
  \item{table}{Qué tabla devolver. Corriente o Constante}
  \item{format}{A -> Anual. T -> Trimestral}
  \item{db}{Ubicación del archivo sqlite}
  \item{download}{Si TRUE llama a downloadPBI}
}
\examples{
  getPBI(table = "pbiConstante", format = "A") # Devuelve la serie anual del PBI Constante (precios 2004)
  getPBI(table = "pbiCorriente", format = "T") # Devuelve la serie trimestral del PBI Coorriente
}
