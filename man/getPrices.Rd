% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPrices.R
\name{getPrices}
\alias{getPrices}
\title{getPrices}
\usage{
getPrices(
  letras,
  settlementDate,
  yield,
  initialFee = 0,
  endingFee = 0,
  endpoint = "price"
)
}
\arguments{
\item{letras}{Puede ser un vector conteniendo múltiples}

\item{settlementDate}{Fecha en formato "yyyy-mm-dd"}

\item{yield}{Puede ser vector.}

\item{initialFee}{Valor único a aplicar upfront. Se recicla}

\item{endingFee}{Valor único a aplicar al final (si se intenta estimar una venta posterio)}

\item{endpoint}{"price"}
}
\value{
tibble with same data plus price & mduration. There are several other values that the API
returns but not implemented yet.
}
\description{
getPrices
}
\details{
Obtiene el precio, dada una tasa, de un activo. Opera contra la API local de yields.
Pueden pasársele vectores. Fees y settlement no opera con vectores.
}
\examples{
getPrices("GD30D", "2023-07-13", 0.28, initialFee = 0.007515, endingFee = 0, endpoint = "yield")

}
