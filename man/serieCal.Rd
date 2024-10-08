% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serieCal.R
\name{serieCal}
\alias{serieCal}
\title{serieCal}
\usage{
serieCal(from, to = Sys.Date(), ...)
}
\arguments{
\item{from}{Fecha de inicio}

\item{to}{Fecha de fin}

\item{...}{Argumentos adicionales para dbGetTable}
}
\value{
df de fechas
}
\description{
Retorna un df de fechas entre dos fechas dadas, excluyendo los días feriados basado en el calendario
disponible en la tabla "calendarioFeriados"
}
\examples{
serieCal("2019-01-01", "2019-01-31")
}
