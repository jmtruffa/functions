\name{curvaAR}
\alias{curvaAR}
\title{Retrieves the Argentinean sovereign hard-dollar debt curve}
\usage{
  curvaAR(from, to, comi)
}
\description{
  Retrieves the Argentinean hard-dollar yield curve.
  Bonds included:

  'AL29D',
  'AL30D',
  'AL35D',
  'AE38D',
  'AL41D',
  'GD29D',
  'GD30D',
  'GD35D',
  'GD38D',
  'GD41D',
  'GD46D'

  Returns ticker, date, price, volume, openingPrice, max, min, initialFee, endingFee, yield, mduration

  Returns one row per date.
}
\arguments{
  \item{from}{Date from. Should be equal or greate than "2020-09-15"}
  \item{to}{Default Sys.Date()}
  \item{comi}{Commision to include in calculations.}
}
\examples{
\dontrun{
  curvaAR(from = "2022-01-01", to = "2022-10-01", comi = 0.0100)
}
}
