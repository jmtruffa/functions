\name{getAlphacast}
\alias{getAlphacast}
\title{Downloads a dataset from Alphacast}
\usage{
  getAlphacast(ds, validUntil = 0, directory = where/you/save/data, APIKey)
}
\description{
Downloads a dataset from Alphacast.

Uses Sys.getenv("alphacast") for the API key. Or provide APIKey

Converts Date into Date format.
}
\arguments{
\item{ds}{the number of the dataset to download from Alphacast}
\item{validUntil}{number of minutes of validity of the file. 1440 = 1 day}
\item{directory}{Where to save the file}
\item{APIKey}{APIKey in string format.}
}
\value{
The dataset with the Date column mapped to Date.
}
\examples{
  getAlphacast(5288, validUntil = 1440)
  getAlphacast(27844, validUntil = 0, directory = '~/data/')
}
