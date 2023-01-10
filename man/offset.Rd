\name{offset}
\alias{offset}
\title{Offset a series from top-down or bottom-up}
\usage{
  offset(df, direction = 1, names = 2)
}
\description{
Calculates an index of base 100 bottom-up or top-down to compare columns in a df.

Does not calculate on first column since assumes it might be the date.
direction = 1 -> Top-down. direction = -1 -> Botton-up (This one is used to calculate index backwards.)

Dataframe must not contain NAs.
}
\arguments{
\item{df}{the dataframe to be passed to the function.}
\item{direction}{1 for forward, -1 for backwards}
\item{names}{indicates in which column resides the names of the data. Ussually 2 for long format. If 0, expects wide.}
}
\examples{
offset(df, direction = 1)
offset(df, direction = -1, names = 2)
offset(df, direction = 1, names = 0)
}
