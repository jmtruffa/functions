% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{setup}
\alias{setup}
\title{Setup para establecer server, port y theme_usado}
\usage{
setup(server = "local", port = 5432)
}
\arguments{
\item{server}{local, aws, medina (REMOTO)}

\item{port}{puerto de conexión}
}
\description{
Establece las variables gloables server, port y theme_usado
Default son server = "local", port = 5432.
}
\examples{
 \dontrun{
setup(server = "medina", 15142)
}

}
