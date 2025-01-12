% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variasPostgres.R
\name{getConf}
\alias{getConf}
\title{getConf}
\usage{
getConf(
  server = "local",
  user = Sys.getenv(paste(toupper(server), "POSTGRES_USER", sep = "_"), "postgres"),
  password = Sys.getenv(paste(toupper(server), "POSTGRES_PASSWORD", sep = "_"), "XXX"),
  host = Sys.getenv(paste(toupper(server), "POSTGRES_HOST", sep = "_"), ""),
  db = Sys.getenv(paste(toupper(server), "POSTGRES_DB", sep = "_"), ""),
  port = Sys.getenv(paste(toupper(server), "POSTGRES_PORT", sep = "_"), "")
)
}
\arguments{
\item{server}{local, aws, medina (REMOTO) o localhost. medina es para accederlo remotamente}

\item{user}{usuario de la DB. (la local es postgres)}

\item{password}{password de la db}

\item{host}{url donde está ubicado}

\item{db}{base_de_datos (data es la usual. localhost es postgres)}
}
\description{
Devuelve una lista con variables necesarias para consultar un determinado servidor POSTGRES.
Es una función que es utilizada, principalmente, por dbGetTable y dbWriteDF, dado que de esa manera
se les agrega un parámetro que es "server" y de esa manera se le indica donde tiene que grabar o
buscar esa tabla.
Se puede desestimar el parámetro en cuestión proveyéndolo.
Requiere del seteo en .Renviron de las variables de entorno siguientes:
LOCAL_POSTGRES_USER
LOCAL_POSTGRES_PASSWORD
LOCAL_POSTGRES_HOST
LOCAL_POSTGRES_DB
}
\details{
AWS_POSTGRES_USER
AWS_POSTGRES_PASSWORD
AWS_POSTGRES_HOST
AWS_POSTGRES_DB

MEDINA_POSTGRES_USER
MEDINA_POSTGRES_PASSWORD
MEDINA_POSTGRES_HOST
MEDINA_POSTGRES_DB

LOCALHOST_POSTGRES_USER
LOCALHOST_POSTGRES_PASSWORD
LOCALHOST_POSTGRES_HOST
LOCALHOST_POSTGRES_DB
}
