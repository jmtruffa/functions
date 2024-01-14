#' dbGetTable
#'
#' Función para traer una tabla desde el servidor postgresQL
#'
#' @param server permite evitar los demás parámetros del servidor y que los obtenga con getConf
#' @param host IP del servidor postgreSQL
#' @param user Usuario de la base de datos
#' @param password Password de la base de datos en el servidor
#' @param port Puerto del servidor
#' @param dbname Base de Datos. Default='data'
#' @param table Acá hay que poner alguna. Controla que no esté vacío

dbGetTable = function(
    table,
    server = "local",
    host = NULL,
    port = NULL,
    dbname = NULL,
    user = NULL,
    password = NULL
) {

  require(RPostgreSQL)
  require(dplyr)
  require(DBI)

  # Use getConf to obtain parameters based on the server argument
  conf <- getConf(server)

  # Override parameters if they are provided during the function call
  conf$host <- ifelse(is.null(host), conf$host, host)
  conf$port <- ifelse(is.null(port), conf$port, port)
  conf$dbname <- ifelse(is.null(dbname), conf$db, dbname)
  conf$user <- ifelse(is.null(user), conf$user, user)
  conf$password <- ifelse(is.null(password), conf$password, password)

  if (is.null(table)) {
    return(warning("No se indicó nombre de tabla a devolver"))
  }

  con <- dbConnect(
    PostgreSQL(),
    host = conf$host,
    port = conf$port,
    dbname = conf$dbname,
    user = conf$user,
    password = conf$password
  )

  # Verificamos que la tabla exista
  if (!dbExistsTable(con, table)) {
    dbDisconnect(con)
    return(warning(paste("La tabla", table, "no existe en la base de datos.")))
  }

  query <- paste0('SELECT * FROM "', table,'"')
  df = dbGetQuery(con, query)
  dbDisconnect(con)

  return(as_tibble(df))

}



#' executeQuery
#'
#' Es un mero wraper de dbGetQuery de DBI para no tener que indicar servidor, puerto y db
#'
#' @param host IP del servidor postgreSQL
#' @param user Usuario de la base de datos
#' @param password Password de la base de datos en el servidor
#' @param port Puerto del servidor
#' @param dbname Base de Datos. Default='data'
#' @param query String con la query. Atención que tablas van con double quotes y valores con single. Recomiendo usar
#' single quotes afuera y double adentro.
#'
#' @examples executeQuery(query = 'SELECT * FROM "A3500"')
#'
#'
dbExecuteQuery = function(
    query,
    server = "local",
    host = NULL,
    port = NULL,
    db = NULL,
    user = NULL,
    password = NULL
) {
  require(RPostgreSQL)
  require(dplyr)
  require(DBI)

  # Use getConf to obtain parameters based on the server argument
  conf <- getConf(server)

  # Override parameters if they are provided during the function call
  conf$host <- ifelse(is.null(host), conf$host, host)
  conf$port <- ifelse(is.null(port), conf$port, port)
  conf$db <- ifelse(is.null(db), conf$db, db)
  conf$user <- ifelse(is.null(user), conf$user, user)
  conf$password <- ifelse(is.null(password), conf$password, password)

  con <- dbConnect(
    PostgreSQL(),
    host = conf$host,
    port = conf$port,
    dbname = conf$db,
    user = conf$user,
    password = conf$password
  )

  retQuery <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(retQuery)
}


#' dbWriteDF
#'
#' Wraper de dbWriteTable del paquete DBI pero que asume las conexiones de mi servidor.
#'
#' @param server permite evitar los demás parámetros del servidor y que los obtenga con getConf
#' @param host IP del servidor postgreSQL
#' @param user Usuario de la base de datos
#' @param password Password de la base de datos en el servidor
#' @param port Puerto del servidor
#' @param dbname Base de Datos. Default='data'
#'
#' @examples dbWriteDF(table = 'data', df = dfAGrabar)
dbWriteDF = function(
    table,
    df,
    server = "local",
    host = NULL,
    port = NULL,
    db = NULL,
    user = NULL,
    password = NULL
) {

  require(RPostgreSQL)
  require(dplyr)
  require(DBI)

  # Use getConf to obtain parameters based on the server argument
  conf <- getConf(server)

  # Override parameters if they are provided during the function call
  conf$host <- ifelse(is.null(host), conf$host, host)
  conf$port <- ifelse(is.null(port), conf$port, port)
  conf$db <- ifelse(is.null(db), conf$db, db)
  conf$user <- ifelse(is.null(user), conf$user, user)
  conf$password <- ifelse(is.null(password), conf$password, password)

  if (is.null(table) || is.null(df)) {
    warning("Both 'table' and 'df' must be provided.")
    return(NULL)
  }
  con <- dbConnect(
    PostgreSQL(),
    host = conf$host,
    port = conf$port,
    dbname = conf$db,
    user = conf$user,
    password = conf$password
  )
  DBI::dbWriteTable(con, table, df, row.names = FALSE)
  dbDisconnect(con)
}


#' getConf devuelve una lista con variables necesarias para consultar un determinado servidor POSTGRES.
#' Es una función que es utilizada, principalmente, por dbGetTable y dbWriteDF, dado que de esa manera
#' se les agrega un parámetro que es "server" y de esa manera se le indica donde tiene que grabar o
#' buscar esa tabla.
#' Se puede desestimar el parámetro en cuestión proveyéndolo.
#' Requiere del seteo en .Renviron de las variables de entorno siguientes:
#'
#' LOCAL_POSTGRES_USER
#' LOCAL_POSTGRES_PASSWORD
#' LOCAL_POSTGRES_HOST
#' LOCAL_POSTGRES_DB
#' AWS_POSTGRES_USER
#' AWS_POSTGRES_PASSWORD
#' AWS_POSTGRES_HOST
#' AWS_POSTGRES_DB
#' MEDINA_POSTGRES_USER
#' MEDINA_POSTGRES_PASSWORD
#' MEDINA_POSTGRES_HOST
#' MEDINA_POSTGRES_DB
#'
#' @param server local, aws o medina. medina es para accederlo remotamente
#' @param user usuario de la DB
#' @param password password de la db
#' @param host url donde está ubicado
#'
#' @example getConf(server = "aws")
#' @example getConf(server = "el-nombre-en-Renviron", host = "192.168.1.1)

# config.R

getConf <- function(server = "local",
                    user = Sys.getenv(paste(toupper(server), "POSTGRES_USER", sep = "_"), "postgres"),
                    password = Sys.getenv(paste(toupper(server), "POSTGRES_PASSWORD", sep = "_"), "XXX"),
                    host = Sys.getenv(paste(toupper(server), "POSTGRES_HOST", sep = "_"), ""),
                    db = Sys.getenv(paste(toupper(server), "POSTGRES_DB", sep = "_"), ""),
                    port = Sys.getenv(paste(toupper(server), "POSTGRES_PORT", sep = "_"), "")) {

  config <- list(
    user = user,
    password = password,
    host = if (host == "") stop("Host not provided") else host,
    db = if (db == "") stop("Database not provided") else db,
    port = if (port == "") port = "5432"
  )

  return(config)
}





