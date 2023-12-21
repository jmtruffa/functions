#' getTableP
#'
#' Función para traer una tabla desde el servidor postgresQL
#'
#' @param host IP del servidor postgreSQL
#' @param user Usuario de la base de datos
#' @param password Password de la base de datos en el servidor
#' @param port Puerto del servidor
#' @param dbname Base de Datos. Default='data'
#' @param table Acá hay que poner alguna. Controla que no esté vacío

dbGetTable = function(

    table,
    host = Sys.getenv("POSTGRES_HOST"),
    port = 5432,
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD")
  ) {

  require(RPostgreSQL)
  require(dplyr)
  require(DBI)


  if (is.null(table)) {

    print("putamadre")

    return (warning("No se indicó nombre de tabla a devolver"))

  }

  con <- dbConnect(PostgreSQL(), host = host, port = port, dbname = dbname, user = user, password = password)

  # Verificamos que la tabla exista
  if (!dbExistsTable(con, table)) {
    dbDisconnect(con)
    return(warning(paste("La tabla", table, "no existe en la base de datos.")))
  }

  query <- paste0('SELECT * FROM "', table,'"')
  df = dbGetQuery(con, query)
  dbDisconnect(con)

  return (as_tibble(df))

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
    host = Sys.getenv("POSTGRES_HOST"),
    port = 5432,
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD")
) {
  require(RPostgreSQL)
  require(dplyr)
  require(DBI)
  # obtain user+pass from environment variables

  con <- dbConnect(PostgreSQL(), host = host, port = port, dbname = dbname, user = user, password = password)
  retQuery = dbGetQuery(con, query)
  dbDisconnect(con)
  return (retQuery)

}


#' dbWriteDF
#'
#' Wraper de dbWriteTable del paquete DBI pero que asume las conexiones de mi servidor.
#'
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
    host = Sys.getenv("POSTGRES_HOST"),
    port = 5432,
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD")
) {

  if (is.null(table) || is.null(df)) {
    warning("Both 'table' and 'df' must be provided.")
    return(NULL)
  }



  con = dbConnectP(host, port, dbname)
  DBI::dbWriteTable(con, table, df)
  dbDisconnect(con)

}



#' dbConnectP
#'
#' Wrapper de dbConnect pero con los parámetros de servidor, puerto, user y password ya establecidos.
#' @param host IP del servidor postgreSQL
#' @param user Usuario de la base de datos
#' @param password Password de la base de datos en el servidor
#' @param port Puerto del servidor
#' @param dbname Base de Datos. Default='data'
#'
#' @examples con <- dbConnectP()
#'
dbConnectP = function(
    host = Sys.getenv("POSTGRES_HOST"),
    port = 5432,
    dbname = Sys.getenv("POSTGRES_DB"),
    user = Sys.getenv("POSTGRES_USER"),
    password = Sys.getenv("POSTGRES_PASSWORD")
  ) {
  require(RPostgreSQL)
  require(DBI)


  # Connect to the PostgreSQL server
  con <- dbConnect(PostgreSQL(), host = host, port = port, dbname = dbname, user = user, password = password)

  return(con)
}





