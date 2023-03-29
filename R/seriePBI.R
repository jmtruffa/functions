downloadPBI = function(db = "", url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_22.xls") {

  require(stringr)
  require(RSQLite)
  require(readxl)
  require(DBI)
  require(tidyverse)

  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/data1.sqlite3"
      tmpPath = "~/Google Drive/Mi unidad/analisis financieros/temp"
    } else {
      db = '~/data/data1.sqlite3'
      tmpPath = "~/Downloads/temp"
    }
  }

  con = dbConnect(RSQLite::SQLite(), dbname = db)
  error = FALSE

  tmpFileName = "PBI.xls"

  set.seed(1973)
  if (file.exists(file.path(tmpPath, tmpFileName))) {
    tempN=round(runif(1)*10)
    file.copy(file.path(tmpPath, tmpFileName), file.path(tmpPath, paste0('tmp', tempN, tmpFileName)), overwrite = TRUE)
  }

  tryCatch(
    {
      download.file(url, destfile = file.path(tmpPath, tmpFileName))
    },
    error = function(e) {
      error <<- TRUE;
      e <<- e;
    }
  )


  if (!error) {
    ret = NULL

    ### PBI a precios Corrientes
    pbiCorriente = readxl::read_xls(file.path(tmpPath, tmpFileName),
                           sheet = "cuadro 8",
                           skip = 4)
    t = pbiCorriente[c(2:4, 6:13),]
    t = t(t) %>% as_tibble()
    colnames(t) = c(
      "PBICorriente",
      "impFOBCorriente",
      "ofertaGlobalCorriente",
      "demandaGlobalCorriente",
      "consumoPrivadoCorriente",
      "consumoPublicoCorriente",
      "exportacionesFOBCorriente",
      "formacionBrutaCapitalFijoCorriente",
      "variacionExistenciasCorriente",
      "objetosValiososCorriente",
      "discrepanciaEstadisticaCorriente"
    )
    t = t[-1,]

    t = t %>% drop_na(PBICorriente)

    t[,1:length(t)] = sapply(t[,1:length(t)], as.numeric) %>% as_tibble()

    a = t %>% filter(row_number() %% 5 == 0)

    t = t %>% filter(row_number() %% 5 != 0)


    DBI::dbWriteTable(con, "pbiCorrienteAnual", a, overwrite = TRUE)
    DBI::dbWriteTable(con, "pbiCorrienteTrimestral", t, overwrite = TRUE)

    ret$pbiCorrienteAnual = a
    ret$pbiCorrienteTrimestral = t

    ### PBI a precios constantes 2004

    pbiConstante = readxl::read_xls(file.path(tmpPath, tmpFileName),
                                    sheet = "cuadro 1",
                                    skip = 4)

    t = pbiConstante[c(2:4, 6:13),]
    t = t(t) %>% as_tibble()
    colnames(t) = c(
      "PBIConstante",
      "impFOBConstante",
      "ofertaGlobalConstante",
      "demandaGlobalConstante",
      "consumoPrivadoConstante",
      "consumoPublicoConstante",
      "exportacionesFOBConstante",
      "formacionBrutaCapitalFijoConstante",
      "variacionExistenciasConstante",
      "objetosValiososConstante",
      "discrepanciaEstadisticaConstante"
    )

    t = t[-1,]

    t = t %>% drop_na(PBIConstante)

    t[,1:length(t)] = sapply(t[,1:length(t)], as.numeric) %>% as_tibble()

    a = t %>% filter(row_number() %% 5 == 0)

    t = t %>% filter(row_number() %% 5 != 0)


    DBI::dbWriteTable(con, "pbiConstanteAnual", a, overwrite = TRUE)
    DBI::dbWriteTable(con, "pbiConstanteTrimestral", t, overwrite = TRUE)

    ret$pbiConstanteAnual = a
    ret$pbiConstanteTrimestral = t

    } else {
      ret(error)
    }

    DBI::dbDisconnect(con)
    return(ret)


}

getPBI = function(table = "pbiCorriente", format = "T", db= "") {

  require(RSQLite)
  require(DBI)



  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/data1.sqlite3"
    } else {
      db = '~/data/data1.sqlite3'
    }
  }

  con = dbConnect(RSQLite::SQLite(), dbname = db)



  switch(
    format,
    T = { ## trimestral. va como viene
      pbi = DBI::dbReadTable(con, paste0(table,"Trimestral"))
      pbi = stats::ts(pbi, start = c(2004, 1), frequency = 4) ## lo convierto a serie de tiempo en trimestres
      pbi
    },
    A = { ## anual. se toma el promedio cada cuatro
      pbi = DBI::dbReadTable(con, paste0(table,"Anual"))
      pbi = stats::ts(pbi, start = c(2004, 1), frequency = 1) ## lo convierto a serie de tiempo en años
    },
    {
      #this is the unnamed or default case

    }
  )

  DBI::dbDisconnect(con)
  return(pbi)
}


