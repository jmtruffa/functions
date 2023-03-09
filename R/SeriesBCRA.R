downloadSeriesBCRA = function(db = "") {
  
  require(stringr)
  require(RSQLite)
  require(readxl)
  
  
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/GoogleDrive/Mi unidad/data/data1.sqlite3"
      tmpPath = "~/Google Drive/Mi unidad/analisis financieros/temp"
    } else {
      db = '/data/data1.sqlite3'
      tmpPath = "~/Downloads/temp"
    }
  }
  
  con = dbConnect(RSQLite::SQLite(), dbname = db)
  error = FALSE
  
  tmpFileName = "series.xls"
  url = "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/series.xlsm"
  
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
    
    ### base monetaria
    
    bm = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                           sheet = "BASE MONETARIA",
                           skip = 8)
    bm = bm %>% 
      select(c(1, 3:15, 17:23,25:32))
    colnames(bm) = c(
      "date",
      "vdFeTotal",
      "vdFeComSP",
      "vdFeComTN",
      "vdFeOtrasOpTNAT",
      "vdFeOtrasOpTNTU",
      "vdFeOtrasOpTNResto",
      "vdFePasesLeliqPases",
      "vdFePasesLeliqLeliqNotaliq",
      "vdFePasesLeliqRedescuentos",
      "vdFePasesLeliqIntereses",
      "vdFeLebacNobac",
      "vdFeRescateCuasi",
      "vdFeOtros",
      "vdBMCMBilletesPublico",
      "vdBMCMBilletesEntidades",
      "vdBMCMBilletesChequesCan",
      "vdBMCtaCteEnBCRA",
      "vdBMTotalsinCuasi",
      "vdBMMasCuasiCuasimonedas",
      "vdVBMTotal",
      "sdBMCMBilletesPublico",
      "sdBMCMBilletesEntidades",
      "sdBMCMBilletesChequesCan",
      "sdBMCtaCteEnBCRA",
      "sdBMTotalsinCuasi",
      "sdBMMasCuasiCuasimonedas",
      "sdVBMTotal",
      "tipoSerie"
    )
    #bm = bm %>% filter(
    #   tipoSerie == "D"
    # )  %>% 
    #   select(-tipoSerie)
    
    DBI::dbWriteTable(con, "bmBCRA", bm, overwrite = TRUE)
    
    ret$bm = bm
    
    #### reservas
    
    reservas = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                           sheet = "RESERVAS",
                           skip = 8)
    reservas = reservas %>% 
      select(c(
        1,3:5, 7:12,14,16:17
      ))
    colnames(reservas) = c(
      "date",
      "stockTotal",
      "stockOroColPlazoOtros",
      "stockDivisasPasePasivoUSDExterior",
      "vdReservasIntl",
      "vdFeCompraDivisas",
      "vdFeOrgIntl",
      "vdFeOtrasOpSP",
      "vdFeEfecMinimo",
      "vdFeOtros",
      "AsigDEGs",
      "TC",
      "tipoSerie"
    )
    # reservas = reservas %>% filter(
    #   tipoSerie == "D"
    # )  %>% 
    #   select(-tipoSerie)
    
    DBI::dbWriteTable(con, "reservas", reservas, overwrite = TRUE)
    
    ret$reservas = reservas
    
    ### depositos
    
    depositos = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                                 sheet = "DEPOSITOS",
                                 skip = 8)
    
    depositos = depositos %>% select(
      c(1:21, 23:24, 26:27, 29:30)
    )
    
    colnames(depositos) = c(
      "date",
      "ptCtaCte",
      "ptCA",
      "ptPFNoAjust",
      "ptPFAjustCERUVA",
      "ptOtros",
      "ptCedrosCER",
      "ptTotalDepositos",
      "ptBodenContabilizado",
      "ptTotal",
      "pSPPesosCtaCte",
      "pSPPesosCA",
      "pSPPesosPFNoAjust",
      "pSPPesosPFAjustCERUVA",
      "pSPPesosOtros",
      "pSPPesosCedrosCER",
      "pSPPesosTotalDepositos",
      "pSPPesosBodenContabilizado",
      "pSPPesosTotal",
      "depositosDolaresExprPesosTotal",
      "depositosDolaresExprPesosSPrivado",
      "depositosTotales",
      "depositosTotalesSectorPrivado",
      "depositosDolaresExprDolaresTotal",
      "depositosDolaresExprDolaresSPrivado",
      "M2",
      "tipoSerie"
    )
    
    DBI::dbWriteTable(con, "depositos", depositos, overwrite = TRUE)
    
    ret$depositos = depositos
    
    ### Prestamos
    
    prestamos = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                                  sheet = "PRESTAMOS",
                                  skip = 8)
    
    prestamos = prestamos %>% select(
      c(1:17, 19, 21:22)
    )
    
    colnames(prestamos) = c(
      "date",
      "prestamosSPPesosAdelantos",
      "prestamosSPPesosDocumentos",
      "prestamosSPPesosHipotecarios",
      "prestamosSPPesosPrendarios",
      "prestamosSPPesosPersonales",
      "prestamosSPPesosTarjetas",
      "prestamosSPPesosOtros",
      "prestamosSPPesosTotal",
      "prestamosSPDolaresAdelantos",
      "prestamosSPDolaresDocumentos",
      "prestamosSPDolaresHipotecarios",
      "prestamosSPDolaresPrendarios",
      "prestamosSPDolaresPersonales",
      "prestamosSPDolaresTarjetas",
      "prestamosSPDolaresOtros",
      "prestamosSPDolaresTotal",
      "prestamosSPMillonesPesosDolares",
      "prestamosSPPesosMasDolares",
      "tipoSerie"
    )
    
    DBI::dbWriteTable(con, "prestamos", prestamos, overwrite = TRUE)
    
    ret$prestamos = prestamos
    
    ### Tasa de Mercado
    
    tasas = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                                  sheet = "TASAS DE MERCADO",
                                  skip = 8)
    
    tasas = tasas %>% select(
      c(1:22)
    )
    
    colnames(tasas) = c(
      "date",
      "PF30.44DiasPesosTotalGeneralTNA",
      "PF30.44DiasPesosHastaCienmilTNA",
      "PF30.44DiasPesosHastaCienmilTEA",
      "PF30.44DiasPesosMasUnmillonTNA",
      "PF30.44DiasDolaresTotalGeneralTNA",
      "PF30.44DiasDolaresHastaCienmilTNA",
      "PF30.44DiasDolaresMasUnmillonTNA",
      "badlarPesosTotalTNA",
      "badlarPesosTotalBancosPrivadosTNA",
      "badlarPesosTotalBancosPrivadosTEA",
      "TM20PesosTotalTNA",
      "TM20PesosBancoprivadosTNA",
      "TM20PesosBancoprivadosTEA",
      "prestamosPersonalesPesosTotalTNA",
      "adelantosPesosTotalTNA",
      "callPesosEntreprivadosTasaTNA",
      "callPesosEntreprivadosMontoMillones",
      "callPesosTotalTasaTNA",
      "callPesosTotalMontoMillones",
      "pasesEntreTerceros1DiaTNA",
      "pasesEntreTercerosMontoMillones"
    )  
    
    DBI::dbWriteTable(con, "tasas", tasas, overwrite = TRUE)
    
    ret$tasas = tasas
    
    ### Instrumentos BCRA
    
    instrBCRA = readxl::read_xlsx(file.path(tmpPath, tmpFileName),
                                  sheet = "INSTRUMENTOS DEL BCRA",
                                  skip = 8,
                                  col_types = c(
                                    "date",
                                    rep("numeric", 44)
                                  ))
    
    # instrBCRA = instrBCRA %>% select(
    #   c(1:45)
    # )
    
    colnames(instrBCRA) = c(
      "date",
      "saldosPasesPasivosPesosTotal",
      "saldosPasesPasivosPesosFCI",
      "saldosPasesActivosPesos",
      "saldosLeliqNotaliq",
      "saldosLebacNobacPesosLegarLeminTotal",
      "saldosLebacNobacPesosLegarLeminEntFinancieras",
      "saldosLebacDolaresLediv",
      "saldosNocom",
      "tasaPolMonTNA",
      "tasaPolMonTEA",
      "tasaPasePesosPasivo1Dia",
      "tasaPasePesosPasivo7Dias",
      "tasaPasePesosActivo1Dia",
      "tasaPasePesosActivo7Dia",
      "tasaLebacPesosLeliq1M",
      "tasaLebacPesosLeliq2M",
      "tasaLebacPesosLeliq3M",
      "tasaLebacPesosLeliq4M",
      "tasaLebacPesosLeliq5M",
      "tasaLebacPesosLeliq6M",
      "tasaLebacPesosLeliq7M",
      "tasaLebacPesosLeliq8M",
      "tasaLebacPesosLeliq9M",
      "tasaLebacPesosLeliq10M",
      "tasaLebacPesosLeliq11M",
      "tasaLebacPesosLeliq12M",
      "tasaLebacPesosLeliq18M",
      "tasaLebacPesosLeliq24M",
      "tasaPesosCER6M",
      "tasaPesosCER12M",
      "tasaPesosCER18M",
      "tasaPesosCER24M",
      "tasaLebacDolar1MLiquidablePesos",
      "tasaLebacDolar6MLiquidablePesos",
      "tasaLebacDolar12MLiquidablePesos",
      "tasaLebacDolar1MLiquidableDolar",
      "tasaLebacDolar3MLiquidableDolar",
      "tasaLebacDolar6MLiquidableDolar",
      "tasaLebacDolar12MLiquidableDolar",
      "tasaNobacPesosVariableBadlarBcoPriv9M",
      "tasaNobacPesosVariableBadlarBcoPriv1A",
      "tasaNobacPesosVariableBadlarTotal2A",
      "tasaNobacPesosVariableBadlarBcoPriv2A",
      "tasaNotaliqPesosVariableTasaPolMon190d"
    )
    
    DBI::dbWriteTable(con, "instrBCRA", instrBCRA, overwrite = TRUE)
    
    ret$instrBCRA = instrBCRA
       
    } else {
    
    ret = (error)
  }
  
  DBI::dbDisconnect(con)
  return(ret)
}


getSeriesBCRA = function(table = "bmBCRA", format = "D", db= "") {
  
  require(RSQLite)
  require(DBI)
  require(lubridate)
  require(tidyr)
  require(dplyr)
  
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/GoogleDrive/Mi unidad/data/data1.sqlite3"
    } else {
      db = '/data/data1.sqlite3'
    }
  }
  
  con = dbConnect(RSQLite::SQLite(), dbname = db)
  df = DBI::dbReadTable(con, table)
  DBI::dbDisconnect(con)
  
  df$date = as.Date(as.POSIXct(df$date,origin = "1970-01-01"))
  
  
  
  return(as_tibble(
    df %>%
      {
        if("tipoSerie" %in% names(.)) filter (., tipoSerie == stringr::str_to_upper(format)) else .
      }
  ))
}
