downloadData = function(dataset,
                        destPath = "~/Google Drive/Mi unidad/analisis financieros/functions/data",
                        lastDate = Sys.Date()) {
  require(functions)
  require(tidyverse)
  require(readxl)
  require(DBI)
  require(bizdays)

  cal <- create.calendar('tmpCalendar', getFeriados(), weekdays = c('saturday','sunday'))
  tmpPath = '~/Google Drive/Mi unidad/analisis financieros/temp'
  dataset = tolower(dataset)
  switch(
    dataset,
    a3500 = {
      url = "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/com3500.xls"
      tmpFileName = 'a3500.xlsx'
      fileName = 'a3500.csv'
      skip = 3
      sheet = 'TCR diario y TCNPM'
      xls = TRUE
      fields = c(1,2)
      names = c("date", "a3500")
      format = "%d-%m-%y"
    },
    itcrm = {
      url = "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx"
      tmpFileName = 'itcrm.xlsx'
      fileName = 'itcrm.csv'
      skip = 1
      sheet = "ITCRM y bilaterales"
      xls = FALSE
      fields = seq(1, 16, 1)
      names = c("date", "ITCRM", "ITCRBBrasil", "ITCRBCanada", "ITCRBChile", "ITCRBEEUU",
                "ITCRBMexico", "ITCRMUruguay", "ITCRMChina", "ITCRMIndia", "ITCRMJapon", "ITCRMUK",
                "ITCRMSuiza", "ITCRMZonaEuro", "ITCRMVietname", "ITCRMSudamerica")
      format = "%d/%m/%y"
    },
    cer = {
      url = 'http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/diar_cer.xls'
      tmpFileName = 'CER.xls'
      fileName = 'CER.csv'
      xls = TRUE
      fields = c(1,2)
      skip = 26
      sheet = "Totales_diarios"
      names = c("date", "CER")
      format = "%d/%m/%Y"
      tabla = "cer"
    },
    uva = {
      url = 'http://www.bcra.gov.ar/Pdfs/PublicacionesEstadisticas/diar_uva.xls'
      tmpFileName = 'UVA.xlsx'
    },
    {
      #this is the unnamed or default case

    }
  )

  download = TRUE
  error = FALSE
  e = FALSE
  ### Verificar fecha
  if (file.exists(file.path(destPath, fileName))) {
    temp = read_csv(file.path(destPath,fileName))
    last = temp %>% tail(n=1) %>% pull(names[1])
    if (last >= bizdays::adjust.previous(lastDate, cal = "tmpCalendar")) {
      download = FALSE
    }
  }



  if (download) {

    ## Primero que haga un backup por las dudas.
    if (file.exists(file.path(tmpPath, tmpFileName))) {
      file.copy(file.path(tmpPath, tmpFileName), file.path(tmpPath, paste0('tmp', tmpFileName)), overwrite = TRUE)
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

  } else {
    return(temp)
  }
  print(paste0("Error: ",error))
  print(paste0("Msg: ",e))

  if (error) {
    return(e)
  } else {
    if (xls) {
      temp = readxl::read_xls(file.path(tmpPath, tmpFileName), sheet = sheet, skip = skip)
    } else {
      temp = readxl::read_xlsx(file.path(tmpPath, tmpFileName), sheet = sheet, skip = skip)
    }
    temp = temp %>% select(all_of(fields))
    temp = temp %>% drop_na()
    colnames(temp) = names
    temp$date = as.Date(temp$date, format = format)
    print(temp)
    print(destPath)
    print(file.path(destPath, fileName))
    write_csv(temp, file.path(destPath, fileName))
    return(temp)
  }

}

