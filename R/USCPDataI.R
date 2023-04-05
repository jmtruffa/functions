downloadUSCPI = function(db = "") {

  require(stringr)
  require(RSQLite)
  require(readr)


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

  tmpFileName = "cu.data.1.AllItems"
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems"

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
  cpi = read_tsv(file.path(tmpPath, tmpFileName))
  ## este es Seasonally Adjusted cpi = cpi %>% filter(series_id == "CUSR0000SA0")
  cpi = cpi %>% filter(series_id == "CUUR0000SA0") ## este es sin ajuste.
  DBI::dbWriteTable(con, "USCPI", cpi, overwrite = TRUE)
  DBI::dbDisconnect(con)
  return(cpi)
}


getUSCPI = function(format = "", db= "") {

  require(stringr)
  require(RSQLite)
  require(DBI)
  require(lubridate)
  require(tidyr)
  require(dplyr)

  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/data1.sqlite3"
    } else {
      db = '~/data/data1.sqlite3'
    }
  }

  con = dbConnect(RSQLite::SQLite(), dbname = db)
  USCPI = DBI::dbReadTable(con, "USCPI")
  DBI::dbDisconnect(con)

  switch(
    format,
    daily = {
      USCPI = USCPI %>%
        mutate(
          date = make_date(year = year, month = substring(period,2), day = 01 )
        ) %>%
        select(-year, -period, -footnote_codes)
      USCPI = USCPI  %>%
        complete(
          date = seq.Date(min(date), ceiling_date(max(USCPI$date), unit = "month")-1, by="day")
        ) %>%
        fill(series_id, value)
    },
    monthly = {
      USCPI = as_tibble(USCPI %>%
        mutate(
          date = make_date(year = year, month = substring(period,2), day = 01 )
        ) %>%
        select(-year, -period, -footnote_codes) %>%
        relocate(date, series_id, value))

    },
    {
      #this is the unnamed or default case

    }
  )
  return(USCPI)
}


