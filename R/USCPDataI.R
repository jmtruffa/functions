downloadUSCPI = function(db = "") {

  require(stringr)
  require(RSQLite)
  require(readr)
  require(tidyverse)


  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/economicData.sqlite3"
      tmpPath = "~/Google Drive/Mi unidad/analisis financieros/temp"
    } else {
      db = '~/data/economicData.sqlite3'
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
      download.file(url, destfile = file.path(tmpPath, tmpFileName), method = "curl")
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

#' @title getUSCPI
#' @description Wrapper to get the US CPI data from local database. It can return daily or monthly data.
#' @param format "daily" or "monthly". Default is "" which returns the raw data.
#' @param db "" for Postgres or "sqlite" for local sqlite database.
#' @return tibble with the CPI data.
#' @export
getUSCPI = function(format = "", db= "", ...) {

  require(stringr)
  require(RSQLite)
  require(DBI)
  require(lubridate)
  require(tidyr)
  require(dplyr)

  if (db == "") {## postgress
    USCPI = functions::dbGetTable(table = "USCPI", ...) %>% filter(period != "M13")
    #print("Postgres")
  } else if (db == "sqlite") {
    db = "~/data/economicData.sqlite3"
    con = dbConnect(RSQLite::SQLite(), dbname = db)
    USCPI = DBI::dbReadTable(con, "USCPI")
    DBI::dbDisconnect(con)
    print("SQLite. OJO")
  }

  switch(
    format,
    daily = {
      USCPI = USCPI %>%
        mutate(
          date = make_date(year = year, month = substring(period,2), day = 01 )
        ) %>%
        select(-year, -period, -footnote_codes) %>%
        drop_na()
      USCPI = USCPI  %>%
        drop_na() %>%
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


