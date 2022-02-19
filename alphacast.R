#########
# Function to get datasets from Alphacast.io
# uses alphacast key from .Renviron
# validUntil = 0 will force re-download.

getAlphacast = function(ds, validUntil = 0) {
  download = FALSE
  fileName = paste0('/Volumes/GoogleDrive/Mi unidad/analisis financieros/functions/data/', ds, '.csv')

  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    download = TRUE
  }

  if (download == TRUE) {
    download.file(paste0('https://api.alphacast.io/datasets/',ds,'/data?apiKey=', Sys.getenv("alphacast"),'&&format=csv' ),
                  fileName,
                  sep='',
                  mode = 'wb')
  }
  read.csv(fileName)
}

getAlphacastDataSets = function() {
  r = httr::GET('https://charts.alphacast.io/api/datasets',
                httr::authenticate(Sys.getenv('alphacast'), ''))
  unique(jsonlite::fromJSON(rawToChar(r$content)))[, -5]
}

searchAlphacastDataSet = function(string, ds) {
  ds[grepl(string, ds$name, ignore.case = TRUE),]
}






