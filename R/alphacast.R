#########
# Function to get datasets from Alphacast.io
# uses alphacast key from .Renviron
# validUntil = 0 will force re-download.
# directory = where to store the downloaded file. If there's a data directory in your working folders specify the absolute path here.
# I leave the default option of an absolute path of myself since that is my data/ directory.

getAlphacast = function(ds, validUntil = 0, directory = '/Users/juan/Google Drive/Mi unidad/analisis financieros/functions/data/') {
  fileName = paste0(directory, ds, '.csv')

  if (!file.exists(fileName) || (file.info(fileName)$ctime + (validUntil * 60) < Sys.time())) {
    download.file(paste0('https://api.alphacast.io/datasets/',ds,'/data?apiKey=', Sys.getenv("alphacast"),'&&format=csv' ),
                  fileName,
                  sep='',
                  mode = 'wb')
  }
  df = read.csv(fileName)
  df$Date = as.Date(df$Date)
  df
}


getAlphacastDataSets = function() {
  r = httr::GET('https://charts.alphacast.io/api/datasets',
                httr::authenticate(Sys.getenv('alphacast'), ''))
  unique(jsonlite::fromJSON(rawToChar(r$content)))[, -5]
}

searchAlphacastDataSet = function(string, ds) {
  ds[grepl(string, ds$name, ignore.case = TRUE),]
}






