#########
# Function to get datasets from Alphacast.io
# uses alphacast key from .Renviron

getAlphacast = function (dataset){
  library(readr)
  library(httr)
  r = GET(
    url = paste0('https://charts.alphacast.io/api/datasets/', dataset ,'.csv'),
    authenticate(Sys.getenv('alphacast'), '')
  )
  read_csv(rawToChar(r$content))
}

getAlphacastDataSets = function() {
  r = httr::GET('https://charts.alphacast.io/api/datasets',
                httr::authenticate(Sys.getenv('alphacast'), ''))
  unique(jsonlite::fromJSON(rawToChar(r$content)))[, -5]
}

searchAlphacastDataSet = function(string, ds) {
  ds[grepl(string, ds$name, ignore.case = TRUE),]
}






