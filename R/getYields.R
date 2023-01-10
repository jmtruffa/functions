### Function to get yields from the local server running yields.go at 127.0.0.1:8080
### fees are recycled for all the bonds in the array
### settlmentDate has a default of today() + 2 based on calendar of bizdays. No holidays applied. Just weekends
### endpoint: yield or apr. apr works on zerocoupon and returns the APR instead of effective.
###           yield returns effective yield.

getYields = function(letras, settlementDate, precios, initialFee = 0, endingFee = 0, endpoint = "yield") {

  require(httr)
  require(tidyverse)
  require(bizdays)
  cal <- create.calendar("Argentina/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
  # if (settlementDate == 0) {
  #   settlementDate = adjust.previous(Sys.Date() + 2, cal)
  # }
  initialFee = rep(initialFee, length(letras))
  endingFee = rep(endingFee, length(letras))
  yield = rep(0, length(letras))
  mduration = rep(0, length(letras))


  result = tibble(
    letras,
    precios,
    initialFee,
    endingFee,
    yield,
    mduration
  )

  url = paste0('http://127.0.0.1:8080/', endpoint)

  for (i in 1:length(letras)) {

      r = GET(url,
            query = list(
              ticker = result$letras[i],
              settlementDate = settlementDate[i],
              price = result$precios[i],
              initialFee = result$initialFee[i],
              endingFee =  result$endingFee[i]
            )
          )

    result$yield[i] = as.data.frame(fromJSON(rawToChar(r$content)))$Yield
    result$mduration[i] = as.data.frame(fromJSON(rawToChar(r$content)))$MDuration
    i=i+1
  }
  result
}


