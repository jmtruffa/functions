### Function to get yields from the local server running yields.go at 127.0.0.1:8080
### fees are recycled for all the bonds in the array
### settlmentDate has a default of today() + 2 based on calendar of bizdays. No holidays applied. Just weekends

getYields = function(letras, settlementDate = 0, precios, initialFee = 0, endingFee = 0) {

  require(httr)
  require(tidyverse)
  require(bizdays)
  cal <- create.calendar("Argentina/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
  if (settlementDate == 0) {
    settlementDate = adjust.previous(Sys.Date() + 2, cal)
  }
  #letras = c('S29A2', 'S31Y2', 'S30J2', 'S29L2', 'S31G2')
  letras = letras
  precios = precios
  # precios = c(97.574,
  #             94.1,
  #             90.6,
  #             87.34,
  #             8385)
  initialFee = rep(initialFee, length(letras))
  endingFee = rep(endingFee, length(letras))
  yields = rep(0, length(letras))
  
  
  result = tibble(
    letras,
    precios,
    initialFee,
    endingFee,
    yields
  )
  
  url = 'http://127.0.0.1:8080/yield'
  
  for (i in 1:length(letras)) {
  
      r = GET(url,
            query = list(
              ticker = result$letras[i],
              settlementDate = settlementDate,
              price = result$precios[i],
              initialFee = result$initialFee[i],
              endingFee =  result$endingFee[i]
            )
          )
      
    result$yields[i] = (rawToChar(r$content))
    i=i+1
  }
  result
}


test = getYields (letras = c('S29A2', 'S31Y2', 'S30J2', 'S29L2', 'S31G2'), 
                  precios = c(97.699,
                  94.22,
                  90.7,
                  87.4,
                  83.8),
                  initialFee = 0.0,
                  endingFee = 0.0)
