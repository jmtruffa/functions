#' getYieds
#'
#' @details Function to get yields from the local server running yields.go at 127.0.0.1:8080.
#' Fees are recycled for all the bonds in the array.
#' @param letras Puede ser un vector conteniendo múltiples
#' @param settlementDate Fecha en formato "yyyy-mm-dd"
#' @param precios Puede ser vector.
#' @param initialFee Valor único a aplicar upfront. Se recicla
#' @param endingFee Valor único a aplicar al final (si se intenta estimar una venta posterio)
#' @param endpoint "yield" o "apr". Dependiendo a qué endpoint del servidor quiera pegársele.
#'
#' @examples getYields("GD30D", "2023-07-13", 32.3, initialFee = 0.007515, endingFee = 0, endpoint = "yield")
#'
#' @return tibble with same data plus yield & duration. There are several other values that the API
#' returns like maturity of the bond, parity, tech value & residual

getYields = function(letras, settlementDate, precios, initialFee = 0, endingFee = 0, endpoint = "yield") {

  require(httr)
  require(tidyverse)
  require(functions)

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
    ## nuevo release con mas data
    result$maturity[i] = as.data.frame(fromJSON(rawToChar(r$content)))$Maturity
    result$parity[i] = as.data.frame(fromJSON(rawToChar(r$content)))$Parity
    result$techValue[i] = as.data.frame(fromJSON(rawToChar(r$content)))$TechnicalValue
    result$residual[i] = as.data.frame(fromJSON(rawToChar(r$content)))$Residual
    i=i+1
  }
  result
}


