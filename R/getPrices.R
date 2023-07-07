#' getPrices
#'
#' @details
#'
#' Obtiene el precio, dada una tasa, de un activo. Opera contra la API local de yields.
#' Pueden pasársele vectores. Fees y settlement no opera con vectores.
#'
#' @param letras Puede ser un vector conteniendo múltiples
#' @param settlementDate Fecha en formato "yyyy-mm-dd"
#' @param yield Puede ser vector.
#' @param initialFee Valor único a aplicar upfront. Se recicla
#' @param endingFee Valor único a aplicar al final (si se intenta estimar una venta posterio)
#' @param endpoint "price"
#' @examples getPrices("GD30D", "2023-07-13", 0.28, initialFee = 0.007515, endingFee = 0, endpoint = "yield")
#'
#' @return tibble with same data plus price & mduration. There are several other values that the API
#' returns but not implemented yet.



getPrices = function(letras, settlementDate, yield, initialFee = 0, endingFee = 0, endpoint = "price") {

  require(httr)
  require(tidyverse)

  initialFee = rep(initialFee, length(letras))
  endingFee = rep(endingFee, length(letras))
  prices = rep(0, length(letras))
  mduration = rep(0, length(letras))


  result = tibble(
    letras,
    prices,
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
              rate = result$yield[i],
              initialFee = result$initialFee[i],
              endingFee =  result$endingFee[i]
            )
    )

    result$prices[i] = as.data.frame(fromJSON(rawToChar(r$content)))$Price
    result$mduration[i] = as.data.frame(fromJSON(rawToChar(r$content)))$MDuration
    i=i+1
  }
  result
}
