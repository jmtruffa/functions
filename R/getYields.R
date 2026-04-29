#' getYields
#'
#' @details
#' Function to get yield-related metrics from the local server running `yields.go`
#' at `127.0.0.1:8080`.
#'
#' `initialFee` and `endingFee` are recycled across all bonds in `letras`.
#'
#' @param letras Character vector with one or more bond tickers.
#' @param settlementDate Settlement date in `"yyyy-mm-dd"` format. Can be a single value or a vector with the same length as `letras`.
#' @param precios Numeric vector with bond prices. Can be a single value or a vector with the same length as `letras`.
#' @param initialFee Numeric value or vector. Upfront fee applied to the initial cashflow. Defaults to `0`.
#' @param endingFee Numeric value or vector. Exit fee applied to the final cashflow, useful when estimating a later sale. Defaults to `0`.
#' @param endpoint Character. API endpoint to call. Usually `"yield"` or `"apr"`.
#' @param host Character. Host where the local API is running. Defaults to `"http://127.0.0.1:8080/"`.
#'
#' @return
#' A tibble with the input data plus the metrics returned by the API, including:
#' `yield`, `tna`, `tem`, `tDirecta`, `mduration`, `convexity`, `maturity`,
#' `parity`, `techValue`, `residual`, `accrualDays`, `accruedInterest`,
#' `coefFechaCalculo`, `coefIssue`, `coefUsed`, `currentCoupon`, `lastAmort`,
#' and `lastCoupon`.
#'
#' @examples
#' \dontrun{
#' getYields(
#'   letras = "GD30D",
#'   settlementDate = "2023-07-13",
#'   precios = 32.3,
#'   initialFee = 0.007515,
#'   endingFee = 0,
#'   endpoint = "yield"
#' )
#' }
#'
#' @export

getYields = function(letras, settlementDate, precios, initialFee = 0, endingFee = 0, endpoint = "yield", host = 'http://127.0.0.1:8080/') {

  require(httr)
  require(tidyverse)
  require(functions)
  require(jsonlite)

  initialFee = rep(initialFee, length(letras))
  endingFee = rep(endingFee, length(letras))

  yield = rep(0, length(letras))
  tna = rep(0, length(letras))
  tem = rep(0, length(letras))
  tDirecta = rep(NA, length(letras))

  mduration = rep(0, length(letras))
  convexity = rep(0, length(letras))

  maturity = rep(NA, length(letras))
  parity = rep(0, length(letras))
  techValue = rep(0, length(letras))
  residual = rep(0, length(letras))

  accrualDays = rep(0, length(letras))
  accruedInterest = rep(0, length(letras))

  coefFechaCalculo = rep(NA, length(letras))
  coefIssue = rep(0, length(letras))
  coefUsed = rep(0, length(letras))

  currentCoupon = rep(0, length(letras))
  lastAmort = rep(0, length(letras))
  lastCoupon = rep(NA, length(letras))

  result = tibble(
    letras,
    precios,
    initialFee,
    endingFee,
    yield,
    tna,
    tem,
    tDirecta,
    mduration,
    convexity,
    maturity,
    parity,
    techValue,
    residual,
    accrualDays,
    accruedInterest,
    coefFechaCalculo,
    coefIssue,
    coefUsed,
    currentCoupon,
    lastAmort,
    lastCoupon
  )

  url = paste0(host, endpoint)

  for (i in seq_along(letras)) {


    r = GET(url,
            query = list(
              ticker = result$letras[i],
              settlementDate = settlementDate[i],
              price = result$precios[i],
              initialFee = result$initialFee[i],
              endingFee = result$endingFee[i]
            )
    )

    respuesta = fromJSON(rawToChar(r$content))

    result$yield[i] = respuesta$Yield
    result$tna[i] = respuesta$TNA
    result$tem[i] = respuesta$TEM
    result$tDirecta[i] = ifelse(is.null(respuesta$TDirecta), NA, respuesta$TDirecta)

    result$mduration[i] = respuesta$MDuration
    result$convexity[i] = respuesta$Convexity

    result$maturity[i] = respuesta$Maturity
    result$parity[i] = respuesta$Parity
    result$techValue[i] = respuesta$TechnicalValue
    result$residual[i] = respuesta$Residual

    result$accrualDays[i] = respuesta$AccrualDays
    result$accruedInterest[i] = respuesta$AccruedInterest

    result$coefFechaCalculo[i] = respuesta$`Coef Fecha de Cálculo`
    result$coefIssue[i] = respuesta$`Coef Issue`
    result$coefUsed[i] = respuesta$`Coef Used`

    result$currentCoupon[i] = respuesta$`CurrentCoupon: `
    result$lastAmort[i] = respuesta$LastAmort
    result$lastCoupon[i] = respuesta$LastCoupon
  }

  result
}


