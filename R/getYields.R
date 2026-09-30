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

getYields <- function (letras, settlementDate, precios, initialFee = 0, endingFee = 0,
                       endpoint = "yield", host = "http://127.0.0.1:8080/") {

  require(httr)
  require(tidyverse)
  require(jsonlite)

  initialFee = rep(initialFee, length(letras))
  endingFee = rep(endingFee, length(letras))

  yield = rep(0, length(letras))
  tna = rep(0, length(letras))
  tem = rep(0, length(letras))
  tDirecta = rep(NA_real_, length(letras))
  mduration = rep(0, length(letras))
  convexity = rep(0, length(letras))
  maturity = rep(NA_character_, length(letras))
  parity = rep(0, length(letras))
  techValue = rep(0, length(letras))
  residual = rep(0, length(letras))
  accrualDays = rep(0, length(letras))
  accruedInterest = rep(0, length(letras))
  coefFechaCalculo = rep(NA_character_, length(letras))
  coefIssue = rep(0, length(letras))
  coefUsed = rep(0, length(letras))
  currentCoupon = rep(0, length(letras))
  lastAmort = rep(0, length(letras))
  lastCoupon = rep(NA_character_, length(letras))

  result = tibble(
    letras, precios, initialFee, endingFee, yield,
    tna, tem, tDirecta, mduration, convexity, maturity, parity,
    techValue, residual, accrualDays, accruedInterest, coefFechaCalculo,
    coefIssue, coefUsed, currentCoupon, lastAmort, lastCoupon
  )

  url = paste0(host, endpoint)

  or_na = function(x, na) if (is.null(x) || length(x) == 0) na else x

  apiKey = Sys.getenv("YIELDS_API_KEY")

  for (i in seq_along(letras)) {

    r = GET(
      url,
      add_headers(`X-API-Key` = apiKey),
      query = list(
        ticker = result$letras[i],
        settlementDate = settlementDate[i],
        price = result$precios[i],
        initialFee = result$initialFee[i],
        endingFee = result$endingFee[i]
      )
    )

    respuesta = fromJSON(rawToChar(r$content))

    # The API returns null metrics for undefined cases (e.g. settlement on or
    # after maturity). Keep NA for that bond instead of failing the whole batch.
    if (status_code(r) >= 400 || !is.null(respuesta$error)) {
      warning(sprintf("%s (%s): HTTP %s %s", result$letras[i], settlementDate[i],
                      status_code(r), paste(or_na(respuesta$error, ""), collapse = " ")),
              call. = FALSE)
    }
    if (!is.null(respuesta$warning)) {
      warning(sprintf("%s (%s): %s", result$letras[i], settlementDate[i], respuesta$warning),
              call. = FALSE)
    }

    result$yield[i] = or_na(respuesta$Yield, NA_real_)
    result$tna[i] = or_na(respuesta$TNA, NA_real_)
    result$tem[i] = or_na(respuesta$TEM, NA_real_)
    result$tDirecta[i] = or_na(respuesta$TDirecta, NA_real_)
    result$mduration[i] = or_na(respuesta$MDuration, NA_real_)
    result$convexity[i] = or_na(respuesta$Convexity, NA_real_)
    result$maturity[i] = or_na(respuesta$Maturity, NA_character_)
    result$parity[i] = or_na(respuesta$Parity, NA_real_)
    result$techValue[i] = or_na(respuesta$TechnicalValue, NA_real_)
    result$residual[i] = or_na(respuesta$Residual, NA_real_)
    result$accrualDays[i] = or_na(respuesta$AccrualDays, NA_real_)
    result$accruedInterest[i] = or_na(respuesta$AccruedInterest, NA_real_)
    result$coefFechaCalculo[i] = or_na(respuesta$`Coef Fecha de Cálculo`, NA_character_)
    result$coefIssue[i] = or_na(respuesta$`Coef Issue`, NA_real_)
    result$coefUsed[i] = or_na(respuesta$`Coef Used`, NA_real_)
    result$currentCoupon[i] = or_na(respuesta$`CurrentCoupon: `, NA_real_)
    result$lastAmort[i] = or_na(respuesta$LastAmort, NA_real_)
    result$lastCoupon[i] = or_na(respuesta$LastCoupon, NA_character_)
  }

  result
}


