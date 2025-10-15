curvaAR = function(from = "2020-09-15", to = Sys.Date(), comi = 0.0) {
  require(tidyverse)
  require(methodsPPI)
  library(bizdays)
  require(functions)

  if (from < "2020-09-15") {
    stop("No puede seleccionar una fecha anterior al canje. 2020-09-15")
  }
  business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
  settlement = "A-48HS"
  tickers = c(
    'AL29D',
    'AL30D',
    'AL35D',
    'AE38D',
    'AL41D',
    'GD29D',
    'GD30D',
    'GD35D',
    'GD38D',
    'GD41D',
    'GD46D'
  )
  type = rep("BONOS", length(tickers))
  methodsPPI::getPPILogin()
  curva = getPPIPriceHistoryMultiple3(token, ticker = tickers,
                                     type = type,
                                     from = from,
                                     to = to,
                                     settlement = settlement)
  curva = curva[[1]]
  curva$ticker = sapply(curva$ticker, str_sub, "1" ,"-2", USE.NAMES = FALSE)
  curva = cbind(curva, getYields(curva$ticker,
                                 settlementDate = as.character(bizdays::offset(curva$date, ifelse(settlement == "INMEDIATA", 0, 2), cal = business_calendar)),
                                 precios = curva$price,
                                 initialFee = comi,
                                 endpoint = 'yield'))
  curva %>% select(-precios, -letras)
}
