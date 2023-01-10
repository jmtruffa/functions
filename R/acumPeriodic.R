############# acumPeriodc
############# Calcula el retorno acumulado de haber puesto una inversión periódica durante n meses en un activo
############# Por defecto asume QQQ
############# Si accum = TRUE devuelve el df con los valores acumulados para cada período

acumPeriodic = function(ticker = "QQQ",
                        amountInvest = 1,
                        start = "1999-03-31",
                        end = Sys.Date(),
                        accum = FALSE
) {

  library(tidyquant)
  library(tidyverse)

  Px = ticker %>%
    tq_get(get = "stock.prices",
           from = start,
           to = end) %>%
    group_by(symbol)

  Ra = Px %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "Ra")

  returns = Ra[,2:3]+1 #data.frame(Ra$Ra + 1)
  res = returns %>% mutate(
    accum = Reduce(function(a,b) a * b + b * amountInvest, returns$Ra[-(1:1)], amountInvest, accumulate = TRUE)
  )

  if (accum == TRUE) {
    res = res[, -2]
  } else {
    res = res$accum[nrow(res):nrow(res)]
  }
  return(res)
}


############# acumPeriodcMultiple
############# Wrapper de accumPeriodic pero recibe un vector para calcular multiples
############# Devuelve un df con los tickers y sus valores.
############# Si accum = TRUE devuelve el df con los valores acumulados para cada período
accumPeriodicMultiple = function (tickers,
                                  amountInvest,
                                  start,
                                  end,
                                  accum = FALSE
)
{
  if (accum == TRUE) {
    full = data.frame(
      ticker = character(),
      date = numeric(),
      accum = numeric()
    )
    names = c('ticker', 'date', 'accum')
  } else {
    full = data.frame(
      ticker = character(),
      amount = numeric()
    )
    names = c('ticker', 'amount')
  }

  for (company in tickers) {
    r = acumPeriodic(company,
                     amountInvest = amountInvest,
                     start = start,
                     end = end,
                     accum = accum)
    r = cbind(rep(company,length(r)), r )
    full = rbind(full, r)
  }
  colnames(full) = names
  return(full)
}


# test = accumPeriodicMultiple(tickers = c('QQQ', 'SPY', 'INTC', 'MSFT', 'KO', 'X'),
#                              amountInvest = 150,
#                              start = '2001-01-01',
#                              end = '2021-12-31',
#                              accum = FALSE)





