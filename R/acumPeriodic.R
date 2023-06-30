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
# require(kableExtra)
# require(numform)
# require(knitr)
# result = accumPeriodicMultiple(tickers = c("QQQ", "YPF","SPY", "TSLA", "AAPL", "MSFT", "XOM", "C", "NVDA", "KO", "INTC", "AMD", "WMT", "META", "JNJ"),
#                       amountInvest = 50,
#                       start = "2010-01-02",
#                       end = Sys.Date() + 1)
# result$amount = as.numeric(result$amount)
# result %>%
#   arrange(ticker) %>%
# mutate(
#   amountLabel =  format(round(amount,0), big.mark = ".") #  f_num(amount, digits = 2, retain.leading.zero = T, big.mark="."),
#   #deltaText = f_num(delta * 100, digits = 0, retain.leading.zero = T)
# ) %>%
#   select(ticker, amountLabel) %>%
#   kbl(
#     escape = FALSE,
#     booktabs = TRUE,
#     digits = 2,
#     col.names = c("Ticker", "Monto Final"),
#     align = "cr",
#     caption = " "
#   ) %>%
#   add_header_above(c("Cuanto obtengo si hubiese invertido 50 usd por mes desde 2010 en:" = 2)) %>%
#   #                    "Fecha" = 2,
#   #                    "Variación Precios" = 3)) %>%
#
#   kable_classic(full_width = F) %>%
#   add_footnote(c("Elaboración propia en base a precios NYSE y NASDAQ"),
#                notation = "symbol")



