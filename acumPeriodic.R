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


acumPeriodicOLD = function(ticker = "QQQ",
                        amountInvest = 1,
                        start = "1999-03-31",
                        end = Sys.Date()) {

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

returns = Ra$Ra + 1
total = 0


for (i in 1:length(returns)){
  total = total + tail(cumprod(returns[i:length(returns)]),1) * amountInvest
}

return (total)

}


