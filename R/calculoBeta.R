#########
## Función que permite calcular el beta de un activo "ticker" contra otro "bench"
## Se calcula de manera sencilla como la pendiente de la regresión y el alpha (ord al origen) es el alpha del
## ticker o cartera.
beta = function(tickers, wts, bench, start, end) {
  library(tidyquant)
  library(timetk)
  library(tidyverse)

    desde = start
  hasta = end
  #wts = c(1)
  
  price_data <- tq_get(tickers,
                       from = desde,
                       to = hasta,
                       get = 'stock.prices')
  ret_data <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret")
  
  port_ret <- ret_data %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = ret,
                 weights = wts,
                 col_rename = 'port_ret',
                 geometric = FALSE)
  bench_price <- tq_get(bench,
                        from = desde,
                        to = hasta,
                        get = 'stock.prices')
  bench_ret <- bench_price %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "bench_ret")
  comb_ret <- left_join(port_ret,bench_ret, by = 'date')
  
  plot = comb_ret %>%
    ggplot(aes(x = bench_ret,
               y = port_ret)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = 'lm',
                se = FALSE) +
    theme_classic() +
    labs(x = 'Retornos Benchmark',
         y = "Retornos Activo",
         title = "Retorno Activo vs Retorno Benchmark") +
    scale_x_continuous(breaks = seq(-0.1,0.1,0.01),
                       labels = scales::percent) +
    scale_y_continuous(breaks = seq(-0.1,0.1,0.01),
                       labels = scales::percent)
  model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)
  model_alpha <- model$coefficients[1]
  
  model_beta <- model$coefficients[2]
  #cat("The portfolio alpha is", model_alpha, "and the portfolio beta is", model_beta)
  value = list('alpha' = model_alpha, 
                'beta' = model_beta,
                'graph' = plot)
  return(value)
}