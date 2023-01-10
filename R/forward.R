forward = function(df, cutoff) {
  cbPalette <- c("#9CD6F9", "#7ACAFA", "#4CAAE2", "#4CAAE2", "#235DBC", "#1C4993", "#14366D", "#0C1F3E", "#C6D6EE",
                 "#CAD1DC", "#ACB0B8", "#939599", "#757679", "#404042")
  business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
  ## ajusta la fecha por si se puso de parámetro una fecha inhábil
  cutoff = bizdays::adjust.next(cutoff, business_calendar)

  vtos = tibble(ticker = c('X18S3', 'X23N2', 'X16D2', 'X21A3', 'X20E3', 'X16J3',  'X17F3', 'X19Y3',
                           'TX23', 'T2X3', 'TC23' , 'TX24', 'T2X4', 'TX26', 'TX28'),
                vto = as.Date(c("2023-09-18", "2022-11-23", "2022-12-16", "2023-04-21", "2023-01-20","2023-06-16", "2023-02-17",  "2023-05-19",
                                "2023-03-25", "2023-08-13", "2023-03-06",  "2024-03-25", "2024-07-26", "2026-11-09",  "2028-11-09")))


  tabla = df %>%
    select(-c(volume, openingPrice, max, min, letras, precios, initialFee, endingFee)) %>%
    group_by(ticker) %>%
    #do(tail(., n=1)) %>%  %>%
    left_join(vtos) %>%
    mutate(
      dias = as.numeric(vto - date)
    ) %>%
    ungroup() %>%
    arrange(date) %>%
    group_by(ticker) %>%
    filter(date == cutoff) %>%
    #do(tail(., n=1)) %>%
    ungroup() %>%
    arrange(dias) %>%
    mutate(
      exp = dias / 365 ,
      exp2 = lag(dias) / 365,
      forward = ( ( ( 1 + yield ) ^ ( exp ) / ( 1 + lag(yield)) ^ ( exp2 ) ) ) ^ ( 1 / (exp - exp2) )   - 1,
      text = paste0(ticker, "/", lag(ticker))
    )  %>%
    select(- exp, -exp2)

  grafo = tabla %>%
    ggplot(aes(x=mduration, y=forward, label = text)) +
    geom_point(aes(color = ticker)) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
    labs(title = "Curvas Forward",
         subtitle = paste0("Fecha: ", cutoff),
         y = 'TEA',
         x = 'Modified Duration',
         caption = "Elaboración propia en base a precios de mercado (BYMA)")+
    theme_tq() +
    theme(legend.position = "none") +
    scale_color_manual(name = "", values = cbPalette) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank()) +
    scale_y_continuous(breaks = breaks_extended(10),
                       labels = scales::percent_format(accuracy = 0.01))  +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1,
                                                      decimal.mark = '.')) +
    ggrepel::geom_text_repel(position = "identity")

  ret = list(tabla, grafo)

}
