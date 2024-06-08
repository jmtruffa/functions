#' getFX
#'
#' Función que combina getPPIDLR y ccl de la tabla postgres en un df y calcula las variaciones diarias y semanales
#'
#' @param from Fecha Inicio. Mínima 2015-05-27
#' @param to hasta donde
#'
#' @returns Una tibble con la serie.
#'
#' @examples getFX(from = "2020-09-14", to = Suys.Date(), settle = "t+0") -> Devuelve la tibble
#'

# getFX = function(from = NULL, to = Sys.Date(), settle = "t+0", server = "local", port = 5432) {

#   require(methodsPPI)
#   cal = create.calendar('tmpCalendar', getFeriados(), weekdays = c('saturday','sunday'))
#   dlr = methodsPPI::getPPIDLR(from = from, to = to, settle = settle)[[1]]
#   ccl = functions::dbGetTable(table = "ccl", server = server, port = port)


#   ## tengo que calcular todos los viernes previos

#   viernesPrevio = ceiling_date(Sys.Date() - weeks(1), unit = "week") - 2
#   ## chequear si fue feriado
#   feriado = TRUE
#   while (feriado) {
#     if (!(viernesPrevio %in% functions::getFeriados())) {
#       break
#     }
#     viernesPrevio = viernesPrevio - days(1)
#   }
#   ## Acá o calculo el lag entre la fecha de hoy y la que surgió de viernesPrevio
#   ## o busco la fecha de los viernes previos y la pego con left_join

#   fx=left_join(dlr, ccl, join_by(date == date)) %>%
#     mutate(
#       across(-c(date, Canje), ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}"),
#       across(c(mepAL, mepGD, cclGD, ccl3, ccl), ~ (. / lag(., 5) - 1) * 100, .names = "varS_{.col}")
#     )


# }
# from = "2023-11-30"
# to = Sys.Date()
# server = "medina"
# port = 19293
# settle = "t+0"
#
# dlr = methodsPPI::getPPIDLR(from = from, to = to, settle = settle)[[1]]
# ccl = functions::dbGetTable(table = "ccl", server = server, port = port)
#
# fx=left_join(dlr, ccl, join_by(date == date)) %>%
#       mutate(
#         across(-c(date, Canje), ~ (. / lag(.) - 1) * 100, .names = "varD_{.col}"),
#         across(c(mepAL, mepGD, cclGD, ccl3, ccl), ~ (. / lag(., 5) - 1) * 100, .names = "varS_{.col}")
#       )
#
# fx %>% tail()
# fx %>% view()




# ### Variaciones mensuales
# fx %>%
#   group_by(mes = format(date, "%Y-%m")) %>%
#   slice_max(date) %>%
#   ungroup() %>%
#   select(Fecha = date, mepAL, ccl3) %>%
#   mutate(
#     across(c(mepAL, ccl3), ~ (. / lag(.) - 1) * 100, .names = "varMen_{.col}")
#   ) %>%
#   mutate(
#     `var MEP` = paste0(sprintf("%.2f%%", varMen_mepAL)),
#     `var CCL` = paste0(sprintf("%.2f%%", varMen_ccl3))
#   ) %>%
#   select(-c(4:5)) %>%
#   rename(MEP = mepAL, CCL = ccl3) %>%
#   slice(-1) %>%
#   as.data.frame() %>%
#   kableExtra::kbl(caption = 'Cierres Mensuales MEP y CCL', digits = c(0, 2, 2, 2, 2), align = "crrrr") %>%
#   row_spec(0, align = "c") %>%
#   kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"),
#                             full_width = F, position = "center")
#
