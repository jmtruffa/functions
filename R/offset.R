### Calcula la variación a partir del primer elemento del df
### para cada uno de los datos
### Trabaja con formato wider o longer.
### Si es longer, default, espera en names el número de columan donde esta los nombres
### direction es para ver si lo hace para adelante o para atras (1 adelante, -1 hacia atrás)
### ideal para pasarle un df con lo que queremos comparar y ver como fue la evolución de cada una respecto al ppio.

## el código comentado es el de la versión original
# offset = function (df, direction = 1){
#   if (direction == 1) {
#     df = df %>% mutate_at(.vars = c(-1),
#                      .funs = list( ~ . / first(.) * 100))
#   } else if (direction == -1) {
#     df = df %>% mutate_at(.vars = c(-1),
#                           .funs = list(~ . / last(.) * 100))
#   }
#   df
# }


# offset = function (df, direction = 1, format = 'longer', names = 2){
#
#   if (format != 'longer') {
#
#     if (direction == 1) {
#       df = df %>% mutate_at(.vars = c(-1),
#                             .funs = list( ~ . / first(.) * 100))
#     } else if (direction == -1) {
#       df = df %>% mutate_at(.vars = c(-1),
#                             .funs = list(~ . / last(.) * 100))
#     }
#   } else {
#     if (direction == 1) {
#       df = df %>% group_by_at(names) %>% mutate_at(.vars = c(-1, -names),
#                                                .funs = list( ~ . / first(.) * 100))
#     } else if (direction == -1) {
#       df = df %>% group_by_at(names) %>% mutate_at(.vars = c(-1, -names),
#                                                .funs = list(~ . / last(.) * 100))
#     }
#   }
#   df
# }

offset = function (df, direction = 1, names = 2, pivot = 100){
  if (direction == 1) {
    df = df %>% group_by_at(names) %>% mutate_at(.vars = c(-1, -names),
                                                 .funs = list( ~ . / first(.) * pivot))
  } else if (direction == -1) {
    df = df %>% group_by_at(names) %>% mutate_at(.vars = c(-1, -names),
                                                 .funs = list(~ . / last(.) * pivot))
  }
  df
}
