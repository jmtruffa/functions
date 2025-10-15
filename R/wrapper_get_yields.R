#' Wrapper con logging para `getYields`
#'
#' Ejecuta `getYields(letras, ...)` sin modificar los argumentos (sin reciclar),
#' captura warnings/errores y devuelve un resultado estandarizado.
#'
#' @param letras Vector de tickers/letras.
#' @param settlementDate Vector o escalar con fechas de liquidación (char o Date).
#' @param precios Vector o escalar con precios.
#' @param initialFee Vector o escalar con comisión inicial.
#' @param endingFee Vector o escalar con comisión final.
#' @param endpoint Endpoint del servicio (por defecto `'yield'`).
#' @param use_global Lógico: si `TRUE`, usa `getYields` del Global Env.
#'
#' @return Lista con `ok` (lógico), `data` (tibble), `issues` (tibble o `NULL`) y `msg` (char).
#'
#' @examples
#' \dontrun{
#' out <- check_getYields(letras, settlementDate, precios, initialFee = 0)
#' if (!out$ok) message(out$msg) else head(out$data)
#' }
#' @export
check_getYields <- function(letras,
                            settlementDate,
                            precios,
                            initialFee = 0,
                            endingFee  = 0,
                            endpoint   = "yield",
                            use_global = TRUE) {
  # --- Validaciones (sin reciclar)
  n <- length(letras)
  if (inherits(settlementDate, "Date")) settlementDate <- as.character(settlementDate)

  must_be_1_or_n <- function(x, name) {
    if (!(length(x) %in% c(1L, n))) {
      stop(sprintf("%s debe tener longitud 1 o %d (recibí %d).", name, n, length(x)), call. = FALSE)
    }
  }
  must_be_1_or_n(settlementDate, "settlementDate")
  must_be_1_or_n(precios,        "precios")
  must_be_1_or_n(initialFee,     "initialFee")
  must_be_1_or_n(endingFee,      "endingFee")

  # --- Elegir getYields (GlobalEnv por defecto)
  gy <- if (use_global) {
    get("getYields", envir = .GlobalEnv, inherits = FALSE)
  } else {
    get("getYields", mode = "function")
  }

  # --- Capturar warnings sin interrumpir
  warn_buf <- character()
  w_handler <- function(w) { warn_buf <<- c(warn_buf, conditionMessage(w)); invokeRestart("muffleWarning") }

  # --- Trace simple para log
  safe_tb <- function(k = 6) {
    cs <- sys.calls(); if (!length(cs)) return("no stack")
    paste(vapply(tail(cs, k), function(cl) paste(deparse(cl, 120L), collapse = " "), ""), collapse = " | ")
  }

  # --- Llamada protegida (sin reciclar inputs)
  res <- withCallingHandlers(
    tryCatch(
      gy(
        letras         = letras,
        settlementDate = settlementDate,
        precios        = precios,
        initialFee     = initialFee,
        endingFee      = endingFee,
        endpoint       = endpoint
      ),
      error = function(e) {
        functions::log_msg(sprintf("getYields ERROR: %s | TB: %s", conditionMessage(e), safe_tb()), "ERROR")
        structure(list(error = TRUE, condition = e), class = "gy_error")
      }
    ),
    warning = w_handler
  )

  if (length(warn_buf)) {
    functions::log_msg(sprintf("getYields warnings: %s", paste(unique(warn_buf), collapse = " || ")), "WARN")
  }

  # --- Manejo de error
  if (inherits(res, "gy_error")) {
    return(list(ok = FALSE, data = NULL, issues = NULL,
                msg = sprintf("getYields falló: %s", conditionMessage(res$condition))))
  }

  # --- Chequeos mínimos del resultado
  if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
    msg <- "getYields devolvió un objeto vacío o no tabular."
    functions::log_msg(msg, "WARN")
    return(list(ok = FALSE, data = res, issues = NULL, msg = msg))
  }

  # --- Marcar NAs en columnas de rendimiento (si existen)
  yield_cols <- intersect(names(res), c("apr","yield","tna","tem","tea","rate","rendimiento"))
  if (length(yield_cols)) {
    bad <- res[Reduce(`|`, lapply(res[yield_cols], is.na)), , drop = FALSE]
    if (nrow(bad)) {
      msg <- sprintf("getYields con %d fila(s) con NA en rendimiento (%s).",
                     nrow(bad), paste(yield_cols, collapse = ", "))
      functions::log_msg(msg, "WARN")
      return(list(ok = FALSE, data = res, issues = bad, msg = msg))
    }
  }

  functions::log_msg("getYields ejecutada correctamente (sin NAs en rendimiento).", "INFO")
  list(ok = TRUE, data = res, issues = NULL, msg = "OK")
}
