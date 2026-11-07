#' Normalización de un Atributo Numérico
#'
#' @description
#' Transforma una variable numérica al intervalo \eqn{[0, 1]}.
#'
#' @param x Vector numérico. No puede contener valores vacíos ni caracteres no numéricos.
#'
#' @return Un vector numérico con los datos acotados en el rango \eqn{[0, 1]}. Si la variable no tiene
#'  variabilidad (\eqn{\max(x) = \min(x)}), devuelve un vector de ceros.
#'
#' @details
#' Se aplica la siguiente formula para calcular el vector normalizado:
#' \deqn{x' = \frac{x - x_{\min}}{x_{\max} - x_{\min}}}
#'
#' @examples
#' normalizar(c(10, 20, 30, 40, 50))
#'
#' @export
normalizar <- function(x) {

  # --- Validaciones de Entrada ---
  if (!is.numeric(x)) {
    stop("La variable debe ser numérica.")
  }
  if (length(x) == 0) {
    stop("El vector no puede estar vacío.")
  }
  if (any(is.na(x))) {
    stop("La variable no puede contener valores NA.")
  }

  minimo <- min(x)
  maximo <- max(x)

  # Varianza Cero (todos los valores son iguales)
  if (maximo == minimo) {
    return(rep(0, length(x)))
  }

  x_normalizado <- (x - minimo) / (maximo - minimo)

  return(x_normalizado)
}


#' Estandarización de un Atributo Numérico
#'
#' @description
#' Centra y escala un atributo numérico para que tenga una media de 0 y una desviación estándar de 1.
#'
#' @param x Vector numérico continuo.
#'
#' @return Un vector numérico estandarizado.
#'
#' @details
#' Se utiliza la siguiente funcion para estandarizar el vector:
#' \deqn{x' = \frac{x - \mu}{\sigma}}
#' donde \eqn{\mu} representa la media aritmética del vector y \eqn{\sigma} su desviación estándar muestral,
#' calculada con la corrección de Bessel:
#' \deqn{\sigma = \sqrt{\frac{\sum_{i=1}^{n} (x_i - \mu)^2}{n - 1}}}
#'
#' @examples
#' estandarizar(c(10, 20, 30, 40, 50))
#'
#' @export
estandarizar <- function(x) {

  # --- Validaciones de Entrada ---
  if (!is.numeric(x)) {
    stop("La variable debe ser numérica.")
  }
  if (any(is.na(x))) {
    stop("La variable no puede contener valores NA.")
  }

  media <- sum(x) / length(x)
  desviacion <- sqrt(sum((x - media)^2) / (length(x) - 1))

  # Denominador cero: Desviación cero
  if (desviacion == 0) {
    return(rep(0, length(x)))
  }

  x_estandarizado <- (x - media) / desviacion

  return(x_estandarizado)
}


#' Transformación de Variables en un Dataset Completo
#'
#' @description
#' Recorre las columnas de un dataset y aplica la transformación seleccionada (\code{"normalizacion"} o
#' \code{"estandarizacion"}) sobre las variables numéricas, manteniendo las categóricas.
#'
#' @param dataset Objeto de tipo \code{data.frame}.
#' @param metodo Cadena de texto que indica el tipo de transformacion a aplicar: \code{"normalizacion"}
#'   o \code{"estandarizacion"}.
#'
#' @return Un \code{data.frame} con la misma dimensión que el de entrada, con sus variables numéricas
#'   tranformadas.
#'
#' @details
#' La función recorre secuencialmente cada columna del dataset, y si el atributo es numerico (\code{is.numeric()}),
#' le aplica el metodo seleccionado. Las columnas que no sean cuantitativas se ignoran.
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(18, 25, 45, 60),
#'   Ingresos = c(1200, 2500, 3800, 1900),
#'   Grupo = c("A", "B", "A", "B")
#' )
#' transformar_dataset(df, metodo = "normalizacion")
#' transformar_dataset(df, metodo = "estandarizacion")
#'
#' @export
transformar_dataset <- function(dataset, metodo) {

  # --- Validaciones Iniciales ---
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data.frame")
  }
  if (!(metodo %in% c("normalizacion", "estandarizacion"))) {
    stop("El método debe ser 'normalizacion' o 'estandarizacion'.")
  }

  resultado <- as.data.frame(dataset, stringsAsFactors = FALSE)

  # --- Procesamiento por Columnas ---
  for (nombre in colnames(resultado)) {

    # Procesar únicamente atributos numéricos
    if (is.numeric(resultado[[nombre]])) {

      if (metodo == "normalizacion") {
        resultado[[nombre]] <- normalizar(resultado[[nombre]])
      } else if (metodo == "estandarizacion") {
        resultado[[nombre]] <- estandarizar(resultado[[nombre]])
      }

    }
  }

  return(resultado)
}
