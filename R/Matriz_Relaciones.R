#' Cálculo del Coeficiente de Correlación
#'
#' @description
#' Se calcula el coeficiente de correlacion de Pearson entre dos variables cuantitativas continuas.
#'
#' @param x Vector numérico continuo.
#' @param y Vector numérico continuo de la misma longitud que \code{x}.
#'
#' @return Un valor numérico en el intervalo \eqn{[-1, 1]}.
#'   Devuelve \code{NA} si alguna de las variables carece de variabilidad.
#'
#' @details
#' La fórmula implementada es:
#' \deqn{r_{xy} = \frac{\sum_{i=1}^{n} (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^{n} (x_i - \bar{x})^2} \sqrt{\sum_{i=1}^{n} (y_i - \bar{y})^2}}}
#' donde \eqn{\bar{x}} y \eqn{\bar{y}} representan las medias aritméticas de cada vector.
#'
#' @examples
#' x <- c(10, 20, 30, 40, 50)
#' y <- c(12, 24, 28, 42, 55)
#' calcular_correlacion(x, y)
#'
#' @export
calcular_correlacion <- function(x, y) {

  # --- Validaciones de Entrada ---
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Las dos variables deben ser numéricas.")
  }
  if (length(x) != length(y)) {
    stop("Las variables deben tener la misma longitud.")
  }
  if (length(x) < 2) {
    stop("Se necesitan al menos dos observaciones para calcular la correlación.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Las variables no pueden contener valores NA.")
  }

  n <- length(x)
  media_x <- sum(x) / n
  media_y <- sum(y) / n

  numerador <- sum((x - media_x) * (y - media_y))
  denominador <- sqrt(sum((x - media_x)^2) * sum((y - media_y)^2))

  # Evitar división entre cero
  if (denominador == 0) {return(NA)}

  resultado <- numerador / denominador
  return(resultado)
}

#' Cálculo de la Información Mutua entre Variables Categóricas
#'
#' @description
#' Cuantifica la cantidad de información compartida entre dos variables discretas expresada en bits (\eqn{\log_2}).
#'
#' @param x Vector categórico, factor o de caracteres.
#' @param y Vector categórico, factor o de caracteres de la misma longitud que \code{x}.
#'
#' @return Un valor numérico mayor o igual a 0.
#'
#' @details
#' La formula utilizada para calcular la informacion mutua es la siguiente:
#' \deqn{I(X; Y) = \sum_{x \in X} \sum_{y \in Y} P(x, y) \log_2 \left( \frac{P(x, y)}{P(x) P(y)} \right)}
#' donde \eqn{P(x, y)} representa la distribución de probabilidad conjunta y \eqn{P(x)}, \eqn{P(y)} las probabilidades marginales.
#'
#'
#' @examples
#' estudios <- c("Bachiller", "Grado", "Grado", "Master", "Bachiller", "Master")
#' puesto   <- c("Junior", "Senior", "Junior", "Senior", "Junior", "Senior")
#' calcular_informacion_mutua(estudios, puesto)
#'
#' @export
calcular_informacion_mutua <- function(x, y) {

  # --- Validaciones de Entrada ---
  if (length(x) != length(y)) {
    stop("Las variables deben tener la misma longitud.")
  }
  if (length(x) == 0) {
    stop("Las variables no pueden estar vacías.")
  }
  if (any(is.na(x)) || any(is.na(y))) {
    stop("Las variables no pueden contener valores NA.")
  }

  n <- length(x)

  # --- Tablas de Probabilidades ---
  px <- table(x) / n
  py <- table(y) / n
  pxy <- table(x, y) / n

  informacion <- 0

  for (i in seq_len(nrow(pxy))) {
    for (j in seq_len(ncol(pxy))) {

      p_xy <- pxy[i, j]

      # Excluir p_xy == 0 para evitar indefinicón del logaritmo
      if (p_xy > 0) {
        p_x <- px[i]
        p_y <- py[j]

        informacion <- informacion + p_xy * log2(p_xy / (p_x * p_y))
      }
    }
  }

  return(as.numeric(informacion))
}


#' Calculo de la Matriz de Relaciones para un Dataset
#'
#' @description
#' Evalúa todas las parejas de variables de un dataset. Calcula el Coeficiente de Correlación
#' de Pearson para pares continuos y la Información Mutua para pares discretos.
#'
#' @param dataset Objeto de tipo \code{data.frame} que contiene las variables a evaluar.
#'
#' @return Una matriz (\code{matrix}) de dimensión \eqn{p \times p} (donde \eqn{p} es el número de columnas),
#'   cuyas filas y columnas reciben los nombres de los atributos originales.
#'
#' @details
#' La función identifica el tipo de dato mediante \code{is.numeric()}:
#' \itemize{
#'   \item \bold{Continuo vs. Continuo:} Aplica \code{calcular_correlacion(x, y)}.
#'   \item \bold{Discreto vs. Discreto:} Aplica \code{calcular_informacion_mutua(x, y)}.
#'   \item \bold{Diagonal Principal (\eqn{i = j}):} Para variables continuas asigna \eqn{1.0}.
#'         Para categóricas calcula \eqn{I(X; X)}, lo que representa la entropía del atributo.
#'   \item \bold{Continuo vs. Discreto:} Devuelve \code{NA_real_}.
#' }
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(18, 25, 45, 60),
#'   Ingresos = c(1200, 2500, 3800, 1900),
#'   Estudios = c("Bajo", "Medio", "Alto", "Medio")
#' )
#' calcular_matriz_relaciones(df)
#'
#' @export
calcular_matriz_relaciones <- function(dataset) {

  # --- Validaciones Iniciales ---
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data.frame.")
  }
  if (ncol(dataset) < 2) {
    stop("El dataset debe tener al menos dos variables.")
  }

  n <- ncol(dataset)

  matriz <- matrix(
    NA_real_,
    nrow = n,
    ncol = n,
    dimnames = list(colnames(dataset), colnames(dataset))
  )

  # --- Evaluación ---
  for (i in 1:n) {
    for (j in 1:n) {

      x <- dataset[[i]]
      y <- dataset[[j]]

      x_numerica <- is.numeric(x)
      y_numerica <- is.numeric(y)

      # Diagonal Principal (Misma variable)
      if (i == j) {
        if (x_numerica && length(unique(x)) > 1) {
          matriz[i, j] <- 1.0
        } else if (!x_numerica) {
          matriz[i, j] <- calcular_informacion_mutua(x, x)
        }

        # Numérico - Numérico
      } else if (x_numerica && y_numerica) {
        matriz[i, j] <- calcular_correlacion(x, y)

        # Categórico - Categórico
      } else if (!x_numerica && !y_numerica) {
        matriz[i, j] <- calcular_informacion_mutua(x, y)

        # Numerico - Categórico
      } else {
        matriz[i, j] <- NA_real_
      }
    }
  }

  return(matriz)
}
