#' Cálculo de la Varianza
#'
#' @description
#' Calcula la varianza de un vector numérico aplicando la corrección
#' de Bessel (división entre \eqn{n - 1}).
#'
#' @param x Vector numérico.
#'
#' @return Un valor numérico (la varianza). Devuelve \code{NA} si el  vector contiene 1 o menos elementos.
#'
#' @details
#' La fórmula matemática implementada es:
#' \deqn{S^2 = \frac{\sum_{i=1}^{n} (x_i - \bar{x})^2}{n - 1}}
#'
#' @examples
#' calcular_varianza(c(2, 4, 4, 4, 5, 5, 7, 9))
#'
#' @export
calcular_varianza <- function(x) {
  n <- length(x)

  if (n <= 1) {
    return(NA)
  }

  media <- sum(x) / n
  varianza <- sum((x - media)^2) / (n - 1)

  return(varianza)
}

#' Cálculo de la Entropía de Shannon.
#'
#' @description
#' Mide la entropia de un vector discreto.
#'
#' @param x Vector categórico, factor, lógico o de cadena de caracteres.
#'
#' @return Un valor numérico expresado en bits (\eqn{\log_2}). Devuelve \code{NA} si el vector no tiene elementos
#'
#' @details
#' Aplica la ecuación de entropía de Shannon:
#' \deqn{H(X) = -\sum P(x_i) \log_2(P(x_i))}
#'
#' @examples
#' calcular_entropia(c("Sí", "No", "Sí", "Sí", "No"))
#'
#' @export
calcular_entropia <- function(x) {
  n <- length(x)

  if (n == 0) {
    return(NA)
  }

  frecuencias <- table(x)
  probabilidades <- frecuencias / n

  # Excluir probabilidades nulas para evitar log2(0)
  probabilidades <- probabilidades[probabilidades > 0]

  entropia <- -sum(probabilidades * log2(probabilidades))
  return(entropia)
}


#' Cálculo del AUC
#'
#' @description
#' Evalúa la capacidad discriminatoria de una variable continua respecto a una variable clase,
#' mediante la Estadística U de Mann-Whitney.
#'
#' @param x Vector numérico continuo.
#' @param clase Vector con la clase binaria.
#'
#' @return Un valor numérico con el AUC, entre 0 y 1 .
#'
#' @details
#' Calcula la probabilidad de que una observación positiva elegida al azar tenga un valor mas grande
#' que una observación negativa elegida al azar:
#' \deqn{\text{AUC} = P(X_{\text{positivo}} > X_{\text{negativo}})}
#'
#' Esta funcion no invierte el valor del AUC. Un valor de \eqn{\text{AUC} > 0.5} indica una asociación
#' directa entre el atributo y la clase positiva, mientras que un \eqn{\text{AUC} < 0.5} representa una asociación inversa.
#'
#' @examples
#' x <- c(140, 160, 190, 210, 250)
#' clase <- c("Sano", "Sano", "Enfermo", "Enfermo", "Enfermo")
#' calcular_auc(x, clase)
#'
#' @export
calcular_auc <- function(x, clase) {
  #--- Validaciones iniciales ---
  if (!is.numeric(x)) {
    stop("x debe ser numérico.")
  }
  if (length(x) != length(clase)) {
    stop("x y clase deben tener la misma longitud.")
  }
  if (any(is.na(x)) || any(is.na(clase))) {
    stop("x y clase no pueden contener valores NA.")
  }

  clases_unicas <- unique(clase)
  if (length(clases_unicas) != 2) {
    stop("La variable clase debe tener solo 2 valores distintos.")
  }

  #Divide la variable x en el subconjunto de los positivos y el de los negativos.
  positivos <- x[clase == clases_unicas[2]]
  negativos <- x[clase == clases_unicas[1]]

  n_positivos <- length(positivos)
  n_negativos <- length(negativos)

  if (n_positivos == 0 || n_negativos == 0) {
    stop("La clase debe contener observaciones de ambas clases.")
  }

  suma <- 0
  for (p in positivos) {
    for (n in negativos) {
      if (p > n) {
        suma <- suma + 1
      } else if (p == n) {
        suma <- suma + 0.5
      }
    }
  }

  auc <- suma / (n_positivos * n_negativos)

  return(auc)
}


#' Cálculo de la Curva ROC
#'
#' @description
#' Genera los puntos (umbrales, Tasa de Falsos Positivos y Tasa de Verdaderos Positivos)
#' requeridos para construir la Curva ROC.
#'
#' @param x Vector numérico continuo.
#' @param clase Vector con la clase binaria.
#'
#' @return Un \code{data.frame} con las columnas: \code{Umbral}, \code{FPR} (Tasa de falsos positivos) y
#' \code{TPR} (Tasa de verdaderos positivos).
#'
#' @details
#' La Curva ROC evalúa el rendimiento de un atributo continuo como clasificador binario a lo largo de
#' múltiples puntos de corte. La funcion realiza los siguientes pasos intermedios:
#' \enumerate{
#'   \item Extrae todos los valores únicos de la variable \eqn{x} y los ordena de forma descendente,
#'         añadiendo los limites \code{Inf} (donde ningún dato es clasificado como positivo) y
#'         \code{-Inf} (donde todos los datos son clasificados como positivos).
#'   \item Para cada umbral \eqn{u_i}, clasifica como positivos aquellos registros donde \eqn{x \ge u_i}.
#'   \item Calcula la Tasa de Verdaderos Positivos (\eqn{\text{TPR} = \frac{\text{TP}}{P}})
#'         y la Tasa de Falsos Positivos (\eqn{\text{FPR} = \frac{\text{FP}}{N}}).
#' }
#'
#' El resultado genera una trayectoria monótona creciente en el plano cartesiano
#' que empieza en el origen \eqn{(0, 0)} y finaliza en la esquina superior derecha \eqn{(1, 1)}.
#'
#' @examples
#' x <- c(140, 160, 190, 210, 250)
#' clase <- c("Sano", "Sano", "Enfermo", "Enfermo", "Enfermo")
#' calcular_roc(x, clase)
#' @export
calcular_roc <- function(x, clase) {

  # --- Validaciones de Entrada ---
  if (!is.numeric(x)) {
    stop("x debe ser numérico.")
  }
  if (length(x) != length(clase)) {
    stop("x y clase deben tener la misma longitud.")
  }
  if (any(is.na(x)) || any(is.na(clase))) {
    stop("x y clase no pueden contener valores NA.")
  }

  clases_unicas <- unique(clase)
  if (length(clases_unicas) != 2) {
    stop("La variable clase debe ser binaria (2 valores distintos).")
  }

  positivos <- x[clase == clases_unicas[2]]
  negativos <- x[clase == clases_unicas[1]]

  n_positivos <- length(positivos)
  n_negativos <- length(negativos)

  if (n_positivos == 0 || n_negativos == 0) {
    stop("La clase debe contener observaciones de ambas clases.")
  }

  # --- Construcción de Umbrales y Tasas ---
  umbrales <- c(Inf, sort(unique(x), decreasing = TRUE), -Inf)
  tpr <- numeric(length(umbrales))
  fpr <- numeric(length(umbrales))

  for (i in seq_along(umbrales)) {
    umbral <- umbrales[i]

    verdaderos_positivos <- sum(positivos >= umbral)
    falsos_positivos <- sum(negativos >= umbral)

    tpr[i] <- verdaderos_positivos / n_positivos
    fpr[i] <- falsos_positivos / n_negativos
  }

  resultado <- data.frame(
    Umbral = umbrales,
    FPR = fpr,
    TPR = tpr
  )

  return(resultado)
}

#' Cálculo global de metricas sobre un Dataset
#'
#' @description
#' Recorre todas las columnas de un dataset y calcula Varianza y AUC para los atributos
#' continuos, y Entropía para los atributos discretos.
#'
#' @param dataset Objeto de tipo \code{data.frame}.
#' @param var_clase Cadena de texto con el nombre de la variable clase.
#'
#' @return Un \code{data.frame} con las siguientes columnas:
#' \item{Atributo}{Nombre de la variable evaluada.}
#' \item{Tipo}{Clasificación del atributo (\code{"Continua"} o \code{"Discreta"}).}
#' \item{Varianza}{Varianza de la variable (solo continuas).}
#' \item{AUC}{AUC calculada respecto a la clase (solo continuas).}
#' \item{Entropia}{Entropía de Shannon expresada en bits (solo discretas).}
#'
#' @details
#' La identificación del tipo de atributo se hace mediante  \code{is.numeric(x)}. Si se quiere
#' evaluar un atributo numérico discreto (ej. número de hijos) mediante Entropía, se tiene que convertir
#' a \code{factor} o \code{character} antes de llamar la función.
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(22, 25, 47, 52, 46, 56),
#'   Estudios = c("ESO", "Grado", "Grado", "Master", "ESO", "Grado"),
#'   Diagnostico = c("NO", "NO", "SI", "SI", "SI", "SI")
#' )
#' calcular_metricas_dataset(df, "Diagnostico")
#'
#' @export
calcular_metricas_dataset <- function(dataset, var_clase) {

  # --- Validaciones de Entrada ---
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data.frame.")
  }
  if (!(var_clase %in% colnames(dataset))) {
    stop(paste0("La variable clase '", var_clase, "' no existe en el dataset."))
  }

  resultados <- data.frame(
    Atributo = character(),
    Tipo = character(),
    Varianza = numeric(),
    AUC = numeric(),
    Entropia = numeric(),
    stringsAsFactors = FALSE
  )

  # --- Recorremos los atributos ---
  for (nombre in colnames(dataset)) {

    # Excluir la variable clase
    if (nombre == var_clase) {
      next
    }

    x <- dataset[[nombre]]

    # Atributos Continuos
    if (is.numeric(x)) {
      varianza <- calcular_varianza(x)
      auc <- calcular_auc(x, dataset[[var_clase]])

      resultados <- rbind(
        resultados,
        data.frame(
          Atributo = nombre,
          Tipo = "Continua",
          Varianza = varianza,
          AUC = auc,
          Entropia = NA,
          stringsAsFactors = FALSE
        )
      )

      # Atributos Discretos
    } else {
      entropia <- calcular_entropia(x)

      resultados <- rbind(
        resultados,
        data.frame(
          Atributo = nombre,
          Tipo = "Discreta",
          Varianza = NA,
          AUC = NA,
          Entropia = entropia,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  return(resultados)
}
