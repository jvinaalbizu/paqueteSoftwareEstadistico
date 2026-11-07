#' Gráfico de la Curva ROC y AUC para variables continuas
#'
#' @description
#' Recorre todas las variables numéricas continuas de un dataset y genera para cada una
#' el gráfico de la Curva ROC, mostrando numéricamente el AUC en el título.
#'
#' @param dataset Objeto de tipo \code{data.frame} que contiene las variables.
#' @param nombre_var_clase Cadena de texto (\code{character}) con el nombre de la variableclase.
#'
#' @return No devuelve ningún valor; genera un gráfico por cada variable numérica en la ventana gráfica
#' (usar la flecha par ver todos los graficos).
#'
#' @details
#' La función identifica los atributos numéricos  mediante \code{is.numeric()}
#' y excluye la variable clase. Para cada atributo continuo:
#' \enumerate{
#'   \item Calcula el AUC empleando la función \code{calcular_auc()}.
#'   \item Extrae las coordenadas de la curva ROC mediante \code{calcular_roc()}.
#'   \item Dibuja la trayectoria de la (\eqn{\text{TPR}}) frente a la Tasa de Falsos Positivos (\eqn{\text{FPR}}).
#'   \item Añade una línea diagonal roja discontinua como referencia (\eqn{\text{AUC} = 0.5}).
#' }
#'
#' Se omiten los atributos que contengan valores (\code{NA}) o las variables que son constantes.
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(20, 25, 30, 35, 40, 45),
#'   Ingresos = c(1000, 1500, 1200, 3000, 3500, 4000),
#'   Clase = c("No", "No", "No", "Sí", "Sí", "Sí")
#' )
#' plot_auc(df, nombre_var_clase = "Clase")
#'
#' @export
plot_auc <- function(dataset, nombre_var_clase) {

  # --- Validaciones de Entrada ---
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data.frame.")
  }
  if (!(nombre_var_clase %in% colnames(dataset))) {
    stop("La variable de clase no existe en el dataset.")
  }

  clase <- dataset[[nombre_var_clase]]

  if (anyNA(clase) || length(unique(clase)) != 2) {
    stop("La variable de clase debe ser binaria y no contener valores NA.")
  }

  # Seleccionar las variables numéricas, excluyendo la variable clase
  variables <- setdiff(colnames(dataset), nombre_var_clase)
  variables <- variables[sapply(dataset[variables], is.numeric)]

  if (length(variables) == 0) {
    stop("No hay variables numéricas continuas para representar.")
  }

  # --- Bucle de Generación de Gráficos ---
  for (nombre in variables) {

    x <- dataset[[nombre]]

    # Comprobar valores ausentes
    if (anyNA(x)) {
      message("Se omite '", nombre, "': contiene valores NA.")
      next
    }

    # Comprobar variabilidad
    if (length(unique(x)) < 2) {
      message("Se omite '", nombre, "': es una variable constante.")
      next
    }

    # Cálculo de auc y puntos ROC
    auc_val <- calcular_auc(x, clase)
    roc_datos <- calcular_roc(x, clase)

    # Grafico
    plot(
      roc_datos$FPR, roc_datos$TPR,
      type = "l",
      lwd = 2,
      col = "blue",
      xlim = c(0, 1),
      ylim = c(0, 1),
      main = paste0("Curva ROC - ", nombre, "\nAUC = ", round(auc_val, 3)),
      xlab = "Tasa de Falsos Positivos",
      ylab = "Tasa de Verdaderos Positivos"
    )

    # Recta de referencia ( AUC = 0.5)
    abline(
      a = 0,
      b = 1,
      col = "red",
      lty = 2
    )

    grid(col = "lightgray", lty = "dotted")
  }
}


#' Gráficos de Correlación e Información Mutua
#'
#' @description
#' Genera gráficos para analizar las relaciones entre todas las variables de un dataset:
#' diagramas de dispersión con recta de ajuste para pares numéricos, y gráficos de barras para pares categóricos.
#'
#' @param dataset Objeto de tipo \code{data.frame} que contiene al menos dos columnas.
#'
#' @return No devuelve ningún valor; muestra los gráficos en la ventana grafica.
#'
#' @details
#' La función evalúa todas las combinaciones de columnas \eqn{(i, j)} donde \eqn{i < j}:
#' \itemize{
#'   \item \bold{Numérico vs. Numérico:} Calcula la Correlación de Pearson mediante \code{calcular_correlacion()}
#'         y genera un diagrama de dispersión dibujando la recta de regresión lineal en rojo.
#'   \item \bold{Categórico vs. Categórico:} Calcula la Información Mutua con \code{calcular_informacion_mutua()}
#'         y construye un gráfico de barras por grupos.
#'   \item \bold{Numerico vs. Categorico:} Las parejas formadas por un atributo continuo y uno categórico se ignoran en la representación gráfica.
#' }
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(20, 25, 30, 35, 40, 45),
#'   Ingresos = c(1000, 1500, 1200, 3000, 3500, 4000),
#'   Nivel = c("Bajo", "Bajo", "Medio", "Alto", "Alto", "Alto")
#' )
#' plot_correlacion_infmutua(df)
#'
#' @export
plot_correlacion_infmutua <- function(dataset) {

  # --- Validaciones Iniciales ---
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data.frame.")
  }

  n <- ncol(dataset)

  if (n < 2) {
    stop("Se necesitan al menos dos variables.")
  }

  # --- Bucle  ---
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {

      x <- dataset[[i]]
      y <- dataset[[j]]

      nombre_x <- colnames(dataset)[i]
      nombre_y <- colnames(dataset)[j]

      # Numerico-Numerico
      if (is.numeric(x) && is.numeric(y)) {

        r_val <- calcular_correlacion(x, y)

        plot(
          x, y,
          main = paste0(nombre_x, " vs ", nombre_y, "\nCorrelación de Pearson = ", round(r_val, 3)),
          xlab = nombre_x,
          ylab = nombre_y,
          pch = 19,
          col = "blue"
        )

        # Recta de regresión lineal
        if (!is.na(r_val) && length(unique(x)) > 1) {
          media_x <- mean(x)
          media_y <- mean(y)

          pendiente <- sum((x - media_x) * (y - media_y)) / sum((x - media_x)^2)
          ordenada <- media_y - pendiente * media_x

          abline(a = ordenada, b = pendiente, col = "red", lwd = 2)
        }

        # Discreto - Discreto
      } else if (!is.numeric(x) && !is.numeric(y)) {

        im_val <- calcular_informacion_mutua(x, y)
        tabla <- table(x, y)

        barplot(
          tabla,
          beside = TRUE,
          legend.text = TRUE,
          main = paste0(nombre_x, " vs ", nombre_y, "\nInformación Mutua = ", round(im_val, 3), " bits"),
          xlab = nombre_y,
          ylab = "Frecuencia Absoluta",
          col = rainbow(nrow(tabla))
        )

      }
    }
  }
}
