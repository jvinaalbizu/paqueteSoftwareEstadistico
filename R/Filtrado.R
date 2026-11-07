#' Filtrado de variables de un Dataset
#'
#' @description
#' Genera una versión reducida de un dataset conservando únicamente las variables que superen
#' los umbrales establecidos para las metricas: varianza normalizada y AUC para variables continuas,
#' y entropía para variables discretas.
#'
#' @param dataset Objeto de tipo \code{data.frame} que contiene las variables a filtrar.
#' @param nombre_var_clase Cadena de texto (\code{character}) con el nombre de la variable
#'   clase. Esta variable se conserva igual.
#'
#' @return Un \code{data.frame} que conserva las columnas que han superado los umbrales
#'   establecidos, incluyendo siempre la variable clase.
#'
#' @details
#' Para las **variables continuas**, la función aplica la normalización para evaluar la varianza de las variable
#' en una escala unificada \eqn{[0, 1]} sin depender de sus unidades originales:
#' \itemize{
#'   \item \bold{Varianza Normalizada > 0.05:}  elimina atributos cuasi-constantes o con muy baja variabilidad.
#'   \item \bold{AUC > 0.60:} Asegura que el atributo tiene cierta capacidad discriminatoria sobre la variable
#'         clase, superior al nivel del azar (\eqn{\text{AUC} = 0.50}).
#' }
#'
#' Para las **variables discretas o categóricas**:
#' \itemize{
#'   \item \bold{Entropía > 0.50 bits:} Elimina los atributos con muy baja incertidumbre.
#' }
#'
#' @examples
#' df <- data.frame(
#'   Edad = c(20, 22, 25, 45, 52, 60),
#'   Constante = c(5, 5, 5, 5, 5, 5),
#'   Nivel = c("Bajo", "Bajo", "Medio", "Alto", "Alto", "Alto"),
#'   Diagnostico = c("NO", "NO", "NO", "SI", "SI", "SI")
#' )
#' filtrar_variables(df, "Diagnostico")
#'
#' @export
filtrar_variables <- function(dataset, nombre_var_clase) {

  # --- Validaciones de entrada ---
  if (!is.data.frame(dataset)) {
    stop("El argumento 'dataset' debe ser un data.frame.")
  }
  if (!(nombre_var_clase %in% colnames(dataset))) {
    stop(paste0("La variable clase '", nombre_var_clase, "' no existe en el dataset."))
  }

  metricas <- calcular_metricas_dataset(dataset, nombre_var_clase)

  variables_mantener <- c()

  # Recorrer las columnas
  for (i in 1:nrow(metricas)) {

    nombre_variable <- metricas$Atributo[i]

    # Excluir la variable clase durante la evaluación
    if (nombre_variable == nombre_var_clase) {
      next
    }

    # --- Criterios para Variables Continuas ---
    if (metricas$Tipo[i] == "Continua") {
      variable_normalizada <- normalizar(dataset[[nombre_variable]])
      varianza_normalizada <- calcular_varianza(variable_normalizada)

      if (varianza_normalizada > 0.05 && metricas$AUC[i] > 0.60) {
        variables_mantener <- c(variables_mantener, nombre_variable)
      }

      # --- Criterios para Variables Discretas ---
    } else {
      if (metricas$Entropia[i] > 0.50) {
        variables_mantener <- c(variables_mantener, nombre_variable)
      }
    }
  }

  variables_mantener <- unique(c(variables_mantener, nombre_var_clase))

  dataset_filtrado <- dataset[, variables_mantener, drop = FALSE]

  return(dataset_filtrado)
}
