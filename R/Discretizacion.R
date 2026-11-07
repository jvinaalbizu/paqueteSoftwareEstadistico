#' Discretización de Igual Anchura
#'
#' @description
#' Convierte un vector numérico en un vector de etiquetas con los intervalos correspondientes.
#' Divide los datos en intervalos con la misma longitud.
#'
#' @param vector_datos Vector numérico que se quiere discretizar. No puede estar vacío,
#'   contener valores \code{NA}, valores infinitos, tiene que tener variabilidad.
#' @param num_intervalos Número entero positivo (\eqn{k \ge 1}) que indica la cantidad de intervalos que se quieren crear.
#'
#' @return Una lista (\code{list}) con los siguientes componentes:
#' \item{datos_discretizados}{Vector de caracteres (\code{character}) de la misma longitud que \code{vector_datos},
#' donde cada elemento contiene el intervalo correspondiente de cada numero (ej. \code{"[a,b)"}).}
#' \item{limites}{Vector con los \eqn{k + 1} puntos de corte  de cada intervalo.}
#'
#' @details
#' El algoritmo calcula el ancho de cada intervalo con la formula:
#' \deqn{\text{ancho} = \frac{\max(x) - \min(x)}{k}}
#' Los primeros \eqn{k - 1} intervalos son semiabiertos por la derecha \code{[a, b)},
#' mientras que el último intervalo \eqn{k} es cerrado por la derecha \code{[a, b]}
#' para incluir el valor máximo del atributo sin desbordamiento.
#'
#' La asignación de cada numero \eqn{x_i} a su intervalo correspondiente se determina mediante la formula:
#' \deqn{\text{índice} = \lfloor \frac{x_i - \min(x)}{\text{ancho}} \rfloor + 1}
#'
#' @examples
#' datos <- c(1, 2, 3, 3.5, 3.9, 7, 8, 8.5, 9, 10)
#' resultado <- discretizar_anchura_atributo(datos, 2)
#' resultado$datos_discretizados
#'
#' @export
discretizar_anchura_atributo <- function(vector_datos, num_intervalos) {

  # --- Validaciones iniciales ---
  if (!is.numeric(vector_datos)) {
    stop("El atributo debe ser numérico.")
  }
  if (length(vector_datos) == 0) {
    stop("El atributo no puede estar vacío.")
  }
  if (num_intervalos < 1 || num_intervalos != as.integer(num_intervalos)) {
    stop("num_intervalos debe ser un numero entero positivo.")
  }
  if (any(is.na(vector_datos))) {
    stop("El atributo no puede contener valores NA.")
  }
  if (any(is.infinite(vector_datos))) {
    stop("El atributo no puede contener valores infinitos.")
  }
  if (min(vector_datos) == max(vector_datos)) {
    val_formateado <- round(min(vector_datos), 2)
    etiqueta_unica <- paste0("[", val_formateado, ",", val_formateado, "]")
    return(list(
      datos_discretizados = rep(etiqueta_unica, length(vector_datos)),
      limites = c(min(vector_datos), max(vector_datos))
    ))
  }

  # --- Cálculo del Ancho de Intervalo ---
  valor_min <- min(vector_datos)
  valor_max <- max(vector_datos)
  ancho <- (valor_max - valor_min) / num_intervalos

  # --- Puntos de Corte y Etiquetas ---
  limites <- numeric(num_intervalos + 1)
  for (i in 0:num_intervalos) {
    limites[i + 1] <- valor_min + (i * ancho)
  }

  limites_red <- round(limites, 2)
  etiquetas <- character(num_intervalos)

  for (k in 1:num_intervalos) {
    if (k == num_intervalos) {
      # El último intervalo es cerrado por la derecha: [a, b]
      etiquetas[k] <- paste0("[", limites_red[k], ",", limites_red[k + 1], "]")
    } else {
      # Intervalos semiabiertos por la derecha: [a, b)
      etiquetas[k] <- paste0("[", limites_red[k], ",", limites_red[k + 1], ")")
    }
  }

  # --- Asignación de Datos a Intervalos ---
  resultado <- character(length(vector_datos))

  for (i in 1:length(vector_datos)) {
    x <- vector_datos[i]

    if (x == valor_max) {
      # Asignación explícita para evitar desbordamiento por precisión en el máximo
      resultado[i] <- etiquetas[num_intervalos]
    } else {
      indice_intervalo <- floor((x - valor_min) / ancho) + 1
      resultado[i] <- etiquetas[indice_intervalo]
    }
  }

  return(list(datos_discretizados = resultado, limites = limites))
}


#' Discretización de igual frecuencia
#'
#' @description
#' Transforma un atributo numérico en intervalos que contienen
#' aproximadamente el mismo número de observaciones.
#'
#' @param vector_datos Vector numérico a discretizar.
#' @param num_intervalos Número entero positivo (\eqn{k \ge 1}) de intervalos que se quieren generar.
#'
#' @return Una lista (\code{list}) con dos elementos:
#' \item{datos_discretizados}{Vector de caracteres (\code{character}) con los intervalos asignados.}
#' \item{limites}{Vector numérico con las puntos de corte extraídos de los datos.}
#'
#' @details
#' El algoritmo obtiene la posición relativa  de cada dato mediante \code{order(order(vector_datos))},
#' para garantizar una distribución equitativa sin cambiar el orden original.
#'
#' Despues, se asigna el intervalo correspondientw mediante:
#' \deqn{\text{grupo} = \lceil \frac{\text{posición}}{n / k} \rceil}
#'
#' @examples
#' datos <- c(12, 5, 30, 18, 7, 25, 40, 15, 33, 10, 22, 28)
#' resultado <- discretizar_frecuencia_atributo(datos, 3)
#' resultado$datos_discretizados
#'
#' @export
discretizar_frecuencia_atributo <- function(vector_datos, num_intervalos) {

  # --- Validaciones de entrada ---
  if (!is.numeric(vector_datos)) {
    stop("El atributo debe ser numérico.")
  }
  if (length(vector_datos) == 0) {
    stop("El atributo no puede estar vacío.")
  }
  if (any(is.na(vector_datos))) {
    stop("El atributo no puede contener valores NA.")
  }
  if (any(is.infinite(vector_datos))) {
    stop("El atributo no puede contener valores infinitos.")
  }
  if (num_intervalos < 1 || num_intervalos != as.integer(num_intervalos)) {
    stop("num_intervalos debe ser un entero mayor o igual a 1.")
  }
  if (min(vector_datos) == max(vector_datos)) {
    val_formateado <- round(min(vector_datos), 2)
    etiqueta_unica <- paste0("[", val_formateado, ",", val_formateado, "]")
    return(list(
      datos_discretizados = rep(etiqueta_unica, length(vector_datos)),
      limites = c(min(vector_datos), max(vector_datos))
    ))
  }

  n <- length(vector_datos)

  # --- Obtener la posición de cada elemento en el vector original---
  posiciones <- order(order(vector_datos))

  # --- Calculamos a qué grupo pertenece cada posición ---
  elementos_por_intervalo <- n / num_intervalos
  indices_intervalo <- ceiling(posiciones / elementos_por_intervalo)
  indices_intervalo[indices_intervalo > num_intervalos] <- num_intervalos

  # --- Cálculo de límites para construir las etiquetas ---
  datos_ordenados <- sort(vector_datos)
  limites <- numeric(num_intervalos + 1)
  limites[1] <- datos_ordenados[1]

  for (k in 1:(num_intervalos - 1)) {
    idx <- round(k * elementos_por_intervalo)
    limites[k + 1] <- datos_ordenados[idx]
  }
  limites[num_intervalos + 1] <- datos_ordenados[n]

  limites_red <- round(limites, 2)

  # --- Construcción de etiquetas en formato texto ---
  etiquetas <- character(num_intervalos)
  for (k in 1:num_intervalos) {
    if (k == num_intervalos) {
      etiquetas[k] <- paste0("[", limites_red[k], ",", limites_red[k + 1], "]")
    } else {
      etiquetas[k] <- paste0("[", limites_red[k], ",", limites_red[k + 1], ")")
    }
  }

  # --- Asignar cada dato a su etiqueta ---
  resultado <- etiquetas[indices_intervalo]

  return(list(datos_discretizados = resultado, limites = limites))
}



#' Discretización de un Dataset
#'
#' @description
#' Aplica la discretizacion seleccionada (\code{"anchura"} o \code{"frecuencia"}) a todas
#' las variables numéricas en un \code{data.frame}.
#'
#' @param dataset Objeto de tipo \code{data.frame} que contiene los vectores a discretizar.
#' @param num_intervalos Número entero positivo de intervalos que se quieren crear.
#' @param metodo Cadena de texto con el metodo a aplicar: \code{"anchura"} o \code{"frecuencia"}.
#'
#' @return Un \code{data.frame} con la misma estructura que el dataset original, donde las columnas numéricas
#'   han sido sustituidas por sus correspondientes intervalosy las columnas no numéricas
#'   (factores, texto o lógicas) se han conservado sin cambiarlas.
#'
#' @details
#' La función recorre cada columna mediante un bucle. Aplica la comprobación \code{is.numeric()},
#' y si la columna es numerica le aplica el metodo seleccionado.
#' Si una columna no es numérica, la instrucción \code{next} la salta sin cambiarla.
#'
#' @examples
#' df <- data.frame(
#'   Nombre = c("Ana", "Juan", "Pedro"),
#'   Edad = c(18, 45, 60),
#'   Ingresos = c(1200, 3800, 1900)
#' )
#' discretizar_dataset(df, 2, "anchura")
#'
#' @export
discretizar_dataset <- function(dataset, num_intervalos, metodo = "anchura") {

  # --- Validaciones Iniciales ---
  if (!is.data.frame(dataset) && !is.matrix(dataset)) {
    stop("El dataset debe ser un data.frame o una matriz.")
  }
  if (ncol(dataset) == 0) {
    stop("El dataset no contiene columnas.")
  }
  if (!(metodo %in% c("anchura", "frecuencia"))) {
    stop("El método debe ser 'anchura' o 'frecuencia'.")
  }

  dataset_discretizado <- dataset
  num_columnas <- ncol(dataset)

  # --- Bucle por cada columna ---
  for (j in 1:num_columnas) {
    columna <- dataset_discretizado[, j]

    # Preservar atributos  no numéricos
    if (!is.numeric(columna)) {
      next
    }

    # Aplicar el método correspondiente
    if (metodo == "anchura") {
      res <- discretizar_anchura_atributo(columna, num_intervalos)
      dataset_discretizado[, j] <- res$datos_discretizados
    } else if (metodo == "frecuencia") {
      res <- discretizar_frecuencia_atributo(columna, num_intervalos)
      dataset_discretizado[, j] <- res$datos_discretizados
    }
  }

  return(dataset_discretizado)
}
