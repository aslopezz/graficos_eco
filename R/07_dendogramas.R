#' Construye la matriz ESPECIE x ESTACION
#'
#' @param df data.frame ya procesado (con ESTACION, ESPECIE, CANTIDAD)
#' @param tipo "abundancia" (usa CANTIDAD, para Bray-Curtis) o
#'             "presencia" (0/1, para Jaccard)
#'
#' @return matrix con ESPECIE como filas, ESTACION como columnas
construir_matriz_especies <- function(df, tipo = "abundancia") {

  df_valido <- df %>%
    dplyr::filter(
      !is.na(ESTACION), trimws(ESTACION) != "",
      !is.na(ESPECIE), trimws(ESPECIE) != ""
    )

  if (nrow(df_valido) == 0) {
    stop("No hay registros válidos con ESTACION y ESPECIE.")
  }

  df_agg <- df_valido %>%
    dplyr::group_by(ESPECIE, ESTACION) %>%
    dplyr::summarise(ABUNDANCIA = sum(as.numeric(CANTIDAD), na.rm = TRUE), .groups = "drop")

  matriz <- df_agg %>%
    tidyr::pivot_wider(
      names_from = ESTACION,
      values_from = ABUNDANCIA,
      values_fill = 0
    ) %>%
    tibble::column_to_rownames("ESPECIE") %>%
    as.matrix()

  if (tipo == "presencia") {
    matriz <- (matriz > 0) * 1
  }

  matriz
}

#' Calcula el clustering jerárquico de especies (Bray-Curtis o Jaccard)
#'
#' @param matriz salida de construir_matriz_especies() [ESPECIE x ESTACION]
#' @param metodo_dist "bray" o "jaccard"
#' @param metodo_clust "average","complete","ward.D2","single"
#'
#' @return objeto hclust
calcular_dendograma_especies <- function(matriz, metodo_dist = "bray", metodo_clust = "average") {

  filas_validas <- rowSums(matriz) > 0
  matriz <- matriz[filas_validas, , drop = FALSE]

  if (nrow(matriz) < 2) {
    stop("Se necesitan al menos 2 especies con registros para construir un dendograma.")
  }

  if (ncol(matriz) < 2) {
    stop("Se necesitan al menos 2 estaciones distintas en el terreno para comparar especies.")
  }

  binario <- metodo_dist == "jaccard"

  distancia <- vegan::vegdist(matriz, method = metodo_dist, binary = binario)
  stats::hclust(distancia, method = metodo_clust)
}

#' Dibuja el dendograma de especies coloreado por corte en k grupos
#'
#' @param hc objeto hclust
#' @param k número de grupos a resaltar
#' @param metodo_dist usado solo para el subtítulo
#'
#' @return objeto ggplot
plot_dendograma_especies <- function(hc, k = 3, metodo_dist = "bray") {

  dend_data <- ggdendro::dendro_data(hc)

  grupos <- stats::cutree(hc, k = k)
  grupos_df <- data.frame(label = names(grupos), grupo = factor(grupos))

  hojas <- dend_data$labels %>%
    dplyr::left_join(grupos_df, by = "label")

  y_max <- max(dend_data$segments$y)
  etiqueta_metodo <- ifelse(metodo_dist == "bray", "Bray-Curtis", "Jaccard")

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = dend_data$segments,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    ggplot2::geom_text(
      data = hojas,
      ggplot2::aes(x = x, y = -0.03 * y_max, label = label, color = grupo),
      angle = 90, hjust = 1, size = 3.5, fontface = "italic"
    ) +
    ggplot2::labs(
      title = "Similitud entre especies",
      subtitle = paste("Distancia:", etiqueta_metodo, "— agrupadas según estaciones donde aparecen"),
      x = "", y = "Distancia"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.35, 0.05))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)
    )
}