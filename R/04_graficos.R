graficar <- function(df, col, titulo, tipo, metrica, paleta) {
  nombre_leyenda <- case_when(
    col == "ESTACION" ~ "Estación",
    col == "PROTOCOLO MUESTREO" ~ "Protocolo de Muestreo",
    col == "ORDEN" ~ "Orden",
    col == "CLASE" ~ "Clase",
    TRUE ~ "Categoría"
  )
  
  # construir resumen por variable
  # 02_utils.R
  res <- resumen_por_variable(df, col, metrica)
  
  if (metrica == "ambas") {
    res_long <- tidyr::pivot_longer(
      res,
      cols = c(abundancia, riqueza),
      names_to = "metrica",
      values_to = "valor"
    )
    
    p <- ggplot(res_long, aes(x = categoria, y = valor, fill = metrica)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = c(
        abundancia = "#209b87",
        riqueza = "#b9e576"
      )) +
      labs(title = titulo, x = NULL, y = "Valor") +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"),
        plot.title = element_text(face = "bold")
      )
    return(p)
  }
  y_lab <- if (metrica == "abundancia") {
    "Nº individuos"
  } else {
    "Nº especies"
  }
  if (tipo == "barras") {
    res <- res |> arrange(desc(valor))
    p <- ggplot(res, aes(x = categoria, y = valor, fill = categoria)) +
      geom_col(width = 0.7, alpha = 0.9, ) +
      geom_text(aes(label = valor), vjust = -0.5, size = 3.5) +
      scale_fill_manual(values = map_colors(res$categoria, paleta)) +
      labs(
        title = titulo,
        x = NULL,
        y = y_lab,
        # fill = nombre_leyenda
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        # eje X e Y visibles
        axis.line = element_line(color = "black"),
        # quitar líneas verticales
        panel.grid.major.x = element_blank(),
        # mantener líneas horizontales
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4),
        plot.title = element_text(face = "bold", size = 11),
        # legend.position = "right"
        legend.position = "none"
      )
  } else {
    res$pct <- round(res$valor / sum(res$valor) * 100, 1)
    
    p <- ggplot(res, aes(x = "", y = valor, fill = categoria)) +
      geom_col(width = 1, color = "white", linewidth = 0.4) +
      # geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), color = "black", size = 4) +
      coord_polar("y") +
      scale_fill_manual(values = map_colors(res$categoria, paleta)) +
      labs(title = titulo, fill = nombre_leyenda) +
      theme_void(base_size = 11) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    }
  p
}

plot_curva_acumulacion <- function(curva) {
  curva$asintota <- max(curva$estimado, na.rm = TRUE)
  
  ggplot(curva, aes(x = esfuerzo)) +
    
    geom_ribbon(
      aes(
        ymin = observado - observado_sd,
        ymax = observado + observado_sd,
        fill = "± 1 DE"
      ),
      alpha = .35
    ) +
    
    scale_x_continuous(expand = c(0, 0)) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    
    geom_line(aes(y = observado, colour = "Observado"), linewidth = 1.3) +
    
    geom_line(aes(
      y = estimado,
      colour = "Estimador",
    ),
    linewidth = 1.2,
    linetype = "dashed") +
    
    geom_line(aes(y = asintota, colour = "Asíntota"), linewidth = 1.2) +
    
    scale_colour_manual(values = c(
      Observado = "#1f77b4",
      Estimador = "#00aa88",
      Asíntota = "#c9a000"
    )) +
    
    scale_fill_manual(values = c("± 1 DE" = "grey80"), name = NULL) +
    
    labs(
      x = "Esfuerzo de muestreo",
      y = "Riqueza de especies",
      color = NULL,
      fill = NULL
    ) +
    
    theme_classic() +
    
    theme (panel.grid.major.y = element_line(color = "grey80", linewidth = 0.4))
}