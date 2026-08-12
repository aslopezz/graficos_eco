calcular_indices <- function(df) {
  abundancias <- df %>%
    group_by(ESPECIE) %>%
    summarise(abundancia = sum(CANTIDAD, na.rm = TRUE),
              .groups = "drop")
  abundancias <- abundancias %>%
    filter(abundancia > 0)
  S <- nrow(abundancias)
  total <- sum(abundancias$abundancia)
  abundancias <- abundancias %>%
    mutate(abundancia_relativa = abundancia / total * 100)
  pi <- abundancias$abundancia_relativa / 100
  
  # Shannon (H')
  H <- -sum(pi * log(pi))
  # diversidad máxima (H' máx)
  Hmax <- log(S)
  pielou <- ifelse(S > 1, H / Hmax, NA)
  dominancia_Simpson <- sum(pi^2)
  
  indices <- data.frame(
    Riqueza = S,
    `Abundancia total` = total,
    `Shannon (H')` = round(H, 4),
    `Diversidad máxima (H' máx)` = round(Hmax, 4),
    `Pielou (J')` = round(pielou, 4),
    `Simpson (D)` = round(dominancia_Simpson, 4),
    check.names = FALSE
  )
  list(indices = indices,
       abundancia = abundancias %>%
         arrange(desc(abundancia)))
}

tabla_riqueza_abundancia <- function(df, agrupador) {
  df %>%
    filter(!is.na(.data[[agrupador]]), .data[[agrupador]] != "") %>%
    group_by(.data[[agrupador]]) %>%
    summarise(
      riqueza = n_distinct(ESPECIE),
      abundancia = sum(CANTIDAD, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(grupo = 1) %>%
    arrange(desc(abundancia))
  
}