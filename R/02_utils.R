

map_colors <- function(categories, pal) {
  cats <- unique(categories)
  setNames(rep_len(pal, length(cats)), cats)
}

# helper para construir resumen por variable
resumen_por_variable <- function(df, var_p, metrica) {
  # var_sym <- as.name(var)
  df <- df[!is.na(df[[var_p]]) & df[[var_p]] != "", ]
  
  if (metrica == "abundancia") {
    res <- df |> group_by(across(all_of(var_p))) |>
      summarise(valor = sum(CANTIDAD, na.rm = TRUE),
                .groups = "drop") |>
      rename(categoria = 1)
  } else if (metrica == "riqueza") {
    res <- df |> group_by(across(all_of(var_p))) |>
      summarise(valor = n_distinct(ESPECIE), .groups = "drop") |>
      rename(categoria = 1)
  } else {
    # ambas → abundancia (eje primario para ggplot simple)
    res <- df |> group_by(across(all_of(var_p))) |>
      summarise(
        abundancia = sum(CANTIDAD, na.rm = TRUE),
        riqueza    = n_distinct(ESPECIE),
        .groups = "drop"
      ) |>
      rename(categoria = 1)
  }
  res
}