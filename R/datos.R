procesar_datos_entrada <- function(df) {
  # limpiar espacios en nombres de columna
  names(df) <- trimws(names(df))
  # nombres vacíos
  names(df)[names(df) == ""] <- paste0("COL_", which(names(df) == ""))
  
  # forzar numérico en CANTIDAD
  df$CANTIDAD <- suppressWarnings(as.numeric(df$CANTIDAD))
  df$CANTIDAD[is.na(df$CANTIDAD)] <- 0
  # construir nombre científico
  df$ESPECIE <- paste(trimws(df$GENERO), trimws(df[["EPITETO ESPECIFICO"]]))
  # print(names(df))
  df$FECHA <- as.Date(df$FECHA, format = "%d-%m-%Y")
  df$MUESTRA <- paste(df$FECHA, df$ESTACION, df$`NOMBRE METODOLOGIA`, sep = "_")
  df
}