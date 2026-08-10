preparar_datos_espaciales <- function(df, huso) {
  extraer_coordenadas <- function(x) {

    if (
      is.na(x) ||
      trimws(as.character(x)) == ""
    ) {
      return(
        c(
          LAT = NA_real_,
          LON = NA_real_
        )
      )
    }

    partes <- strsplit(
      as.character(x),
      ",",
      fixed = TRUE
    )[[1]]

    if (length(partes) != 2) {

      return(
        c(
          LAT = NA_real_,
          LON = NA_real_
        )
      )
    }

    lat <- suppressWarnings(
      as.numeric(
        trimws(partes[1])
      )
    )

    lon <- suppressWarnings(
      as.numeric(
        trimws(partes[2])
      )
    )

    # Validación 
    if (
      is.na(lat) ||
      is.na(lon) ||
      lat < -90 ||
      lat > 90 ||
      lon < -180 ||
      lon > 180
    ) {

      return(
        c(
          LAT = NA_real_,
          LON = NA_real_
        )
      )
    }

    c(
      LAT = lat,
      LON = lon
    )
  }

  coords <- t(
    vapply(
      df$COORDENADAS,
      extraer_coordenadas,
      FUN.VALUE = c(
        LAT = NA_real_,
        LON = NA_real_
      )
    )
  )

  df$LAT <- coords[, "LAT"]
  df$LON <- coords[, "LON"]

  df$ORIGEN_COORDENADA <- ifelse(
    !is.na(df$LAT) &
      !is.na(df$LON),
    "ORIGINAL",
    NA_character_
  )

  protocolo <- trimws(
    tolower(
      as.character(
        df$`PROTOCOLO MUESTREO`
      )
    )
  )

  es_punto_aves <- grepl(
    "punto de aves",
    protocolo,
    fixed = TRUE
  )

  es_ecolocalizacion <- grepl(
    "detección de ecolocalizaciones",
    protocolo,
    fixed = TRUE
  )


  # ------------------------------------------------------------
  # Buscar coordenadas faltantes
  # en caso de no encontrar
  # MISMA ESTACION
  # Primero Punto de Aves
  # Luego Detección de ecolocalizaciones
  # ------------------------------------------------------------

  for (i in seq_len(nrow(df))) {
    # Si ya tiene coordenadas, no hacer nada
    if (
      !is.na(df$LAT[i]) &&
      !is.na(df$LON[i])
    ) {
      next
    }

    # Estación del registro
    estacion <- df$ESTACION[i]

    if (
      is.na(estacion) ||
      trimws(as.character(estacion)) == ""
    ) {
      next
    }

    estacion <- trimws(
      as.character(estacion)
    )

    candidatos <- which(
      !is.na(df$ESTACION) &
      trimws(
        as.character(df$ESTACION)
      ) == estacion &
      !is.na(df$LAT) &
      !is.na(df$LON)
    )

    if (length(candidatos) == 0) {
      next
    }

    candidato_aves <- candidatos[es_punto_aves[candidatos]]

    if (length(candidato_aves) > 0) {
      candidato <- candidato_aves[1]
    } else {
      candidato_ecoloc <- candidatos[es_ecolocalizacion[candidatos]]

      if (length(candidato_ecoloc) == 0) {
        # No hay coordenada de reemplazo
        next
      }
      candidato <- candidato_ecoloc[1]
    }

    df$LAT[i] <-df$LAT[candidato]

    df$LON[i] <-df$LON[candidato]

    if (es_punto_aves[candidato]) {
      df$ORIGEN_COORDENADA[i] <-
        "ESTACION_PUNTO_DE_AVES"
    } else {
      df$ORIGEN_COORDENADA[i] <-
        "ESTACION_ECOLOCALIZACION"
    }
  }

# UTM
  epsg <- switch(
    huso,
    "18S" = 32718,
    "19S" = 32719,
    stop(
      paste(
        "Huso inválido:",
        huso
      )
    )
  )

  df$COOR_X <- NA_real_
  df$COOR_Y <- NA_real_

  coordenadas_validas <- (
    !is.na(df$LAT) &
      !is.na(df$LON)
  )

  if (any(coordenadas_validas)) {
    datos_coord <- df[
      coordenadas_validas,
      ,
      drop = FALSE
    ]

    puntos_wgs84 <- sf::st_as_sf(
      datos_coord,
      coords = c("LON","LAT"),
      crs = 4326,
      remove = FALSE
    )


    puntos_utm <- sf::st_transform(
      puntos_wgs84,
      crs = epsg
    )

    xy <- sf::st_coordinates(puntos_utm)

    df$COOR_X[coordenadas_validas] <- xy[, "X"]

    df$COOR_Y[coordenadas_validas] <- xy[, "Y"]
  }

  geometria <- vector(
    "list",
    nrow(df)
  )


  for (i in seq_len(nrow(df))) {

    if (
      !is.na(df$COOR_X[i]) &&
      !is.na(df$COOR_Y[i])
    ) {

      geometria[[i]] <- sf::st_point(
        c(
          df$COOR_X[i],
          df$COOR_Y[i]
        )
      )

    } else {

      geometria[[i]] <- sf::st_point()
    }
  }


  geometria <- sf::st_sfc(
    geometria,
    crs = epsg
  )

  resultado <- sf::st_sf(
    df,
    geometry = geometria
  )

  message("")
  message("==============================")
  message("DIAGNOSTICO ESPACIAL")
  message("==============================")

  message(
    "Total registros: ",
    nrow(resultado)
  )

  message(
    "Coordenadas originales: ",
    sum(
      resultado$ORIGEN_COORDENADA ==
        "ORIGINAL",
      na.rm = TRUE
    )
  )

  message(
    "Coordenadas Punto de Aves: ",
    sum(
      resultado$ORIGEN_COORDENADA ==
        "ESTACION_PUNTO_DE_AVES",
      na.rm = TRUE
    )
  )

  message(
    "Coordenadas ecolocalización: ",
    sum(
      resultado$ORIGEN_COORDENADA ==
        "ESTACION_ECOLOCALIZACION",
      na.rm = TRUE
    )
  )

  message(
    "Sin coordenadas: ",
    sum(
      is.na(resultado$COOR_X) |
        is.na(resultado$COOR_Y)
    )
  )

  message("==============================")
  message("")

  resultado
}
