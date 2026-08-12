calcular_curva_acumulacion <- function(matriz,
                                       estimador = "Chao 1",
                                       nperm = 500) {
  # Curva observada
  obs <- vegan::specaccum(matriz, method = "random", permutations = nperm)
  
  n <- nrow(matriz)
  riqueza <- matrix(NA, nrow = nperm, ncol = n)
  
  for (i in seq_len(nperm)) {
    orden <- sample(n)
    datos <- matriz[orden, , drop = FALSE]
    
    for (j in seq_len(n)) {
      sub <- datos[1:j, , drop = FALSE]
      
      valor <- switch(
        estimador,
        "Chao 1" = {
          # vegan::estimateR(colSums(sub))[2]
          abund <- colSums(sub)
          if (sum(abund > 0) < 2) {
            NA
          } else {
            vegan::estimateR(abund)[2]
          }
        },
        "ACE" = {
          # vegan::estimateR(colSums(sub))[4]
          abund <- colSums(sub)
          if (sum(abund > 0) < 2) {
            NA
          } else {
            vegan::estimateR(abund)[4]
          }
        },
        "Chao 2" = {
          sub.pa <- sub
          sub.pa[sub.pa > 0] <- 1
          vegan::specpool(sub.pa)$chao
        },
        "Jackknife 1" = {
          sub.pa <- sub
          sub.pa[sub.pa > 0] <- 1
          vegan::specpool(sub.pa)$jack1
        },
        "Bootstrap" = {
          sub.pa <- sub
          sub.pa[sub.pa > 0] <- 1
          vegan::specpool(sub.pa)$boot
        }
      )
      riqueza[i, j] <- valor
    }
  }
  
  data.frame(
    esfuerzo = obs$sites,
    observado = obs$richness,
    observado_sd = obs$sd,
    estimado = colMeans(riqueza, na.rm = TRUE),
    estimado_sd = apply(riqueza, 2, sd, na.rm = TRUE)
  )
}