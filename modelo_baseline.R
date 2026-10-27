library(data.table)

dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Médias de 2024 e 2025
base_2024 <- dados[NU_ANO == 2024]
base_2025 <- dados[NU_ANO == 2025]

# Associar cada região à sua média anterior
comparacao <- merge(
  base_2025,
  base_2024,
  by = "REGIAO",
  suffixes = c("_REAL", "_PREVISTO")
)

categorias <- c(
  "MEDIA_HUMANAS",
  "MEDIA_EXATAS",
  "MEDIA_REDACAO"
)

metricas <- list()

for (categoria in categorias) {

  real <- comparacao[[paste0(categoria, "_REAL")]]
  previsto <- comparacao[[paste0(categoria, "_PREVISTO")]]

  mae <- mean(abs(real - previsto))
  rmse <- sqrt(mean((real - previsto)^2))

  metricas[[categoria]] <- data.table(
    MODELO = "Baseline 2024",
    CATEGORIA = categoria,
    MAE = mae,
    RMSE = rmse
  )

  cat("\nCategoria:", categoria, "\n")
  cat("MAE:", round(mae, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
}

resultado <- rbindlist(metricas)

dir.create("resultados", showWarnings = FALSE)

fwrite(
  resultado,
  "resultados/metricas_baseline.csv"
)

cat("\n--- RESULTADOS BASELINE ---\n")
print(resultado)