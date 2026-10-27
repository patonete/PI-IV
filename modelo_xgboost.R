library(data.table)
library(xgboost)

# Carregar os dados
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Separar treinamento e teste
treino <- copy(dados[NU_ANO <= 2024])
teste <- copy(dados[NU_ANO == 2025])

# Criar variáveis explicativas
# Ano centralizado para facilitar a interpretação
treino[, ANO_MODELO := NU_ANO - 2020]
teste[, ANO_MODELO := NU_ANO - 2020]

# Codificar regiões em variáveis numéricas
regioes <- sort(unique(dados$REGIAO))

criar_matriz <- function(base) {
  matriz <- matrix(
    0,
    nrow = nrow(base),
    ncol = length(regioes) + 1
  )

  colnames(matriz) <- c(
    "ANO_MODELO",
    paste0("REGIAO_", seq_along(regioes))
  )

  matriz[, 1] <- base$ANO_MODELO

  for (i in seq_along(regioes)) {
    matriz[, i + 1] <- as.integer(
      base$REGIAO == regioes[i]
    )
  }

  matriz
}

x_treino <- criar_matriz(treino)
x_teste <- criar_matriz(teste)

# Categorias analisadas
categorias <- c(
  "MEDIA_HUMANAS",
  "MEDIA_EXATAS",
  "MEDIA_REDACAO"
)

set.seed(42)

metricas <- list()
previsoes <- list()

for (categoria in categorias) {

  cat("\nCategoria:", categoria, "\n")

  y_treino <- treino[[categoria]]
  y_teste <- teste[[categoria]]

  # Treinar XGBoost
  modelo <- xgboost(
    data = x_treino,
    label = y_treino,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 2,
    eta = 0.05,
    subsample = 1,
    colsample_bytree = 1,
    nthread = 2,
    verbose = 0
  )

  # Prever 2025
  previsto <- predict(
    modelo,
    x_teste
  )

  # Calcular métricas
  mae <- mean(abs(y_teste - previsto))
  rmse <- sqrt(mean((y_teste - previsto)^2))

  metricas[[categoria]] <- data.table(
    MODELO = "XGBoost",
    CATEGORIA = categoria,
    MAE = mae,
    RMSE = rmse
  )

  previsoes[[categoria]] <- data.table(
    ANO = 2025,
    REGIAO = teste$REGIAO,
    CATEGORIA = categoria,
    REAL = y_teste,
    PREVISTO = as.numeric(previsto),
    ERRO_ABSOLUTO = abs(y_teste - previsto)
  )

  cat("MAE:", round(mae, 4), "\n")
  cat("RMSE:", round(rmse, 4), "\n")
}

# Consolidar resultados
resultado_metricas <- rbindlist(metricas)
resultado_previsoes <- rbindlist(previsoes)

# Salvar resultados
dir.create("resultados", showWarnings = FALSE)

fwrite(
  resultado_metricas,
  "resultados/metricas_xgboost.csv"
)

fwrite(
  resultado_previsoes,
  "resultados/previsoes_xgboost_2025.csv"
)

cat("\n--- RESULTADOS FINAIS ---\n")
print(resultado_metricas)

cat("\n--- PREVISÕES POR REGIÃO ---\n")
print(resultado_previsoes)