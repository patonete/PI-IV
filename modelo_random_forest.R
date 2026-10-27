library(data.table)
library(randomForest)

# Carregar dados
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Separar treinamento e teste
treino <- copy(dados[NU_ANO <= 2024])
teste <- copy(dados[NU_ANO == 2025])

# Preparar variáveis
treino[, REGIAO := factor(REGIAO)]
teste[, REGIAO := factor(
  REGIAO,
  levels = levels(treino$REGIAO)
)]

# Categorias
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

  formula <- as.formula(
    paste(categoria, "~ NU_ANO + REGIAO")
  )

  # Treinar Random Forest
  modelo <- randomForest(
    formula,
    data = treino,
    ntree = 500,
    mtry = 2,
    nodesize = 2
  )

  # Prever 2025
  previsto <- predict(
    modelo,
    newdata = teste
  )

  real <- teste[[categoria]]

  # Calcular erros
  mae <- mean(abs(real - previsto))
  rmse <- sqrt(mean((real - previsto)^2))

  metricas[[categoria]] <- data.table(
    MODELO = "Random Forest",
    CATEGORIA = categoria,
    MAE = mae,
    RMSE = rmse
  )

  previsoes[[categoria]] <- data.table(
    ANO = 2025,
    REGIAO = as.character(teste$REGIAO),
    CATEGORIA = categoria,
    REAL = real,
    PREVISTO = as.numeric(previsto),
    ERRO_ABSOLUTO = abs(real - previsto)
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
  "resultados/metricas_random_forest.csv"
)

fwrite(
  resultado_previsoes,
  "resultados/previsoes_random_forest_2025.csv"
)

cat("\n--- RESULTADOS FINAIS ---\n")
print(resultado_metricas)

cat("\n--- PREVISÕES POR REGIÃO ---\n")
print(resultado_previsoes)