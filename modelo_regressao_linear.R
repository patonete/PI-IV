library(data.table)

# Carregar dados
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Separar treinamento e teste
treino <- dados[NU_ANO <= 2024]
teste <- dados[NU_ANO == 2025]

# Converter região em fator
treino[, REGIAO := factor(REGIAO)]

teste[, REGIAO := factor(
  REGIAO,
  levels = levels(treino$REGIAO)
)]

# Categorias que serão previstas
categorias <- c(
  "MEDIA_HUMANAS",
  "MEDIA_EXATAS",
  "MEDIA_REDACAO"
)

# Criar pasta para resultados
dir.create("resultados", showWarnings = FALSE)

# Armazenar métricas e previsões
metricas <- list()
previsoes <- list()

# Treinar um modelo para cada categoria
for (categoria in categorias) {

  cat("\nCategoria:", categoria, "\n")

  formula <- as.formula(
    paste(categoria, "~ NU_ANO + REGIAO")
  )

  # Treinar regressão linear
  modelo <- lm(
    formula,
    data = treino
  )

  # Prever as médias de 2025
  previsto <- predict(
    modelo,
    newdata = teste
  )

  real <- teste[[categoria]]

  # Calcular métricas
  mae <- mean(abs(real - previsto))
  rmse <- sqrt(mean((real - previsto)^2))

  # Armazenar métricas
  metricas[[categoria]] <- data.table(
    MODELO = "Regressão Linear",
    CATEGORIA = categoria,
    MAE = mae,
    RMSE = rmse
  )

  # Armazenar previsões
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

# Salvar arquivos
fwrite(
  resultado_metricas,
  "resultados/metricas_regressao_linear.csv"
)

fwrite(
  resultado_previsoes,
  "resultados/previsoes_regressao_linear_2025.csv"
)

cat("\n--- RESULTADOS FINAIS ---\n")
print(resultado_metricas)

cat("\n--- PREVISÕES POR REGIÃO ---\n")
print(resultado_previsoes)