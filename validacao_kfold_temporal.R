# ============================================================
# VALIDACAO CRUZADA TEMPORAL - ENEM 2020 A 2025
# Modelos: Regressao Linear, Random Forest e XGBoost
# ============================================================

library(data.table)
library(randomForest)
library(xgboost)

set.seed(42)

# 1. Carregar os dados
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# 2. Definir categorias
categorias <- c(
  "MEDIA_HUMANAS",
  "MEDIA_EXATAS",
  "MEDIA_REDACAO"
)

# 3. Definir os anos de teste
anos_teste <- 2022:2025

# Manter a mesma codificacao das regioes
regioes <- sort(unique(dados$REGIAO))

dados[, REGIAO := factor(REGIAO, levels = regioes)]

# 4. Criar tabelas de resultados
resultados_kfold <- data.table()
previsoes_kfold <- data.table()

# 5. Executar os quatro folds
for (ano_teste in anos_teste) {

  cat("\n==================================\n")
  cat("FOLD - TESTE:", ano_teste, "\n")
  cat("==================================\n")

  # Separacao temporal
  treino <- dados[NU_ANO < ano_teste]
  teste <- dados[NU_ANO == ano_teste]

  # Preparar variaveis para o XGBoost
  x_treino <- model.matrix(
    ~ NU_ANO + REGIAO - 1,
    data = treino
  )

  x_teste <- model.matrix(
    ~ NU_ANO + REGIAO - 1,
    data = teste
  )

  for (categoria in categorias) {

    y_treino <- treino[[categoria]]
    y_teste <- teste[[categoria]]

    formula_modelo <- as.formula(
      paste(categoria, "~ NU_ANO + REGIAO")
    )

    # ------------------------------------
    # REGRESSAO LINEAR
    # ------------------------------------
    modelo_linear <- lm(
      formula_modelo,
      data = treino
    )

    prev_linear <- as.numeric(
      predict(modelo_linear, newdata = teste)
    )

    # ------------------------------------
    # RANDOM FOREST
    # ------------------------------------
    set.seed(42)

    modelo_rf <- randomForest(
      formula_modelo,
      data = treino,
      ntree = 500
    )

    prev_rf <- as.numeric(
      predict(modelo_rf, newdata = teste)
    )

    # ------------------------------------
    # XGBOOST
    # ------------------------------------
    set.seed(42)

    modelo_xgb <- xgboost(
      data = x_treino,
      label = y_treino,
      objective = "reg:squarederror",
      nrounds = 100,
      max_depth = 2,
      eta = 0.1,
      verbose = 0
    )

    prev_xgb <- as.numeric(
      predict(modelo_xgb, x_teste)
    )

    # ------------------------------------
    # ORGANIZAR PREVISOES
    # ------------------------------------
    modelos <- list(
      "Regressão Linear" = prev_linear,
      "Random Forest" = prev_rf,
      "XGBoost" = prev_xgb
    )

    for (nome_modelo in names(modelos)) {

      previsto <- modelos[[nome_modelo]]

      # Calcular metricas
      mae <- mean(abs(y_teste - previsto))

      rmse <- sqrt(
        mean((y_teste - previsto)^2)
      )

      r2 <- 1 -
        sum((y_teste - previsto)^2) /
        sum((y_teste - mean(y_teste))^2)

      # Salvar metricas do fold
      resultados_kfold <- rbind(
        resultados_kfold,
        data.table(
          ANO_TESTE = ano_teste,
          CATEGORIA = categoria,
          MODELO = nome_modelo,
          MAE = mae,
          RMSE = rmse,
          R2 = r2
        )
      )

      # Salvar valores reais e previstos
      previsoes_kfold <- rbind(
        previsoes_kfold,
        data.table(
          ANO_TESTE = ano_teste,
          REGIAO = as.character(teste$REGIAO),
          CATEGORIA = categoria,
          MODELO = nome_modelo,
          REAL = y_teste,
          PREVISTO = previsto,
          ERRO_ABSOLUTO = abs(y_teste - previsto)
        )
      )
    }
  }
}

# 6. Calcular metricas gerais dos quatro folds
resumo_kfold <- previsoes_kfold[
  ,
  .(
    MAE = mean(abs(REAL - PREVISTO)),
    RMSE = sqrt(mean((REAL - PREVISTO)^2)),
    R2 = 1 -
      sum((REAL - PREVISTO)^2) /
      sum((REAL - mean(REAL))^2)
  ),
  by = .(CATEGORIA, MODELO)
]

# 7. Identificar o menor MAE por categoria
resumo_kfold[
  ,
  MELHOR_MODELO := MAE == min(MAE),
  by = CATEGORIA
]

# 8. Arredondar resultados
resultados_kfold[
  ,
  c("MAE", "RMSE", "R2") :=
    lapply(.SD, round, 4),
  .SDcols = c("MAE", "RMSE", "R2")
]

resumo_kfold[
  ,
  c("MAE", "RMSE", "R2") :=
    lapply(.SD, round, 4),
  .SDcols = c("MAE", "RMSE", "R2")
]

# 9. Organizar resultados
setorder(
  resultados_kfold,
  CATEGORIA,
  ANO_TESTE,
  MAE
)

setorder(
  resumo_kfold,
  CATEGORIA,
  MAE
)

# 10. Salvar arquivos
dir.create("resultados", showWarnings = FALSE)

fwrite(
  resultados_kfold,
  "resultados/metricas_kfold_temporal.csv"
)

fwrite(
  previsoes_kfold,
  "resultados/previsoes_kfold_temporal.csv"
)

fwrite(
  resumo_kfold,
  "resultados/resumo_kfold_temporal.csv"
)

# 11. Exibir resultados
cat("\n==================================\n")
cat("RESULTADOS POR FOLD\n")
cat("==================================\n")

print(resultados_kfold)

cat("\n==================================\n")
cat("RESULTADOS GERAIS\n")
cat("==================================\n")

print(resumo_kfold)

cat("\nArquivos salvos com sucesso!\n")