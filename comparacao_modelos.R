library(data.table)

# Resultados dos modelos avaliados em 2025
comparacao <- data.table(
  CATEGORIA = rep(
    c("Humanas e Linguagens", "Exatas e Natureza", "Redação"),
    each = 4
  ),

  MODELO = rep(
    c("Regressão Linear", "Random Forest",
      "XGBoost", "Baseline"),
    times = 3
  ),

  MAE = c(
    1.820393, 3.187672, 8.278717, 4.167005,
    5.626019, 2.737876, 5.124276, 2.477796,
    58.442675, 39.745308, 30.098594, 41.677983
  ),

  RMSE = c(
    2.144751, 3.598647, 8.955317, 4.303662,
    6.520995, 3.708811, 5.842037, 3.550753,
    59.065455, 40.321098, 32.165473, 42.082584
  )
)

# Identificar o melhor modelo de cada categoria
comparacao[
  , MELHOR_MODELO := MAE == min(MAE),
  by = CATEGORIA
]

# Ordenar os modelos pelo menor erro
setorder(comparacao, CATEGORIA, MAE)

# Criar pasta de resultados
dir.create("resultados", showWarnings = FALSE)

# Salvar a tabela
fwrite(
  comparacao,
  "resultados/comparacao_modelos_2025.csv"
)

# Exibir resultados
cat("\n--- COMPARAÇÃO DOS MODELOS ---\n")
print(comparacao)

cat("\n--- MELHORES MODELOS ---\n")
print(comparacao[MELHOR_MODELO == TRUE])

cat("\nTabela salva com sucesso!\n")