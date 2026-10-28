library(data.table)

# Juntar os resultados dos quatro métodos
comparacao_train_test <- rbindlist(
  list(
    resultados,
    resultados_rf,
    resultados_xgb,
    resultados_baseline
  ),
  use.names = TRUE
)

# Identificar o melhor modelo pelo MAE em cada categoria
comparacao_train_test[
  ,
  MELHOR_MODELO := MAE == min(MAE),
  by = CATEGORIA
]

# Ordenar os resultados
setorder(comparacao_train_test, CATEGORIA, MAE)

# Salvar o arquivo
fwrite(
  comparacao_train_test,
  "resultados/comparacao_train_test_70_30.csv"
)

print(comparacao_train_test)