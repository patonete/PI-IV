library(data.table)
library(xgboost)

# Carregar dados completos
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

dados[, REGIAO := factor(REGIAO)]

# Criar registros para 2026
futuro <- data.table(
  NU_ANO = 2026,
  REGIAO = factor(
    levels(dados$REGIAO),
    levels = levels(dados$REGIAO)
  )
)

# 1. HUMANAS E LINGUAGENS: REGRESSÃO LINEAR

modelo_humanas <- lm(
  MEDIA_HUMANAS ~ NU_ANO + REGIAO,
  data = dados
)

futuro[, PREV_HUMANAS := as.numeric(
  predict(modelo_humanas, newdata = futuro)
)]

# 2. EXATAS E NATUREZA: BASELINE

# Repetir a média de 2025 para cada região
medias_2025 <- dados[
  NU_ANO == 2025,
  .(REGIAO, PREV_EXATAS = MEDIA_EXATAS)
]

futuro <- merge(
  futuro,
  medias_2025,
  by = "REGIAO",
  sort = FALSE
)

# 3. REDAÇÃO: XGBOOST

regioes <- levels(dados$REGIAO)

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

  matriz[, 1] <- base$NU_ANO - 2020

  for (i in seq_along(regioes)) {
    matriz[, i + 1] <- as.integer(
      as.character(base$REGIAO) == regioes[i]
    )
  }

  matriz
}

x_treino <- criar_matriz(dados)
x_futuro <- criar_matriz(futuro)

set.seed(42)

modelo_redacao <- xgboost(
  data = x_treino,
  label = dados$MEDIA_REDACAO,
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = 2,
  eta = 0.05,
  subsample = 1,
  colsample_bytree = 1,
  nthread = 2,
  verbose = 0
)

futuro[, PREV_REDACAO := as.numeric(
  predict(modelo_redacao, x_futuro)
)]

# Organizar e salvar resultados
setorder(futuro, REGIAO)

resultado <- futuro[, .(
  NU_ANO,
  REGIAO,
  PREV_HUMANAS,
  PREV_EXATAS,
  PREV_REDACAO
)]

dir.create("resultados", showWarnings = FALSE)

fwrite(
  resultado,
  "resultados/previsoes_enem_2026.csv"
)

cat("\n--- PREVISÕES DO ENEM PARA 2026 ---\n")
print(resultado)