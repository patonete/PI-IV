
library(data.table)

dados <- fread(
  "resultados/estatisticas_enem_2020_2025.csv"
)

# Ordenar por area, regiao e ano
setorder(dados, AREA, REGIAO, NU_ANO)

# Calcular a variacao em relacao ao ano anterior
dados[
  ,
  `:=`(
    MEDIA_ANTERIOR = shift(MEDIA),
    VARIACAO_ANUAL = MEDIA - shift(MEDIA)
  ),
  by = .(AREA, REGIAO)
]

# Manter somente os anos com comparacao
variacoes <- dados[
  !is.na(VARIACAO_ANUAL),
  .(
    NU_ANO,
    REGIAO,
    AREA,
    MEDIA_ANTERIOR,
    MEDIA,
    VARIACAO_ANUAL
  )
]

# Salvar resultados completos
fwrite(
  variacoes,
  "resultados/variacoes_anuais_2021_2025.csv"
)

# Maior aumento de cada area
maiores_altas <- variacoes[
  ,
  .SD[which.max(VARIACAO_ANUAL)],
  by = AREA
]

# Maior queda de cada area
maiores_quedas <- variacoes[
  ,
  .SD[which.min(VARIACAO_ANUAL)],
  by = AREA
]

cat("\n===== MAIORES ALTAS ANUAIS =====\n")
print(maiores_altas)

cat("\n===== MAIORES QUEDAS ANUAIS =====\n")
print(maiores_quedas)

cat("\nTotal de variacoes calculadas:",
    nrow(variacoes), "\n")

cat("\nANALISE CONCLUIDA!\n")
