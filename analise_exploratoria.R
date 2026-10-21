
library(data.table)

# Carregar estatisticas ja calculadas
dados <- fread(
  "resultados/estatisticas_enem_2020_2025.csv"
)

# Selecionar apenas 2020 e 2025
comparacao <- dados[
  NU_ANO %in% c(2020, 2025),
  .(NU_ANO, REGIAO, AREA, MEDIA)
]

# Organizar as medias dos dois anos
comparacao <- dcast(
  comparacao,
  REGIAO + AREA ~ NU_ANO,
  value.var = "MEDIA"
)

# Renomear colunas
setnames(
  comparacao,
  c("2020", "2025"),
  c("MEDIA_2020", "MEDIA_2025")
)

# Calcular variacao absoluta em pontos
comparacao[
  ,
  VARIACAO_ABSOLUTA := MEDIA_2025 - MEDIA_2020
]

# Calcular variacao percentual
comparacao[
  ,
  VARIACAO_PERCENTUAL :=
    (VARIACAO_ABSOLUTA / MEDIA_2020) * 100
]

# Organizar resultados
setorder(comparacao, AREA, REGIAO)

# Arredondar valores
colunas_numericas <- c(
  "MEDIA_2020",
  "MEDIA_2025",
  "VARIACAO_ABSOLUTA",
  "VARIACAO_PERCENTUAL"
)

comparacao[
  ,
  (colunas_numericas) :=
    lapply(.SD, round, digits = 2),
  .SDcols = colunas_numericas
]

# Salvar tabela
dir.create("resultados", showWarnings = FALSE)

fwrite(
  comparacao,
  "resultados/variacao_medias_2020_2025.csv"
)

cat("\n===== VARIACAO DAS MEDIAS 2020-2025 =====\n")

print(comparacao)

cat("\nTotal de combinacoes:", nrow(comparacao), "\n")
cat("\nAnalise concluida!\n")
