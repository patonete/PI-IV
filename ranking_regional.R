
library(data.table)

# Carregar estatisticas ja calculadas
dados <- fread(
  "resultados/estatisticas_enem_2020_2025.csv"
)

# Criar ranking por ano e area
ranking <- copy(dados)

ranking[
  ,
  POSICAO := frank(-MEDIA, ties.method = "min"),
  by = .(NU_ANO, AREA)
]

# Ordenar resultados
setorder(ranking, AREA, NU_ANO, POSICAO)

# Selecionar colunas principais
ranking <- ranking[
  ,
  .(
    NU_ANO,
    AREA,
    POSICAO,
    REGIAO,
    MEDIA
  )
]

# Salvar ranking completo
fwrite(
  ranking,
  "resultados/ranking_regional_2020_2025.csv"
)

# Identificar regioes em primeiro lugar
primeiros <- ranking[
  POSICAO == 1
]

cat("\n===== REGIOES COM MAIORES MEDIAS =====\n")
print(primeiros)

# Contar quantas vezes cada regiao ficou em primeiro
resumo <- primeiros[
  ,
  .(VEZES_EM_PRIMEIRO = .N),
  by = REGIAO
][order(-VEZES_EM_PRIMEIRO)]

cat("\n===== RESUMO DO RANKING =====\n")
print(resumo)

cat("\nTotal de posicoes:", nrow(ranking), "\n")
cat("\nRanking concluido!\n")
