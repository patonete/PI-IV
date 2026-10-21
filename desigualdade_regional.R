
library(data.table)

# Carregar estatisticas do ENEM
dados <- fread(
  "resultados/estatisticas_enem_2020_2025.csv"
)

# Calcular a maior e menor media por ano e area
diferencas <- dados[
  ,
  .(
    MAIOR_MEDIA = max(MEDIA),
    MENOR_MEDIA = min(MEDIA),
    REGIAO_MAIOR = REGIAO[which.max(MEDIA)],
    REGIAO_MENOR = REGIAO[which.min(MEDIA)]
  ),
  by = .(NU_ANO, AREA)
]

# Calcular a diferenca entre maior e menor media
diferencas[
  ,
  DIFERENCA := MAIOR_MEDIA - MENOR_MEDIA
]

# Organizar resultados
setorder(diferencas, AREA, NU_ANO)

# Comparar 2020 e 2025
comparacao <- dcast(
  diferencas[NU_ANO %in% c(2020, 2025)],
  AREA ~ NU_ANO,
  value.var = "DIFERENCA"
)

# Renomear colunas
setnames(
  comparacao,
  c("2020", "2025"),
  c("DIFERENCA_2020", "DIFERENCA_2025")
)

# Calcular a variacao da diferenca regional
comparacao[
  ,
  VARIACAO_DIFERENCA :=
    DIFERENCA_2025 - DIFERENCA_2020
]

# Arredondar valores para apresentacao
colunas_numericas <- c(
  "DIFERENCA_2020",
  "DIFERENCA_2025",
  "VARIACAO_DIFERENCA"
)

comparacao[
  ,
  (colunas_numericas) := lapply(.SD, round, digits = 2),
  .SDcols = colunas_numericas
]

# Criar pasta de resultados caso nao exista
dir.create("resultados", showWarnings = FALSE)

# Salvar as diferencas de todos os anos
fwrite(
  diferencas,
  "resultados/diferencas_regionais_2020_2025.csv"
)

# Salvar comparacao entre 2020 e 2025
fwrite(
  comparacao,
  "resultados/comparacao_diferencas_2020_2025.csv"
)

# Exibir resultados
cat("\n===== DIFERENCAS REGIONAIS =====\n")
print(diferencas)

cat("\n===== COMPARACAO 2020 x 2025 =====\n")
print(comparacao)

cat("\nTotal de combinacoes:", nrow(diferencas), "\n")
cat("\nANALISE CONCLUIDA COM SUCESSO!\n")
