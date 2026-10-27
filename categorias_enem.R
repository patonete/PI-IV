library(data.table)

# Pasta de saída
dir.create("resultados", showWarnings = FALSE)

# Lista para armazenar os resultados
resultados <- list()

# Processar um ano por vez para economizar memória
for (ano in 2020:2025) {

  cat("\nProcessando ano:", ano, "\n")

  caminho <- paste0(
    "dados_tratados/enem_tratado_", ano, ".csv"
  )

  dados <- fread(
    caminho,
    select = c(
      "NU_ANO", "REGIAO",
      "NU_NOTA_CH", "NU_NOTA_LC",
      "NU_NOTA_MT", "NU_NOTA_CN",
      "NU_NOTA_REDACAO"
    )
  )

  # Criar as três categorias
  dados[, HUMANAS_LINGUAGENS := fifelse(
    !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_LC),
    (NU_NOTA_CH + NU_NOTA_LC) / 2,
    NA_real_
  )]

  dados[, EXATAS_NATUREZA := fifelse(
    !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_CN),
    (NU_NOTA_MT + NU_NOTA_CN) / 2,
    NA_real_
  )]

  dados[, REDACAO := NU_NOTA_REDACAO]

  # Calcular médias por ano e região
  resumo <- dados[, .(
    MEDIA_HUMANAS = if (all(is.na(HUMANAS_LINGUAGENS)))
      NA_real_ else mean(HUMANAS_LINGUAGENS, na.rm = TRUE),

    MEDIA_EXATAS = if (all(is.na(EXATAS_NATUREZA)))
      NA_real_ else mean(EXATAS_NATUREZA, na.rm = TRUE),

    MEDIA_REDACAO = if (all(is.na(REDACAO)))
      NA_real_ else mean(REDACAO, na.rm = TRUE),

    N_HUMANAS = sum(!is.na(HUMANAS_LINGUAGENS)),
    N_EXATAS = sum(!is.na(EXATAS_NATUREZA)),
    N_REDACAO = sum(!is.na(REDACAO))
  ), by = .(NU_ANO, REGIAO)]

  resultados[[as.character(ano)]] <- resumo

  rm(dados)
  gc(verbose = FALSE)
}

# Juntar os seis anos
categorias <- rbindlist(resultados)

setorder(categorias, NU_ANO, REGIAO)

# Salvar o resultado
fwrite(
  categorias,
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Mostrar os resultados
print(categorias)

cat("\nTotal de linhas:", nrow(categorias), "\n")