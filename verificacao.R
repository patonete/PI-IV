arquivos <- file.path(
  "dados_originais",
  paste0("MICRODADOS_ENEM_", 2020:2025, ".csv")
)

resultado <- lapply(seq_along(arquivos), function(i) {

  cabecalho <- readLines(
    arquivos[i],
    n = 1,
    warn = FALSE
  )

  data.frame(
    ANO = 2020 + i - 1,
    COLUNAS = length(
      strsplit(cabecalho, ";", fixed = TRUE)[[1]]
    ),
    POSSUI_UF = grepl("SG_UF_PROVA", cabecalho),
    POSSUI_NOTA_MT = grepl("NU_NOTA_MT", cabecalho),
    POSSUI_NOTA_REDACAO = grepl("NU_NOTA_REDACAO", cabecalho)
  )
})

print(do.call(rbind, resultado), row.names = FALSE)