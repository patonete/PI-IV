library(data.table)

arquivos <- file.path(
  "dados_tratados",
  paste0("enem_tratado_", 2020:2025, ".csv")
)

esperados <- c(
  2762899, 2386116, 2504014,
  2836806, 3182843, 3473543
)

resultado <- lapply(seq_along(arquivos), function(i) {

  arquivo <- arquivos[i]

  dados <- fread(
    arquivo,
    select = c("NU_ANO", "REGIAO"),
    showProgress = FALSE
  )

  verificacao <- data.frame(
    ANO = 2020 + i - 1,
    REGISTROS = nrow(dados),
    ESPERADOS = esperados[i],
    COLUNAS = length(names(fread(arquivo, nrows = 0))),
    ANO_CORRETO = all(dados$NU_ANO == 2020 + i - 1),
    REGIOES = uniqueN(dados$REGIAO)
  )

  rm(dados)
  gc(verbose = FALSE)

  verificacao
})

resultado <- rbindlist(resultado)

resultado[, REGISTROS_OK := REGISTROS == ESPERADOS]

print(resultado)

cat("\nVALIDACAO GERAL:",
    if (all(resultado$REGISTROS_OK) &&
        all(resultado$COLUNAS == 13) &&
        all(resultado$ANO_CORRETO) &&
        all(resultado$REGIOES == 5)) {
      "APROVADA"
    } else {
      "VERIFICAR INCONSISTENCIAS"
    },
    "\n")