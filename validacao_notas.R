library(data.table)

notas <- c(
  "NU_NOTA_CN",
  "NU_NOTA_CH",
  "NU_NOTA_LC",
  "NU_NOTA_MT",
  "NU_NOTA_REDACAO"
)

resultados <- list()

for (ano in 2021:2025) {

  cat("\nVerificando ano:", ano, "\n")

  arquivo <- file.path(
    "dados_tratados",
    paste0("enem_tratado_", ano, ".csv")
  )

  dados <- fread(
    arquivo,
    select = notas,
    showProgress = FALSE
  )

  fora_intervalo <- sum(
    vapply(dados, function(x) {
      sum(x < 0 | x > 1000, na.rm = TRUE)
    }, numeric(1))
  )

  sem_nota <- sum(
    rowSums(!is.na(dados)) == 0
  )

  resultados[[as.character(ano)]] <- data.table(
    ANO = ano,
    REGISTROS = nrow(dados),
    FORA_INTERVALO = fora_intervalo,
    SEM_NENHUMA_NOTA = sem_nota
  )

  rm(dados)
  gc(verbose = FALSE)
}

resultado_final <- rbindlist(resultados)

cat("\n===== RESULTADO DA VALIDACAO =====\n")
print(resultado_final)

if (
  all(resultado_final$FORA_INTERVALO == 0) &&
  all(resultado_final$SEM_NENHUMA_NOTA == 0)
) {
  cat("\nVALIDACAO: APROVADA\n")
} else {
  cat("\nVALIDACAO: VERIFICAR INCONSISTENCIAS\n")
}