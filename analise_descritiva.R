
library(data.table)

# Criar pasta de resultados
dir.create("resultados", showWarnings = FALSE)

# Areas do ENEM
areas <- c(
  "NU_NOTA_CN",
  "NU_NOTA_CH",
  "NU_NOTA_LC",
  "NU_NOTA_MT",
  "NU_NOTA_REDACAO"
)

resultados <- list()

# Processar um ano por vez
for (ano in 2020:2025) {

  cat("\nProcessando ano:", ano, "\n")

  arquivo <- file.path(
    "dados_tratados",
    paste0("enem_tratado_", ano, ".csv")
  )

  dados <- fread(
    arquivo,
    select = c("NU_ANO", "REGIAO", areas),
    showProgress = FALSE
  )

  # Calcular estatisticas por area e regiao
  for (area in areas) {

    dados_area <- dados[
      !is.na(get(area)),
      .(NU_ANO, REGIAO, NOTA = as.numeric(get(area)))
    ]

    estatisticas <- dados_area[
      ,
      .(
        N = .N,
        MEDIA = as.numeric(mean(NOTA)),
        MEDIANA = as.numeric(median(NOTA)),
        DESVIO_PADRAO = as.numeric(sd(NOTA))
      ),
      by = .(NU_ANO, REGIAO)
    ]

    estatisticas[, AREA := area]

    resultados[[paste(ano, area, sep = "_")]] <- estatisticas

    rm(dados_area, estatisticas)
  }

  rm(dados)
  gc(verbose = FALSE)
}

# Unificar os resultados
resultado_final <- rbindlist(resultados)

setcolorder(
  resultado_final,
  c(
    "NU_ANO",
    "REGIAO",
    "AREA",
    "N",
    "MEDIA",
    "MEDIANA",
    "DESVIO_PADRAO"
  )
)

setorder(resultado_final, AREA, NU_ANO, REGIAO)

# Verificar quantidade de combinacoes
cat("\n===== RESUMO FINAL =====\n")

cat("Total de combinacoes:", nrow(resultado_final), "\n")

print(resultado_final[, .N, by = AREA])

if (nrow(resultado_final) != 150) {
  stop("ERRO: Esperadas 150 combinacoes.")
}

if (anyNA(resultado_final)) {
  stop("ERRO: Existem valores ausentes nas estatisticas.")
}

# Salvar resultados
fwrite(
  resultado_final,
  "resultados/estatisticas_enem_2020_2025.csv"
)

cat("\nANALISE CONCLUIDA COM SUCESSO!\n")
cat("Arquivo salvo em resultados/estatisticas_enem_2020_2025.csv\n")
