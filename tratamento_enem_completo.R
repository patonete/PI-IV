# TRATAMENTO DOS MICRODADOS DO ENEM 2020-2025
# Execute com a pasta Projeto_ENEM aberta no VS Code.
# Nao altera os arquivos originais e nao calcula estatisticas descritivas.

library(data.table)

pasta_entrada <- "dados_originais"
pasta_saida <- "dados_tratados"
dir.create(pasta_saida, showWarnings = FALSE, recursive = TRUE)

colunas <- c(
  "NU_ANO", "SG_UF_PROVA",
  "TP_PRESENCA_CN", "TP_PRESENCA_CH", "TP_PRESENCA_LC", "TP_PRESENCA_MT",
  "NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT",
  "NU_NOTA_REDACAO", "TP_STATUS_REDACAO"
)
notas <- c("NU_NOTA_CN", "NU_NOTA_CH", "NU_NOTA_LC", "NU_NOTA_MT", "NU_NOTA_REDACAO")
ufs_por_regiao <- list(
  Norte = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  Nordeste = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  `Centro-Oeste` = c("DF", "GO", "MT", "MS"),
  Sudeste = c("ES", "MG", "RJ", "SP"),
  Sul = c("PR", "RS", "SC")
)
mapa_regiao <- setNames(
  rep(names(ufs_por_regiao), lengths(ufs_por_regiao)),
  unlist(ufs_por_regiao, use.names = FALSE)
)

# Totais conferidos pela contagem de linhas nos CSVs locais.
esperados <- c(`2020` = 5783109, `2021` = 3389832,
               `2022` = 3476105, `2023` = 3933955,
               `2024` = 4332944, `2025` = 4810772)

resumo <- vector("list", length(espr <- 2020:2025))
for (i in seq_along(espr)) {
  ano <- espr[i]
  entrada <- file.path(pasta_entrada, sprintf("MICRODADOS_ENEM_%d.csv", ano))
  saida <- file.path(pasta_saida, sprintf("enem_tratado_%d.csv", ano))
  temporario <- paste0(saida, ".parcial")

  cat("\n======================\nANO:", ano, "\n")
  if (!file.exists(entrada)) stop("Arquivo nao encontrado: ", entrada)

  # Confere o cabecalho antes de ler o CSV inteiro.
  nomes_disponiveis <- names(fread(entrada, sep = ";", nrows = 0L, showProgress = FALSE))
  faltantes <- setdiff(colunas, nomes_disponiveis)
  if (length(faltantes)) stop("Colunas ausentes em ", ano, ": ", paste(faltantes, collapse = ", "))

  dados <- fread(
    entrada, sep = ";", select = colunas,
    na.strings = c("", "NA"),
    showProgress = TRUE
  )
  if (nrow(dados) != esperados[as.character(ano)]) {
    stop("Quantidade de linhas inesperada em ", ano, ": ", nrow(dados),
         " (esperado: ", esperados[as.character(ano)], ").")
  }
  if (anyNA(dados$NU_ANO) || any(dados$NU_ANO != ano)) {
    stop("NU_ANO inconsistente em ", ano)
  }
  if (any(!vapply(dados[, ..notas], is.numeric, logical(1)))) {
    stop("Ha coluna de notas que nao foi lida como numero em ", ano)
  }
  fora_intervalo <- vapply(dados[, ..notas], function(x) sum(!is.na(x) & (x < 0 | x > 1000)), numeric(1))
  if (any(fora_intervalo > 0)) {
    stop("Notas fora do intervalo 0-1000 em ", ano, ": ",
         paste(names(fora_intervalo)[fora_intervalo > 0], collapse = ", "))
  }

  # Registro sem UF em 2021 era descartado na metodologia original.
  uf_invalida <- is.na(dados$SG_UF_PROVA) | !(dados$SG_UF_PROVA %in% names(mapa_regiao))
  n_uf_invalida <- sum(uf_invalida)
  if (n_uf_invalida) {
    cat("Registros sem UF valida:", n_uf_invalida, "\n")
    if (ano != 2021 || n_uf_invalida != 1L) {
      stop("UF inesperada: revisar os registros antes de descartar qualquer um.")
    }
    dados <- dados[!uf_invalida]
  }

  # A regiao se refere ao local de realizacao da prova.
  dados[, REGIAO := unname(mapa_regiao[SG_UF_PROVA])]
  if (anyNA(dados$REGIAO)) stop("Falha ao classificar a regiao em ", ano)

  # Conserva quem tem pelo menos uma nota (incluindo nota zero).
  com_nota <- rowSums(!is.na(as.matrix(dados[, ..notas]))) > 0L
  n_sem_nota <- sum(!com_nota)
  dados <- dados[com_nota]
  rm(com_nota)

  # Ordem das 13 colunas do projeto.
  setcolorder(dados, c(colunas, "REGIAO"))
  if (ncol(dados) != 13L) stop("Numero incorreto de colunas em ", ano)

  cat("Registros analisaveis:", nrow(dados), "\n")
  cat("Sem nenhuma nota:", n_sem_nota, "\n")
  cat("Salvando:", saida, "\n")

  # Arquivo temporario evita confundir uma gravacao interrompida com o resultado final.
  if (file.exists(temporario)) file.remove(temporario)
  fwrite(dados, temporario, sep = ";", na = "", quote = TRUE)
  if (file.exists(saida) && !file.remove(saida)) stop("Nao foi possivel substituir: ", saida)
  if (!file.rename(temporario, saida)) stop("Falha ao finalizar: ", saida)

  resumo[[i]] <- data.table(
    ANO = ano,
    ORIGINAIS = as.integer(esperados[as.character(ano)]),
    UF_INVALIDA = n_uf_invalida,
    SEM_NENHUMA_NOTA = n_sem_nota,
    ANALITICOS = nrow(dados),
    CINCO_NOTAS = sum(rowSums(!is.na(as.matrix(dados[, ..notas]))) == 5L)
  )
  rm(dados)
  gc(verbose = FALSE)
}

relatorio <- rbindlist(resumo)
fwrite(relatorio, file.path(pasta_saida, "relatorio_validacao.csv"), sep = ";")
cat("\n===== RELATORIO FINAL =====\n")
print(relatorio)
cat("\nTOTAL ORIGINAL:", sum(relatorio$ORIGINAIS), "\n")
cat("TOTAL ANALITICO:", sum(relatorio$ANALITICOS), "\n")
cat("Os seis CSVs tratados foram salvos em:", normalizePath(pasta_saida), "\n")
