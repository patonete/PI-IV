
# Instalar ggplot2 somente se necessario
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(data.table)
library(ggplot2)

# Criar pasta para os graficos
dir.create("graficos", showWarnings = FALSE)

# Carregar resultados estatisticos
dados <- fread(
  "resultados/estatisticas_enem_2020_2025.csv"
)

# Nomes das areas
nomes_areas <- c(
  NU_NOTA_CN = "Ciencias da Natureza",
  NU_NOTA_CH = "Ciencias Humanas",
  NU_NOTA_LC = "Linguagens",
  NU_NOTA_MT = "Matematica",
  NU_NOTA_REDACAO = "Redacao"
)

# Criar um grafico para cada area
for (area in names(nomes_areas)) {

  dados_area <- dados[AREA == area]

  grafico <- ggplot(
    dados_area,
    aes(
      x = NU_ANO,
      y = MEDIA,
      color = REGIAO,
      group = REGIAO
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_x_continuous(breaks = 2020:2025) +
    labs(
      title = paste(
        "Evolucao das medias -",
        nomes_areas[[area]]
      ),
      x = "Ano",
      y = "Media da nota",
      color = "Regiao"
    ) +
    theme_minimal(base_size = 13)

  # Salvar grafico em PNG
  ggsave(
    filename = file.path(
      "graficos",
      paste0("evolucao_", area, ".png")
    ),
    plot = grafico,
    width = 11,
    height = 6,
    dpi = 300
  )

  cat("Grafico salvo:", nomes_areas[[area]], "\n")
}

cat("\nTODOS OS GRAFICOS FORAM GERADOS!\n")
