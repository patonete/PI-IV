library(data.table)
library(ggplot2)

# Carregar dados históricos e previsões
historico <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

previsoes <- fread(
  "resultados/previsoes_enem_2026.csv"
)

dir.create("graficos", showWarnings = FALSE)

cores <- c(
  "Norte" = "#E69F00",
  "Nordeste" = "#D55E00",
  "Centro-Oeste" = "#009E73",
  "Sudeste" = "#0072B2",
  "Sul" = "#CC79A7"
)

criar_grafico <- function(
  coluna_historica,
  coluna_previsao,
  titulo,
  arquivo
) {

  # Histórico de 2020 a 2025
  observado <- historico[, .(
    NU_ANO,
    REGIAO,
    MEDIA = get(coluna_historica)
  )]

  # Previsões de 2026
  previsto <- previsoes[, .(
    NU_ANO,
    REGIAO,
    MEDIA = get(coluna_previsao)
  )]

  # Conectar o último ano observado à previsão
  conexao <- rbindlist(list(
    observado[NU_ANO == 2025],
    previsto
  ))

  grafico <- ggplot() +

    # Linhas históricas
    geom_line(
      data = observado,
      aes(
        x = NU_ANO,
        y = MEDIA,
        color = REGIAO,
        group = REGIAO
      ),
      linewidth = 1.1
    ) +

    # Pontos históricos
    geom_point(
      data = observado,
      aes(
        x = NU_ANO,
        y = MEDIA,
        color = REGIAO
      ),
      size = 2.5
    ) +

    # Linhas tracejadas para 2026
    geom_line(
      data = conexao,
      aes(
        x = NU_ANO,
        y = MEDIA,
        color = REGIAO,
        group = REGIAO
      ),
      linetype = "dashed",
      linewidth = 1.2
    ) +

    # Pontos previstos
    geom_point(
      data = previsto,
      aes(
        x = NU_ANO,
        y = MEDIA,
        color = REGIAO
      ),
      shape = 17,
      size = 3.5
    ) +

    geom_vline(
      xintercept = 2025.5,
      linetype = "dotted",
      color = "gray50"
    ) +

    scale_color_manual(values = cores) +

    scale_x_continuous(
      breaks = 2020:2026
    ) +

    labs(
      title = titulo,
      subtitle = "Linhas contínuas: dados reais | Linhas tracejadas: previsão",
      x = "Ano",
      y = "Média das notas",
      color = "Região",
      caption = "Fonte: Microdados do ENEM 2020–2025 | Previsões próprias para 2026"
    ) +

    theme_minimal(base_size = 13) +

    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  ggsave(
    paste0("graficos/", arquivo),
    plot = grafico,
    width = 12,
    height = 7,
    dpi = 300
  )

  print(grafico)
}

# 1. Humanas e Linguagens
criar_grafico(
  "MEDIA_HUMANAS",
  "PREV_HUMANAS",
  "Evolução e Previsão de Humanas e Linguagens (2020–2026)",
  "previsao_humanas_2026.png"
)

# 2. Exatas e Natureza
criar_grafico(
  "MEDIA_EXATAS",
  "PREV_EXATAS",
  "Evolução e Previsão de Exatas e Natureza (2020–2026)",
  "previsao_exatas_2026.png"
)

# 3. Redação
criar_grafico(
  "MEDIA_REDACAO",
  "PREV_REDACAO",
  "Evolução e Previsão de Redação (2020–2026)",
  "previsao_redacao_2026.png"
)

cat("\nOs três gráficos de previsão foram gerados!\n")