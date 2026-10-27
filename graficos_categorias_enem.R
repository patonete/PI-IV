library(data.table)
library(ggplot2)

# Carregar as médias calculadas
dados <- fread(
  "resultados/medias_categorias_enem_2020_2025.csv"
)

# Criar a pasta dos gráficos
dir.create("graficos", showWarnings = FALSE)

# Definir cores fixas para as regiões
cores <- c(
  "Norte" = "#E69F00",
  "Nordeste" = "#D55E00",
  "Centro-Oeste" = "#009E73",
  "Sudeste" = "#0072B2",
  "Sul" = "#CC79A7"
)

# Função para gerar os gráficos
criar_grafico <- function(coluna, titulo, arquivo) {

  grafico <- ggplot(
    dados,
    aes(
      x = NU_ANO,
      y = .data[[coluna]],
      color = REGIAO,
      group = REGIAO
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.8) +

    # Cores das regiões
    scale_color_manual(values = cores) +

    # Anos apresentados
    scale_x_continuous(
      breaks = 2020:2025
    ) +

    # Escala vertical ajustada automaticamente
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 6),
      expand = expansion(mult = c(0.08, 0.08))
    ) +

    # Títulos e legendas
    labs(
      title = titulo,
      subtitle = "Comparação entre as cinco regiões brasileiras",
      x = "Ano",
      y = "Média das notas",
      color = "Região",
      caption = paste(
        "Fonte: Microdados do ENEM 2020–2025",
        "| Elaboração própria"
      )
    ) +

    # Aparência
    theme_minimal(base_size = 13) +

    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        size = 12
      ),
      axis.title = element_text(
        size = 12
      ),
      axis.text = element_text(
        size = 11
      ),
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(
        color = "gray40",
        size = 9
      )
    )

  # Salvar o gráfico
  ggsave(
    filename = paste0("graficos/", arquivo),
    plot = grafico,
    width = 12,
    height = 7,
    dpi = 300,
    bg = "white"
  )

  # Mostrar no R
  print(grafico)
}

# Gráfico 1 — Humanas e Linguagens
criar_grafico(
  "MEDIA_HUMANAS",
  "Evolução das Médias de Humanas e Linguagens (2020–2025)",
  "evolucao_humanas_linguagens.png"
)

# Gráfico 2 — Exatas e Natureza
criar_grafico(
  "MEDIA_EXATAS",
  "Evolução das Médias de Exatas e Natureza (2020–2025)",
  "evolucao_exatas_natureza.png"
)

# Gráfico 3 — Redação
criar_grafico(
  "MEDIA_REDACAO",
  "Evolução das Médias de Redação (2020–2025)",
  "evolucao_redacao.png"
)

cat("\nOs três gráficos foram gerados com sucesso!\n")