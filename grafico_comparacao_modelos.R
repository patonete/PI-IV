library(data.table)
library(ggplot2)

# Carregar os resultados dos modelos
dados <- fread(
  "resultados/comparacao_modelos_2025.csv"
)

# Organizar os nomes dos modelos
dados[, MODELO := factor(
  MODELO,
  levels = c(
    "Regressão Linear",
    "Random Forest",
    "XGBoost",
    "Baseline"
  )
)]

# Criar gráfico
grafico <- ggplot(
  dados,
  aes(
    x = MODELO,
    y = MAE,
    fill = MELHOR_MODELO
  )
) +
  geom_col(width = 0.65) +

  geom_text(
    aes(label = sprintf("%.2f", MAE)),
    vjust = -0.5,
    size = 4
  ) +

  facet_wrap(
    ~ CATEGORIA,
    scales = "free_y",
    ncol = 1
  ) +

  scale_fill_manual(
    values = c(
      "TRUE" = "#009E73",
      "FALSE" = "#7A8CA3"
    ),
    labels = c(
      "FALSE" = "Demais modelos",
      "TRUE" = "Melhor modelo"
    ),
    name = NULL
  ) +

  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +

  labs(
    title = "Comparação do MAE dos Modelos Preditivos",
    subtitle = "Avaliação das previsões para 2025",
    x = NULL,
    y = "Erro Médio Absoluto (MAE)",
    caption = "Fonte: Elaboração própria | Menor MAE indica melhor desempenho"
  ) +

  theme_minimal(base_size = 13) +

  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Criar pasta
dir.create("graficos", showWarnings = FALSE)

# Salvar imagem
ggsave(
  "graficos/comparacao_mae_modelos_2025.png",
  plot = grafico,
  width = 12,
  height = 11,
  dpi = 300,
  bg = "white"
)

print(grafico)

cat("\nGráfico comparativo salvo com sucesso!\n")