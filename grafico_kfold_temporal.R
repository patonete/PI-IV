library(data.table)
library(ggplot2)

# Carregar os resultados da validação temporal
dados <- fread(
  "resultados/metricas_kfold_temporal.csv"
)

# Organizar os nomes das categorias
dados[, CATEGORIA := factor(
  CATEGORIA,
  levels = c(
    "MEDIA_HUMANAS",
    "MEDIA_EXATAS",
    "MEDIA_REDACAO"
  ),
  labels = c(
    "Humanas e Linguagens",
    "Exatas e Natureza",
    "Redação"
  )
)]

# Organizar os modelos
dados[, MODELO := factor(
  MODELO,
  levels = c(
    "Regressão Linear",
    "Random Forest",
    "XGBoost"
  )
)]

# Destacar o menor MAE de cada ano e categoria
dados[
  ,
  DESTAQUE := ifelse(
    MAE == min(MAE),
    "Menor MAE",
    "Demais modelos"
  ),
  by = .(CATEGORIA, ANO_TESTE)
]

# Criar o gráfico
grafico <- ggplot(
  dados,
  aes(
    x = factor(ANO_TESTE),
    y = MAE,
    fill = MODELO
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  facet_wrap(
    ~ CATEGORIA,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Regressão Linear" = "#9CA3AF",
      "Random Forest" = "#F59E0B",
      "XGBoost" = "#2563EB"
    )
  ) +
  labs(
    title = "Validação Temporal dos Modelos - ENEM",
    subtitle = "Comparação do MAE em cada ano de teste",
    x = "Ano de teste",
    y = "Erro Médio Absoluto (MAE)",
    fill = "Modelo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# Salvar o gráfico
dir.create("graficos", showWarnings = FALSE)

ggsave(
  "graficos/validacao_kfold_temporal.png",
  grafico,
  width = 14,
  height = 6,
  dpi = 300,
  bg = "white"
)

print(grafico)