library(data.table)
library(ggplot2)

# Carregar os resultados dos modelos
dados <- fread("resultados/comparacao_modelos_2025.csv")

# Padronizar os nomes das categorias
dados[, CATEGORIA := as.character(CATEGORIA)]

dados[CATEGORIA == "MEDIA_HUMANAS",
      CATEGORIA := "Humanas e Linguagens"]

dados[CATEGORIA == "MEDIA_EXATAS",
      CATEGORIA := "Exatas e Natureza"]

dados[CATEGORIA == "MEDIA_REDACAO",
      CATEGORIA := "Redação"]

# Padronizar os nomes dos modelos
dados[, MODELO := as.character(MODELO)]

dados[grepl("linear", MODELO, ignore.case = TRUE),
      MODELO := "Regressão Linear"]

dados[grepl("forest", MODELO, ignore.case = TRUE),
      MODELO := "Random Forest"]

dados[grepl("xgboost", MODELO, ignore.case = TRUE),
      MODELO := "XGBoost"]

dados[grepl("baseline", MODELO, ignore.case = TRUE),
      MODELO := "Baseline"]

# Definir a ordem dos modelos
dados[, MODELO := factor(
  MODELO,
  levels = c(
    "Regressão Linear",
    "Random Forest",
    "XGBoost",
    "Baseline"
  )
)]

# Definir a ordem das categorias
dados[, CATEGORIA := factor(
  CATEGORIA,
  levels = c(
    "Exatas e Natureza",
    "Humanas e Linguagens",
    "Redação"
  )
)]

# Destacar o melhor modelo treinado de cada categoria
# O Baseline continua no gráfico apenas como referência
dados[, DESTAQUE := "Demais métodos"]

dados[
  CATEGORIA == "Exatas e Natureza" &
  MODELO == "Random Forest",
  DESTAQUE := "Melhor modelo treinado"
]

dados[
  CATEGORIA == "Humanas e Linguagens" &
  MODELO == "Regressão Linear",
  DESTAQUE := "Melhor modelo treinado"
]

dados[
  CATEGORIA == "Redação" &
  MODELO == "XGBoost",
  DESTAQUE := "Melhor modelo treinado"
]

# Criar o gráfico
grafico <- ggplot(
  dados,
  aes(
    x = MODELO,
    y = MAE,
    fill = DESTAQUE
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = sprintf("%.2f", MAE)),
    vjust = -0.4,
    size = 3.5
  ) +
  facet_wrap(
    ~ CATEGORIA,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c(
      "Melhor modelo treinado" = "#00A67A",
      "Demais métodos" = "#8093AD"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Comparação do MAE dos Modelos Preditivos",
    subtitle = "Avaliação das previsões para 2025",
    x = NULL,
    y = "Erro Médio Absoluto (MAE)",
    fill = NULL,
    caption = paste(
      "Fonte: Elaboração própria |",
      "Destaque: menor MAE entre os modelos treinados"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# Criar a pasta caso não exista
dir.create("graficos", showWarnings = FALSE)

# Salvar o gráfico
ggsave(
  "graficos/comparacao_mae_modelos_2025.png",
  plot = grafico,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "white"
)

# Exibir
print(grafico)

cat("\nGráfico atualizado com sucesso!\n")