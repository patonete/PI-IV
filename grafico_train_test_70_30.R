library(data.table)
library(ggplot2)

# Carregar os resultados do Train/Test
dados <- fread("resultados/comparacao_train_test_70_30.csv")

# Organizar os nomes das categorias
dados[
  ,
  CATEGORIA := factor(
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
  )
]

# Destacar o melhor modelo de cada categoria
dados[
  ,
  DESTAQUE := ifelse(
    MELHOR_MODELO,
    "Melhor modelo",
    "Outros modelos"
  )
]

# Criar o gráfico
grafico <- ggplot(
  dados,
  aes(
    x = reorder(MODELO, MAE),
    y = MAE,
    fill = DESTAQUE
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", MAE)),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip(clip = "off") +
  facet_wrap(
    ~ CATEGORIA,
    scales = "free_x"
  ) +
  scale_fill_manual(
    values = c(
      "Melhor modelo" = "#22A06B",
      "Outros modelos" = "#9CA3AF"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Comparação dos Modelos - Train/Test",
    subtitle = "Treinamento: 2020–2023 | Teste: 2024–2025",
    x = NULL,
    y = "Erro Médio Absoluto (MAE)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Criar a pasta, caso não exista
dir.create("graficos", showWarnings = FALSE)

# Salvar o gráfico atualizado
ggsave(
  "graficos/comparacao_train_test_70_30.png",
  plot = grafico,
  width = 14,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Exibir o gráfico
print(grafico)

cat("\nGráfico salvo em: graficos/comparacao_train_test_70_30.png\n")