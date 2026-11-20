# Usa a última execução da janela expansiva na sessão R.
library(dplyr)
library(tidyr)
library(ggplot2)

perdas_basicos <- if (exists("basicos_media_gamma")) {
  basicos_media_gamma$perdas
} else {
  message("Usando as perdas salvas da auditoria da janela expansiva.")
  readRDS("tmp/perdas_media_auditoria_dm.rds")[["basicos_media_gamma"]]
}

# data é o último pregão do alvo, quando toda a perda já pode ser observada.
pares_basicos <- perdas_basicos |>
  filter(comparavel, horizonte %in% c(1, 5, 22), modelo %in% c("gpr", "vr")) |>
  select(origem, data, horizonte, modelo, MSE, QLIKE)
stopifnot(!anyDuplicated(pares_basicos[c("origem", "horizonte", "modelo")]),
          setequal(pares_basicos$horizonte, c(1, 5, 22)))
pares_basicos <- pares_basicos |>
  pivot_wider(names_from = modelo, values_from = c(MSE, QLIKE))
stopifnot(all(is.finite(as.matrix(select(pares_basicos, MSE_gpr, MSE_vr, QLIKE_gpr, QLIKE_vr)))))

acumuladas_basicos <- pares_basicos |>
  transmute(data, horizonte, MSE = MSE_vr - MSE_gpr, QLIKE = QLIKE_vr - QLIKE_gpr) |>
  pivot_longer(c(MSE, QLIKE), names_to = "perda", values_to = "diferenca") |>
  arrange(data) |>
  group_by(horizonte, perda) |>
  mutate(acumulada = cumsum(diferenca)) |>
  ungroup() |>
  mutate(horizonte = factor(horizonte, c(1, 5, 22), c("1 pregão", "5 pregões", "22 pregões")),
         perda = factor(perda, c("MSE", "QLIKE")))

figura_perdas_basicos <- ggplot(acumuladas_basicos, aes(data, acumulada)) +
  facet_wrap(vars(perda), ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
  geom_line(aes(colour = horizonte, linetype = horizonte), linewidth = 1.15,
            lineend = "round") +
  scale_colour_manual(values = c("1 pregão" = "#222222", "5 pregões" = "#C43B38",
                                 "22 pregões" = "#3269B3")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(subtitle = "Previsão da variância média com janela expansiva",
       x = "Último pregão do horizonte previsto", y = "Diferença acumulada (VR − GPR)",
       colour = "Horizonte", linetype = "Horizonte") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.width = grid::unit(1.4, "cm"),
        legend.text = element_text(size = 16), legend.title = element_text(size = 16),
        axis.text = element_text(size = 15, colour = "grey20"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
        strip.text = element_text(size = 18, face = "bold", hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA))

if (interactive()) print(figura_perdas_basicos)
ggsave("graficos_tabelas/modelo/perdas_acumuladas_basicos.pdf", figura_perdas_basicos,
       device = grDevices::cairo_pdf, width = 10, height = 7)
ggsave("graficos_tabelas/modelo/perdas_acumuladas_basicos.png", figura_perdas_basicos,
       width = 10, height = 7, dpi = 300)
