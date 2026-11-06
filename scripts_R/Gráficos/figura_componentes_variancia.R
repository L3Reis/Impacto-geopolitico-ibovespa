# Aqui vai o código para criar a figura, antes é necessário rodar os modelos nos scripts de janelas_k,basicos_k, etc.

# Pacotes
library(dplyr)
library(ggplot2)


#* Modelo log GPR com 24 defasagens

head(modelo_gpr_global_log_24$df.fitted)

# Variância condicional total: h_{i,t} = tau_t * g_{i,t}.
# O mfGARCH já fornece os componentes g (curto prazo) e tau (longo prazo)
# em df.fitted, alinhados às datas diárias.
componentes_variancia_ibov <- modelo_gpr_global_log_24$df.fitted |>
  mutate(variancia_total = tau * g) |>
  select(date, any_of("year_month"), g, tau, variancia_total) |>
  arrange(date)

# g é adimensional. Como o modelo usa ret_ibov_100, tau e variancia_total
# estão em pontos percentuais ao quadrado. A volatilidade seria sqrt(tau * g).
# Os NAs iniciais do filtro MIDAS são mantidos para preservar as datas.
head(componentes_variancia_ibov)

# Figura do IBOV: curto prazo, longo prazo e variância total, nessa ordem.
dados_figura_ibov <- componentes_variancia_ibov  |>
  tidyr::pivot_longer(
    cols = c(g, tau, variancia_total),
    names_to = "componente",
    values_to = "valor"
  ) |>
  mutate(componente = factor(componente, levels = c("g", "tau", "variancia_total")))

figura_componentes_ibov <- ggplot(
  dados_figura_ibov, aes(x = date, y = valor, color = componente)
) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  # O eixo de datas é comum; cada componente tem sua própria escala vertical.
  facet_wrap(
    vars(componente), ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      g = "Componente de curto prazo (g; adimensional)",
      tau = "Componente de longo prazo (tau; p.p.²)",
      variancia_total = "Variância total (tau × g; p.p.²)"
    ))
  ) +
  scale_color_manual(values = c(g = "#009E73", tau = "#D55E00", variancia_total = "#0072B2")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Componentes da variância condicional do retorno do IBOV",
    subtitle = "GARCH-MIDAS: log GPR global, K = 24",
    x = "Data",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  )

print(figura_componentes_ibov)



#* Modelo do consumo com 24 defasagens

head(modelo_consumo_24$df.fitted)

# Variância condicional total: h_{i,t} = tau_t * g_{i,t}.

componentes_variancia_consumo <- modelo_consumo_24$df.fitted |>
  mutate(variancia_total = tau * g) |>
  select(date, any_of("year_month"), g, tau, variancia_total) |>
  arrange(date)

head(componentes_variancia_consumo)

# Figura do consumo: três painéis, usando todo o período disponível.
dados_figura_consumo <- componentes_variancia_consumo |>
  tidyr::pivot_longer(
    cols = c(g, tau, variancia_total),
    names_to = "componente",
    values_to = "valor"
  ) |>
  mutate(componente = factor(componente, levels = c("g", "tau", "variancia_total")))

figura_componentes_consumo <- ggplot(
  dados_figura_consumo, aes(x = date, y = valor, color = componente)
) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  # O eixo de datas é comum; cada componente tem sua própria escala vertical.
  facet_wrap(
    vars(componente), ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      g = "Componente de curto prazo (g; adimensional)",
      tau = "Componente de longo prazo (tau; p.p.²)",
      variancia_total = "Variância total (tau × g; p.p.²)"
    ))
  ) +
  scale_color_manual(values = c(g = "#009E73", tau = "#D55E00", variancia_total = "#0072B2")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Componentes da variância condicional do retorno do Consumo",
    subtitle = "GARCH-MIDAS: log GPR global, K = 24",
    x = "Data",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  )

print(figura_componentes_consumo)


#* Modelo do bens_basicos com 24 defasagens

head(modelo_bens_basicos_24$df.fitted)

# Variância condicional total: h_{i,t} = tau_t * g_{i,t}.

componentes_variancia_bens_basicos <- modelo_bens_basicos_24$df.fitted |>
  mutate(variancia_total = tau * g) |>
  select(date, any_of("year_month"), g, tau, variancia_total) |>
  arrange(date)

head(componentes_variancia_bens_basicos)

# Figura do bens_basicos: três painéis, usando todo o período disponível.
dados_figura_bens_basicos <- componentes_variancia_bens_basicos |>
  tidyr::pivot_longer(
    cols = c(g, tau, variancia_total),
    names_to = "componente",
    values_to = "valor"
  ) |>
  mutate(componente = factor(componente, levels = c("g", "tau", "variancia_total")))

figura_componentes_bens_basicos <- ggplot(
  dados_figura_bens_basicos, aes(x = date, y = valor, color = componente)
) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  # O eixo de datas é comum; cada componente tem sua própria escala vertical.
  facet_wrap(
    vars(componente), ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      g = "Componente de curto prazo (g; adimensional)",
      tau = "Componente de longo prazo (tau; p.p.²)",
      variancia_total = "Variância total (tau × g; p.p.²)"
    ))
  ) +
  scale_color_manual(values = c(g = "#009E73", tau = "#D55E00", variancia_total = "#0072B2")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Componentes da variância condicional do retorno do bens_basicos",
    subtitle = "GARCH-MIDAS: log GPR global, K = 24",
    x = "Data",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  )

print(figura_componentes_bens_basicos)


#* Figura conjunta: IBOV, consumo e bens básicos (K = 24)

# Reúne as tabelas já preparadas acima, identificando cada índice.
# Mantém todo o período disponível de cada série, sem filtro de datas.
dados_figura_conjunta <- bind_rows(
  IBOV = dados_figura_ibov,
  Consumo = dados_figura_consumo,
  "Bens básicos" = dados_figura_bens_basicos,
  .id = "serie"
) |>
  mutate(serie = factor(serie, levels = c("IBOV", "Consumo", "Bens básicos"))) |>
  arrange(componente, serie, date)

# Cada painel mostra o mesmo componente dos três índices na mesma escala.
figura_componentes_conjunta <- ggplot(
  dados_figura_conjunta,
  aes(x = date, y = valor, color = serie, group = serie)
) +
  geom_line(linewidth = 0.5, na.rm = TRUE) +
  facet_wrap(
    vars(componente), ncol = 1, scales = "free_y",
    labeller = as_labeller(c(
      g = "Componente de curto prazo (g; adimensional)",
      tau = "Componente de longo prazo (tau; p.p.²)",
      variancia_total = "Variância total (tau × g; p.p.²)"
    ))
  ) +
  scale_color_manual(values = c(
    IBOV = "#4908fc", Consumo = "#f00c0c", "Bens básicos" = "#3ef307"
  )) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Componentes da variância condicional: IBOV, consumo e bens básicos",
    subtitle = "GARCH-MIDAS: log GPR global, K = 24",
    x = "Data",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", hjust = 0),
    panel.grid.minor = element_blank()
  )

print(figura_componentes_conjunta)


#* Figura conjunta com zoom a partir de dezembro de 2021

inicio_zoom <- as.Date("2021-12-01")

# Remove os valores ausentes antes de definir os períodos dos gráficos.
# Assim, as datas iniciais sem componentes estimados não alongam o eixo x.
dados_figura_conjunta_sem_na <- dados_figura_conjunta |>
  filter(!is.na(valor))

dados_figura_zoom <- bind_rows(
  "Amostra completa" = dados_figura_conjunta_sem_na,
  "2022-2025" = dados_figura_conjunta_sem_na |>
    filter(date >= inicio_zoom),
  .id = "periodo"
) |>
  mutate(periodo = factor(
    periodo, levels = c("Amostra completa", "2022-2025")
  )) |>
  arrange(componente, periodo, serie, date)

figura_componentes_zoom <- ggplot(
  dados_figura_zoom,
  aes(x = date, y = valor, color = serie, group = serie)
) +
  geom_line(linewidth = 0.5) +
  # Cada linha contém um componente; cada coluna contém um período.
  # As escalas livres também ampliam as oscilações no eixo y do zoom.
  facet_wrap(
    vars(componente, periodo), ncol = 2, scales = "free",
    labeller = function(rotulos) {
      titulos <- c(
        g = "Componente de curto prazo (g; adimensional)",
        tau = "Componente de longo prazo (tau; p.p.²)",
        variancia_total = "Variância total (tau × g; p.p.²)"
      )
      texto <- unname(titulos[as.character(rotulos$componente)])
      # Mostra o título de cada coluna apenas nos painéis da primeira linha.
      list(ifelse(
        rotulos$componente == "g",
        paste(rotulos$periodo, texto, sep = "\n"),
        texto
      ))
    }
  ) +
  scale_color_manual(values = c(
    IBOV = "#0072B2", Consumo = "#D55E00", "Bens básicos" = "#009E73"
  )) +
  scale_x_date(breaks = scales::breaks_pretty(n = 5), date_labels = "%Y") +
  labs(
    x = "Data",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", hjust = 0,size = 14 ),
    panel.grid.minor = element_blank()
  )

print(figura_componentes_zoom)

ggsave(
  filename = "graficos_tabelas/modelo/componentes_variancia_zoom.pdf",
  plot = figura_componentes_zoom,
  device = cairo_pdf,
  width = 14,
  height = 9,
  units = "in"
)

