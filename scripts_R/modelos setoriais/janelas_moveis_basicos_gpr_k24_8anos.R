# GARCH-MIDAS com GPR para bens básicos em janelas móveis de 8 anos.
# O GPR pré-amostral permite usar os retornos desde fevereiro de 2000.
# Execute com o diretório de trabalho na raiz do projeto.
# Os resultados ficam em resultados_janelas_basicos e o gráfico em grafico_theta_basicos.

library(readxl)
library(mfGARCH)
library(dplyr)
library(ggplot2)

# Configuração ---------------------------------------------------------------
K <- 24L
janela_meses <- 8L * 12L
passo_meses <- 1L                        # Reestimação a cada mês.
arquivo_base <- "dados/base_setores_final.xlsx"
arquivo_gpr <- "dados/data_gpr_export.xls"

# Preparação dos dados ------------------------------------------------------
# Os retornos da base já estão em percentual, como no script de referência.
painel_basicos <- read_excel(arquivo_base) %>%
  transmute(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    basic_products_log_ret = as.numeric(basic_products_log_ret),
    log_gpr_global = as.numeric(log_gpr_global)
  ) %>%
  arrange(date) %>%
  as.data.frame()

if (nrow(painel_basicos) == 0L || anyNA(painel_basicos) ||
    any(!is.finite(painel_basicos$basic_products_log_ret)) ||
    any(!is.finite(painel_basicos$log_gpr_global))) {
  stop("Confira a base. As quatro colunas utilizadas precisam conter dados válidos.")
}
if (anyDuplicated(painel_basicos$date)) {
  stop("Há datas diárias duplicadas na base.")
}
if (any(format(painel_basicos$date, "%Y-%m") !=
        format(painel_basicos$year_month, "%Y-%m"))) {
  stop("A coluna year_month precisa corresponder ao mês de date.")
}
gpr_painel <- painel_basicos %>% distinct(year_month, log_gpr_global)
if (anyDuplicated(gpr_painel$year_month)) {
  stop("O GPR deve ter um único valor em cada mês.")
}

meses <- sort(unique(painel_basicos$year_month))
if (!identical(meses, seq.Date(min(meses), max(meses), by = "month"))) {
  stop("Há meses ausentes na base. Confira a sequência mensal antes de estimar.")
}

# Histórico mensal do GPR ---------------------------------------------------
# Lê apenas month e GPR, com tipos explícitos, inclusive nos anos pré-amostrais.
colunas_gpr <- names(read_excel(arquivo_gpr, n_max = 0))
if (!all(c("month", "GPR") %in% colunas_gpr)) {
  stop("O arquivo do GPR precisa conter as colunas month e GPR.")
}
tipos_gpr <- rep("skip", length(colunas_gpr))
tipos_gpr[colunas_gpr == "month"] <- "date"
tipos_gpr[colunas_gpr == "GPR"] <- "numeric"

primeiro_mes_historico <- seq.Date(min(meses), by = "-1 month", length.out = K + 1L)[K + 1L]
meses_necessarios <- seq.Date(primeiro_mes_historico, max(meses), by = "month")
gpr_mensal <- read_excel(arquivo_gpr, col_types = tipos_gpr) %>%
  transmute(
    year_month = as.Date(month),
    log_gpr_global = log1p(GPR)           # Mesma transformação da base setorial.
  ) %>%
  filter(year_month >= primeiro_mes_historico, year_month <= max(meses)) %>%
  arrange(year_month)

if (!identical(gpr_mensal$year_month, meses_necessarios) ||
    any(!is.finite(gpr_mensal$log_gpr_global))) {
  stop("Confira o GPR. É necessária uma observação válida por mês, incluindo o histórico pré-amostral.")
}
gpr_sobreposto <- gpr_mensal$log_gpr_global[match(gpr_painel$year_month, gpr_mensal$year_month)]
if (any(abs(gpr_sobreposto - gpr_painel$log_gpr_global) > 1e-10)) {
  stop("Os valores do GPR de origem divergem dos valores da base setorial. Confira a versão da série.")
}

# Ajuste com histórico separado ---------------------------------------------
# Reutiliza a estimação, a recursão GARCH e a inferência do mfGARCH.
# Uma cópia local do filtro tau recebe os K meses anteriores à janela.
# O painel entregue à estimação contém os 96 meses efetivos de retornos.
ajustar_com_historico <- function(dados, historico_gpr, K) {
  if (length(historico_gpr) != K || any(!is.finite(historico_gpr))) {
    stop("Forneça exatamente K valores mensais válidos de GPR anteriores à janela.")
  }
  ambiente_midas <- new.env(parent = asNamespace("mfGARCH"))
  ambiente_midas$historico_gpr <- as.numeric(historico_gpr)
  ambiente_midas$calculate_tau_mf <- function(df, x, low.freq, w1, w2, theta, m, K) {
    pesos <- calculate_phi(w1, w2, K)
    tau_mensal <- exp(sum_tau(
      m = m, theta = theta, phivar = pesos,
      covariate = c(historico_gpr, x), K = K
    ))
    df$tau <- tau_mensal[match(df[[low.freq]], unique(df[[low.freq]]))]
    df
  }
  environment(ambiente_midas$calculate_tau_mf) <- ambiente_midas

  # A verossimilhança e os valores ajustados usam o mesmo filtro com histórico.
  ambiente_midas$llh_mf <- getFromNamespace("llh_mf", "mfGARCH")
  environment(ambiente_midas$llh_mf) <- ambiente_midas
  fit_historico <- mfGARCH::fit_mfgarch
  environment(fit_historico) <- ambiente_midas

  # A inicialização g_zero é calculada pelo pacote com os retornos da janela.
  fit_historico(
    data = dados,
    y = "basic_products_log_ret",
    x = "log_gpr_global",
    low.freq = "year_month",
    K = K,
    gamma = TRUE,
    weighting = "beta.restricted",
    multi.start = TRUE
  )
}

if (length(meses) < janela_meses) {
  stop("São necessários pelo menos 96 meses de retornos para a primeira janela.")
}
# Com a base atual, a primeira janela vai de fevereiro/2000 a janeiro/2008.
fins_janelas <- seq.int(janela_meses, length(meses), by = passo_meses)
resultados_lista <- vector("list", length(fins_janelas))

# Estimações ----------------------------------------------------------------
for (i in seq_along(fins_janelas)) {
  fim <- fins_janelas[i]
  inicio <- fim - janela_meses + 1L
  posicao_gpr <- match(meses[inicio], gpr_mensal$year_month)
  indices_historico <- seq.int(posicao_gpr - K, posicao_gpr - 1L)
  historico_gpr <- gpr_mensal$log_gpr_global[indices_historico]

  dados_janela <- painel_basicos %>%
    filter(year_month >= meses[inicio], year_month <= meses[fim])
  datas_efetivas <- dados_janela$date

  message(sprintf(
    "Janela %d/%d | %s a %s",
    i, length(fins_janelas), min(datas_efetivas), max(datas_efetivas)
  ))

  linha <- data.frame(
    inicio_historico = gpr_mensal$year_month[indices_historico[1L]],
    inicio_janela = min(datas_efetivas),
    fim_janela = max(datas_efetivas),
    n_obs = length(datas_efetivas),
    theta_estimado = NA_real_,
    theta = NA_real_,
    erro_padrao_robusto = NA_real_,
    p_valor = NA_real_,
    ic_inferior = NA_real_,
    ic_superior = NA_real_,
    convergencia = NA_integer_,
    valida = FALSE,
    motivo = ""
  )

  ajuste <- tryCatch({
    # Reduz as mensagens repetidas do pacote e mantém o progresso acima.
    invisible(capture.output(modelo <- ajustar_com_historico(
      dados = dados_janela,
      historico_gpr = historico_gpr,
      K = K
    )))
    modelo
  }, error = identity)

  if (inherits(ajuste, "error")) {
    linha$motivo <- conditionMessage(ajuste)
  } else {
    parametros <- ajuste$par
    persistencia <- parametros["alpha"] + parametros["beta"] + parametros["gamma"] / 2
    linha$theta_estimado <- unname(parametros["theta"])
    linha$convergencia <- ajuste$optim$convergence

    # Confere também as restrições, pois multi.start usa diferentes otimizadores.
    parametros_validos <- all(is.finite(parametros)) &&
      parametros["alpha"] >= 0 && parametros["beta"] >= 0 &&
      parametros["alpha"] + parametros["gamma"] >= 0 &&
      persistencia >= 0 && persistencia < 1 && parametros["w2"] >= 1
    amostra_correta <- identical(
      ajuste$df.fitted$date[is.finite(ajuste$df.fitted$residuals)],
      datas_efetivas
    )
    linha$valida <- isTRUE(linha$convergencia == 0L) &&
      parametros_validos && amostra_correta

    if (linha$valida) {
      linha$theta <- linha$theta_estimado
      inferencia_theta <- ajuste$broom.mgarch %>% filter(term == "theta")
      linha$erro_padrao_robusto <- inferencia_theta$rob.std.err
      linha$p_valor <- inferencia_theta$p.value
      if (is.finite(linha$erro_padrao_robusto) && linha$erro_padrao_robusto > 0) {
        margem <- qnorm(0.975) * linha$erro_padrao_robusto
        linha$ic_inferior <- linha$theta - margem
        linha$ic_superior <- linha$theta + margem
      }
    } else {
      linha$motivo <- paste(
        "Confira o ajuste. Convergência", linha$convergencia,
        "| parâmetros válidos", parametros_validos,
        "| amostra correta", amostra_correta
      )
    }
  }

  resultados_lista[[i]] <- linha
  if (!linha$valida) message("  ", linha$motivo)
}

resultados_janelas_basicos <- bind_rows(resultados_lista)
message(sprintf(
  "Estimações concluídas. %d de %d janelas válidas.",
  sum(resultados_janelas_basicos$valida), nrow(resultados_janelas_basicos)
))
print(resultados_janelas_basicos)

# Gráfico -------------------------------------------------------------------
# Cada estimativa é posicionada na última data observada da sua janela.
# Janelas com falha ficam com theta = NA, produzindo lacunas na trajetória.
if (!any(resultados_janelas_basicos$valida)) {
  stop("Nenhuma janela válida. Consulte a coluna motivo em resultados_janelas_basicos.")
}

# A escala acompanha as estimativas de theta, com margem de 8% em cada ponta.
limites_theta <- range(resultados_janelas_basicos$theta, finite = TRUE)
margem_theta <- 0.08 * diff(limites_theta)
if (margem_theta == 0) {
  margem_theta <- 0.08 * max(abs(limites_theta), 1)
}
limites_theta <- limites_theta + c(-margem_theta, margem_theta)

grafico_theta_basicos <- ggplot(
  resultados_janelas_basicos,
  aes(x = fim_janela, y = theta)
) +
  geom_ribbon(
    aes(ymin = ic_inferior, ymax = ic_superior),
    fill = "#27628C", alpha = 0.18, na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_line(color = "#174A70", linewidth = 0.75, na.rm = TRUE) +
  geom_point(color = "#174A70", size = 0.8, na.rm = TRUE) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  coord_cartesian(ylim = limites_theta) +
  labs(
    title = "GPR e volatilidade de bens básicos",
    subtitle = "GARCH-MIDAS | K = 24 | janela efetiva de 8 anos | avanço mensal",
    x = "Data final da janela",
    y = expression(hat(theta)),
    caption = "Faixa sombreada corresponde ao IC robusto de 95%, recortado aos limites do eixo."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(grafico_theta_basicos)
