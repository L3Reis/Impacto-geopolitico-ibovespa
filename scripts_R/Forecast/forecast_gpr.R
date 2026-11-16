# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)

options(width = 300)
source("scripts_R/Forecast/funcoes_forecast.R", encoding = "UTF-8")


# Base
base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparação para o mfGARCH
base_mfgarch_completa <- base_teste_modelo_2 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_bra = as.numeric(gpr_bra),
    gpr_global = as.numeric(gpr_global),
    gpr_global_z = as.numeric(gpr_global_z),
    d_gpr_global = as.numeric(d_gpr_global),
    log_gpr_global = as.numeric(log_gpr_global),
    d_log_gpr_global = as.numeric(d_log_gpr_global),
    ret_ibov_100 = ret_ibov * 100
  ) |> filter(date >= '2000-02-02') |>
  na.omit()


# Variância realizada mensal do Ibovespa
base_mfgarch_completa <- base_mfgarch_completa |>
  group_by(year_month) |>
  mutate(vr_ibov = sum(ret_ibov_100^2)) |>
  ungroup()


# Amostras
data_corte <- as.Date("2015-08-31")
calendario_forecast <- preparar_calendario(base_mfgarch_completa)
amostra_treino <- dados_na_origem(calendario_forecast, data_corte)

amostra_teste <- base_mfgarch_completa |>
  filter(date > data_corte) |>
  select(date, year_month, ret_ibov_100, log_gpr_global, vr_ibov)

# Benchmark com VR: estimação no treino

modelo_benchmark_vr <- fit_mfgarch(
  data = amostra_treino,
  y = "ret_ibov_100",
  x = "vr_ibov",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_benchmark_vr$par
modelo_benchmark_vr$broom.mgarch
modelo_benchmark_vr$optim$convergence

df_componentes <- modelo_benchmark_vr$df.fitted

par(mfrow = c(3, 1))

with(df_componentes, {
  plot(date, g, type = "l", main = "Componente de curto prazo")
  plot(date, tau, type = "l", main = "Componente de longo prazo")
  plot(date, sqrt(g * tau), type = "l",
       main = "Volatilidade diária estimada")
})

par(mfrow = c(1, 1))


# Testes dos resíduos padronizados
testar_residuos <- function(residuos, lags = c(5, 10, 20), lag_arch = 10) {
  if (any(!is.finite(residuos)) || length(residuos) <= max(c(lags, lag_arch)) + 1) {
    stop("Resíduos inválidos ou amostra insuficiente.")
  }

  extrair <- function(teste, nome, lag) {
    data.frame(teste = nome, lag = lag,
               estatistica = unname(teste$statistic), p_value = teste$p.value)
  }

  ljung <- lapply(lags, function(k) {
    rbind(
      extrair(Box.test(residuos, lag = k, type = "Ljung-Box", fitdf = 0),
              "Ljung-Box - residuos", k),
      extrair(Box.test(residuos^2, lag = k, type = "Ljung-Box", fitdf = 0),
              "Ljung-Box - residuos^2", k)
    )
  })
  arch <- extrair(FinTS::ArchTest(residuos, lags = lag_arch, demean = TRUE),
                  "ARCH-LM - residuos", lag_arch)
  tabela <- do.call(rbind, c(ljung, list(arch)))
  transform(tabela, rejeita_5pct = p_value < 0.05)
}

# Testes dos resíduos padronizados (remove apenas os NA iniciais)
residuos_vr <- as.numeric(modelo_benchmark_vr$df.fitted$residuals)
primeiro_residuo <- match(TRUE, !is.na(residuos_vr))
if (is.na(primeiro_residuo)) stop("Não há resíduos padronizados observados.")
residuos_vr <- residuos_vr[seq.int(primeiro_residuo, length(residuos_vr))]

testes_residuos_vr <- testar_residuos(residuos_vr)
print(testes_residuos_vr, row.names = FALSE)


#* Estimação do modelo com GPR global: estimação no treino

modelo_gpr_treino <- fit_mfgarch(
  data = amostra_treino,
  y = "ret_ibov_100",
  x = "gpr_info",
  low.freq = "mes_gpr",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_gpr_treino$par
modelo_gpr_treino$broom.mgarch
modelo_gpr_treino$optim$convergence

# teste dos resíduos padronizados
residuos_gpr_treino <- as.numeric(modelo_gpr_treino$df.fitted$residuals)
primeiro_residuo <- match(TRUE, !is.na(residuos_gpr_treino))
if (is.na(primeiro_residuo)) stop("Não há resíduos padronizados observados.")
residuos_gpr_treino <- residuos_gpr_treino[seq.int(primeiro_residuo, length(residuos_gpr_treino))]


teste_residuos_gpr_treino <- testar_residuos(residuos_gpr_treino)
print(teste_residuos_gpr_treino, row.names = FALSE)

# Pseudo-OOS com série revisada; divulgação do GPR no dia 1 por hipótese.
# Reestimação na primeira origem de cada mês; estados atualizados diariamente.
pasta_forecast <- "resultados/forecast/expanding_mensal_k24"
previsoes_oos <- forecast_expansivo(
  calendario_forecast, data_corte, K = 24,
  pasta = pasta_forecast,
  n_previsoes = getOption("forecast.n_previsoes", Inf),
  modelos_iniciais = list(gpr = modelo_gpr_treino, vr = modelo_benchmark_vr)
)

head(previsoes_oos)

# Precisão OOS: h = 1; retorno ao quadrado como proxy da variância diária.
stopifnot(nrow(previsoes_oos) > 0, !anyDuplicated(previsoes_oos$data),
          identical(previsoes_oos$data, head(amostra_teste$date, nrow(previsoes_oos))),
          with(previsoes_oos, all(
            is.finite(retorno_quadrado) & retorno_quadrado >= 0 &
            is.finite(var_gpr) & var_gpr > 0 & is.finite(var_vr) & var_vr > 0
          )))

perdas_oos <- previsoes_oos |>
  select(origem, data, retorno_quadrado, var_gpr, var_vr) |>
  pivot_longer(c(var_gpr, var_vr), names_to = "modelo",
               names_prefix = "var_", values_to = "previsao") |>
  mutate(modelo = toupper(modelo),
         MSE = (retorno_quadrado - previsao)^2,
         QLIKE = log(previsao) + retorno_quadrado / previsao)

precisao_oos <- perdas_oos |>
  group_by(modelo) |>
  summarise(n = n(), MSE = mean(MSE), QLIKE = mean(QLIKE), .groups = "drop")

print(as.data.frame(precisao_oos), row.names = FALSE, digits = 7)

# modelo    n      MSE    QLIKE
#    GPR 2564 69.14873 1.494432
#     VR 2564 76.15822 1.491091

write.csv(precisao_oos, file.path(pasta_forecast, "precisao_oos.csv"), row.names = FALSE)
write.csv(perdas_oos, file.path(pasta_forecast, "perdas_oos.csv"), row.names = FALSE)

# Outros horizontes, sem reestimar (tau constante na origem):
# previsoes_h <- prever_horizontes(pasta_forecast, c(1, 5, 10, 22, 30))

previsoes_h10 <- prever_horizontes(pasta_forecast, 10)

realizado_diario <- amostra_teste |>
  transmute(data_alvo = date, observado = ret_ibov_100^2)

perdas_h10 <- previsoes_h10 |>
  filter(!is.na(data_alvo)) |>
  left_join(realizado_diario, by = "data_alvo") |>
  mutate(
    MSE = (observado - variancia)^2,
    QLIKE = log(variancia) + observado / variancia
  )

precisao_h10 <- perdas_h10 |>
  group_by(modelo, horizonte) |>
  summarise(
    n = n(),
    MSE = mean(MSE),
    QLIKE = mean(QLIKE),
    .groups = "drop"
  )

print(as.data.frame(precisao_h10), row.names = FALSE, digits = 7)

# modelo horizonte    n      MSE    QLIKE
#    gpr        10 2555 93.72995 1.613610
#     vr        10 2555 95.31830 1.612546

# Precisão OOS: variância do 30º pregão à frente, com tau constante na origem.
previsoes_h30 <- prever_horizontes(pasta_forecast, 30)
realizado_diario <- amostra_teste |>
  transmute(data_alvo = date, observado = ret_ibov_100^2)

perdas_h30 <- previsoes_h30 |>
  filter(!is.na(data_alvo)) |>
  left_join(realizado_diario, by = "data_alvo") |>
  mutate(MSE = (observado - variancia)^2,
         QLIKE = log(variancia) + observado / variancia)

stopifnot(nrow(perdas_h30) > 0,
          all(is.finite(perdas_h30$MSE)), all(is.finite(perdas_h30$QLIKE)))

precisao_h30 <- perdas_h30 |>
  group_by(modelo, horizonte) |>
  summarise(n = n(), MSE = mean(MSE), QLIKE = mean(QLIKE), .groups = "drop")

print(as.data.frame(precisao_h30), row.names = FALSE, digits = 7)

# modelo horizonte    n      MSE    QLIKE
#    gpr        30 2535 94.49235 1.784503
#     vr        30 2535 93.49103 1.781478

write.csv(precisao_h30, file.path(pasta_forecast, "precisao_h30.csv"), row.names = FALSE)
write.csv(perdas_h30, file.path(pasta_forecast, "perdas_h30.csv"), row.names = FALSE)
