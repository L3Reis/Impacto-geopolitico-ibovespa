# Etapa 1: estimação única e predict() original do mfGARCH.
# Origem fixa: 31/08/2015. Horizontes de 1, 5 e 22 pregões.
library(dplyr)
library(readxl)
library(tidyr)
library(mfGARCH)

# Dados: retornos em percentual e VR mensal na mesma escala.
dados <- read_excel("dados/base_mfgarch_completa.xlsx") |>
  drop_na() |>
  mutate(date = as.Date(date),
         year_month = as.Date(paste0(year_month, "-01")),
         ret_ibov_100 = as.numeric(ret_ibov) * 100) |>
  filter(date >= as.Date("2000-02-02")) |>
  arrange(date) |>
  group_by(year_month) |>
  mutate(vr_ibov = sum(ret_ibov_100^2)) |>
  ungroup() |>
  as.data.frame()

data_corte <- as.Date("2015-08-31")
treino <- filter(dados, date <= data_corte)
teste <- filter(dados, date > data_corte)
horizontes <- c(1L, 5L, 22L)
stopifnot(nrow(teste) >= max(horizontes))

# Estimação: os dois modelos usam apenas a amostra de treino.
modelo_gpr_fixo <- fit_mfgarch(
  data = treino, y = "ret_ibov_100", x = "log_gpr_global",
  low.freq = "year_month", K = 24, gamma = TRUE,
  weighting = "beta.restricted", multi.start = TRUE
)

modelo_vr_fixo <- fit_mfgarch(
  data = treino, y = "ret_ibov_100", x = "vr_ibov",
  low.freq = "year_month", K = 24, gamma = TRUE,
  weighting = "beta.restricted", multi.start = TRUE
)



# Previsão: tau fixo no último valor estimado, com regressores defasados.
# Evita usar o GPR de agosto, divulgado após a origem. Não usa dados do teste.
previsoes_fixas <- data.frame(
  origem = data_corte, data = teste$date[horizontes], horizonte = horizontes,
  observado = teste$ret_ibov_100[horizontes]^2,
  gpr = predict(modelo_gpr_fixo, horizon = horizontes,
                fcts.tau = tail(modelo_gpr_fixo$tau, 1)),
  vr = predict(modelo_vr_fixo, horizon = horizontes,
               fcts.tau = tail(modelo_vr_fixo$tau, 1))
)

# Avaliação: variância prevista contra o retorno ao quadrado de cada data.
perdas_fixas <- previsoes_fixas |>
  pivot_longer(c(gpr, vr), names_to = "modelo", values_to = "previsao") |>
  mutate(MSE = (observado - previsao)^2,
         QLIKE = log(previsao) + observado / previsao)

stopifnot(all(is.finite(perdas_fixas$previsao) & perdas_fixas$previsao > 0))

# Uma origem fixa: há apenas um erro por modelo e horizonte (n = 1).
precisao_fixa <- perdas_fixas |>
  group_by(modelo, horizonte) |>
  summarise(n = n(), MSE = mean(MSE), QLIKE = mean(QLIKE), .groups = "drop")

head(previsoes_fixas)
print(as.data.frame(precisao_fixa), row.names = FALSE, digits = 7)

# modelo horizonte n       MSE    QLIKE
#    gpr         1 1 11.188780 3.219187
#    gpr         5 1  6.372506 1.158205
#    gpr        22 1  5.830288 1.120234
#     vr         1 1 11.900802 3.263711
#     vr         5 1  5.640733 1.110519
#     vr        22 1  4.729356 1.039425