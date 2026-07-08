# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)


# Adaptando para o modelo


base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
base_mfgarch_completa <- base_teste_modelo_2 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_global = as.numeric(gpr_global),
    d_gpr_global = as.numeric(d_gpr_global),
    log_gpr_global = as.numeric(log_gpr_global),
    d_log_gpr_global = as.numeric(d_log_gpr_global),
    ret_ibov_100 = ret_ibov * 100,
    ic_br = as.numeric(ic_br),
    d_ic_br = as.numeric(d_ic_br),
    log_ic_br = as.numeric(log_ic_br),
    d_log_ic_br = as.numeric(d_log_ic_br)
  ) %>% select(
    date,
    year_month,
    gpr_global,
    d_gpr_global,
    log_gpr_global,
    d_log_gpr_global,
    ret_ibov_100,
    ic_br,
    d_ic_br,
    log_ic_br,
    d_log_ic_br
  ) |> na.omit()


## GARCH-MIDAS direto com o IC-BR

# Modelo log-diff do IC-BR com 6 lags
# Negativo e  não significativo

modelo_d_log_ic_br <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06199757  0.07029776  0.91577198  1.00051856 -2.94424247  2.38518324 
# p-value: 2.342717e-01

modelo_d_log_ic_br$par
modelo_d_log_ic_br$broom.mgarch



# Modelo log-diff do IC-BR com 3 lags
# Negativo e  não significativo

modelo_d_log_ic_br_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06033875  0.07051743  0.91592702  1.01671043 -1.42689938  1.73791279 
# p-value: 3.963114e-01

modelo_d_log_ic_br_3$par
modelo_d_log_ic_br_3$broom.mgarch


# Modelo log-diff do IC-BR com 1 lags
# Negativo e significativo

modelo_d_log_ic_br_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 1,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta 
# 0.06105302  0.07099190  0.91615797  1.04606631 -0.02122050 
# p-value: 1.631886e-10

modelo_d_log_ic_br_1$par
modelo_d_log_ic_br_1$broom.mgarch
