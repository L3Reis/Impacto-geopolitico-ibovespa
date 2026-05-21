# TESTANDO a variância realizada do câmbio junto com o GPR global

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
    var_cambio = as.numeric(var_cambio),
    d_var_cambio = as.numeric(d_var_cambio),
    log_var_cambio = as.numeric(log_var_cambio),
    d_log_var_cambio = as.numeric(d_log_var_cambio)
  ) %>% select(
    date,
    year_month,
    gpr_global,
    d_gpr_global,
    log_gpr_global,
    d_log_gpr_global,
    ret_ibov_100,
    var_cambio,
    d_var_cambio,
    log_var_cambio,
    d_log_var_cambio
  ) |> na.omit()


## GARCH-MIDAS direto com o câmbio

# Modelo Cambio em nível com 6 lags
# Negativo e  não significativo

modelo_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "var_cambio",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu        alpha         beta            m        theta           w2 
#  0.06093546   0.07084734   0.91553712   1.01426782 -18.33662232  12.20932989 
# p-value: 5.885932e-02

modelo_cambio$par
modelo_cambio$broom.mgarch


# Modelo Cambio em nível com 3 lags
# Negativo e  significativo

modelo_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "var_cambio",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu        alpha         beta            m        theta           w2 
#  0.05983640   0.07105492   0.91595143   1.04907693 -18.43218708  18.51699707 
# p-value: 2.733432e-02
modelo_cambio_3$par
modelo_cambio_3$broom.mgarch


## Cambio em log

# Modelo Cambio em log com 6 lags
# Negativo e  significativo

modelo_log_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06008176  0.06870940  0.92317663 -1.37375355 -0.35696820  2.99056314 
# p-value: 0.0142916501
modelo_log_cambio$par
modelo_log_cambio$broom.mgarch


# Modelo Cambio em log com 3 lags
# Negativo e  significativo

modelo_log_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05922396  0.07034779  0.92088437 -0.57101830 -0.24688507  2.04272965 
# p-value: 0.0061962286
modelo_log_cambio_3$par
modelo_log_cambio_3$broom.mgarch

