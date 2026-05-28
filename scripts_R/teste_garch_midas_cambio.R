# TESTANDO a variância realizada do câmbio no GARCH-MIDAS

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

# Modelo Cambio em log com 1 lag
# Negativo e  significativo

modelo_log_cambio_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 1,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta 
# 0.06094074  0.07205343  0.91760234  0.32885537 -0.11540550 
# p-value: 0.0098525039
modelo_log_cambio_1$par
modelo_log_cambio_1$broom.mgarch







# Modelo com câmbio em diferença

# Modelo Cambio em diferença com 6 lags
# Negativo e  não significativo

modelo_d_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_var_cambio",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06119944  0.07084562  0.91491397  0.97952104 -8.88737090  8.11733017 
# p-value: 7.182883e-01

modelo_d_cambio$par
modelo_d_cambio$broom.mgarch


# Modelo Cambio em diferença com 3 lags
# Negativo e significativo

modelo_d_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_var_cambio",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06028632  0.07092006  0.91544014  1.00793546 -8.72467597  4.93054111
# p-value: 3.636182e-01

modelo_d_cambio_3$par
modelo_d_cambio_3$broom.mgarch


## Modelo do cambio em log-diferença

# Modelo Cambio em log-diferença com 6 lags
# Negativo e quase significativo

modelo_d_log_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06135037  0.07170312  0.91434608  0.98145596 -0.25498646  2.24421524 

modelo_d_log_cambio$par
modelo_d_log_cambio$broom.mgarch


# Modelo Cambio em log-diferença com 3 lags
# Negativo e  significativo

modelo_d_log_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)


# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05988750  0.07176563  0.91446016  1.00631503 -0.12223631  1.71322344 
# p-value: 1.645788e-01

modelo_d_log_cambio_3$par
modelo_d_log_cambio_3$broom.mgarch

# Modelo Cambio em log-diferença com 1 lag
# Negativo (quase zero) e  significativo

modelo_d_log_cambio_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 1,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#        mu       alpha        beta           m       theta 
# 0.06097683  0.07159734  0.91540028  1.04485274 -0.03749853 
# p-value: 1.402929e-10

modelo_d_log_cambio_1$par
modelo_d_log_cambio_1$broom.mgarch


