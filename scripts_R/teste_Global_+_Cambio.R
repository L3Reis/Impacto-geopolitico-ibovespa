# Testando o modelo de GPR Global + Variância Realizada do Câmbio

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



### Modelo GPR Global + Cambio

## MODELO GPR GLOBAL + LOG-DIFF DO CAMBIO

# Modelo GPR Global + Cambio em log-diferença com 3 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e nao significativo

modelo_d_log_gpr_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03570376  0.02894955  0.91467523  0.07579538  0.91503796  0.41605025 
#         w2   theta.two      w2.two 
# 1.16717610 -0.15302780  1.82261049 
# theta p-value: 1.631930e-01
# theta 2 p-value: 1.053562e-01 

modelo_d_log_gpr_cambio$par
modelo_d_log_gpr_cambio$broom.mgarch
modelo_d_log_gpr_cambio$bic

# Modelo GPR Global 3 lags + Cambio em log-diferença com 2 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 10%

modelo_d_log_gpr_cambio_2 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 2,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03638967  0.02932549  0.91461330  0.07518892  0.91712189  0.42786123 
#         w2   theta.two      w2.two 
# 1.05218086 -0.10395138  1.67336811
# theta p-value: 1.413663e-01
# theta 2 p-value: 8.664759e-02


modelo_d_log_gpr_cambio_2$par
modelo_d_log_gpr_cambio_2$broom.mgarch


# Modelo GPR Global 3 lags + Cambio em log-diferença com 1 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e não significativo

modelo_d_log_gpr_cambio_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03593228  0.02941606  0.91539030  0.07399126  0.92031049  0.46972417 
#         w2   theta.two 
# 1.17841257 -0.04257989 
# theta p-value: 1.031413e-01
# theta 2 p-value: 1.785023e-01


modelo_d_log_gpr_cambio_1$par
modelo_d_log_gpr_cambio_1$broom.mgarch


## MODELO GPR GLOBAL + LOG DO CAMBIO

# Modelo GPR Global 3 lags + Cambio em log com 3 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 1%

modelo_log_gpr_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03299528  0.02757848  0.92634511  0.07059531 -0.84205848  0.36542818 
#         w2   theta.two      w2.two 
# 1.00066657 -0.26857747  2.24030483 
# theta p-value: 1.854043e-01
# theta 2 p-value: 6.278443e-03


modelo_log_gpr_cambio_3$par
modelo_log_gpr_cambio_3$broom.mgarch


# Modelo GPR Global 3 lags + Cambio em log com 1 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 1%

modelo_log_gpr_cambio_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03461704  0.02837320  0.92088800  0.07265358  0.01303607  0.42878809 
#         w2   theta.two 
# 1.10114489 -0.13616742
# theta p-value: 1.351998e-01
# theta 2 p-value: 4.258034e-03


modelo_log_gpr_cambio_1$par
modelo_log_gpr_cambio_1$broom.mgarch
