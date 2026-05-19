# TESTANDO GPR BRASIL E SUAS TRANSFORMAÇÕES

# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)


# Testando GPR Global e suas transformações


base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
base_mfgarch_completa <- base_teste_modelo_2 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_bra = as.numeric(gpr_bra),
    d_gpr_bra = as.numeric(d_gpr_bra),
    log_gpr_bra = as.numeric(log_gpr_bra),
    d_log_gpr_bra = as.numeric(d_log_gpr_bra),
    ret_ibov_100 = ret_ibov * 100
  ) %>% select(
    date,
    year_month,
    gpr_bra,
    d_gpr_bra,
    log_gpr_bra,
    d_log_gpr_bra,
    ret_ibov_100
  ) |> na.omit()


## Modelo GPR em nível - No geral, independente do lag, coeficiente theta negativo

# Modelo GPR Brasil em nível com 12 lags
# Negativo e significativo

modelo_gpr_bra <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_bra",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#           mu        alpha         beta            m        theta           w2 
#  0.05802864   0.06999846   0.90804808   1.58828345 -12.24555804   1.00002503 
# p-value: 2.368378e-06 

modelo_gpr_bra$par
modelo_gpr_bra$broom.mgarch

# Modelo GPR Brasil em nível com 6 lags
# Negativo e significativo
modelo_gpr_bra_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_bra",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu       alpha        beta           m       theta          w2 
# 0.06130306  0.07126645  0.90865189  1.41116895 -8.66698246  1.02684215 
# p-value: 0.0001854622
modelo_gpr_bra_6$par
modelo_gpr_bra_6$broom.mgarch


# Modelo GPR Brasil em nível com 3 lags
# Negativo e significativo
modelo_gpr_bra_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_bra",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu       alpha        beta           m       theta          w2 
# 0.06018246  0.07151142  0.91073088  1.21827068 -4.55035555  1.57322340 
# p-value: 2.305537e-02
modelo_gpr_bra_3$par
modelo_gpr_bra_3$broom.mgarch


## Modelo GPR Brasil em diferenças - No geral, negativo, mas com alguns resultados próximos do 0 com lags menores


# Modelo GPR Brasil em diferenças com 12 lags
# Negativo e não significativo

modelo_d_gpr_bra <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_bra",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu       alpha        beta           m       theta          w2 
#  0.05796583  0.07095279  0.91510033  0.98567427 -2.40169497  3.85427897 
# p-value: 5.133088e-01

modelo_d_gpr_bra$par
modelo_d_gpr_bra$broom.mgarch


# Modelo GPR Brasil em diferenças com 6 lags
# Negativo (mas pouco) e não significativo

modelo_d_gpr_bra_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_bra",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06106385  0.07041378  0.91541409  0.97861637 -0.93012687  5.51567290 
# p-value: 6.654917e-01
modelo_d_gpr_bra_6$par
modelo_d_gpr_bra_6$broom.mgarch


# Modelo GPR Brasil em diferenças com 3 lags
# Negativo (mas pouco) e não significativo

modelo_d_gpr_bra_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_bra",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)


# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.0599524  0.0703918  0.9161096  1.0100371 -0.6991845  3.8376088 
# p-value: 5.089613e-01
modelo_d_gpr_bra_3$par
modelo_d_gpr_bra_3$broom.mgarch


## Modelo GPR Brasil com log-diferença

# Modelo GPR Brasil com log-diferença e 12 lags
# Negativo e não significativo

modelo_d_log_gpr_bra <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_bra",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05789518  0.07088597  0.91518180  0.98600043 -0.09809285  2.55583570 
# p-value: 7.621245e-01

modelo_d_log_gpr_bra$par
modelo_d_log_gpr_bra$broom.mgarch

# Modelo GPR Brasil com log-diferença e 6 lags
# Negativo e não significativo

modelo_d_log_gpr_bra_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_bra",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu       alpha        beta           m       theta          w2 
# 0.06115319  0.06995318  0.91598876  0.97891981 -0.24097834  1.00004665 
# p-value: 5.594379e-01 

modelo_d_log_gpr_bra_6$par
modelo_d_log_gpr_bra_6$broom.mgarch

# Modelo GPR Brasil com log-diferença e 3 lags
# Negativo e não significativo

modelo_d_log_gpr_bra_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_bra",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05753820  0.07102408  0.91496328  1.00449599 -0.03141253  1.00087575 
# p-value: 6.864986e-01
modelo_d_log_gpr_bra_3$par
modelo_d_log_gpr_bra_3$broom.mgarch
