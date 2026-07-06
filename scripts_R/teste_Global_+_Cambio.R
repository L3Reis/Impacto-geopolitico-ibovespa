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

##* MODELO LOG-DIFF GPR GLOBAL + LOG-DIFF DO CAMBIO

# Modelo GPR Global + Cambio em log-diferença com 3 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e nao significativo

modelo_d_log_gpr_dlog_cambio_3 <- fit_mfgarch(
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

modelo_d_log_gpr_dlog_cambio_3$par
modelo_d_log_gpr_dlog_cambio_3$broom.mgarch
modelo_d_log_gpr_cambio$bic

# Modelo GPR Global 12 lags + Cambio em log-diferença com 12 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 10%

modelo_d_log_gpr_dlog_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03032115  0.02224244  0.91862155  0.08469005  0.90047650 -0.17141317  2.74872272 
#  theta.two      w2.two 
# -0.75190534  2.56180378 
# theta p-value: 4.192098e-01 
# theta 2 p-value: 2.575099e-03


modelo_d_log_gpr_dlog_cambio$par
modelo_d_log_gpr_dlog_cambio$broom.mgarch


# Modelo com 6 lags

modelo_d_log_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03668601  0.02784301  0.91584575  0.07657694  0.90182811  0.20942658  3.76330747 
#  theta.two      w2.two 
# -0.36976673  2.06881011 
# theta p-value: 5.135833e-01
# theta 2 p-value: 1.198206e-02


modelo_d_log_gpr_dlog_cambio_6$par
modelo_d_log_gpr_dlog_cambio_6$broom.mgarch


# Modelo com 3 lags GPR e 1 lag cambio

modelo_d_log_gpr_dlog_cambio_3_1 <- fit_mfgarch(
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
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03593228  0.02941606  0.91539030  0.07399126  0.92031049  0.46972417  1.17841257 
#  theta.two 
# -0.04257989 
# theta p-value: 1.031413e-01
# theta 2 p-value: 1.785023e-01


modelo_d_log_gpr_dlog_cambio_3_1$par
modelo_d_log_gpr_dlog_cambio_3_1$broom.mgarch


##* MODELO GPR GLOBAL LOG DIFF + LOG DO CAMBIO


modelo_dlog_gpr_log_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03203227  0.02291849  0.92821567  0.07576297 -1.18873065 -0.73050391  1.40616358 
#  theta.two      w2.two 
# -0.31051401  8.98854951
# theta p-value: 6.032088e-01
# theta 2 p-value: 2.006513e-02


modelo_dlog_gpr_log_cambio$par
modelo_dlog_gpr_log_cambio$broom.mgarch

# Modelo com 6 lags

modelo_dlog_gpr_log_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03634332  0.02670329  0.92980994  0.06812903 -1.52303966  0.07911725  5.90576016 
#  theta.two      w2.two 
# -0.36401968  3.48384251 
# theta p-value: 5.996887e-01
# theta 2 p-value: 1.733362e-02


modelo_dlog_gpr_log_cambio_6$par
modelo_dlog_gpr_log_cambio_6$broom.mgarch



# Modelo GPR Global 3 lags + Cambio em log com 3 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 1%

modelo_dlog_gpr_log_cambio_3 <- fit_mfgarch(
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


modelo_dlog_gpr_log_cambio_3$par
modelo_dlog_gpr_log_cambio_3$broom.mgarch


# Modelo GPR Global 3 lags + Cambio em log com 1 lags assimetrico

# GPR positivo e e nao significativo
# cambio negativo e significativo a 1%

modelo_dlog_gpr_log_cambio_3_1 <- fit_mfgarch(
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


modelo_dlog_gpr_log_cambio_3_1$par
modelo_dlog_gpr_log_cambio_3_1$broom.mgarch

#* MODELO GPR GLOBAL EM LOG E E CAMBIO EM LOG

# MODELO COM K = 12

modelo_log_gpr_log_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.03389155  0.02214534  0.92943860  0.07609221 -1.05914835 -0.06145932  3.52415544 
#  theta.two      w2.two 
# -0.33529312  8.34932634 
# theta p-value: 9.090960e-01
# theta 2 p-value: 2.719213e-02


modelo_log_gpr_log_cambio$par
modelo_log_gpr_log_cambio$broom.mgarch
modelo_log_gpr_log_cambio$optim$convergence

# modelo com k = 6

modelo_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#          mu        alpha         beta        gamma            m        theta 
# 0.037924480  0.026840918  0.929832609  0.067081001  0.000117883 -0.352262225 
#          w2    theta.two       w2.two 
# 1.236384079 -0.376580769  3.329706982 
# theta p-value: 3.357481e-02
# theta 2 p-value: 8.200381e-04


modelo_log_gpr_log_cambio_6$par
modelo_log_gpr_log_cambio_6$broom.mgarch
modelo_log_gpr_log_cambio_6$optim$message

# modelo com k = 3

modelo_log_gpr_log_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
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
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03297165  0.02727027  0.92600314  0.07123916 -0.42015345 -0.08834788  4.48761590 
#  theta.two      w2.two 
# -0.26641485  2.27525758 
# theta p-value: 5.101898e-01
# theta 2 p-value: 6.282453e-03


modelo_log_gpr_log_cambio_3$par
modelo_log_gpr_log_cambio_3$broom.mgarch
modelo_log_gpr_log_cambio_3$optim$convergence

# modelo com k = 3 no gpr e k = 1 no cambio

modelo_log_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
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
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03361756  0.02837913  0.91954019  0.07422747  0.58735096 -0.12589111  3.03125788 
#  theta.two 
# -0.13876042 
# theta p-value: 3.147437e-01
# theta 2 p-value: 2.757346e-03


modelo_log_gpr_log_cambio_3_1$par
modelo_log_gpr_log_cambio_3_1$broom.mgarch
modelo_log_gpr_log_cambio_3_1$optim$convergence

#* MODELO COM GPR EM LOG E CAMBIO EM LOG-DIFF

modelo_log_gpr_dlog_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03141767  0.02303381  0.91773648  0.08350633  1.63400476 -0.16116395  4.68688947 
#  theta.two      w2.two 
# -0.74077543  2.58105848 
# theta p-value: 4.690951e-01
# theta 2 p-value: 2.806829e-03


modelo_log_gpr_dlog_cambio$par
modelo_log_gpr_dlog_cambio$broom.mgarch
modelo_log_gpr_dlog_cambio$optim$convergence

# Modelo com 6 lags

modelo_log_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03774033  0.02713369  0.91526482  0.07674401  2.40383826 -0.32925516  1.61618992 
# theta.two      w2.two 
# -0.36699540  2.05423377  
# theta p-value: 1.911537e-01
# theta 2 p-value: 1.023479e-02


modelo_log_gpr_dlog_cambio_6$par
modelo_log_gpr_dlog_cambio_6$broom.mgarch
modelo_log_gpr_dlog_cambio_6$optim$convergence

# Modelo com 3 lags

modelo_log_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
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
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03437367  0.02937241  0.91526155  0.07327523  1.22235801 -0.06824420  4.65033403 
#  theta.two      w2.two 
# -0.17486288  1.60116274  
# theta p-value: 5.971577e-01
# theta 2 p-value: 5.136145e-02


modelo_log_gpr_dlog_cambio_3$par
modelo_log_gpr_dlog_cambio_3$broom.mgarch

# Modelo com 3 lags no GPR e 1 lag no cambio

modelo_log_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
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
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03611359  0.02955213  0.91418008  0.07464997  1.35062828 -0.09348915  4.19686397 
#  theta.two 
# -0.03963378 
# theta p-value: 4.537448e-01
# theta 2 p-value: 2.180806e-01


modelo_log_gpr_dlog_cambio_3_1$par
modelo_log_gpr_dlog_cambio_3_1$broom.mgarch
