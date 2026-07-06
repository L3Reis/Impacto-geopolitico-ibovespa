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
    d_log_ic_br = as.numeric(d_log_ic_br)*100 # Multipliquei por 100 pois os coeficientes estavam desproporcionais
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

#* MODELO IC-BR somente
#! Utilizaremos somente LOG-DIFF por conta da estacionariedade

modelo_d_log_ic <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05848919  0.07139059  0.91539046  1.04927224 -0.07946252  1.00006540 
# p-value: 1.336436e-01
modelo_d_log_ic$par
modelo_d_log_ic$broom.mgarch

# 6 Lags

modelo_d_log_ic_6 <- fit_mfgarch(
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
# 0.06220059  0.07042348  0.91566250  1.00215888 -0.02978036  2.37066652 
# p-value: 2.296318e-01
modelo_d_log_ic_6$par
modelo_d_log_ic_6$broom.mgarch


# 3 Lags

modelo_d_log_ic_3 <- fit_mfgarch(
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
# 0.06029738  0.07054320  0.91593496  1.01803450 -0.01451505  1.70800191 
# p-value: 3.869985e-01
modelo_d_log_ic_3$par
modelo_d_log_ic_3$broom.mgarch

#* MODELO IC-BR ASSIMETRICO
#! Utilizaremos somente LOG-DIFF por conta da estacionariedade

modelo_d_log_ic <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03203688  0.02304079  0.92042040  0.08023228  0.97421186 -0.10492934  1.14676866  
# p-value: 3.635481e-02
modelo_d_log_ic$par
modelo_d_log_ic$broom.mgarch
modelo_d_log_ic$optim$convergence # modelo converge

# 6 Lags

modelo_d_log_ic_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 6,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03879341  0.02889531  0.91760141  0.07212147  0.92993431 -0.03330880  2.59618985  
# p-value: 1.508352e-01
modelo_d_log_ic_6$par
modelo_d_log_ic_6$broom.mgarch


# 3 Lags

modelo_d_log_ic_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03588874  0.02889753  0.91671390  0.07332446  0.93299802 -0.01810423  2.17365352 
# p-value: 2.393775e-01
modelo_d_log_ic_3$par
modelo_d_log_ic_3$broom.mgarch









####todo Modelo GPR Global + IC-BR

#* LOG/LOG-DIFF

modelo_log_gpr_ic <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03094920  0.02335175  0.91963945  0.07998711  1.77746778 -0.17534705  3.49994152 -0.10276253  1.17911667 
# theta p-value: 5.051173e-01
# theta 2 p-value: 3.825089e-02 
# BIC: 23392.36

modelo_log_gpr_ic$par
modelo_log_gpr_ic$broom.mgarch
modelo_log_gpr_ic$bic


# 6 LAGS

modelo_log_gpr_ic_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03991284  0.02931910  0.91597167  0.07146522  3.42405094 -0.54135027  1.44306154 -0.04883840  2.32055092  
# theta p-value: 3.969389e-02
# theta 2 p-value: 6.153536e-02
# BIC: 23900.2

modelo_log_gpr_ic_6$par
modelo_log_gpr_ic_6$broom.mgarch
modelo_log_gpr_ic_6$bic
modelo_log_gpr_ic_6$optim$convergence

# 3 LAGS

modelo_log_gpr_ic_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03333361  0.02830010  0.91606015  0.07428225  2.34128945 -0.30522010  1.00148429 -0.02438644  1.83682181  
# theta p-value: 4.097897e-01
# theta 2 p-value: 1.594213e-01
# BIC: 24160.61

modelo_log_gpr_ic_3$par
modelo_log_gpr_ic_3$broom.mgarch
modelo_log_gpr_ic_3$bic

# 3 LAGS e 1 lag

modelo_log_gpr_ic_3_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#          mu        alpha         beta        gamma            m        theta           w2    theta.two 
# 0.035938845  0.027999671  0.914680280  0.075488153  2.228198513 -0.285960209  1.000016740 -0.006876284  
# theta p-value: 3.947608e-01
# theta 2 p-value: 3.918258e-01
# BIC: 24154.19

modelo_log_gpr_ic_3_1$par
modelo_log_gpr_ic_3_1$broom.mgarch
modelo_log_gpr_ic_3_1$bic

#* LOG-DIFF/LOG-DIFF

modelo_dlog_gpr_ic <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03188031  0.02280805  0.92049933  0.08049957  0.97425937 -0.22970060  1.13491952 -0.10442777  1.15623507 
# theta p-value: 9.962384e-01
# theta 2 p-value: 5.860719e-01
# BIC: 23394.09

modelo_dlog_gpr_ic$par
modelo_dlog_gpr_ic$broom.mgarch
modelo_dlog_gpr_ic$bic


# 6 lags

modelo_dlog_gpr_ic_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03855515  0.02893480  0.91743969  0.07236649  0.93233801  0.24286280  3.89246513 -0.03443185  2.48583793 
# theta p-value: 4.159843e-01
# theta 2 p-value: 1.429759e-01
# BIC: 23906.31

modelo_dlog_gpr_ic_6$par
modelo_dlog_gpr_ic_6$broom.mgarch
modelo_dlog_gpr_ic_6$bic


# 3 lags

modelo_dlog_gpr_ic_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03649197  0.02886922  0.91719783  0.07282128  0.93152258  0.42244231  1.18430881 -0.01705444  2.26216470 
# theta p-value: 1.422239e-01
# theta 2 p-value: 3.019392e-01
# BIC: 24162.68

modelo_dlog_gpr_ic_3$par
modelo_dlog_gpr_ic_3$broom.mgarch
modelo_dlog_gpr_ic_3$bic


# 3 lags e 1 lag

modelo_dlog_gpr_ic_3_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#          mu        alpha         beta        gamma            m        theta           w2    theta.two 
# 0.035928531  0.029217905  0.916613484  0.072934522  0.927633102  0.439104780  1.206389254 -0.005365835 
# theta p-value: 1.332671e-01
# theta 2 p-value: 5.028391e-01
# BIC: 24155.3

modelo_dlog_gpr_ic_3_1$par
modelo_dlog_gpr_ic_3_1$broom.mgarch
modelo_dlog_gpr_ic_3_1$bic









## MODELO GPR GLOBAL + LOG-DIFF DO IC-BR

# Modelo GPR Global + IC-BRem log-diferença com 1 lag assimetrico

# GPR positivo e e nao significativo
# cambio negativo e nao significativo

modelo_d_log_gpr_comm_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 1,
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

modelo_d_log_gpr_comm_1$par
modelo_d_log_gpr_comm_1$broom.mgarch
modelo_d_log_gpr_cambio$bic
