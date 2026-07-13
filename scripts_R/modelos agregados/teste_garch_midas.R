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

# Preparando dados para o Pacote mfGARCH. 
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
  ) %>%
  na.omit()


#* Modelo GPR em nível

# Modelo GARCH-MIDAS com GPR Global 12 lags
# não significativo
modelo_gpr_global <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu        alpha         beta        gamma            m        theta           w2 
# 3.164574e-02 2.372217e-02 9.176529e-01 8.019092e-02 8.882598e-01 6.612811e-05 1.699971e+00 
# p-value: 4.048712e-01
modelo_gpr_global$par
modelo_gpr_global$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 6 lags
# não significativo

modelo_gpr_global_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03888738  0.03013854  0.91583312  0.07123760  1.14542132 -0.00224774  1.07649968 
# p-value: 2.793743e-01
modelo_gpr_global_6$par
modelo_gpr_global_6$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 3 lags
# não significativo

modelo_gpr_global_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#           mu         alpha          beta         gamma             m         theta            w2 
# 0.0358603621  0.0293627266  0.9154907378  0.0736980517  0.9564472050 -0.0003191518  1.9690496015
# p-value: 5.009995e-01
modelo_gpr_global_3$par
modelo_gpr_global_3$broom.mgarch


#* GPR em nível simétrico

# Modelo GPR em nível

# Modelo GARCH-MIDAS com GPR Global 12 lags
# não significativo
modelo_gpr_global_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu        alpha         beta            m        theta           w2 
# 0.058170172  0.071453701  0.913617662  1.344517676 -0.003503815  2.588399376 
# p-value: 2.028305e-01
modelo_gpr_global_sim$par
modelo_gpr_global_sim$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 6 lags
# não significativo

modelo_gpr_global_6_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu        alpha         beta            m        theta           w2 
# 0.061062777  0.070849221  0.914166265  1.462225507 -0.004616317  1.000086036 
# p-value: 8.260498e-02
modelo_gpr_global_6_sim$par
modelo_gpr_global_6_sim$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 3 lags
# não significativo

modelo_gpr_global_3_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06029270  0.07131621  0.91422585  1.13794506 -0.00130876  1.00006246 
# p-value: 2.261793e-01
modelo_gpr_global_3_sim$par
modelo_gpr_global_3_sim$broom.mgarch











# Modelo  GARCH-MIDAS com GPR Global padronizado
# não significativo
modelo_gpr_global_z <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global_z",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
modelo_gpr_global_z$par
modelo_gpr_global_z$broom.mgarch


## Vamos testar a estacionariedade do GPR
acf(
  base_mfgarch_completa$gpr_global,
  lag.max = 48,
  main = "ACF - GPR Global em nível"
)

# Muita persistência, necessário estacionarizar



# Modelo com Log-diferença do GPR com 6 lags
# Deu positivo, mas não significativo (primeiro theta positivo até agora!)
# Modelo com 12 lags deu negativo

modelo_gpr_global_dlog_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03830274 0.02944666 0.91574443 0.07301990 0.90930602 0.25552214 3.55470523 
# p-value: 4.220011e-01

modelo_gpr_global_dlog_6$par
modelo_gpr_global_dlog_6$broom.mgarch


# # Modelo com Log-diferença do GPR com 3 lags

# Positivo e significativo a 10% ! p-value 0.00895

modelo_gpr_global_dlog_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03597286 0.02917944 0.91655669 0.07285777 0.92191373 0.43949946 1.16563611 
# p-value: 1.288602e-01

modelo_gpr_global_dlog_3$par
modelo_gpr_global_dlog_3$broom.mgarch

# comparando BIC entre os dois
modelo_gpr_global_dlog_3$bic
modelo_gpr_global_dlog$bic

# resultados semelhantes, mas k = 6 melhor


# # Modelo com Log-diferença do GPR com 3 lags e assimétrico

# Positivo e  e nao significativo

modelo_gpr_global_dlog_3_assim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03597286 0.02917944 0.91655669 0.07285777 0.92191373 0.43949946 1.16563611 
# p-value: 1.288602e-01 
# opg.p.value: 5.060728e-02

modelo_gpr_global_dlog_3_assim$par
modelo_gpr_global_dlog_3_assim$broom.mgarch





# # Modelo com Log-diferença do GPR com 2 lags

# Positivo e não significativo

modelo_gpr_global_dlog_2 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 2,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.06041784 0.07061676 0.91546665 0.99566673 0.21760519 1.00001592 
# p-value: 6.985779e-01

modelo_gpr_global_dlog_2$par
modelo_gpr_global_dlog_2$broom.mgarch


# # Modelo com Log-diferença do GPR com 4 lags

# Positivo e não significativo

modelo_gpr_global_dlog_4 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 4,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#       mu      alpha       beta          m      theta         w2 
# 0.06036051 0.07018106 0.91559755 0.97573642 0.19933884 2.42827781 
# p-value: 5.337953e-01

modelo_gpr_global_dlog_4$par
modelo_gpr_global_dlog_4$broom.mgarch


### MODELOS COM DIFERENÇA DO GPR GLOBAL

## Modelo com Diferença do GPR 6 lags
# positivo, mas não significativo 

modelo_gpr_global_d_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = TRUE,
  weighting = "beta.restricted"
)

modelo_gpr_global_d_6$par
modelo_gpr_global_d_6$broom.mgarch

# Modelo com diferença do GPR com 3 lags
# positivo, mas não significativo a 10%

modelo_gpr_global_d_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

modelo_gpr_global_d_3$par
modelo_gpr_global_d_3$broom.mgarch

## GPR GLOBAL EM LOG

# # Modelo com Log do GPR com 12 lags assimétrico

# Negativo e não significativo

modelo_gpr_global_log_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03283620  0.02505897  0.91598111  0.07930984  1.89965057 -0.21797808  4.53067995
# p-value: 3.448345e-01
#! MODELO CONVERGE

modelo_gpr_global_log_12$par
modelo_gpr_global_log_12$broom.mgarch
modelo_gpr_global_log_12$optim$convergence

# # Modelo com Log do GPR com 6 lags assimétrico


modelo_gpr_global_log_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03901995  0.03003374  0.91423474  0.07222827  2.80058120 -0.41198534  1.37587320 
# p-value: 1.007506e-01

modelo_gpr_global_log_6$par
modelo_gpr_global_log_6$broom.mgarch

# # Modelo com Log do GPR com 3 lags assimétrico

# Positivo e não significativo

modelo_gpr_global_log_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03604818  0.02961751  0.91380634  0.07603684  1.62457008 -0.14778201  1.00000692 
# p-value: 1.542663e-01

modelo_gpr_global_log_3$par
modelo_gpr_global_log_3$broom.mgarch

# ! Hipótese por enquanto: Devido a diversas especificações estarem dando não significativas, partimos do pressuposto de que a agregação do ibovespa está afetando os coeficientes
# ! e diferentes setores reagem de forma diferente ao risco geopolítico. Dessa forma, precisamos fazer a análise setorial deles

#* GPR SIMETRICO

## GPR GLOBAL EM LOG

# # Modelo com Log do GPR com 12 lags simétrico

# Negativo e não significativo

modelo_gpr_global_log_12_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05831367  0.07154329  0.91231240  3.70632548 -0.59585001  2.76715749 
# p-value: 0.0568209250

modelo_gpr_global_log_12_sim$par
modelo_gpr_global_log_12_sim$broom.mgarch

# # Modelo com Log do GPR com 6 lags simétrico

# Positivo e não significativo

modelo_gpr_global_log_6_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06170380  0.07107284  0.91240234  4.17729599 -0.69912994  1.00007967 
# p-value: 0.0140724570

modelo_gpr_global_log_6_sim$par
modelo_gpr_global_log_6_sim$broom.mgarch

# # Modelo com Log do GPR com 3 lags assimétrico

# Positivo e não significativo

modelo_gpr_global_log_3_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06136771  0.07139919  0.91280250  2.60126043 -0.35207538  1.00005584  
# p-value: 8.642575e-01

modelo_gpr_global_log_3_sim$par
modelo_gpr_global_log_3_sim$broom.mgarch

#* Modelos com LOG-DIFF simetricos

# Modelo com Log-diferença do GPR com 6 lags)

modelo_gpr_global_dlog_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#          mu        alpha         beta            m        theta           w2 
# 0.058013155  0.071061124  0.914988642  0.985385831 -0.005578794  2.470243992 
# p-value: 3.339807e-01

modelo_gpr_global_dlog_sim$par
modelo_gpr_global_dlog_sim$broom.mgarch



# Modelo com Log-diferença do GPR com 6 lags

modelo_gpr_global_dlog_6_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.06141032 0.07045546 0.91539729 0.97960369 0.26632892 2.86182396 
# p-value: 5.134995e-01

modelo_gpr_global_dlog_6_sim$par
modelo_gpr_global_dlog_6_sim$broom.mgarch


# # Modelo com Log-diferença do GPR com 3 lags

modelo_gpr_global_dlog_3_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.05959316 0.06973897 0.91704556 1.01012147 0.49603868 1.00002991 
# p-value: 8.955343e-02

modelo_gpr_global_dlog_3_sim$par
modelo_gpr_global_dlog_3_sim$broom.mgarch
