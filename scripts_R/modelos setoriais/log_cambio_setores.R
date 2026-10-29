# Pacotes
library(readxl)
library(mfGARCH)
library(dplyr)

# Painel com setores
painel_midas <- read_excel("D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados/base_setores_final.xlsx")


# Arrumando dados para o pacote mfgarch
painel_midas_limpo <- painel_midas %>%
  mutate(
    # 1. Corrige o problema do POSIXct que causou o erro anterior
    date = as.Date(date),
    
    # 2. Garante que a chave do MIDAS seja lida como um rótulo de grupo
    year_month = as.Date(paste0(year_month, "-01")),
    
    # 3. Força a tipagem numérica (segurança extra após importação do Excel)
    finance_log_ret = as.numeric(finance_log_ret),
    energy_log_ret = as.numeric(energy_log_ret),
    basic_products_log_ret = as.numeric(basic_products_log_ret),
    consumer_log_ret = as.numeric(consumer_log_ret),
    log_gpr_global = as.numeric(log_gpr_global)) %>%
  na.omit()

  #* TESTANDO CAMBIO E GPR + CAMBIO PARA OS SETORES

  #* 1. Consumo
    
    # 12 LAGS
modelo_consumo_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03294971  0.03075880  0.90976412  0.08052339 -0.27698945 -0.12313484 17.75317577 
# P-VALUE: 2.859376e-01
modelo_consumo_cambio$par
modelo_consumo_cambio$broom.mgarch
modelo_consumo_cambio$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03294971  0.01462610 2.427162e-02  0.099848386 7.414019e-01
# alpha alpha  0.03075880  0.00778708 7.815810e-05  0.007204095 1.958000e-05
# beta   beta  0.90976412  0.01354965 0.000000e+00  0.006670231 0.000000e+00
# gamma gamma  0.08052339  0.01485100 5.890737e-08  0.015240589 1.267509e-07
# m         m -0.27698945  0.78337031 7.236485e-01  0.489203164 5.712542e-01
# theta theta -0.12313484  0.11539466 2.859376e-01  0.057785286 3.309713e-02
# w2       w2 17.75317577 32.64796720 5.865957e-01 16.255225566 2.747663e-01



# LOG-LOG

modelo_consumo_log_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 18,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 18,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)


# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03099629 0.02855877 0.89997327 0.08920559 0.51046077 0.24360086 1.56366919 
# theta.two     w2.two 
# 0.16279982 1.00002089 
# P-VALUE THETA: 7.482024e-01
# P-VALUE THETA 2: 9.007169e-01
modelo_consumo_log_gpr_log_cambio$par
modelo_consumo_log_gpr_log_cambio$broom.mgarch
modelo_consumo_log_gpr_log_cambio$optim$convergence
#! modelo converge

#               term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu 0.03099629  0.01534348 4.336673e-02 0.108560974 7.752461e-01
# alpha         alpha 0.02855877  0.01247605 2.207425e-02 0.007577180 1.638705e-04
# beta           beta 0.89997327  0.03792827 0.000000e+00 0.007307899 0.000000e+00
# gamma         gamma 0.08920559  0.02013817 9.437432e-06 0.016996300 1.533175e-07
# m                 m 0.51046077 12.13748566 9.664536e-01 1.094756091 6.410165e-01
# theta         theta 0.24360086  0.75885622 7.482024e-01 0.194045412 2.093406e-01
# w2               w2 1.56366919  2.33651521 5.033479e-01 2.747114284 5.692175e-01
# theta.two theta.two 0.16279982  1.30494791 9.007169e-01 0.076652185 3.368042e-02
# w2.two       w2.two 1.00002089 19.93932029 9.600003e-01 0.827232204 2.267106e-01




#* Bens Básicos
# 12 LAGS
modelo_basicos_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06155564 0.04768864 0.90916610 0.04352587 2.34083070 0.15582820 1.22519132 
# P-VALUE: 1.863331e-01
modelo_basicos_cambio$par
modelo_basicos_cambio$broom.mgarch
modelo_basicos_cambio$optim$convergence
#! modelo converge

#        term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.06155564  0.02230101 5.776293e-03 0.210844160 7.703260e-01
# alpha alpha 0.04768864  0.01045640 5.098065e-06 0.010621707 7.131214e-06
# beta   beta 0.90916610  0.02362187 0.000000e+00 0.009214065 0.000000e+00
# gamma gamma 0.04352587  0.01762807 1.354444e-02 0.021740219 4.527506e-02
# m         m 2.34083070  0.81039106 3.870595e-03 0.765837206 2.238894e-03
# theta theta 0.15582820  0.11791724 1.863331e-01 0.108287401 1.501436e-01
# w2       w2 1.22519132  1.74839164 4.834569e-01 1.424911672 3.898791e-01


# LOG-LOG

modelo_basicos_log_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
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


# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06378032  0.05088419  0.89251903  0.04855974  6.14396258 -0.66289008 
#         w2   theta.two      w2.two 
# 4.51407078  0.26912015  1.48314728 
# P-VALUE THETA: 1.098843e-03
# P-VALUE THETA 2: 1.414307e-02
modelo_basicos_log_gpr_log_cambio$par
modelo_basicos_log_gpr_log_cambio$broom.mgarch
modelo_basicos_log_gpr_log_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.06378032  0.02212331 3.939738e-03  0.21206948 7.636036e-01
# alpha         alpha  0.05088419  0.01033008 8.400337e-07  0.01146634 9.092561e-06
# beta           beta  0.89251903  0.02237275 0.000000e+00  0.01100502 0.000000e+00
# gamma         gamma  0.04855974  0.01756123 5.689371e-03  0.02384924 4.173908e-02
# m                 m  6.14396258  1.43989897 1.981724e-05  1.15825715 1.129889e-07
#theta         theta -0.66289008  0.20309664 1.098843e-03  0.18734455 4.026402e-04
# w2               w2  4.51407078  1.73927846 9.448920e-03  2.86927481 1.156622e-01
# theta.two theta.two  0.26912015  0.10968339 1.414307e-02  0.09341190 3.964092e-03
# w2.two       w2.two  1.48314728  0.72028222 3.948278e-02  0.85319460 8.214989e-02

#* Financeiro

# 12 LAGS
modelo_financeiro_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.06302550  0.03165248  0.92541715  0.05845362  0.42486873 -0.12066945  9.29270041 
# P-VALUE: 3.110956e-01 
modelo_financeiro_cambio$par
modelo_financeiro_cambio$broom.mgarch
modelo_financeiro_cambio$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06302550  0.02037772 1.982351e-03  0.148595030 6.714617e-01
# alpha alpha  0.03165248  0.00663558 1.841207e-06  0.006937899 5.060669e-06
# beta   beta  0.92541715  0.01128613 0.000000e+00  0.007025042 0.000000e+00
# gamma gamma  0.05845362  0.01232845 2.122879e-06  0.014289512 4.301358e-05
# m         m  0.42486873  0.82474979 6.064492e-01  0.706972929 5.478607e-01
# theta theta -0.12066945  0.11912960 3.110956e-01  0.088335735 1.719289e-01
# w2       w2  9.29270041  8.00707198 2.458202e-01 10.094599958 3.572794e-01



# LOG-LOG

modelo_financeiro_log_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06363328  0.03201620  0.92493075  0.05797684  1.07238641 -0.14688258  4.49759649 -0.12489352  8.00275691 
# P-VALUE THETA: 5.473273e-01 
# P-VALUE THETA 2: 2.486774e-01
# BIC: 24331.27
modelo_financeiro_log_gpr_log_cambio$par
modelo_financeiro_log_gpr_log_cambio$broom.mgarch
modelo_financeiro_log_gpr_log_cambio$optim$convergence
#! NÃo converge

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06363328 0.020338633 1.755873e-03  0.154304526 6.800553e-01
# alpha         alpha  0.03201620 0.006649849 1.475135e-06  0.007061298 5.786633e-06
# beta           beta  0.92493075 0.011022877 0.000000e+00  0.007227952 0.000000e+00
# gamma         gamma  0.05797684 0.012034330 1.452805e-06  0.014795251 8.906071e-05
# m                 m  1.07238641 1.474436009 4.670301e-01  1.313267547 4.141692e-01
# theta         theta -0.14688258 0.244084301 5.473273e-01  0.229771173 5.226565e-01
# w2               w2  4.49759649 2.369255920 5.765446e-02 12.676273154 7.227362e-01
#theta.two theta.two -0.12489352 0.108267180 2.486774e-01  0.094607514 1.867941e-01
#w2.two       w2.two  8.00275691 3.441714369 2.005998e-02  8.731532579 3.593863e-01



#* Energia
# 12 LAGS
modelo_energia_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.05821519 0.04957252 0.88012545 0.07492618 2.34127744 0.16805249 1.69593385 
# P-VALUE: 1.935791e-01 
modelo_energia_cambio$par
modelo_energia_cambio$broom.mgarch
modelo_energia_cambio$optim$convergence
#! modelo converge

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.05821519 0.020343319 4.214599e-03 0.175472149 7.400681e-01
# alpha alpha 0.04957252 0.008990783 3.513585e-08 0.010622966 3.063106e-06
# beta   beta 0.88012545 0.018072054 0.000000e+00 0.009866774 0.000000e+00
# gamma gamma 0.07492618 0.020363729 2.337890e-04 0.022918996 1.078620e-03
# m         m 2.34127744 0.880010292 7.802208e-03 0.598509685 9.159349e-05
# theta theta 0.16805249 0.129264793 1.935791e-01 0.086183264 5.118286e-02
# w2       w2 1.69593385 0.743406280 2.253061e-02 1.497851353 2.575317e-01


# LOG-LOG

modelo_energia_log_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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
  
  gamma = TRUE,
  multi.start = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.05821052  0.04972402  0.87900800  0.07493506  3.13699093 -0.15264148  3.57677110  0.18146926  1.67872711 

# BIC: 24289.3
modelo_energia_log_gpr_log_cambio$par
modelo_energia_log_gpr_log_cambio$broom.mgarch
modelo_energia_log_gpr_log_cambio$optim$convergence
#! converge

#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.05821052 0.020338462 4.208564e-03  0.17652573 7.415839e-01
# alpha         alpha  0.04972402 0.008999513 3.291284e-08  0.01069517 3.332221e-06
# beta           beta  0.87900800 0.018079445 0.000000e+00  0.01000695 0.000000e+00
# gamma         gamma  0.07493506 0.020211440 2.092686e-04  0.02310164 1.179830e-03
# m                 m  3.13699093 1.337523747 1.900810e-02  1.04920216 2.790850e-03
# theta         theta -0.15264148 0.203820207 4.539159e-01  0.18077165 3.984524e-01
# w2               w2  3.57677110 2.477531135 1.488280e-01 10.16433553 7.249176e-01
# theta.two theta.two  0.18146926 0.127291362 1.539777e-01  0.08511703 3.300709e-02
# w2.two       w2.two  1.67872711 0.708759525 1.785833e-02  1.36030199 2.171716e-01


