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

# melhorar visualização
options(width = 300)

#* Setor de financeiro (obtido do scirpt teste_setores_GPR)
 
modelo_financeiro_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_financeiro_12$par
modelo_financeiro_12$broom.mgarch
modelo_financeiro_12$optim$convergence
modelo_financeiro_12$variance.ratio
modelo_financeiro_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06267784  0.03280560  0.92014965  0.05862086  2.08060785 -0.18602086  4.02411828 


#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06267784 0.020364424 2.085275e-03 0.165613178 7.050895e-01
# alpha alpha  0.03280560 0.006747823 1.164067e-06 0.007370937 8.560302e-06
# beta   beta  0.92014965 0.010243131 0.000000e+00 0.007348658 0.000000e+00
# gamma gamma  0.05862086 0.012444460 2.469904e-06 0.015730491 1.940992e-04
# m         m  2.08060785 1.072247054 5.232887e-02 0.956436521 2.960205e-02
# theta theta -0.18602086 0.231250416 4.211585e-01 0.212000993 3.802408e-01
# w2       w2  4.02411828 1.805456331 2.582289e-02 8.949043917 6.529483e-01

# BIC: 24316.69

#! Modelo converge


modelo_financeiro_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_financeiro_18$par
modelo_financeiro_18$broom.mgarch
modelo_financeiro_18$optim$convergence
modelo_financeiro_18$variance.ratio
modelo_financeiro_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06462939  0.03367191  0.92051171  0.05713436  2.15624183 -0.20199657  7.63167940 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06462939 0.020550417 1.661330e-03  0.164868061 6.950532e-01
# alpha alpha  0.03367191 0.006890971 1.027067e-06  0.007419631 5.673145e-06
# beta   beta  0.92051171 0.010778408 0.000000e+00  0.007299554 0.000000e+00
# gamma gamma  0.05713436 0.012567040 5.458008e-06  0.015805050 3.004132e-04
# m         m  2.15624183 1.101592526 5.030210e-02  0.986982228 2.891221e-02
# theta theta -0.20199657 0.235695260 3.914314e-01  0.217502755 3.530404e-01
# w2       w2  7.63167940 4.075660547 6.113729e-02 15.195655695 6.155074e-01

# BIC: 23772.82

#! Modelo converge


modelo_financeiro_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_financeiro_24$par
modelo_financeiro_24$broom.mgarch
modelo_financeiro_24$optim$convergence
modelo_financeiro_24$variance.ratio
modelo_financeiro_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06487614  0.03531190  0.91734279  0.05870523  3.16357931 -0.41845401  8.50152466 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06487614 0.020674681 1.701331e-03  0.165387834 6.948611e-01
# alpha alpha  0.03531190 0.006890873 2.984150e-07  0.007788694 5.794873e-06
# beta   beta  0.91734279 0.010107245 0.000000e+00  0.007690375 0.000000e+00
# gamma gamma  0.05870523 0.013049342 6.836635e-06  0.016679949 4.323461e-04
# m         m  3.16357931 1.200308539 8.397917e-03  1.175815802 7.133651e-03
# theta theta -0.41845401 0.257050731 1.035452e-01  0.240321492 8.164398e-02
# w2       w2  8.50152466 2.684221278 1.539028e-03 10.908006021 4.357536e-01

# BIC: 23247.21

#! Modelo converge


modelo_financeiro_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_financeiro_36$par
modelo_financeiro_36$broom.mgarch
modelo_financeiro_36$optim$convergence
modelo_financeiro_36$variance.ratio
modelo_financeiro_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06688177  0.03758942  0.91345878  0.05785884  3.93645277 -0.59293540 10.07935397 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06688177 0.020912008 1.382657e-03  0.174247750 7.011034e-01
# alpha alpha  0.03758942 0.007227479 1.983219e-07  0.008450245 8.654306e-06
# beta   beta  0.91345878 0.011136755 0.000000e+00  0.008407696 0.000000e+00
# gamma gamma  0.05785884 0.014008989 3.625697e-05  0.018285184 1.554847e-03
# m         m  3.93645277 1.348802245 3.517421e-03  1.235191065 1.437975e-03
# theta theta -0.59293540 0.290208229 4.103880e-02  0.258826522 2.197150e-02
# w2       w2 10.07935397 6.639092475 1.289684e-01 10.623805120 3.427469e-01

# BIC: 22119.2

#! Modelo converge

#* Setor de financeiro com a variancia do cambio

# modelo log cambio
modelo_financeiro_cambio_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_financeiro_cambio_12$par
modelo_financeiro_cambio_12$broom.mgarch
modelo_financeiro_cambio_12$optim$convergence
modelo_financeiro_cambio_12$variance.ratio
modelo_financeiro_cambio_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06302514  0.03164872  0.92541297  0.05845287  0.42486874 -0.12066958  9.29270041 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06302514 0.020376669 1.981408e-03  0.148636427 6.715497e-01
# alpha alpha  0.03164872 0.006635084 1.843124e-06  0.006937893 5.073630e-06
# beta   beta  0.92541297 0.011294410 0.000000e+00  0.007025808 0.000000e+00
# gamma gamma  0.05845287 0.012327897 2.121290e-06  0.014291374 4.312232e-05
# m         m  0.42486874 0.825467741 6.067623e-01  0.706790947 5.477576e-01
# theta theta -0.12066958 0.119282549 3.117159e-01  0.088314839 1.718270e-01
# w2       w2  9.29270041 8.018914067 2.465183e-01 10.092909085 3.571988e-01

# BIC: 24314.39
#! Modelo converge


modelo_financeiro_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_financeiro_cambio_18$par
modelo_financeiro_cambio_18$broom.mgarch
modelo_financeiro_cambio_18$optim$convergence
modelo_financeiro_cambio_18$variance.ratio
modelo_financeiro_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06306936  0.03221128  0.92829678  0.05702880 -0.27057602 -0.22856010 10.77652428 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06306936 0.020559018 2.156970e-03 0.133122772 6.356651e-01
# alpha alpha  0.03221128 0.006632402 1.193859e-06 0.006833120 2.429089e-06
# beta   beta  0.92829678 0.010777473 0.000000e+00 0.006731031 0.000000e+00
# gamma gamma  0.05702880 0.011777048 1.283012e-06 0.013375502 2.010917e-05
# m         m -0.27057602 1.015652024 7.899264e-01 0.791928089 7.326012e-01
#  theta theta -0.22856010 0.150764402 1.295172e-01 0.111442179 4.027378e-02
# w2       w2 10.77652428 5.249797766 4.009679e-02 7.287203883 1.391862e-01

# BIC: 23766.28

#! Modelo converge

modelo_financeiro_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_financeiro_cambio_24$par
modelo_financeiro_cambio_24$broom.mgarch
modelo_financeiro_cambio_24$optim$convergence
modelo_financeiro_cambio_24$variance.ratio
modelo_financeiro_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06519357  0.03367597  0.92435065  0.05880014 -0.08598106 -0.19684952 12.90433722 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06519357 0.020663826 1.605195e-03  0.133533612 6.253952e-01
# alpha alpha  0.03367597 0.006670337 4.450231e-07  0.007170944 2.650861e-06
# beta   beta  0.92435065 0.011105390 0.000000e+00  0.007223904 0.000000e+00
# gamma gamma  0.05880014 0.012598694 3.053885e-06  0.014115664 3.105412e-05
# m         m -0.08598106 1.015441805 9.325209e-01  0.830783609 9.175710e-01
# theta theta -0.19684952 0.147204585 1.811406e-01  0.117423069 9.365739e-02
# w2       w2 12.90433722 4.837293527 7.637762e-03 10.492352792 2.187420e-01

# BIC: 23245.54

#! Modelo converge


modelo_financeiro_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_financeiro_cambio_36$par
modelo_financeiro_cambio_36$broom.mgarch
modelo_financeiro_cambio_36$optim$convergence
modelo_financeiro_cambio_36$variance.ratio
modelo_financeiro_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06552811  0.03510096  0.92321316  0.05823163 -0.34066883 -0.23247340 18.82660227 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06552811  0.02079860 1.629344e-03  0.142518299 6.456689e-01
# alpha alpha  0.03510096  0.00692252 3.966747e-07  0.007663836 4.648044e-06
# beta   beta  0.92321316  0.01149013 0.000000e+00  0.007490357 0.000000e+00
#  gamma gamma  0.05823163  0.01314572 9.436401e-06  0.015202551 1.279428e-04
# m         m -0.34066883  1.05158741 7.459708e-01  0.794767145 6.681854e-01
# theta theta -0.23247340  0.15525366 1.342950e-01  0.119712089 5.214466e-02
# w2       w2 18.82660227  7.76461688 1.532248e-02 13.379085202 1.593780e-01

# BIC: 22117.9

#! Modelo converge

#* GPR + cambio para o financeiro

# LOG/LOG
modelo_financeiro_log_gpr_log_cambio_12 <- fit_mfgarch(
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
  
  gamma = TRUE,
  multi.start = TRUE
)

modelo_financeiro_log_gpr_log_cambio_12$par
modelo_financeiro_log_gpr_log_cambio_12$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_12$optim$convergence
modelo_financeiro_log_gpr_log_cambio_12$variance.ratio
modelo_financeiro_log_gpr_log_cambio_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06363285  0.03200964  0.92492661  0.05797408  1.07238649 -0.14688237  4.49759648 -0.12489410  8.00275693 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06363285 0.020335559 1.753175e-03  0.154360901 6.801677e-01
# alpha         alpha  0.03200964 0.006648717 1.476359e-06  0.007061118 5.808955e-06
# beta           beta  0.92492661 0.011032316 0.000000e+00  0.007228949 0.000000e+00
# gamma         gamma  0.05797408 0.012032295 1.448545e-06  0.014797152 8.931576e-05
# m                 m  1.07238649 1.473894205 4.668663e-01  1.312642278 4.139468e-01
# theta         theta -0.14688237 0.243788297 5.468416e-01  0.229638729 5.224173e-01
# w2               w2  4.49759648 2.368680881 5.759382e-02 12.672433608 7.226557e-01
# theta.two theta.two -0.12489410 0.108421843 2.493509e-01  0.094569968 1.866171e-01
# w2.two       w2.two  8.00275693 3.446693561 2.024021e-02  8.729006059 3.592472e-01

# BIC: 24331.27
#! modelo converge


modelo_financeiro_log_gpr_log_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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

modelo_financeiro_log_gpr_log_cambio_18$par
modelo_financeiro_log_gpr_log_cambio_18$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_18$optim$convergence
modelo_financeiro_log_gpr_log_cambio_18$variance.ratio
modelo_financeiro_log_gpr_log_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06346267  0.03314414  0.92776509  0.05587904  0.92345772 -0.24633696  6.19485838 -0.22279300 10.98655128 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
#  mu               mu  0.06346267 0.020503998 1.967226e-03  0.140790483 6.521628e-01
# alpha         alpha  0.03314414 0.006718392 8.083362e-07  0.007020952 2.349799e-06
# beta           beta  0.92776509 0.010697625 0.000000e+00  0.006886018 0.000000e+00
# gamma         gamma  0.05587904 0.011654289 1.629033e-06  0.013971258 6.345737e-05
# m                 m  0.92345772 1.507087859 5.400462e-01  1.417449207 5.147285e-01
# theta         theta -0.24633696 0.268718222 3.592941e-01  0.270182692 3.619045e-01
# w2               w2  6.19485838 2.936107067 3.486797e-02 11.554566877 5.918622e-01
# theta.two theta.two -0.22279300 0.144552422 1.232533e-01  0.110369548 4.352784e-02
# w2.two       w2.two 10.98655128 4.916462596 2.544063e-02  7.573351603 1.468675e-01

# BIC: 23782.74
#! modelo converge

modelo_financeiro_log_gpr_log_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 24,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)

modelo_financeiro_log_gpr_log_cambio_24$par
modelo_financeiro_log_gpr_log_cambio_24$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_24$optim$convergence
modelo_financeiro_log_gpr_log_cambio_24$variance.ratio
modelo_financeiro_log_gpr_log_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06463875  0.03437194  0.92378860  0.05801202  2.07014740 -0.46302568  8.72084797 -0.19790735 13.35007600 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06463875 0.020585446 1.689352e-03  0.144840799 6.553990e-01
# alpha         alpha  0.03437194 0.006716134 3.090929e-07  0.007360671 3.016764e-06
# beta           beta  0.92378860 0.010970477 0.000000e+00  0.007420720 0.000000e+00
# gamma         gamma  0.05801202 0.012482042 3.357573e-06  0.014923330 1.013500e-04
# m                 m  2.07014740 1.558996146 1.842204e-01  1.649883336 2.095792e-01
# theta         theta -0.46302568 0.293361123 1.144859e-01  0.279401861 9.747838e-02
# w2               w2  8.72084797 3.075198766 4.570179e-03 10.536740649 4.078626e-01
# theta.two theta.two -0.19790735 0.143790970 1.687119e-01  0.115518278 8.667346e-02
# w2.two       w2.two 13.35007600 5.012434009 7.735727e-03 10.744006927 2.140299e-01

# BIC: 23259.2
#! modelo converge


modelo_financeiro_log_gpr_log_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 36,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 36,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)

modelo_financeiro_log_gpr_log_cambio_36$par
modelo_financeiro_log_gpr_log_cambio_36$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_36$optim$convergence
modelo_financeiro_log_gpr_log_cambio_36$variance.ratio
modelo_financeiro_log_gpr_log_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06545656  0.03641068  0.92008719  0.05772522  2.50930606 -0.55057848  9.66948860 -0.18704486 21.62797906 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06545656 0.020788638 1.640154e-03  0.156710284 6.761727e-01
# alpha         alpha  0.03641068 0.007028879 2.217031e-07  0.008017411 5.586849e-06
#  beta           beta  0.92008719 0.011687438 0.000000e+00  0.008050817 0.000000e+00
# gamma         gamma  0.05772522 0.013245363 1.311690e-05  0.016533519 4.804863e-04
# m                 m  2.50930606 1.798414097 1.629289e-01  1.695744546 1.389356e-01
# theta         theta -0.55057848 0.319641145 8.498090e-02  0.300883539 6.726902e-02
# w2               w2  9.66948860 7.729881243 2.109625e-01 11.749620879 4.105297e-01
# theta.two theta.two -0.18704486 0.132842423 1.591250e-01  0.109226040 8.681254e-02
# w2.two       w2.two 21.62797906 9.127200184 1.780647e-02 17.897222132 2.268725e-01

# BIC: 22131.08
#! modelo converge