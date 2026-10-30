# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)

# arrumando a questão da visualização dos dados
options(width = 300)

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
  ) |>filter(date >= '2000-02-02') |> 
  na.omit()
  


# Conforme a referência de Segnon (2024), foram selecionados como especificações principais os modelos agregados em transformação logarítmica e com K = 12
# Eles são destacados nesse script em especial para melhor localização e para facilitar possíveis diagnósticos futuros

#* Modelos GPR Global assimétrico e simétrico (originais do script teste_garch_midas)

# Modelo assimétrico

# 12 lags

modelo_gpr_global_log_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_gpr_global_log_12$par
modelo_gpr_global_log_12$broom.mgarch
modelo_gpr_global_log_12$optim$convergence
modelo_gpr_global_log_12$variance.ratio
modelo_gpr_global_log_12$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03494588  0.02598677  0.91431982  0.07864421  1.29605982 -0.09431131  4.37428087 


#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03494588 0.017245450 4.272558e-02  0.122649674 7.757021e-01
# alpha alpha  0.02598677 0.006427221 5.271854e-05  0.006970291 1.928379e-04
# beta   beta  0.91431982 0.010709313 0.000000e+00  0.006599816 0.000000e+00
# gamma gamma  0.07864421 0.014535211 6.281396e-08  0.014863643 1.216191e-07
# m         m  1.29605982 1.013012379 2.007521e-01  0.878458834 1.401106e-01
# theta theta -0.09431131 0.217564488 6.646612e-01  0.187645495 6.152430e-01
# w2       w2  4.37428087 1.897310524 2.113766e-02 16.670592255 7.930169e-01

#BIC: 22325.39

#! MODELO CONVERGE

# 18 lags

modelo_gpr_global_log_18 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 18,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_gpr_global_log_18$par
modelo_gpr_global_log_18$broom.mgarch
modelo_gpr_global_log_18$optim$convergence
modelo_gpr_global_log_18$variance.ratio
modelo_gpr_global_log_18$bic

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03902487 0.02568512 0.91471257 0.07882581 0.42660317 0.09037345 1.01143547 


#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.03902487 0.017412205 2.501074e-02 0.116029739 7.366182e-01
# alpha alpha 0.02568512 0.006519931 8.165955e-05 0.006968503 2.279074e-04
# beta   beta 0.91471257 0.010917784 0.000000e+00 0.006649223 0.000000e+00
# gamma gamma 0.07882581 0.015074351 1.703050e-07 0.014480060 5.217073e-08
# m         m 0.42660317 2.238533254 8.488604e-01 1.054958370 6.859339e-01
# theta theta 0.09037345 0.483831795 8.518277e-01 0.228294103 6.922053e-01
# w2       w2 1.01143547 4.608664050 8.262887e-01 6.286767555 8.721854e-01

#BIC: 21821.67

#! MODELO CONVERGE

# 24 lags

modelo_gpr_global_log_24 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_gpr_global_log_24$par
modelo_gpr_global_log_24$broom.mgarch
modelo_gpr_global_log_24$optim$convergence
modelo_gpr_global_log_24$variance.ratio
modelo_gpr_global_log_24$bic

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03967623 0.02560153 0.91379241 0.07894686 0.34697648 0.10327459 2.57473590 


#       term   estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu 0.03967623 0.017446846 2.295876e-02  0.130783207 7.616049e-01
# alpha alpha 0.02560153 0.006586468 1.014954e-04  0.007345852 4.918287e-04
# beta   beta 0.91379241 0.010827197 0.000000e+00  0.006861019 0.000000e+00
# gamma gamma 0.07894686 0.015012925 1.451621e-07  0.016265419 1.212018e-06
# m         m 0.34697648 0.298470766 2.450267e-01  1.197035676 7.719210e-01
# theta theta 0.10327459 0.058521954 7.761163e-02  0.246691334 6.754801e-01
# w2       w2 2.57473590 3.208751849 4.223156e-01 12.546642464 8.374056e-01

#BIC: 21279

#! MODELO CONVERGE

# 36 lags

modelo_gpr_global_log_36 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 36,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_gpr_global_log_36$par
modelo_gpr_global_log_36$broom.mgarch
modelo_gpr_global_log_36$optim$convergence
modelo_gpr_global_log_36$variance.ratio
modelo_gpr_global_log_36$bic


 #        mu       alpha        beta       gamma           m       theta          w2 
 # 0.04305986  0.02860936  0.90852616  0.07767929  2.99001783 -0.47238272 11.53381577 

#        term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.04305986  0.01758911 1.436129e-02  0.131566629 7.434519e-01
# alpha alpha  0.02860936  0.00695033 3.850711e-05  0.007783881 2.374189e-04
# beta   beta  0.90852616  0.01141223 0.000000e+00  0.007614400 0.000000e+00
# gamma gamma  0.07767929  0.01595924 1.131021e-06  0.016795837 3.747554e-06
# m         m  2.99001783  1.17850441 1.117657e-02  1.056942661 4.670377e-03
# theta theta -0.47238272  0.25318562 6.207544e-02  0.220986144 3.254822e-02
#w2       w2 11.53381577  4.58639265 1.191032e-02 12.533134042 3.574338e-01

# BIC: 20204.04


# 24 lags simétrico

modelo_gpr_global_log_24_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  gamma = FALSE,
  weighting = "beta.restricted",
  multi.start = TRUE
)




modelo_gpr_global_log_24_sim$par
modelo_gpr_global_log_24_sim$broom.mgarch
modelo_gpr_global_log_24_sim$optim$convergence
modelo_gpr_global_log_24_sim$variance.ratio
modelo_gpr_global_log_24_sim$bic

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06312441  0.07153595  0.91180091  3.72624454 -0.60740859  7.37025594 


#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.06312441 0.017644241 0.0003467357 0.076419417 0.408789473
# alpha alpha  0.07153595 0.008345217 0.0000000000 0.006442255 0.000000000
# beta   beta  0.91180091 0.009921534 0.0000000000 0.007461679 0.000000000
# m         m  3.72624454 1.397173529 0.0076534485 1.239018513 0.002634786
# theta theta -0.60740859 0.296334665 0.0403899517 0.264923397 0.021861213
# w2       w2  7.37025594 2.802264153 0.0085357874 6.682829843 0.270086003

#BIC:  21330.55

#! MODELO CONVERGE



#* VARIANCIA CAMBIAL

modelo_log_cambio_12_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch
modelo_log_cambio_12_asi$optim$convergence
modelo_log_cambio_12_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03469713  0.02451824  0.92670322  0.07364268 -1.05595128 -0.28447703  9.01072228 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03469713 0.017128085 4.279091e-02 0.097213520 7.211541e-01
# alpha alpha  0.02451824 0.006173974 7.150413e-05 0.006322360 1.053101e-04
# beta   beta  0.92670322 0.010326013 0.000000e+00 0.005878277 0.000000e+00
# gamma gamma  0.07364268 0.012684641 6.411408e-09 0.011815547 4.584846e-10
# m         m -1.05595128 0.923164384 2.526905e-01 0.723490689 1.444212e-01
# theta theta -0.28447703 0.133038706 3.249218e-02 0.088334951 1.279959e-03
# w2       w2  9.01072228 3.522038017 1.051602e-02 3.994014875 2.406711e-02

#BIC: 22309.39

#! MODELO CONVERGE


modelo_log_cambio_18_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 18,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_18_asi$par
modelo_log_cambio_18_asi$broom.mgarch
modelo_log_cambio_18_asi$optim$convergence
modelo_log_cambio_18_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03706328  0.02508687  0.92723321  0.07353351 -1.32327196 -0.32668400 13.55044442 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03706328 0.017266662 3.183131e-02 0.088558599 6.755692e-01
# alpha alpha  0.02508687 0.006276615 6.418297e-05 0.006394636 8.741234e-05
# beta   beta  0.92723321 0.010224798 0.000000e+00 0.005837643 0.000000e+00
# gamma gamma  0.07353351 0.012493811 3.966246e-09 0.011372535 1.007072e-10
# m         m -1.32327196 1.060982243 2.123190e-01 0.672374684 4.906144e-02
# theta theta -0.32668400 0.158028113 3.871037e-02 0.093376949 4.677939e-04
# w2       w2 13.55044442 6.119162453 2.679935e-02 5.477203888 1.336205e-02

#BIC: 21801.31

#! MODELO CONVERGE


modelo_log_cambio_24_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_24_asi$par
modelo_log_cambio_24_asi$broom.mgarch
modelo_log_cambio_24_asi$optim$convergence
modelo_log_cambio_24_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03863316  0.02589676  0.92647957  0.07214614 -1.26393468 -0.31439496 16.42383058 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03863316 0.017257545 2.518036e-02 0.090478801 6.693897e-01
# alpha alpha  0.02589676 0.006325774 4.242720e-05 0.006623818 9.243304e-05
# beta   beta  0.92647957 0.010466660 0.000000e+00 0.006016234 0.000000e+00
# gamma gamma  0.07214614 0.013002601 2.879773e-08 0.011880883 1.259681e-09
# m         m -1.26393468 1.091164069 2.467269e-01 0.736508593 8.614069e-02
# theta theta -0.31439496 0.161962999 5.223973e-02 0.101566276 1.965039e-03
# w2       w2 16.42383058 6.522656901 1.180346e-02 7.264771820 2.377476e-02

#BIC: 21262.57

#! MODELO CONVERGE

modelo_log_cambio_36_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 36,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_36_asi$par
modelo_log_cambio_36_asi$broom.mgarch
modelo_log_cambio_36_asi$optim$convergence
modelo_log_cambio_36_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.04013573  0.02669292  0.92557334  0.07054324 -1.44065167 -0.33299377 21.58232763 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.04013573 0.017442598 2.139062e-02 0.104490634 7.008981e-01
# alpha alpha  0.02669292 0.006389373 2.944644e-05 0.006898646 1.091489e-04
# beta   beta  0.92557334 0.010645000 0.000000e+00 0.006288397 0.000000e+00
# gamma gamma  0.07054324 0.013467771 1.623875e-07 0.012960094 5.235769e-08
# m         m -1.44065167 1.036888516 1.647115e-01 0.768577145 6.086982e-02
# theta theta -0.33299377 0.154511204 3.115063e-02 0.108727972 2.193997e-03
# w2       w2 21.58232763 5.828595041 2.132033e-04 9.516024757 2.332931e-02

#BIC: 20192.94

#! MODELO CONVERGE

# 24 lags simétrico

modelo_log_cambio_24_sim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 24,
  gamma = FALSE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_24_sim$par
modelo_log_cambio_24_sim$broom.mgarch
modelo_log_cambio_24_sim$optim$convergence
modelo_log_cambio_24_sim$bic

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.06182296  0.06969371  0.92116757 -1.38173627 -0.34722873 12.46783228 

#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.06182296 0.017705244 0.0004797987 0.074266818 0.405158430
# alpha alpha  0.06969371 0.007888083 0.0000000000 0.006137572 0.000000000
# beta   beta  0.92116757 0.009420341 0.0000000000 0.006806219 0.000000000
# m         m -1.38173627 1.265489086 0.2748948644 0.875736515 0.114611774
# theta theta -0.34722873 0.189535512 0.0669516746 0.133200681 0.009139032
# w2       w2 12.46783228 5.967715957 0.0366885156 6.132436275 0.042042815

#BIC: 21262.57

#! MODELO CONVERGE





#* MODELO GPR GLOBAL EM LOG E E CAMBIO EM LOG

# MODELO COM K = 12

modelo_log_gpr_log_cambio_12 <- fit_mfgarch(
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
  
  gamma = TRUE,
  multi.start = TRUE
)


modelo_log_gpr_log_cambio_12$par
modelo_log_gpr_log_cambio_12$broom.mgarch
modelo_log_gpr_log_cambio_12$optim$convergence
modelo_log_gpr_log_cambio_12$optim$final
modelo_log_gpr_log_cambio_12$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03512525  0.02477402  0.92691413  0.07284412 -0.89910790 -0.04194172  3.13872471 -0.28967102  8.61194104 

#               term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03512525  0.017598046 4.593670e-02  0.099480803 7.240242e-01
# alpha         alpha  0.02477402  0.006892385 3.251346e-04  0.006415847 1.127486e-04
# beta           beta  0.92691413  0.011806180 0.000000e+00  0.005962161 0.000000e+00
# gamma         gamma  0.07284412  0.012587409 7.162784e-09  0.012053240 1.507779e-09
# m                 m -0.89910790  7.274954404 9.016403e-01  1.348597990 5.049649e-01
# theta         theta -0.04194172  1.466967105 9.771910e-01  0.241927598 8.623647e-01
# w2               w2  3.13872471 33.089481986 9.244295e-01 30.972928038 9.192824e-01
# theta.two theta.two -0.28967102  0.144440281 4.491255e-02  0.091139781 1.481332e-03
# w2.two       w2.two  8.61194104  3.253576110 8.122965e-03  3.813413658 2.392518e-02

#BIC: 22326.87

#! MODELO CONVERGE

# MODELO COM K = 18

modelo_log_gpr_log_cambio_18 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
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



modelo_log_gpr_log_cambio_18$par
modelo_log_gpr_log_cambio_18$broom.mgarch
modelo_log_gpr_log_cambio_18$optim$convergence
modelo_log_gpr_log_cambio_18$optim$final
modelo_log_gpr_log_cambio_18$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03606092  0.02422238  0.92936504  0.07370892 -3.53559954  0.41897572  1.00029529 -0.36888480 11.50441223 

#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.03606092 0.017281472 3.691696e-02 0.085125431 6.718422e-01
# alpha         alpha  0.02422238 0.006132690 7.824450e-05 0.006388543 1.497197e-04
# beta           beta  0.92936504 0.009442749 0.000000e+00 0.005643371 0.000000e+00
# gamma         gamma  0.07370892 0.011929900 6.471228e-10 0.011177507 4.269873e-11
# m                 m -3.53559954 2.815410462 2.091876e-01 1.646154399 3.173017e-02
# theta         theta  0.41897572 0.485766466 3.884100e-01 0.314170386 1.823370e-01
# w2               w2  1.00029529 0.939104932 2.868044e-01 1.512244431 5.083148e-01
# theta.two theta.two -0.36888480 0.156399259 1.834354e-02 0.104213921 4.005999e-04
# w2.two       w2.two 11.50441223 3.921073481 3.346290e-03 4.424917265 9.324664e-03

#BIC: 21818.46

#! NÃO CONVERGE

# MODELO COM K = 24

modelo_log_gpr_log_cambio_24 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
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


modelo_log_gpr_log_cambio_24$par
modelo_log_gpr_log_cambio_24$broom.mgarch
modelo_log_gpr_log_cambio_24$optim$convergence
modelo_log_gpr_log_cambio_24$optim$final
modelo_log_gpr_log_cambio_24$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03791115  0.02640990  0.92543230  0.07216134  0.35503404 -0.32468128  9.58969413 -0.29960677 17.43063307 

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03791115 0.017297177 2.839729e-02  0.100265146 7.053496e-01
# alpha         alpha  0.02640990 0.006375977 3.441486e-05  0.006718028 8.452347e-05
# beta           beta  0.92543230 0.010605567 0.000000e+00  0.006263529 0.000000e+00
# gamma         gamma  0.07216134 0.012968182 2.629174e-08  0.012696036 1.317576e-08
# m                 m  0.35503404 1.598971932 8.242836e-01  1.579271714 8.221281e-01
# theta         theta -0.32468128 0.259300001 2.105169e-01  0.263879994 2.185426e-01
# w2               w2  9.58969413 3.395550166 4.739961e-03 14.212537580 4.998443e-01
# theta.two theta.two -0.29960677 0.157556288 5.722535e-02  0.098352959 2.317224e-03
#w2.two       w2.two 17.43063307 7.247777342 1.617412e-02  7.994182543 2.922674e-02

#BIC: 21277.65

#! CONVERGE

# MODELO COM K = 36

modelo_log_gpr_log_cambio_36 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
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



modelo_log_gpr_log_cambio_36$par
modelo_log_gpr_log_cambio_36$broom.mgarch
modelo_log_gpr_log_cambio_36$optim$convergence
modelo_log_gpr_log_cambio_36$optim$final
modelo_log_gpr_log_cambio_36$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.04329693  0.02665012  0.90662489  0.08101341  0.47667385 -0.31797000 17.07285579 -0.26483658  0.21633242

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.04329693 0.017526327 1.349644e-02  0.132535974 7.439095e-01
# alpha         alpha  0.02665012 0.007017507 1.460674e-04  0.007642585 4.883702e-04
# beta           beta  0.90662489 0.011701556 0.000000e+00  0.007819690 0.000000e+00
# gamma         gamma  0.08101341 0.016491064 8.989313e-07  0.016872195 1.574088e-06
# m                 m  0.47667385 1.970624170 8.088656e-01  1.559350852 7.598427e-01
# theta         theta -0.31797000 0.227867514 1.628904e-01  0.204135048 1.193174e-01
# w2               w2 17.07285579 6.846640238 1.264511e-02 23.577202689 4.689886e-01
# theta.two theta.two -0.26483658 0.212023005 2.116308e-01  0.145548940 6.882442e-02
#w2.two       w2.two  0.21633242 0.727913404 7.663174e-01  0.409769980 5.975436e-01

#BIC: 20216.49

#! CONVERGE

