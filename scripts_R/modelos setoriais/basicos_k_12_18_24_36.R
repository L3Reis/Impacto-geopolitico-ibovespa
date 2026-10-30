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

#* Setor de bens_basicos (obtido do scirpt teste_setores_GPR)
 
modelo_bens_basicos_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_bens_basicos_12$par
modelo_bens_basicos_12$broom.mgarch
modelo_bens_basicos_12$optim$convergence
modelo_bens_basicos_12$variance.ratio
modelo_bens_basicos_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06376516  0.04873574  0.90469504  0.04335063  3.77976098 -0.54272031  4.27747223 


#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06376516  0.02222240 4.112390e-03  0.20983350 7.612159e-01
# alpha alpha  0.04873574  0.01022177 1.862151e-06  0.01079596 6.353894e-06
# beta   beta  0.90469504  0.02059045 0.000000e+00  0.00958066 0.000000e+00
# gamma gamma  0.04335063  0.01632603 7.923639e-03  0.02199395 4.872104e-02
# m         m  3.77976098  1.05009586 3.188986e-04  0.93097822 4.907510e-05
# theta theta -0.54272031  0.22247503 1.470898e-02  0.20553678 8.278328e-03
# w2       w2  4.27747223  1.90983734 2.511008e-02  3.44766790 2.147219e-01

# BIC: 24649.56

#! Modelo converge


modelo_bens_basicos_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_bens_basicos_18$par
modelo_bens_basicos_18$broom.mgarch
modelo_bens_basicos_18$optim$convergence
modelo_bens_basicos_18$variance.ratio
modelo_bens_basicos_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06581700  0.04736005  0.90873202  0.04143036  4.37731397 -0.67041862  4.91006626 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06581700  0.02249286 3.432085e-03 0.208340678 7.520705e-01
# alpha alpha  0.04736005  0.01045139 5.857730e-06 0.010511940 6.626130e-06
# beta   beta  0.90873202  0.02126651 0.000000e+00 0.009147839 0.000000e+00
# gamma gamma  0.04143036  0.01597471 9.500565e-03 0.021347430 5.228672e-02
# m         m  4.37731397  1.20359171 2.759645e-04 1.082630138 5.272166e-05
# theta theta -0.67041862  0.25365478 8.216639e-03 0.240395590 5.290142e-03
# w2       w2  4.91006626  2.81493049 8.110784e-02 3.573467506 1.694311e-01

# BIC: 24166.49

#! Modelo converge


modelo_bens_basicos_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_bens_basicos_24$par
modelo_bens_basicos_24$broom.mgarch
modelo_bens_basicos_24$optim$convergence
modelo_bens_basicos_24$variance.ratio
modelo_bens_basicos_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06506964  0.04835103  0.90679235  0.04328961  5.74084150 -0.96099939  8.28298581 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06506964 0.022661713 4.087269e-03 0.195288365 7.389850e-01
# alpha alpha  0.04835103 0.009861761 9.444148e-07 0.010546279 4.547403e-06
# beta   beta  0.90679235 0.019096766 0.000000e+00 0.009366382 0.000000e+00
# gamma gamma  0.04328961 0.016093771 7.148708e-03 0.021212052 4.127046e-02
# m         m  5.74084150 1.253936664 4.688714e-06 1.144101635 5.227408e-07
# theta theta -0.96099939 0.262264616 2.480782e-04 0.245823757 9.256306e-05
# w2       w2  8.28298581 2.113616739 8.896568e-05 5.237324125 1.137569e-01

# BIC: 23662.26

#! Modelo converge


modelo_bens_basicos_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_bens_basicos_36$par
modelo_bens_basicos_36$broom.mgarch
modelo_bens_basicos_36$optim$convergence
modelo_bens_basicos_36$variance.ratio
modelo_bens_basicos_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06038400  0.04971401  0.90580592  0.04223191  6.98663582 -1.23345735 10.39678295 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06038400 0.023122149 9.014016e-03 0.202379106 7.654202e-01
# alpha alpha  0.04971401 0.009602924 2.255233e-07 0.011082418 7.262546e-06
# beta   beta  0.90580592 0.018953197 0.000000e+00 0.009669507 0.000000e+00
# gamma gamma  0.04223191 0.016965863 1.280206e-02 0.021887978 5.367415e-02
# m         m  6.98663582 1.416274908 8.093174e-07 1.308142960 9.249598e-08
# theta theta -1.23345735 0.298884343 3.677314e-05 0.284752366 1.479730e-05
# w2       w2 10.39678295 4.443828180 1.930437e-02 6.076797791 8.709989e-02

# BIC: 22702.92

#! Modelo converge

#* Setor de bens_basicos com a variancia do cambio

# modelo log cambio
modelo_bens_basicos_cambio_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_bens_basicos_cambio_12$par
modelo_bens_basicos_cambio_12$broom.mgarch
modelo_bens_basicos_cambio_12$optim$convergence
modelo_bens_basicos_cambio_12$variance.ratio
modelo_bens_basicos_cambio_12$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06155876 0.04766382 0.90915439 0.04351110 2.34083170 0.15582775 1.22519113 

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.06155876  0.02230324 5.778700e-03 0.210982255 7.704608e-01
# alpha alpha 0.04766382  0.01044407 5.025518e-06 0.010620552 7.193350e-06
# beta   beta 0.90915439  0.02366166 0.000000e+00 0.009217586 0.000000e+00
# gamma gamma 0.04351110  0.01761949 1.353070e-02 0.021739439 4.534045e-02
# m         m 2.34083170  0.80950197 3.831714e-03 0.764594737 2.202070e-03
# theta theta 0.15582775  0.11783399 1.860235e-01 0.108120148 1.495151e-01
# w2       w2 1.22519113  1.74628980 4.829307e-01 1.423180848 3.893029e-01

# BIC: 24657.51

#! Modelo converge


modelo_bens_basicos_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_bens_basicos_cambio_18$par
modelo_bens_basicos_cambio_18$broom.mgarch
modelo_bens_basicos_cambio_18$optim$convergence
modelo_bens_basicos_cambio_18$variance.ratio
modelo_bens_basicos_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06532788  0.04238633  0.92688287  0.03828013  0.17983177 -0.16871478 30.07033244 

#       term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06532788  0.022306080 3.403795e-03  0.174002210 7.073312e-01
# alpha alpha  0.04238633  0.009402806 6.549158e-06  0.008762552 1.316725e-06
# beta   beta  0.92688287  0.018618450 0.000000e+00  0.007036078 0.000000e+00
# gamma gamma  0.03828013  0.014050100 6.439019e-03  0.016660158 2.157859e-02
# m         m  0.17983177  0.674156954 7.896612e-01  0.543200956 7.405996e-01
# theta theta -0.16871478  0.106075367 1.117181e-01  0.076009557 2.644239e-02
# w2       w2 30.07033244 30.789778510 3.287506e-01 27.980628914 2.825163e-01

# BIC: 24165.94

#! Modelo converge

modelo_bens_basicos_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_bens_basicos_cambio_24$par
modelo_bens_basicos_cambio_24$broom.mgarch
modelo_bens_basicos_cambio_24$optim$convergence
modelo_bens_basicos_cambio_24$variance.ratio
modelo_bens_basicos_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06598617  0.04466621  0.92304258  0.03857296  0.25441797 -0.15348863 29.27262028 

#       term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06598617  0.022439690 3.275764e-03  0.181409698 7.160513e-01
# alpha alpha  0.04466621  0.009284554 1.503209e-06  0.009376897 1.903278e-06
# beta   beta  0.92304258  0.017827438 0.000000e+00  0.007666526 0.000000e+00
# gamma gamma  0.03857296  0.014389971 7.350249e-03  0.018045379 3.255303e-02
# m         m  0.25441797  0.632149720 6.873420e-01  0.648100255 6.946448e-01
# theta theta -0.15348863  0.094518064 1.043955e-01  0.089317586 8.571288e-02
# w2       w2 29.27262028 14.562101661 4.441087e-02 28.980064316 3.124497e-01

# BIC: 23678.16

#! Modelo converge


modelo_bens_basicos_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_bens_basicos_cambio_36$par
modelo_bens_basicos_cambio_36$broom.mgarch
modelo_bens_basicos_cambio_36$optim$convergence
modelo_bens_basicos_cambio_36$variance.ratio
modelo_bens_basicos_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05963829  0.04949517  0.91265627  0.03911096  0.25207809 -0.15386761  1.01374771 

#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.05963829 0.022678827 8.546267e-03 0.212751642 0.779232898
# alpha alpha  0.04949517 0.009617344 2.654629e-07 0.011044827 0.000007419
# beta   beta  0.91265627 0.024451700 0.000000e+00 0.009032442 0.000000000
# gamma gamma  0.03911096 0.019404856 4.384974e-02 0.021862528 0.073622933
# m         m  0.25207809 1.945745972 8.969197e-01 1.667670060 0.879852776
# theta theta -0.15386761 0.284986544 5.892578e-01 0.245079078 0.530116360
# w2       w2  1.01374771 3.784960915 7.888258e-01 1.591046598 0.524022079

# BIC: 22728.04

#! Modelo converge

#* GPR + cambio para o bens_basicos

# LOG/LOG
modelo_bens_basicos_log_gpr_log_cambio_12 <- fit_mfgarch(
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
  
  gamma = TRUE,
  multi.start = TRUE
)

modelo_bens_basicos_log_gpr_log_cambio_12$par
modelo_bens_basicos_log_gpr_log_cambio_12$broom.mgarch
modelo_bens_basicos_log_gpr_log_cambio_12$optim$convergence
modelo_bens_basicos_log_gpr_log_cambio_12$variance.ratio
modelo_bens_basicos_log_gpr_log_cambio_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06378239  0.05089990  0.89252909  0.04856643  6.14396223 -0.66289502  4.51407074  0.26912713  1.48314721 


#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.06378239  0.02212405 3.939780e-03  0.21201257 7.635346e-01
# alpha         alpha  0.05089990  0.01033604 8.457381e-07  0.01146757 9.054775e-06
# beta           beta  0.89252909  0.02236181 0.000000e+00  0.01100275 0.000000e+00
# gamma         gamma  0.04856643  0.01756756 5.700104e-03  0.02385132 4.172870e-02
# m                 m  6.14396223  1.44104523 2.012083e-05  1.15915547 1.155631e-07
# theta         theta -0.66289502  0.20324741 1.108175e-03  0.18748821 4.067541e-04
# w2               w2  4.51407074  1.74027708 9.489947e-03  2.87072595 1.158464e-01
# theta.two theta.two  0.26912713  0.10976542 1.421282e-02  0.09348559 3.991803e-03
# w2.two       w2.two  1.48314721  0.72038395 3.951064e-02  0.85376205 8.235357e-02

# BIC: 24656.3
#! modelo converge


modelo_bens_basicos_log_gpr_log_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
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

modelo_bens_basicos_log_gpr_log_cambio_18$par
modelo_bens_basicos_log_gpr_log_cambio_18$broom.mgarch
modelo_bens_basicos_log_gpr_log_cambio_18$optim$convergence
modelo_bens_basicos_log_gpr_log_cambio_18$variance.ratio
modelo_bens_basicos_log_gpr_log_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06589510  0.04339382  0.92077972  0.04014372  2.79731790 -0.52031842  6.28536114 -0.13405561 35.18658810


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06589510  0.02229322 3.118227e-03  0.191886201 7.312921e-01
# alpha         alpha  0.04339382  0.01001677 1.476808e-05  0.009308821 3.137898e-06
# beta           beta  0.92077972  0.02096279 0.000000e+00  0.007959893 0.000000e+00
# gamma         gamma  0.04014372  0.01461128 6.006120e-03  0.018540934 3.037696e-02
# m                 m  2.79731790  1.65803609 9.157888e-02  1.281925402 2.910051e-02
# theta         theta -0.52031842  0.30296405 8.590131e-02  0.268228870 5.240035e-02
# w2               w2  6.28536114  3.09687658 4.239917e-02  5.656813448 2.665195e-01
# theta.two theta.two -0.13405561  0.08677026 1.223586e-01  0.071920011 6.232812e-02
# w2.two       w2.two 35.18658810 27.15686027 1.950859e-01 45.557896943 4.399079e-01

# BIC: 24176.81
#! modelo converge

modelo_bens_basicos_log_gpr_log_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
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

modelo_bens_basicos_log_gpr_log_cambio_24$par
modelo_bens_basicos_log_gpr_log_cambio_24$broom.mgarch
modelo_bens_basicos_log_gpr_log_cambio_24$optim$convergence
modelo_bens_basicos_log_gpr_log_cambio_24$variance.ratio
modelo_bens_basicos_log_gpr_log_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06521849  0.04568418  0.91548434  0.04170447  5.22246404 -0.98608823  7.85700511 -0.09597260 42.75974147 


#               term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06521849  0.022538784 3.808388e-03  0.184751501 7.240835e-01
# alpha         alpha  0.04568418  0.009793037 3.086692e-06  0.009741407 2.736197e-06
# beta           beta  0.91548434  0.019837673 0.000000e+00  0.008636034 0.000000e+00
# gamma         gamma  0.04170447  0.014963540 5.318653e-03  0.019327462 3.094469e-02
# m                 m  5.22246404  1.402569117 1.964810e-04  1.364697106 1.298040e-04
# theta         theta -0.98608823  0.299611368 9.975232e-04  0.273762081 3.157907e-04
# w2               w2  7.85700511  2.008112582 9.129569e-05  5.014081061 1.171175e-01
# theta.two theta.two -0.09597260  0.076276399 2.083117e-01  0.074245442 1.961357e-01
# w2.two       w2.two 42.75974147 19.537696414 2.862764e-02 72.887827792 5.574379e-01

# BIC: 23675.79
#! modelo converge


modelo_bens_basicos_log_gpr_log_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
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

modelo_bens_basicos_log_gpr_log_cambio_36$par
modelo_bens_basicos_log_gpr_log_cambio_36$broom.mgarch
modelo_bens_basicos_log_gpr_log_cambio_36$optim$convergence
modelo_bens_basicos_log_gpr_log_cambio_36$variance.ratio
modelo_bens_basicos_log_gpr_log_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.05924694  0.05069263  0.89810395  0.04553397  7.10039374 -1.04255532 12.21314175  0.15000778  4.68896433  


#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.05924694 0.023024408 1.007562e-02  0.21131799 7.791947e-01
# alpha         alpha  0.05069263 0.009692587 1.694719e-07  0.01171112 1.500682e-05
# beta           beta  0.89810395 0.021725174 0.000000e+00  0.01094915 0.000000e+00
# gamma         gamma  0.04553397 0.018439429 1.353472e-02  0.02354709 5.314504e-02
# m                 m  7.10039374 1.695732486 2.824003e-05  1.58594073 7.566862e-06
# theta         theta -1.04255532 0.243101167 1.798306e-05  0.25397804 4.044840e-05
# w2               w2 12.21314175 4.566590810 7.485081e-03  7.59198581 1.076844e-01
# theta.two theta.two  0.15000778 0.177021510 3.967730e-01  0.14142177 2.888208e-01
# w2.two       w2.two  4.68896433 3.069645886 1.266302e-01  6.72034882 4.853484e-01

# BIC: 22718.27
#! modelo converge