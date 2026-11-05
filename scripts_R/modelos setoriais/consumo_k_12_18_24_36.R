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

#* Setor de consumo (obtido do scirpt teste_setores_GPR)
 
modelo_consumo_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_consumo_12$par
modelo_consumo_12$broom.mgarch
modelo_consumo_12$optim$convergence
modelo_consumo_12$variance.ratio
modelo_consumo_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03177581  0.02714343  0.90344830  0.08881391 -2.46746577  0.64805396  1.20715035  
# P-VALUE: 2.064101e-02

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03177581 0.014631235 2.987235e-02 0.106893882 7.662642e-01
# alpha alpha  0.02714343 0.007989332 6.801460e-04 0.007304158 2.022722e-04
# beta   beta  0.90344830 0.012546514 0.000000e+00 0.006796200 0.000000e+00
# gamma gamma  0.08881391 0.015341588 7.076666e-09 0.016364468 5.723183e-08
# m         m -2.46746577 1.294368911 5.661026e-02 0.948947596 9.316594e-03
# theta theta  0.64805396 0.277951336 1.972504e-02 0.194646846 8.703853e-04
# w2       w2  1.20715035 0.734263673 1.001703e-01 0.816413879 1.392470e-01

# BIC: 20303.11 

#! Modelo converge


modelo_consumo_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_consumo_18$par
modelo_consumo_18$broom.mgarch
modelo_consumo_18$optim$convergence
modelo_consumo_18$variance.ratio
modelo_consumo_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03555884  0.02724151  0.90157374  0.08925328 -3.18447427  0.79661424  1.73533200 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03555884 0.014705935 1.560641e-02 0.101711203 7.266345e-01
# alpha alpha  0.02724151 0.007969352 6.301720e-04 0.007388414 2.268613e-04
# beta   beta  0.90157374 0.012756375 0.000000e+00 0.007080855 0.000000e+00
# gamma gamma  0.08925328 0.015641942 1.156466e-08 0.016139394 3.199566e-08
# m         m -3.18447427 1.247444431 1.068613e-02 0.947666904 7.784920e-04
# theta theta  0.79661424 0.266829088 2.831290e-03 0.197783392 5.632263e-05
# w2       w2  1.73533200 0.811576868 3.249868e-02 1.067046136 1.038869e-01

# BIC: 19802.76

#! Modelo converge


modelo_consumo_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_consumo_24$par
modelo_consumo_24$broom.mgarch
modelo_consumo_24$optim$convergence
modelo_consumo_24$variance.ratio
modelo_consumo_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03713623  0.02670641  0.90291590  0.08513653 -3.00858571  0.75045131  2.25198083 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03713623 0.014794970 1.207115e-02 0.110867573 7.376551e-01
# alpha alpha  0.02670641 0.007933064 7.613741e-04 0.007556080 4.086619e-04
# beta   beta  0.90291590 0.011962706 0.000000e+00 0.007273885 0.000000e+00
# gamma gamma  0.08513653 0.015210161 2.176586e-08 0.017078549 6.196254e-07
# m         m -3.00858571 1.247167862 1.585074e-02 1.018349277 3.133030e-03
# theta theta  0.75045131 0.266652258 4.887682e-03 0.208141793 3.115751e-04
# w2       w2  2.25198083 0.925400619 1.495281e-02 1.411218097 1.105399e-01

# BIC: 19240.37

#! Modelo converge


modelo_consumo_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_consumo_36$par
modelo_consumo_36$broom.mgarch
modelo_consumo_36$optim$convergence
modelo_consumo_36$variance.ratio
modelo_consumo_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03914312  0.02671980  0.90033616  0.08564732 -3.18311313  0.78153530  1.43975742 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03914312 0.014880662 8.526710e-03 0.114270760 7.319392e-01
# alpha alpha  0.02671980 0.008071587 9.317825e-04 0.007791474 6.049964e-04
# beta   beta  0.90033616 0.012806706 0.000000e+00 0.007719881 0.000000e+00
# gamma gamma  0.08564732 0.016388073 1.730308e-07 0.017564042 1.081010e-06
# m         m -3.18311313 1.361493883 1.938952e-02 1.147587437 5.541536e-03
# theta theta  0.78153530 0.293118092 7.669548e-03 0.235285401 8.948748e-04
# w2       w2  1.43975742 0.580287569 1.309739e-02 0.758229563 5.758538e-02

# BIC: 18235.4

#! Modelo converge

#* Setor de consumo com a variancia do cambio

# modelo log cambio
modelo_consumo_cambio_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_consumo_cambio_12$par
modelo_consumo_cambio_12$broom.mgarch
modelo_consumo_cambio_12$optim$convergence
modelo_consumo_cambio_12$variance.ratio
modelo_consumo_cambio_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03295138  0.03075477  0.90975355  0.08052788 -0.27698980 -0.12313601 17.75317580

#       term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03295138  0.014624971 2.425348e-02  0.099866644 7.414349e-01
# alpha alpha  0.03075477  0.007786455 7.822380e-05  0.007204123 1.963062e-05
# beta   beta  0.90975355  0.013561211 0.000000e+00  0.006671309 0.000000e+00
# gamma gamma  0.08052788  0.014852448 5.898233e-08  0.015242009 1.268876e-07
# m         m -0.27698980  0.784323937 7.239704e-01  0.489078160 5.711553e-01
# theta theta -0.12313601  0.115573795 2.866805e-01  0.057773101 3.305845e-02
# w2       w2 17.75317580 32.701115509 5.872041e-01 16.252739515 2.746929e-01

# BIC: 20305.64

#! Modelo converge


modelo_consumo_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_consumo_cambio_18$par
modelo_consumo_cambio_18$broom.mgarch
modelo_consumo_cambio_18$optim$convergence
modelo_consumo_cambio_18$variance.ratio
modelo_consumo_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03594623  0.03144472  0.91094895  0.07887576 -0.60852572 -0.17063478 17.67384235 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03594623 0.014731058 1.468050e-02  0.092507303 6.975889e-01
# alpha alpha  0.03144472 0.007780812 5.315205e-05  0.007203712 1.270810e-05
# beta   beta  0.91094895 0.012481661 0.000000e+00  0.006690979 0.000000e+00
# gamma gamma  0.07887576 0.014355128 3.915852e-08  0.014611272 6.728217e-08
# m         m -0.60852572 0.692867369 3.797964e-01  0.522332011 2.440121e-01
# theta theta -0.17063478 0.103063374 9.779684e-02  0.068786051 1.311395e-02
# w2       w2 17.67384235 9.752010282 6.993551e-02 11.681085157 1.302718e-01

# BIC: 19806.77

#! Modelo converge

modelo_consumo_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_consumo_cambio_24$par
modelo_consumo_cambio_24$broom.mgarch
modelo_consumo_cambio_24$optim$convergence
modelo_consumo_cambio_24$variance.ratio
modelo_consumo_cambio_24$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03778794 0.02939572 0.90229561 0.08174201 1.34818580 0.12817552 1.64317719

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.03778794 0.014814418 1.074900e-02 0.109395390 7.297746e-01
# alpha alpha 0.02939572 0.007926205 2.083466e-04 0.007688468 1.316498e-04
# beta   beta 0.90229561 0.012288943 0.000000e+00 0.007458247 0.000000e+00
# gamma gamma 0.08174201 0.015379792 1.067177e-07 0.016949583 1.416591e-06
# m         m 1.34818580 1.081356428 2.124876e-01 0.696429508 5.288559e-02
# theta theta 0.12817552 0.160886005 4.256338e-01 0.101952293 2.086773e-01
# w2       w2 1.64317719 0.587099448 5.129202e-03 1.764767367 3.518012e-01

# BIC: 19248.39

#! Modelo converge


modelo_consumo_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_consumo_cambio_36$par
modelo_consumo_cambio_36$broom.mgarch
modelo_consumo_cambio_36$optim$convergence
modelo_consumo_cambio_36$variance.ratio
modelo_consumo_cambio_36$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.04031251 0.02898915 0.89919217 0.08213599 1.44765104 0.14978096 2.49473224 

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.04031251 0.014885781 6.766510e-03 0.117576439 7.317021e-01
# alpha alpha 0.02898915 0.008063637 3.243337e-04 0.008030851 3.065235e-04
# beta   beta 0.89919217 0.013165952 0.000000e+00 0.008012558 0.000000e+00
# gamma gamma 0.08213599 0.016502906 6.455656e-07 0.017941810 4.696446e-06
# m         m 1.44765104 1.154373615 2.098211e-01 0.779146020 6.316943e-02
# theta theta 0.14978096 0.171317440 3.819609e-01 0.116745934 1.995043e-01
# w2       w2 2.49473224 1.333804584 6.142994e-02 2.576483293 3.329094e-01

# BIC: 18243

#! Modelo converge

#* GPR + cambio para o consumo

# LOG/LOG
modelo_consumo_log_gpr_log_cambio_12 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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

modelo_consumo_log_gpr_log_cambio_12$par
modelo_consumo_log_gpr_log_cambio_12$broom.mgarch
modelo_consumo_log_gpr_log_cambio_12$optim$convergence
modelo_consumo_log_gpr_log_cambio_12$variance.ratio
modelo_consumo_log_gpr_log_cambio_12$bic

# Resultado
#       mu      alpha       beta      gamma          m      theta         w2  theta.two     w2.two 
# 0.03099629 0.02855877 0.89997327 0.08920559 0.51046077 0.24360086 1.56366919 0.16279982 1.00002089 


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

# BIC: 20322.69
#! modelo converge


modelo_consumo_log_gpr_log_cambio_18 <- fit_mfgarch(
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

modelo_consumo_log_gpr_log_cambio_18$par
modelo_consumo_log_gpr_log_cambio_18$broom.mgarch
modelo_consumo_log_gpr_log_cambio_18$optim$convergence
modelo_consumo_log_gpr_log_cambio_18$variance.ratio
modelo_consumo_log_gpr_log_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03438192  0.02772363  0.90780767  0.08641432 -4.25412713  0.84470277  1.53134003 -0.12768867 26.22835633  


#               term    estimate  rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03438192  0.014740205 1.967300e-02  0.094485944 7.159446e-01
# alpha         alpha  0.02772363  0.007783579 3.683059e-04  0.007194265 1.164060e-04
# beta           beta  0.90780767  0.012276022 0.000000e+00  0.006828668 0.000000e+00
# gamma         gamma  0.08641432  0.014869168 6.186216e-09  0.015019661 8.745931e-09
# m                 m -4.25412713  1.539730026 5.728951e-03  1.104279876 1.169660e-04
# theta         theta  0.84470277  0.293313593 3.978488e-03  0.215306297 8.735846e-05
# w2               w2  1.53134003  0.600633271 1.078644e-02  0.974485198 1.160817e-01
# theta.two theta.two -0.12768867  0.080638658 1.133150e-01  0.057847420 2.729062e-02
# w2.two       w2.two 26.22835633 22.728710778 2.485106e-01 23.379452548 2.619241e-01

# BIC: 19813.6
#! modelo converge

modelo_consumo_log_gpr_log_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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

modelo_consumo_log_gpr_log_cambio_24$par
modelo_consumo_log_gpr_log_cambio_24$broom.mgarch
modelo_consumo_log_gpr_log_cambio_24$optim$convergence
modelo_consumo_log_gpr_log_cambio_24$variance.ratio
modelo_consumo_log_gpr_log_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03682444  0.02533574  0.90081323  0.08765116 -1.87849385  0.69773485  2.17550574  0.13203199  2.03267529  


#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.03682444 0.014798771 1.283394e-02 0.112194612 7.427458e-01
# alpha         alpha  0.02533574 0.007955805 1.449743e-03 0.007580288 8.308171e-04
# beta           beta  0.90081323 0.012582669 0.000000e+00 0.007611649 0.000000e+00
# gamma         gamma  0.08765116 0.015739350 2.563253e-08 0.017294365 4.016130e-07
# m                 m -1.87849385 1.763145943 2.866852e-01 1.263274831 1.370139e-01
# theta         theta  0.69773485 0.262015191 7.745760e-03 0.203061992 5.902224e-04
# w2               w2  2.17550574 1.042659491 3.693364e-02 1.446220530 1.325119e-01
# theta.two theta.two  0.13203199 0.141681948 3.513934e-01 0.096208367 1.699531e-01
# w2.two       w2.two  2.03267529 0.715520231 4.499551e-03 2.085710051 3.297724e-01

# BIC: 19256.29
#! modelo converge


modelo_consumo_log_gpr_log_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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

modelo_consumo_log_gpr_log_cambio_36$par
modelo_consumo_log_gpr_log_cambio_36$broom.mgarch
modelo_consumo_log_gpr_log_cambio_36$optim$convergence
modelo_consumo_log_gpr_log_cambio_36$variance.ratio
modelo_consumo_log_gpr_log_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03880793  0.02424890  0.89660331  0.09022234 -2.04980462  0.82420492  1.13123452  0.19801444  3.27146401  


#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.03880793 0.014888169 9.143834e-03 0.117277204 7.407142e-01
# alpha         alpha  0.02424890 0.008191315 3.073207e-03 0.007799732 1.877594e-03
# beta           beta  0.89660331 0.013757978 0.000000e+00 0.008235908 0.000000e+00
# gamma         gamma  0.09022234 0.017364267 2.037729e-07 0.017956878 5.050379e-07
# m                 m -2.04980462 1.609970609 2.029493e-01 1.279830837 1.092393e-01
# theta         theta  0.82420492 0.278280287 3.058665e-03 0.231340258 3.670058e-04
# w2               w2  1.13123452 0.389325934 3.665224e-03 0.554053517 4.117710e-02
# theta.two theta.two  0.19801444 0.153124488 1.959558e-01 0.116924723 9.035639e-02
# w2.two       w2.two  3.27146401 1.141179093 4.147233e-03 2.484488467 1.879206e-01

# BIC: 18249.93
#! modelo converge