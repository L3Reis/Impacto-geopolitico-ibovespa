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

#* Setor de energia (obtido do scirpt teste_setores_GPR)
 
modelo_energia_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_energia_12$par
modelo_energia_12$broom.mgarch
modelo_energia_12$optim$convergence
modelo_energia_12$variance.ratio
modelo_energia_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05875969  0.05065364  0.88539647  0.07027062  1.85462330 -0.13733609  3.30473089 


#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.05875969 0.020275379 3.754539e-03  0.173081789 7.342400e-01
# alpha alpha  0.05065364 0.008822035 9.372073e-09  0.010482590 1.350541e-06
#beta   beta  0.88539647 0.015572387 0.000000e+00  0.009134439 0.000000e+00
# gamma gamma  0.07027062 0.018533954 1.497683e-04  0.022115436 1.485780e-03
# m         m  1.85462330 1.068565169 8.263101e-02  0.913598529 4.235450e-02
# theta theta -0.13733609 0.228214911 5.473179e-01  0.196419282 4.844279e-01
# w2       w2  3.30473089 2.542766124 1.937176e-01 11.045806915 7.647995e-01

# BIC: 24274.63

#! Modelo converge


modelo_energia_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_energia_18$par
modelo_energia_18$broom.mgarch
modelo_energia_18$optim$convergence
modelo_energia_18$variance.ratio
modelo_energia_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06159009  0.05088281  0.88579715  0.06974083  1.84609364 -0.13585843  5.86790445 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06159009 0.020413491 2.551830e-03  0.173507514 7.226115e-01
# alpha alpha  0.05088281 0.008973728 1.426333e-08  0.010581767 1.520396e-06
# beta   beta  0.88579715 0.016296843 0.000000e+00  0.009182965 0.000000e+00
# gamma gamma  0.06974083 0.018780797 2.044838e-04  0.022305893 1.768612e-03
# m         m  1.84609364 1.138634185 1.049484e-01  0.963016276 5.523906e-02
# theta theta -0.13585843 0.241931313 5.744173e-01  0.207240544 5.121078e-01
# w2       w2  5.86790445 5.468027736 2.832128e-01 19.910031632 7.682070e-01

# BIC: 23767.71

#! Modelo converge


modelo_energia_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_energia_24$par
modelo_energia_24$broom.mgarch
modelo_energia_24$optim$convergence
modelo_energia_24$variance.ratio
modelo_energia_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06164366  0.05140576  0.88549688  0.06907814  2.87898261 -0.35938295  8.31100072 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06164366 0.020563520 2.720083e-03  0.176974917 7.276009e-01
# alpha alpha  0.05140576 0.008897427 7.577439e-09  0.010796272 1.922201e-06
# beta   beta  0.88549688 0.015262064 0.000000e+00  0.009342784 0.000000e+00
# gamma gamma  0.06907814 0.018561816 1.980282e-04  0.022831118 2.481291e-03
# m         m  2.87898261 1.265715224 2.293056e-02  1.061095670 6.663349e-03
# theta theta -0.35938295 0.268434232 1.806319e-01  0.221133965 1.041238e-01
# w2       w2  8.31100072 3.754962436 2.687436e-02 12.747468025 5.144188e-01

# BIC: 23234.75

#! Modelo converge


modelo_energia_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_energia_36$par
modelo_energia_36$broom.mgarch
modelo_energia_36$optim$convergence
modelo_energia_36$variance.ratio
modelo_energia_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06530651  0.05353080  0.88705565  0.06128869  3.29863377 -0.45711751  9.48599287 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06530651  0.02078702 1.679718e-03  0.187660198 7.278371e-01
# alpha alpha  0.05353080  0.00926405 7.544264e-09  0.011466841 3.036832e-06
# beta   beta  0.88705565  0.01546446 0.000000e+00  0.009756267 0.000000e+00
# gamma gamma  0.06128869  0.01898205 1.243238e-03  0.023525966 9.183383e-03
# m         m  3.29863377  1.51124105 2.905557e-02  1.166488972 4.686453e-03
# theta theta -0.45711751  0.32214163 1.559003e-01  0.246804720 6.400536e-02
# w2       w2  9.48599287 11.63136218 4.147558e-01 13.392719093 4.787622e-01

# BIC: 22122.78

#! Modelo converge

#* Setor de energia com a variancia do cambio

# modelo log cambio
modelo_energia_cambio_12 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_energia_cambio_12$par
modelo_energia_cambio_12$broom.mgarch
modelo_energia_cambio_12$optim$convergence
modelo_energia_cambio_12$variance.ratio
modelo_energia_cambio_12$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.05776517 0.04928701 0.87963375 0.07573594 2.39553964 0.17607480 1.70530917 

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.05776517 0.020346950 4.525389e-03 0.175498475 7.420430e-01
# alpha alpha 0.04928701 0.008974401 3.975081e-08 0.010602508 3.341664e-06
# beta   beta 0.87963375 0.018168630 0.000000e+00 0.009903712 0.000000e+00
# gamma gamma 0.07573594 0.020479366 2.171694e-04 0.022961039 9.721871e-04
# m         m 2.39553964 0.867526374 5.756383e-03 0.593421677 5.417938e-05
# theta theta 0.17607480 0.127445697 1.671046e-01 0.085542239 3.955774e-02
# w2       w2 1.70530917 0.739295831 2.107340e-02 1.431022439 2.333899e-01

# BIC: 24271.2

#! Modelo converge


modelo_energia_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 18,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_energia_cambio_18$par
modelo_energia_cambio_18$broom.mgarch
modelo_energia_cambio_18$optim$convergence
modelo_energia_cambio_18$variance.ratio
modelo_energia_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06059038  0.04910209  0.90101468  0.06332629  0.01874641 -0.18358577 19.13837357 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.06059038 0.020359522 2.920144e-03  0.159702842 7.043950e-01
# alpha alpha  0.04910209 0.008573267 1.020122e-08  0.009882685 6.747396e-07
# beta   beta  0.90101468 0.019622830 0.000000e+00  0.008347281 0.000000e+00
# gamma gamma  0.06332629 0.018875861 7.939885e-04  0.019508266 1.169871e-03
# m         m  0.01874641 0.985940456 9.848301e-01  0.637155190 9.765280e-01
# theta theta -0.18358577 0.147113182 2.120596e-01  0.091731984 4.535708e-02
# w2       w2 19.13837357 7.710573742 1.306126e-02 14.704672154 1.930817e-01

# BIC: 23762.85

#! Modelo converge

modelo_energia_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_energia_cambio_24$par
modelo_energia_cambio_24$broom.mgarch
modelo_energia_cambio_24$optim$convergence
modelo_energia_cambio_24$variance.ratio
modelo_energia_cambio_24$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06125071 0.05045704 0.88057059 0.07402627 2.19189416 0.14773271 2.61290949 

#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.06125071 0.020598765 2.944113e-03 0.175531227 7.271307e-01
# alpha alpha 0.05045704 0.009092785 2.870966e-08 0.010795267 2.953950e-06
# beta   beta 0.88057059 0.020156271 0.000000e+00 0.009984718 0.000000e+00
# gamma gamma 0.07402627 0.021430741 5.519085e-04 0.023201767 1.420050e-03
# m         m 2.19189416 1.148092012 5.624114e-02 0.784663402 5.215399e-03
# theta theta 0.14773271 0.166987955 3.763234e-01 0.116572802 2.050480e-01
# w2       w2 2.61290949 4.829814772 5.885105e-01 3.278641496 4.254808e-01

# BIC: 23236.27

#! Modelo converge


modelo_energia_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 36,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_energia_cambio_36$par
modelo_energia_cambio_36$broom.mgarch
modelo_energia_cambio_36$optim$convergence
modelo_energia_cambio_36$variance.ratio
modelo_energia_cambio_36$bic

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06478627 0.05214396 0.88606091 0.06525818 1.58665729 0.06016604 3.56051203

#       term   estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu 0.06478627 0.020818178 1.858215e-03  0.186067710 7.277001e-01
# alpha alpha 0.05214396 0.009230142 1.610904e-08  0.011285623 3.830226e-06
# beta   beta 0.88606091 0.018498901 0.000000e+00  0.009970366 0.000000e+00
# gamma gamma 0.06525818 0.020446570 1.414655e-03  0.023525411 5.538116e-03
# m         m 1.58665729 1.372673220 2.477267e-01  0.953619729 9.614721e-02
# theta theta 0.06016604 0.200935008 7.646117e-01  0.142941422 6.738179e-01
# w2       w2 3.56051203 3.495758641 3.084293e-01 13.293050065 7.888166e-01

# BIC: 22126.65

#! Modelo converge

#* GPR + cambio para o energia

# LOG/LOG
modelo_energia_log_gpr_log_cambio_12 <- fit_mfgarch(
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

modelo_energia_log_gpr_log_cambio_12$par
modelo_energia_log_gpr_log_cambio_12$broom.mgarch
modelo_energia_log_gpr_log_cambio_12$optim$convergence
modelo_energia_log_gpr_log_cambio_12$variance.ratio
modelo_energia_log_gpr_log_cambio_12$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.05821052  0.04972402  0.87900800  0.07493506  3.13699093 -0.15264148  3.57677110  0.18146926  1.67872711 


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

# BIC: 24287.81
#! modelo converge


modelo_energia_log_gpr_log_cambio_18 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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

modelo_energia_log_gpr_log_cambio_18$par
modelo_energia_log_gpr_log_cambio_18$broom.mgarch
modelo_energia_log_gpr_log_cambio_18$optim$convergence
modelo_energia_log_gpr_log_cambio_18$variance.ratio
modelo_energia_log_gpr_log_cambio_18$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06133248  0.05058764  0.88004516  0.07362360  2.81544843 -0.12110752  6.44864799  0.15569250  2.24629849 


#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.06133248 0.020455077 2.714022e-03  0.17558597 7.268628e-01
# alpha         alpha  0.05058764 0.009134339 3.056259e-08  0.01074707 2.512538e-06
# beta           beta  0.88004516 0.020367721 0.000000e+00  0.01001758 0.000000e+00
# gamma         gamma  0.07362360 0.021161185 5.029486e-04  0.02304502 1.399409e-03
# m                 m  2.81544843 1.414419037 4.653220e-02  1.14286412 1.375870e-02
# theta         theta -0.12110752 0.216285821 5.755190e-01  0.19142832 5.269609e-01
# w2               w2  6.44864799 5.634666920 2.524331e-01 23.19699179 7.810162e-01
# theta.two theta.two  0.15569250 0.149708986 2.983549e-01  0.10200388 1.269254e-01
# w2.two       w2.two  2.24629849 1.937579414 2.463207e-01  2.32293351 3.335393e-01

# BIC: 23782.8
#! modelo converge

modelo_energia_log_gpr_log_cambio_24 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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

modelo_energia_log_gpr_log_cambio_24$par
modelo_energia_log_gpr_log_cambio_24$broom.mgarch
modelo_energia_log_gpr_log_cambio_24$optim$convergence
modelo_energia_log_gpr_log_cambio_24$variance.ratio
modelo_energia_log_gpr_log_cambio_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06143519  0.05127224  0.88158512  0.07153595  3.67596140 -0.31420011  9.04631924  0.15068870  2.16440506 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06143519 0.020589130 2.846321e-03  0.178098001 7.301308e-01
# alpha         alpha  0.05127224 0.009082059 1.647576e-08  0.010896836 2.535664e-06
# beta           beta  0.88158512 0.019377763 0.000000e+00  0.009877746 0.000000e+00
# gamma         gamma  0.07153595 0.020704592 5.501370e-04  0.023262889 2.104248e-03
# m                 m  3.67596140 1.531414412 1.637850e-02  1.246674440 3.192026e-03
# theta         theta -0.31420011 0.246658064 2.027241e-01  0.205399202 1.260899e-01
# w2               w2  9.04631924 4.092014930 2.705489e-02 15.142907328 5.502427e-01
#  theta.two theta.two  0.15068870 0.177357182 3.955286e-01  0.120933535 2.127477e-01
# w2.two       w2.two  2.16440506 4.329573697 6.171372e-01  2.620197354 4.087777e-01

# BIC: 23250.53
#! modelo converge


modelo_energia_log_gpr_log_cambio_36 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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

modelo_energia_log_gpr_log_cambio_36$par
modelo_energia_log_gpr_log_cambio_36$broom.mgarch
modelo_energia_log_gpr_log_cambio_36$optim$convergence
modelo_energia_log_gpr_log_cambio_36$variance.ratio
modelo_energia_log_gpr_log_cambio_36$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06588456  0.05317042  0.88650819  0.06143866  2.61445408 -0.40553012  8.82995010 -0.06498271 -0.42976133


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06588456 0.020689317 1.450181e-03  0.188733801 7.270238e-01
# alpha         alpha  0.05317042 0.009133765 5.839038e-09  0.011413623 3.185141e-06
# beta           beta  0.88650819 0.015829430 0.000000e+00  0.009813035 0.000000e+00
# gamma         gamma  0.06143866 0.018970405 1.200934e-03  0.023557123 9.105397e-03
# m                 m  2.61445408 2.137821468 2.213476e-01  1.659905734 1.152422e-01
# theta         theta -0.40553012 0.312873021 1.949241e-01  0.249682958 1.043373e-01
#  w2               w2  8.82995010 8.765353896 3.137572e-01 14.205471145 5.342128e-01
# theta.two theta.two -0.06498271 0.166412454 6.961728e-01  0.135537409 6.316222e-01
# w2.two       w2.two -0.42976133 2.384085312 8.569464e-01  2.527207499 8.649678e-01

# BIC: 22139.27
#! modelo converge