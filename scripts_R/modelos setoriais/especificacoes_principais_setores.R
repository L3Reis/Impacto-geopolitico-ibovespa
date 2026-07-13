# Para fácil diagnóstico e localização, nesse script contém as especificações principais para os setores

#* Setor de consumo (obtido do scirpt teste_setores_GPR)
 
modelo_consumo <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03177581  0.02714343  0.90344830  0.08881391 -2.46746577  0.64805396  1.20715035  
# P-VALUE: 2.064101e-02
modelo_consumo$par
modelo_consumo$broom.mgarch
modelo_consumo$optim$convergence
modelo_consumo$variance.ratio
#! Modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03177581 0.014631235 2.987235e-02 0.106893882 7.662642e-01
# alpha alpha  0.02714343 0.007989332 6.801460e-04 0.007304158 2.022722e-04
# beta   beta  0.90344830 0.012546514 0.000000e+00 0.006796200 0.000000e+00
# gamma gamma  0.08881391 0.015341588 7.076666e-09 0.016364468 5.723183e-08
# m         m -2.46746577 1.294368911 5.661026e-02 0.948947596 9.316594e-03
# theta theta  0.64805396 0.277951336 1.972504e-02 0.194646846 8.703853e-04
# w2       w2  1.20715035 0.734263673 1.001703e-01 0.816413879 1.392470e-01


#* Setor de consumo com a variancia do cambio

# modelo log cambio
modelo_consumo_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
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





# Modelo com dlog cambio

modelo_consumo_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.02738379  0.02973283  0.90534692  0.08555742  0.56456147 -0.42106820  3.28073592 
# P-VALUE: 7.580976e-01
modelo_consumo_dlog_cambio$par
modelo_consumo_dlog_cambio$broom.mgarch
modelo_consumo_dlog_cambio$optim$convergence

#! modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.02738379 0.014926288 6.656476e-02 0.102056083 7.884523e-01
# alpha alpha  0.02973283 0.008479214 4.539484e-04 0.007315749 4.819296e-05
# beta   beta  0.90534692 0.015036724 0.000000e+00 0.006915261 0.000000e+00
# gamma gamma  0.08555742 0.014701705 5.900350e-09 0.015880879 7.146482e-08
# m         m  0.56456147 0.115030875 9.205089e-07 0.226766305 1.278805e-02
# theta theta -0.42106820 1.367193398 7.580976e-01 0.343105163 2.197370e-01
# w2       w2  3.28073592 9.018769571 7.160316e-01 2.727924694 2.291122e-0


#* GPR + cambio para o consumo

# LOG/LOG
modelo_consumo_log_gpr_log_cambio <- fit_mfgarch(
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


# LOG/LOG-DIFF

modelo_consumo_log_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.02792469  0.02490800  0.90699788  0.09108835 -1.22332409  0.38107230  1.01150889 -0.65305694
# P-VALUE THETA: 1.957604e-01
# P-VALUE THETA 2: 6.548037e-02 
modelo_consumo_log_gpr_dlog_cambio$par
modelo_consumo_log_gpr_dlog_cambio$broom.mgarch
modelo_consumo_log_gpr_dlog_cambio$optim$convergence
#! modelo nao converge
#todo ha uma grande mudança quando usa multi ou nao



#* Bens básicos

 modelo_bens_basicos <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 12,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06337388  0.04841047  0.90506880  0.04317313  3.77950566 -0.54322738 
#         w2 
# 4.27747777 
# P-VALUE: 1.438017e-02
modelo_bens_basicos$par
modelo_bens_basicos$broom.mgarch
modelo_bens_basicos$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06337388  0.02221837 4.340147e-03 0.210392074 7.632482e-01
# alpha alpha  0.04841047  0.01012203 1.729703e-06 0.010749817 6.688224e-06
# beta   beta  0.90506880  0.02043366 0.000000e+00 0.009558332 0.000000e+00
# gamma gamma  0.04317313  0.01619599 7.683542e-03 0.021906582 4.874865e-02
# m         m  3.77950566  1.04758509 3.087724e-04 0.928445818 4.685447e-05
# theta theta -0.54322738  0.22194072 1.438017e-02 0.204978304 8.045124e-03
# w2       w2  4.27747777  1.90759646 2.493948e-02 3.435554453 2.131090e-01

#todo PEGAR DEPOIS OS MODELOS DE variancia cambial

modelo_basicos_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06334150  0.04543091  0.91600639  0.04266287  1.29745106 -0.46012532 
#         w2 
# 3.65773265 
# P-VALUE: 5.611909e-02
modelo_basicos_dlog_cambio$par
modelo_basicos_dlog_cambio$broom.mgarch
modelo_basicos_dlog_cambio$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06334150 0.022150784 4.242289e-03 0.198537460 7.496958e-01
# alpha alpha  0.04543091 0.009875406 4.216602e-06 0.009863247 4.103287e-06
# beta   beta  0.91600639 0.019474363 0.000000e+00 0.008327821 0.000000e+00
# gamma gamma  0.04266287 0.015464723 5.802793e-03 0.019847411 3.159137e-02
# m         m  1.29745106 0.130836916 0.000000e+00 0.218011548 2.660289e-09
# theta theta -0.46012532 0.240889477 5.611909e-02 0.454616103 3.114814e-01
# w2       w2  3.65773265 1.504497736 1.504897e-02 3.700644400 3.229547e-01


# LOG/LOG-DIFF
modelo_basicos_log_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
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
  
  gamma = TRUE,
  multi.start = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06482894  0.04724652  0.90690269  0.04529137  4.04373264 -0.59949785  4.01739632 
#  theta.two      w2.two 
# -0.44051019  3.84226097 
# P-VALUE THETA: 2.079305e-02
# P-VALUE THETA 2: 8.852193e-02
modelo_basicos_log_gpr_dlog_cambio$par
modelo_basicos_log_gpr_dlog_cambio$broom.mgarch
modelo_basicos_log_gpr_dlog_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.06482894  0.02213779 3.406805e-03 0.204971416 7.517879e-01
# alpha         alpha  0.04724652  0.01002255 2.428739e-06 0.010402754 5.579401e-06
# beta           beta  0.90690269  0.01968598 0.000000e+00 0.009392265 0.000000e+00
# gamma         gamma  0.04529137  0.01558211 3.653487e-03 0.021419038 3.446934e-02
# m                 m  4.04373264  1.07101142 1.596099e-04 0.966673307 2.875065e-05
# theta         theta -0.59949785  0.22699744 8.266428e-03 0.214877841 5.271663e-03
# w2               w2  4.01739632  1.88660761 3.321881e-02 2.982112935 1.779273e-01
# theta.two theta.two -0.44051019  0.20463430 3.134460e-02 0.404804443 2.765046e-01
# w2.two       w2.two  3.84226097  1.24532211 2.033092e-03 3.648642822 2.923108e-01

#todo liberado fazer tabela, mas lembrar de atualizar a tabela de apendices com a versao multi e tabela excel



#* Financeiro


modelo_financeiro <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 12,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06250966  0.03295878  0.91990364  0.05860811  2.08057078 -0.18617158  4.02411755 

modelo_financeiro$par
modelo_financeiro$broom.mgarch
modelo_financeiro$optim$convergence
#! modelo converge

# mu       mu  0.06250966 0.020364970 2.144392e-03 0.165897142 7.063240e-01
# alpha alpha  0.03295878 0.006772979 1.137548e-06 0.007395451 8.325843e-06
# beta   beta  0.91990364 0.010305372 0.000000e+00 0.007374175 0.000000e+00
# gamma gamma  0.05860811 0.012460759 2.558463e-06 0.015786329 2.051519e-04
# m         m  2.08057078 1.072737416 5.244087e-02 0.955687272 2.947748e-02
# theta theta -0.18617158 0.231345098 4.209724e-01 0.211766663 3.793278e-01
# w2       w2  4.02411755 1.807167553 2.596372e-02 8.945496195 6.528197e-01

#todo PEGAR DEPOIS OS MODELOS DE variancia cambial

# 12 LAGS
modelo_financeiro_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06135552  0.03190656  0.92114071  0.06136383  1.23058007 -0.41448985  2.41795779
# P-VALUE: 1.088464e-01 
modelo_financeiro_dlog_cambio$par
modelo_financeiro_dlog_cambio$broom.mgarch
modelo_financeiro_dlog_cambio$optim$convergence


#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.06135552 0.020345809 2.564421e-03 0.153772014 6.898909e-01
# alpha alpha  0.03190656 0.006688422 1.838459e-06 0.007172729 8.654651e-06
# beta   beta  0.92114071 0.009504846 0.000000e+00 0.007260949 0.000000e+00
# gamma gamma  0.06136383 0.012258482 5.562301e-07 0.015113622 4.903769e-05
# m         m  1.23058007 0.123833333 0.000000e+00 0.248740031 7.526780e-07
# theta theta -0.41448985 0.258506864 1.088464e-01 0.488173680 3.958466e-01
# w2       w2  2.41795779 0.608250880 7.030125e-05 2.840953899 3.947095e-01

#LOG/LOG-DIFF


modelo_financeiro_log_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
  
  gamma = TRUE,
  multi.start = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06191451  0.03229944  0.92051628  0.06096947  1.94215737 -0.15458715  3.75125712 
#  theta.two      w2.two 
# -0.46416878  2.19043302
# BIC: 24329.44
modelo_financeiro_log_gpr_dlog_cambio$par
modelo_financeiro_log_gpr_dlog_cambio$broom.mgarch
modelo_financeiro_log_gpr_dlog_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06191451 0.020327729 2.320503e-03  0.158314917 6.957348e-01
# alpha         alpha  0.03229944 0.006688115 1.369600e-06  0.007305595 9.815781e-06
# beta           beta  0.92051628 0.009544666 0.000000e+00  0.007364329 0.000000e+00
# gamma         gamma  0.06096947 0.012250288 6.458299e-07  0.015543904 8.766971e-05
# m                 m  1.94215737 1.095491127 7.625116e-02  0.987595922 4.923504e-02
# theta         theta -0.15458715 0.236367321 5.131038e-01  0.219467405 4.811998e-01
# w2               w2  3.75125712 1.689603107 2.640518e-02 10.340699130 7.167795e-01
# theta.two theta.two -0.46416878 0.251517279 6.496852e-02  0.472329937 3.257445e-01
# w2.two       w2.two  2.19043302 0.433645339 4.390236e-07  2.203056867 3.200915e-01


#todo novamente usei o multi, liberado fazer tabela, mas atualizar tabela no apendice e excel



#* Setor de energia


modelo_energia <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "log_gpr_global",      
  low.freq = "year_month",   
  K = 12,                    
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05875969  0.05065364  0.88539647  0.07027062  1.85462330 -0.13733609  3.30473089   
modelo_energia$par
modelo_energia$broom.mgarch
modelo_energia$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.05875969 0.020275379 3.754539e-03  0.173081789 7.342400e-01
# alpha alpha  0.05065364 0.008822035 9.372073e-09  0.010482590 1.350541e-06
# beta   beta  0.88539647 0.015572387 0.000000e+00  0.009134439 0.000000e+00
# gamma gamma  0.07027062 0.018533954 1.497683e-04  0.022115436 1.485780e-03
# m         m  1.85462330 1.068565169 8.263101e-02  0.913598529 4.235450e-02
# theta theta -0.13733609 0.228214911 5.473179e-01  0.196419282 4.844279e-01
# w2       w2  3.30473089 2.542766124 1.937176e-01 11.045806915 7.647995e-01


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



modelo_energia_log_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
#0.05688844  0.04932442  0.89044775  0.06943691  1.42623490 -0.04391278  1.93609210 
#  theta.two      w2.two 
# -0.41083240  3.20327648 

modelo_energia_log_gpr_dlog_cambio$par
modelo_energia_log_gpr_dlog_cambio$broom.mgarch
modelo_energia_log_gpr_dlog_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.05688844  0.02123972 7.397504e-03  0.170308978 7.383563e-01
# alpha         alpha  0.04932442  0.01006748 9.613822e-07  0.010336581 1.825587e-06
# beta           beta  0.89044775  0.01680367 0.000000e+00  0.009043532 0.000000e+00
# gamma         gamma  0.06943691  0.02203817 1.628483e-03  0.021468637 1.219235e-03
# m                 m  1.42623490 11.81943006 9.039535e-01  1.016078491 1.604183e-01
# theta         theta -0.04391278  2.56575769 9.863449e-01  0.218992415 8.410724e-01
# w2               w2  1.93609210 78.49639729 9.803224e-01 21.831420336 9.293333e-01
# theta.two theta.two -0.41083240  0.22686037 7.014886e-02  0.452303909 3.637142e-01
# w2.two       w2.two  3.20327648  0.68998240 3.441385e-06  3.578611171 3.707244e-01