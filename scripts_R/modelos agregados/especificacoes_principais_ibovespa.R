# Conforme a referência de Segnon (2024), foram selecionados como especificações principais os modelos agregados em transformação logarítmica e com K = 12
# Eles são destacados nesse script em especial para melhor localização e para facilitar possíveis diagnósticos futuros

#* Modelos GPR Global assimétrico e simétrico (originais do script teste_garch_midas)

# Modelo assimétrico

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
modelo_gpr_global_log_12$variance.ratio

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03283620 0.017086906 5.464136e-02 0.114529209 7.743375e-01
# alpha alpha  0.02505897 0.006217528 5.568794e-05 0.006599899 1.465337e-04
# beta   beta  0.91598111 0.010247367 0.000000e+00 0.006321467 0.000000e+00
# gamma gamma  0.07930984 0.013974658 1.384768e-08 0.013989438 1.434106e-08
# m         m  1.89965057 1.067745169 7.521951e-02 0.868986858 2.881179e-02
# theta theta -0.21797808 0.230748616 3.448345e-01 0.186336816 2.420787e-01
# w2       w2  4.53067995 2.035179899 2.600202e-02 7.256821965 5.324083e-01

# Modelo simétrico

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
modelo_gpr_global_log_12_sim$optim$convergence
#! Modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.05831367 0.017301827 0.0007506548 0.074269245 0.4323563341
# alpha alpha  0.07154329 0.008339148 0.0000000000 0.006125858 0.0000000000
# beta   beta  0.91231240 0.010057082 0.0000000000 0.007115726 0.0000000000
# m         m  3.70632548 1.439995952 0.0100575502 1.078874162 0.0005917584
# theta theta -0.59585001 0.312833754 0.0568209250 0.234277428 0.0109794497
# w2       w2  2.76715749 1.638190182 0.0911896889 2.199259929 0.2083114376

#* Modelo com variancia do cambio (obtidas do teste_garch_midas_cambio)

#* #* Modelo com multi.start = TRUE

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

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03163418  0.02309645  0.92756525  0.07621090 -1.09573688 -0.29676316  9.49480000 

modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03163418 0.016907716 6.134635e-02 0.087589894 7.179780e-01
# alpha alpha  0.02309645 0.006053031 1.358071e-04 0.006030742 1.282553e-04
# beta   beta  0.92756525 0.010182886 0.000000e+00 0.005612065 0.000000e+00
# gamma gamma  0.07621090 0.012519446 1.147580e-09 0.011097231 6.530998e-12
# m         m -1.09573688 0.969967815 2.586182e-01 0.666237290 1.000390e-01
# theta theta -0.29676316 0.140999272 3.531620e-02 0.083227428 3.629009e-04
# w2       w2  9.49480000 4.755285824 4.585959e-02 3.851196881 1.368545e-02

#* Olhando a convergência
modelo_log_cambio_12_asi$optim$convergence
modelo_log_cambio_12_asi$optim$message
modelo_log_cambio_12_asi$variance.ratio
# 15% de variance ratio

#! modelo converge


# Modelo só com a variância do cambio

modelo_dlog_cambio_12_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03107596  0.02279998  0.91852730  0.08363307  0.89466098 -0.74785165  2.56368336 
# p-value: 4.103421e-02
modelo_dlog_cambio_12_asi$par
modelo_dlog_cambio_12_asi$broom.mgarch
modelo_dlog_cambio_12_asi$optim$convergence

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03107596 0.017008790 6.769176e-02 0.108424323 7.744076e-01
# alpha alpha  0.02279998 0.006085658 1.793115e-04 0.006424512 3.868368e-04
# beta   beta  0.91852730 0.008905437 0.000000e+00 0.006186429 0.000000e+00
# gamma gamma  0.08363307 0.013076068 1.596170e-10 0.013616522 8.147427e-10
# m         m  0.89466098 0.118473525 4.307665e-14 0.256914615 4.970742e-04
# theta theta -0.74785165 0.250360350 2.816367e-03 0.447257572 9.450801e-02
# w2       w2  2.56368336 0.412835758 5.300642e-10 1.550353053 9.820625e-02

# modelo com variancia simetrico

modelo_dlog_cambio_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05823967  0.07277454  0.91420659  0.99466279 -0.58758328  2.57414863 
# p-value: 4.103421e-02
modelo_dlog_cambio_12$par
modelo_dlog_cambio_12$broom.mgarch
modelo_dlog_cambio_12$optim$convergence


#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.05823967 0.017281138 7.513292e-04 0.072575410   0.4222805
# alpha alpha  0.07277454 0.008007846 0.000000e+00 0.006173658   0.0000000
# beta   beta  0.91420659 0.009072069 0.000000e+00 0.006878294   0.0000000
# m         m  0.99466279 0.146185194 1.016631e-11 0.119783294   0.0000000
# theta theta -0.58758328 0.240499221 1.455841e-02 0.493868408   0.2341420
# w2       w2  2.57414863 0.442319142 5.896617e-09 2.160412738   0.2334543



# Modelo GPR + variancia do cambio

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
  
  gamma = TRUE,
  multi.start = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03221238  0.02359453  0.92677448  0.07573665 -0.09042795 -0.21097249  4.42415942 
#  theta.two      w2.two 
# -0.29112046  9.66986420 


modelo_log_gpr_log_cambio$par
modelo_log_gpr_log_cambio$broom.mgarch
modelo_log_gpr_log_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu               mu  0.03221238 0.016935117 5.715739e-02 0.090927424 7.231405e-01
# alpha         alpha  0.02359453 0.006091937 1.074708e-04 0.006122472 1.163186e-04
# beta           beta  0.92677448 0.010333478 0.000000e+00 0.005793704 0.000000e+00
# gamma         gamma  0.07573665 0.012441455 1.147280e-09 0.011427081 3.406786e-11
# m                 m -0.09042795 1.496982007 9.518316e-01 1.229294579 9.413598e-01
# theta         theta -0.21097249 0.239227659 3.778363e-01 0.224409069 3.471536e-01
# w2               w2  4.42415942 2.101517010 3.527201e-02 7.648264486 5.629585e-01
# theta.two theta.two -0.29112046 0.136343155 3.274434e-02 0.082774144 4.363783e-04
# w2.two       w2.two  9.66986420 4.663547439 3.812580e-02 3.991811318 1.541730e-02



# LOG/LOG-DIFF

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


modelo_log_gpr_dlog_cambio$par
modelo_log_gpr_dlog_cambio$broom.mgarch
modelo_log_gpr_dlog_cambio$optim$convergence
#! modelo converge

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03141767 0.017005891 6.468080e-02  0.110510170 7.761831e-01
# alpha         alpha  0.02303381 0.006067821 1.470146e-04  0.006482562 3.805824e-04
# beta           beta  0.91773648 0.009011940 0.000000e+00  0.006352703 0.000000e+00
# gamma         gamma  0.08350633 0.013070327 1.669733e-10  0.013841953 1.611002e-09
# m                 m  1.63400476 1.031427560 1.131444e-01  0.895296323 6.798603e-02
# theta         theta -0.16116395 0.222616862 4.690951e-01  0.190206451 3.968220e-01
# w2               w2  4.68688947 2.130652236 2.782506e-02 10.020500912 6.399776e-01
# theta.two theta.two -0.74077543 0.247905375 2.806829e-03  0.444489283 9.559863e-02
# w2.two       w2.two  2.58105848 0.411197803 3.453307e-10  1.567114600 9.955533e-02