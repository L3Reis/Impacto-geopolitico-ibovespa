# Script para testar as especificações principais dos modelos agregados, com o numero de observações do Ibovespa semelhante ao dos setores
# Teste para verificar se há mudança de sinal significativo

#* CONCLUSAO: Os sinais permanecem majoritariamente iguais, exceto para uma excessão nao significativa. As magnitudes também se mantém.

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
base_mfgarch_completa <- base_teste_modelo_2 |> 
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
  ) |> filter(date >= '2000-02-02') |> 
  na.omit()

#* Modelos GPR Global assimétrico e simétrico (originais do script teste_garch_midas)

# Modelo assimétrico

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

# Resultados 1
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03283620  0.02505897  0.91598111  0.07930984  1.89965057 -0.21797808  4.53067995
# p-value: 3.448345e-01

# resultados nova amostra

#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03494588  0.02598677  0.91431982  0.07864421  1.29605982 -0.09431131  4.37428087 

#! sinal nao troca, mas magnitude muda


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

# nova amostra
#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03494588 0.017245450 4.272558e-02  0.122649674 7.757021e-01
# alpha alpha  0.02598677 0.006427221 5.271854e-05  0.006970291 1.928379e-04
# beta   beta  0.91431982 0.010709313 0.000000e+00  0.006599816 0.000000e+00
# gamma gamma  0.07864421 0.014535211 6.281396e-08  0.014863643 1.216191e-07
# m         m  1.29605982 1.013012379 2.007521e-01  0.878458834 1.401106e-01
# theta theta -0.09431131 0.217564488 6.646612e-01  0.187645495 6.152430e-01
# w2       w2  4.37428087 1.897310524 2.113766e-02 16.670592255 7.930169e-01

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

# nova amostra
#         mu       alpha        beta           m       theta          w2 
# 0.05951099  0.07205892  0.91169954  2.88310050 -0.42013877  2.73812043 


modelo_gpr_global_log_12_sim$par
modelo_gpr_global_log_12_sim$broom.mgarch
modelo_gpr_global_log_12_sim$optim$convergence
#! sinal nao mudou, magnitude mudou pouco, mas deixou de ser significativo

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.05831367 0.017301827 0.0007506548 0.074269245 0.4323563341
# alpha alpha  0.07154329 0.008339148 0.0000000000 0.006125858 0.0000000000
# beta   beta  0.91231240 0.010057082 0.0000000000 0.007115726 0.0000000000
# m         m  3.70632548 1.439995952 0.0100575502 1.078874162 0.0005917584
# theta theta -0.59585001 0.312833754 0.0568209250 0.234277428 0.0109794497
# w2       w2  2.76715749 1.638190182 0.0911896889 2.199259929 0.2083114376

# nova amostra
#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.05951099 0.017474295 0.0006601153 0.075417129 0.430058765
# alpha alpha  0.07205892 0.008563003 0.0000000000 0.006323757 0.000000000
# beta   beta  0.91169954 0.010294212 0.0000000000 0.007318145 0.000000000
# m         m  2.88310050 1.368530037 0.0351424339 1.079175623 0.007549581
# theta theta -0.42013877 0.296011776 0.1558024551 0.234201773 0.072826259
# w2       w2  2.73812043 1.561959848 0.0796014774 3.133350692 0.382192658


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

# nova amostra
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03469713  0.02451824  0.92670322  0.07364268 -1.05595128 -0.28447703  9.01072228

modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch

#! sinal nao muda, magnitude muda muito pouco e significancia tambem quase nao muda

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03163418 0.016907716 6.134635e-02 0.087589894 7.179780e-01
# alpha alpha  0.02309645 0.006053031 1.358071e-04 0.006030742 1.282553e-04
# beta   beta  0.92756525 0.010182886 0.000000e+00 0.005612065 0.000000e+00
# gamma gamma  0.07621090 0.012519446 1.147580e-09 0.011097231 6.530998e-12
# m         m -1.09573688 0.969967815 2.586182e-01 0.666237290 1.000390e-01
# theta theta -0.29676316 0.140999272 3.531620e-02 0.083227428 3.629009e-04
# w2       w2  9.49480000 4.755285824 4.585959e-02 3.851196881 1.368545e-02

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03469713 0.017128085 4.279091e-02 0.097213520 7.211541e-01
# alpha alpha  0.02451824 0.006173974 7.150413e-05 0.006322360 1.053101e-04
# beta   beta  0.92670322 0.010326013 0.000000e+00 0.005878277 0.000000e+00
# gamma gamma  0.07364268 0.012684641 6.411408e-09 0.011815547 4.584846e-10
# m         m -1.05595128 0.923164384 2.526905e-01 0.723490689 1.444212e-01
# theta theta -0.28447703 0.133038706 3.249218e-02 0.088334951 1.279959e-03
# w2       w2  9.01072228 3.522038017 1.051602e-02 3.994014875 2.406711e-02

#nova amostra


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

#nova amostra
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03435447  0.02421941  0.91773187  0.08098877  0.86039753 -0.76431096  2.39767003 

modelo_dlog_cambio_12_asi$par
modelo_dlog_cambio_12_asi$broom.mgarch
modelo_dlog_cambio_12_asi$optim$convergence
#! pouca mudança novamente

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03107596 0.017008790 6.769176e-02 0.108424323 7.744076e-01
# alpha alpha  0.02279998 0.006085658 1.793115e-04 0.006424512 3.868368e-04
# beta   beta  0.91852730 0.008905437 0.000000e+00 0.006186429 0.000000e+00
# gamma gamma  0.08363307 0.013076068 1.596170e-10 0.013616522 8.147427e-10
# m         m  0.89466098 0.118473525 4.307665e-14 0.256914615 4.970742e-04
# theta theta -0.74785165 0.250360350 2.816367e-03 0.447257572 9.450801e-02
# w2       w2  2.56368336 0.412835758 5.300642e-10 1.550353053 9.820625e-02

#nova amostra
#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03435447 0.017181937 4.555941e-02 0.112887468 7.608804e-01
# alpha alpha  0.02421941 0.006297648 1.201663e-04 0.006758326 3.388388e-04
# beta   beta  0.91773187 0.009192696 0.000000e+00 0.006399986 0.000000e+00
# gamma gamma  0.08098877 0.013345019 1.288381e-09 0.014000775 7.268837e-09
# m         m  0.86039753 0.119248534 5.386802e-13 0.253065694 6.741101e-04
# theta theta -0.76431096 0.262449671 3.588707e-03 0.466183917 1.011081e-01
# w2       w2  2.39767003 0.380221140 2.863607e-10 1.461661983 1.009278e-01

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

#nova amostra
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.03539178  0.02402314  0.91723983  0.08148778  0.74823386  0.02274862  3.75391710 
#  theta.two      w2.two 
# -0.70584889  2.53416745
modelo_log_gpr_dlog_cambio$par
modelo_log_gpr_dlog_cambio$broom.mgarch
modelo_log_gpr_dlog_cambio$optim$convergence
#! mudou o sinal do gpr, mas ele permanece nao significativo. para o cambio, pouca mudança

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

#nova amostra

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03539178 0.017172260 3.930441e-02  0.114369633 7.569786e-01
# alpha         alpha  0.02402314 0.006258484 1.237923e-04  0.006806889 4.167560e-04
# beta           beta  0.91723983 0.009359877 0.000000e+00  0.006538419 0.000000e+00
# gamma         gamma  0.08148778 0.013465742 1.435135e-09  0.014168652 8.857397e-09
# m                 m  0.74823386 0.586659907 2.021630e-01  0.927796773 4.199759e-01
# theta         theta  0.02274862 0.124051622 8.544997e-01  0.196968955 9.080540e-01
# w2               w2  3.75391710 4.668724882 4.213645e-01 61.933189371 9.516679e-01
# theta.two theta.two -0.70584889 0.254062433 5.465259e-03  0.467586170 1.311560e-01
# w2.two       w2.two  2.53416745 0.425110121 2.503887e-09  1.683709463 1.322959e-01

