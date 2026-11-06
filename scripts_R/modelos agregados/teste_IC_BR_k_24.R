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
    gpr_global = as.numeric(gpr_global),
    d_gpr_global = as.numeric(d_gpr_global),
    log_gpr_global = as.numeric(log_gpr_global),
    d_log_gpr_global = as.numeric(d_log_gpr_global),
    ret_ibov_100 = ret_ibov * 100,
    ic_br = as.numeric(ic_br),
    d_ic_br = as.numeric(d_ic_br),
    log_ic_br = as.numeric(log_ic_br),
    d_log_ic_br = as.numeric(d_log_ic_br)*100 # Multipliquei por 100 pois os coeficientes estavam desproporcionais
  ) %>% select(
    date,
    year_month,
    gpr_global,
    d_gpr_global,
    log_gpr_global,
    d_log_gpr_global,
    ret_ibov_100,
    ic_br,
    d_ic_br,
    log_ic_br,
    d_log_ic_br
  ) |> na.omit() |>filter(date >= '2000-02-02') |> 
  na.omit()


#* Modelo Dlog IC-BR com 24 defasagens

modelo_dlog_ic_24_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_ic_br",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_ic_24_asi$par
modelo_log_ic_24_asi$broom.mgarch
modelo_log_ic_24_asi$optim$convergence
modelo_log_ic_24_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.04541925  0.02527096  0.91691397  0.07443381  0.80955175 -0.03083542  

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.04541925 0.017391603 9.012964e-03  0.123545683 7.131482e-01
# alpha alpha  0.02527096 0.006422497 8.328118e-05  0.007081076 3.586168e-04
# beta   beta  0.91691397 0.009893378 0.000000e+00  0.006619761 0.000000e+00
# gamma gamma  0.07443381 0.013639709 4.838629e-08  0.014769549 4.663085e-07
# m         m  0.80955175 0.101306890 1.332268e-15  0.221165716 2.518362e-04
# theta theta -0.03083542 0.014893224 3.841181e-02  0.026319361 2.413629e-01
# w2       w2 11.29216951 7.171221103 1.153371e-01 11.984312185 3.460667e-01

#BIC: 21277.14

#! MODELO CONVERGE


# MODELO COM K = 24

modelo_log_gpr_dlog_ic_24 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 24,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)



modelo_log_gpr_dlog_ic_24$par
modelo_log_gpr_dlog_ic_24$broom.mgarch
modelo_log_gpr_dlog_ic_24$optim$convergence
modelo_log_gpr_dlog_ic_24$optim$final
modelo_log_gpr_dlog_ic_24$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.03985655  0.02595797  0.91456264  0.07795871  2.66779719 -0.38583006 10.18651364 -0.07244921  4.78076942 

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03985655 0.017385481 2.187568e-02  0.121964795 7.438287e-01
# alpha         alpha  0.02595797 0.006527354 6.984942e-05  0.007115237 2.640606e-04
# beta           beta  0.91456264 0.010029718 0.000000e+00  0.006790567 0.000000e+00
# gamma         gamma  0.07795871 0.014047934 2.865224e-08  0.015147617 2.652511e-07
# m                 m  2.66779719 1.168989224 2.248107e-02  1.059435570 1.179795e-02
# theta         theta -0.38583006 0.246843712 1.180399e-01  0.213446155 7.066528e-02
# w2               w2 10.18651364 3.287461538 1.944403e-03 11.905057316 3.921937e-01
# theta.two theta.two -0.07244921 0.047107452 1.240592e-01  0.044693948 1.050161e-01
# w2.two       w2.two  4.78076942 1.704180829 5.026694e-03  3.482788006 1.698504e-01

#BIC: 21287.95

#! CONVERGE

#* Testando para os bens básicos

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



modelo_bens_basicos_ic_24 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_ic_br",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 24,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_bens_basicos_ic_24$par
modelo_bens_basicos_ic_24$broom.mgarch
modelo_bens_basicos_ic_24$optim$convergence
modelo_bens_basicos_ic_24$variance.ratio
modelo_bens_basicos_ic_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.08704827  0.03902766  0.95008307  0.02177851 -0.40085041 -5.45430854  3.04368243 

#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.08704827 0.028194004 2.018651e-03  0.136997641 5.251677e-01
# alpha alpha  0.03902766 0.009010770 1.482829e-05  0.006972728 2.178573e-08
# beta   beta  0.95008307 0.007914937 0.000000e+00  0.004151542 0.000000e+00
# gamma gamma  0.02177851 0.008303435 8.720247e-03  0.011554022 5.943946e-02
# m         m -0.40085041 1.513251482 7.910915e-01  0.499775210 4.225174e-01
# theta theta -5.45430854 6.355042955 3.907464e-01 14.416935691 7.051881e-01
# w2       w2  3.04368243 0.889501889 6.221195e-04  8.055480971 7.055495e-01

# BIC: 23751.7

#! Modelo converge

modelo_bens_basicos_log_gpr_dlog_ic_24 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: IC-BR
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 24,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)

modelo_bens_basicos_log_gpr_dlog_ic_24$par
modelo_bens_basicos_log_gpr_dlog_ic_24$broom.mgarch
modelo_bens_basicos_log_gpr_dlog_ic_24$optim$convergence
modelo_bens_basicos_log_gpr_dlog_ic_24$variance.ratio
modelo_bens_basicos_log_gpr_dlog_ic_24$bic

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06562523  0.04906669  0.90512837  0.04333223  6.01355699 -1.01690991  8.43670469 -2.92907795  9.21154202 


#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.06562523 0.022616721 3.712332e-03  0.197368031 7.395104e-01
# alpha         alpha  0.04906669 0.009812586 5.721654e-07  0.010712094 4.638779e-06
# beta           beta  0.90512837 0.018873567 0.000000e+00  0.009494441 0.000000e+00
# gamma         gamma  0.04333223 0.016099393 7.112208e-03  0.021525705 4.410982e-02
# m                 m  6.01355699 1.433589112 2.731651e-05  1.194516814 4.795960e-07
# theta         theta -1.01690991 0.298905329 6.686682e-04  0.257000301 7.594673e-05
# w2               w2  8.43670469 2.035426619 3.399147e-05  5.163283122 1.022629e-01
# theta.two theta.two -2.92907795 2.891828289 3.111170e-01  3.674734346 4.254013e-01
# w2.two       w2.two  9.21154202 3.565880824 9.787593e-03 14.242084523 5.177722e-01

# BIC: 23677.57
#! modelo converge