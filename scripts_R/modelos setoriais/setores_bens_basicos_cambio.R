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


#* SETOR DE BENS BASICOS COM CAMBIO SOMENTE

# MODELO COM LOG CAMBIO
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

# 6 LAGS
modelo_basicos_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
#0.06406038  0.04266024  0.92379217  0.03947380  0.53684331 -0.11530169 
#         w2 
# 20.73852041
# P-VALUE: 1.948950e-02
modelo_basicos_cambio_6$par
modelo_basicos_cambio_6$broom.mgarch

# 3 LAGS
modelo_basicos_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06394678  0.04133045  0.92612263  0.03939166  0.50952608 -0.12186009 
#         w2 
# 12.84642234
# P-VALUE: 1.024505e-02
modelo_basicos_cambio_3$par
modelo_basicos_cambio_3$broom.mgarch

# 1 LAGS
modelo_basicos_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06291297  0.04066646  0.92697291  0.03994482  0.54022224 -0.12007493 
# P-VALUE: 9.288950e-03
modelo_basicos_cambio_1$par
modelo_basicos_cambio_1$broom.mgarch

# MODELO COM LOG-DIFF CAMBIO

# 12 LAGS
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

# 6 LAGS
modelo_basicos_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06407617  0.04433756  0.91796458  0.04190534  1.30877315 -0.37334182 
#         w2 
# 2.36663404 
# P-VALUE: 2.433820e-01
modelo_basicos_dlog_cambio_6$par
modelo_basicos_dlog_cambio_6$broom.mgarch

# 3 LAGS
modelo_basicos_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06344938  0.04372800  0.91906598  0.04158229  1.32035349 -0.26446845 
#         w2 
# 1.50268164 
# P-VALUE: 2.681828e-03
modelo_basicos_dlog_cambio_3$par
modelo_basicos_dlog_cambio_3$broom.mgarch

# 1 LAGS
modelo_basicos_dlog_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06159035  0.04260416  0.92090456  0.04140795  1.33630316 -0.06367541 4 
# P-VALUE: 6.949754e-02
modelo_basicos_dlog_cambio_1$par
modelo_basicos_dlog_cambio_1$broom.mgarch

#todo Para o setor de bens básicos, vamos testar o GPR + Cambio

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

# 6 lags

modelo_basicos_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06463696  0.04482556  0.91343318  0.04239626  3.78681445 -0.66618446  1.84451604 
#  theta.two      w2.two 
# -0.08437887 15.76965653 
# P-VALUE THETA: 3.877673e-03
# P-VALUE THETA 2: 1.435372e-01
modelo_basicos_log_gpr_log_cambio_6$par
modelo_basicos_log_gpr_log_cambio_6$broom.mgarch
modelo_basicos_log_gpr_log_cambio_6$optim$convergence

# 3 lags

modelo_basicos_log_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06346997  0.04316296  0.91662613  0.04279520  2.84816190 -0.47934884  1.81794163 
# theta.two      w2.two 
# -0.09741584  8.35962347 
# P-VALUE THETA: 5.552814e-02 
# P-VALUE THETA 2: 1.064020e-01
modelo_basicos_log_gpr_log_cambio_3$par
modelo_basicos_log_gpr_log_cambio_3$broom.mgarch

# 3 lags GPR e 1 lag cambio

modelo_basicos_log_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06346549  0.04323711  0.91626270  0.04305894  2.90091210 -0.48832278  1.89347474 
#  theta.two 
# -0.09568937
# P-VALUE THETA: 5.497270e-02 
# P-VALUE THETA 2: 8.653396e-02
modelo_basicos_log_gpr_log_cambio_3_1$par
modelo_basicos_log_gpr_log_cambio_3_1$broom.mgarch

#* LOG/LOG-DIFF

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
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06490621  0.04724327  0.90685512  0.04450965  3.76513780 -0.54050744  4.67480891 
#  theta.two      w2.two 
# -0.36084097  4.54794508 
# P-VALUE THETA: 2.079305e-02
# P-VALUE THETA 2: 8.852193e-02
modelo_basicos_log_gpr_dlog_cambio$par
modelo_basicos_log_gpr_dlog_cambio$broom.mgarch


# 6 lags

modelo_basicos_log_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06463525  0.04582988  0.90772095  0.04553326  4.40630600 -0.67819805  1.96702908 
#  theta.two      w2.two 
# -0.39749935  2.20992540 
# P-VALUE THETA: 2.137370e-03
# P-VALUE THETA 2: 2.057145e-01
modelo_basicos_log_gpr_dlog_cambio_6$par
modelo_basicos_log_gpr_dlog_cambio_6$broom.mgarch

# 3 lags

modelo_basicos_log_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06447515  0.04527583  0.90862552  0.04617593  3.49943727 -0.47919314 
#         w2   theta.two      w2.two 
# 2.24952917 -0.27678276  1.41109843 
# P-VALUE THETA: 3.011873e-02
# P-VALUE THETA 2: 2.117717e-03
modelo_basicos_log_gpr_dlog_cambio_3$par
modelo_basicos_log_gpr_dlog_cambio_3$broom.mgarch

# 3 lags GPR e 1 lag cambio

modelo_basicos_log_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06324756  0.04547727  0.90762610  0.04545867  3.75291368 -0.53502724 
#         w2   theta.two 
# 1.39954972 -0.06283267 
# P-VALUE THETA: 8.656796e-03
# P-VALUE THETA 2: 7.218758e-02
modelo_basicos_log_gpr_dlog_cambio_3_1$par
modelo_basicos_log_gpr_dlog_cambio_3_1$broom.mgarch


#* LOG-DIFF/LOG

modelo_basicos_dlog_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
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
#          mu        alpha         beta        gamma            m        theta           w2 
# 0.062414247  0.046771157  0.913193955  0.041214141  1.351054164 -1.339338231  1.007205040 
#   theta.two       w2.two 
# 0.008314036  4.794102168
# P-VALUE THETA: 3.007848e-01
# P-VALUE THETA 2: 8.779980e-02
modelo_basicos_dlog_gpr_log_cambio$par
modelo_basicos_dlog_gpr_log_cambio$broom.mgarch

# 6 lags

modelo_basicos_dlog_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06380891  0.04262703  0.92361522  0.03960112  0.52324402 -0.43467948  1.25852823 -0.11746363 
#     w2.two 
# 15.15603809 
# P-VALUE THETA: 4.821780e-01 
# P-VALUE THETA 2: 2.922210e-02
modelo_basicos_dlog_gpr_log_cambio_6$par
modelo_basicos_dlog_gpr_log_cambio_6$broom.mgarch

# 3 lags

modelo_basicos_dlog_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06386871  0.04084574  0.92753220  0.03866407  0.46277979 -0.12039261  7.31251853 -0.12883954 
#     w2.two 
# 9.46911423 
# P-VALUE THETA: 3.879247e-01 
# P-VALUE THETA 2: 1.100727e-02
modelo_basicos_dlog_gpr_log_cambio_3$par
modelo_basicos_dlog_gpr_log_cambio_3$broom.mgarch

# 3 lags GPR e 1 lag cambio

modelo_basicos_dlog_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06361630  0.04098900  0.92700695  0.03910737  0.48540231 -0.12054886  6.94393034 -0.12584983 
# P-VALUE THETA: 3.659807e-01
# P-VALUE THETA 2: 7.292073e-03
modelo_basicos_dlog_gpr_log_cambio_3_1$par
modelo_basicos_dlog_gpr_log_cambio_3_1$broom.mgarch


#* LOG-DIFF/LOG-DIFF

modelo_basicos_dlog_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
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
# 0.06326500  0.04440596  0.91652682  0.04332066  1.29737525 -1.51934647  1.05610480 -0.46694455 
#     w2.two 
# 3.67985962 
# P-VALUE THETA: 2.441087e-01
# P-VALUE THETA 2: 5.195768e-02
modelo_basicos_dlog_gpr_dlog_cambio$par
modelo_basicos_dlog_gpr_dlog_cambio$broom.mgarch

# 6 lags

modelo_basicos_dlog_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06418298  0.04436408  0.91732031  0.04220852  1.30572558 -0.59645283  1.24396915 -0.37707079 
#     w2.two 
# 2.35991153
# P-VALUE THETA: 0.3333504182
# P-VALUE THETA 2: 0.2452397186
modelo_basicos_dlog_gpr_dlog_cambio_6$par
modelo_basicos_dlog_gpr_dlog_cambio_6$broom.mgarch

# 3 lags

modelo_basicos_dlog_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 3,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06340881  0.04340049  0.91999189  0.04121999  1.32243594 -0.15548724  3.83092658 -0.27329395 
#     w2.two 
# 1.48791437 
# P-VALUE THETA: 3.439774e-01
# P-VALUE THETA 2: 1.945458e-03
modelo_basicos_dlog_gpr_dlog_cambio_3$par
modelo_basicos_dlog_gpr_dlog_cambio_3$broom.mgarch

# 3 lags para o GPR e 1 lag para cambio

modelo_basicos_dlog_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "basic_products_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06405252  0.04330620  0.92043134  0.03982245  1.31427238  0.41927616  1.00000362 -0.06926226
# P-VALUE THETA: 6.479492e-01
# P-VALUE THETA 2: 6.308154e-02
modelo_basicos_dlog_gpr_dlog_cambio_3_1$par
modelo_basicos_dlog_gpr_dlog_cambio_3_1$broom.mgarch
