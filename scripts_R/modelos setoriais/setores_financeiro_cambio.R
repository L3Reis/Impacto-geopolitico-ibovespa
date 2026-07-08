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


#* SETOR FINANCEIRO COM CAMBIO SOMENTE

# MODELO COM LOG CAMBIO
# 12 LAGS
modelo_financeiro_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.06302550  0.03165248  0.92541715  0.05845362  0.42486873 -0.12066945  9.29270041 
# P-VALUE: 3.110956e-01 
modelo_financeiro_cambio$par
modelo_financeiro_cambio$broom.mgarch

# 6 LAGS
modelo_financeiro_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06412826  0.03113862  0.92505390  0.05944518  0.36127387 -0.12839052  3.61311798 
# P-VALUE: 2.806522e-01
modelo_financeiro_cambio_6$par
modelo_financeiro_cambio_6$broom.mgarch

# 3 LAGS
modelo_financeiro_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06266143  0.02966585  0.92848927  0.05826887  0.31003982 -0.13930785  1.00001513 
# P-VALUE: 1.380920e-01 
modelo_financeiro_cambio_3$par
modelo_financeiro_cambio_3$broom.mgarch

# 1 LAG
modelo_financeiro_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06141700  0.03027547  0.92447447  0.06081453  0.81196424 -0.06525176 
# P-VALUE: 1.326929e-01 
modelo_financeiro_cambio_1$par
modelo_financeiro_cambio_1$broom.mgarch

# MODELO COM LOG-DIFF CAMBIO

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


# 6 LAGS
modelo_financeiro_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06170530  0.03149707  0.92007093  0.06236205  1.22456010 -0.20752895  1.95028230 
# P-VALUE: 1.602704e-01 
modelo_financeiro_dlog_cambio_6$par
modelo_financeiro_dlog_cambio_6$broom.mgarch

# 3 LAGS
modelo_financeiro_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06191883  0.03126634  0.92122859  0.06076444  1.23273105 -0.04272896  4.78801083 
# P-VALUE: 2.358670e-01
modelo_financeiro_dlog_cambio_3$par
modelo_financeiro_dlog_cambio_3$broom.mgarch

# 1 LAG
modelo_financeiro_dlog_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.06166046  0.03055216  0.92269827  0.06067219  1.24586957 -0.02835777 
# P-VALUE: 3.370918e-01
modelo_financeiro_dlog_cambio_1$par
modelo_financeiro_dlog_cambio_1$broom.mgarch




#todo TESTANDO GPR + CAMBIO PARA O SETOR FINANCEIRO

#* LOG-LOG

modelo_financeiro_log_gpr_log_cambio <- fit_mfgarch(
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
  
  gamma = TRUE
)


# Resultado
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06363328  0.03201620  0.92493075  0.05797684  1.07238641 -0.14688258  4.49759649 -0.12489352  8.00275691 
# P-VALUE THETA: 5.473273e-01 
# P-VALUE THETA 2: 2.486774e-01
# BIC: 24331.27
modelo_financeiro_log_gpr_log_cambio$par
modelo_financeiro_log_gpr_log_cambio$broom.mgarch
modelo_financeiro_log_gpr_log_cambio$bic


# 6 LAGS
modelo_financeiro_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06316966  0.03172962  0.92340096  0.05916470  1.37668764 -0.18924497  1.45164964 -0.10659391  4.23096860
# P-VALUE THETA: 5.368312e-01 
# P-VALUE THETA 2: 5.096258e-01
# BIC: 24841.36
modelo_financeiro_log_gpr_log_cambio_6$par
modelo_financeiro_log_gpr_log_cambio_6$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_6$bic

# 3 LAGS

modelo_financeiro_log_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06411049  0.03000856  0.92842143  0.05723500  0.70602122 -0.12063529  1.00010414 -0.15991453  1.01566303 
# P-VALUE THETA: 5.536193e-01 
# P-VALUE THETA 2: 9.425350e-02
# BIC: 25115.43
modelo_financeiro_log_gpr_log_cambio_3$par
modelo_financeiro_log_gpr_log_cambio_3$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_3$bic

# 3 LAGS e 1 LAG

modelo_financeiro_log_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06188602  0.03065448  0.92373909  0.06003114  1.25682951 -0.07907391  1.12791122 -0.05195664 
# P-VALUE THETA: 3.417335e-01
# P-VALUE THETA 2: 2.329354e-01
# BIC: 25108.41
modelo_financeiro_log_gpr_log_cambio_3_1$par
modelo_financeiro_log_gpr_log_cambio_3_1$broom.mgarch
modelo_financeiro_log_gpr_log_cambio_3_1$bic


#* LOG/LOG-DIFF


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
  
  gamma = TRUE
)


# Resultado
#        mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06203758  0.03244692  0.92070366  0.06054675  1.94206228 -0.15463023  3.75124052 -0.46424489  2.19039653 
# P-VALUE THETA: 5.142672e-01
# P-VALUE THETA 2: 6.566468e-02
# BIC: 24329.44
modelo_financeiro_log_gpr_dlog_cambio$par
modelo_financeiro_log_gpr_dlog_cambio$broom.mgarch
modelo_financeiro_log_gpr_dlog_cambio$bic

# 6 LAGS

modelo_financeiro_log_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06132631  0.03108977  0.91911049  0.06231732  1.98827595 -0.16803762  2.50267757 -0.20736818  1.87338647  
# P-VALUE THETA: 8.360342e-01
# P-VALUE THETA 2: 1.636167e-01
# BIC: 24841.1
modelo_financeiro_log_gpr_dlog_cambio_6$par
modelo_financeiro_log_gpr_dlog_cambio_6$broom.mgarch
modelo_financeiro_log_gpr_dlog_cambio_6$bic

# 3 LAGS

modelo_financeiro_log_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06074019  0.03093561  0.92193223  0.06114426  1.48177421 -0.05214917  4.46859284 -0.03715406  6.51498831  
# P-VALUE THETA: 6.453616e-01
# P-VALUE THETA 2: 2.533800e-01
# BIC: 25119.4
modelo_financeiro_log_gpr_dlog_cambio_3$par
modelo_financeiro_log_gpr_dlog_cambio_3$broom.mgarch
modelo_financeiro_log_gpr_dlog_cambio_3$bic


# 3 LAGS e 1 LAG

modelo_financeiro_log_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.06312098  0.03161649  0.91981130  0.06114797  2.13362770 -0.19561003  1.00010869 -0.03404258   
# P-VALUE THETA: 6.393017e-01
# P-VALUE THETA 2: 2.517720e-01
# BIC: 25108.7
modelo_financeiro_log_gpr_dlog_cambio_3_1$par
modelo_financeiro_log_gpr_dlog_cambio_3_1$broom.mgarch
modelo_financeiro_log_gpr_dlog_cambio_3_1$bic

#* LOG-DIFF/LOG

modelo_financeiro_dlog_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06301490  0.03142000  0.92614581  0.05818969  0.32020473  0.14927716  6.03853657 -0.13595747  7.85816793 
# P-VALUE THETA: 6.715549e-01
# P-VALUE THETA 2: 2.372679e-01
# BIC: 24331.58
modelo_financeiro_dlog_gpr_log_cambio$par
modelo_financeiro_dlog_gpr_log_cambio$broom.mgarch
modelo_financeiro_dlog_gpr_log_cambio$bic

# 6 Lags

modelo_financeiro_dlog_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06326292  0.03100954  0.92540910  0.05935818  0.31876188  0.11629907  5.31211231 -0.13504047  3.25299540 
# P-VALUE THETA: 6.179527e-01
# P-VALUE THETA 2: 2.502151e-01
# BIC: 24842.34
modelo_financeiro_dlog_gpr_log_cambio_6$par
modelo_financeiro_dlog_gpr_log_cambio_6$broom.mgarch
modelo_financeiro_dlog_gpr_log_cambio_6$bic


# 3 Lags

modelo_financeiro_dlog_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06286646  0.02973385  0.92823559  0.05819041  0.35128876  0.07775981  3.89800086 -0.13250874  1.00000672 
# P-VALUE THETA: 5.271209e-01
# P-VALUE THETA 2: 1.911355e-01
# BIC: 25116.43
modelo_financeiro_dlog_gpr_log_cambio_3$par
modelo_financeiro_dlog_gpr_log_cambio_3$broom.mgarch
modelo_financeiro_dlog_gpr_log_cambio_3$bic

# 3 Lags 3 1 Lag

modelo_financeiro_dlog_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
# 0.06250793  0.03094476  0.92340083  0.06081852  0.80418440  0.05227601  4.62410601 -0.06506936 
# P-VALUE THETA: 5.559456e-01
# P-VALUE THETA 2: 1.402804e-01
# BIC: 25109.09

modelo_financeiro_dlog_gpr_log_cambio_3_1$par
modelo_financeiro_dlog_gpr_log_cambio_3_1$broom.mgarch
modelo_financeiro_dlog_gpr_log_cambio_3_1$bic

#* LOG-DIFF/LOG-DIFF


modelo_financeiro_dlog_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06270290  0.03287262  0.92082418  0.06027420  1.23064944  0.17696647  5.79292057 -0.43614833  2.31456964  
# P-VALUE THETA: 6.143724e-01 
# P-VALUE THETA 2: 9.802569e-02
# BIC: 24330.03
modelo_financeiro_dlog_gpr_dlog_cambio$par
modelo_financeiro_dlog_gpr_dlog_cambio$broom.mgarch
modelo_financeiro_dlog_gpr_dlog_cambio$bic


# 6 lags
modelo_financeiro_dlog_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06144359  0.03160984  0.91978516  0.06260632  1.22652297  0.11544701  5.42649860 -0.20085177  1.94943420 
# P-VALUE THETA: 6.309910e-01
# P-VALUE THETA 2: 1.783904e-01
# BIC: 24842.24
modelo_financeiro_dlog_gpr_dlog_cambio_6$par
modelo_financeiro_dlog_gpr_dlog_cambio_6$broom.mgarch
modelo_financeiro_dlog_gpr_dlog_cambio_6$bic

# 3 Lags

modelo_financeiro_dlog_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two      w2.two 
# 0.06275684  0.03153504  0.92108746  0.06076912  1.23530220  0.19768314  1.81660275 -0.03863838  7.01274610 
# P-VALUE THETA: 5.003122e-01
# P-VALUE THETA 2: 2.283301e-01
# BIC: 25118.81
modelo_financeiro_dlog_gpr_dlog_cambio_3$par
modelo_financeiro_dlog_gpr_dlog_cambio_3$broom.mgarch
modelo_financeiro_dlog_gpr_dlog_cambio_3$bic


# 3 Lag e 1 lag

modelo_financeiro_dlog_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "finance_log_ret",
  
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
# 0.06214884  0.03137071  0.92104621  0.06107178  1.23553877  0.14690513  2.57141089 -0.03543175  
# P-VALUE THETA: 0.9988782
# P-VALUE THETA 2: 0.9577326
# BIC: 25110
modelo_financeiro_dlog_gpr_dlog_cambio_3_1$par
modelo_financeiro_dlog_gpr_dlog_cambio_3_1$broom.mgarch
modelo_financeiro_dlog_gpr_dlog_cambio_3_1$bic
