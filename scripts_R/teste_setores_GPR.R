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



#* A Estimação no setor financeiro
#! Negativo e nao significativo = os bancos não são tao afetados pelos choques geopoliticos. Ganha nas duas pontas?

# MODELOS COM LOG GLOBAL

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
# P-value:  4.209724e-01
modelo_financeiro$par
modelo_financeiro$broom.mgarch

# MODELO COM 6 LAGS

modelo_financeiro_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 6,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.06328591  0.03263851  0.91931578  0.05891237  2.52340689 -0.28368202  1.03520035
# P-value:  2.182422e-01
modelo_financeiro_6$par
modelo_financeiro_6$broom.mgarch

# MODELO COM 3 LAGS

modelo_financeiro_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 3,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.06290182  0.03127375  0.92101125  0.06001591  2.19457293 -0.20918941  1.00002981
# P-value:  7.913522e-01
modelo_financeiro_3$par
modelo_financeiro_3$broom.mgarch

#todo MODELOS COM LOG_DIFF

modelo_dlog_financeiro <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "d_log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 12,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06261291 0.03231362 0.92118258 0.05910725 1.22499025 0.07050108 2.28145396 
# P-value:  9.114901e-01
modelo_dlog_financeiro$par
modelo_dlog_financeiro$broom.mgarch

# Modelo com 6 lags

modelo_dlog_financeiro_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "d_log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 6,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06410586 0.03196513 0.92019052 0.06039786 1.21791999 0.18591133 3.42892560 
# P-value:  5.140173e-01
modelo_dlog_financeiro_6$par
modelo_dlog_financeiro_6$broom.mgarch

# Modelo com 3 lags

modelo_dlog_financeiro_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "finance_log_ret",     # O alvo: canal de fuga de capitais
  x = "d_log_gpr_global",      # O choque: risco geopolítico global em log
  low.freq = "year_month",   # A chave de transição MIDAS
  K = 3,                    # Memória do conflito (12 meses)
  gamma = TRUE,              # Controle de assimetria doméstica (obrigatório)
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.06627953 0.03132881 0.92220094 0.06050769 1.24584665 0.15996262 2.06716334  
# P-value:  5.910168e-01
modelo_dlog_financeiro_3$par
modelo_dlog_financeiro_3$broom.mgarch

## * SETOR DE CONSUMO
#! Positivo e significativo

modelo_consumo <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#          mu       alpha        beta       gamma           m       theta 
# 0.03186632  0.02734408  0.90350657  0.08845895 -2.40782682  0.63522007 
#        w2 
# 1.11177142 
# P-VALUE: 2.064101e-02
modelo_consumo$par
modelo_consumo$broom.mgarch

# Modelo com 6 Lags
modelo_consumo_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03114080  0.02848406  0.90637308  0.08567955 -1.08716087  0.35692932  1.00000078
# P-VALUE: 3.952544e-02
modelo_consumo_6$par
modelo_consumo_6$broom.mgarch
modelo_consumo_6$optim$convergence

# Modelo com 3 lags

modelo_consumo_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03066220 0.03142921 0.90520391 0.08464017 0.07415321 0.11700418 2.02087180 
# P-VALUE: 5.693874e-01
modelo_consumo_3$par
modelo_consumo_3$broom.mgarch

#todo Modelo com log-diff

modelo_dlog_consumo <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03343354  0.03116619  0.90373081  0.08309206  0.54731882 -0.23557492  3.24742266 
# P-VALUE: 8.137457e-01
modelo_dlog_consumo$par
modelo_dlog_consumo$broom.mgarch

# Modelo com 6 lags

modelo_dlog_consumo_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03277641  0.03102782  0.90551902  0.08318741  0.57756656 -0.31850716  1.91601244 
# P-VALUE: 5.832869e-01
modelo_dlog_consumo_6$par
modelo_dlog_consumo_6$broom.mgarch

# Modelo com 3 lags

modelo_dlog_consumo_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_gpr_global",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03295618  0.03317643  0.90505208  0.08178595  0.60983838 -0.07512927  4.57992844 
# P-VALUE: 7.043475e-01
modelo_dlog_consumo_3$par
modelo_dlog_consumo_3$broom.mgarch

#* SETOR DE BENS BASICOS (ONDE FICAM AS COMMODITIES)
#! NEGATIVO E SIGNIFICATIVO
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

# MODELO COM 6 LAGS

 modelo_bens_basicos_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 6,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06418877  0.04723098  0.90573282  0.04358374  4.45416249 -0.68848824  1.98479079 
# P-VALUE: 1.077834e-03
modelo_bens_basicos_6$par
modelo_bens_basicos_6$broom.mgarch
modelo_bens_basicos_6$optim$convergence

# MODELO COM 3 LAGS

 modelo_bens_basicos_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 3,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06188493  0.04581825  0.90735964  0.04454393  3.82935984 -0.55155774  1.57728700 
# P-VALUE: 7.311827e-03
modelo_bens_basicos_3$par
modelo_bens_basicos_3$broom.mgarch

#todo Modelos com log-diff

 modelo_dlog_bens_basicos <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "d_log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 12,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06155721  0.04662239  0.91367579  0.04122071  1.29891360 -1.19003286  1.00030243 
# P-VALUE: 3.548689e-01
modelo_dlog_bens_basicos$par
modelo_dlog_bens_basicos$broom.mgarch

# Modelo com 6 lags

 modelo_dlog_bens_basicos_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "d_log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 6,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06187998  0.04493785  0.91670161  0.04042065  1.30450206 -0.53814394  1.06622920 
# P-VALUE: 3.726492e-01
modelo_dlog_bens_basicos_6$par
modelo_dlog_bens_basicos_6$broom.mgarch

# modelo com 3 lags

 modelo_dlog_bens_basicos_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "basic_products_log_ret", # O alvo: O escudo das commodities
  x = "d_log_gpr_global",         # O choque exógeno
  low.freq = "year_month",   
  K = 3,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.06219779  0.04347224  0.92074811  0.03901867  1.31947898 -0.08290045  7.17706732 
# P-VALUE: 5.422342e-01
modelo_dlog_bens_basicos_3$par
modelo_dlog_bens_basicos_3$broom.mgarch


#* SETOR DE ENERGIA
#! Não significativo, o que pode significar que o setor de energia tem fatores a favor e contra
modelo_energia <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "log_gpr_global",      
  low.freq = "year_month",   
  K = 12,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.05861465 0.04996186 0.88669265 0.07031000 0.88964234 0.06983208 1.00008836  
# P-VALUE: 9.118329e-01
modelo_energia$par
modelo_energia$broom.mgarch

# MOdelo com 6 Lags

modelo_energia_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "log_gpr_global",      
  low.freq = "year_month",   
  K = 6,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05949141  0.04769138  0.88601001  0.07402279  2.31508351 -0.23883945  2.46287511  
# P-VALUE: 2.368271e-01
modelo_energia_6$par
modelo_energia_6$broom.mgarch

# Modelo com 3 lags

modelo_energia_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "log_gpr_global",      
  low.freq = "year_month",   
  K = 3,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05960231  0.04810186  0.88722062  0.07141179  1.72024811 -0.10905171  1.03506323  
# P-VALUE: 3.854352e-01
modelo_energia_3$par
modelo_energia_3$broom.mgarch

#todo Modelo com log-diff

modelo_dlog_energia <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "d_log_gpr_global",      
  low.freq = "year_month",   
  K = 12,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05846265  0.05033399  0.88569367  0.07101698  1.22062455 -0.04881422  3.02896849   
# P-VALUE: 5.121965e-01
modelo_dlog_energia$par
modelo_dlog_energia$broom.mgarch

# Modelo com 6 lags

modelo_dlog_energia_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "d_log_gpr_global",      
  low.freq = "year_month",   
  K = 6,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05750879  0.04860053  0.88640430  0.07277381  1.22542802 -0.01227031  3.51887328 
# P-VALUE: 8.469080e-01
modelo_dlog_energia_6$par
modelo_dlog_energia_6$broom.mgarch

# Modelo com 3 lags

modelo_dlog_energia_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",      # O último pilar: O escudo do petróleo
  x = "d_log_gpr_global",      
  low.freq = "year_month",   
  K = 3,                    
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05970371  0.04778515  0.88895014  0.07118667  1.22534224 -0.09540620 25.70614641 
# P-VALUE: 5.259435e-01
modelo_dlog_energia_3$par
modelo_dlog_energia_3$broom.mgarch


##TODO testando agora os modelos 2 e 3 para os diferentes setores

#* Para o setor de consumo, vamos testar o GPR + Cambio
#! AMBOS DERAM NAO SIGNIFICATIVOS, ENTÃO PRECISAMOS ORTOGONALIZAR

modelo_consumo_cambio <- fit_mfgarch(
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
  
  gamma = TRUE
)


# Resultado
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03099629 0.02855877 0.89997327 0.08920559 0.51046077 0.24360086 1.56366919 
# theta.two     w2.two 
# 0.16279982 1.00002089 
# P-VALUE THETA: 7.482024e-01
# P-VALUE THETA 2: 9.007169e-01
modelo_consumo_cambio$par
modelo_consumo_cambio$broom.mgarch

##* ORTOGONALIZAÇÃO VIE OSL (ISOLANDO O EFEITO DO GPR NO CAMBIO)

regressao_ortogonal <- lm(log_var_cambio ~ log_gpr_global, data = painel_midas_limpo)

# Extraindo o Resíduo (A Volatilidade Cambial Puramente Doméstica)
painel_midas_limpo$cambio_ortogonal <- residuals(regressao_ortogonal)


modelo_consumo_cambio_ort <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "cambio_ortogonal",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)
