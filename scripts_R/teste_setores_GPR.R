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
#         mu       alpha        beta       gamma           m       theta 
# 0.06337388  0.04841047  0.90506880  0.04317313  3.77950566 -0.54322738 
#         w2 
# 4.27747777 
# P-VALUE: 1.438017e-02
modelo_bens_basicos_3$par
modelo_bens_basicos_3$broom.mgarch


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
#         mu       alpha        beta       gamma           m       theta 
# 0.06337388  0.04841047  0.90506880  0.04317313  3.77950566 -0.54322738 
#         w2 
# 4.27747777 
# P-VALUE: 1.438017e-02
modelo_energia$par
modelo_energia$broom.mgarch


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
