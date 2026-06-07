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

##TODO testando agora os modelos 2 e 3 para o setor de consumo

#* SETOR DE CONSUMO COM CAMBIO SOMENTE



# 12 LAGS
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

# 6 LAGS
modelo_consumo_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03229898  0.03076462  0.91178150  0.08023875 -0.45622713 -0.15417696  5.81479079 
# P-VALUE: 2.540549e-01
modelo_consumo_cambio_6$par
modelo_consumo_cambio_6$broom.mgarch

# 3 LAGS
modelo_consumo_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.02975001  0.03112171  0.91393786  0.07993476 -0.63971664 -0.19050374  1.67091411 
# P-VALUE: 1.232933e-01
modelo_consumo_cambio_3$par
modelo_consumo_cambio_3$broom.mgarch

# 1 LAGS
modelo_consumo_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "consumer_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.02966895  0.03083748  0.91011728  0.08292104 -0.08730680 -0.10556294 
# P-VALUE: 7.915561e-01
modelo_consumo_cambio_1$par
modelo_consumo_cambio_1$broom.mgarch

#TODO TESTAR DEPOIS LOG-DIFF DO CAMBIO


#* Para o setor de consumo, vamos testar o GPR + Cambio

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
  
  gamma = TRUE
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























##* ORTOGONALIZAÇÃO VIE OSL (ISOLANDO O EFEITO DO GPR NO CAMBIO)

# 1. Extração da Matriz de Baixa Frequência (Apenas uma linha por mês)
base_mensal_pura <- painel_midas_limpo %>%
  select(year_month, log_gpr_global, log_var_cambio) %>%
  distinct() %>% # Garante que temos exatamente os ~313 meses únicos
  na.omit()

# 2. A Ortogonalização Econométrica Correta (N = 313)
regressao_ortogonal <- lm(log_var_cambio ~ log_gpr_global, data = base_mensal_pura)

# 3. Salvando o resíduo puramente doméstico
base_mensal_pura$cambio_ortogonal <- residuals(regressao_ortogonal)

# 4. O Merge (Devolvendo o resíduo perfeito para os dias úteis correspondentes)
painel_midas_limpo <- painel_midas_limpo %>%
  # Removemos a tentativa anterior falha (se existir) para não duplicar colunas
  select(-any_of("cambio_ortogonal")) %>% 
  left_join(base_mensal_pura %>% select(year_month, cambio_ortogonal), by = "year_month")

# Opcional: Remover qualquer linha que tenha ficado órfã no cruzamento
painel_midas_limpo <- na.omit(painel_midas_limpo)


# Modelo GPR + Log Cambio ortogonalizado com 12 lags
#! GPR DEU POSITIVO E SIGNIFICATIVO (AUMENTOU O THETA), MAS O CAMBIO DEU NEGATIVO E NAO SIGNIFICATIVO
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

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03126534  0.02779945  0.90975440  0.08524289 -2.56650089  0.67372322  1.00009036 
#  theta.two      w2.two 
# -0.13232998 14.37934895 
# P-VALUE THETA: 3.045773e-02
# P-VALUE THETA 2: 1.474329e-01
modelo_consumo_cambio_ort$par
modelo_consumo_cambio_ort$broom.mgarch
