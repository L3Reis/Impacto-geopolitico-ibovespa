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

# LOG-LOG

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

# 6 Lags

modelo_consumo_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.03069741  0.02975614  0.90798024  0.08542665 -1.09489618  0.21348506  1.00006002 -0.10302789 
#     w2.two 
# 12.55809630  
# P-VALUE THETA: 1.031629e-01
# P-VALUE THETA 2: 1.033638e-01
modelo_consumo_log_gpr_log_cambio_6$par
modelo_consumo_log_gpr_log_cambio_6$broom.mgarch


# 3 Lags

modelo_consumo_log_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.02997311  0.03334014  0.90768925  0.08283123 -0.53885355  0.07334083  2.89698558 -0.12413813 
#     w2.two 
# 5.87275921  
# P-VALUE THETA: 9.629440e-01
# P-VALUE THETA 2: 3.084553e-01
modelo_consumo_log_gpr_log_cambio_3$par
modelo_consumo_log_gpr_log_cambio_3$broom.mgarch


# 3 Lags GPR e 1 lag cambio

modelo_consumo_log_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.02975337  0.03134770  0.90873858  0.08389483 -0.43356230  0.07694204  3.61367322 -0.10476085  
# P-VALUE THETA: 4.756885e-01
# P-VALUE THETA 2: 2.577261e-02
modelo_consumo_log_gpr_log_cambio_3_1$par
modelo_consumo_log_gpr_log_cambio_3_1$broom.mgarch

#* LOG/LOG-DIFF

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

# 6 lags

modelo_consumo_log_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.02987095  0.02858689  0.90554111  0.08817202 -0.70358466  0.27566369  1.00034196 -0.34188599 
#     w2.two 
# 1.98318745 
# P-VALUE THETA: 5.549981e-02
# P-VALUE THETA 2: 3.086661e-02 
modelo_consumo_log_gpr_dlog_cambio_6$par
modelo_consumo_log_gpr_dlog_cambio_6$broom.mgarch

# 3 lags

modelo_consumo_log_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.03011595  0.03128059  0.90453148  0.08693325  0.19523082  0.09248333  1.56467900 -0.15454745 
#     w2.two 
# 1.42443377  
# P-VALUE THETA: 6.649377e-01
# P-VALUE THETA 2: 9.516128e-02 
modelo_consumo_log_gpr_dlog_cambio_3$par
modelo_consumo_log_gpr_dlog_cambio_3$broom.mgarch


# 3 lags GPR e 1 lag cambio

modelo_consumo_log_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#          mu        alpha         beta        gamma            m        theta           w2 
# 0.030379474  0.031158473  0.905164178  0.085391214 -0.001513647  0.133215007  1.922265566 
#   theta.two 
# -0.056069842  
# P-VALUE THETA: 2.592553e-07
# P-VALUE THETA 2: 4.837666e-02
modelo_consumo_log_gpr_dlog_cambio_3_1$par
modelo_consumo_log_gpr_dlog_cambio_3_1$broom.mgarch

#* LOG-DIFF/LOG

modelo_consumo_dlog_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.03335661  0.03094538  0.91160010  0.07932045 -0.53348212 -0.67070288  2.01475103 -0.16192206 
#     w2.two 
# 11.40055475 
# P-VALUE THETA: 5.178536e-01
# P-VALUE THETA 2: 1.173486e-01 
modelo_consumo_dlog_gpr_log_cambio$par
modelo_consumo_dlog_gpr_log_cambio$broom.mgarch

# 6 Lags

modelo_consumo_dlog_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.03203132  0.03090062  0.91150734  0.08059133 -0.46722166 -0.38965607  1.94522631 -0.15569221 
#     w2.two 
# 6.14658113
# P-VALUE THETA: 4.787132e-01
# P-VALUE THETA 2: 2.820990e-01 
modelo_consumo_dlog_gpr_log_cambio_6$par
modelo_consumo_dlog_gpr_log_cambio_6$broom.mgarch

# 3 lags

modelo_consumo_dlog_gpr_log_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.02985952  0.03221107  0.90946566  0.08268346 -0.16473997 -0.09603169  6.60799516 -0.12004202 
#     w2.two 
# 5.91014544 
# P-VALUE THETA: 4.647212e-01
# P-VALUE THETA 2: 1.421933e-01 
modelo_consumo_dlog_gpr_log_cambio_3$par
modelo_consumo_dlog_gpr_log_cambio_3$broom.mgarch


# 3 lags GPR 1 lag cambio

modelo_consumo_dlog_gpr_log_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.02997434  0.03154897  0.90953089  0.08281820 -0.10074572 -0.08760361  7.34424207 -0.10840407 
# P-VALUE THETA: 5.017160e-01
# P-VALUE THETA 2: 2.030868e-02 
modelo_consumo_dlog_gpr_log_cambio_3_1$par
modelo_consumo_dlog_gpr_log_cambio_3_1$broom.mgarch

#* LOG-DIFF/LOG-DIFF

modelo_consumo_dlog_gpr_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.03229317  0.03006331  0.90685935  0.08362779  0.54773694 -0.18675143  4.91449493 -0.71541596 
#     w2.two 
# 2.02738172 
# P-VALUE THETA: 5.029656e-01
# P-VALUE THETA 2: 6.855464e-02 
modelo_consumo_dlog_gpr_dlog_cambio$par
modelo_consumo_dlog_gpr_dlog_cambio$broom.mgarch

# 6 lags

modelo_consumo_dlog_gpr_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.03125088  0.02984810  0.90665852  0.08511795  0.57716145 -0.14568396  5.43213589 -0.35055673 
#     w2.two 
# 1.92383418 
# P-VALUE THETA: 6.078204e-01
# P-VALUE THETA 2: 2.435983e-02 
modelo_consumo_dlog_gpr_dlog_cambio_6$par
modelo_consumo_dlog_gpr_dlog_cambio_6$broom.mgarch

# 3 lags

modelo_consumo_dlog_gpr_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.03043871  0.03179850  0.90473327  0.08545983  0.61301556 -0.07636468  4.09324842 -0.15685588 
#     w2.two 
# 1.37062767 
# P-VALUE THETA: 7.404211e-01
# P-VALUE THETA 2: 7.471822e-02 
modelo_consumo_dlog_gpr_dlog_cambio_3$par
modelo_consumo_dlog_gpr_dlog_cambio_3$broom.mgarch

# 3 lags GPR 1 lag cambio

modelo_consumo_dlog_gpr_dlog_cambio_3_1 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "consumer_log_ret",
  
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
# 0.03129836  0.03169085  0.90486513  0.08491603  0.60968521 -0.03802741  3.28022456 -0.05218201
# P-VALUE THETA: 7.081964e-01
# P-VALUE THETA 2: 6.609934e-02 
modelo_consumo_dlog_gpr_dlog_cambio_3_1$par
modelo_consumo_dlog_gpr_dlog_cambio_3_1$broom.mgarch


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
