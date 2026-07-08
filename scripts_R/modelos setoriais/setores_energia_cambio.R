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


#* SETOR DE ENERGIA COM CAMBIO SOMENTE

# MODELO COM LOG CAMBIO
# 12 LAGS
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

# 6 LAGS
modelo_energia_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05713675  0.04829763  0.89303130  0.07000900  0.64431257 -0.08818904 14.09504177 
# P-VALUE: 3.105306e-01 
modelo_energia_cambio_6$par
modelo_energia_cambio_6$broom.mgarch

# 3 LAGS
modelo_energia_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05959688  0.04711173  0.89850988  0.06679196  0.31072759 -0.13703636  4.37643006 
# P-VALUE: 2.356904e-01 
modelo_energia_cambio_3$par
modelo_energia_cambio_3$broom.mgarch

# 1 LAG
modelo_energia_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.05714407  0.04711900  0.89652655  0.06877544  0.59454429 -0.09831997 
# P-VALUE: 1.080793e-01 
modelo_energia_cambio_1$par
modelo_energia_cambio_1$broom.mgarch

# MODELO COM LOG-DIFF CAMBIO

modelo_energia_dlog_cambio <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 12,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05750827  0.04920757  0.89017958  0.07033446  1.22855264 -0.38693524  3.39634795  
# P-VALUE: 8.904292e-02
modelo_energia_dlog_cambio$par
modelo_energia_dlog_cambio$broom.mgarch

# 6 LAGS

modelo_energia_dlog_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 6,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05625899  0.04725753  0.89001845  0.07305081  1.22914568 -0.33177219  2.23075928 
# P-VALUE: 4.056466e-02
modelo_energia_dlog_cambio_6$par
modelo_energia_dlog_cambio_6$broom.mgarch

# 3 LAGS

modelo_energia_dlog_cambio_3 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 3,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.05799654  0.04729172  0.88990095  0.07214008  1.22688305 -0.19319606  1.59516361 
# P-VALUE: 4.231067e-02
modelo_energia_dlog_cambio_3$par
modelo_energia_dlog_cambio_3$broom.mgarch

# 1 LAG

modelo_energia_dlog_cambio_1 <- fit_mfgarch(
  data = painel_midas_limpo, 
  y = "energy_log_ret",    # O alvo: O setor vulnerável ao choque cambial e juros
  x = "d_log_var_cambio",      # O choque: Risco global em nível
  low.freq = "year_month",   
  K = 1,                    # Memória de um ano para a propagação da crise
  gamma = TRUE,              
  weighting = "beta.restricted"
)

# Resultado
#         mu       alpha        beta       gamma           m       theta 
# 0.05747110  0.04763475  0.88861359  0.07302404  1.24423733 -0.03640285  
# P-VALUE: 2.724546e-01
modelo_energia_dlog_cambio_1$par
modelo_energia_dlog_cambio_1$broom.mgarch


#todo TESTANDO GPR + CAMBIO PARA O SETOR DE ENERGIA

#* LOG-LOG

modelo_energia_log_gpr_log_cambio <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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
#        mu      alpha       beta      gamma          m      theta         w2  theta.two     w2.two 
#0.05635955 0.04930768 0.87954481 0.07650421 1.96348940 0.06845183 1.28424683 0.15898351 1.76042910  
# P-VALUE THETA: 6.222225e-01
# P-VALUE THETA 2: 2.242320e-01
# BIC: 24289.3
modelo_energia_log_gpr_log_cambio$par
modelo_energia_log_gpr_log_cambio$broom.mgarch
modelo_energia_log_gpr_log_cambio$bic

# 6 Lags

modelo_energia_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = painel_midas_limpo,
  y = "energy_log_ret",
  
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
# 0.05837420  0.04868162  0.89331218  0.06805309  1.62327283 -0.22673284  1.00006116 -0.09686746 11.30063343 
# P-VALUE THETA: 8.467430e-01
# P-VALUE THETA 2: 3.224720e-01
# BIC: 24798.07
modelo_energia_log_gpr_log_cambio_6$par
modelo_energia_log_gpr_log_cambio_6$broom.mgarch
modelo_energia_log_gpr_log_cambio_6$bic


#! Tentativa de loop para o setor de energia


# Combinações de janelas que você está utilizando
pares_k <- data.frame(
  K1 = c(12, 6, 3, 3),
  K2 = c(12, 6, 3, 1)
)

# Combinações das transformações
pares_transformacoes <- expand.grid(
  x = c(
    "log_gpr_global",
    "d_log_gpr_global"
  ),
  x_two = c(
    "log_var_cambio",
    "d_log_var_cambio"
  ),
  stringsAsFactors = FALSE
)

# Cruzar transformações com os pares de K
especificacoes <- do.call(
  rbind,
  lapply(seq_len(nrow(pares_transformacoes)), function(i) {
    data.frame(
      x = pares_transformacoes$x[i],
      x_two = pares_transformacoes$x_two[i],
      K1 = pares_k$K1,
      K2 = pares_k$K2,
      stringsAsFactors = FALSE
    )
  })
)

# Nomes mais fáceis para a tabela
especificacoes$transformacao1 <- ifelse(
  especificacoes$x == "log_gpr_global",
  "LOG",
  "LOG-DIFF"
)

especificacoes$transformacao2 <- ifelse(
  especificacoes$x_two == "log_var_cambio",
  "LOG",
  "LOG-DIFF"
)

print(especificacoes)

extrair_valor <- function(tabela, termo, coluna) {
  
  posicao <- which(tabela$term == termo)
  
  if (length(posicao) != 1) {
    return(NA_real_)
  }
  
  as.numeric(tabela[posicao, coluna])
}

# Lista que guardará os modelos completos
modelos_energia <- list()

# Lista que guardará apenas o resumo para o Excel
linhas_resultados <- vector(
  mode = "list",
  length = nrow(especificacoes)
)

for (i in seq_len(nrow(especificacoes))) {
  
  esp <- especificacoes[i, ]
  
  nome_modelo <- paste0(
    "energia_gpr_",
    tolower(gsub("-", "_", esp$transformacao1)),
    "_k", esp$K1,
    "_cambio_",
    tolower(gsub("-", "_", esp$transformacao2)),
    "_k", esp$K2
  )
  
  cat(
    "\nEstimando",
    i,
    "de",
    nrow(especificacoes),
    ":",
    nome_modelo,
    "\n"
  )
  
  modelo <- tryCatch(
    
    fit_mfgarch(
      data = painel_midas_limpo,
      y = "energy_log_ret",
      
      x = esp$x,
      low.freq = "year_month",
      K = esp$K1,
      weighting = "beta.restricted",
      
      x.two = esp$x_two,
      low.freq.two = "year_month",
      K.two = esp$K2,
      weighting.two = "beta.restricted",
      
      gamma = TRUE
    ),
    
    error = function(e) e
  )
  
  # Caso a estimação dê erro
  if (inherits(modelo, "error")) {
    
    linhas_resultados[[i]] <- data.frame(
      setor = "Energia",
      variavel1 = "GPR",
      transformacao1 = esp$transformacao1,
      K1 = esp$K1,
      variavel2 = "Cambio",
      transformacao2 = esp$transformacao2,
      K2 = esp$K2,
      theta1 = NA_real_,
      p_value1 = NA_real_,
      theta2 = NA_real_,
      p_value2 = NA_real_,
      BIC = NA_real_,
      erro = conditionMessage(modelo)
    )
    
    next
  }
  
  # Guardar o objeto completo
  modelos_energia[[nome_modelo]] <- modelo
  
  tabela_modelo <- modelo$broom.mgarch
  
  # Extrair resultados para a tabela
  linhas_resultados[[i]] <- data.frame(
    setor = "Energia",
    
    variavel1 = "GPR",
    transformacao1 = esp$transformacao1,
    K1 = esp$K1,
    
    variavel2 = "Cambio",
    transformacao2 = esp$transformacao2,
    K2 = esp$K2,
    
    theta1 = extrair_valor(
      tabela_modelo,
      "theta",
      "estimate"
    ),
    
    p_value1 = extrair_valor(
      tabela_modelo,
      "theta",
      "p.value"
    ),
    
    theta2 = extrair_valor(
      tabela_modelo,
      "theta.two",
      "estimate"
    ),
    
    p_value2 = extrair_valor(
      tabela_modelo,
      "theta.two",
      "p.value"
    ),
    
    BIC = as.numeric(modelo$bic),
    erro = NA_character_
  )
}

resultados_energia <- do.call(
  rbind,
  linhas_resultados
)

# Arredondar apenas para visualização
resultados_energia_excel <- resultados_energia

colunas_numericas <- c(
  "theta1",
  "p_value1",
  "theta2",
  "p_value2",
  "BIC"
)

resultados_energia_excel[colunas_numericas] <-
  round(
    resultados_energia_excel[colunas_numericas],
    4
  )

print(resultados_energia_excel)
