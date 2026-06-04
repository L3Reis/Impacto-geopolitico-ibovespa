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

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
base_mfgarch_completa <- base_teste_modelo_2 %>%
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
  ) %>%
  na.omit()


# Modelo GPR em nível

# Modelo GARCH-MIDAS com GPR Global 12 lags
# não significativo
modelo_gpr_global <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
modelo_gpr_global$par
modelo_gpr_global$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 6 lags
# não significativo

modelo_gpr_global_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
modelo_gpr_global_6$par
modelo_gpr_global_6$broom.mgarch

# Modelo  GARCH-MIDAS com GPR Global 3 lags
# não significativo

modelo_gpr_global_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
modelo_gpr_global_3$par
modelo_gpr_global_3$broom.mgarch



# Modelo  GARCH-MIDAS com GPR Global padronizado
# não significativo
modelo_gpr_global_z <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "gpr_global_z",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
modelo_gpr_global_z$par
modelo_gpr_global_z$broom.mgarch


## Vamos testar a estacionariedade do GPR
acf(
  base_mfgarch_completa$gpr_global,
  lag.max = 48,
  main = "ACF - GPR Global em nível"
)

# Muita persistência, necessário estacionarizar


# ACF das versões do GPR Global

par(mfrow = c(3, 1))

acf(
  na.omit(base_mfgarch_completa$gpr_global),
  lag.max = 48,
  main = "ACF - GPR Global em nível"
)

acf(
  na.omit(base_mfgarch_completa$d_gpr_global),
  lag.max = 48,
  main = "ACF - Diferença simples do GPR Global"
)

acf(
  na.omit(base_mfgarch_completa$d_log_gpr_global),
  lag.max = 48,
  main = "ACF - Log-diferença do GPR Global"
)

Apar(mfrow = c(1, 1))


# Modelo com Log-diferença do GPR com 6 lags
# Deu positivo, mas não significativo (primeiro theta positivo até agora!)
# Modelo com 12 lags deu negativo

modelo_gpr_global_dlog_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.06141032 0.07045546 0.91539729 0.97960369 0.26632892 2.86182396 
# p-value: 5.134995e-01

modelo_gpr_global_dlog_6$par
modelo_gpr_global_dlog_6$broom.mgarch


# # Modelo com Log-diferença do GPR com 3 lags

# Positivo e significativo a 10% ! p-value 0.00895

modelo_gpr_global_dlog_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.05959316 0.06973897 0.91704556 1.01012147 0.49603868 1.00002991 
# p-value: 8.955343e-02

modelo_gpr_global_dlog_3$par
modelo_gpr_global_dlog_3$broom.mgarch

# comparando BIC entre os dois
modelo_gpr_global_dlog_3$bic
modelo_gpr_global_dlog$bic

# resultados semelhantes, mas k = 6 melhor


# # Modelo com Log-diferença do GPR com 3 lags e assimétrico

# Positivo e  e nao significativo

modelo_gpr_global_dlog_3_assim <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03597286 0.02917944 0.91655669 0.07285777 0.92191373 0.43949946 1.16563611 
# p-value: 1.288602e-01 
# opg.p.value: 5.060728e-02

modelo_gpr_global_dlog_3_assim$par
modelo_gpr_global_dlog_3_assim$broom.mgarch





# # Modelo com Log-diferença do GPR com 2 lags

# Positivo e não significativo

modelo_gpr_global_dlog_2 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 2,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#        mu      alpha       beta          m      theta         w2 
# 0.06041784 0.07061676 0.91546665 0.99566673 0.21760519 1.00001592 
# p-value: 6.985779e-01

modelo_gpr_global_dlog_2$par
modelo_gpr_global_dlog_2$broom.mgarch


# # Modelo com Log-diferença do GPR com 4 lags

# Positivo e não significativo

modelo_gpr_global_dlog_4 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 4,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#       mu      alpha       beta          m      theta         w2 
# 0.06036051 0.07018106 0.91559755 0.97573642 0.19933884 2.42827781 
# p-value: 5.337953e-01

modelo_gpr_global_dlog_4$par
modelo_gpr_global_dlog_4$broom.mgarch


### MODELOS COM DIFERENÇA DO GPR GLOBAL

## Modelo com Diferença do GPR 6 lags
# positivo, mas não significativo 

modelo_gpr_global_d_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_global",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

modelo_gpr_global_d_6$par
modelo_gpr_global_d_6$broom.mgarch

# Modelo com diferença do GPR com 3 lags
# positivo, mas não significativo a 10%

modelo_gpr_global_d_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

modelo_gpr_global_d_3$par
modelo_gpr_global_d_3$broom.mgarch

## GPR GLOBAL EM LOG

# # Modelo com Log do GPR com 12 lags assimétrico

# Negativo e não significativo

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

modelo_gpr_global_log_12$par
modelo_gpr_global_log_12$broom.mgarch

# # Modelo com Log do GPR com 24 lags assimétrico

# Positivo e não significativo

modelo_gpr_global_log_24 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03460597  0.02561087  0.91482518  0.07883306  0.88906659 -0.00644732  1.05942863
# p-value: 8.893332e-01

modelo_gpr_global_log_24$par
modelo_gpr_global_log_24$broom.mgarch

# # Modelo com Log do GPR com 3 lags assimétrico

# Positivo e não significativo

modelo_gpr_global_log_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03604818  0.02961751  0.91380634  0.07603684  1.62457008 -0.14778201  1.00000692 
# p-value: 1.542663e-01

modelo_gpr_global_log_3$par
modelo_gpr_global_log_3$broom.mgarch

# ! Hipótese por enquanto: Devido a diversas especificações estarem dando não significativas, partimos do pressuposto de que a agregação do ibovespa está afetando os coeficientes
# ! e diferentes setores reagem de forma diferente ao risco geopolítico. Dessa forma, precisamos fazer a análise setorial deles