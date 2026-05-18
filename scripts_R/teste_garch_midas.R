# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)

## Importando base de dados crua

# Base diária
base_teste_modelo_1 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_gpr.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
base_mfgarch <- base_teste_modelo_1 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_bra = as.numeric(gpr_bra),
    gpr_global = as.numeric(gpr_global),
    gpr_global_z = as.numeric(gpr_global_z),
    ret_ibov_100 = ret_ibov * 100
  ) %>%
  select(
    date,
    ret_ibov_100,
    year_month,
    gpr_bra
  ) %>%
  na.omit()

# Modelo 1: GARCH-MIDAS com GPR Brasil

modelo_gpr <- fit_mfgarch(
  data = base_mfgarch,
  y = "ret_ibov_100",
  x = "gpr_bra",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Coeficientes

modelo_gpr$par

modelo_gpr$broom.mgarch


# Teste com GPR padronizado
# GPR padronizado = (GPR - média do GPR) / desvio-padrão do GPR

base_mfgarch_2 <- base_mfgarch %>%
  mutate(
    gpr_bra_z = as.numeric(scale(gpr_bra))
  )

modelo_gpr_2 <- fit_mfgarch(
  data = base_mfgarch_2,
  y = "ret_ibov_100",
  x = "gpr_bra_z",
  low.freq = "year_month",
  K = 6,
  gamma = FALSE,
  weighting = "beta.restricted"
)

modelo_gpr_2$par
modelo_gpr_2$broom.mgarch


# Testando GPR Global


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

# Modelo 3: GARCH-MIDAS com GPR Global 12 lags
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

# Modelo 4: GARCH-MIDAS com GPR Global 6 lags
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

# Modelo 5: GARCH-MIDAS com GPR Global 3 lags
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



# Modelo 6: GARCH-MIDAS com GPR Global padronizado
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

modelo_gpr_global_dlog_6$par
modelo_gpr_global_dlog_6$broom.mgarch

# # Modelo com Log-diferença do GPR com 3 lags

# Positivo e significativo! p-value 0.00895

modelo_gpr_global_dlog_3 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  gamma = FALSE,
  weighting = "beta.restricted"
)

modelo_gpr_global_dlog_3$par
modelo_gpr_global_dlog_3$broom.mgarch

# comparando BIC entre os dois
modelo_gpr_global_dlog_3$bic
modelo_gpr_global_dlog$bic

# resultados semelhantes, mas k = 6 melhor

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
