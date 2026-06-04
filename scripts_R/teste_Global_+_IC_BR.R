# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)


# Adaptando para o modelo


base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
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
    d_log_ic_br = as.numeric(d_log_ic_br)
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
  ) |> na.omit()


### Modelo GPR Global + IC-BR

## MODELO GPR GLOBAL + LOG-DIFF DO IC-BR

# Modelo GPR Global + IC-BRem log-diferença com 1 lag assimetrico

# GPR positivo e e nao significativo
# cambio negativo e nao significativo

modelo_d_log_gpr_comm_1 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "d_log_gpr_global",
  low.freq = "year_month",
  K = 3,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_ic_br",
  low.freq.two = "year_month",
  K.two = 1,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta 
# 0.03570376  0.02894955  0.91467523  0.07579538  0.91503796  0.41605025 
#         w2   theta.two      w2.two 
# 1.16717610 -0.15302780  1.82261049 
# theta p-value: 1.631930e-01
# theta 2 p-value: 1.053562e-01 

modelo_d_log_gpr_comm_1$par
modelo_d_log_gpr_comm_1$broom.mgarch
modelo_d_log_gpr_cambio$bic
