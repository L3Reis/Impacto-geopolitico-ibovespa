# Pacotes
library(dplyr)
library(xts)
library(rumidas)


# Dados -------------------------------------------------------------------


# Log-retorno Ibovespa 03-01-2000 a 30-12-2025
r_ibov <- readRDS(file = 'dados/retorno_ibovespa.rds')

# Indice GPR Brasil 01-1995 a 01-2026
gpr <- readRDS( file = 'dados/indice_GPRH.rds')


# transformando em xts para entrar no rumidas

ibov_df <- as.data.frame(r_ibov) |>
  rename(date = data) |>
  mutate(
    date = as.Date(date),
    retorno = as.numeric(retorno)
  ) |>
  arrange(date)

ibov_xts <- xts(ibov_df$retorno, order.by = ibov_df$date)

gpr_not_xts  <- as.data.frame(gpr) |> 
  mutate(
    mes_ano_date = as.Date(mes_ano),                 # vira 1º dia do mês
    mes_ano_date = as.Date(format(mes_ano_date, "%Y-%m-01")),
    GPRH = as.numeric(GPRH)
  ) |> 
  distinct(mes_ano_date, .keep_all = TRUE) |> 
  arrange(mes_ano_date)

gpr_xts <- xts(gpr_not_xts$GPRH, order.by = gpr_not_xts$mes_ano_date)



# Estimação ---------------------------------------------------------------

# Lags do Midas
K = 12

# Ar(1)
ar_1 <- arima(as.numeric(ibov_xts), order = c(1,0,0), include.mean = TRUE)

# Residuos do AR

res <- xts(as.numeric(residuals(ar_1)), order.by = ibov_df$date)

# Criação de matriz
mv_mat_res <- mv_into_mat(x = res, mv = gpr_xts, K = K, type = 'monthly')

# modelos
fit_sym_ar1 <- ugmfit(model="GM", skew="NO",  distribution="std", res, mv_mat_res, K = K)
fit_asy_ar1 <- ugmfit(model="GM", skew="YES", distribution="std", res, mv_mat_res, K = K)

# Outputs (para comparar com mfGARCH)
fit_sym_ar1$rob_coef_mat
fit_asy_ar1$rob_coef_mat

# (opcional) summaries
summary(fit_sym_ar1)
summary(fit_asy_ar1)
