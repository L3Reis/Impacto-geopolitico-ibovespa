# Pacotes
library(quantmod)
library(forecast)
library(rugarch)
library(mfGARCH)
library(broom)
library(lubridate)
library(dplyr)

# Dados -------------------------------------------------------------------

# Log-retorno Ibovespa 03-01-2000 a 30-12-2025
r_ibov <- readRDS(file = 'dados/retorno_ibovespa.rds')

# Indice GPR Brasil 01-2000 a 01-2026
gpr <- readRDS( file = 'dados/indice_GPRH.rds')


# Unindo as bases
base <- left_join(
  x = r_ibov,
  y = gpr,
  by = 'mes_ano'
) |> rename(date = data )



# Estimação In-Sample -----------------------------------------------------

# Amostra
base_in_sample <- base |> dplyr::filter(date <= '2022-12-31')

# colocar 'mes_ano' no formato date

base_in_sample <- base_in_sample |>
  mutate(mes_ano = as.Date(format(date, "%Y-%m-01")))

# Estimação sem AR(1)

m_1 <- fit_mfgarch(
  data = as.data.frame(base_in_sample),
  y = 'retorno',  # Usamos o "erro" limpo do AR
  x = 'GPRH',
  low.freq = 'mes_ano',
  K = 12,
  weighting = "beta.restricted",
  gamma = TRUE
)

# Resultado
m_1$broom.mgarch

# Testando AR(1)-GARCH-MIDAS

# AR(1)
modelo_ar <- arima(base_in_sample$retorno, order = c(1, 0, 0))

# 2. Extrair os resíduos (o que o AR não explicou)
base_in_sample$residuos_ar <- residuals(modelo_ar)


# 3. Rodar o GARCH-MIDAS sobre os resíduos em vez dos retornos brutos
a_1 <- fit_mfgarch(
  data = as.data.frame(base_in_sample),
  y = 'residuos_ar',  # Usamos o "erro" limpo do AR
  x = 'GPRH',
  low.freq = 'mes_ano',
  K = 12,
  weighting = "beta.restricted",
  gamma = TRUE
)

a_1$broom.mgarch


# Testes ------------------------------------------------------------------

# Ljung-box manual
df <- a_1$df.fitted

# manter só linhas válidas
df_ok <- df[is.finite(df$residuals) & is.finite(df$g) & is.finite(df$tau), ]

# sigma_t = sqrt(tau_t * g_t)
sigma <- sqrt(as.numeric(df_ok$tau) * as.numeric(df_ok$g))

# resíduo padronizado
z <- as.numeric(df_ok$residuals) / sigma
z <- z[is.finite(z)]

# Ljung–Box em z e z^2 (lags típicos para diário)
lags <- c(5, 10, 20, 60)

out <- do.call(rbind, lapply(lags, function(L){
  cbind(
    lag = L,
    p_z  = Box.test(z,   lag = L, type = "Ljung-Box")$p.value,
    p_z2 = Box.test(z^2, lag = L, type = "Ljung-Box")$p.value
  )
}))

print(out)

# Grafico

# manter só linhas válidas
ok <- is.finite(df$date) & is.finite(df$tau) & is.finite(df$g)
d  <- df$date[ok]

vol_total <- sqrt(df$tau[ok] * df$g[ok])
vol_lp    <- sqrt(df$tau[ok])
vol_sp    <- sqrt(df$g[ok])

# padronizar (z-score) para caber no mesmo eixo
vt <- as.numeric(scale(vol_total))
vl <- as.numeric(scale(vol_lp))
vs <- as.numeric(scale(vol_sp))

plot(d, vt, type="l", xlab="Data", ylab="Volatilidade (padronizada)", col = 'black')
lines(d, vl, col = 'blue')
lines(d, vs, col = 'red')

legend("topright",
       legend=c("Total  sqrt(tau*g)", "Longo prazo  sqrt(tau)", "Curto prazo  sqrt(g)"),
       col=c("black","blue","red"),
       lty=c(1,2,3),
       lwd=2,
       bty="n")
grid()



# Previsao 1 mês ----------------------------------------------------------------

base <- base %>%
  mutate(
    date   = as.Date(date),
    mes_ano = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  arrange(date)


# ============================================================
# 0) Base pronta
# ============================================================
# 0) Base pronta ==========================================================
base <- as.data.frame(base)

base$date <- as.Date(base$date)
base$mes_ano <- as.Date(format(base$date, "%Y-%m-01"))

base <- base[order(base$date), ]

# checagens rápidas
stopifnot(inherits(base$date, "Date"))
stopifnot(inherits(base$mes_ano, "Date"))
stopifnot(!any(is.na(base$date)))
stopifnot(!any(is.na(base$mes_ano)))

# ============================================================
# 1) Helpers
# ============================================================

# pesos MIDAS
get_weights_midas <- function(fit) {
  w <- fit$est.weighting
  if (is.data.frame(w) || is.matrix(w)) {
    w <- w[, ncol(w), drop = TRUE]
  }
  as.numeric(w)
}

# tau_{t+1|t}
calc_tau_next <- function(fit, gpr_monthly, origin_month) {
  w <- get_weights_midas(fit)
  K <- length(w)
  
  m     <- as.numeric(fit$par["m"])
  theta <- as.numeric(fit$par["theta"])
  
  pos <- match(origin_month, gpr_monthly$mes_ano)
  if (is.na(pos)) return(NA_real_)
  
  idx <- pos:(pos - K + 1)
  if (min(idx) < 1) return(NA_real_)
  
  xlags <- gpr_monthly$GPRH[idx]
  
  exp(m + theta * sum(w * xlags))
}

# previsão mensal do MIDAS
forecast_one_month_midas <- function(fit, window_df, base_full) {
  
  origin_month <- max(window_df$mes_ano, na.rm = TRUE)
  target_month <- origin_month %m+% months(1)
  
  # datas do mês seguinte
  dates_next <- base_full$date[base_full$mes_ano == target_month]
  N_next <- length(dates_next)
  if (N_next == 0) return(NULL)
  
  # tau_{t+1|t}
  gpr_monthly <- window_df |>
    distinct(mes_ano, GPRH) |>
    arrange(mes_ano)
  
  tau_next <- calc_tau_next(fit, gpr_monthly, origin_month)
  if (!is.finite(tau_next)) return(NULL)
  
  # último estado estimado
  df <- fit$df.fitted
  df_ok <- df[is.finite(df$g) & is.finite(df$tau) & is.finite(df$residuals), ]
  if (nrow(df_ok) == 0) return(NULL)
  
  last <- df_ok[nrow(df_ok), ]
  
  g_last   <- as.numeric(last$g)
  tau_last <- as.numeric(last$tau)
  eps_last <- as.numeric(last$residuals)
  
  u2_last <- (eps_last^2) / tau_last
  I_last  <- as.numeric(eps_last < 0)
  
  alpha <- as.numeric(fit$par["alpha"])
  beta  <- as.numeric(fit$par["beta"])
  gamma <- if ("gamma" %in% names(fit$par)) as.numeric(fit$par["gamma"]) else 0
  
  phi <- alpha + beta + gamma/2
  c0  <- 1 - phi
  
  g_fore <- numeric(N_next)
  
  # primeiro dia previsto
  g_fore[1] <- c0 + beta * g_last + (alpha + gamma * I_last) * u2_last
  
  # passos seguintes em expectativa
  if (N_next >= 2) {
    for (j in 2:N_next) {
      g_fore[j] <- c0 + phi * g_fore[j - 1]
    }
  }
  
  RV_hat <- sum(tau_next * g_fore)
  
  RV_real <- base_full |>
    filter(mes_ano == target_month) |>
    summarise(RV = sum(retorno^2, na.rm = TRUE)) |>
    pull(RV)
  
  tibble(
    target_month = target_month,
    Real_RV = RV_real,
    Prev_MIDAS = RV_hat
  )
}

# previsão mensal do GARCH benchmark
forecast_one_month_garch <- function(fit_garch, base_full, origin_month) {
  
  target_month <- origin_month %m+% months(1)
  N_next <- sum(base_full$mes_ano == target_month)
  if (N_next == 0) return(NA_real_)
  
  fc <- ugarchforecast(fit_garch, n.ahead = N_next)
  sum(as.numeric(fc@forecast$sigmaFor)^2)
}

# ============================================================
# 2) Rolling OOS mensal com fixed-length window
# ============================================================

roll_oos_1m_compare <- function(base_full,
                                insample_end = as.Date("2022-12-31"),
                                K = 12,
                                gamma_on = TRUE,
                                n_months_test = NULL) {
  
  base_full <- base_full |>
    mutate(
      date = as.Date(date),
      mes_ano = as.Date(format(date, "%Y-%m-01"))
    ) |>
    arrange(date)
  
  # tamanho fixo da janela
  base_in <- base_full |>
    filter(date <= insample_end)
  
  window_size <- nrow(base_in)
  
  # último pregão de cada mês
  month_ends <- base_full |>
    group_by(mes_ano) |>
    summarise(month_end = max(date), .groups = "drop") |>
    arrange(mes_ano)
  
  origin_start_month <- as.Date(format(insample_end, "%Y-%m-01"))
  
  origin_months <- month_ends |>
    filter(mes_ano >= origin_start_month) |>
    filter(mes_ano < max(mes_ano)) |>
    arrange(mes_ano)
  
  if (!is.null(n_months_test)) {
    origin_months <- origin_months |> slice(1:n_months_test)
  }
  
  out_list <- vector("list", nrow(origin_months))
  
  for (i in seq_len(nrow(origin_months))) {
    
    cat("Iteração", i, "de", nrow(origin_months),
        "- origem:", as.character(origin_months$mes_ano[i]), "\n")
    
    origin_month <- origin_months$mes_ano[i]
    origin_end   <- origin_months$month_end[i]
    
    idx_end <- max(which(base_full$date <= origin_end))
    idx_start <- idx_end - window_size + 1
    if (idx_start < 1) next
    
    window_df <- base_full[idx_start:idx_end, , drop = FALSE]
    
    # --------------------------------------------------------
    # MODELO A: AR(1)-GARCH-MIDAS
    # --------------------------------------------------------
    ar1 <- tryCatch(
      stats::arima(window_df$retorno, order = c(1,0,0), include.mean = TRUE),
      error = function(e) NULL
    )
    if (is.null(ar1)) next
    
    window_df$residuos_ar <- as.numeric(stats::residuals(ar1))
    
    fit_midas <- tryCatch(
      fit_mfgarch(
        data = as.data.frame(window_df),
        y = "residuos_ar",
        x = "GPRH",
        low.freq = "mes_ano",
        K = K,
        weighting = "beta.restricted",
        gamma = gamma_on
      ),
      error = function(e) NULL
    )
    if (is.null(fit_midas)) next
    
    prev_midas_tbl <- forecast_one_month_midas(fit_midas, window_df, base_full)
    if (is.null(prev_midas_tbl)) next
    
    # --------------------------------------------------------
    # MODELO B: AR(1)-GARCH benchmark
    # --------------------------------------------------------
    spec_garch <- rugarch::ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
      distribution.model = "norm"
    )
    
    fit_garch <- tryCatch(
      rugarch::ugarchfit(spec = spec_garch, data = window_df$retorno, solver = "hybrid"),
      error = function(e) NULL
    )
    if (is.null(fit_garch)) next
    
    prev_garch <- forecast_one_month_garch(fit_garch, base_full, origin_month)
    
    out_list[[i]] <- prev_midas_tbl |>
      mutate(
        origin_month = origin_month,
        Prev_GARCH = prev_garch
      ) |>
      select(origin_month, target_month, Real_RV, Prev_MIDAS, Prev_GARCH)
  }
  
  bind_rows(out_list)
}

# ============================================================
# 3) Rodar teste curto primeiro (3 meses)
# ============================================================

df_previsoes <- roll_oos_1m_compare(
  base_full = base,
  insample_end = as.Date("2022-12-31"),
  K = 12,
  gamma_on = TRUE,
  n_months_test = NULL
)

df_previsoes

df_previsoes <- df_previsoes |>
  mutate(
    MSE_MIDAS = (Real_RV - Prev_MIDAS)^2,
    MSE_GARCH = (Real_RV - Prev_GARCH)^2,
    QLIKE_MIDAS = log(Prev_MIDAS) + Real_RV/Prev_MIDAS,
    QLIKE_GARCH = log(Prev_GARCH) + Real_RV/Prev_GARCH
  )

colMeans(df_previsoes[,6:9], na.rm=TRUE)

previsao_garch_midas_1m <- df_previsoes

#salvando as previsoes
saveRDS(previsao_garch_midas_1m, file = 'previsoes/previsao_garch_midas_1m.rds')

# dividindo em dois blocos
n <- nrow(df_previsoes)

# dividir em dois blocos e calcular MSE / QLIKE
metricas_blocos <- df_previsoes |>
  mutate(
    bloco = ifelse(
      target_month < as.Date("2024-07-01"),
      "Bloco_1",
      "Bloco_2"
    )
  ) |>
  group_by(bloco) |>
  summarise(
    MSE_MIDAS = mean((Real_RV - Prev_MIDAS)^2, na.rm = TRUE),
    MSE_GARCH = mean((Real_RV - Prev_GARCH)^2, na.rm = TRUE),
    QLIKE_MIDAS = mean(log(Prev_MIDAS) + Real_RV / Prev_MIDAS, na.rm = TRUE),
    QLIKE_GARCH = mean(log(Prev_GARCH) + Real_RV / Prev_GARCH, na.rm = TRUE),
    .groups = "drop"
  )

metricas_blocos
