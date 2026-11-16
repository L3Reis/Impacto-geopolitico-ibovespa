# Rolling mensal; previsões diárias com predict() original do mfGARCH.
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(mfGARCH)

data_corte <- as.Date("2015-08-31")
horizontes <- c(1L, 5L, 22L)
janela_anos <- 2L
K <- 24L
n_origens <- Inf                       # Inf para todo o teste.

dados <- read_excel("dados/base_mfgarch_completa.xlsx") |>
  drop_na() |>
  mutate(date = as.Date(date), year_month = as.Date(paste0(year_month, "-01")),
         ret_ibov_100 = as.numeric(ret_ibov) * 100) |>
  filter(date >= as.Date("2000-02-02")) |>
  arrange(date)
mensais <- dados |>
  group_by(year_month) |>
  summarise(gpr = first(log_gpr_global), vr_ibov = sum(ret_ibov_100^2), .groups = "drop") |>
  mutate(gpr_disponivel = lag(gpr))       # Mais o lag 1 do pacote: GPR em lag 2.
dados <- dados |>
  select(date, year_month, ret_ibov_100) |>
  left_join(mensais, by = "year_month") |>
  select(-gpr) |>
  as.data.frame()

# Ajuste restrito do pacote, com poucos inícios para contornar falhas numéricas.
estimar <- function(w, x, inicio, anterior = NULL) {
  r <- w$ret_ibov_100[w$date >= inicio]
  p0 <- c(mu = mean(r), alpha = .05, beta = .85, gamma = .10,
          m = log(var(r)), theta = 0, w2 = 2)
  p1 <- p0; p1[c("beta", "w2")] <- c(.70, 5)
  inicios <- list(p0, p1, NULL)
  if (!is.null(anterior)) inicios <- append(inicios, list(anterior), after = 1)
  for (k in seq_along(inicios)) {
    m <- tryCatch({
      invisible(capture.output(z <- fit_mfgarch(
        w, y = "ret_ibov_100", x = x, low.freq = "year_month", K = K,
        gamma = TRUE, weighting = "beta.restricted", multi.start = FALSE,
        control = list(par.start = inicios[[k]]))))
      z
    }, error = identity)
    if (inherits(m, "error")) next
    p <- m$par
    rho <- p["alpha"] + p["beta"] + p["gamma"]/2
    if (m$optim$convergence == 0 && all(is.finite(p)) &&
        p["alpha"] >= 0 && p["beta"] >= 0 && p["alpha"] + p["gamma"] >= 0 &&
        rho >= 0 && rho < 1 && p["w2"] >= 1 &&
        all(is.finite(c(tail(m$g, 1), tail(m$tau, 1), m$tau.forecast))) &&
        min(tail(m$g, 1), tail(m$tau, 1), m$tau.forecast) > 0 &&
        identical(m$df.fitted$date[is.finite(m$df.fitted$residuals)], w$date[w$date >= inicio])) {
      attr(m, "tentativa") <- k
      return(m)
    }
  }
  stop("Nenhuma tentativa produziu ajuste convergente e admissível.")
}

origens <- seq.int(which(dados$date > data_corte)[1] - 1L, nrow(dados) - 1L)
origens <- head(origens, min(length(origens), n_origens))
grupos <- split(origens, dados$year_month[origens + 1L])
variaveis <- c(gpr = "gpr_disponivel", vr = "vr_ibov")
anteriores <- list(gpr = NULL, vr = NULL)
resultados <- list(data.frame(origem = as.Date(character()), horizonte = integer(),
                             modelo = character(), previsao = double()))
falhas <- ajustes <- list()
filtro_g <- getFromNamespace("calculate_g", "mfGARCH")

for (dias in grupos) {
  j0 <- dias[1]
  origem_ajuste <- dados$date[j0]
  inicio <- (origem_ajuste %m-% years(janela_anos)) + 1L
  mes_inicio <- floor_date(inicio, "month")
  # K meses de histórico + dois anos de retornos efetivamente estimados.
  w <- filter(dados, date >= (mes_inicio %m-% months(K)), date <= origem_ajuste,
              year_month < mes_inicio | date >= inicio)
  mes <- dados$year_month[j0]
  w$vr_ibov[w$year_month == mes] <-
    sum(dados$ret_ibov_100[dados$year_month == mes & dados$date <= origem_ajuste]^2)
  message("Ajuste mensal: ", origem_ajuste)
  for (nome in names(variaveis)) {
    m <- tryCatch(estimar(w, variaveis[[nome]], inicio, anteriores[[nome]]), error = identity)
    if (inherits(m, "error")) {
      falhas[[length(falhas) + 1L]] <- data.frame(estimacao = origem_ajuste, modelo = nome,
                                                motivo = conditionMessage(m))
      message("  Falha em ", nome, ": ", conditionMessage(m))
      next
    }
    anteriores[[nome]] <- m$par
    ajustes[[length(ajustes) + 1L]] <- data.frame(estimacao = origem_ajuste, modelo = nome,
                                                tentativa = attr(m, "tentativa"))
    p <- m$par
    g <- tail(m$g, 1)
    tau_atual <- tail(m$tau, 1)
    # Tau conhecido na origem, mantido fixo nos horizontes: não prevê GPR futuro.
    tau <- if (dados$year_month[j0 + 1L] != mes) m$tau.forecast else tau_atual
    for (j in dias) {
      h <- horizontes[j + horizontes <= nrow(dados)]
      v <- tryCatch(predict(m, horizon = h, fcts.tau = tau,
                           y.last = dados$ret_ibov_100[j], cond.var = g, cond.tau = tau_atual),
                    error = function(e) rep(NA_real_, length(h)))
      if (any(!is.finite(v) | v <= 0)) {
        falhas[[length(falhas) + 1L]] <- data.frame(estimacao = origem_ajuste, modelo = nome,
                                                  motivo = paste("Previsão inválida em", dados$date[j]))
        break
      }
      resultados[[length(resultados) + 1L]] <- data.frame(
        origem = dados$date[j], horizonte = h, modelo = nome, previsao = v)
      g <- tail(filtro_g(
        omega = 1-p["alpha"]-p["beta"]-p["gamma"]/2,
        alpha = p["alpha"], beta = p["beta"], gamma = p["gamma"],
        returns = c((dados$ret_ibov_100[j]-p["mu"])/sqrt(tau_atual), 0), g0 = g), 1)
      tau_atual <- tau
    }
  }
}

# Variância pontual no pregão t+h; comparação nas mesmas datas para os dois modelos.
previsoes_rolling <- expand_grid(j = origens, horizonte = horizontes, modelo = names(variaveis)) |>
  filter(j + horizonte <= nrow(dados)) |>
  mutate(origem = dados$date[j], data = dados$date[j + horizonte],
         observado = dados$ret_ibov_100[j + horizonte]^2) |>
  select(-j) |>
  left_join(bind_rows(resultados), by = c("origem", "horizonte", "modelo"))

perdas_rolling <- previsoes_rolling |>
  group_by(origem, horizonte) |>
  mutate(comparavel = all(is.finite(previsao) & previsao > 0)) |>
  ungroup() |>
  mutate(MSE = if_else(comparavel, (observado-previsao)^2, NA_real_),
         QLIKE = if_else(comparavel, log(previsao)+observado/previsao, NA_real_))

precisao_rolling <- perdas_rolling |>
  group_by(modelo, horizonte) |>
  summarise(n_total = n(), n_validas = sum(comparavel),
            MSE = mean(MSE, na.rm = TRUE), QLIKE = mean(QLIKE, na.rm = TRUE), .groups = "drop")

falhas_rolling <- bind_rows(falhas)
ajustes_rolling <- bind_rows(ajustes)
print(as.data.frame(precisao_rolling), row.names = FALSE, digits = 7)
if (nrow(falhas_rolling)) print(falhas_rolling) else message("Nenhuma falha de ajuste ou previsão.")

perdas_rolling |>
  filter(comparavel, modelo == "vr") |>
  group_by(horizonte) |>
  slice_max(QLIKE, n = 5, with_ties = FALSE) |>
  select(origem, data, horizonte, observado, previsao, QLIKE) |>
  print(n = Inf)
