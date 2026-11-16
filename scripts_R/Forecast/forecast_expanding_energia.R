# Energia: expanding mensal; GPR divulgado no dia 01 e predict() original do mfGARCH.
energia_lag1 <- local({
  library(dplyr)
  library(tidyr)
  data_corte <- as.Date("2015-08-31")
  K <- 24L
  horizontes <- c(1L, 5L, 22L)
  n_origens <- Inf                       # Toda a amostra de teste.

  mensais <- readxl::read_excel("dados/base_mfgarch_completa.xlsx") |>
    transmute(year_month = as.Date(paste0(year_month, "-01")), gpr = as.numeric(log_gpr_global)) |>
    distinct() |> drop_na() |> arrange(year_month) |>
    mutate(gpr_anterior = lag(gpr))
  d <- readxl::read_excel("dados/base_setores_final.xlsx") |>
    transmute(date = as.Date(date), year_month = as.Date(paste0(year_month, "-01")),
              ret_energia_100 = as.numeric(energy_log_ret)) |>  # Ja em percentual.
    drop_na() |> filter(date >= as.Date("2000-02-02")) |> arrange(date) |>
    group_by(year_month) |> mutate(vr_energia = sum(ret_energia_100^2)) |> ungroup() |>
    left_join(mensais, by = "year_month") |> as.data.frame()
  inicio <- sort(unique(d$year_month))[K + 1L]  # Historico inicial para as defasagens.
  stopifnot(!anyNA(d), !anyDuplicated(d$date))

  ajustar <- function(w, x, anterior) {
    r <- w$ret_energia_100[w$date >= inicio]
    p0 <- c(mu = mean(r), alpha = .05, beta = .85, gamma = .10,
            m = log(var(r)), theta = 0, w2 = 2)
    p1 <- p0; p1[c("beta", "w2")] <- c(.70, 5)
    inicios <- list(p0, p1, NULL)
    if (!is.null(anterior)) inicios <- append(inicios, list(anterior), after = 1)
    if (x == "gpr") {
      p2 <- p0; p2["theta"] <- .5
      p2["m"] <- log(var(r)) - .5 * mean(unique(w[c("year_month", x)])[[x]])
      inicios <- append(inicios, list(p2))  # Inicio centrado, apenas se os anteriores falharem.
    }
    motivo <- "Ajuste fora das restricoes do modelo."
    for (k in seq_along(inicios)) {
      par0 <- inicios[[k]]
      if (k > 1L) message("  Nova tentativa ", k, " para ", x)
      z <- tryCatch({
        invisible(capture.output(z <- mfGARCH::fit_mfgarch(
          w, y = "ret_energia_100", x = x, low.freq = "year_month", K = K,
          gamma = TRUE, weighting = "beta.restricted", multi.start = FALSE,
          control = list(par.start = par0))))
        z
      }, error = identity)
      if (inherits(z, "error")) { motivo <- conditionMessage(z); next }
      p <- z$par
      rho <- p["alpha"] + p["beta"] + p["gamma"]/2
      if (z$optim$convergence == 0 && all(is.finite(p)) &&
          p["alpha"] >= 0 && p["beta"] >= 0 && p["alpha"] + p["gamma"] >= 0 &&
          rho >= 0 && rho < 1 && p["w2"] >= 1 &&
          all(is.finite(c(tail(z$g, 1), tail(z$tau, 1), z$tau.forecast))) &&
          min(tail(z$g, 1), tail(z$tau, 1), z$tau.forecast) > 0 &&
          identical(z$df.fitted$date[is.finite(z$df.fitted$residuals)], w$date[w$date >= inicio])) return(z)
      motivo <- paste("Ajuste rejeitado; convergencia:", z$optim$convergence,
                      "persistencia:", signif(rho, 5))
    }
    stop(motivo)
  }

  origens <- seq.int(which(d$date > data_corte)[1] - 1L, nrow(d) - 1L)
  origens <- head(origens, min(length(origens), n_origens))
  grupos <- split(origens, d$year_month[origens + 1L])
  variaveis <- c(gpr = "gpr", vr = "vr_energia")
  anteriores <- list(gpr = NULL, vr = NULL)
  modelos <- list(gpr = list(), vr = list())
  falhas <- list()
  novas <- list(data.frame(origem = as.Date(character()), horizonte = integer(),
                           modelo = character(), previsao = double(), mes_gpr = as.Date(character())))
  soma_tau <- getFromNamespace("sum_tau_fcts", "mfGARCH")
  filtro_g <- getFromNamespace("calculate_g", "mfGARCH")

  for (dias in grupos) {
    j0 <- dias[1]
    o <- d$date[j0]
    mes <- d$year_month[j0]
    w <- filter(d, date <= o)
    # A entrada GPR do mes corrente nao entra na verossimilhanca (MIDAS lag 1).
    w$gpr[w$year_month == mes] <- w$gpr_anterior[w$year_month == mes]
    w$vr_energia[w$year_month == mes] <- sum(d$ret_energia_100[d$year_month == mes & d$date <= o]^2)
    message("Energia; ajuste mensal: ", o)
    for (nome in names(variaveis)) {
      m <- tryCatch(ajustar(w, variaveis[[nome]], anteriores[[nome]]), error = identity)
      if (inherits(m, "error")) {
        falhas[[length(falhas) + 1L]] <- data.frame(estimacao = o, modelo = nome, motivo = conditionMessage(m))
        message("  Falha em ", nome, ": ", conditionMessage(m))
        next
      }
      anteriores[[nome]] <- m$par
      modelos[[nome]][[as.character(o)]] <- m
      p <- m$par
      g <- tail(m$g, 1)
      tau_atual <- tail(m$tau, 1)
      tau_vr <- if (d$year_month[j0 + 1L] != mes) m$tau.forecast else tau_atual
      for (j in dias) {
        mes_gpr <- as.Date(NA)
        if (nome == "gpr") {
          disponivel <- mensais$year_month < d$year_month[j]
          tau_atual <- exp(soma_tau(m = p["m"], i = K + 1L, theta = p["theta"],
            phivar = m$est.weighting, covariate = c(tail(mensais$gpr[disponivel], K), NA_real_), K = K))
          tau <- tau_atual
          mes_gpr <- tail(mensais$year_month[disponivel], 1)
        } else tau <- tau_vr
        h <- horizontes[j + horizontes <= nrow(d)]
        # Tau conhecido na origem fica fixo nos horizontes; nao usa GPR futuro.
        v <- tryCatch(stats::predict(m, horizon = h, fcts.tau = tau, cond.tau = tau_atual,
                                    y.last = d$ret_energia_100[j], cond.var = g),
                      error = function(e) rep(NA_real_, length(h)))
        if (any(!is.finite(v) | v <= 0)) {
          falhas[[length(falhas) + 1L]] <- data.frame(estimacao = o, modelo = nome,
                                                    motivo = paste("Previsao invalida em", d$date[j]))
          break
        }
        novas[[length(novas) + 1L]] <- data.frame(
          origem = d$date[j], horizonte = h, modelo = nome, previsao = v, mes_gpr = mes_gpr)
        g <- tail(filtro_g(omega = 1-p["alpha"]-p["beta"]-p["gamma"]/2,
          alpha = p["alpha"], beta = p["beta"], gamma = p["gamma"],
          returns = c((d$ret_energia_100[j]-p["mu"])/sqrt(tau_atual), 0), g0 = g), 1)
        tau_atual <- tau
      }
    }
  }

  # Variancia pontual em t+h; perdas comparadas nas mesmas origens.
  previsoes <- expand_grid(j = origens, horizonte = horizontes, modelo = names(variaveis)) |>
    filter(j + horizonte <= nrow(d)) |>
    mutate(origem = d$date[j], data = d$date[j + horizonte], observado = d$ret_energia_100[j + horizonte]^2) |>
    select(-j) |>
    left_join(bind_rows(novas), by = c("origem", "horizonte", "modelo"))
  perdas <- previsoes |>
    group_by(origem, horizonte) |>
    mutate(comparavel = all(is.finite(previsao) & previsao > 0)) |>
    ungroup() |>
    mutate(MSE = if_else(comparavel, (observado-previsao)^2, NA_real_),
           QLIKE = if_else(comparavel, log(previsao)+observado/previsao, NA_real_))
  precisao <- perdas |>
    group_by(modelo, horizonte) |>
    summarise(n_total = n(), n_validas = sum(comparavel),
              MSE = mean(MSE, na.rm = TRUE), QLIKE = mean(QLIKE, na.rm = TRUE), .groups = "drop")
  print(as.data.frame(precisao), row.names = FALSE, digits = 7)
  falhas <- bind_rows(falhas)
  if (nrow(falhas)) print(falhas) else message("Nenhuma falha de ajuste ou previsao para energia.")
  perdas_expanding <- perdas
  source("scripts_R/Forecast/teste_dm.R", local = TRUE, encoding = "UTF-8")
  list(previsoes = previsoes, perdas = perdas, precisao = precisao, dm = resultados_dm,
       falhas = falhas, modelos = modelos)
})

comparacao_relativa_energia <- energia_lag1$precisao |>
  select(horizonte, modelo, MSE, QLIKE) |>
  pivot_wider(names_from = modelo, values_from = c(MSE, QLIKE)) |>
  transmute(horizonte, MSE_relativo = MSE_gpr / MSE_vr,
            ganho_MSE_pct = 100 * (1 - MSE_relativo),
            QLIKE_relativo = if_else(QLIKE_vr > 0, QLIKE_gpr / QLIKE_vr, NA_real_),
            QLIKE_diferenca = QLIKE_gpr - QLIKE_vr)
print(as.data.frame(comparacao_relativa_energia), row.names = FALSE, digits = 6)

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 185.6513 1.974221
#    gpr         5    2560      2560 221.1660 2.065021
#    gpr        22    2543      2543 232.7439 2.296925
#     vr         1    2564      2564 212.4746 1.972199
#     vr         5    2560      2560 236.1360 2.061667
#    vr        22    2543      2543 232.2569 2.298211


# horizonte perda    n lag_HAC    diferenca         DM  p_value
#         1   MSE 2564      26 -26.82331416 -1.0233210 0.306156
#         1 QLIKE 2564      25   0.00202284  0.3890529 0.697237
#         5   MSE 2560      21 -14.97000140 -1.0111366 0.311951
#         5 QLIKE 2560      20   0.00335464  0.4320089 0.665735
#        22   MSE 2543      29   0.48692030  0.1736112 0.862171
#        22 QLIKE 2543      30  -0.00128599 -0.0836997 0.933295

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     0.873758     12.624242        1.00103      0.00202284
#         5     0.936604      6.339567        1.00163      0.00335464
#        22     1.002096     -0.209647        0.99944     -0.00128599