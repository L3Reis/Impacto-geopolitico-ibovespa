# Expanding mensal; previsões diárias com predict() original do mfGARCH.
library(dplyr)
library(tidyr)
library(readxl)
library(mfGARCH)

data_corte <- as.Date("2015-08-31")
horizontes <- c(1L, 5L, 22L)
K <- 24L
n_origens <- Inf                       # Toda a amostra de teste.

dados <- read_excel("dados/base_mfgarch_completa.xlsx") |>
  drop_na() |>
  mutate(date = as.Date(date), year_month = as.Date(paste0(year_month, "-01")),
         ret_ibov_100 = as.numeric(ret_ibov) * 100) |>
  arrange(date)
mensais <- dados |>
  group_by(year_month) |>
  summarise(gpr = first(log_gpr_global), .groups = "drop") |>
  mutate(gpr_disponivel = lag(gpr))       # Mais o lag 1 do pacote: GPR em lag 2.
dados <- dados |>
  select(date, year_month, ret_ibov_100) |>
  left_join(select(mensais, year_month, gpr_disponivel), by = "year_month") |>
  filter(date >= as.Date("2000-02-02")) |>
  group_by(year_month) |>
  mutate(vr_ibov = sum(ret_ibov_100^2)) |>
  ungroup() |>
  as.data.frame()
inicio <- sort(unique(dados$year_month))[K + 1L]  # K meses para formar as defasagens.

# Ajuste restrito do pacote, com poucos inícios para contornar falhas numéricas.
estimar <- function(w, x, anterior = NULL) {
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
  w <- filter(dados, date <= origem_ajuste)  # Inicio fixo; fim cresce mensalmente.
  mes <- dados$year_month[j0]
  w$vr_ibov[w$year_month == mes] <-
    sum(dados$ret_ibov_100[dados$year_month == mes & dados$date <= origem_ajuste]^2)
  message("Ajuste mensal: ", origem_ajuste)
  for (nome in names(variaveis)) {
    m <- tryCatch(estimar(w, variaveis[[nome]], anteriores[[nome]]), error = identity)
    if (inherits(m, "error")) {
      falhas[[length(falhas) + 1L]] <- data.frame(estimacao = origem_ajuste, modelo = nome,
                                                motivo = conditionMessage(m))
      message("  Falha em ", nome, ": ", conditionMessage(m))
      next
    }
    anteriores[[nome]] <- m$par
    ajustes[[length(ajustes) + 1L]] <- data.frame(estimacao = origem_ajuste, modelo = nome,
                                                tentativa = attr(m, "tentativa"), theta = m$par["theta"],
                                                tau_final = tail(m$tau, 1), tau_proximo = m$tau.forecast)
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

previsoes_expanding <- expand_grid(j = origens, horizonte = horizontes, modelo = names(variaveis)) |>
  filter(j + horizonte <= nrow(dados)) |>
  mutate(origem = dados$date[j], data = dados$date[j + horizonte],
         observado = dados$ret_ibov_100[j + horizonte]^2) |>
  select(-j) |>
  left_join(bind_rows(resultados), by = c("origem", "horizonte", "modelo"))

perdas_expanding <- previsoes_expanding |>
  group_by(origem, horizonte) |>
  mutate(comparavel = all(is.finite(previsao) & previsao > 0)) |>
  ungroup() |>
  mutate(MSE = if_else(comparavel, (observado-previsao)^2, NA_real_),
         QLIKE = if_else(comparavel, log(previsao)+observado/previsao, NA_real_))

precisao_expanding <- perdas_expanding |>
  group_by(modelo, horizonte) |>
  summarise(n_total = n(), n_validas = sum(comparavel),
            MSE = mean(MSE, na.rm = TRUE), QLIKE = mean(QLIKE, na.rm = TRUE), .groups = "drop")
falhas_expanding <- bind_rows(falhas)
ajustes_expanding <- bind_rows(ajustes)
print(as.data.frame(precisao_expanding), row.names = FALSE, digits = 7)

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 70.34209 1.492966
#    gpr         5    2560      2560 86.80758 1.541961
#    gpr        22    2543      2543 95.51417 1.770839
#     vr         1    2564      2564 78.78827 1.489742
#     vr         5    2560      2560 92.76719 1.539393
#     vr        22    2543      2543 95.00567 1.774134

if (nrow(falhas_expanding)) print(falhas_expanding) else message("Nenhuma falha de ajuste ou previsão.")
message("Fim da execução: ", length(origens), " origens; última em ", dados$date[tail(origens, 1)], ".")

# Menor previsão e distribuição do QLIKE
perdas_expanding |>
  filter(comparavel) |>
  group_by(modelo, horizonte) |>
  summarise(
    min_previsao = min(previsao),
    mediana_QLIKE = median(QLIKE),
    max_QLIKE = max(QLIKE),
    .groups = "drop"
  ) |>
  print(n = Inf)

# Cinco maiores QLIKE de cada modelo e horizonte
perdas_expanding |>
  filter(comparavel) |>
  group_by(modelo, horizonte) |>
  slice_max(QLIKE, n = 5, with_ties = FALSE) |>
  ungroup() |>
  select(modelo, horizonte, origem, data, observado, previsao, QLIKE) |>
  arrange(modelo, horizonte, desc(QLIKE)) |>
  print(n = Inf)

save.image("resultados/sessao_forecast.RData")

# Versao 2: GPR do mes anterior a origem, divulgado sempre no dia 01.
# Execute daqui se a primeira versao ja estiver na memoria. O benchmark VR e reaproveitado.
ibov_lag1 <- local({
  library(dplyr)
  library(tidyr)
  K <- 24L
  horizontes <- c(1L, 5L, 22L)
  stopifnot(exists("previsoes_expanding"))
  mensais <- readxl::read_excel("dados/base_mfgarch_completa.xlsx") |>
    transmute(year_month = as.Date(paste0(year_month, "-01")), gpr = as.numeric(log_gpr_global)) |>
    distinct() |> drop_na() |> arrange(year_month) |>
    mutate(gpr_anterior = lag(gpr))
  d <- readxl::read_excel("dados/base_mfgarch_completa.xlsx") |>
    drop_na() |>
    transmute(date = as.Date(date), year_month = as.Date(paste0(year_month, "-01")),
              ret_ibov_100 = as.numeric(ret_ibov) * 100) |>
    drop_na() |> filter(date >= as.Date("2000-02-02")) |> arrange(date) |>
    left_join(mensais, by = "year_month") |> as.data.frame()
  inicio <- sort(unique(d$year_month))[K + 1L]
  origens <- match(sort(unique(previsoes_expanding$origem)), d$date)
  stopifnot(!anyNA(origens), !anyNA(d), !anyDuplicated(d$date),
    all(previsoes_expanding$observado == d$ret_ibov_100[match(previsoes_expanding$data, d$date)]^2))
  grupos <- split(origens, d$year_month[origens + 1L])
  soma_tau <- getFromNamespace("sum_tau_fcts", "mfGARCH")
  filtro_g <- getFromNamespace("calculate_g", "mfGARCH")

  # Funcao local: nao depende de estimar(), K ou inicio restaurados da sessao.
  ajustar <- function(w, anterior) {
    r <- w$ret_ibov_100[w$date >= inicio]
    p0 <- c(mu = mean(r), alpha = .05, beta = .85, gamma = .10,
            m = log(var(r)), theta = 0, w2 = 2)
    p1 <- p0; p1[c("beta", "w2")] <- c(.70, 5)
    inicios <- list(p0, p1, NULL)
    if (!is.null(anterior)) inicios <- append(inicios, list(anterior), after = 1)
    motivo <- "Ajuste fora das restricoes do modelo."
    for (par0 in inicios) {
      z <- tryCatch({
        invisible(capture.output(z <- mfGARCH::fit_mfgarch(
          w, y = "ret_ibov_100", x = "gpr", low.freq = "year_month", K = K,
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

  novas <- list(data.frame(origem = as.Date(character()), horizonte = integer(),
                           modelo = character(), previsao = double(), mes_gpr = as.Date(character())))
  falhas <- modelos <- list()
  anterior <- NULL

  for (dias in grupos) {
    j0 <- dias[1]
    o <- d$date[j0]
    w <- filter(d, date <= o)
    # O GPR do mes corrente ainda nao foi divulgado. Essa ultima entrada nao entra
    # na verossimilhanca, pois o MIDAS ja usa lag 1; repetimos o ultimo valor conhecido.
    w$gpr[w$year_month == d$year_month[j0]] <- w$gpr_anterior[w$year_month == d$year_month[j0]]
    message("Ibovespa; GPR lag 1; ajuste mensal: ", o)
    m <- tryCatch(ajustar(w, anterior), error = identity)
    if (inherits(m, "error")) {
      falhas[[length(falhas) + 1L]] <- data.frame(estimacao = o, motivo = conditionMessage(m))
      message("  Falha: ", conditionMessage(m))
      next
    }
    anterior <- m$par
    modelos[[as.character(o)]] <- m
    p <- m$par
    g <- tail(m$g, 1)
    for (j in dias) {
      disponivel <- mensais$year_month < d$year_month[j]
      tau <- exp(soma_tau(m = p["m"], i = K + 1L, theta = p["theta"],
                         phivar = m$est.weighting,
                         covariate = c(tail(mensais$gpr[disponivel], K), NA_real_), K = K))
      h <- horizontes[j + horizontes <= nrow(d)]
      # Tau conhecido na origem fica fixo em todos os horizontes; predict() original.
      v <- tryCatch(stats::predict(m, horizon = h, fcts.tau = tau, cond.tau = tau,
                           y.last = d$ret_ibov_100[j], cond.var = g),
                    error = function(e) rep(NA_real_, length(h)))
      if (any(!is.finite(v) | v <= 0)) {
        falhas[[length(falhas) + 1L]] <- data.frame(estimacao = o,
                                                  motivo = paste("Previsao invalida em", d$date[j]))
        break
      }
      novas[[length(novas) + 1L]] <- data.frame(
        origem = d$date[j], horizonte = h, modelo = "gpr", previsao = v,
        mes_gpr = tail(mensais$year_month[disponivel], 1))
      g <- tail(filtro_g(omega = 1-p["alpha"]-p["beta"]-p["gamma"]/2,
        alpha = p["alpha"], beta = p["beta"], gamma = p["gamma"],
        returns = c((d$ret_ibov_100[j]-p["mu"])/sqrt(tau), 0), g0 = g), 1)
    }
  }

  previsoes <- previsoes_expanding |>
    select(-previsao) |>
    left_join(bind_rows(
      previsoes_expanding |> filter(modelo == "vr") |> select(origem, horizonte, modelo, previsao),
      bind_rows(novas)), by = c("origem", "horizonte", "modelo"))
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
  if (nrow(falhas)) print(falhas) else message("Nenhuma falha na versao GPR lag 1.")
  perdas_expanding <- perdas
  source("scripts_R/Forecast/teste_dm.R", local = TRUE, encoding = "UTF-8")
  list(previsoes = previsoes, perdas = perdas, precisao = precisao, dm = resultados_dm,
       falhas = falhas, modelos = modelos)
})

# Perdas relativas ao benchmark VR, no mesmo formato dos bens basicos.
comparacao_relativa_ibov <- ibov_lag1$precisao |>
  select(horizonte, modelo, MSE, QLIKE) |>
  pivot_wider(names_from = modelo, values_from = c(MSE, QLIKE)) |>
  transmute(
    horizonte,
    MSE_relativo = MSE_gpr / MSE_vr,
    ganho_MSE_pct = 100 * (1 - MSE_relativo),
    QLIKE_relativo = if_else(QLIKE_vr > 0, QLIKE_gpr / QLIKE_vr, NA_real_),
    QLIKE_diferenca = QLIKE_gpr - QLIKE_vr
  )
print(as.data.frame(comparacao_relativa_ibov), row.names = FALSE, digits = 6)

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 70.38637 1.491816
#    gpr         5    2560      2560 86.88602 1.541604
#    gpr        22    2543      2543 95.45703 1.766147
#     vr         1    2564      2564 78.78827 1.489742
#     vr         5    2560      2560 92.76719 1.539393
#     vr        22    2543      2543 95.00567 1.774134

# horizonte perda    n lag_HAC   diferenca        DM  p_value
#         1   MSE 2564      30 -8.40190095 -1.015342 0.309943
#         1 QLIKE 2564      22  0.00207464  0.488636 0.625099
#         5   MSE 2560      22 -5.88117749 -1.032282 0.301940
#         5 QLIKE 2560      20  0.00221107  0.448641 0.653691
#        22   MSE 2543      26  0.45135473  0.358910 0.719662
#        22 QLIKE 2543      31 -0.00798673 -0.534011 0.593334

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     0.893361     10.663898       1.001393      0.00207464
#         5     0.936603      6.339717       1.001436      0.00221107
#        22     1.004751     -0.475082       0.995498     -0.00798673