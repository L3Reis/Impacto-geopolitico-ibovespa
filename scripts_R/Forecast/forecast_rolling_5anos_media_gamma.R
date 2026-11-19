# Variancia media futura: rolling de cinco anos efetivos, reestimacao mensal e origens diarias.
library(dplyr)
library(tidyr)

prever_rolling_5anos <- function(serie = c("ibov", "basicos", "consumo", "energia", "financeiro"),
                                 data_corte = as.Date("2015-08-31"), n_origens = Inf) {
  serie <- match.arg(serie)
  stopifnot(length(n_origens) == 1L, n_origens >= 1, n_origens == floor(n_origens))
  K <- 24L
  meses_efetivos <- 120L
  horizontes <- c(1L, 5L, 22L)

  # Copia local do predict(): troca apenas a recursao, sem alterar o pacote.
  filtro_g <- getFromNamespace("calculate_g", "mfGARCH")
  predict_gamma <- getFromNamespace("predict.mfGARCH", "mfGARCH")
  environment(predict_gamma) <- list2env(list(forecast_garch =
    function(omega, alpha, beta, gamma, g, ret, steps.ahead) {
      # Choque observado: gamma integral, como na estimacao. Em E[g], permanece gamma/2.
      g1 <- tail(filtro_g(omega, alpha, beta, gamma,
                         returns = c(as.numeric(unlist(ret)), 0), g0 = g), 1)
      rho <- alpha + beta + gamma/2
      media_g <- omega / (1-rho)
      media_g + rho^(steps.ahead-1) * (g1-media_g)
    }), parent = environment(predict_gamma))

  base <- readxl::read_excel("dados/base_mfgarch_completa.xlsx")
  mensais <- base |>
    transmute(year_month = as.Date(paste0(year_month, "-01")), gpr = as.numeric(log_gpr_global)) |>
    distinct() |> drop_na() |> arrange(year_month) |> mutate(gpr_anterior = lag(gpr))

  colunas <- c(ibov = "ret_ibov", basicos = "basic_products_log_ret", consumo = "consumer_log_ret",
               energia = "energy_log_ret", financeiro = "finance_log_ret")
  if (serie == "ibov") {
    base <- drop_na(base)   # Mesma amostra do Ibovespa usada na expanding.
  } else {
    base <- readxl::read_excel("dados/base_setores_final.xlsx")
  }
  escala <- if (serie == "ibov") 100 else 1
  d <- base |>
    transmute(date = as.Date(date), year_month = as.Date(paste0(year_month, "-01")),
              retorno = as.numeric(.data[[colunas[[serie]]]]) * escala) |>
    drop_na() |> filter(date >= as.Date("2000-02-02")) |> arrange(date) |>
    group_by(year_month) |> mutate(vr = sum(retorno^2)) |> ungroup() |>
    left_join(mensais, by = "year_month") |> as.data.frame()
  meses <- unique(d$year_month)
  stopifnot(!anyNA(d), !anyDuplicated(d$date),
            identical(meses, seq(min(meses), max(meses), by = "month")))

  ajustar <- function(w, x, anterior, inicio) {
    r <- w$retorno[w$date >= inicio]
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
          w, y = "retorno", x = x, low.freq = "year_month", K = K,
          gamma = TRUE, weighting = "beta.restricted", multi.start = FALSE,
          control = list(par.start = par0))))
        z
      }, error = identity)
      if (inherits(z, "error")) { motivo <- conditionMessage(z); next }
      p <- z$par
      rho <- p["alpha"] + p["beta"] + p["gamma"]/2
      if (isTRUE(z$optim$convergence == 0) && all(is.finite(p)) &&
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

  j_corte <- match(data_corte, d$date)
  stopifnot(!is.na(j_corte), j_corte < nrow(d), d$year_month[j_corte] != d$year_month[j_corte + 1L],
            match(d$year_month[j_corte], meses) >= K + meses_efetivos)
  origens <- seq.int(j_corte, nrow(d) - 1L)
  origens <- head(origens, min(length(origens), n_origens))
  grupos <- split(origens, d$year_month[origens + 1L])
  variaveis <- c(gpr = "gpr", vr = "vr")
  anteriores <- list(gpr = NULL, vr = NULL)
  modelos <- list(gpr = list(), vr = list())
  janelas <- list()
  falhas <- list()
  novas <- list(data.frame(origem = as.Date(character()), horizonte = integer(),
                           modelo = character(), previsao = double(), previsao_original = double(),
                           mes_gpr = as.Date(character())))
  soma_tau <- getFromNamespace("sum_tau_fcts", "mfGARCH")

  for (dias in grupos) {
    j0 <- dias[1]
    o <- d$date[j0]
    mes <- d$year_month[j0]
    fim <- match(mes, meses)
    inicio <- meses[fim - meses_efetivos + 1L]
    historico <- meses[fim - meses_efetivos - K + 1L]
    w <- filter(d, year_month >= historico, date <= o)
    stopifnot(n_distinct(w$year_month) == meses_efetivos + K)
    janelas[[length(janelas) + 1L]] <- tibble(estimacao = o, inicio_historico = historico,
      inicio_efetivo = inicio, n_retornos = sum(w$date >= inicio))
    # A entrada GPR do mes corrente nao entra na verossimilhanca (MIDAS lag 1).
    w$gpr[w$year_month == mes] <- w$gpr_anterior[w$year_month == mes]
    w$vr[w$year_month == mes] <- sum(d$retorno[d$year_month == mes & d$date <= o]^2)
    message(serie, " | rolling 5 anos | ajuste mensal: ", o)
    for (nome in names(variaveis)) {
      m <- tryCatch(ajustar(w, variaveis[[nome]], anteriores[[nome]], inicio), error = identity)
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
        passos <- seq_len(max(h))
        v_dias <- tryCatch(predict_gamma(m, horizon = passos, fcts.tau = tau, cond.tau = tau_atual,
                                    y.last = d$retorno[j], cond.var = g),
                      error = function(e) rep(NA_real_, length(passos)))
        if (any(!is.finite(v_dias) | v_dias <= 0)) {
          falhas[[length(falhas) + 1L]] <- data.frame(estimacao = o, modelo = nome,
                                                    motivo = paste("Previsao invalida em", d$date[j]))
          break
        }
        v <- vapply(h, function(k) mean(v_dias[seq_len(k)]), numeric(1))
        # Referencia nativa nos mesmos ajustes e estados: isola o efeito da correcao.
        v0_dias <- tryCatch(stats::predict(m, horizon = passos, fcts.tau = tau, cond.tau = tau_atual,
                                     y.last = d$retorno[j], cond.var = g),
                       error = function(e) rep(NA_real_, length(passos)))
        v0 <- vapply(h, function(k) mean(v0_dias[seq_len(k)]), numeric(1))
        novas[[length(novas) + 1L]] <- data.frame(
          origem = d$date[j], horizonte = h, modelo = nome, previsao = v,
          previsao_original = v0, mes_gpr = mes_gpr)
        g <- tail(filtro_g(omega = 1-p["alpha"]-p["beta"]-p["gamma"]/2,
          alpha = p["alpha"], beta = p["beta"], gamma = p["gamma"],
          returns = c((d$retorno[j]-p["mu"])/sqrt(tau_atual), 0), g0 = g), 1)
        tau_atual <- tau
      }
    }
  }

  # Alvo: media de r^2 entre t+1 e t+h. data marca o ultimo pregao do intervalo.
  # Primeiro agrega previsoes e observados; depois calcula MSE e QLIKE.
  previsoes <- expand_grid(j = origens, horizonte = horizontes, modelo = names(variaveis)) |>
    filter(j + horizonte <= nrow(d)) |>
    mutate(origem = d$date[j], inicio_alvo = d$date[j + 1L], data = d$date[j + horizonte],
           observado = mapply(function(i, k) mean(d$retorno[i + seq_len(k)]^2), j, horizonte)) |>
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
  if (nrow(falhas)) print(falhas) else message("Nenhuma falha de ajuste ou previsao para ", serie, ".")
  relativas <- precisao |>
    select(horizonte, modelo, MSE, QLIKE) |>
    pivot_wider(names_from = modelo, values_from = c(MSE, QLIKE)) |>
    transmute(horizonte, MSE_relativo = MSE_gpr / MSE_vr, ganho_MSE_pct = 100 * (1 - MSE_relativo),
              QLIKE_relativo = if_else(QLIKE_vr > 0, QLIKE_gpr / QLIKE_vr, NA_real_),
              QLIKE_diferenca = QLIKE_gpr - QLIKE_vr)
  print(as.data.frame(relativas), row.names = FALSE, digits = 6)

  source("scripts_R/Forecast/dm_hac.R", local = TRUE, encoding = "UTF-8")
  dm <- perdas |>
    select(origem, horizonte, modelo, MSE, QLIKE) |>
    pivot_longer(c(MSE, QLIKE), names_to = "perda", values_to = "valor") |>
    pivot_wider(names_from = modelo, values_from = valor) |>
    arrange(origem) |> group_by(horizonte, perda) |>
    group_modify(function(.x, .y) {
      d <- .x$gpr - .x$vr
      tryCatch(testar_dm(d, .y$horizonte), error = function(e) {
        message("DM indisponivel para ", .y$perda, " h=", .y$horizonte, ": ", conditionMessage(e))
        tibble(n = sum(!is.na(d)), lag_HAC = NA_integer_, diferenca = mean(d, na.rm = TRUE),
               DM = NA_real_, p_value = NA_real_)
      })
    }) |> ungroup()
  print(as.data.frame(dm), row.names = FALSE, digits = 6)
  list(serie = serie, previsoes = previsoes, perdas = perdas, precisao = precisao, relativas = relativas, dm = dm,
       falhas = falhas, modelos = modelos, janelas = bind_rows(janelas))
}

# Execute a partir da raiz do projeto. Para um setor, troque "ibov" por seu nome acima.
rolling_5anos <- prever_rolling_5anos("basicos")
 
# Ibovespa -  5 anos

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 68.75173 1.500428
#    gpr         5    2560      2560 40.87389 1.520627
#    gpr        22    2543      2543 25.69024 1.668853
#     vr         1    2564      2564 69.32066 1.495586
#     vr         5    2560      2560 41.16907 1.515602
#     vr        22    2543      2543 25.79241 1.661706

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     0.991793      0.820716        1.00324      0.00484181
#         5     0.992830      0.717012        1.00332      0.00502502
#        22     0.996039      0.396122        1.00430      0.00714687

# horizonte perda    n lag_HAC   diferenca        DM  p_value
#         1   MSE 2564      19 -0.56892576 -0.846833 0.397088
#         1 QLIKE 2564       2  0.00484181  0.715972 0.474008
#         5   MSE 2560      26 -0.29518724 -0.825579 0.409043
#         5 QLIKE 2560      19  0.00502502  0.662981 0.507343
#        22   MSE 2543      21 -0.10216936 -0.718821 0.472251
#        22 QLIKE 2543      35  0.00714687  0.627641 0.530239

# Bens básicos - 5 anos

# modelo horizonte n_total n_validas       MSE    QLIKE
#    gpr         1    2564      2564 138.23831 2.114993
#    gpr         5    2560      2560  68.34411 2.137910
#    gpr        22    2543      2543  33.98821 2.204722
#     vr         1    2564      2564 136.85733 2.106431
#     vr         5    2560      2560  68.37700 2.135474
#     vr        22    2543      2543  37.13043 2.204176

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     1.010091     -1.009063        1.00406     0.008562303
#         5     0.999519      0.048102        1.00114     0.002435819
#        22     0.915373      8.462669        1.00025     0.000545665

# horizonte perda    n lag_HAC    diferenca         DM  p_value
#         1   MSE 2564      17  1.380977178  0.5827536 0.560059
#         1 QLIKE 2564      13  0.008562303  0.8746776 0.381749
#         5   MSE 2560      27 -0.032890713 -0.0152567 0.987827
#         5 QLIKE 2560      23  0.002435819  0.2213609 0.824811
#        22   MSE 2543      38 -3.142225841 -1.0110157 0.312009
#        22 QLIKE 2543      37  0.000545665  0.0323922 0.974159

# Execute a partir da raiz do projeto. Para um setor, troque "ibov" por seu nome acima.
rolling_10anos <- prever_rolling_5anos("consumo")

# Ibovespa - 10 anos

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 67.88539 1.484520
#    gpr         5    2560      2560 40.47650 1.507519
#    gpr        22    2543      2543 26.26409 1.640697
#     vr         1    2564      2564 67.94832 1.484387
#     vr         5    2560      2560 40.53445 1.507407
#     vr        22    2543      2543 26.65226 1.638089

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     0.999074     0.0926158        1.00009     0.000132936
#         5     0.998570     0.1429644        1.00007     0.000111455
#        22     0.985436     1.4564309        1.00159     0.002608245

# horizonte perda    n lag_HAC    diferenca         DM  p_value
#         1   MSE 2564       9 -0.062930908 -0.1884604 0.850516
#         1 QLIKE 2564      19  0.000132936  0.0330237 0.973656
#         5   MSE 2560       6 -0.057949849 -0.1469296 0.883188
#         5 QLIKE 2560      27  0.000111455  0.0226243 0.981950
#        22   MSE 2543      25 -0.388171800 -0.6797541 0.496660
#        22 QLIKE 2543      37  0.002608245  0.4040950 0.686143



# básicos - 10 anos

# modelo horizonte n_total n_validas       MSE    QLIKE
#    gpr         1    2564      2564 132.58841 2.090156
#    gpr         5    2560      2560  65.76763 2.111145
#    gpr        22    2543      2543  34.02951 2.179734
#     vr         1    2564      2564 134.08823 2.111249
#     vr         5    2560      2560  67.29165 2.136218
#     vr        22    2543      2543  37.77620 2.204364

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     0.988815       1.11853       0.990009      -0.0210927
#         5     0.977352       2.26479       0.988263      -0.0250729
#        22     0.900819       9.91813       0.988827      -0.0246297

# horizonte perda    n lag_HAC  diferenca        DM    p_value
#         1   MSE 2564      24 -1.4998181 -0.797066 0.42541284
#         1 QLIKE 2564      24 -0.0210927 -2.358098 0.01836886
#         5   MSE 2560      24 -1.5240179 -0.875732 0.38117585
#         5 QLIKE 2560      12 -0.0250729 -2.708749 0.00675375

# consumo - 10 anos

# modelo horizonte n_total n_validas      MSE    QLIKE
#    gpr         1    2564      2564 56.05368 1.356797
#    gpr         5    2560      2560 34.03044 1.384175
#    gpr        22    2543      2543 17.98496 1.536420
#     vr         1    2564      2564 55.84754 1.356560
#     vr         5    2560      2560 34.09207 1.382734
#     vr        22    2543      2543 18.37030 1.529791

# horizonte MSE_relativo ganho_MSE_pct QLIKE_relativo QLIKE_diferenca
#         1     1.003691     -0.369106        1.00017     0.000236167
#         5     0.998192      0.180773        1.00104     0.001441409
#        22     0.979024      2.097599        1.00433     0.006629161

# horizonte perda    n lag_HAC    diferenca         DM  p_value
#         1   MSE 2564     102  0.206136598  0.9196550 0.357753
#         1 QLIKE 2564       7  0.000236167  0.0893861 0.928775
#         5   MSE 2560      26 -0.061629144 -0.2681360 0.788595
#         5 QLIKE 2560      16  0.001441409  0.4455900 0.655893
#        22   MSE 2543      25 -0.385335261 -0.8551643 0.392460
#        22 QLIKE 2543      34  0.006629161  1.2881572 0.197691