# Hipótese do exercício: GPR disponível no fechamento do dia 1 do mês seguinte.
preparar_calendario <- function(base) {
  base <- as.data.frame(base[order(base$date), ])
  stopifnot(!anyDuplicated(base$date), all(is.finite(base$ret_ibov_100)))
  meses <- base |>
    dplyr::group_by(year_month) |>
    dplyr::summarise(
      gpr = dplyr::first(log_gpr_global),
      n_gpr = dplyr::n_distinct(log_gpr_global),
      vr = sum(ret_ibov_100^2), divulgacao_vr = max(date), .groups = "drop"
    ) |>
    as.data.frame()
  stopifnot(all(meses$n_gpr == 1), all(is.finite(meses$gpr)),
            identical(meses$year_month,
                      seq(min(meses$year_month), max(meses$year_month), by = "month")))
  meses$divulgacao_gpr <- as.Date(format(meses$year_month + 32, "%Y-%m-01"))
  diarios <- base[, c("date", "year_month", "ret_ibov_100")]
  diarios$origem <- c(base$date[1] - 1, head(base$date, -1))
  diarios$mes_gpr <- as.Date(format(diarios$origem, "%Y-%m-01"))
  list(diarios = diarios, meses = meses)
}

dados_na_origem <- function(calendario, origem) {
  d <- calendario$diarios[calendario$diarios$date <= origem, ]
  m <- calendario$meses
  gpr_ok <- m$divulgacao_gpr <= origem
  vr_ok <- m$divulgacao_vr <= origem
  if (!any(gpr_ok) || !any(vr_ok)) stop("Histórico publicado insuficiente.")
  # O valor do último bloco não entra no tau estimado (o filtro começa em lag 1).
  # Preenchemos blocos ainda indisponíveis sem consultar seus valores futuros.
  gpr <- rep(tail(m$gpr[gpr_ok], 1), nrow(m))
  vr <- rep(tail(m$vr[vr_ok], 1), nrow(m))
  gpr[gpr_ok] <- m$gpr[gpr_ok]
  vr[vr_ok] <- m$vr[vr_ok]
  d$gpr_info <- gpr[match(d$mes_gpr, m$year_month)]
  d$vr_ibov <- vr[match(d$year_month, m$year_month)]
  stopifnot(all(is.finite(d$gpr_info)), all(is.finite(d$vr_ibov)))
  d
}

ajustar_forecast <- function(dados, modelo, K, inicio = NULL) {
  avisos <- character()
  invisible(capture.output(ajuste <- withCallingHandlers(
    mfGARCH::fit_mfgarch(
      data = dados, y = "ret_ibov_100",
      x = if (modelo == "gpr") "gpr_info" else "vr_ibov",
      low.freq = if (modelo == "gpr") "mes_gpr" else "year_month",
      K = K, gamma = TRUE, weighting = "beta.restricted", multi.start = TRUE,
      control = list(par.start = inicio)
    ), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )))
  valido <- function(x) all(is.finite(tail(x, 1))) && tail(x, 1) > 0
  if (ajuste$optim$convergence != 0 || any(!is.finite(ajuste$par)) ||
      !valido(ajuste$g) || !valido(ajuste$tau)) stop("Ajuste sem convergência válida.")
  ajuste$avisos <- paste(unique(avisos), collapse = " | ")
  ajuste
}

tau_na_origem <- function(ajuste, calendario, origem, modelo) {
  m <- calendario$meses
  publicado <- if (modelo == "gpr") m$divulgacao_gpr <= origem else m$divulgacao_vr <= origem
  m <- m[publicado, ]
  if (nrow(m) < ajuste$K) stop("Menos de K meses publicados.")
  valores <- if (modelo == "gpr") m$gpr else m$vr
  tau <- exp(ajuste$par["m"] + ajuste$par["theta"] *
               sum(ajuste$est.weighting * rev(tail(valores, ajuste$K))))
  list(tau = as.numeric(tau), mes = tail(m$year_month, 1),
       divulgacao = tail(if (modelo == "gpr") m$divulgacao_gpr else m$divulgacao_vr, 1))
}

prever_1dia <- function(ajuste, tau, ultimo = NULL) {
  p <- ajuste$par
  if (is.null(ultimo)) {
    d <- tail(ajuste$df.fitted, 1)
    ultimo <- list(retorno = d[[ajuste$y]], g = d$g, tau = d$tau)
  }
  u <- (ultimo$retorno - p["mu"]) / sqrt(ultimo$tau)
  # Gamma integral no choque negativo; gamma/2 apenas na normalização.
  g <- 1 - p["alpha"] - p["beta"] - p["gamma"] / 2 +
    (p["alpha"] + p["gamma"] * (u < 0)) * u^2 + p["beta"] * ultimo$g
  variancia <- as.numeric(tau * g)
  if (!is.finite(variancia) || variancia <= 0) stop("Previsão de variância inválida.")
  variancia
}

forecast_expansivo <- function(calendario, corte, K = 24, n_previsoes = Inf,
                              pasta = "resultados/forecast/expanding_mensal_k24",
                              modelos_iniciais = NULL, retomar = TRUE) {
  corte <- as.Date(corte)
  d <- calendario$diarios
  alvos <- which(d$date > corte)
  alvos <- head(alvos, min(length(alvos), n_previsoes))
  if (!length(alvos)) stop("Não há datas para previsão.")
  dir.create(file.path(pasta, "estimacoes"), recursive = TRUE, showWarnings = FALSE)
  arquivo <- file.path(pasta, "checkpoint.rds")
  assinatura <- list(calendario = calendario, corte = corte, K = K,
                     versao = "mensal_estados_v2", pacote = packageVersion("mfGARCH"))
  progresso <- list(assinatura = assinatura, linhas = list(), estados = list(),
                    coeficientes = list(), modelos = NULL, ultima_estimacao = as.Date(NA))
  if (retomar && file.exists(arquivo)) {
    progresso <- readRDS(arquivo)
    if (!identical(progresso$assinatura, assinatura))
      stop("Checkpoint de outra configuração; use outra pasta.")
  }
  salvar <- function() {
    if (!length(progresso$linhas)) return(invisible(NULL))
    saveRDS(progresso, arquivo)
    write.csv(do.call(rbind, progresso$linhas), file.path(pasta, "previsoes.csv"), row.names = FALSE)
    estados <- do.call(rbind, progresso$estados)
    saveRDS(estados, file.path(pasta, "estados.rds"))
    write.csv(estados, file.path(pasta, "estados.csv"), row.names = FALSE)
    write.csv(do.call(rbind, progresso$coeficientes),
              file.path(pasta, "parametros_mensais.csv"), row.names = FALSE)
  }
  concluidas <- length(progresso$linhas)
  message(sprintf("Reestimação mensal: %d previsões solicitadas; %d já salvas.",
                  length(alvos), concluidas))
  if (concluidas >= length(alvos)) return(do.call(rbind, progresso$linhas[seq_along(alvos)]))
  on.exit(salvar(), add = TRUE)
  for (i in seq.int(concluidas + 1L, length(alvos))) {
    alvo <- alvos[i]
    origem <- d$origem[alvo]
    modelos <- progresso$modelos
    ultima_estimacao <- progresso$ultima_estimacao
    reestimar <- is.null(modelos) ||
      format(origem, "%Y-%m") != format(ultima_estimacao, "%Y-%m")
    if (reestimar) {
      message(sprintf("[%d/%d] Reestimação em %s", i, length(alvos), origem))
      treino <- dados_na_origem(calendario, origem)
      anteriores <- modelos
      if (i == 1L && !is.null(modelos_iniciais)) {
        modelos <- modelos_iniciais
      } else {
        modelos <- setNames(lapply(c("gpr", "vr"), function(nome) {
          message("  Estimando ", toupper(nome), "...")
          inicio <- if (!is.null(anteriores)) anteriores[[nome]]$par else NULL
          ajuste <- tryCatch(ajustar_forecast(treino, nome, K, inicio), error = identity)
          if (inherits(ajuste, "error") && !is.null(inicio)) {
            message("  Repetindo com valores iniciais padrão...")
            ajuste <- tryCatch(ajustar_forecast(treino, nome, K), error = identity)
          }
          if (inherits(ajuste, "error"))
            stop(sprintf("%s em %s: %s", nome, origem, conditionMessage(ajuste)))
          ajuste
        }), c("gpr", "vr"))
      }
      ultima_estimacao <- origem
    }
    stopifnot(all(vapply(modelos, function(m) m$K == K &&
      tail(m$df.fitted$date, 1) == ultima_estimacao, logical(1))),
      "mes_gpr" %in% names(modelos$gpr$df.fitted))
    passos <- lapply(c("gpr", "vr"), function(nome) {
      ajuste <- modelos[[nome]]
      if (reestimar) {
        ultimo <- tail(ajuste$df.fitted, 1)
        atual <- list(retorno = ultimo[[ajuste$y]], g = ultimo$g, tau = ultimo$tau)
      } else {
        anterior <- progresso$estados[[i - 1L]]
        anterior <- anterior[anterior$modelo == nome, ]
        stopifnot(nrow(anterior) == 1L, anterior$data == origem)
        atual <- list(retorno = d$ret_ibov_100[alvo - 1L],
                      g = anterior$g_proximo, tau = anterior$tau_proximo)
      }
      t <- tau_na_origem(ajuste, calendario, origem, nome)
      g <- prever_1dia(ajuste, 1, atual)
      estado <- data.frame(
        modelo = nome, origem = origem, data = d$date[alvo], estimacao = ultima_estimacao,
        K = K, retorno_origem = atual$retorno, g = atual$g, tau = atual$tau,
        g_proximo = g, tau_proximo = t$tau,
        as.list(ajuste$par), row.names = NULL
      )
      list(estado = estado, t = t, variancia = t$tau * g)
    })
    names(passos) <- c("gpr", "vr")
    tg <- passos$gpr$t
    tv <- passos$vr$t
    vg <- passos$gpr$variancia
    vv <- passos$vr$variancia
    if (any(!is.finite(c(vg, vv))) || any(c(vg, vv) <= 0)) stop("Variância prevista inválida.")
    linha <- data.frame(
      origem = origem, data = d$date[alvo],
      n_treino = nrow(modelos$gpr$df.fitted), n_disponiveis = alvo - 1L,
      estimacao = ultima_estimacao, reestimado = reestimar,
      gpr_referencia = tg$mes, divulgacao_gpr = tg$divulgacao,
      vr_referencia = tv$mes, fechamento_vr = tv$divulgacao,
      var_gpr = vg, var_vr = vv, vol_gpr = sqrt(vg), vol_vr = sqrt(vv),
      tau_gpr = tg$tau, tau_vr = tv$tau,
      mu_gpr = unname(modelos$gpr$par["mu"]), mu_vr = unname(modelos$vr$par["mu"]),
      retorno = d$ret_ibov_100[alvo], retorno_quadrado = d$ret_ibov_100[alvo]^2,
      avisos_gpr = if (is.null(modelos$gpr$avisos)) "" else modelos$gpr$avisos,
      avisos_vr = if (is.null(modelos$vr$avisos)) "" else modelos$vr$avisos
    )
    # Só confirma o progresso depois de concluir ambos os modelos e estados.
    proximo <- progresso
    proximo$linhas[[i]] <- linha
    proximo$estados[[i]] <- rbind(passos$gpr$estado, passos$vr$estado)
    proximo$modelos <- modelos
    proximo$ultima_estimacao <- ultima_estimacao
    if (reestimar) {
      proximo$coeficientes[[length(proximo$coeficientes) + 1L]] <- do.call(rbind,
        lapply(c("gpr", "vr"), function(nome) {
          data.frame(estimacao = origem, modelo = nome, n = nrow(treino), K = K,
                     as.list(modelos[[nome]]$par), row.names = NULL)
        }))
      saveRDS(list(origem = origem, K = K, modelos = modelos),
              file.path(pasta, "estimacoes", paste0(origem, ".rds")))
    }
    progresso <- proximo
    if (reestimar || i %% 20L == 0L || i == length(alvos)) {
      salvar()
      message(sprintf("%d/%d: origem %s; alvo %s", i, length(alvos), origem, d$date[alvo]))
    }
  }
  message("Forecast concluído. Resultados em ", pasta)
  do.call(rbind, progresso$linhas)
}

# Previsões iteradas com tau constante no nível conhecido em cada origem.
# Não estima parâmetros; h é medido em pregões. Inclui soma e média das variâncias.
prever_horizontes <- function(pasta = "resultados/forecast/expanding_mensal_k24",
                             horizontes = c(1, 5, 10, 22, 30)) {
  stopifnot(length(horizontes) > 0, all(is.finite(horizontes)),
            all(horizontes >= 1 & horizontes == as.integer(horizontes)))
  h <- sort(unique(horizontes))
  e <- readRDS(file.path(pasta, "estados.rds"))
  calendario <- readRDS(file.path(pasta, "checkpoint.rds"))$assinatura$calendario$diarios$date
  resultado <- do.call(rbind, lapply(seq_len(nrow(e)), function(i) {
    s <- e[i, ]
    rho <- s$alpha + s$beta + s$gamma / 2
    v <- s$tau_proximo * (1 + rho^(seq_len(max(h)) - 1) * (s$g_proximo - 1))
    if (any(!is.finite(v)) || any(v <= 0)) stop("Trajetória de variância inválida.")
    acumulada <- cumsum(v)[h]
    data.frame(origem = s$origem, modelo = s$modelo, estimacao = s$estimacao,
               horizonte = h, data_alvo = calendario[match(s$origem, calendario) + h],
               variancia = v[h], volatilidade = sqrt(v[h]),
               variancia_acumulada = acumulada, variancia_media = acumulada / h)
  }))
  saveRDS(resultado, file.path(pasta, "previsoes_horizontes.rds"))
  write.csv(resultado, file.path(pasta, "previsoes_horizontes.csv"), row.names = FALSE)
  resultado
}
