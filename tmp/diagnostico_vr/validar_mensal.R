Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
source("scripts_R/Forecast/funcoes_forecast.R", encoding = "UTF-8")
parse("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
antigo <- readRDS("resultados/forecast/expanding_k24/checkpoint.rds")
cal <- antigo$assinatura$calendario
corte <- antigo$assinatura$corte
iniciais <- readRDS("tmp/diagnostico_vr/modelos_alinhados_iniciais.rds")
pasta <- "resultados/forecast/expanding_mensal_k24"

p4 <- forecast_expansivo(cal, corte, n_previsoes = 4, pasta = pasta,
                         modelos_iniciais = iniciais)
primeiras <- do.call(rbind, antigo$linhas[1:2])
stopifnot(isTRUE(all.equal(p4$var_gpr[1:2], primeiras$var_gpr, tolerance = 1e-7)),
          isTRUE(all.equal(p4$var_vr[1:2], primeiras$var_vr, tolerance = 1e-7)))

# Retoma dentro de setembro, depois atravessa a virada para outubro.
p <- forecast_expansivo(cal, corte, n_previsoes = 30, pasta = pasta)
stopifnot(identical(p4, p[1:4, ]), nrow(p) == 30,
          all(p$divulgacao_gpr <= p$origem), all(p$fechamento_vr <= p$origem),
          all(p$var_gpr > 0 & is.finite(p$var_gpr)),
          all(p$var_vr > 0 & is.finite(p$var_vr)),
          all(diff(p$n_disponiveis) == 1L))
datas <- c(p$origem[1], p$origem[-1][diff(as.integer(format(p$origem, "%Y%m"))) != 0])
stopifnot(identical(p$origem[p$reestimado], datas), length(datas) == 3)
cat("Agendamento mensal, retomada e disponibilidade dos dados: OK.\n")

cp <- readRDS(file.path(pasta, "checkpoint.rds"))
e <- readRDS(file.path(pasta, "estados.rds"))
stopifnot(nrow(e) == 60, length(cp$coeficientes) == 3,
          length(list.files(file.path(pasta, "estimacoes"), pattern = "[.]rds$")) == 3)
for (grupo in split(e, interaction(e$modelo, e$estimacao, drop = TRUE))) {
  for (nome in c("mu", "alpha", "beta", "gamma", "m", "theta", "w2"))
    stopifnot(length(unique(grupo[[nome]])) == 1L)
  a <- grupo[1, ]
  u <- (grupo$retorno_origem - grupo$mu) / sqrt(grupo$tau)
  g_cpp <- get("calculate_g", asNamespace("mfGARCH"))(
    omega = 1 - a$alpha - a$beta - a$gamma / 2,
    alpha = a$alpha, beta = a$beta, gamma = a$gamma,
    returns = c(u, 0), g0 = a$g)
  stopifnot(isTRUE(all.equal(as.numeric(head(g_cpp, -1)), grupo$g)),
            isTRUE(all.equal(as.numeric(tail(g_cpp, -1)), grupo$g_proximo)))
}
for (i in which(!p$reestimado)) {
  stopifnot(p$n_treino[i] == p$n_treino[i - 1],
            all(cp$estados[[i]]$g == cp$estados[[i - 1]]$g_proximo),
            all(cp$estados[[i]]$tau == cp$estados[[i - 1]]$tau_proximo))
}
for (data in datas) {
  fit <- readRDS(file.path(pasta, "estimacoes", paste0(as.Date(data, origin = "1970-01-01"), ".rds")))
  stopifnot(all(vapply(fit$modelos, function(m) m$optim$convergence == 0, logical(1))))
}
cat("Ajustes salvos, parâmetros fixos no mês e recursão diária contra C++: OK.\n")

arquivos <- file.info(list.files(file.path(pasta, "estimacoes"), full.names = TRUE))
h <- prever_horizontes(pasta, c(1, 5, 10, 22, 30))
stopifnot(nrow(h) == nrow(e) * 5,
          all(h$variancia > 0 & is.finite(h$variancia)),
          identical(arquivos, file.info(list.files(file.path(pasta, "estimacoes"), full.names = TRUE))))
for (nome in c("gpr", "vr")) {
  h1 <- h[h$modelo == nome & h$horizonte == 1, ]
  stopifnot(isTRUE(all.equal(h1$variancia, p[[paste0("var_", nome)]])),
            identical(h1$data_alvo, p$data))
}
s <- e[1, ]
g <- s$g_proximo
v <- numeric(30)
for (j in 1:30) {
  v[j] <- s$tau_proximo * g
  # Média das duas inovações simétricas, com variância unitária.
  g <- mean(1 - s$alpha - s$beta - s$gamma / 2 +
              (s$alpha + s$gamma * (c(-1, 1) < 0)) * g + s$beta * g)
}
hh <- h[h$origem == s$origem & h$modelo == s$modelo, ]
stopifnot(isTRUE(all.equal(hh$variancia, v[hh$horizonte])),
          isTRUE(all.equal(hh$variancia_acumulada, cumsum(v)[hh$horizonte])))
stopifnot(identical(p, forecast_expansivo(cal, corte, n_previsoes = 30, pasta = pasta)))
cat("Horizontes 1/5/10/22/30, agregação e reutilização sem reestimar: OK.\n")
print(p[p$reestimado, c("origem", "data", "n_treino", "estimacao", "reestimado")], row.names = FALSE)
