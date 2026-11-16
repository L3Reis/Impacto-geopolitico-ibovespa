Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
for (expr in codigo) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("modelo_benchmark_vr"))) break
  eval(expr)
}
pasta <- "resultados/forecast/expanding_mensal_k24"
saida <- file.path(pasta, "auditoria")
dir.create(saida, showWarnings = FALSE)
cp <- readRDS(file.path(pasta, "checkpoint.rds"))
cal <- cp$assinatura$calendario
p <- do.call(rbind, cp$linhas)
e <- readRDS(file.path(pasta, "estados.rds"))
coef <- do.call(rbind, cp$coeficientes)
checks <- list()
registrar <- function(nome, resultado, detalhe = "") {
  checks[[length(checks) + 1L]] <<- data.frame(
    verificacao = nome, passou = isTRUE(resultado), detalhe = detalhe)
  if (!isTRUE(resultado)) message("FALHOU: ", nome)
}
igual <- function(a, b, tolerancia = 1e-10)
  isTRUE(all.equal(a, b, tolerance = tolerancia, check.attributes = FALSE))
registrar("Base atual idêntica à usada no checkpoint", identical(cal, calendario_forecast))
registrar("Amostra de teste idêntica aos alvos", identical(amostra_teste$date, p$data))
registrar("2564 previsões únicas e em ordem", nrow(p) == 2564 &&
            !anyDuplicated(p$data) && all(diff(p$data) > 0))
registrar("Estados completos e iguais ao checkpoint", nrow(e) == 2 * nrow(p) &&
            identical(e, do.call(rbind, cp$estados)))
registrar("Origem é o pregão observado anterior", identical(p$origem,
            cal$diarios$origem[match(p$data, cal$diarios$date)]))
registrar("Parâmetros estimados até a origem", all(p$estimacao <= p$origem & p$origem < p$data))
esperadas <- c(p$origem[1], p$origem[-1][format(p$origem[-1], "%Y-%m") !=
                                           format(head(p$origem, -1), "%Y-%m")])
registrar("125 ajustes por modelo na programação mensal", length(esperadas) == 125 &&
            identical(p$origem[p$reestimado], esperadas) && nrow(coef) == 250)
registrar("Datas informacionais respeitam calendário assumido", all(
  p$divulgacao_gpr <= p$origem & p$fechamento_vr <= p$origem))
registrar("Variâncias previstas finitas e positivas", all(
  is.finite(p$var_gpr) & p$var_gpr > 0 & is.finite(p$var_vr) & p$var_vr > 0))
registrar("Retornos observados corretos e na escala percentual", igual(
  p$retorno, amostra_teste$ret_ibov_100) && igual(p$retorno_quadrado, p$retorno^2))
registrar("Tamanho da janela de estimação correto", all(p$n_treino ==
  match(p$estimacao, cal$diarios$date)) && all(p$n_disponiveis ==
  match(p$origem, cal$diarios$date)))
csv <- read.csv(file.path(pasta, "previsoes.csv"))
registrar("CSV de previsões consistente com checkpoint", all(vapply(names(p), function(n) {
  if (is.numeric(p[[n]])) igual(p[[n]], csv[[n]])
  else {
    lido <- as.character(csv[[n]])
    lido[is.na(lido)] <- ""
    identical(as.character(p[[n]]), lido)
  }
}, logical(1))))
vr_independente <- tapply(cal$diarios$ret_ibov_100^2, cal$diarios$year_month, sum)
registrar("VR mensal é a soma dos retornos percentuais ao quadrado", igual(
  as.numeric(vr_independente[as.character(cal$meses$year_month)]), cal$meses$vr))
base_bruta <- readxl::read_excel("dados/base_mfgarch_completa.xlsx")
raw_validos <- base_bruta |>
  filter(as.Date(date) >= as.Date("2000-02-02"), is.finite(as.numeric(ret_ibov)),
         is.finite(as.numeric(log_gpr_global)))
registrar("Limpeza não remove pregões adicionais das variáveis utilizadas",
          nrow(raw_validos) == nrow(cal$diarios))
cat("Amostras, escala, datas e checkpoint conferidos.\n")

arquivos <- list.files(file.path(pasta, "estimacoes"), pattern = "[.]rds$", full.names = TRUE)
registrar("125 arquivos de estimação preservados", length(arquivos) == 125)
modelos <- list()
ajustes <- list()
calculate_g <- get("calculate_g", asNamespace("mfGARCH"))
for (arq in arquivos) {
  a <- readRDS(arq)
  treino <- dados_na_origem(cal, a$origem)
  for (nome in c("gpr", "vr")) {
    m <- a$modelos[[nome]]
    par <- m$par
    freq <- if (nome == "gpr") "mes_gpr" else "year_month"
    x <- if (nome == "gpr") "gpr_info" else "vr_ibov"
    df <- m$df.fitted
    blocos <- unique(treino[, c(freq, x)])
    tau_bloco <- rep(NA_real_, nrow(blocos))
    for (j in seq.int(m$K + 1L, nrow(blocos)))
      tau_bloco[j] <- exp(par["m"] + par["theta"] *
                           sum(m$est.weighting * rev(blocos[[x]][(j-m$K):(j-1L)])))
    tau <- tau_bloco[match(treino[[freq]], blocos[[freq]])]
    u <- (treino$ret_ibov_100 - par["mu"]) / sqrt(tau)
    g <- c(rep(NA_real_, sum(is.na(u))), calculate_g(
      omega = 1 - par["alpha"] - par["beta"] - par["gamma"]/2,
      alpha = par["alpha"], beta = par["beta"], gamma = par["gamma"],
      returns = u[!is.na(u)], g0 = var(treino$ret_ibov_100)))
    rho <- par["alpha"] + par["beta"] + par["gamma"]/2
    ajustes[[length(ajustes) + 1L]] <- data.frame(
      origem = a$origem, modelo = nome, n = nrow(df), efetivos = sum(is.finite(df$residuals)),
      inicio_efetivo = min(df$date[is.finite(df$residuals)]),
      convergencia = m$optim$convergence, rho = unname(rho),
      parametros_validos = all(is.finite(par)) && par["alpha"] >= 0 && par["beta"] >= 0 &&
        par["alpha"] + par["gamma"] >= 0 && rho < 1 && rho >= 0 && par["w2"] >= 1,
      entradas_corretas = identical(df$date, treino$date) &&
        igual(df$ret_ibov_100, treino$ret_ibov_100) &&
        igual(df[[x]], treino[[x]]) && identical(df[[freq]], treino[[freq]]) &&
        max(df$date) == a$origem,
      tau_correto = igual(df$tau, tau), g_correto = igual(df$g, g),
      estados_validos = all(is.finite(df$g[!is.na(tau)]) & df$g[!is.na(tau)] > 0 &
                             is.finite(tau[!is.na(tau)]) & tau[!is.na(tau)] > 0),
      residuos_corretos = igual(df$residuals, (treino$ret_ibov_100-par["mu"])/sqrt(g*tau)),
      pesos_corretos = igual(m$est.weighting,
                            get("calculate_phi", asNamespace("mfGARCH"))(1, par["w2"], m$K)),
      avisos = if (is.null(m$avisos)) "" else m$avisos
    )
    modelos[[paste(a$origem, nome)]] <- m
  }
}
ajustes <- do.call(rbind, ajustes)
for (n in c("parametros_validos", "entradas_corretas", "tau_correto", "g_correto", "estados_validos",
            "residuos_corretos", "pesos_corretos"))
  registrar(paste("250 ajustes:", n), all(ajustes[[n]]))
registrar("250 otimizações com código zero", all(ajustes$convergencia == 0))
cat("250 ajustes históricos conferidos contra entradas e filtros independentes.\n")

tau_ok <- parametros_ok <- inicio_ok <- informacao_ok <- TRUE
for (i in seq_len(nrow(e))) {
  s <- e[i, ]
  m <- modelos[[paste(s$estimacao, s$modelo)]]
  t <- tau_na_origem(m, cal, s$origem, s$modelo)
  tau_ok <- tau_ok && igual(s$tau_proximo, t$tau)
  parametros_ok <- parametros_ok && igual(unlist(s[names(m$par)]), m$par)
  if (s$origem == s$estimacao) {
    fim <- tail(m$df.fitted, 1)
    inicio_ok <- inicio_ok && igual(s$g, fim$g) && igual(s$tau, fim$tau) &&
      igual(s$retorno_origem, fim$ret_ibov_100)
  }
}
registrar("Tau previsto usa apenas os K meses publicados na origem", tau_ok)
registrar("Coeficientes de cada origem correspondem ao ajuste mensal", parametros_ok)
registrar("Estados nas reestimações reiniciam no filtro do próprio ajuste", inicio_ok)
for (nome in c("gpr", "vr")) {
  s <- e[e$modelo == nome, ]
  refit <- s$origem == s$estimacao
  registrar(paste(nome, "estados atualizados diariamente"), all(
    s$g[-1][!refit[-1]] == head(s$g_proximo, -1)[!refit[-1]]) && all(
      s$tau[-1][!refit[-1]] == head(s$tau_proximo, -1)[!refit[-1]]))
  registrar(paste(nome, "previsão igual a tau vezes g"), igual(
    s$tau_proximo*s$g_proximo, p[[paste0("var_", nome)]]))
}
for (bloco in split(e, interaction(e$modelo, e$estimacao, drop = TRUE))) {
  s <- bloco[1, ]
  u <- (bloco$retorno_origem - bloco$mu)/sqrt(bloco$tau)
  g <- calculate_g(1-s$alpha-s$beta-s$gamma/2, s$alpha, s$beta, s$gamma,
                   returns = c(u, 0), g0 = s$g)
  informacao_ok <- informacao_ok && igual(head(g,-1), bloco$g) &&
    igual(tail(g,-1), bloco$g_proximo)
}
registrar("Recursão diária de todos os blocos coincide com C++ do pacote", informacao_ok)

futuro_ok <- TRUE
for (data in esperadas) {
  origem <- as.Date(data, origin = "1970-01-01")
  mod <- cal
  mod$diarios$ret_ibov_100[mod$diarios$date > origem] <- 99999
  mod$meses$gpr[mod$meses$divulgacao_gpr > origem] <- -999
  mod$meses$vr[mod$meses$divulgacao_vr > origem] <- 99999
  futuro_ok <- futuro_ok && identical(dados_na_origem(cal, origem), dados_na_origem(mod, origem))
  for (nome in c("gpr", "vr")) {
    m <- modelos[[paste(origem, nome)]]
    futuro_ok <- futuro_ok && identical(tau_na_origem(m, cal, origem, nome),
                                        tau_na_origem(m, mod, origem, nome))
  }
}
registrar("Alterar dados futuros não altera entradas nem tau nas 125 reestimações", futuro_ok)
cat("Estados diários e teste de alteração das informações futuras conferidos.\n")

metricas <- list()
for (h in c(1L, 10L, 20L, 30L)) {
  idx <- match(e$origem, cal$diarios$date) + h
  observado <- cal$diarios$ret_ibov_100[idx]^2
  rho <- e$alpha + e$beta + e$gamma/2
  v <- e$tau_proximo*(1+rho^(h-1L)*(e$g_proximo-1))
  for (nome in c("gpr", "vr")) {
    ok <- e$modelo == nome & is.finite(observado)
    metricas[[length(metricas)+1L]] <- data.frame(
      horizonte = h, modelo = nome, n = sum(ok),
      MSE = mean((observado[ok]-v[ok])^2),
      QLIKE = mean(log(v[ok])+observado[ok]/v[ok]))
  }
  registrar(paste("Contagem e datas no horizonte", h), sum(is.finite(observado)) ==
              2*(nrow(p)-h+1L) && all(idx[is.finite(observado)] <= nrow(cal$diarios)))
  if (h == 1L) {
    registrar("h=1 coincide com previsão original de ambos os modelos", igual(
      v[e$modelo == "gpr"], p$var_gpr) && igual(v[e$modelo == "vr"], p$var_vr))
  }
  arquivo <- if (h == 1L) "perdas_oos.csv" else paste0("perdas_h",h,".csv")
  if (file.exists(file.path(pasta, arquivo))) {
    salvo <- read.csv(file.path(pasta, arquivo))
    ch <- paste(e$origem, e$modelo)
    j <- match(paste(salvo$origem, tolower(salvo$modelo)), ch)
    registrar(paste("Perdas salvas corretas para h", h), !anyNA(j) &&
      igual(salvo$MSE, (observado[j]-v[j])^2) &&
      igual(salvo$QLIKE, log(v[j])+observado[j]/v[j]))
  }
}
h_salvo <- readRDS(file.path(pasta, "previsoes_horizontes.rds"))
j <- match(paste(h_salvo$origem, h_salvo$modelo), paste(e$origem,e$modelo))
rho <- e$alpha[j]+e$beta[j]+e$gamma[j]/2
v <- e$tau_proximo[j]*(1+rho^(h_salvo$horizonte-1L)*(e$g_proximo[j]-1))
registrar("Arquivo de horizontes correto e com todas as origens", !anyNA(j) &&
  nrow(h_salvo) == nrow(e)*length(unique(h_salvo$horizonte)) &&
  igual(h_salvo$variancia, v) && identical(h_salvo$data_alvo,
    cal$diarios$date[match(h_salvo$origem,cal$diarios$date)+h_salvo$horizonte]))

metricas <- do.call(rbind,metricas)
checks <- do.call(rbind,checks)
write.csv(checks, file.path(saida,"verificacoes.csv"), row.names=FALSE)
write.csv(ajustes, file.path(saida,"ajustes.csv"), row.names=FALSE)
write.csv(metricas,file.path(saida,"metricas_recalculadas.csv"),row.names=FALSE)
cat("\nVerificações:", sum(checks$passou),"/",nrow(checks),"aprovadas.\n")
print(checks[!checks$passou,],row.names=FALSE)
print(metricas,row.names=FALSE,digits=8)
print(subset(p, origem %in% as.Date(c("2023-01-02","2025-09-01")),
             select=c(origem,data,gpr_referencia,divulgacao_gpr)),row.names=FALSE)
cat("Avisos em ajustes:",sum(nzchar(ajustes$avisos)),"; rho entre",
    range(ajustes$rho),"\n")
stopifnot(all(checks$passou))
