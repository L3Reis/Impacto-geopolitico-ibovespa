Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
for (expr in codigo) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("modelo_benchmark_vr"))) break
  eval(expr)
}
cal <- calendario_forecast
fake <- list(K = 24, par = c(m = 0.3, theta = 0.01),
             est.weighting = rep(1 / 24, 24))
mes <- function(s) as.Date(s)
stopifnot(
  tau_na_origem(fake, cal, mes("2015-08-31"), "gpr")$mes == mes("2015-07-01"),
  tau_na_origem(fake, cal, mes("2015-09-01"), "gpr")$mes == mes("2015-08-01"),
  tau_na_origem(fake, cal, mes("2015-08-31"), "vr")$mes == mes("2015-08-01"),
  tau_na_origem(fake, cal, mes("2016-10-03"), "gpr")$mes == mes("2016-09-01")
)
treino <- dados_na_origem(cal, data_corte)
mudado <- cal
mudado$meses$gpr[mudado$meses$divulgacao_gpr > data_corte] <- 999
mudado$meses$vr[mudado$meses$divulgacao_vr > data_corte] <- 999999
stopifnot(identical(treino, dados_na_origem(mudado, data_corte)),
          identical(tau_na_origem(fake, cal, data_corte, "gpr"),
                    tau_na_origem(fake, mudado, data_corte, "gpr")),
          identical(tau_na_origem(fake, cal, data_corte, "vr"),
                    tau_na_origem(fake, mudado, data_corte, "vr")))
setembro <- dados_na_origem(cal, mes("2015-09-02"))
stopifnot(setembro$mes_gpr[setembro$date == mes("2015-09-01")] == mes("2015-08-01"),
          setembro$mes_gpr[setembro$date == mes("2015-09-02")] == mes("2015-09-01"))

sintetico <- list(par = c(mu = 0, alpha = 0.05, beta = 0.8, gamma = 0.2),
                  y = "r", df.fitted = data.frame(r = -1, g = 1, tau = 1))
for (retorno in c(-1, 1)) {
  sintetico$df.fitted$r <- retorno
  g_cpp <- get("calculate_g", asNamespace("mfGARCH"))(
    omega = 0.05, alpha = 0.05, beta = 0.8, gamma = 0.2,
    returns = c(retorno, 0), g0 = 1
  )
  stopifnot(isTRUE(all.equal(prever_1dia(sintetico, 1), tail(g_cpp, 1))))
}
cat("Calendário, informação futura e recursão assimétrica: OK.\n")

iniciais <- list()
for (nome in c("gpr", "vr")) {
  cat("Ajustando", nome, "na origem inicial...\n")
  print(system.time(iniciais[[nome]] <- ajustar_forecast(treino, nome, 24)))
  cat("Convergência:", iniciais[[nome]]$optim$convergence, "\n")
}
stopifnot(isTRUE(all.equal(tau_na_origem(iniciais$gpr, cal, data_corte, "gpr")$tau,
                          as.numeric(tail(iniciais$gpr$tau, 1)))))
saveRDS(iniciais, "tmp/diagnostico_vr/modelos_alinhados_iniciais.rds")

prev <- forecast_expansivo(cal, data_corte, n_previsoes = 3,
                          pasta = "tmp/diagnostico_vr/smoke_forecast",
                          modelos_iniciais = iniciais)
stopifnot(nrow(prev) == 3, all(prev$origem < prev$data),
          all(prev$divulgacao_gpr <= prev$origem), all(prev$fechamento_vr <= prev$origem),
          all(is.finite(prev$var_gpr) & prev$var_gpr > 0),
          all(is.finite(prev$var_vr) & prev$var_vr > 0),
          identical(prev$gpr_referencia, mes(c("2015-07-01", "2015-08-01", "2015-08-01"))),
          all(diff(prev$n_treino) == 1))
retomado <- forecast_expansivo(cal, data_corte, n_previsoes = 3,
                              pasta = "tmp/diagnostico_vr/smoke_forecast")
stopifnot(identical(prev, retomado))
print(prev[, 1:10], row.names = FALSE)
cat("Previsões e retomada: OK.\n")
