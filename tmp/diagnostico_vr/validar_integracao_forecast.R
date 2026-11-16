Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
options(forecast.n_previsoes = 3)
iniciais <- readRDS("tmp/diagnostico_vr/modelos_alinhados_iniciais.rds")
png("tmp/diagnostico_vr/componentes_alinhados_%02d.png", width = 1400, height = 900)
codigo <- parse("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
for (expr in codigo) {
  nome <- if (is.call(expr) && identical(expr[[1]], as.name("<-"))) as.character(expr[[2]])[1] else ""
  if (nome == "modelo_benchmark_vr") modelo_benchmark_vr <- iniciais$vr
  else if (nome == "modelo_gpr_treino") modelo_gpr_treino <- iniciais$gpr
  else eval(expr)
}
graphics.off()
stopifnot(nrow(previsoes_oos) == 3)

cal <- calendario_forecast
origem <- as.Date("2016-09-30")
i <- match(as.Date("2016-10-03"), cal$diarios$date)
stopifnot(cal$diarios$origem[i] == origem,
          cal$diarios$mes_gpr[i] == as.Date("2016-09-01"))
fake <- list(K = 24, par = c(m = 0.3, theta = 0.01), est.weighting = rep(1 / 24, 24))
stopifnot(tau_na_origem(fake, cal, origem, "gpr")$mes == as.Date("2016-08-01"),
          tau_na_origem(fake, cal, origem, "vr")$mes == as.Date("2016-09-01"))

erro <- tryCatch(forecast_expansivo(cal, data_corte, K = 12, n_previsoes = 3), error = identity)
stopifnot(inherits(erro, "error"))
prev4 <- forecast_expansivo(cal, data_corte, n_previsoes = 4)
stopifnot(nrow(prev4) == 4, identical(prev4[1:3, ], previsoes_oos),
          tail(prev4$n_treino, 1) == 3859,
          tail(prev4$data, 1) == as.Date("2015-09-04"))
cat("Integração, fim de semana, proteção do checkpoint e retomada para nova origem: OK.\n")
print(prev4[, 1:10], row.names = FALSE)
