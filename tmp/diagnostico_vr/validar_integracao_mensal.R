Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
options(forecast.n_previsoes = 30)
iniciais <- readRDS("tmp/diagnostico_vr/modelos_alinhados_iniciais.rds")
png("tmp/diagnostico_vr/componentes_mensal_%02d.png", width = 1400, height = 900)
codigo <- parse("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
for (expr in codigo) {
  nome <- if (is.call(expr) && identical(expr[[1]], as.name("<-"))) as.character(expr[[2]])[1] else ""
  if (nome == "modelo_benchmark_vr") modelo_benchmark_vr <- iniciais$vr
  else if (nome == "modelo_gpr_treino") modelo_gpr_treino <- iniciais$gpr
  else eval(expr)
}
graphics.off()
stopifnot(nrow(previsoes_oos) == 30)
origens <- calendario_forecast$diarios$origem[calendario_forecast$diarios$date > data_corte]
cat("Integração do script: OK. Origens:", length(origens),
    "; ajustes mensais por modelo:", 1 + sum(diff(as.integer(format(origens, "%Y%m"))) != 0), "\n")
