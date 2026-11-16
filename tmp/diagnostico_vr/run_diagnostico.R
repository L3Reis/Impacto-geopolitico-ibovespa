Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
saida <- file.path("resultados", "forecast", "diagnostico_vr_treino")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)

# Execução independente da sessão interativa; preserva os objetos do IDE.
png(file.path(saida, "diagnostico_vr_%02d.png"),
    width = 1600, height = 1100, res = 140)
source("scripts_R/Forecast/forecast_gpr.R", encoding = "UTF-8")
graphics.off()

write.csv(testes_residuos_vr, file.path(saida, "testes_residuos_vr.csv"),
          row.names = FALSE)
write.csv(resumo_residuos_vr, file.path(saida, "resumo_residuos_vr.csv"),
          row.names = FALSE)
saveRDS(modelo_benchmark_vr, file.path(saida, "modelo_benchmark_vr_treino.rds"))

cat("\nParâmetros reproduzidos:\n")
print(modelo_benchmark_vr$par)
cat("\nConvergência:", modelo_benchmark_vr$optim$convergence, "\n")
cat("\nComponentes na amostra efetiva:\n")
print(vapply(list(g = modelo_benchmark_vr$g, tau = modelo_benchmark_vr$tau),
             function(x) all(is.finite(x[!is.na(x)]) & x[!is.na(x)] > 0),
             logical(1)))
cat("\nResultados salvos em:", normalizePath(saida), "\n")
