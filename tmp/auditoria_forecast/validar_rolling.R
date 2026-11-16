Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_rolling_mfgarch.R",encoding="UTF-8")
bloco_loop <- which(vapply(codigo, function(e) is.call(e) && identical(e[[1]],as.name("for")), logical(1)))
for (expr in codigo[seq_len(bloco_loop-1L)]) eval(expr)
corpo <- as.list(codigo[[bloco_loop]][[4]])[-1]
mensagem <- which(vapply(corpo,function(e) is.call(e) && identical(e[[1]],as.name("message")),logical(1)))[1]
preparar <- corpo[seq_len(mensagem-1L)]
ns <- integer(length(origens))
for (i in seq_along(origens)) {
  for (expr in preparar) eval(expr)
  meses <- unique(janela$year_month)
  nativo <- janela$date[match(janela$year_month,meses)>K]
  esperado <- janela$date[janela$date >= inicio_estimacao]
  stopifnot(identical(nativo,esperado), length(esperado)>400,
            max(janela$date)==origem, all(janela$date<=origem),
            min(janela$year_month)==inicio_historico,
            min(esperado)>=inicio_estimacao,
            janela$vr_ibov[nrow(janela)] == sum(
              dados$ret_ibov_100[dados$year_month==mes_origem & dados$date<=origem]^2))
  ns[i] <- length(esperado)
}
cat("Todas as",length(origens),"janelas têm dois anos efetivos e K meses anteriores; n entre",range(ns),".\n")
contagens <- sapply(horizontes,function(h)sum(origens+h<=nrow(dados)))
stopifnot(identical(as.integer(contagens),c(2564L,2560L,2543L)))
print(data.frame(horizonte=horizontes,n_por_modelo=contagens),row.names=FALSE)

# Mesmo preenchimento do mês corrente depois de alterar todos os retornos futuros.
dados_originais <- dados
for (i in c(1L,3L,which(dados$date[origens]==as.Date("2024-02-29")))) {
  for (expr in preparar) eval(expr)
  antes <- janela
  dados$ret_ibov_100[dados$date>origem] <- 99999
  dados$vr_ibov[dados$year_month>=mes_origem] <- 99999
  for (expr in preparar) eval(expr)
  stopifnot(identical(antes,janela))
  dados <- dados_originais
}
cat("Alterar retornos e VR futuros não altera a janela informacional.\n")

# Reexecuta o script nas três origens que já têm ajustes reais. Bloqueia novas
# estimações para comprovar a retomada; predict() continua sendo o do mfGARCH.
fit_mfgarch <- function(...) stop("A retomada não deveria reestimar.")
for (expr in codigo) {
  nome <- if (is.call(expr) && identical(expr[[1]],as.name("<-"))) as.character(expr[[2]])[1] else ""
  if (nome=="n_origens") n_origens <- 3L else eval(expr)
}
rm(fit_mfgarch)
stopifnot(nrow(previsoes_rolling)==9L,nrow(perdas_rolling)==18L,
          all(precisao_rolling$n==3L))
for (origem_chr in unique(as.character(previsoes_rolling$origem))) {
  s <- readRDS(file.path(pasta,"ajustes",paste0(origem_chr,".rds")))
  linhas <- previsoes_rolling[as.character(previsoes_rolling$origem)==origem_chr,]
  idx <- match(as.Date(origem_chr),dados$date)
  muda <- dados$year_month[idx+1L] != dados$year_month[idx]
  for (nome in c("gpr","vr")) {
    m <- s$modelos[[nome]]
    tau <- if (muda) m$tau.forecast else tail(m$tau,1)
    stopifnot(isTRUE(all.equal(linhas[[nome]],predict(m,horizon=linhas$horizonte,fcts.tau=tau))),
              identical(linhas$data,dados$date[idx+linhas$horizonte]),
              identical(linhas$observado,dados$ret_ibov_100[idx+linhas$horizonte]^2),
              m$optim$convergence==0)
    if (nome=="gpr") {
      # Variável mensal com uma defasagem + lag 1 do pacote = GPR em lag 2.
      tabela <- unique(s$janela[c("year_month","gpr_disponivel")])
      ref <- tabela$year_month %m-% months(1)
      stopifnot(identical(tabela$gpr_disponivel,mensais$gpr[match(ref,mensais$year_month)]))
    }
  }
}
cat("Seis ajustes reais, datas, predict() nativo e retomada sem reestimar: OK.\n")
