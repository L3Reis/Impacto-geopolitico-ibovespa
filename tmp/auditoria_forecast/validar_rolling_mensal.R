Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_rolling_mfgarch.R",encoding="UTF-8")
executar <- function(n, futuro = NULL) {
  e <- new.env(parent = globalenv())
  e$fit_mfgarch <- function(...) stop("Não deveria reestimar: ajustes de teste já salvos.")
  for (expr in codigo) {
    nome <- if (is.call(expr) && identical(expr[[1]],as.name("<-"))) as.character(expr[[2]])[1] else ""
    if (nome == "n_origens") e$n_origens <- n else eval(expr,e)
    if (nome == "dados" && !is.null(futuro)) {
      e$dados$ret_ibov_100[e$dados$date>futuro] <- 99999
      e$dados$vr_ibov[e$dados$year_month>floor_date(futuro,"month")] <- 99999
      e$dados$gpr_disponivel[e$dados$year_month>floor_date(futuro,"month")] <- -999
    }
  }
  e
}
e <- executar(30L)
p <- e$previsoes_rolling
d <- e$dados
stopifnot(nrow(p)==90L,nrow(e$precisao_rolling)==6L,all(e$precisao_rolling$n==30L),
          identical(unique(p$estimacao),as.Date(c("2015-08-31","2015-09-30"))))
comparar <- function(a,b) isTRUE(all.equal(a,b,tolerance=1e-10,check.attributes=FALSE))
for (data_fit in unique(as.character(p$estimacao))) {
  arq <- file.path(e$pasta,"ajustes",paste0(data_fit,".rds"))
  fit <- readRDS(arq)
  bloco <- p[as.character(p$estimacao)==data_fit,]
  j0 <- match(as.Date(data_fit),d$date)
  dias <- unique(bloco$origem)
  stopifnot(length(unique(bloco$inicio_estimacao))==1,
            length(unique(bloco$n_estimacao))==1,
            all(bloco$reestimado == (bloco$origem==as.Date(data_fit))))
  for (nome in c("gpr","vr")) {
    m <- fit$modelos[[nome]]
    par <- m$par
    g <- as.numeric(tail(m$g,1))
    tau_atual <- as.numeric(tail(m$tau,1))
    tau_mes <- if (d$year_month[j0+1]!=d$year_month[j0]) m$tau.forecast else tail(m$tau,1)
    gs <- numeric(length(dias))
    for (i in seq_along(dias)) {
      j <- match(dias[i],d$date)
      lin <- bloco[bloco$origem==dias[i],]
      v <- predict(m,horizon=lin$horizonte,fcts.tau=tau_mes,
                   y.last=d$ret_ibov_100[j],cond.var=g,cond.tau=tau_atual)
      stopifnot(comparar(lin[[nome]],v),
                identical(lin$data,d$date[j+lin$horizonte]),
                identical(lin$observado,d$ret_ibov_100[j+lin$horizonte]^2))
      gs[i] <- g
      # Verificação independente do filtro C++ usado no script.
      u <- (d$ret_ibov_100[j]-par["mu"])/sqrt(tau_atual)
      g <- as.numeric(1-par["alpha"]-par["beta"]-par["gamma"]/2 +
                        (par["alpha"]+par["gamma"]*(u<0))*u^2+par["beta"]*g)
      tau_atual <- tau_mes
    }
    stopifnot(length(unique(gs))>1L)
  }
}
cat("Parâmetros fixos no mês, g atualizado diariamente e predict() nativo: OK.\n")

origens <- seq.int(which(d$date>e$data_corte)[1]-1L,nrow(d)-1L)
meses_alvo <- d$year_month[origens+1L]
ajustes <- c(TRUE,tail(meses_alvo,-1)!=head(meses_alvo,-1))
stopifnot(sum(ajustes)==124L)
for (j in origens[ajustes]) {
  inicio <- (d$date[j] %m-% years(e$janela_anos))+1L
  mes_inicio <- floor_date(inicio,"month")
  hist <- mes_inicio %m-% months(e$K)
  janela <- d[d$date>=hist & d$date<=d$date[j] &
                (d$year_month<mes_inicio | d$date>=inicio),]
  periodos <- unique(janela$year_month)
  stopifnot(identical(janela$date[match(janela$year_month,periodos)>e$K],
                      janela$date[janela$date>=inicio]),min(periodos)==hist)
}
contagens <- sapply(e$horizontes,function(h)sum(origens+h<=nrow(d)))
stopifnot(identical(as.integer(contagens),c(2564L,2560L,2543L)))
cat("Agenda completa: 124 ajustes por modelo e contagens 2564/2560/2543: OK.\n")

limite <- as.Date("2015-09-15")
n <- sum(e$dados$date[e$origens] <= limite)
e_futuro <- executar(n,futuro=limite)
antes <- p[p$origem<=limite,]
depois <- e_futuro$previsoes_rolling
stopifnot(comparar(antes$gpr,depois$gpr),comparar(antes$vr,depois$vr),
          identical(antes$origem,depois$origem))
cat("Alterar dados posteriores à origem não altera as previsões: OK.\n")

# Restaura os CSVs da execução de 30 origens, após a execução alterada de teste.
write.csv(e$previsoes_rolling,file.path(e$pasta,"previsoes.csv"),row.names=FALSE)
write.csv(e$perdas_rolling,file.path(e$pasta,"perdas.csv"),row.names=FALSE)
write.csv(e$precisao_rolling,file.path(e$pasta,"precisao.csv"),row.names=FALSE)
