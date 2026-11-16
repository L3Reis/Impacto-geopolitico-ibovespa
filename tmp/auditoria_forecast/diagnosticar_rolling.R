Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_rolling_mfgarch.R",encoding="UTF-8")
for (expr in codigo) {
  if (is.call(expr) && identical(expr[[1]],as.name("for"))) break
  eval(expr)
}
origem <- as.Date("2015-09-01")
inicio_estimacao <- (origem %m-% years(2)) + 1L
primeiro_mes <- floor_date(inicio_estimacao,"month")
inicio_historico <- primeiro_mes %m-% months(K)
janela <- filter(dados,date>=inicio_historico,date<=origem,
                 year_month<primeiro_mes | date>=inicio_estimacao)
mes_origem <- floor_date(origem,"month")
janela$vr_ibov[janela$year_month==mes_origem] <-
  sum(dados$ret_ibov_100[dados$year_month==mes_origem & dados$date<=origem]^2)
anterior <- readRDS(file.path(pasta,"ajustes","2015-08-31.rds"))$modelos$gpr
gpr <- fit_mfgarch(data=janela,y="ret_ibov_100",x="gpr_disponivel",low.freq="year_month",
                   K=K,gamma=TRUE,weighting="beta.restricted",multi.start=TRUE,
                   control=list(par.start=anterior$par))
saveRDS(gpr,"tmp/auditoria_forecast/gpr_rolling_2015-09-01.rds")
print(gpr$par)
print(gpr$optim$convergence)
print(gpr$par["alpha"]+gpr$par["beta"]+gpr$par["gamma"]/2)
efetivos <- gpr$df.fitted$date[is.finite(gpr$df.fitted$residuals)]
esperado <- janela$date[janela$date>=inicio_estimacao]
print(list(identicos=identical(efetivos,esperado),n_efetivos=length(efetivos),n_esperado=length(esperado),
           inicio_efetivo=head(efetivos,1),inicio_esperado=head(esperado,1),
           dif_efetivos=setdiff(efetivos,esperado),dif_esperado=setdiff(esperado,efetivos)))
