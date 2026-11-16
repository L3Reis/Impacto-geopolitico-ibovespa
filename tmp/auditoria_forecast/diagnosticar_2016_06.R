Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
codigo <- parse("scripts_R/Forecast/forecast_rolling_mfgarch.R", encoding="UTF-8")
for (expr in codigo) {
  if (is.call(expr) && identical(expr[[1]],as.name("for"))) break
  eval(expr)
}
origem <- as.Date("2016-06-30")
inicio_estimacao <- (origem %m-% years(janela_anos)) + 1L
primeiro_mes <- floor_date(inicio_estimacao,"month")
inicio_historico <- primeiro_mes %m-% months(K)
janela <- filter(dados,date>=inicio_historico,date<=origem,
                 year_month<primeiro_mes | date>=inicio_estimacao)
mes_origem <- floor_date(origem,"month")
janela$vr_ibov[janela$year_month==mes_origem] <-
  sum(dados$ret_ibov_100[dados$year_month==mes_origem & dados$date<=origem]^2)
ajuste <- fit_mfgarch(data=janela,y="ret_ibov_100",x="vr_ibov",low.freq="year_month",
                      K=K,gamma=TRUE,weighting="beta.restricted",multi.start=TRUE)
saveRDS(list(janela=janela,ajuste=ajuste),"tmp/auditoria_forecast/vr_2016-06-30_padrao.rds")
print(ajuste$par,digits=12)
print(ajuste$optim)
p <- ajuste$par
print(list(rho=p["alpha"]+p["beta"]+p["gamma"]/2,
           g=tail(ajuste$g,1),tau=tail(ajuste$tau,1),w2=p["w2"],
           periodo_correto=identical(ajuste$df.fitted$date[is.finite(ajuste$df.fitted$residuals)],
                                     janela$date[janela$date>=inicio_estimacao])))
