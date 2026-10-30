# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)


# Testando GPR Global e suas transformações


base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. 
base_mfgarch_completa <- base_teste_modelo_2 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_bra = as.numeric(gpr_bra),
    gpr_global = as.numeric(gpr_global),
    gpr_global_z = as.numeric(gpr_global_z),
    d_gpr_global = as.numeric(d_gpr_global),
    log_gpr_global = as.numeric(log_gpr_global),
    d_log_gpr_global = as.numeric(d_log_gpr_global),
    ret_ibov_100 = ret_ibov * 100
  ) |>filter(date >= '2000-02-02') |> 
  na.omit()
  


# Conforme a referência de Segnon (2024), foram selecionados como especificações principais os modelos agregados em transformação logarítmica e com K = 12
# Eles são destacados nesse script em especial para melhor localização e para facilitar possíveis diagnósticos futuros

#* Modelos GPR Global assimétrico e simétrico (originais do script teste_garch_midas)

# Modelo assimétrico

# 12 lags

modelo_gpr_global_log_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_gpr_global_log_12$par
modelo_gpr_global_log_12$broom.mgarch
modelo_gpr_global_log_12$optim$convergence
modelo_gpr_global_log_12$variance.ratio
modelo_gpr_global_log_12$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03494588  0.02598677  0.91431982  0.07864421  1.29605982 -0.09431131  4.37428087 


#       term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu  0.03494588 0.017245450 4.272558e-02  0.122649674 7.757021e-01
# alpha alpha  0.02598677 0.006427221 5.271854e-05  0.006970291 1.928379e-04
# beta   beta  0.91431982 0.010709313 0.000000e+00  0.006599816 0.000000e+00
# gamma gamma  0.07864421 0.014535211 6.281396e-08  0.014863643 1.216191e-07
# m         m  1.29605982 1.013012379 2.007521e-01  0.878458834 1.401106e-01
# theta theta -0.09431131 0.217564488 6.646612e-01  0.187645495 6.152430e-01
# w2       w2  4.37428087 1.897310524 2.113766e-02 16.670592255 7.930169e-01

#BIC: 22325.39

#! MODELO CONVERGE

# 18 lags

modelo_gpr_global_log_18 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 18,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)


modelo_gpr_global_log_18$par
modelo_gpr_global_log_18$broom.mgarch
modelo_gpr_global_log_18$optim$convergence
modelo_gpr_global_log_18$variance.ratio
modelo_gpr_global_log_18$bic

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03902487 0.02568512 0.91471257 0.07882581 0.42660317 0.09037345 1.01143547 


#       term   estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.03902487 0.017412205 2.501074e-02 0.116029739 7.366182e-01
# alpha alpha 0.02568512 0.006519931 8.165955e-05 0.006968503 2.279074e-04
# beta   beta 0.91471257 0.010917784 0.000000e+00 0.006649223 0.000000e+00
# gamma gamma 0.07882581 0.015074351 1.703050e-07 0.014480060 5.217073e-08
# m         m 0.42660317 2.238533254 8.488604e-01 1.054958370 6.859339e-01
# theta theta 0.09037345 0.483831795 8.518277e-01 0.228294103 6.922053e-01
# w2       w2 1.01143547 4.608664050 8.262887e-01 6.286767555 8.721854e-01

#BIC: 21821.67

#! MODELO CONVERGE

# 24 lags

modelo_gpr_global_log_24 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)



modelo_gpr_global_log_24$par
modelo_gpr_global_log_24$broom.mgarch
modelo_gpr_global_log_24$optim$convergence
modelo_gpr_global_log_24$variance.ratio
modelo_gpr_global_log_24$bic

# Resultados
#        mu      alpha       beta      gamma          m      theta         w2 
# 0.03967623 0.02560153 0.91379241 0.07894686 0.34697648 0.10327459 2.57473590 


#       term   estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu       mu 0.03967623 0.017446846 2.295876e-02  0.130783207 7.616049e-01
# alpha alpha 0.02560153 0.006586468 1.014954e-04  0.007345852 4.918287e-04
# beta   beta 0.91379241 0.010827197 0.000000e+00  0.006861019 0.000000e+00
# gamma gamma 0.07894686 0.015012925 1.451621e-07  0.016265419 1.212018e-06
# m         m 0.34697648 0.298470766 2.450267e-01  1.197035676 7.719210e-01
# theta theta 0.10327459 0.058521954 7.761163e-02  0.246691334 6.754801e-01
# w2       w2 2.57473590 3.208751849 4.223156e-01 12.546642464 8.374056e-01

#BIC: 21279

#! MODELO CONVERGE

# 36 lags

modelo_gpr_global_log_36 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 36,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)




modelo_gpr_global_log_36$par
modelo_gpr_global_log_36$broom.mgarch
modelo_gpr_global_log_36$optim$convergence
modelo_gpr_global_log_36$variance.ratio
modelo_gpr_global_log_36$bic


#* VARIANCIA CAMBIAL

modelo_log_cambio_12_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch
modelo_log_cambio_12_asi$optim$convergence
modelo_log_cambio_12_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03469713  0.02451824  0.92670322  0.07364268 -1.05595128 -0.28447703  9.01072228 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03469713 0.017128085 4.279091e-02 0.097213520 7.211541e-01
# alpha alpha  0.02451824 0.006173974 7.150413e-05 0.006322360 1.053101e-04
# beta   beta  0.92670322 0.010326013 0.000000e+00 0.005878277 0.000000e+00
# gamma gamma  0.07364268 0.012684641 6.411408e-09 0.011815547 4.584846e-10
# m         m -1.05595128 0.923164384 2.526905e-01 0.723490689 1.444212e-01
# theta theta -0.28447703 0.133038706 3.249218e-02 0.088334951 1.279959e-03
# w2       w2  9.01072228 3.522038017 1.051602e-02 3.994014875 2.406711e-02

#BIC: 22309.39

#! MODELO CONVERGE


modelo_log_cambio_18_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 18,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_18_asi$par
modelo_log_cambio_18_asi$broom.mgarch
modelo_log_cambio_18_asi$optim$convergence
modelo_log_cambio_18_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03706328  0.02508687  0.92723321  0.07353351 -1.32327196 -0.32668400 13.55044442 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03706328 0.017266662 3.183131e-02 0.088558599 6.755692e-01
# alpha alpha  0.02508687 0.006276615 6.418297e-05 0.006394636 8.741234e-05
# beta   beta  0.92723321 0.010224798 0.000000e+00 0.005837643 0.000000e+00
# gamma gamma  0.07353351 0.012493811 3.966246e-09 0.011372535 1.007072e-10
# m         m -1.32327196 1.060982243 2.123190e-01 0.672374684 4.906144e-02
# theta theta -0.32668400 0.158028113 3.871037e-02 0.093376949 4.677939e-04
# w2       w2 13.55044442 6.119162453 2.679935e-02 5.477203888 1.336205e-02

#BIC: 21801.31

#! MODELO CONVERGE


modelo_log_cambio_24_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 24,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

modelo_log_cambio_24_asi$par
modelo_log_cambio_24_asi$broom.mgarch
modelo_log_cambio_24_asi$optim$convergence
modelo_log_cambio_24_asi$bic

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03863316  0.02589676  0.92647957  0.07214614 -1.26393468 -0.31439496 16.42383058 

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03863316 0.017257545 2.518036e-02 0.090478801 6.693897e-01
# alpha alpha  0.02589676 0.006325774 4.242720e-05 0.006623818 9.243304e-05
# beta   beta  0.92647957 0.010466660 0.000000e+00 0.006016234 0.000000e+00
# gamma gamma  0.07214614 0.013002601 2.879773e-08 0.011880883 1.259681e-09
# m         m -1.26393468 1.091164069 2.467269e-01 0.736508593 8.614069e-02
# theta theta -0.31439496 0.161962999 5.223973e-02 0.101566276 1.965039e-03
# w2       w2 16.42383058 6.522656901 1.180346e-02 7.264771820 2.377476e-02

#BIC: 21262.57

#! MODELO CONVERGE

modelos <- list(
  K12 = modelo_log_cambio_12_asi,
  K18 = modelo_log_cambio_18_asi,
  K24 = modelo_log_cambio_24_asi
)

comparacao <- do.call(rbind, lapply(names(modelos), function(nome) {

  mod <- modelos[[nome]]
  datas <- as.Date(mod$df.fitted$date)

  validas <- !is.na(mod$df.fitted$tau)

  data.frame(
    Modelo = nome,
    K = mod$K,
    N_total = nrow(mod$df.fitted),
    N_efetivo = sum(validas),
    Inicio_efetivo = as.character(min(datas[validas])),
    Fim_efetivo = as.character(max(datas[validas])),
    BIC = mod$bic,
    LogLik = mod$llh,
    Convergencia = mod$optim$convergence
  )
}))

print(comparacao)
