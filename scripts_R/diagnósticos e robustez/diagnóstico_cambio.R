#* Testes para diagnóstico do sinal negativo para o theta do câmbio


# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)
library(tidyr)
library(lmtest)
library(sandwich)


# Adaptando para o modelo


base_teste_modelo_2 <- read_excel(
  "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_mfgarch_completa.xlsx"
) |> drop_na()

# Preparando dados para o Pacote mfGARCH. Ele recomenda multiplicar os log-retornos por 100
base_mfgarch_completa <- base_teste_modelo_2 %>%
  mutate(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov = as.numeric(ret_ibov),
    gpr_global = as.numeric(gpr_global),
    d_gpr_global = as.numeric(d_gpr_global),
    log_gpr_global = as.numeric(log_gpr_global),
    d_log_gpr_global = as.numeric(d_log_gpr_global),
    ret_ibov_100 = ret_ibov * 100,
    var_cambio = as.numeric(var_cambio),
    d_var_cambio = as.numeric(d_var_cambio),
    log_var_cambio = as.numeric(log_var_cambio),
    d_log_var_cambio = as.numeric(d_log_var_cambio)
  ) %>% select(
    date,
    year_month,
    gpr_global,
    d_gpr_global,
    log_gpr_global,
    d_log_gpr_global,
    ret_ibov_100,
    var_cambio,
    d_var_cambio,
    log_var_cambio,
    d_log_var_cambio
  ) |> na.omit()


#TODO Cheque: (i) se o mfgarch está convergindo bem e se o parâmetro w do polinômio Beta não está colado no limite
#TODO Testaremos primeiramente com a especificação principal em log e K = 12

modelo_log_cambio_12_asi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03223207  0.02329109  0.92747000  0.07590670 -1.03933539 -0.28778132  9.88877991
# p-value: 4.103421e-02
modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03223207  0.01692771 5.689697e-02 0.088094032 7.144527e-01
# alpha alpha  0.02329109  0.00608565 1.296018e-04 0.006057307 1.204922e-04
# beta   beta  0.92747000  0.01025134 0.000000e+00 0.005620132 0.000000e+00
# gamma gamma  0.07590670  0.01256616 1.535780e-09 0.011142496 9.600987e-12
# m         m -1.03933539  0.97502992 2.864452e-01 0.654396681 1.122333e-01
# theta theta -0.28778132  0.14084943 4.103421e-02 0.081629355 4.227423e-04
# w2       w2  9.88877991  5.35229444 6.466346e-02 4.094176948 1.572105e-02

#* Olhando a convergência
modelo_log_cambio_12_asi$optim$convergence
modelo_log_cambio_12_asi$optim$message

#! modelo não converge


#* Modelo com multi.start = TRUE

modelo_log_cambio_12_asi_multi <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted",
  multi.start = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03163418  0.02309645  0.92756525  0.07621090 -1.09573688 -0.29676316  9.49480000 

modelo_log_cambio_12_asi$par
modelo_log_cambio_12_asi$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03163418 0.016907716 6.134635e-02 0.087589894 7.179780e-01
# alpha alpha  0.02309645 0.006053031 1.358071e-04 0.006030742 1.282553e-04
# beta   beta  0.92756525 0.010182886 0.000000e+00 0.005612065 0.000000e+00
# gamma gamma  0.07621090 0.012519446 1.147580e-09 0.011097231 6.530998e-12
# m         m -1.09573688 0.969967815 2.586182e-01 0.666237290 1.000390e-01
# theta theta -0.29676316 0.140999272 3.531620e-02 0.083227428 3.629009e-04
# w2       w2  9.49480000 4.755285824 4.585959e-02 3.851196881 1.368545e-02

#* Olhando a convergência
modelo_log_cambio_12_asi_multi$optim$convergence
modelo_log_cambio_12_asi$optim$message
modelo_log_cambio_12_asi$variance.ratio
# 15% de variance ratio

#! modelo converge, modelo sem o multi nao converge em log

# 6 lags

modelo_log_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 6,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03673735  0.02769281  0.92443288  0.07157280 -0.62954213 -0.23220358  6.74223886 
modelo_log_cambio_6$par
modelo_log_cambio_6$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03673735 0.016869183 2.942252e-02 0.096101071 7.022550e-01
# alpha alpha  0.02769281 0.006910513 6.140500e-05 0.006432701 1.669816e-05
# beta   beta  0.92443288 0.010590840 0.000000e+00 0.005813241 0.000000e+00
# gamma gamma  0.07157280 0.011339378 2.756642e-10 0.011986917 2.359342e-09
# m         m -0.62954213 1.308492310 6.304310e-01 0.519779492 2.258297e-01
# theta theta -0.23220358 0.195087289 2.339463e-01 0.066704230 4.993734e-04
# w2       w2  6.74223886 9.673058685 4.857952e-01 3.015060898 2.533955e-02

#* Olhando a convergência
modelo_log_cambio_6$optim$convergence
modelo_log_cambio_6$optim$message

#! MODELO CONVERGE


#todo TESTANDO O MODELO SIMETRICO COM K = 12

modelo_log_cambio_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = FALSE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta           m       theta          w2 
# 0.05750119  0.07068999  0.92033154 -1.02078328 -0.30431562  7.78864945 

modelo_log_cambio_12$par
modelo_log_cambio_12$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err opg.p.value
# mu       mu  0.05750119 0.017340744 0.0009132934 0.071446788 0.420928526
# alpha alpha  0.07068999 0.008069888 0.0000000000 0.005947713 0.000000000
# beta   beta  0.92033154 0.009370478 0.0000000000 0.006460453 0.000000000
# m         m -1.02078328 1.072983438 0.3414265011 0.687495908 0.137600913
# theta theta -0.30431562 0.157411153 0.0532050128 0.101455871 0.002704342
# w2       w2  7.78864945 4.679811638 0.0960508303 3.533610591 0.027512968

#* Olhando a convergência
modelo_log_cambio_12$optim$convergence
modelo_log_cambio_12$optim$message

#! Esse modelo converge!

#TODO TESTAREMOS AGORA A TRANSFORMAÇÃO LOG-DIFF

modelo_dlog_cambio_12 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  x = "d_log_var_cambio",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03107596  0.02279998  0.91852730  0.08363307  0.89466098 -0.74785165  2.56368336 
# p-value: 4.103421e-02
modelo_dlog_cambio_12$par
modelo_dlog_cambio_12$broom.mgarch

#! w2 não aparece colado no limite

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03107596 0.017008790 6.769176e-02 0.108424323 7.744076e-01
# alpha alpha  0.02279998 0.006085658 1.793115e-04 0.006424512 3.868368e-04
# beta   beta  0.91852730 0.008905437 0.000000e+00 0.006186429 0.000000e+00
# gamma gamma  0.08363307 0.013076068 1.596170e-10 0.013616522 8.147427e-10
# m         m  0.89466098 0.118473525 4.307665e-14 0.256914615 4.970742e-04
# theta theta -0.74785165 0.250360350 2.816367e-03 0.447257572 9.450801e-02
# w2       w2  2.56368336 0.412835758 5.300642e-10 1.550353053 9.820625e-02

#* Olhando a convergência
modelo_dlog_cambio_12$optim$convergence

#! modelo converge!

#* MODELO GPR GLOBAL EM LOG E E CAMBIO EM LOG

# MODELO COM K = 12

modelo_log_gpr_log_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE,
  multi.start = TRUE
)

# Resultados
#        mu       alpha        beta       gamma           m       theta          w2 
# 0.03389155  0.02214534  0.92943860  0.07609221 -1.05914835 -0.06145932  3.52415544 
#  theta.two      w2.two 
# -0.33529312  8.34932634 
# theta p-value: 9.090960e-01
# theta 2 p-value: 2.719213e-02


modelo_log_gpr_log_cambio$par
modelo_log_gpr_log_cambio$broom.mgarch
modelo_log_gpr_log_cambio$optim$convergence

#               term    estimate rob.std.err      p.value  opg.std.err  opg.p.value
# mu               mu  0.03389155 0.016919164 4.516163e-02  0.083732028 6.856524e-01
# alpha         alpha  0.02214534 0.005944246 1.949218e-04  0.005920283 1.835901e-04
# beta           beta  0.92943860 0.009830212 0.000000e+00  0.005515772 0.000000e+00
# gamma         gamma  0.07609221 0.012099467 3.197460e-10  0.010692460 1.107558e-12
# m                 m -1.05914835 2.919318923 7.167493e-01  1.334709690 4.274620e-01
# theta         theta -0.06145932 0.538272169 9.090960e-01  0.238170478 7.963702e-01
# w2               w2  3.52415544 9.350259396 7.062445e-01 22.239621227 8.740920e-01
# theta.two theta.two -0.33529312 0.151802298 2.719213e-02  0.089833003 1.896604e-04
# w2.two       w2.two  8.34932634 3.668783104 2.285946e-02  3.130891450 7.658705e-03

#! modelo converge

# K = 6

modelo_log_gpr_log_cambio_6 <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 6,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "log_var_cambio",
  low.freq.two = "year_month",
  K.two = 6,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#          mu        alpha         beta        gamma            m        theta 
# 0.037924480  0.026840918  0.929832609  0.067081001  0.000117883 -0.352262225 
#          w2    theta.two       w2.two 
# 1.236384079 -0.376580769  3.329706982 
# theta p-value: 3.357481e-02
# theta 2 p-value: 8.200381e-04


modelo_log_gpr_log_cambio_6$par
modelo_log_gpr_log_cambio_6$broom.mgarch
modelo_log_gpr_log_cambio_6$optim$convergence

#! MODELO NAO CONVERGE


#* TESTANDO COM LOG-DIFF DO CAMBIO


modelo_log_gpr_dlog_cambio <- fit_mfgarch(
  data = base_mfgarch_completa,
  y = "ret_ibov_100",
  
  # Primeira variável MIDAS: GPR
  x = "log_gpr_global",
  low.freq = "year_month",
  K = 12,
  weighting = "beta.restricted",
  
  # Segunda variável MIDAS: câmbio
  x.two = "d_log_var_cambio",
  low.freq.two = "year_month",
  K.two = 12,
  weighting.two = "beta.restricted",
  
  gamma = TRUE
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2   theta.two 
# 0.03141767  0.02303381  0.91773648  0.08350633  1.63400476 -0.16116395  4.68688947 -0.74077543 
#     w2.two 
# 2.58105848 



modelo_log_gpr_dlog_cambio$par
modelo_log_gpr_dlog_cambio$broom.mgarch
modelo_log_gpr_dlog_cambio$optim$convergence

#! MODELO CONVERGE

#todo (ii) plote o τ estimado contra a volatilidade realizada do Ibovespa para ver se o componente de longo prazo faz sentido;

#* TESTANDO O LOG CAMBIO

# volatilidade realizada do Ibovespa
rv_ibov_mensal <- base_mfgarch_completa |> 
  group_by(year_month) |> 
  summarise(
    rv_ibov = sum(ret_ibov_100^2, na.rm = TRUE),
    vol_realizada_ibov = sqrt(rv_ibov),
    .groups = "drop"
  )

# Extraindo o τ estimado

tau_cambio_mensal <- modelo_log_cambio_12_asi$df.fitted %>%
  group_by(year_month) %>%
  summarise(
    tau = mean(tau, na.rm = TRUE),
    g = mean(g, na.rm = TRUE),
    var_total_modelo = mean(tau * g, na.rm = TRUE),
    .groups = "drop"
  ) |> filter(!is.na(tau))

# juntando os dois

comparacao_tau_rv <- tau_cambio_mensal %>%
  left_join(rv_ibov_mensal, by = "year_month") %>%
  mutate(
    tau_z = as.numeric(scale(tau)),
    rv_z = as.numeric(scale(rv_ibov)),
    var_total_z = as.numeric(scale(var_total_modelo)),
    vol_tau = sqrt(tau),
    vol_tau_z = as.numeric(scale(vol_tau)),
    vol_realizada_z = as.numeric(scale(vol_realizada_ibov))
  )

# grafico do tau e variancia realizada do ibovespa padronizadas para caber na mesma escala

ggplot(comparacao_tau_rv, aes(x = year_month)) +
  geom_line(aes(y = var_total_z, linetype = "Variância total estimada e padronizada")) +
  geom_line(aes(y = rv_z, linetype = "RV do Ibovespa padronizado")) +
  labs(
    title = "Volatilidade total  vs variância realizada do Ibovespa",
    x = "Mês",
    y = "Séries padronizadas",
    linetype = ""
  ) +
  theme_minimal()

#! Aparentemente o modelo captura muito bem a volatilidade de longo prazo do IBovespa

ggplot(comparacao_tau_rv, aes(x = year_month)) +
  geom_line(aes(y = tau_z, linetype = "Tau estimado padronizado"), color = 'red') +
  geom_line(aes(y = rv_z, linetype = "RV do Ibovespa padronizado")) +
  labs(
    title = "Tau estimado vs variância realizada do Ibovespa",
    x = "Mês",
    y = "Séries padronizadas",
    linetype = 'Séries'
  ) +
  theme_minimal()

#! o Componente de longo prazo não captura bem a dinâmica


ggplot(comparacao_tau_rv, aes(x = year_month)) +
  geom_line(aes(y = tau_z, linetype = "Tau estimado padronizado"), color = 'red') +
  geom_line(aes(y = vol_realizada_z, linetype = "RV do Ibovespa padronizado")) +
  labs(
    title = "Tau estimado vs variância realizada do Ibovespa",
    x = "Mês",
    y = "Séries padronizadas",
    linetype = 'Séries'
  ) +
  theme_minimal()

# correlação

comparacao_tau_rv %>%
  summarise(
    cor_tau_rv = cor(tau, rv_ibov, use = "complete.obs"),
    cor_var_total_rv = cor(var_total_modelo, rv_ibov, use = "complete.obs")
  )

#  cor_tau_rv cor_var_total_rv
#       <dbl>            <dbl>
#    -0.0721            0.908

#! Aparentemente o log do cambio não captura muito bem a volatilidade de longo prazo do ibovespa

# Variance ratio

modelo_log_cambio_12$variance.ratio
# 15.94518

#! componente de longo prazo responde por uma parcela moderada da variação da variância condicional total

#* TESTANDO O LOG DIFF

# Extraindo o τ estimado

tau_cambio_mensal_dlog <- modelo_dlog_cambio_12$df.fitted %>%
  group_by(year_month) %>%
  summarise(
    tau = mean(tau, na.rm = TRUE),
    g = mean(g, na.rm = TRUE),
    var_total_modelo = mean(tau * g, na.rm = TRUE),
    .groups = "drop"
  ) |> filter(!is.na(tau))

# juntando os dois

comparacao_tau_rv_dlog <- tau_cambio_mensal_dlog %>%
  left_join(rv_ibov_mensal, by = "year_month") %>%
  mutate(
    tau_z = as.numeric(scale(tau)),
    rv_z = as.numeric(scale(rv_ibov)),
    var_total_z = as.numeric(scale(var_total_modelo)),
    vol_tau = sqrt(tau),
    vol_tau_z = as.numeric(scale(vol_tau)),
    vol_realizada_z = as.numeric(scale(vol_realizada_ibov))
  )

ggplot(comparacao_tau_rv_dlog, aes(x = year_month)) +
  geom_line(aes(y = var_total_z, linetype = "Variância total estimada e padronizada")) +
  geom_line(aes(y = rv_z, linetype = "RV do Ibovespa padronizado")) +
  labs(
    title = "Volatilidade total (dlog)  vs variância realizada do Ibovespa",
    x = "Mês",
    y = "Séries padronizadas",
    linetype = ""
  ) +
  theme_minimal()

ggplot(comparacao_tau_rv_dlog, aes(x = year_month)) +
  geom_line(aes(y = tau_z, linetype = "Tau estimado padronizado"), color = 'blue') +
  geom_line(aes(y = rv_z, linetype = "RV do Ibovespa padronizado")) +
  labs(
    title = "Tau estimado (dlog) vs variância realizada do Ibovespa",
    x = "Mês",
    y = "Séries padronizadas",
    linetype = ""
  ) +
  theme_minimal()

#! Pelos gráficos, a dinãmica do dlog é muito semelhante a do log

comparacao_tau_rv_dlog %>%
  summarise(
    cor_tau_rv = cor(tau, rv_ibov, use = "complete.obs"),
    cor_var_total_rv = cor(var_total_modelo, rv_ibov, use = "complete.obs")
  )

#  cor_tau_rv cor_var_total_rv
#       <dbl>            <dbl>
#     -0.155            0.924

# variance ratio

modelo_dlog_cambio_12$variance.ratio
# 4.757406

#! o componente de longo prazo explica pouco do variancia total

# todo (iv) como benchmark, rode o modelo com a RV do próprio Ibovespa como variável de baixa frequência

base_benchmark <- base_mfgarch_completa |> 
  left_join(rv_ibov_mensal, by = 'year_month') |> mutate( log_rv_ibov = log(rv_ibov))


modelo_benchmark <- fit_mfgarch(
  data = base_benchmark,
  y = "ret_ibov_100",
  x = "rv_ibov",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.032347395 0.022604623 0.902838174 0.091535335 0.636145384 0.003861526 1.000005969

modelo_benchmark$par
modelo_benchmark$broom.mgarch
modelo_benchmark$optim$convergence
#! não converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu 0.032347395 0.017070452 5.810103e-02 0.124716848 7.953523e-01
# alpha alpha 0.022604623 0.006890381 1.035844e-03 0.006918450 1.085801e-03
# beta   beta 0.902838174 0.015960807 0.000000e+00 0.007893145 0.000000e+00
# gamma gamma 0.091535335 0.018472425 7.224036e-07 0.016048064 1.171491e-08
# m         m 0.636145384 0.115538146 3.672165e-08 0.217507196 3.447778e-03
# theta theta 0.003861526 0.001229197 1.680824e-03 0.000943115 4.231935e-05
# w2       w2 1.000005969 0.454495551 2.778883e-02 0.329299098 2.391270e-03


modelo_benchmark_log <- fit_mfgarch(
  data = base_benchmark,
  y = "ret_ibov_100",
  x = "log_rv_ibov",
  low.freq = "year_month",
  K = 12,
  gamma = TRUE,
  weighting = "beta.restricted"
)

# Resultados
#         mu       alpha        beta       gamma           m       theta          w2 
# 0.03051837  0.01057078  0.88199409  0.11109362 -1.63012497  0.65949663  1.56519608 

modelo_benchmark_log$par
modelo_benchmark_log$broom.mgarch
modelo_benchmark_log$optim$convergence
#! modelo converge

#       term    estimate rob.std.err      p.value opg.std.err  opg.p.value
# mu       mu  0.03051837  0.01708924 7.412781e-02 0.143779741 8.319059e-01
# alpha alpha  0.01057078  0.01075473 3.256586e-01 0.007193435 1.416961e-01
# beta   beta  0.88199409  0.02974818 0.000000e+00 0.010265117 0.000000e+00
# gamma gamma  0.11109362  0.02781700 6.504143e-05 0.018419984 1.628004e-09
# m         m -1.63012497  0.36852066 9.714658e-06 0.374466060 1.341658e-05
# theta theta  0.65949663  0.09436029 2.765788e-12 0.071576922 0.000000e+00
# w2       w2  1.56519608  1.45355921 2.815685e-01 0.371991914 2.580883e-05

#todo primeiro mostre que o GPR move a variância cambial (uma regressão simples da RV cambial no GPR, tipo primeiro estágio); 

base_canal_mensal <- base_mfgarch_completa %>%
  distinct(year_month, .keep_all = TRUE) %>%
  arrange(year_month) %>%
  select(
    year_month,
    log_var_cambio,
    d_log_var_cambio,
    log_gpr_global,
    d_log_gpr_global
  )

#* log do cambio e log do gpr
reg_log <- lm(
  log_var_cambio ~ log_gpr_global,
  data = base_canal_mensal
)

summary(reg_log)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -7.2745     0.7043 -10.328   <2e-16 ***
# log_gpr_global   0.1088     0.1523   0.715    0.475    

#* dlog cambio e gpr


reg_dlog <- lm(
  d_log_var_cambio ~ log_gpr_global,
  data = base_canal_mensal
)

summary(reg_dlog)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)      -0.01025    0.04653  -0.220    0.826
# d_log_gpr_global -0.16358    0.20398  -0.802    0.423




base_canal_mensal <- base_canal_mensal %>%
  arrange(year_month) %>%
  mutate(
    d_log_gpr_l1 = lag(d_log_gpr_global, 1),
    d_log_gpr_l2 = lag(d_log_gpr_global, 2),
    d_log_gpr_l3 = lag(d_log_gpr_global, 3),
    log_gpr_l1 = lag(d_log_gpr_global, 1),
    log_gpr_l2 = lag(d_log_gpr_global, 2),
    log_gpr_l3 = lag(d_log_gpr_global, 3)
  )

reg_dlog_lags <- lm(
  d_log_var_cambio ~ d_log_gpr_global + d_log_gpr_l1 +
    d_log_gpr_l2 + d_log_gpr_l3,
  data = base_canal_mensal
)

summary(reg_dlog_lags)


reg_log_lags <- lm(
  log_var_cambio ~ log_gpr_global + log_gpr_l1 +
    log_gpr_l2 + log_gpr_l3,
  data = base_canal_mensal
)

summary()
#todo depois compare o θ do GPR com e sem o controle do câmbio