library(FinTS)

#* Função de diagnóstico


diagnosticar_mfgarch <- function(
    modelo,
    nome_modelo = "Modelo",
    lags_ljung = c(10, 20),
    lags_arch = 12,
    graficos = TRUE
) {

  # -------------------------------------------------------
  # 1. Verificações iniciais
  # -------------------------------------------------------

  if (is.null(modelo$par)) {
    stop("O objeto não possui o elemento $par.")
  }

  if (is.null(modelo$df.fitted$residuals)) {
    stop("O objeto não possui resíduos em $df.fitted$residuals.")
  }

  parametros <- modelo$par

  pegar_parametro <- function(nome, padrao = NA_real_) {
    if (nome %in% names(parametros)) {
      return(as.numeric(parametros[nome]))
    }

    padrao
  }

  alpha <- pegar_parametro("alpha")
  beta  <- pegar_parametro("beta")
  gamma <- pegar_parametro("gamma", 0)

  # Para GJR-GARCH:
  # persistência = alpha + beta + gamma/2
  persistencia <- alpha + beta + gamma / 2

  # -------------------------------------------------------
  # 2. Convergência do otimizador
  # -------------------------------------------------------

  codigo_convergencia <- NA_integer_
  mensagem_convergencia <- NA_character_

  if (!is.null(modelo$optim$convergence)) {
    codigo_convergencia <- modelo$optim$convergence
  }

  if (!is.null(modelo$optim$message)) {
    mensagem_convergencia <- modelo$optim$message
  }

  convergiu <- ifelse(
    is.na(codigo_convergencia),
    NA,
    codigo_convergencia == 0
  )

  # -------------------------------------------------------
  # 3. Hessiana / erros-padrão robustos
  # -------------------------------------------------------

  hessiana_ok <- FALSE

  if (
    !is.null(modelo$broom.mgarch) &&
    "rob.std.err" %in% names(modelo$broom.mgarch)
  ) {
    hessiana_ok <- all(
      is.finite(modelo$broom.mgarch$rob.std.err)
    )
  }

  # -------------------------------------------------------
  # 4. Componentes da variância
  # -------------------------------------------------------

componente_positivo <- function(x) {

  # Caso o componente não exista
  if (is.null(x)) {
    return(NA)
  }

  # Converter em vetor numérico
  x <- as.numeric(x)

  # Remover apenas valores NA
  x <- x[!is.na(x)]

  # Caso não sobre nenhum valor
  if (length(x) == 0) {
    return(NA)
  }

  # Verificar se todos são finitos e estritamente positivos
  all(is.finite(x)) && all(x > 0)
}

g_ok <- componente_positivo(modelo$g)
tau_ok <- componente_positivo(modelo$tau)

  # -------------------------------------------------------
  # 5. Resíduos padronizados
  # -------------------------------------------------------

  residuos <- as.numeric(
    modelo$df.fitted$residuals
  )

  residuos <- residuos[
    is.finite(residuos)
  ]

  # -------------------------------------------------------
  # 6. Ljung-Box dos resíduos
  # -------------------------------------------------------

  ljung_residuos <- lapply(
    lags_ljung,
    function(lag_escolhido) {

      teste <- Box.test(
        residuos,
        lag = lag_escolhido,
        type = "Ljung-Box",
        fitdf = 0
      )

      data.frame(
        teste = "Ljung-Box — resíduos",
        lag = lag_escolhido,
        estatistica = as.numeric(teste$statistic),
        p_value = as.numeric(teste$p.value)
      )
    }
  )

  ljung_residuos <- do.call(
    rbind,
    ljung_residuos
  )

  # -------------------------------------------------------
  # 7. Ljung-Box dos resíduos ao quadrado
  # -------------------------------------------------------

  ljung_quadrados <- lapply(
    lags_ljung,
    function(lag_escolhido) {

      teste <- Box.test(
        residuos^2,
        lag = lag_escolhido,
        type = "Ljung-Box",
        fitdf = 0
      )

      data.frame(
        teste = "Ljung-Box — resíduos²",
        lag = lag_escolhido,
        estatistica = as.numeric(teste$statistic),
        p_value = as.numeric(teste$p.value)
      )
    }
  )

  ljung_quadrados <- do.call(
    rbind,
    ljung_quadrados
  )

  # -------------------------------------------------------
  # 8. ARCH-LM residual
  # -------------------------------------------------------

  teste_arch <- FinTS::ArchTest(
    residuos,
    lags = lags_arch,
    demean = TRUE
  )

  resultado_arch <- data.frame(
    teste = "ARCH-LM",
    lag = lags_arch,
    estatistica = as.numeric(teste_arch$statistic),
    p_value = as.numeric(teste_arch$p.value)
  )

  # -------------------------------------------------------
  # 9. Consolidar testes
  # -------------------------------------------------------

  tabela_testes <- rbind(
    ljung_residuos,
    ljung_quadrados,
    resultado_arch
  )

  tabela_testes$interpretacao <- ifelse(
    tabela_testes$p_value >= 0.05,
    "OK: não rejeita H0",
    "Atenção: rejeita H0"
  )

  # -------------------------------------------------------
  # 10. Resumo do modelo
  # -------------------------------------------------------

  resumo <- data.frame(
    modelo = nome_modelo,
    observacoes = length(residuos),

    alpha = alpha,
    beta = beta,
    gamma = gamma,

    persistencia = persistencia,
    persistencia_menor_1 = persistencia < 1,

    alpha_positivo = alpha >= 0,
    beta_positivo = beta >= 0,
    alpha_mais_gamma_positivo = alpha + gamma >= 0,

    codigo_convergencia = codigo_convergencia,
    convergiu = convergiu,

    hessiana_ok = hessiana_ok,
    g_positivo = g_ok,
    tau_positivo = tau_ok,

    BIC = as.numeric(modelo$bic)
  )

  # -------------------------------------------------------
  # 11. Gráficos
  # -------------------------------------------------------

  if (graficos) {

    par(mfrow = c(2, 2))

    plot(
      residuos,
      type = "l",
      main = paste(nome_modelo, "— resíduos padronizados"),
      xlab = "Observação",
      ylab = "Resíduo padronizado"
    )

    acf(
      residuos,
      lag.max = 30,
      main = "ACF — resíduos padronizados",
      na.action = na.pass
    )

    acf(
      residuos^2,
      lag.max = 30,
      main = "ACF — resíduos padronizados²",
      na.action = na.pass
    )

    qqnorm(
      residuos,
      main = "Q-Q plot — resíduos padronizados"
    )

    qqline(residuos)

    par(mfrow = c(1, 1))
  }

  # -------------------------------------------------------
  # 12. Exibir resultados
  # -------------------------------------------------------

  cat("\n-------------------------------------\n")
  cat("DIAGNÓSTICO:", nome_modelo, "\n")
  cat("-------------------------------------\n\n")

  print(resumo)

  cat("\nTestes residuais:\n")
  print(tabela_testes)

  if (!is.na(mensagem_convergencia)) {
    cat(
      "\nMensagem do otimizador:",
      mensagem_convergencia,
      "\n"
    )
  }

  invisible(
    list(
      resumo = resumo,
      testes = tabela_testes,
      residuos = residuos
    )
  )
}

#todo MODELO LOG GPR ASSIMÉTRICO 6 LAGS

diag_log_gpr_6 <- diagnosticar_mfgarch(
  modelo_gpr_global_log_6,
  nome_modelo = "Ibov - GPR log 6"
)


# -------------------------------------
# DIAGNÓSTICO: Ibov - GPR log 6 
# -------------------------------------

#    modelo         observacoes  alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo beta_positivo
# Ibov - GPR log 6     6545 0.03003374 0.9142347 0.07222827    0.9803826                 TRUE           TRUE          TRUE
# alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo      BIC
#                      TRUE                   0      TRUE        TRUE      FALSE        FALSE 23889.23

#Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10    7.366203 0.6904796 OK: não rejeita H0
#  Ljung-Box — resíduos  20   16.239329 0.7016701 OK: não rejeita H0
# Ljung-Box — resíduos²  10   11.143803 0.3464248 OK: não rejeita H0
# Ljung-Box — resíduos²  20   19.440672 0.4933699 OK: não rejeita H0
#               ARCH-LM  12   11.626273 0.4761413 OK: não rejeita H0

#! Esse modelo está adequado, passou nos testes de diagnósticos (entender mais cada um deles no futuro)

#todo MODELO LOG GPR + LOG CAMBIO PARA O IBOV 6 LAGS

diag_log_gpr_log_cambio_6 <- diagnosticar_mfgarch(
  modelo_log_gpr_log_cambio_6,
  nome_modelo = "Ibov - Log GPR + Log cambio - 6 lags"
)
#! Modelo não convergiu

# -------------------------------------
# DIAGNÓSTICO: Ibov - Log GPR + Log cambio - 6 lags 
#-------------------------------------

#                                modelo observacoes      alpha      beta    gamma persistencia persistencia_menor_1 alpha_positivo
# Ibov - Log GPR + Log cambio - 6 lags        6545 0.02684092 0.9298326 0.067081     0.990214                 TRUE           TRUE
#  beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo      BIC
#          TRUE                      TRUE                   1     FALSE        TRUE       TRUE         TRUE 23884.35

#Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10    6.436216 0.7773804 OK: não rejeita H0
#  Ljung-Box — resíduos  20   15.575259 0.7426002 OK: não rejeita H0
# Ljung-Box — resíduos²  10   10.708155 0.3807028 OK: não rejeita H0
# Ljung-Box — resíduos²  20   18.799782 0.5348726 OK: não rejeita H0
#               ARCH-LM  12   11.311476 0.5024306 OK: não rejeita H0

#todo MODELO COM GPR EM LOG E CAMBIO EM LOG-DIFF 12 lags

diag_log_gpr_dlog_cambio <- diagnosticar_mfgarch(
  modelo_log_gpr_dlog_cambio,
  nome_modelo = "Ibov - Log GPR + DLog cambio - 12 lags"
)

# -------------------------------------
# DIAGNÓSTICO: Ibov - Log GPR + DLog cambio - 12 lags 
# -------------------------------------

#                                  modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1
# Ibov - Log GPR + DLog cambio - 12 lags        6421 0.02303381 0.9177365 0.08350633    0.9825235                 TRUE
#  alpha_positivo beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo
#           TRUE          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE
#      BIC
# 23382.75

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10     6.93315 0.7317406 OK: não rejeita H0
#  Ljung-Box — resíduos  20    15.46633 0.7491314 OK: não rejeita H0
# Ljung-Box — resíduos²  10    12.27375 0.2671532 OK: não rejeita H0
# Ljung-Box — resíduos²  20    18.50735 0.5540246 OK: não rejeita H0
#               ARCH-LM  12    12.82429 0.3819403 OK: não rejeita H0

#todo MODELO COM GPR EM LOG E IC-BR EM LOG-DIFF 6 LAGS

diag_log_gpr_ic_6 <- diagnosticar_mfgarch(
  modelo_log_gpr_ic_6,
  nome_modelo = 'Ibov - Log GPR + Dlog IC-Br - 6 lags'
)

# -------------------------------------
# DIAGNÓSTICO: Ibov - Log GPR + Dlog IC-Br - 6 lags 
# -------------------------------------

#                                modelo observacoes     alpha      beta      gamma persistencia persistencia_menor_1
# Ibov - Log GPR + Dlog IC-Br - 6 lags        6545 0.0293191 0.9159717 0.07146522    0.9810234                 TRUE
# alpha_positivo beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo
#           TRUE          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE
#     BIC
# 23900.2

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10    7.265348 0.7001794 OK: não rejeita H0
#  Ljung-Box — resíduos  20   16.456109 0.6879555 OK: não rejeita H0
# Ljung-Box — resíduos²  10   11.692357 0.3061734 OK: não rejeita H0
# Ljung-Box — resíduos²  20   20.718028 0.4138931 OK: não rejeita H0
#               ARCH-LM  12   12.439603 0.4110510 OK: não rejeita H0


#todo Consumo com LOG GPR 6 lags

diag_consumo_6 <- diagnosticar_mfgarch(
  modelo_consumo_6,
  nome_modelo = 'Consumo - LOG GPR 6 lags'
)

# -------------------------------------
# DIAGNÓSTICO: Consumo - LOG GPR 6 lags 
# -------------------------------------

#                    modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo
# Consumo - LOG GPR 6 lags        6299 0.02848406 0.9063731 0.08567955    0.9776969                 TRUE           TRUE
# beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo      BIC
#          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 20832.46

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10    11.15193 0.3458047 OK: não rejeita H0
#  Ljung-Box — resíduos  20    26.15683 0.1606946 OK: não rejeita H0
# Ljung-Box — resíduos²  10    11.61241 0.3118334 OK: não rejeita H0
# Ljung-Box — resíduos²  20    20.43569 0.4309884 OK: não rejeita H0
#               ARCH-LM  12    14.16561 0.2902583 OK: não rejeita H0


#todo MODELO GPR + CAMBIO PARA O SETOR DE CONSUMO 6 LAGS

diag_consumo_log_gpr_log_cambio_6 <- diagnosticar_mfgarch(
  modelo_consumo_log_gpr_log_cambio_6,
  nome_modelo = 'consumo - GPR + cambio'
)


#                  modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo
# consumo - GPR + cambio        6299 0.02975614 0.9079802 0.08542665    0.9804497                 TRUE           TRUE
#  beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo   BIC
#          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 20844

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos  10    11.19631 0.3424299 OK: não rejeita H0
#  Ljung-Box — resíduos  20    26.13224 0.1614886 OK: não rejeita H0
# Ljung-Box — resíduos²  10    11.85313 0.2950066 OK: não rejeita H0
# Ljung-Box — resíduos²  20    21.13352 0.3893115 OK: não rejeita H0
#               ARCH-LM  12    14.70010 0.2582480 OK: não rejeita H0

#todo Modelo Bens básicos e GPR 6 lags

diag_bens_basicos_6 <- diagnosticar_mfgarch(
  modelo_bens_basicos_6,
  nome_modelo = 'basicos - log gpr'
)

# -------------------------------------
# DIAGNÓSTICO: basicos - log gpr 
# -------------------------------------

#             modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo beta_positivo
# basicos - log gpr        6299 0.04723098 0.9057328 0.04358374    0.9747557                 TRUE           TRUE          TRUE
# alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo      BIC
#                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 25194.27

# Testes residuais:
#                  teste lag estatistica    p_value       interpretacao
#  Ljung-Box — resíduos  10   22.992184 0.01077547 Atenção: rejeita H0
#  Ljung-Box — resíduos  20   34.475990 0.02307944 Atenção: rejeita H0
# Ljung-Box — resíduos²  10    2.648957 0.98852273  OK: não rejeita H0
# Ljung-Box — resíduos²  20    6.457555 0.99812137  OK: não rejeita H0
#               ARCH-LM  12    4.126286 0.98104819  OK: não rejeita H0

#! O Ljung-box mostra que o modelo não eliminou totalmente a depedência linear dos retornos, mas isso não afeta a volatilidade

#todo mMODELO GPR + CAMBIO 6 LAGS PARA OS BENS BASICOS

diag_basicos_log_gpr_log_cambio_6 <- diagnosticar_mfgarch(
  modelo_basicos_log_gpr_log_cambio_6,
  nome_modelo = 'basicos - GPR + cambio'
)

#
# -------------------------------------
#DIAGNÓSTICO: basicos - GPR + cambio 
#-------------------------------------
#
#                  modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo
# basicos - GPR + cambio        6299 0.04482556 0.9134332 0.04239626    0.9794569                 TRUE           TRUE
#  beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo     BIC
#          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 25207.6

# Testes residuais:
#                  teste lag estatistica    p_value       interpretacao
#  Ljung-Box — resíduos  10   22.409194 0.01315045 Atenção: rejeita H0
#  Ljung-Box — resíduos  20   33.648366 0.02860411 Atenção: rejeita H0
# Ljung-Box — resíduos²  10    3.080709 0.97946034  OK: não rejeita H0
# Ljung-Box — resíduos²  20    6.897895 0.99700671  OK: não rejeita H0
#               ARCH-LM  12    4.537076 0.97168506  OK: não rejeita H0

#* CONCLUSÃO: NO GERAL, TIRANDO O MODELO COM LOG CAMBIO PARA O IBOVESPA, TODOS OS MODELOS PASSARAM NOS TESTES DE DIAGNÓSTICOS