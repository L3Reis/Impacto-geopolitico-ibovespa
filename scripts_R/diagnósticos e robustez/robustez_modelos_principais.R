library(FinTS)

#* Função de diagnóstico


diagnosticar_mfgarch <- function(
    modelo,
    nome_modelo = "Modelo",
    lags_ljung = c(5, 10, 20),
    lags_arch = 10,
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

#todo MODELO LOG GPR ASSIMÉTRICO 24 LAGS


diag_log_gpr_global_24<- diagnosticar_mfgarch(
  modelo_gpr_global_log_24,
  nome_modelo = "Ibov - Log GPR - 24 lags"
)

# -------------------------------------
# DIAGNÓSTICO: Ibov - Log GPR - 24 lags 
# -------------------------------------

#                    modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo   BIC
# Ibov - Log GPR - 24 lags        5927 0.02560153 0.9137924 0.07894686    0.9788674                 TRUE           TRUE          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 21279

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos   5    2.188473 0.8224992 OK: não rejeita H0
#  Ljung-Box — resíduos  10    7.559048 0.6718211 OK: não rejeita H0
#  Ljung-Box — resíduos  20   14.966942 0.7782960 OK: não rejeita H0
# Ljung-Box — resíduos²   5    6.756830 0.2393624 OK: não rejeita H0
# Ljung-Box — resíduos²  10   11.942339 0.2889341 OK: não rejeita H0
# Ljung-Box — resíduos²  20   17.932391 0.5918621 OK: não rejeita H0
#               ARCH-LM  10   12.053308 0.2815046 OK: não rejeita H0

#* O modelo parece adequado, com resíduos padronizados sem autocorrelação significativa e sem efeito ARCH remanescente.


#todo Consumo com LOG GPR 24 lags

diag_consumo_24 <- diagnosticar_mfgarch(
  modelo_consumo_24,
  nome_modelo = 'Consumo - LOG GPR 24 lags'
)

# -------------------------------------
# DIAGNÓSTICO: Consumo - LOG GPR 24 lags 
#-------------------------------------

#                     modelo observacoes      alpha      beta      gamma persistencia persistencia_menor_1 alpha_positivo beta_positivo alpha_mais_gamma_positivo codigo_convergencia convergiu hessiana_ok g_positivo tau_positivo      BIC
# Consumo - LOG GPR 24 lags        5929 0.02670641 0.9029159 0.08513653    0.9721906                 TRUE           TRUE          TRUE                      TRUE                   0      TRUE        TRUE       TRUE         TRUE 19240.37

# Testes residuais:
#                  teste lag estatistica   p_value      interpretacao
#  Ljung-Box — resíduos   5    4.501124 0.4797330 OK: não rejeita H0
#  Ljung-Box — resíduos  10   14.532368 0.1500636 OK: não rejeita H0
#  Ljung-Box — resíduos  20   28.219568 0.1043028 OK: não rejeita H0
# Ljung-Box — resíduos²   5    6.392549 0.2698734 OK: não rejeita H0
# Ljung-Box — resíduos²  10    9.698843 0.4673002 OK: não rejeita H0
# Ljung-Box — resíduos²  20   16.630036 0.6768497 OK: não rejeita H0
#               ARCH-LM  10    9.777776 0.4602015 OK: não rejeita H0



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

