# Ibovespa: K = 12, 18, 24 e 36 com a MESMA amostra efetiva.
# Referencia: janelas_k_12_18_24_ibov.R (comparacao de K, sem janelas moveis).
# Execute com o diretorio de trabalho na raiz do projeto.
#
# O primeiro retorno e definido pelo filtro nativo com K_max = 36.
# O historico mensal de GPR e cambio e preservado ANTES desse retorno.
# Todos os modelos sao REESTIMADOS; nao se recalcula apenas o BIC de ajustes antigos.
# Saidas: modelos_amostra_comum, comparacao_amostra_comum,
#         parametros_amostra_comum e amostra_efetiva_comum.

library(readxl)
library(dplyr)
library(tidyr)
library(mfGARCH)

# Configuracao -------------------------------------------------------------
valores_K <- c(12L, 18L, 24L, 36L)
K_max <- max(valores_K)
arquivo_base <- "dados/base_mfgarch_completa.xlsx"
data_inicial_base <- as.Date("2000-02-02")
multi_start <- TRUE
incluir_simetricos_k24 <- TRUE          # Mesmos dois controles do script original.
salvar_resultados <- TRUE
pasta_resultados <- "resultados/janelas_k_12_18_24_36_ibov_amostra_comum"

# Dados e historico mensal -------------------------------------------------
# Mantem a limpeza do script original para reproduzir a referencia K = 36.
# ret_ibov vem em fracao e e convertido para percentual apenas uma vez.
base_com_historico <- read_excel(arquivo_base) %>%
  drop_na() %>%
  transmute(
    date = as.Date(date),
    year_month = as.Date(paste0(year_month, "-01")),
    ret_ibov_100 = as.numeric(ret_ibov) * 100,
    log_gpr_global = as.numeric(log_gpr_global),
    log_var_cambio = as.numeric(log_var_cambio)
  ) %>%
  filter(date >= data_inicial_base) %>%
  arrange(date) %>%
  as.data.frame()

colunas_numericas <- c("ret_ibov_100", "log_gpr_global", "log_var_cambio")
if (nrow(base_com_historico) == 0L || anyNA(base_com_historico) ||
    any(!is.finite(as.matrix(base_com_historico[colunas_numericas])))) {
  stop("A base precisa conter datas e valores numericos validos nas colunas utilizadas.")
}
if (anyDuplicated(base_com_historico$date)) stop("Ha datas diarias duplicadas.")
if (any(format(base_com_historico$date, "%Y-%m") !=
        format(base_com_historico$year_month, "%Y-%m"))) {
  stop("year_month precisa corresponder ao mes de date.")
}

historico_mensal <- base_com_historico %>%
  distinct(year_month, log_gpr_global, log_var_cambio) %>%
  arrange(year_month)
if (anyDuplicated(historico_mensal$year_month)) {
  stop("GPR e cambio precisam ter um unico valor por mes.")
}
meses <- historico_mensal$year_month
if (length(meses) <= K_max ||
    !identical(meses, seq.Date(min(meses), max(meses), by = "month"))) {
  stop("Sao necessarios mais de 36 meses consecutivos, sem lacunas no historico.")
}

# Identifica as datas efetivas de K = 36 com o proprio filtro do mfGARCH.
# Os parametros abaixo servem apenas para localizar as defasagens disponiveis.
filtro_nativo <- getFromNamespace("calculate_tau_mf", "mfGARCH")
referencia_k36 <- filtro_nativo(
  df = base_com_historico, x = historico_mensal$log_gpr_global,
  low.freq = "year_month", w1 = 1, w2 = 3, theta = 0, m = 0, K = K_max,
  x.two = historico_mensal$log_var_cambio, K.two = K_max,
  theta.two = 0, low.freq.two = "year_month", w1.two = 1, w2.two = 3
)
datas_referencia <- referencia_k36$date[is.finite(referencia_k36$tau)]
if (length(datas_referencia) < 2L) stop("A referencia K = 36 nao tem retornos suficientes.")
inicio_comum <- min(datas_referencia)
fim_comum <- max(datas_referencia)
base_amostra_comum <- base_com_historico %>%
  filter(date >= inicio_comum, date <= fim_comum)
stopifnot(identical(base_amostra_comum$date, datas_referencia))

# O historico fica separado dos retornos, sem retornos ficticios ou anteriores
# na verossimilhanca. Isso tambem fixa g_zero = var(retornos comuns) em todo K.
historico_anterior <- historico_mensal %>%
  filter(year_month < min(base_amostra_comum$year_month))
amostra_efetiva_comum <- base_amostra_comum[c("date", "year_month", "ret_ibov_100")]
n_comum <- nrow(amostra_efetiva_comum)
message(sprintf("Amostra comum: %s a %s | %d retornos | K_max = %d",
                inicio_comum, fim_comum, n_comum, K_max))

# Estimacao com historico separado -----------------------------------------
# Copias locais de fit_mfgarch e llh_mf usam um filtro tau com pre-historico.
# Nao altera o namespace do pacote. Pesos Beta, recursao GARCH, otimizacao e
# inferencia continuam sendo os do mfGARCH (verificado na versao 0.2.2).
# sum_tau devolve as somas para os meses K+1,...,T; os K valores anteriores
# permitem que o primeiro mes dos retornos comuns ja tenha tau definido.
ajustar_amostra_comum <- function(dados, historico, x, K, x_two = NULL,
                                 gamma = TRUE, multi_start = TRUE) {
  if (nrow(historico) < K) stop("Historico mensal insuficiente para este K.")
  ambiente <- new.env(parent = asNamespace("mfGARCH"))
  ambiente$hist_x <- tail(historico[[x]], K)
  ambiente$hist_x_two <- if (!is.null(x_two)) tail(historico[[x_two]], K) else NULL
  ambiente$calculate_tau_mf <- function(
      df, x, low.freq, w1, w2, theta, m, K, x.two = NULL, K.two = NULL,
      theta.two = NULL, low.freq.two = NULL, w1.two = NULL, w2.two = NULL) {
    tau_mensal <- exp(sum_tau(
      m = m, theta = theta, phivar = calculate_phi(w1, w2, K),
      covariate = c(hist_x, x), K = K
    ))
    df$tau <- tau_mensal[match(df[[low.freq]], unique(df[[low.freq]]))]
    if (!is.null(x.two)) {
      tau_dois <- exp(sum_tau(
        m = 0, theta = theta.two,
        phivar = calculate_phi(w1.two, w2.two, K.two),
        covariate = c(hist_x_two, x.two), K = K.two
      ))
      df$tau <- df$tau * tau_dois[match(df[[low.freq.two]], unique(df[[low.freq.two]]))]
    }
    df
  }
  environment(ambiente$calculate_tau_mf) <- ambiente
  ambiente$llh_mf <- getFromNamespace("llh_mf", "mfGARCH")
  environment(ambiente$llh_mf) <- ambiente
  fit_com_historico <- mfGARCH::fit_mfgarch
  environment(fit_com_historico) <- ambiente

  argumentos <- list(
    data = dados, y = "ret_ibov_100", x = x, low.freq = "year_month", K = K,
    gamma = gamma, weighting = "beta.restricted", multi.start = multi_start
  )
  if (!is.null(x_two)) {
    argumentos <- c(argumentos, list(
      x.two = x_two, low.freq.two = "year_month", K.two = K,
      weighting.two = "beta.restricted"
    ))
  }
  do.call(fit_com_historico, argumentos)
}

# As tres familias do script original; K.two acompanha K no modelo conjunto.
especificacoes <- bind_rows(
  data.frame(familia = "GPR", K = valores_K, x = "log_gpr_global", x_two = NA_character_),
  data.frame(familia = "Cambio", K = valores_K, x = "log_var_cambio", x_two = NA_character_),
  data.frame(familia = "GPR + cambio", K = valores_K,
             x = "log_gpr_global", x_two = "log_var_cambio")
) %>% mutate(gamma = TRUE)
if (incluir_simetricos_k24) {
  especificacoes <- bind_rows(
    especificacoes,
    data.frame(familia = c("GPR", "Cambio"), K = 24L,
               x = c("log_gpr_global", "log_var_cambio"),
               x_two = NA_character_, gamma = FALSE)
  )
}

modelos_amostra_comum <- list()
linhas_comparacao <- vector("list", nrow(especificacoes))
linhas_parametros <- list()

for (i in seq_len(nrow(especificacoes))) {
  esp <- especificacoes[i, ]
  nome <- paste(esp$familia, paste0("K", esp$K),
                if (esp$gamma) "assimetrico" else "simetrico", sep = " | ")
  message(sprintf("Modelo %d/%d: %s", i, nrow(especificacoes), nome))
  linha <- data.frame(
    modelo = nome, familia = esp$familia, K = esp$K, gamma = esp$gamma,
    inicio = inicio_comum, fim = fim_comum, n = NA_integer_, p = NA_integer_,
    logLik = NA_real_, BIC = NA_real_, BIC_pacote = NA_real_,
    convergencia = NA_integer_, amostra_correta = FALSE,
    parametros_validos = FALSE, elegivel = FALSE, motivo = ""
  )
  ajuste <- tryCatch({
    invisible(capture.output(modelo <- ajustar_amostra_comum(
      dados = base_amostra_comum, historico = historico_anterior,
      x = esp$x, K = esp$K,
      x_two = if (is.na(esp$x_two)) NULL else esp$x_two,
      gamma = esp$gamma, multi_start = multi_start
    )))
    modelo
  }, error = identity)

  if (inherits(ajuste, "error")) {
    linha$motivo <- conditionMessage(ajuste)
  } else {
    modelos_amostra_comum[[nome]] <- ajuste
    df <- ajuste$df.fitted
    efetivas <- is.finite(df$residuals) & is.finite(df$tau) & df$tau > 0 &
      is.finite(df$g) & df$g > 0
    linha$n <- sum(efetivas)
    linha$amostra_correta <- identical(df$date[efetivas], amostra_efetiva_comum$date) &&
      identical(df$ret_ibov_100[efetivas], amostra_efetiva_comum$ret_ibov_100)
    if (!linha$amostra_correta) {
      stop("Amostra efetiva divergente em ", nome, ". Comparacao interrompida.")
    }

    # Extrai a log-verossimilhanca e confere seu sinal/convencao pela densidade
    # normal com variancia condicional tau*g, sobre TODOS os retornos comuns.
    linha$logLik <- as.numeric(ajuste$llh)
    ll_conferida <- sum(dnorm(df$ret_ibov_100, mean = ajuste$par["mu"],
                              sd = sqrt(df$tau * df$g), log = TRUE))
    if (!isTRUE(all.equal(linha$logLik, ll_conferida, tolerance = 1e-7))) {
      stop("A log-verossimilhanca nao confere em ", nome, ".")
    }
    linha$p <- length(ajuste$par)          # w1 fixo nao e parametro livre; K tambem nao.
    linha$BIC <- -2 * linha$logLik + linha$p * log(linha$n)
    linha$BIC_pacote <- as.numeric(ajuste$bic)
    if (!isTRUE(all.equal(linha$BIC, linha$BIC_pacote, tolerance = 1e-7))) {
      stop("BIC manual diverge do BIC do pacote em ", nome, ".")
    }
    linha$convergencia <- ajuste$optim$convergence
    par <- ajuste$par
    assimetria <- if (esp$gamma) par["gamma"] else 0
    persistencia <- par["alpha"] + par["beta"] + assimetria / 2
    pesos <- par[intersect(c("w2", "w2.two"), names(par))]
    linha$parametros_validos <- all(is.finite(par)) &&
      par["alpha"] >= 0 && par["beta"] >= 0 && par["alpha"] + assimetria >= 0 &&
      persistencia >= 0 && persistencia < 1 && all(pesos >= 1)
    linha$elegivel <- isTRUE(linha$convergencia == 0L) && linha$parametros_validos
    if (!linha$elegivel) {
      linha$motivo <- paste("Convergencia:", linha$convergencia,
                            "| parametros validos:", linha$parametros_validos)
    }
    linhas_parametros[[nome]] <- cbind(modelo = nome, ajuste$broom.mgarch)
  }
  linhas_comparacao[[i]] <- linha
  if (nzchar(linha$motivo)) message("  ", linha$motivo)
}

# Comparacao e auditoria ---------------------------------------------------
# Menor BIC entre ajustes elegiveis. Falhas permanecem na tabela, sem ranking.
comparacao_amostra_comum <- bind_rows(linhas_comparacao) %>%
  group_by(familia, gamma) %>%
  mutate(
    grupo_completo = n() == length(valores_K) && all(elegivel),
    delta_BIC = if (any(elegivel)) if_else(elegivel, BIC - min(BIC[elegivel]), NA_real_)
                else NA_real_,
    ranking_BIC = min_rank(if_else(elegivel, BIC, NA_real_))
  ) %>%
  ungroup() %>%
  arrange(familia, desc(gamma), K)
parametros_amostra_comum <- bind_rows(linhas_parametros)
print(as.data.frame(comparacao_amostra_comum), row.names = FALSE, digits = 8)
if (any(!comparacao_amostra_comum$elegivel)) {
  warning("Ha ajustes sem convergencia ou fora das restricoes. Consulte motivo antes de comparar K.")
}
# Com a ponderacao beta.restricted: p = 7 no assimetrico de uma covariavel,
# p = 9 no conjunto e p = 6 no simetrico. Dentro de cada familia, p nao muda com K.
# Se n e p sao iguais, a diferenca de BIC decorre somente da log-verossimilhanca.

if (salvar_resultados) {
  dir.create(pasta_resultados, recursive = TRUE, showWarnings = FALSE)
  write.csv(comparacao_amostra_comum, file.path(pasta_resultados, "comparacao_BIC.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(parametros_amostra_comum, file.path(pasta_resultados, "parametros.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(amostra_efetiva_comum, file.path(pasta_resultados, "amostra_efetiva.csv"),
            row.names = FALSE)
  saveRDS(list(
    modelos = modelos_amostra_comum, comparacao = comparacao_amostra_comum,
    parametros = parametros_amostra_comum, amostra = amostra_efetiva_comum,
    historico_mensal = historico_mensal, especificacoes = especificacoes,
    g_zero = var(base_amostra_comum$ret_ibov_100), sessao = sessionInfo()
  ), file.path(pasta_resultados, "ajustes_amostra_comum.rds"))
  message("Resultados salvos em: ", pasta_resultados)
}
