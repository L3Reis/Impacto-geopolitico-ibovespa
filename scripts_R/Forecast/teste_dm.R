# DM bilateral: diferencial de perdas GPR - VR, com variância HAC/Newey-West.
library(dplyr)
library(tidyr)

testar_dm <- function(d, h) {
  n <- length(d)
  if (n <= h + 1L || !all(is.finite(d)) || !is.finite(var(d)) || var(d) == 0) {
    warning("DM nao calculado: poucas observacoes, perdas invalidas ou diferencial constante.", call. = FALSE)
    return(tibble(n = n, lag_HAC = NA_integer_, diferenca = if (n) mean(d) else NA_real_,
                  DM = NA_real_, p_value = NA_real_))
  }
  ajuste <- lm(d ~ 1)
  lag_hac <- min(n - 2L, max(h - 1L,
    floor(sandwich::bwNeweyWest(ajuste, prewhite = FALSE))))
  variancia <- sandwich::NeweyWest(ajuste, lag = lag_hac, prewhite = FALSE)[1, 1]
  stopifnot(is.finite(variancia), variancia > 0)
  dm <- mean(d) / sqrt(variancia)
  tibble(n = n, lag_HAC = lag_hac, diferenca = mean(d),
         DM = dm, p_value = 2 * pnorm(-abs(dm)))
}

base_dm <- perdas_expanding |>
  filter(comparavel) |>
  select(origem, horizonte, modelo, MSE, QLIKE) |>
  pivot_longer(c(MSE, QLIKE), names_to = "perda", values_to = "valor") |>
  pivot_wider(names_from = modelo, values_from = valor) |>
  arrange(horizonte, perda, origem)
stopifnot(!anyDuplicated(base_dm[c("origem", "horizonte", "perda")]))

resultados_dm <- if (nrow(base_dm)) {
  base_dm |>
    group_by(horizonte, perda) |>
    group_modify(~ testar_dm(.x$gpr - .x$vr, .y$horizonte)) |>
    ungroup()
} else tibble()

if (nrow(resultados_dm)) {
  print(as.data.frame(resultados_dm), row.names = FALSE, digits = 6)
} else message("DM nao calculado: nenhuma previsao pareada valida.")

# horizonte perda    n lag_HAC   diferenca        DM  p_value
#         1   MSE 2564      30 -8.44618027 -1.018860 0.308269
#         1 QLIKE 2564      23  0.00322436  0.732380 0.463937
#         5   MSE 2560      22 -5.95961237 -1.046717 0.295230
#         5 QLIKE 2560      21  0.00256810  0.485016 0.627665
#        22   MSE 2543      26  0.50849892  0.401144 0.688314
#        22 QLIKE 2543      31 -0.00329470 -0.288161 0.773224
