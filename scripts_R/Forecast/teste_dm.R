# DM bilateral: diferencial de perdas GPR - VR, com variância HAC/Newey-West.
library(dplyr)
library(tidyr)

testar_dm <- function(d, h) {
  n <- length(d)
  stopifnot(n > h + 1L, all(is.finite(d)), var(d) > 0)
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

resultados_dm <- base_dm |>
  group_by(horizonte, perda) |>
  group_modify(~ testar_dm(.x$gpr - .x$vr, .y$horizonte)) |>
  ungroup()

print(as.data.frame(resultados_dm), row.names = FALSE, digits = 6)
