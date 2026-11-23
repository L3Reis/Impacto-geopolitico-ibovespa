# DM bilateral: diferencial de perdas GPR - VR, com variância HAC/Newey-West.
library(dplyr)
library(tidyr)

source("scripts_R/Forecast/dm_hac.R", local = TRUE, encoding = "UTF-8")

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
