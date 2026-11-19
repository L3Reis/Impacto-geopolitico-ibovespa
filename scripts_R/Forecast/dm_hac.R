# DM bilateral sobre GPR - VR; uma entrada por origem, em ordem cronologica.
testar_dm <- function(d, h) {
  total <- length(d)
  pares <- !is.na(d)
  n <- sum(pares)
  stopifnot(n > h + 1L, all(is.finite(d[pares])), var(d[pares]) > 0)
  diferenca <- mean(d[pares])
  # Uma origem sem par tem escore centrado zero para o HAC e nao entra na media.
  # O fator total/n converte o erro padrao para a media das n origens pareadas.
  d[!pares] <- diferenca
  ajuste <- lm(d ~ 1)
  lag_hac <- min(total - 2L, max(h - 1L,
    floor(sandwich::bwNeweyWest(ajuste, prewhite = FALSE))))
  variancia <- sandwich::NeweyWest(ajuste, lag = lag_hac, prewhite = FALSE)[1, 1]
  stopifnot(is.finite(variancia), variancia > 0)
  dm <- diferenca / (sqrt(variancia) * (total/n))
  tibble::tibble(n = n, lag_HAC = lag_hac, diferenca = diferenca,
                 DM = dm, p_value = 2 * pnorm(-abs(dm)))
}
