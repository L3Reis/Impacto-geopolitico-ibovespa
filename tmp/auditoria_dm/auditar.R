Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

bases <- readRDS("tmp/perdas_media_auditoria_dm.rds")
saida <- "tmp/auditoria_dm"
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
env <- new.env(parent = globalenv())
codigo <- parse("scripts_R/Forecast/teste_dm.R", encoding = "UTF-8")
for (x in codigo) if (is.call(x) && identical(x[[1]], as.name("<-")) &&
  identical(x[[2]], as.name("testar_dm"))) eval(x, env)

# Autocovariancias pela definicao: denominador n em todas as defasagens.
autocov <- function(d, L) {
  n <- length(d); u <- d - mean(d)
  vapply(0:L, function(k) sum(u[seq_len(n-k)] * u[k+seq_len(n-k)]) / n, numeric(1))
}
hac_manual <- function(d, L) {
  ac <- autocov(d, L)
  s <- ac[1] + if (L) 2 * sum((1-seq_len(L)/(L+1)) * ac[-1]) else 0
  c(lrv = s, var_media = s/length(d), ep = sqrt(s/length(d)))
}
# Formula Bartlett do seletor automatico, independente da chamada ao sandwich.
bw_manual <- function(d) {
  n <- length(d); m <- floor(4*(n/100)^(2/9)); ac <- autocov(d, m)
  s0 <- ac[1]+2*sum(ac[-1]); s1 <- 2*sum(seq_len(m)*ac[-1])
  1.1447*((s1/s0)^2)^(1/3)*n^(1/3)
}
tab <- acfs <- sens <- influ <- conjuntos <- list()
for (nome in names(bases)) {
  p <- bases[[nome]]
  stopifnot(!anyDuplicated(p[c("origem", "horizonte", "modelo")]))
  for (h in sort(unique(p$horizonte))) {
    z <- p |> filter(horizonte == h, comparavel) |> arrange(origem, modelo)
    a <- z |> filter(modelo == "gpr") |> arrange(origem)
    b <- z |> filter(modelo == "vr") |> arrange(origem)
    stopifnot(identical(a$origem, b$origem), identical(a$data, b$data),
      identical(a$observado, b$observado), all(is.finite(c(a$previsao, b$previsao))),
      all(c(a$previsao, b$previsao) > 0))
    for (perda in c("MSE", "QLIKE")) {
      # Recalcula as perdas a partir do observado e das previsoes.
      la <- if (perda == "MSE") (a$observado-a$previsao)^2 else log(a$previsao)+a$observado/a$previsao
      lb <- if (perda == "MSE") (b$observado-b$previsao)^2 else log(b$previsao)+b$observado/b$previsao
      stopifnot(isTRUE(all.equal(la, a[[perda]], tolerance = 1e-12)),
        isTRUE(all.equal(lb, b[[perda]], tolerance = 1e-12)))
      d <- la-lb; n <- length(d); ajuste <- lm(d ~ 1)
      bw <- sandwich::bwNeweyWest(ajuste, prewhite = FALSE)
      bm <- bw_manual(d)
      L <- min(n-2L, max(h-1L, floor(bw)))
      calc <- hac_manual(d, L)
      vp <- sandwich::NeweyWest(ajuste, lag = L, prewhite = FALSE, adjust = FALSE)[1,1]
      dm <- mean(d)/calc["ep"]; pv <- 2*pnorm(-abs(dm))
      atual <- env$testar_dm(d, h)
      stopifnot(abs(bw-bm) < 1e-8, abs(vp-calc["var_media"])/vp < 1e-10,
        atual$lag_HAC == L, abs(atual$diferenca-mean(d)) < 1e-12,
        abs(atual$DM-dm) < 1e-9, abs(atual$p_value-pv) < 1e-9, L >= h-1L)
      u <- d-mean(d); idx <- order(abs(d), decreasing = TRUE)
      crise <- a$origem >= as.Date("2020-03-01") & a$origem <= as.Date("2020-04-30")
      tab[[length(tab)+1L]] <- data.frame(serie=nome, horizonte=h, perda=perda,
        n=n, media=mean(d), bw_pacote=bw, bw_manual=bm, lag_HAC=L,
        gamma0=mean(u^2), lrv=calc["lrv"], var_media_pacote=vp,
        var_media_manual=calc["var_media"], erro_padrao=calc["ep"],
        DM=dm, p_value=pv, erro_relativo_variancia=abs(vp-calc["var_media"])/vp,
        maior_share_abs=max(abs(d))/sum(abs(d)), maior_share_quadrados=max(u^2)/sum(u^2),
        top5_share_abs=sum(abs(d[idx[1:5]]))/sum(abs(d)),
        share_marabr_abs=sum(abs(d[crise]))/sum(abs(d)),
        media_fora_marabr=mean(d[!crise]), row.names=NULL)
      ac <- autocov(d, max(L, 60L))
      k <- 0:(length(ac)-1L); pesos <- ifelse(k <= L, 1-k/(L+1), 0)
      acfs[[length(acfs)+1L]] <- data.frame(serie=nome, horizonte=h, perda=perda,
        lag=k, autocov=ac, autocor=ac/ac[1], peso_HAC=pesos,
        contribuicao_lrv=ifelse(k == 0, ac, 2*pesos*ac))
      for (l in sort(unique(c(0L, h-1L, L, 5L, 10L, 22L, 40L, 60L, 100L)))) {
        c1 <- hac_manual(d, l); t1 <- mean(d)/c1["ep"]
        sens[[length(sens)+1L]] <- data.frame(serie=nome, horizonte=h, perda=perda,
          lag=l, cobre_h_menos_1=l>=h-1L, lrv=c1["lrv"], ep=c1["ep"],
          DM=t1, p_value=2*pnorm(-abs(t1)), row.names=NULL)
      }
      for (j in idx[1:10]) influ[[length(influ)+1L]] <- data.frame(
        serie=nome, horizonte=h, perda=perda, origem=a$origem[j],
        inicio_alvo=a$inicio_alvo[j], fim_alvo=a$data[j], observado=a$observado[j],
        previsao_gpr=a$previsao[j], previsao_vr=b$previsao[j], diferencial=d[j],
        share_abs=abs(d[j])/sum(abs(d)), share_quadrados=u[j]^2/sum(u^2))
      conjuntos[[paste(nome,h,perda,sep="/")]] <- data.frame(origem=a$origem, d=d)
    }
  }
}
resumo <- bind_rows(tab); autocovariancias <- bind_rows(acfs)
sensibilidade <- bind_rows(sens); influentes <- bind_rows(influ)
write.csv(resumo, file.path(saida,"resumo.csv"), row.names=FALSE)
write.csv(autocovariancias, file.path(saida,"autocovariancias.csv"), row.names=FALSE)
write.csv(sensibilidade, file.path(saida,"sensibilidade_lag.csv"), row.names=FALSE)
write.csv(influentes, file.path(saida,"origens_influentes.csv"), row.names=FALSE)
saveRDS(conjuntos, file.path(saida,"diferenciais.rds"))

# Compara com os valores registrados pelo usuario nos scripts atuais.
arquivos <- c(ibov_media_gamma="ibovespa", basicos_media_gamma="basicos",
  consumo_media_gamma="consumo", energia_media_gamma="energia", financeiro_media_gamma="financeiro")
erros_tabelas <- numeric()
for (nome in names(arquivos)) {
  linhas <- readLines(paste0("scripts_R/Forecast/forecast_expanding_",arquivos[[nome]],"_media_gamma.R"), encoding="UTF-8")
  for (linha in linhas[grepl("^#\\s+(1|5|22)\\s+(MSE|QLIKE)\\s+",linhas)]) {
    v <- strsplit(trimws(sub("^#", "", linha)), "\\s+")[[1]]
    if (length(v) != 7L) next
    r <- resumo[resumo$serie==nome & resumo$horizonte==as.integer(v[1]) & resumo$perda==v[2],]
    stopifnot(nrow(r)==1L, r$n==as.integer(v[3]), r$lag_HAC==as.integer(v[4]))
    erros_tabelas <- c(erros_tabelas, abs(r$DM-as.numeric(v[6])), abs(r$p_value-as.numeric(v[7])))
  }
}
stopifnot(length(erros_tabelas)==60L, max(erros_tabelas) < 1e-6)
cat("AUDITORIA: 30 testes atuais reproduzidos; maior erro relativo da variancia HAC:",
  max(resumo$erro_relativo_variancia), "; maior diferenca frente as tabelas:",max(erros_tabelas),"\n")
print(resumo |> select(serie,horizonte,perda,media,lag_HAC,erro_padrao,DM,p_value), row.names=FALSE)
cat("\nCONCENTRACAO DO MSE, h=1\n")
print(resumo |> filter(perda=="MSE",horizonte==1) |>
  select(serie,maior_share_abs,maior_share_quadrados,top5_share_abs,share_marabr_abs,media_fora_marabr),row.names=FALSE)
cat("\nPRINCIPAIS ORIGENS DO MSE, h=1\n")
print(influentes |> filter(perda=="MSE",horizonte==1) |> group_by(serie) |> slice_head(n=3) |>
  select(serie,origem,fim_alvo,observado,previsao_gpr,previsao_vr,diferencial),n=15)
