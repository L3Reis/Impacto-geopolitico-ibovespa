# Estatísticas descritivas das séries

# Instruções Better Comments
# ! CRÍTICO: Lembre-se de manter K = 12 para o GPR não perder a memória longa
# ? DÚVIDA: Testar se o setor de Consumo inverte o sinal de theta
# TODO: Ortogonalizar o modelo 4 antes de rodar a Máxima Verossimilhança
# * IMPORTANTE: O teste ADF validou o uso de log em nível.

# pacotes
import pandas as pd
import numpy as np
from statsmodels.graphics.tsaplots import plot_acf,plot_pacf
from statsmodels.tsa.stattools import adfuller
from scipy.stats import skew, kurtosis, jarque_bera
import matplotlib.pyplot as plt
from pathlib import Path
from statsmodels.tsa.stattools import kpss
# Carregando base mensal para análise descritiva

base_descritiva = pd.read_excel(
    io = "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados//base_descritiva_mensal.xlsx"
)


#todo ANÁLISE DO GPR E SUAS TRANSFORMAÇÕES

tabela_gpr = base_descritiva[[
    'month',
    'gpr_global',
    'log_gpr_global',
    'd_log_gpr_global']
].copy()

series_gpr = [
    'gpr_global',
    'log_gpr_global',
    'd_log_gpr_global'
]

# Função para as estatísticas descritivas

def descritivas_series(df, colunas):
    resultados = []

    for col in colunas:
        serie = df[col].dropna()
        jb_stat, jb_pvalue = jarque_bera(serie)

        resultados.append({
            "serie": col,
            "n_obs": serie.count(),
            "media": serie.mean(),
            "mediana": serie.median(),
            "minimo": serie.min(),
            "maximo": serie.max(),
            "desvio_padrao": serie.std(),
            "variancia": serie.var(),
            "assimetria": skew(serie),
            "curtose_pearson": kurtosis(serie, fisher=False),
            "jarque_bera": jb_stat,
            "jb_pvalue": jb_pvalue
        })

    return pd.DataFrame(resultados)

desc_gpr = descritivas_series(tabela_gpr, series_gpr)
desc_gpr

#   serie	         n_obs	media	   mediana	    minimo	    maximo	    desvio_padrao	variancia	assimetria	curtose_pearson	jarque_bera	jb_pvalue
#	gpr_global	     324	106.822099	92.977024	45.060562	512.529724	50.872539	    2588.015246	4.056717	28.024149	    9342.483798	0.000000e+00
#	log_gpr_global	 324	4.610995	4.543050	3.829957	6.241308	0.346448	    0.120026	1.079647	6.099726	    192.656545	1.462726e-42
#	d_log_gpr_global 323	0.002222	-0.008759	-0.594270	2.037860	0.228460	    0.052194	2.354644	21.982388	    5147.926170	0.000000e+00

#* ADF do GPR GLOBAL
def teste_adf_series(df, colunas):
    resultados = []

    for col in colunas:
        serie = df[col].dropna()

        adf = adfuller(serie, autolag="AIC")

        resultados.append({
            "serie": col,
            "ADF_stat": adf[0],
            "p_value": adf[1],
            "lags_usados": adf[2],
            "n_obs": adf[3],
            "critico_1%": adf[4]["1%"],
            "critico_5%": adf[4]["5%"],
            "critico_10%": adf[4]["10%"]
        })

    return pd.DataFrame(resultados)

adf_gpr = teste_adf_series(tabela_gpr, series_gpr)

adf_gpr


# serie	            ADF_stat	p_value	      lags_usados	n_obs	critico_1%	critico_5%	critico_10%
# gpr_global	    -6.042269	1.337215e-07	2	         321	-3.450887	-2.870586	-2.571590
# log_gpr_global	-6.300479	3.421938e-08	0	         323	-3.450759	-2.870530	-2.571560
# d_log_gpr_global	-8.404149	2.190607e-13	9	         313	-3.451416	-2.870819	-2.571714



#* KPSS DAS SERIES DO GPR

def teste_kpss_series(df, colunas):
    resultados = []

    for col in colunas:
        serie = df[col].dropna()

        stat, p_value, lags, crit = kpss(
            serie,
            regression="c",
            nlags="auto"
        )

        resultados.append({
            "serie": col,
            "KPSS_stat": stat,
            "p_value": p_value,
            "lags_usados": lags,
            "critico_10%": crit["10%"],
            "critico_5%": crit["5%"],
            "critico_2.5%": crit["2.5%"],
            "critico_1%": crit["1%"]
        })

    return pd.DataFrame(resultados)

kpss_gpr = teste_kpss_series(tabela_gpr, series_gpr)

kpss_gpr

#	serie	         KPSS_stat	p_value	lags_usados	critico_10%	critico_5%	critico_2.5%	critico_1%
#	gpr_global	     0.197222	0.1	     9	        0.347	     0.463	     0.574	         0.739
#	log_gpr_global	 0.210883	0.1	     10	        0.347	     0.463	     0.574	         0.739
#	d_log_gpr_globa  0.048494	0.1	     23	        0.347	     0.463	     0.574	         0.739



#! Ambos o ADF quanto o KPSS reforçam a hipótese de estacionariedade das séries


#* Gráficos das séries do GPR

fig, axes = plt.subplots(3, 1, figsize=(11, 9), sharex=True)

# 1) GPR em nível
media_gpr = tabela_gpr["gpr_global"].mean()
axes[0].plot(tabela_gpr["month"], tabela_gpr["gpr_global"], linewidth=1)
axes[0].axhline(media_gpr, linestyle="--", linewidth=1, label=f"Média = {media_gpr:.2f}", color = 'black')
axes[0].set_title("GPR Global mensal")
axes[0].set_ylabel("Índice")
axes[0].grid(False)
axes[0].legend()

# 2) GPR em log
media_log = tabela_gpr["log_gpr_global"].mean()
axes[1].plot(tabela_gpr["month"], tabela_gpr["log_gpr_global"], linewidth=1)
axes[1].axhline(media_log, linestyle="--", linewidth=1, label=f"Média = {media_log:.2f}", color = 'black')
axes[1].set_title("Log do GPR Global")
axes[1].set_ylabel("log(GPR)")
axes[1].grid(False)
axes[1].legend()

# 3) Diferença logarítmica do GPR
media_dlog = tabela_gpr["d_log_gpr_global"].mean()
axes[2].plot(tabela_gpr["month"], tabela_gpr["d_log_gpr_global"], linewidth=1)
axes[2].axhline(media_dlog, linestyle="--", linewidth=1, label=f"Média = {media_dlog:.3f}", color = 'black')
axes[2].set_title("Variação logarítmica mensal do GPR Global")
axes[2].set_ylabel("Dif. log")
axes[2].set_xlabel("Data")
axes[2].grid(False)
axes[2].legend()

plt.margins(x=0.01)
plt.tight_layout()
plt.savefig(
    "graficos_tabelas/descritiva/series/gpr_transformacoes_series.pdf",
    bbox_inches="tight"
)
plt.show()

#! a utilização do log suaviza os movimentos bruscos da série em nível, tornando mais palatável a estimação
#! além da melhor interpretação econômica utilizando o logaritmico

#* ACFs e PACFs do GPR

fig, axes = plt.subplots(3, 2, figsize=(12, 11))

# -------------------------
# 1) GPR Global em nível
# -------------------------

plot_acf(
    tabela_gpr["gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[0, 0]
)

axes[0, 0].set_title("ACF — GPR Global")
axes[0, 0].set_xlabel("Defasagem mensal")
axes[0, 0].set_ylabel("Autocorrelação")
axes[0, 0].grid(False)

plot_pacf(
    tabela_gpr["gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[0, 1]
)

axes[0, 1].set_title("PACF — GPR Global")
axes[0, 1].set_xlabel("Defasagem mensal")
axes[0, 1].set_ylabel("Autocorrelação parcial")
axes[0, 1].grid(False)


# -------------------------
# 2) Log do GPR Global
# -------------------------

plot_acf(
    tabela_gpr["log_gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[1, 0]
)

axes[1, 0].set_title("ACF — Log do GPR Global")
axes[1, 0].set_xlabel("Defasagem mensal")
axes[1, 0].set_ylabel("Autocorrelação")
axes[1, 0].grid(False)

plot_pacf(
    tabela_gpr["log_gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[1, 1]
)

axes[1, 1].set_title("PACF — Log do GPR Global")
axes[1, 1].set_xlabel("Defasagem mensal")
axes[1, 1].set_ylabel("Autocorrelação parcial")
axes[1, 1].grid(False)


# -------------------------
# 3) Diferença logarítmica
# -------------------------

plot_acf(
    tabela_gpr["d_log_gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[2, 0]
)

axes[2, 0].set_title("ACF — Diferença logarítmica do GPR Global")
axes[2, 0].set_xlabel("Defasagem mensal")
axes[2, 0].set_ylabel("Autocorrelação")
axes[2, 0].grid(False)

plot_pacf(
    tabela_gpr["d_log_gpr_global"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[2, 1]
)

axes[2, 1].set_title("PACF — Diferença logarítmica do GPR Global")
axes[2, 1].set_xlabel("Defasagem mensal")
axes[2, 1].set_ylabel("Autocorrelação parcial")
axes[2, 1].grid(False)


# -------------------------
# Ajustes finais
# -------------------------

fig.suptitle("ACF e PACF das transformações do GPR Global", fontsize=14)

plt.tight_layout(rect=[0, 0, 1, 0.97])

plt.savefig(
    "graficos_tabelas/descritiva/acf/acf_pacf_gpr_transformacoes.pdf",
    bbox_inches="tight"
)

plt.show()

##________________________________________________________________________________________________________________________________________________
##________________________________________________________________________________________________________________________________________________
##________________________________________________________________________________________________________________________________________________
##________________________________________________________________________________________________________________________________________________
##________________________________________________________________________________________________________________________________________________

#TODO =============================================================================
#todo ESTATÍSTICAS DESCRITIVAS DO CÂMBIO
#TODO =============================================================================

tabela_cambio = base_descritiva[[
    'month',
    'var_cambio',
    'log_var_cambio',
    'd_log_var_cambio']
].copy()

series_cambio = [
    'var_cambio',
    'log_var_cambio',
    'd_log_var_cambio'
]

#* Tabela descritiva
desc_cambio = descritivas_series(tabela_cambio, series_cambio)
desc_cambio


#   serie	      n_obs	 media	    mediana	     minimo	     maximo	 desvio_padrao	variancia	assimetria	curtose_pearson	jarque_bera	jb_pvalue
# var_cambio	    324	0.001989	0.001091	0.000043	0.042279	0.003552	0.000013	7.216078	69.968674	63356.719632	0.000000
# log_var_cambio	324	-6.762410	-6.820788	-10.055964	-3.163473	0.961289	0.924077	0.343495	3.879957	16.824783	0.000222
# d_log_var_cambio	323	-0.010613	-0.071713	-2.691355	2.677456	0.835749	0.698477	0.284886	3.612784	9.422769	0.008992


#* ADF DO CAMBIO

adf_cambio = teste_adf_series(tabela_cambio, series_cambio)

adf_cambio


#   serie	         ADF_stat	p_value	     lags_usados	n_obs	critico_1%	critico_5%	critico_10%
#	var_cambio	     -9.988303	2.027630e-17	1	        322	    -3.450823	-2.870558	-2.571575
#	log_var_cambio   -6.959912	9.221370e-10	1	        322	    -3.450823	-2.870558	-2.571575
#	d_log_var_cambio -12.954959	3.315913e-24	3	        319	    -3.451017	-2.870643	-2.571620

#! Em todas as tranformações, a variância do câmbio é estacionária

#* KPSS do cambio

kpss_cambio = teste_kpss_series(tabela_cambio, series_cambio)

kpss_cambio


#   serie	          KPSS_stat	p_value	lags_usados	critico_10%	critico_5%	critico_2.5%	critico_1%
#	var_cambio	      0.201474	0.1	     7	        0.347	    0.463	    0.574	        0.739
#	log_var_cambio	  0.062871	0.1	     9	        0.347 	    0.463	    0.574	        0.739
#	d_log_var_cambio  0.107015	0.1	     21	        0.347	    0.463	    0.574	        0.739

#! Confirma o resultado do ADF


#* Gráficos das séries do Cambio

fig, axes = plt.subplots(3, 1, figsize=(11, 9), sharex=True)

# 1) Câmbio em nível
media_cambio = tabela_cambio["var_cambio"].mean()
axes[0].plot(tabela_cambio["month"], tabela_cambio["var_cambio"], linewidth=1)
axes[0].axhline(media_cambio, linestyle="--", linewidth=1, label=f"Média = {media_cambio:.3f}", color = 'black')
axes[0].set_title("Variância mensal do câmbio")
axes[0].set_ylabel("Variância realizada")
axes[0].grid(False)
axes[0].legend()

# 2) Câmbio em log
media_log = tabela_cambio["log_var_cambio"].mean()
axes[1].plot(tabela_cambio["month"], tabela_cambio["log_var_cambio"], linewidth=1)
axes[1].axhline(media_log, linestyle="--", linewidth=1, label=f"Média = {media_log:.2f}", color = 'black')
axes[1].set_title("Log da variância do câmbio")
axes[1].set_ylabel("log(variância)")
axes[1].grid(False)
axes[1].legend()

# 3) Diferença logarítmica do Câmbio
media_dlog = tabela_cambio["d_log_var_cambio"].mean()
axes[2].plot(tabela_cambio["month"], tabela_cambio["d_log_var_cambio"], linewidth=1)
axes[2].axhline(media_dlog, linestyle="--", linewidth=1, label=f"Média = {media_dlog:.3f}", color = 'black')
axes[2].set_title("Variação logarítmica mensal da variância do câmbio")
axes[2].set_ylabel("Dif. log")
axes[2].set_xlabel("Data")
axes[2].grid(False)
axes[2].legend()

plt.margins(x=0.01)
plt.tight_layout()
plt.savefig(
    "graficos_tabelas/descritiva/series/cambio_transformacoes_series.pdf",
    bbox_inches="tight"
)
plt.show()

#* -----------------------------------------------------------------------------
#* ACF E PACF do Câmbio
#* -----------------------------------------------------------------------------

fig, axes = plt.subplots(3, 2, figsize=(12, 11))

# -------------------------
# 1) Cambio em nível
# -------------------------

plot_acf(
    tabela_cambio["var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[0, 0]
)

axes[0, 0].set_title("ACF — Variância mensal do câmbio")
axes[0, 0].set_xlabel("Defasagem mensal")
axes[0, 0].set_ylabel("Autocorrelação")
axes[0, 0].grid(False)

plot_pacf(
    tabela_cambio["var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[0, 1]
)

axes[0, 1].set_title("PACF — Variância mensal do câmbio")
axes[0, 1].set_xlabel("Defasagem mensal")
axes[0, 1].set_ylabel("Autocorrelação parcial")
axes[0, 1].grid(False)


# -------------------------
# 2) Log do Cambio
# -------------------------

plot_acf(
    tabela_cambio["log_var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[1, 0]
)

axes[1, 0].set_title("ACF — Log da variância do câmbio")
axes[1, 0].set_xlabel("Defasagem mensal")
axes[1, 0].set_ylabel("Autocorrelação")
axes[1, 0].grid(False)

plot_pacf(
    tabela_cambio["log_var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[1, 1]
)

axes[1, 1].set_title("PACF — Log da variância do câmbio")
axes[1, 1].set_xlabel("Defasagem mensal")
axes[1, 1].set_ylabel("Autocorrelação parcial")
axes[1, 1].grid(False)


# -------------------------
# 3) Diferença logarítmica
# -------------------------

plot_acf(
    tabela_cambio["d_log_var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[2, 0]
)

axes[2, 0].set_title("ACF — Diferença logarítmica da variância do câmbio")
axes[2, 0].set_xlabel("Defasagem mensal")
axes[2, 0].set_ylabel("Autocorrelação")
axes[2, 0].grid(False)

plot_pacf(
    tabela_cambio["d_log_var_cambio"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[2, 1]
)

axes[2, 1].set_title("PACF — Diferença logarítmica da variância do câmbio")
axes[2, 1].set_xlabel("Defasagem mensal")
axes[2, 1].set_ylabel("Autocorrelação parcial")
axes[2, 1].grid(False)


# -------------------------
# Ajustes finais
# -------------------------

fig.suptitle("ACF e PACF das transformações da variância do câmbio", fontsize=14)

plt.tight_layout(rect=[0, 0, 1, 0.97])

plt.savefig(
    "graficos_tabelas/descritiva/acf/acf_pacf_cambio_transformacoes.pdf",
    bbox_inches="tight"
)

plt.show()


#todo =============================================================================
#todo ESTATÍSTICAS DESCRITIVAS DO IC-BR
#todo =============================================================================

tabela_ic = base_descritiva[[
    'month',
    'ic_br',
    'log_ic_br',
    'd_log_ic_br']
].copy()

series_ic = [
    'ic_br',
    'log_ic_br',
    'd_log_ic_br'
]

#* Tabela descritiva
desc_ic = descritivas_series(tabela_ic, series_ic)
desc_ic

#	serie	     n_obs	media	    mediana	    minimo	    maximo	    desvio_padrao	variancia	    assimetria	curtose_pearson	jarque_bera	jb_pvalue
#	ic_br	    324	    169.757222	120.350000	43.050000	482.570000	116.018215	    13460.226179	1.252045	3.250142	    85.496029	2.721280e-19
#	log_ic_br	324	    4.936159	4.790402	3.762362	6.179126	0.612934	    0.375689	    0.430206	2.357710	    15.563400	4.173022e-04
#	d_log_ic_br	323	    0.007138	0.003522	-0.124452	0.210191	0.039637	    0.001571	    0.702591	5.420821	    105.444843	1.267486e-23



#* ADF DO IC-BR

adf_ic = teste_adf_series(tabela_ic, series_ic)

adf_ic


#	serie	    ADF_stat	p_value	      lags_usados	n_obs	critico_1%	critico_5%	critico_10%
#	ic_br	     0.962724	9.938566e-01	14	        309	    -3.451691	-2.870939	-2.571778
#	log_ic_br	-0.259493	9.310486e-01	1	        322	    -3.450823	-2.870558	-2.571575
#	d_log_ic_br	-14.495834	6.043263e-27	0	        322	    -3.450823	-2.870558	-2.571575

#! Para nível e log, IC-BR não é estacionário

#* KPSS do IC-BR

kpss_ic = teste_kpss_series(tabela_ic, series_ic)

kpss_ic

#	serie	   KPSS_stat	p_value	lags_usados	critico_10%	critico_5%	critico_2.5%	critico_1%
#	ic_br	    2.211930	0.01	11	        0.347	     0.463	      0.574	        0.739
#	log_ic_br	2.544481	0.01	11	        0.347	     0.463	      0.574	        0.739
#	d_log_ic_br	0.058149	0.10	4	        0.347	     0.463	      0.574	        0.739


#! Reforça o resultado do ADF


#* Gráficos das transformações do IC-BR

fig, axes = plt.subplots(3, 1, figsize=(11, 9), sharex=True)

# 1) Câmbio em nível
media_ic = tabela_ic["ic_br"].mean()
axes[0].plot(tabela_ic["month"], tabela_ic["ic_br"], linewidth=1)
axes[0].set_title("Índice de Commodities (IC-Br)")
axes[0].set_ylabel("Índice")
axes[0].grid(False)

# 2) Câmbio em log
media_log = tabela_ic["log_ic_br"].mean()
axes[1].plot(tabela_ic["month"], tabela_ic["log_ic_br"], linewidth=1)
axes[1].set_title("Log do Índice de Commodities")
axes[1].set_ylabel("log(IC-Br)")
axes[1].grid(False)

# 3) Diferença logarítmica do Câmbio
media_dlog = tabela_ic["d_log_ic_br"].mean()
axes[2].plot(tabela_ic["month"], tabela_ic["d_log_ic_br"], linewidth=1)
axes[2].axhline(media_dlog, linestyle="--", linewidth=1, label=f"Média = {media_dlog:.3f}", color = 'black')
axes[2].set_title("Variação logarítmica do Índice de Commodities")
axes[2].set_ylabel("Dif. log")
axes[2].set_xlabel("Data")
axes[2].grid(False)
axes[2].legend()

plt.margins(x=0.01)
plt.tight_layout()
plt.savefig(
    "graficos_tabelas/descritiva/series/ic_br_transformacoes_series.pdf",
    bbox_inches="tight"
)
plt.show()



#* -----------------------------------------------------------------------------
#* ACF E PACF do IC-Br
#* -----------------------------------------------------------------------------

fig, axes = plt.subplots(3, 2, figsize=(12, 11))

# -------------------------
# 1) IC-Br em nível
# -------------------------

plot_acf(
    tabela_ic["ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[0, 0]
)

axes[0, 0].set_title("ACF — Índice de Commodities (IC-Br)")
axes[0, 0].set_xlabel("Defasagem mensal")
axes[0, 0].set_ylabel("Autocorrelação")
axes[0, 0].grid(False)

plot_pacf(
    tabela_ic["ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[0, 1]
)

axes[0, 1].set_title("PACF — Índice de Commodities (IC-Br)")
axes[0, 1].set_xlabel("Defasagem mensal")
axes[0, 1].set_ylabel("Autocorrelação parcial")
axes[0, 1].grid(False)


# -------------------------
# 2) Log do IC-Br
# -------------------------

plot_acf(
    tabela_ic["log_ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[1, 0]
)

axes[1, 0].set_title("ACF — Log do IC-Br")
axes[1, 0].set_xlabel("Defasagem mensal")
axes[1, 0].set_ylabel("Autocorrelação")
axes[1, 0].grid(False)

plot_pacf(
    tabela_ic["log_ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[1, 1]
)

axes[1, 1].set_title("PACF — Log do IC-Br")
axes[1, 1].set_xlabel("Defasagem mensal")
axes[1, 1].set_ylabel("Autocorrelação parcial")
axes[1, 1].grid(False)


# -------------------------
# 3) Diferença logarítmica
# -------------------------

plot_acf(
    tabela_ic["d_log_ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    ax=axes[2, 0]
)

axes[2, 0].set_title("ACF — Diferença logarítmica do IC-Br")
axes[2, 0].set_xlabel("Defasagem mensal")
axes[2, 0].set_ylabel("Autocorrelação")
axes[2, 0].grid(False)

plot_pacf(
    tabela_ic["d_log_ic_br"].dropna(),
    lags=24,
    alpha=0.05,
    zero=False,
    method="ywm",
    ax=axes[2, 1]
)

axes[2, 1].set_title("PACF — Diferença logarítmica do IC-Br")
axes[2, 1].set_xlabel("Defasagem mensal")
axes[2, 1].set_ylabel("Autocorrelação parcial")
axes[2, 1].grid(False)


# -------------------------
# Ajustes finais
# -------------------------

fig.suptitle("ACF e PACF das transformações do Índice de Commodities (IC-Br)", fontsize=14)

plt.tight_layout(rect=[0, 0, 1, 0.97])

plt.savefig(
    "graficos_tabelas/descritiva/acf/acf_pacf_ic_br_transformacoes.pdf",
    bbox_inches="tight"
)

plt.show()






































# ACFs

print(base_descritiva.columns)

# ACF do GPR Brasil

fig, axes = plt.subplots(4, 1, figsize=(12, 10))

plot_acf(
    base_descritiva["gpr_bra"].dropna(),
    lags=48,
    ax=axes[0]
)
axes[0].set_title("ACF - GPR Brasil em nível")

plot_acf(
    base_descritiva["d_gpr_bra"].dropna(),
    lags=48,
    ax=axes[1]
)
axes[1].set_title("ACF - Diferença simples do GPR Brasil")

plot_acf(
    base_descritiva["log_gpr_bra"].dropna(),
    lags=48,
    ax=axes[2]
)
axes[2].set_title("ACF - Log GPR Brasil")

plot_acf(
    base_descritiva["d_log_gpr_bra"].dropna(),
    lags=48,
    ax=axes[3]
)
axes[3].set_title("ACF - Log-diferença do GPR Brasil")

plt.tight_layout()
plt.show()

# Salvando
from pathlib import Path

pasta_descritiva = Path("graficos_tabelas/descritiva/acf")
pasta_descritiva.mkdir(parents=True, exist_ok=True)

fig.savefig(pasta_descritiva / "acf_gpr_brasil.png", dpi=300)
plt.close(fig)


## TABELA DE CORRELAÇÃO ENTRE AS VARIAVEIS DE LONGO PRAZO

base_corr = base_descritiva[[
    "d_log_gpr_global",
    "log_var_cambio",
    "d_log_ic_br"
]].copy()

corr = base_corr.corr().round(3)

print(corr)

#                  d_log_gpr_global  log_var_cambio  d_log_ic_br
# d_log_gpr_global             1.000          -0.031       -0.028
# log_var_cambio              -0.031           1.000        0.188
# d_log_ic_br                 -0.028           0.188        1.000


## TESTE ADF DAS SERIES

# GPR Global em log
# Resultado: Estacionário
adf_log_gpr = adfuller(base_descritiva["log_gpr_global"].dropna())
print(f"Estatística ADF: {adf_log_gpr [0]:.4f}")
print(f"P-value: {adf_log_gpr [1]:.4f}")
print(f"Valores críticos: {adf_log_gpr [4]}")


# GPR Global em log-diff
# Resultado: Estacionário
adf_d_log_gpr = adfuller(base_descritiva["d_log_gpr_global"].dropna())
print(f"Estatística ADF: {adf_d_log_gpr[0]:.4f}")
print(f"P-value: {adf_d_log_gpr[1]:.4f}")
print(f"Valores críticos: {adf_d_log_gpr[4]}")




# Cambio em nivel

adf_cambio = adfuller(base_descritiva["var_cambio"].dropna())
print(f"Estatística ADF: {adf_cambio[0]:.4f}")
print(f"P-value: {adf_cambio[1]:.4f}")
print(f"Valores críticos: {adf_cambio[4]}")

# Cambio em log
# Resultado: Estacionário
adf_log_cambio = adfuller(base_descritiva["log_var_cambio"].dropna())
print(f"Estatística ADF: {adf_log_cambio[0]:.4f}")
print(f"P-value: {adf_log_cambio[1]:.4f}")
print(f"Valores críticos: {adf_log_cambio[4]}")

# Cambio em log-DIFF
# Resultado: Estacionário
adf_d_log_cambio = adfuller(base_descritiva["d_log_var_cambio"].dropna())
print(f"Estatística ADF: {adf_d_log_cambio[0]:.4f}")
print(f"P-value: {adf_d_log_cambio[1]:.4f}")
print(f"Valores críticos: {adf_d_log_cambio[4]}")


# IC-BR em log
# Resultado: Não estacionário
adf_log_ic_br = adfuller(base_descritiva["log_ic_br"].dropna())
print(f"Estatística ADF: {adf_log_ic_br[0]:.4f}")
print(f"P-value: {adf_log_ic_br[1]:.4f}")
print(f"Valores críticos: {adf_log_ic_br[4]}")

# IC-BR em log-diff
# Resultado: Estacionário
adf_d_log_ic_br = adfuller(base_descritiva["d_log_ic_br"].dropna())
print(f"Estatística ADF: {adf_d_log_ic_br[0]:.4f}")
print(f"P-value: {adf_d_log_ic_br[1]:.4f}")
print(f"Valores críticos: {adf_d_log_ic_br[4]}")
