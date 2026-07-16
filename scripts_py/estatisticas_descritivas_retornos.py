# pacotes
import pandas as pd
import numpy as np
from statsmodels.tsa.stattools import adfuller
import matplotlib.pyplot as plt
from pathlib import Path
from scipy.stats import skew, kurtosis, jarque_bera
from statsmodels.stats.diagnostic import acorr_ljungbox, het_arch
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import statsmodels.api as sm


#todo Pegando o log-retorno do ibovespa
ibov = pd.read_excel(
    io = "dados/base_mfgarch_completa.xlsx"
)

# No GARCH Entra multiplicado por 100
ibov_100 = ibov['ret_ibov'] * 100

#* Auditoria inicial
print(ibov_100.info())
print(ibov_100.isna().sum())
print(ibov_100.index.duplicated().sum())

print(
    ibov_100.replace([np.inf, -np.inf], np.nan)
    .isna()
    .sum()
)

# tudo correto

#* Descritivas básicas

descritivas_ibov = ibov_100.describe(
    percentiles=[0.01, 0.05, 0.25, 0.75, 0.95, 0.99]
).T

descritivas_ibov["mediana"] = ibov_100.median()
descritivas_ibov["assimetria"] = ibov_100.skew()
descritivas_ibov["curtose excedente"] = ibov_100.kurt()
descritivas_ibov["curtose de Pearson"] = ibov_100.kurt() + 3
print(descritivas_ibov)

#count                 6686.000000
#mean                     0.047032
#std                      1.764330
#min                    -15.993027
#1%                      -4.595121
#5%                      -2.682798
#25%                     -0.864089
#75%                      1.001650
#95%                      2.588682
#99%                      4.453841
#max                     28.832454
#mediana                  0.071546
#assimetria               0.330881
#curtose excedente       16.604249
#curtose de Pearson      19.604249



# Aparentemente está tudo correto, só analisar posteriormente os valores max e min

#* Teste Jarque-Bera

jb_stat, jb_pvalue = jarque_bera(ibov_100)

teste_jb_ibov = pd.DataFrame({
    "JB_stat": [jb_stat],
    "JB_pvalue": [jb_pvalue]
})

teste_jb_ibov


#    JB_stat	     JB_pvalue
#	76804.567377	0.0


# interpretação

if jb_pvalue < 0.05:
    print("Rejeita normalidade ao nível de 5%.")
else:
    print("Não rejeita normalidade ao nível de 5%.")

# Rejeita normalidade ao nível de 5%. O que é esperado para séries financeiras

#* Teste ADF - verificar estacionaridade

adf_result = adfuller(ibov_100, autolag="AIC")

teste_adf_ibov = pd.DataFrame({
    "ADF_stat": [adf_result[0]],
    "p_value": [adf_result[1]],
    "lags_usados": [adf_result[2]],
    "n_obs": [adf_result[3]],
    "critico_1%": [adf_result[4]["1%"]],
    "critico_5%": [adf_result[4]["5%"]],
    "critico_10%": [adf_result[4]["10%"]]
})

teste_adf_ibov
# ADF_stat	    p_value	   lags_usados	n_obs	critico_1%	critico_5%	critico_10%
# -18.699129	2.037376e-30	15	    6670	-3.431331	-2.861973	-2.567001

if adf_result[1] < 0.01:
    print("Rejeita raiz unitária: a série é estacionária ao nível de 1%.")
else:
    print("Não rejeita raiz unitária: possível não estacionariedade.")

# Rejeita raiz unitária: a série é estacionária ao nível de 1%.


#* Teste Ljung-Box - testar autocorrelação conjunta até determinadas defasagens

ljung_ibov = acorr_ljungbox(
    ibov_100,
    lags=[5, 10, 20],
    return_df=True
)

ljung_ibov
#	    lb_stat	    lb_pvalue
# 5	    14.178354	0.014515
# 10	35.709255	0.000094
# 20	51.891852	0.000118

# Nos lags, rejeitamos ausência de correlação - Então há autocorrelação

# Para verificar os retornos ao quadrado

ljung_ibov_quad = acorr_ljungbox(
    ibov_100**2,
    lags=[5, 10, 20],
    return_df=True
)

ljung_ibov_quad
#	    lb_stat	    lb_pvalue
# 5	    1176.758703	3.177247e-252
# 10	1616.897137	0.000000e+00
# 20	1897.890494	0.000000e+00

#! Existe autocorrelação nos retornos ao quadrado, justificando o uso do GARCH, pois é uma evidência de clusterização de volatilidade

#* Teste ARCH

arch_stat, arch_pvalue, _, _ = het_arch(ibov_100, nlags=10)

teste_arch_ibov = pd.DataFrame({
    "ARCH_stat": [arch_stat],
    "ARCH_pvalue": [arch_pvalue]
})

teste_arch_ibov

#   ARCH_stat	ARCH_pvalue
#	1353.508873	1.079858e-284

# Rejeita ausência de efeitos ARCH: há evidência de heterocedasticidade condicional.

#* Gráfico da série

serie_ibov= pd.DataFrame({
    'date': ibov["date"].values,
    'ibov_100': ibov_100.values
})

plt.figure(figsize= (12,5))
plt.plot(serie_ibov['date'],serie_ibov['ibov_100'])
plt.xlabel('Data')
plt.ylabel("Retorno diário (%)")
plt.grid(False)
plt.margins(x= 0.01)

plt.savefig("graficos_tabelas/descritiva/series/log_retornos_ibovespa.pdf", bbox_inches="tight")

plt.show()

#* Histograma do Ibov
plt.figure(figsize=(8, 5))
plt.hist(ibov_100, bins=50, density=True)
plt.xlabel("Retorno diário (%)")
plt.ylabel("Densidade")
plt.grid(False)

plt.savefig("graficos_tabelas/descritiva/series/histograma_ibovespa.pdf", bbox_inches="tight")
plt.show()

#* Boxplot do Ibov

plt.figure( figsize=(6,5))
plt.boxplot(
    ibov_100,
    vert=False,
    patch_artist=True,
    widths=0.5,
    boxprops=dict(facecolor="lightgray", edgecolor="black", linewidth=1),
    medianprops=dict(color="darkblue", linewidth=2),
    whiskerprops=dict(color="black", linewidth=1),
    capprops=dict(color="black", linewidth=1),
    flierprops=dict(
        marker='o',
        markerfacecolor='white',
        markeredgecolor='black',
        markersize=4,
        linestyle='none',
        alpha=0.6
    )
)

plt.title("Boxplot dos retornos diários do Ibovespa")
plt.xlabel("Retorno diário (%)")
plt.yticks([])
plt.grid(False)
plt.tight_layout()

plt.savefig("graficos_tabelas/descritiva/series/boxplot_ibovespa.pdf", bbox_inches="tight")
plt.show()


#* ACF dos retornos e retornos ao quadrado

fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))

# ACF dos retornos
plot_acf(
    ibov_100,
    lags=40,
    zero=False,
    alpha=0.05,
    ax=axes[0]
)

axes[0].set_title("Retornos")
axes[0].set_xlabel("Defasagem diária")
axes[0].set_ylabel("Autocorrelação")
axes[0].set_ylim(-0.10, 0.10)

# ACF dos retornos ao quadrado
plot_acf(
    ibov_100**2,
    lags=40,
    zero=False,
    alpha=0.05,
    ax=axes[1]
)

axes[1].set_title("Retornos ao quadrado")
axes[1].set_xlabel("Defasagem diária")
axes[1].set_ylabel("Autocorrelação")
axes[1].set_ylim(-0.05, 0.30)

plt.tight_layout()

# Salvar em alta resolução
plt.savefig(
    "graficos_tabelas/descritiva/acf/acf_retornos_ibovespa.pdf",
    bbox_inches="tight"
)

plt.show()


# =============================================================================
# Estatísticas descritivas para os setores
# =============================================================================

setores = pd.read_excel(
    io = "dados/base_setores_final.xlsx"
)

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

tabela_setores = setores[[
    'date',
    'consumer_log_ret',
    'basic_products_log_ret',
    'finance_log_ret',
    'energy_log_ret']
].copy()

series_setores = [
    'consumer_log_ret',
    'basic_products_log_ret',
    'finance_log_ret',
    'energy_log_ret'
]

desc_setores = descritivas_series(tabela_setores, series_setores)
desc_setores

#	serie	               n_obs	media	mediana	      minimo	maximo	  desvio_padrao	variancia	assimetria	curtose_pearson	jarque_bera	jb_pvalue
#	consumer_log_ret	   6423	 0.028069	0.062133	-16.566528	12.373398	1.476483	2.180003	-0.411780	11.875032	   21261.317130	0.0
#	basic_products_log_ret 6423	 0.062602	0.066888	-17.032132	16.010241	1.994225	3.976933	-0.186911	8.704274	   8745.578106	0.0
#	finance_log_ret	       6423	 0.062254	0.067549	-13.977472	19.783081	1.942661	3.773932	 0.078823	8.482458	   050.749450	0.0
#	energy_log_ret	       6423	 0.053331	0.093939	-20.649959	14.633259	1.949480	3.800474	-0.546722	10.157747	   14031.299493	0.0


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

adf_setores = teste_adf_series(tabela_setores, series_setores)

adf_setores    

#	serie	               ADF_stat	     p_value	lags_usados	n_obs	critico_1%	critico_5%	critico_10%
#	consumer_log_ret	   -17.930446	2.884509e-30	17	    6405	-3.431371	-2.861991	-2.567010
#	basic_products_log_ret -19.844302	0.000000e+00	15	    6407	-3.431371	-2.861991	-2.567010
#	finance_log_ret	       -16.287805	3.361792e-29	24	    6398	-3.431372	-2.861992	-2.567011
#	energy_log_ret	       -29.754190	0.000000e+00	6	    6416	-3.431370	-2.861991	-2.567010

#* Teste ARCH

def teste_arch_series(df, colunas, nlags=10):
    resultados = []

    for col in colunas:
        serie = (
            df[col]
            .replace([np.inf, -np.inf], np.nan)
            .dropna()
        )

        arch_stat, arch_pvalue, _, _ = het_arch(serie, nlags=nlags)

        resultados.append({
            "serie": col,
            "n_obs": serie.count(),
            "ARCH_stat": arch_stat,
            "ARCH_pvalue": arch_pvalue,
            "nlags": nlags,
            "conclusao_5%": (
                "Rejeita H0: há efeitos ARCH"
                if arch_pvalue < 0.05
                else "Não rejeita H0"
            )
        })

    return pd.DataFrame(resultados)


teste_arch_setores = teste_arch_series(
    tabela_setores,
    series_setores,
    nlags=10
)

teste_arch_setores

#	serie	               n_obs	ARCH_stat	  ARCH_pvalue	nlags	conclusao_5%
#	consumer_log_ret	    6423	1646.930347	 0.000000e+00	10	    Rejeita H0: há efeitos ARCH
#	basic_products_log_ret	6423	1188.474911	 4.410718e-249	10	    Rejeita H0: há efeitos ARCH
#	finance_log_ret	        6423	1301.706549	 1.638357e-273	10	    Rejeita H0: há efeitos ARCH
#	energy_log_ret	        6423	1466.473344	 4.390332e-309	10	    Rejeita H0: há efeitos ARCH



#* Teste Ljung-Box - testar autocorrelação conjunta até determinadas defasagens

ljung_ibov = acorr_ljungbox(
    ibov_100,
    lags=[5, 10, 20],
    return_df=True
)

ljung_ibov
#	    lb_stat	    lb_pvalue
# 5	    14.178354	0.014515
# 10	35.709255	0.000094
# 20	51.891852	0.000118

# Nos lags, rejeitamos ausência de correlação - Então há autocorrelação

# Para verificar os retornos ao quadrado

ljung_ibov_quad = acorr_ljungbox(
    ibov_100**2,
    lags=[5, 10, 20],
    return_df=True
)

ljung_ibov_quad

def teste_ljungbox_series(df, colunas, lags=[5, 10, 20], ao_quadrado=False):
    resultados = []

    for col in colunas:
        serie = (
            df[col]
            .replace([np.inf, -np.inf], np.nan)
            .dropna()
        )

        if ao_quadrado:
            serie_teste = serie ** 2
            tipo = "retornos_ao_quadrado"
        else:
            serie_teste = serie
            tipo = "retornos"

        ljung = acorr_ljungbox(
            serie_teste,
            lags=lags,
            return_df=True
        )

        for lag, row in ljung.iterrows():
            resultados.append({
                "serie": col,
                "tipo": tipo,
                "lag": lag,
                "LB_stat": row["lb_stat"],
                "LB_pvalue": row["lb_pvalue"],
                "conclusao_5%": (
                    "Rejeita H0: há autocorrelação"
                    if row["lb_pvalue"] < 0.05
                    else "Não rejeita H0"
                )
            })

    return pd.DataFrame(resultados)


ljung_setores = teste_ljungbox_series(
    tabela_setores,
    series_setores,
    lags=[5, 10, 20],
    ao_quadrado=False
)

ljung_setores


#   serie	                tipo	   lag	LB_stat	   LB_pvalue	conclusao_5%
#	consumer_log_ret	    retornos	5	13.626653	0.018163	Rejeita H0: há autocorrelação
#	consumer_log_ret	    retornos	10	33.147896	0.000257	Rejeita H0: há autocorrelação
#	consumer_log_ret	    retornos	20	57.448879	0.000018	Rejeita H0: há autocorrelação
#	basic_products_log_ret	retornos	5	14.066716	0.015191	Rejeita H0: há autocorrelação
#	basic_products_log_ret	retornos	10	27.414134	0.002239	Rejeita H0: há autocorrelação
#	basic_products_log_ret	retornos	20	48.837330	0.000324	Rejeita H0: há autocorrelação
#	finance_log_ret	        retornos	5	17.246675	0.004055	Rejeita H0: há autocorrelação
#	finance_log_ret	        retornos	10	31.737207	0.000443	Rejeita H0: há autocorrelação
#	finance_log_ret	        retornos	20	49.748419	0.000241	Rejeita H0: há autocorrelação
#	energy_log_ret	        retornos	5	5.564580	0.350916	Não rejeita H0
#	energy_log_ret	        retornos	10	24.439508	0.006515	Rejeita H0: há autocorrelação
#	energy_log_ret	        retornos	20	32.758286	0.035859	Rejeita H0: há autocorrelação



ljung_setores_quad = teste_ljungbox_series(
    tabela_setores,
    series_setores,
    lags=[5, 10, 20],
    ao_quadrado=True
)

ljung_setores_quad


# serie	                   tipo	               lag	LB_stat	    LB_pvalue	conclusao_5%
#	consumer_log_ret	   retornos_ao_quadrado	5	3189.916711	0.0	    Rejeita H0: há autocorrelação
#	consumer_log_ret	   retornos_ao_quadrado	10	3896.565493	0.0	    Rejeita H0: há autocorrelação
#	consumer_log_ret	   retornos_ao_quadrado	20	4609.501774	0.0	    Rejeita H0: há autocorrelação
#	basic_products_log_ret retornos_ao_quadrado	5	2346.750044	0.0	    Rejeita H0: há autocorrelação
#	basic_products_log_ret retornos_ao_quadrado	10	3181.980970	0.0	    Rejeita H0: há autocorrelação
#	basic_products_log_ret retornos_ao_quadrado	20	4082.605697	0.0	    Rejeita H0: há autocorrelação
#	finance_log_ret	       retornos_ao_quadrado	5	2038.776000	0.0	    Rejeita H0: há autocorrelação
#	finance_log_ret	       retornos_ao_quadrado	10	3720.383707	0.0	    Rejeita H0: há autocorrelação
#	finance_log_ret	       retornos_ao_quadrado	20	5269.756574	0.0	    Rejeita H0: há autocorrelação
#	energy_log_ret	       retornos_ao_quadrado	5	2999.621353	0.0	    Rejeita H0: há autocorrelação
#	energy_log_ret	       retornos_ao_quadrado	10	3843.769374	0.0	    Rejeita H0: há autocorrelação
#	energy_log_ret	       retornos_ao_quadrado	20	4421.507182	0.0	    Rejeita H0: há autocorrelação


#* Gráfico das séries do Ibovespa

fig, axes = plt.subplots(4, 1, figsize=(11, 9), sharex=True)

# 1) Consumo
axes[0].plot(tabela_setores['date'], tabela_setores['consumer_log_ret'], linewidth=1)
# axes[0].axhline(media_gpr, linestyle="--", linewidth=1, label=f"Média = {media_gpr:.2f}", color = 'black')
axes[0].set_title("Setor - Consumo")
axes[0].set_ylabel("Retorno diário (%)")
axes[0].grid(False)
# axes[0].legend()

# 2) Bens básicos
axes[1].plot(tabela_setores['date'], tabela_setores['basic_products_log_ret'], linewidth=1)
# axes[1].axhline(media_log, linestyle="--", linewidth=1, label=f"Média = {media_log:.2f}", color = 'black')
axes[1].set_title("Setor - Bens Básicos")
axes[1].set_ylabel("Retorno diário (%)")
axes[1].grid(False)
# axes[1].legend()

# 3) Financeiro
axes[2].plot(tabela_setores['date'], tabela_setores['finance_log_ret'], linewidth=1)
# axes[2].axhline(media_dlog, linestyle="--", linewidth=1, label=f"Média = {media_dlog:.3f}", color = 'black')
axes[2].set_title("Setor - Financeiro")
axes[2].set_ylabel("Retorno diário (%)")
# axes[2].set_xlabel("Data")
axes[2].grid(False)
# axes[2].legend()

# 3) Energia
axes[3].plot(tabela_setores['date'], tabela_setores['energy_log_ret'], linewidth=1)
# axes[3].axhline(media_dlog, linestyle="--", linewidth=1, label=f"Média = {media_dlog:.3f}", color = 'black')
axes[3].set_title("Setor - Energia")
axes[3].set_ylabel("Retorno diário (%)")
axes[3].set_xlabel("Data")
axes[3].grid(False)
# axes[3].legend()

plt.margins(x=0.01)
plt.tight_layout()
plt.savefig(
    "graficos_tabelas/descritiva/series/retornos_setores.pdf",
    bbox_inches="tight"
)
plt.show()

#* ACF dos setores

fig, axes = plt.subplots(4, 2, figsize=(12, 11))

# -------------------------
# 1) Consumo
# -------------------------

plot_acf(
    tabela_setores['consumer_log_ret'].dropna(),
    lags= 40,
    alpha=0.05,
    zero=False,
    ax=axes[0, 0]
)

axes[0, 0].set_title("Consumo")
axes[0, 0].set_xlabel("Defasagem diária")
axes[0, 0].set_ylabel("Autocorrelação")
axes[0, 0].set_ylim(-0.10, 0.10)
axes[0, 0].grid(False)

plot_acf(
    tabela_setores['consumer_log_ret'].dropna()**2,
    lags= 40,
    alpha=0.05,
    zero=False,
    ax=axes[0, 1]
)

axes[0, 1].set_title("Consumo ao quadrado")
axes[0, 1].set_xlabel("Defasagem diária")
axes[0, 1].set_ylabel("Autocorrelação")
axes[0, 1].set_ylim(-0.10, 0.50)
axes[0, 1].grid(False)


# -------------------------
# 2) Bens Básicos
# -------------------------

plot_acf(
    tabela_setores['basic_products_log_ret'].dropna(),
    lags=40,
    alpha=0.05,
    zero=False,
    ax=axes[1, 0]
)

axes[1, 0].set_title("Bens Básicos")
axes[1, 0].set_xlabel("Defasagem diária")
axes[1, 0].set_ylabel("Autocorrelação")
axes[1, 0].set_ylim(-0.10, 0.10)
axes[1, 0].grid(False)

plot_acf(
    tabela_setores['basic_products_log_ret'].dropna()**2,
    lags= 40,
    alpha=0.05,
    zero=False,
    ax=axes[1, 1]
)

axes[1, 1].set_title("Bens Básicos ao quadrado")
axes[1, 1].set_xlabel("Defasagem diária")
axes[1, 1].set_ylabel("Autocorrelação")
axes[1, 1].set_ylim(-0.10, 0.50)
axes[1, 1].grid(False)


# -------------------------
# 3) Financeiro
# -------------------------

plot_acf(
    tabela_setores['finance_log_ret'].dropna(),
    lags=40,
    alpha=0.05,
    zero=False,
    ax=axes[2, 0]
)

axes[2, 0].set_title("Financeiro")
axes[2, 0].set_xlabel("Defasagem diária")
axes[2, 0].set_ylabel("Autocorrelação")
axes[2, 0].set_ylim(-0.10, 0.10)
axes[2, 0].grid(False)

plot_acf(
    tabela_setores['finance_log_ret'].dropna()**2,
    lags=40,
    alpha=0.05,
    zero=False,
    ax=axes[2, 1]
)

axes[2, 1].set_title("Financeiro ao quadrado")
axes[2, 1].set_xlabel("Defasagem diária")
axes[2, 1].set_ylabel("Autocorrelação")
axes[2, 1].set_ylim(-0.10, 0.50)
axes[2, 1].grid(False)


# -------------------------
# 4) Energia
# -------------------------

plot_acf(
    tabela_setores['energy_log_ret'].dropna(),
    lags=40,
    alpha=0.05,
    zero=False,
    ax=axes[3, 0]
)

axes[3, 0].set_title("Energia")
axes[3, 0].set_xlabel("Defasagem diária")
axes[3, 0].set_ylabel("Autocorrelação")
axes[3, 0].set_ylim(-0.10, 0.10)
axes[3, 0].grid(False)

plot_acf(
    tabela_setores['energy_log_ret'].dropna()**2,
    lags=40,
    alpha=0.05,
    zero=False,
    ax=axes[3, 1]
)

axes[3, 1].set_title("Energia ao quadrado")
axes[3, 1].set_xlabel("Defasagem diária")
axes[3, 1].set_ylabel("Autocorrelação")
axes[3, 1].set_ylim(-0.10, 0.50)
axes[3, 1].grid(False)

# -------------------------
# Ajustes finais
# -------------------------

#fig.suptitle("ACF e PACF das transformações do GPR Global", fontsize=14)

plt.tight_layout(rect=[0, 0, 1, 0.97])

plt.savefig(
    "graficos_tabelas/descritiva/acf/acf_setores.pdf",
    bbox_inches="tight"
)

plt.show()