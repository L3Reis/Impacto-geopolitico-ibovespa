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