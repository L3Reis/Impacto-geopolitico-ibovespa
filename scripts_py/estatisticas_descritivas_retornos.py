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

# Aparentemente está tudo correto, só analisar posteriormente os valores max e min

#* Teste Jarque-Bera

jb_stat, jb_pvalue = jarque_bera(ibov_100)

teste_jb_ibov = pd.DataFrame({
    "JB_stat": [jb_stat],
    "JB_pvalue": [jb_pvalue]
})

teste_jb_ibov

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
#	lb_stat	lb_pvalue
# 5	1176.758703	3.177247e-252
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
