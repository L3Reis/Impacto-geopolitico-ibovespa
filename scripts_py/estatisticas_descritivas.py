# Estatísticas descritivas das séries

# Instruções Better Comments
# ! CRÍTICO: Lembre-se de manter K = 12 para o GPR não perder a memória longa
# ? DÚVIDA: Testar se o setor de Consumo inverte o sinal de theta
# TODO: Ortogonalizar o modelo 4 antes de rodar a Máxima Verossimilhança
# * IMPORTANTE: O teste ADF validou o uso de log em nível.

# pacotes
import pandas as pd
import numpy as np
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import adfuller
import matplotlib.pyplot as plt
from pathlib import Path

# Carregando base mensal para análise descritiva

base_descritiva = pd.read_excel(
    io = "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados//base_descritiva_mensal.xlsx"
)

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
