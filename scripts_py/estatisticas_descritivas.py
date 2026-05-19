# Estatísticas descritivas das séries

# pacotes
import pandas as pd
import numpy as np
from statsmodels.graphics.tsaplots import plot_acf
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
