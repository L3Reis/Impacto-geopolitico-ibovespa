# Pacotes
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt
from bcb import sgs
import plotnine as p9

# Carregando dados

## Ibovespa
ibov = yf.download("^BVSP", start = '1999-01-01', end = '2025-12-31')
preco = ibov["Close"] # equivalente ao select do R

# Log-retorno
retorno = np.log(preco / preco.shift(1))
retorno = retorno.dropna()

print(retorno.head()) 
print(retorno.shape)

# Visualização
retorno['^BVSP'].plot(figsize=(10, 5))

plt.title("Log-retorno Ibovespa")
plt.xlabel("Data")
plt.ylabel("Log-retorno")

plt.tight_layout()
plt.show()
plt.close()


## Cambio
cambio = yf.download("BRL=X", start = '1999-01-01', end = '2025-12-31')


# Retorno

retorno_cambio = np.log(cambio['Close'] / cambio['Close'].shift(1)).dropna()

# Variancia Realizada
var_cambio = (retorno_cambio**2).resample("ME").sum()


# visualização cambio
var_cambio["BRL=X"].plot(figsize=(10, 5))

plt.title("Variância realizada mensal do câmbio")
plt.xlabel("Data")
plt.ylabel("Variância realizada")

plt.tight_layout()
plt.show()
plt.close()

## Indice de Commodities
comm = sgs.get({"ic_br": 27574}, start = '1999-01-01', end = '2025-12-31')

comm.plot()

# Variação mensal = log-retorno
ret_icbr = np.log(comm/ comm.shift(1)).dropna()

ret_icbr.plot()


## GPR Brasil

arquivo = "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/data_gpr_export.xls"

gpr_raw = pd.read_excel(
    arquivo,
    sheet_name="Sheet1",
    engine="xlrd"
)

print(gpr_raw.head())
print(gpr_raw.columns)

## Ver colunas relacionadas ao Brasil

colunas_brasil = [col for col in gpr_raw.columns if "BRA" in str(col)]

print(colunas_brasil)

# Extrair GPR Brasil

gpr_bra = gpr_raw[["month", "GPRC_BRA"]].copy()

gpr_bra = gpr_bra.rename(columns={
    "month": "month",
    "GPRC_BRA": "gpr_bra"
})

gpr_bra["month"] = pd.to_datetime(gpr_bra["month"], errors="coerce")

gpr_bra = gpr_bra.dropna()
gpr_bra = gpr_bra.sort_values("month")

print(gpr_bra.head())
print(gpr_bra.tail())

## Filtrar período da dissertação

gpr_bra = gpr_bra[
    (gpr_bra["month"] >= "1999-01-01") &
    (gpr_bra["month"] <= "2025-12-31")
].copy()

print(gpr_bra.head())
print(gpr_bra.tail())
print(gpr_bra.shape)

# %% Gráfico GPR Brasil

plt.figure(figsize=(10, 5))

plt.plot(gpr_bra["month"], gpr_bra["gpr_bra"])

plt.title("Índice de Risco Geopolítico - Brasil")
plt.xlabel("Data")
plt.ylabel("GPR Brasil")

plt.tight_layout()
plt.show()