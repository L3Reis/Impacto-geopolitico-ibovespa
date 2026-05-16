# Carregando os dados, fazendo transformações iniciais e juntando em um Excel inicial


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
plt.close()
retorno['^BVSP'].plot(figsize=(10, 5))

plt.title("Log-retorno Ibovespa")
plt.xlabel("Data")
plt.ylabel("Log-retorno")

plt.tight_layout()
plt.show()



## Cambio
# sgs aceita no maximo janela de 10 anos
cambio_99 = sgs.get({"cambio": 1}, start="1999-01-01", end="2009-01-01")
cambio_09 = sgs.get({"cambio": 1}, start="2009-01-01", end="2019-01-01")
cambio_19 = sgs.get({"cambio": 1}, start="2019-01-01", end="2025-12-31")

# concatenar
cambio_sgs = pd.concat([cambio_99, cambio_09, cambio_19])

cambio_sgs = cambio_sgs.sort_index()

print(cambio_sgs.head())
print(cambio_sgs.tail())

# Cambio 1999 a 2025
plt.close()
cambio_sgs['cambio'].plot(figsize=(10, 5))
plt.title("Taxa de cambio")
plt.xlabel("Data")
plt.ylabel("Taxa")

plt.tight_layout()
plt.show()

# Retorno

retorno_cambio = np.log(cambio_sgs["cambio"] / cambio_sgs["cambio"].shift(1))

retorno_cambio = retorno_cambio.dropna()

# Variancia Realizada

var_cambio = (retorno_cambio ** 2).resample("ME").sum()

var_cambio = var_cambio.to_frame(name="var_cambio")

print(var_cambio.head())
print(var_cambio.tail())


# visualização cambio
plt.close()
var_cambio["var_cambio"].plot(figsize=(10, 5))

plt.title("Variância realizada mensal do câmbio")
plt.xlabel("Data")
plt.ylabel("Variância realizada")

plt.tight_layout()
plt.show()
plt.close()

## Indice de Commodities
comm = sgs.get({"ic_br": 27574}, start = '1999-01-01', end = '2025-12-31')

# visualização commodities
comm["ic_br"].plot(figsize=(10, 5))

plt.title("IC-BR")
plt.xlabel("Data")
plt.ylabel("Índice")

plt.tight_layout()
plt.show()

plt.close()

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

## Gráfico GPR Brasil

plt.figure(figsize=(10, 5))

plt.plot(gpr_bra["month"], gpr_bra["gpr_bra"])

plt.title("Índice de Risco Geopolítico - Brasil")
plt.xlabel("Data")
plt.ylabel("GPR Brasil")

plt.tight_layout()
plt.show()

# Tabela agrupada inicial 
# Retorno do Ibovespa e retorno do cambio 
dados_diarios = retorno.join(other = retorno_cambio, how = 'outer')


# Variancia realizada do Cambio, IC-BR em nível e GPR-BRA em nível

#Padronizar índices mensais

import pandas as pd

# Câmbio
var_cambio = var_cambio.copy()

if isinstance(var_cambio, pd.Series):
    var_cambio = var_cambio.to_frame(name="var_cambio")

var_cambio.index = pd.to_datetime(var_cambio.index)
var_cambio.index = var_cambio.index.to_period("M").to_timestamp()
var_cambio = var_cambio.sort_index()


# GPR Brasil
gpr_bra = gpr_bra.copy()

if "month" in gpr_bra.columns:
    gpr_bra["month"] = pd.to_datetime(gpr_bra["month"])
    gpr_bra = gpr_bra.set_index("month")

gpr_bra.index = pd.to_datetime(gpr_bra.index)
gpr_bra.index = gpr_bra.index.to_period("M").to_timestamp()
gpr_bra = gpr_bra.sort_index()


# IC-Br / Commodities
comm = comm.copy()

if "month" in comm.columns:
    comm["month"] = pd.to_datetime(comm["month"])
    comm = comm.set_index("month")

comm.index = pd.to_datetime(comm.index)
comm.index = comm.index.to_period("M").to_timestamp()
comm = comm.sort_index()

# Juntar variáveis mensais

dados_mensais = pd.concat(
    [var_cambio, gpr_bra, comm],
    axis=1,
    join="outer"
).sort_index()

print(dados_mensais.head(15))
print(dados_mensais.tail())

# salvando dados

with pd.ExcelWriter("D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados/base_dissertacao.xlsx", engine="openpyxl") as writer:
    
    dados_diarios.to_excel(
        writer,
        sheet_name="dados_diarios",
        index=True,
        index_label="date"
    )
    
    dados_mensais.to_excel(
        writer,
        sheet_name="dados_mensais",
        index=True,
        index_label="month"
    )