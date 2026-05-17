# Formando a base de dados para o teste do Garch-Midas

# para rodar o GARCH-MIDAS no R, o ideal é ter uma base diária em uma única sheet, contendo:

# date | ret_ibov | month | year_month | gpr_bra

import pandas as pd
import numpy as np

# base diaria
base_diaria = pd.read_excel(
    io = 'D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_dissertacao.xlsx',
    sheet_name = 'dados_diarios'
).dropna()

# base mensal
base_mensal = pd.read_excel(
    io = 'D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_dissertacao.xlsx',
    sheet_name = 'dados_mensais'
).dropna() 

# adicionando GPR global aqui

arquivo = "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/data_gpr_export.xls"

gpr_raw = pd.read_excel(
    arquivo,
    sheet_name="Sheet1",
    engine="xlrd"
)

print(gpr_raw.head())
print(gpr_raw.columns)

# Extrair GPR Global

gpr_global = gpr_raw[["month", "GPR"]].copy()

gpr_global = gpr_global.rename(columns={
    "GPR": "gpr_global"
})

# Garantir que month está como data
gpr_global["month"] = pd.to_datetime(gpr_global["month"], errors="coerce")

# Remover linhas sem GPR
gpr_global = gpr_global.dropna(subset=["month", "gpr_global"])

# Filtrar período da dissertação
gpr_global = gpr_global[
    (gpr_global["month"] >= "1999-01-01") &
    (gpr_global["month"] <= "2025-12-31")
].copy()

# Padronizar mês para início do mês
gpr_global["month"] = gpr_global["month"].dt.to_period("M").dt.to_timestamp()

# Colocar month como índice
gpr_global = gpr_global.set_index("month")

# Ordenar
gpr_global = gpr_global.sort_index()

print(gpr_global.head())
print(gpr_global.tail())
print(gpr_global.shape)

# Padronizar GPR Global

gpr_global["gpr_global_z"] = (
    gpr_global["gpr_global"] - gpr_global["gpr_global"].mean()
) / gpr_global["gpr_global"].std()

print(gpr_global.head())
print(gpr_global[["gpr_global", "gpr_global_z"]].describe())

# Juntar GPR Global na base mensal

# Corrigir índice da base mensal usando a coluna month

base_mensal = base_mensal.copy()

base_mensal["month"] = pd.to_datetime(base_mensal["month"])
base_mensal["month"] = base_mensal["month"].dt.to_period("M").dt.to_timestamp()

base_mensal = base_mensal.set_index("month")
base_mensal = base_mensal.sort_index()

# Remover colunas antigas caso existam
base_mensal = base_mensal.drop(
    columns=["gpr_global", "gpr_global_z"],
    errors="ignore"
)

# Juntar GPR Global
base_mensal = base_mensal.join(gpr_global, how="left")

# Conferir
print(base_mensal.head())
print(base_mensal.tail())
print(base_mensal.columns)
print(base_mensal[["gpr_global", "gpr_global_z"]].isna().sum())
# Juntar base diária com variáveis mensais

# Garantir que a data diária está como data
base_diaria["date"] = pd.to_datetime(base_diaria["date"])

# Criar mês na base diária
base_diaria["month"] = base_diaria["date"].dt.to_period("M").dt.to_timestamp()

# Garantir que a coluna month da base mensal está como data
base_mensal["month"] = pd.to_datetime(base_mensal["month"])
base_mensal["month"] = base_mensal["month"].dt.to_period("M").dt.to_timestamp()

# Juntar: cada dia recebe as variáveis mensais do respectivo mês
base_final = base_diaria.merge(
    base_mensal,
    on="month",
    how="left"
)

# Ver resultado
print(base_final.head())
print(base_final.tail())
print(base_final.columns)
print(base_final.shape)


# Renomear colunas para nomes mais simples

base_final = base_final.rename(columns={
    "^BVSP": "ret_ibov",
    "cambio": "ret_cambio"
})


# Criar coluna de mês em formato simples para o R
base_final["year_month"] = base_final["month"].dt.strftime("%Y-%m")

# Conferir
print(base_final.head())
print(base_final.columns)

# Teste só GPR
base_mfgarch_gpr = base_final[[
    "date",
    "ret_ibov",
    "year_month",
    "gpr_bra"
]].dropna()

print(base_mfgarch_gpr.head())
print(base_mfgarch_gpr.tail())
print(base_mfgarch_gpr.shape)



# Salvando para teste no R
base_mfgarch_gpr.to_excel(
    "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados//base_mfgarch_gpr.xlsx",
    sheet_name="dados",
    index=False
)

# Base completa (por enquanto)
# Salvando para teste no R
base_final.to_excel(
    "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados//base_mfgarch_completa.xlsx",
    index=False
)
