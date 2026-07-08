# Pacotes
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt
from bcb import sgs
import plotnine as p9

# Carregando setores do NEFIN

arquivo = "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/industry_portfolios.csv"
setores = pd.read_csv(
    arquivo
)

#Selecionando setores
setores_alvo = ['energy', 'basic_products', 'finance', 'consumer']
retornos_setores = retornos_setores[retornos_setores['date'] <= '2025-12-31']
retornos_setores.dropna(inplace=True)

# Retorno Logarítmico Contínuo e Escala: ln(1 + R) * 100
# np.log1p é numericamente mais estável para retornos próximos de zero.
novas_colunas = [f"{setor}_log_ret" for setor in setores_alvo]
retornos_setores[novas_colunas] = np.log1p(retornos_setores[setores_alvo]) * 100


# Criação do indexador "Ano-Mês" necessário para a função fit_mfgarch no R
# ! isso é necessário para o funcionamento da ponderação beta no MIDAS
retornos_setores['date'] = pd.to_datetime(retornos_setores['date'])
retornos_setores['year_month'] = retornos_setores['date'].dt.to_period('M').astype(str)


## Juntando com as variaveis de longo prazo

base_mensal = pd.read_excel(
    "D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/Impacto-geopolitico-ibovespa/dados/base_descritiva_mensal.xlsx",
    parse_dates=['month']
)

# criando chave de conexão com outra tabela
base_mensal['year_month'] = base_mensal['month'].dt.to_period('M').astype(str)
base_mensal = base_mensal[base_mensal['year_month'] >= '2000-02']

# fazendo o merge
base_setores_final = pd.merge(
    retornos_setores,
    base_mensal,
    on = 'year_month',
    how = 'inner'
)

# Salvando dados
with pd.ExcelWriter("D:/OneDrive/UFABC/Dissertação/volatilidade-IBOV-GPR/impacto-geopolitico-ibovespa/dados/base_setores_final.xlsx", engine="openpyxl") as writer:
    
    base_setores_final.to_excel(
        writer,
        sheet_name="base_setores",
        index=False,
    )
