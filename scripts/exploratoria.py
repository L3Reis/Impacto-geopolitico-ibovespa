## Pacotes
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib.pyplot as plt



## Carregando dados


## Ibovespa
ibov = yf.download("^BVSP", start = '1999-01-01', end = '2025-12-31')
preco = ibov["Close"] # equivalente ao select do R

# Log-retorno
retorno = np.log(preco / preco.shift(1))
retorno = retorno.dropna()

print(retorno.head()) 
print(retorno.shape)
retorno.plot()

## Cambio
cambio = yf.download("BRL=X", '1999-01-01', end = '2025-12-31')


# Retorno

retorno_cambio = np.log(cambio['Close'] / cambio['Close'].shift(1)).dropna()

# Variancia Realizada
var_cambio = (retorno_cambio**2).resample("ME").sum()
