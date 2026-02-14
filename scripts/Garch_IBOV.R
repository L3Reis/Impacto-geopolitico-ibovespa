
# Garch simples com Ibovespa ----------------------------------------------

# Pacotes necessários

library(quantmod)
library(forecast)
library(FinTS)
library(rugarch)



# Dados e tratamento ------------------------------------------------------

# Carregando dados do Ibovespa

getSymbols("^BVSP", from="2000-01-01", to="2025-12-31") 

# Selecionando Fechamento ajustado

bvsp <- Ad(BVSP)

# Obtendo log-retorno

r.bvsp <- dailyReturn(bvsp, type="log") * 100 # Multiplica por 100 para facilitar a modelagem
r.ibov <- na.omit(r.bvsp)

# Primeira visualização
plot(r.ibov, main="Log-Retornos Ibovespa (%)", yaxis.right= FALSE)



# Testes para Autocorrelacao Serial e para Heterocedasticidade ------------


# Teste de Box-Pierce ou Ljung-Box - H0: Nao Existe autocorrelacao na serie 

Box.test(r.ibov, 12, type="Ljung-Box")
Box.test(r.ibov^2, 12, type="Ljung-Box")

# O primeiro teste apontou autocorrelação serial, o que justifica a adesão de um AR(1) na média e o segundo mostrou uma autocorrelação serial muito forte na variancia.
# Esses resultados justificam o GARCH.

# Teste de Heterocedasticidade (LM)- H0: Nao existe Heterocedasticidade

ArchTest(r.ibov, lags=6, demean = TRUE)	

# Pelo resultado, fica claro que há um efeito ARCH na série


# Modelagem ---------------------------------------------------------------

# AR(1)-GARCH(1,1)

garch_spec_1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

garch_fit_1 <- ugarchfit(garch_spec_1, data= r.ibov)
garch_fit_1

# AR(1) foi estimado por conta da autocorrelação serial obtida no teste Ljung-Box
# Distribuição Student-t escolhida devido as caudas pesadas características de séries financeiras

# Resultados: o modelo apresentou AR(1) como não significativo, então vamos reestimar sem esse fator

# GARCH(1,1)
# Nova especificação SEM o AR(1) - Apenas Constante (Média)
garch_spec_2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), # Mudamos de (1,0) para (0,0)
  distribution.model = "std"
)

garch_fit_2 <- ugarchfit(spec = garch_spec_2, data = r.ibov)
garch_fit_2

# Aparentemente esse segundo modelo é melhor 


# Extração de informações do modelo ---------------------------------------

# Coeficientes
coef(garch_fit_2)

# Critério de informação
infocriteria(garch_fit_2)

# Volatilidade esperada
vol_esperada <- sigma(garch_fit_2)
plot(vol_esperada)

# residuos
tsdisplay(garch_fit_2@fit$z)	  	# Residuos padronizados do Modelo da Media Condicional 
tsdisplay(garch_fit_2@fit$z^2)		# Residuos padronizados do Modelo da Variancia Condicional 

# Panorama geral do modelo
plot(garch_fit_2, which="all")

# VaR
plot(garch_fit_2, which= 2 )



# Previsão ----------------------------------------------------------------

previsao <- ugarchforecast(garch_fit_2, n.ahead = 5)

print(previsao)

# Previsao da Volatilidade
plot(previsao, which = 1)

# Trajetória da Volatilidade
plot(previsao, which = 3)
