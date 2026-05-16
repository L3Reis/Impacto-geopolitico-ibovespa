# Pacotes
library(dplyr)
library(ggplot2)
library(mfGARCH)
library(readxl)

# Importando base de dados crua

# Base diária
base_diaria <- read_excel(
  "data/processed/base_dissertacao.xlsx",
  sheet = "dados_diarios"
)
# Pacote mfGARCH recomenda multiplicar os log-retornos por 100
