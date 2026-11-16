# Rolling GARCH-MIDAS com funções originais do mfGARCH

Nota de 11/11/2026: esta pasta preserva os resultados da versão com reestimação diária. O script atual usa reestimação mensal e grava em `resultados/forecast/rolling_mensal_2anos_k24_mfgarch_gpr_lag2`. As descrições abaixo se referem à versão diária anterior.

Script: `scripts_R/Forecast/forecast_rolling_mfgarch.R`.

- Corte inicial: 31/08/2015. Origens avançam um pregão por vez durante o teste.
- Janela de dois anos de calendário efetivos, mais K=24 meses anteriores de histórico. Os primeiros 24 meses não contribuem para a verossimilhança; o pacote também usa o histórico para inicialização.
- Ambos os modelos são reestimados a cada origem, com `fit_mfgarch()` e valores iniciais padrão. As previsões usam o `predict()` original, sem alteração local da assimetria da versão 0.2.2 discutida anteriormente.
- GPR conservador: julho entra em tau de setembro. A defasagem é implementada na variável mensal mais o lag 1 interno do pacote.
- A VR do mês corrente é preenchida somente com retornos já observados. O tau do primeiro pregão previsto é obtido pelo pacote, respeitando a virada de mês, e fica constante durante a projeção.
- Alvos: retorno ao quadrado no 1º, 5º e 22º pregões seguintes, e não a soma de retornos ao quadrado do intervalo. O uso de r² como proxy de variância mantém a aproximação de média desprezível.
- Usa uma única versão da série, sem reconstrução de vintages: exercício pseudo-out-of-sample.

## Execução e retomada

`n_origens <- Inf` calcula todas as origens. `n_origens <- 3L` permite uma execução curta. Ajustes válidos são salvos por origem em `ajustes/`; ao reexecutar com a mesma base e configuração, esses ajustes são reutilizados. A rotina interrompe se um ajuste não convergir, em vez de retirar observações silenciosamente.

A execução completa exige 2.564 origens, com dois modelos por origem: 5.128 ajustes. Os números avaliáveis por modelo serão 2.564 em h=1, 2.560 em h=5 e 2.543 em h=22.

Os CSVs exportados ao final refletem apenas as origens da execução realizada. Na validação inicial foram efetivamente estimadas três origens (seis ajustes); portanto, os primeiros CSVs têm n=3 por horizonte. As demais 2.561 origens ainda precisam ser estimadas na execução completa. O recorte temporal, a cobertura dos horizontes e a disponibilidade da VR foram verificados nas 2.564 janelas sem repetir todas as estimações.
