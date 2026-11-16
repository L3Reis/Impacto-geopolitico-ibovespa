# Rolling com reestimação mensal e previsões diárias

Script: `scripts_R/Forecast/forecast_rolling_mfgarch.R`.

- A janela de estimação avança antes da primeira previsão de cada mês. O primeiro ajuste é feito em 31/08/2015 para setembro; o segundo, em 30/09/2015 para outubro.
- Cada ajuste usa dois anos efetivos de retornos, mais K=24 meses anteriores de histórico para as defasagens e a inicialização do pacote. A janela e os coeficientes ficam fixos até o próximo ajuste mensal.
- As origens das previsões avançam diariamente. Os retornos observados depois do ajuste entram somente na atualização dos estados, não em uma nova otimização.
- `calculate_g`, obtida com `getFromNamespace`, é a rotina interna original do mfGARCH para filtrar g. `predict()` original recebe o retorno e os estados atualizados na origem. Nenhuma correção local do comportamento assimétrico de `predict()` na versão 0.2.2 foi feita.
- GPR conservador: julho entra em tau de setembro, pela defasagem adicional na variável mensal mais o lag 1 interno do pacote.
- O tau do mês de previsão é calculado pelo pacote com informações já conhecidas no ajuste. É mantido constante nos horizontes 1, 5 e 22 pregões de cada origem.
- Alvos: retorno ao quadrado no pregão específico; a avaliação usa a aproximação de média desprezível. Não se avalia a soma dos retornos ao quadrado do intervalo.
- Trata-se de pseudo-OOS com uma versão única da série, sem reconstrução de vintages históricos.

## Execução e resultados

`n_origens <- Inf` percorre as 2.564 origens. São 124 ajustes mensais por modelo, totalizando 248 estimações. Ao final, haverá 2.564 perdas por modelo em h=1, 2.560 em h=5 e 2.543 em h=22.

Os ajustes ficam em `ajustes/` e são reutilizados ao executar novamente com os mesmos dados e configuração. A rotina interrompe em caso de ajuste inválido. Os CSVs de previsões, perdas e precisão refletem apenas a execução concluída.

Se o ajuste padrão for inválido e houver um ajuste anterior válido, a rotina repete somente o modelo afetado com os parâmetros anteriores como valores iniciais de `fit_mfgarch()`. A janela e a especificação permanecem iguais. O resultado só é aceito se passar novamente por todos os critérios; caso contrário, a rotina interrompe com a causa da rejeição. O campo `recuperados` do RDS identifica os modelos recuperados dessa maneira.

A validação inicial percorreu 30 origens, com quatro ajustes reais (dois modelos em duas datas de reestimação). Portanto, os primeiros CSVs têm n=30 por horizonte; ainda não são os resultados do período completo.

Atualização: após a falha numérica da VR em 30/06/2016, a nova tentativa foi validada e o teste avançou até 210 origens. Os CSVs dessa validação têm n=210 por horizonte. Os resultados das origens anteriores foram preservados; isso ainda não equivale à execução das 2.564 origens.

Manter os parâmetros fixos por um período e atualizar os estados com os dados já observados é compatível com avaliação fora da amostra. A escolha mensal reduz o custo e a velocidade de adaptação dos parâmetros; não garante a mesma precisão da reestimação diária. Ver [Forecasting: Principles and Practice, seção 13.8](https://otexts.com/fpptr/training-test.html).
