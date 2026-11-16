# Auditoria do forecasting GARCH-MIDAS

Data: 10/11/2026. Pacote instalado: mfGARCH 0.2.2.

## Conclusão

A implementação e os arquivos salvos passaram em 41 verificações computacionais, sob a hipótese de disponibilidade do GPR no fechamento do dia 1 do mês seguinte. Isso não valida essa hipótese como calendário histórico real. Os resultados são de um exercício pseudo-out-of-sample com a versão da série contida na base, sem controle de vintages históricos.

A validação está em `verificacoes.csv`; os 250 ajustes inspecionados estão em `ajustes.csv`; as perdas médias recalculadas estão em `metricas_recalculadas.csv`.

## Verificações realizadas

- A base atual coincide com a registrada no checkpoint.
- As 2.564 datas previstas são exatamente as datas de `amostra_teste`: 01/09/2015 a 30/12/2025.
- A origem de cada previsão corresponde ao pregão observado anterior ao primeiro alvo.
- Há 125 arquivos de ajustes mensais, cada um com os modelos GPR e VR: 250 ajustes ao todo.
- Todos os ajustes têm código de convergência zero, parâmetros admissíveis e estados finitos e positivos depois do aquecimento. A persistência está entre 0,9453023 e 0,9807502.
- Os dados efetivamente usados em cada ajuste terminam na origem e coincidem com `dados_na_origem`.
- Os componentes tau foram recalculados independentemente com os K=24 blocos anteriores e seus pesos.
- Os componentes g foram refeitos com a rotina C++ `calculate_g` do pacote, incluindo a assimetria integral nos choques negativos.
- Os resíduos salvos coincidem com `(retorno-mu)/sqrt(g*tau)`.
- Entre ajustes mensais, os estados são atualizados diariamente com o retorno já observado; os parâmetros ficam fixos.
- Alterar os retornos futuros e os valores mensais ainda indisponíveis não altera as entradas nem o tau previsto nas 125 origens de reestimação, segundo o calendário implementado.
- A VR mensal coincide com a soma dos retornos diários percentuais ao quadrado. A limpeza da base não elimina pregões adicionais com as variáveis do exercício disponíveis.
- Checkpoint, estados e CSVs são consistentes; não há alvos duplicados ou variâncias previstas inválidas.
- As métricas salvas para h=1, 10 e 20 foram reproduzidas. O arquivo de horizontes atualmente salvo contém h=30 e também foi validado.

## Métricas reproduzidas

| Horizonte em pregões | Modelo | Origens avaliáveis | MSE | QLIKE |
|---:|:---|---:|---:|---:|
| 1 | GPR | 2564 | 69,148728 | 1,4944321 |
| 1 | VR | 2564 | 76,158220 | 1,4910908 |
| 10 | GPR | 2555 | 93,729950 | 1,6136101 |
| 10 | VR | 2555 | 95,318301 | 1,6125461 |
| 20 | GPR | 2545 | 96,049295 | 1,7581000 |
| 20 | VR | 2545 | 95,259727 | 1,7623951 |
| 30 | GPR | 2535 | 94,492353 | 1,7845029 |
| 30 | VR | 2535 | 93,491026 | 1,7814781 |

As perdas usam r² como observado, MSE=(r²-v)² e QLIKE=log(v)+r²/v. A última expressão omite termos que dependem somente do observado e, por isso, preserva a comparação entre os modelos.

## Ressalvas que precisam constar da metodologia

### Calendário de divulgação do GPR

O código impõe disponibilidade no fechamento do dia 1. A [página oficial](https://www.matteoiacoviello.com/gpr.htm) informa atualização no começo do mês e adiamento para o próximo dia útil quando o primeiro dia é feriado federal. Portanto, a regra fixa não comprova disponibilidade histórica.

Exemplo identificado nos resultados: a previsão com origem em 01/09/2025 para 02/09/2025 usa o GPR de agosto, registrado como disponível em 01/09. Esse dia foi Labor Day, conforme o [calendário federal dos EUA](https://www.opm.gov/policy-data-oversight/pay-leave/federal-holidays/). Aplicando a regra atualmente publicada pela fonte, a divulgação seria adiada: essa origem precisa ser conferida no histórico efetivo de atualizações. Também há uma origem em 02/01/2023 que merece conferência por causa do feriado observado de Ano-Novo.

Isso indica risco de antecipação da informação em algumas viradas de mês. Corrigir o calendário exige rever o alinhamento histórico do regressor na estimação, os estados e as previsões GPR; editar apenas as datas dos CSVs não resolve. Os ajustes auditados permanecem preservados.

### Versões históricas da série

A fonte oficial informa revisões e oferece vintages. O pipeline usa uma única versão da série, sem demonstrar os valores disponíveis em cada origem. A [página da versão antiga](https://www.matteoiacoviello.com/gpr2019.htm) registra divulgação por volta do dia 10 e mudança metodológica em outubro de 2021. Não se pode projetar automaticamente a regra atual para toda a história.

O experimento deve ser descrito como pseudo-OOS com série revisada e calendário assumido. Um backtest em tempo real exige também as versões históricas e as datas efetivas de divulgação.

### Horizontes maiores

Para h>1, a função mantém os parâmetros estimados na origem e o tau do primeiro alvo constantes durante a projeção. Calcula E[g] por recursão com persistência alpha+beta+gamma/2. É uma previsão iterada sob essa hipótese, sem usar retornos ou regressores futuros observados.

O alvo avaliado é r² no h-ésimo pregão. Não é a variância acumulada dos h pregões. Para um exercício acumulado, devem ser comparadas `variancia_acumulada` e a soma dos h retornos ao quadrado.

### Média dos retornos e proxy de variância

Como o modelo estima mu diferente de zero, E[r²|informação]=variância+mu². Avaliar r² contra a variância usa a aproximação de média desprezível. Neste exercício, em h=1, mu² representa em média 0,0807% da variância prevista no GPR e 0,0936% na VR. Acrescentar mu² às previsões não muda o ranking por MSE nem por QLIKE nesse horizonte. A aproximação deve ser declarada; para avaliar o segundo momento exatamente, usa-se variância+mu².

### Comparações estatísticas

As médias das perdas não demonstram significância estatística. Os testes futuros devem considerar dependência serial das diferenças de perda. Os modelos devem ser comparados nas mesmas datas dentro de cada horizonte; o número de origens avaliáveis diminui em h-1 nas extremidades da amostra.

## Pequenos ajustes feitos no código

- A hipótese de divulgação no dia 1 foi identificada explicitamente nos comentários.
- A avaliação de h=1 agora valida suas datas contra `amostra_teste`, permitindo também execuções parciais.
- Os observados dos blocos h=10 e h=30 do script atual agora vêm explicitamente de `amostra_teste`. As datas e os valores permanecem iguais aos auditados.
- O script atual foi editado pelo usuário para h=30; o arquivo anterior de h=20 continua consistente com os estados salvos.

Nenhum modelo foi reestimado nesta auditoria. Para reproduzir as verificações na pasta do projeto:

```r
source("tmp/auditoria_forecast/auditar.R", encoding = "UTF-8")
```
