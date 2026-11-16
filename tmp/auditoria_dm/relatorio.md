# Auditoria dos testes DM - variancia media

Data: 16/11/2026. Dados: perdas atuais exportadas pelo usuario. Sem reestimacao dos modelos e sem alterar os scripts de forecasting ou teste_dm.R.

## Conclusao

Os 30 testes reportados foram reproduzidos por um calculo independente. Nao foi identificado erro de implementacao no diferencial, na selecao da largura Bartlett ou na escala da variancia HAC da media. A proximidade de varias estatisticas MSE a -1 e compativel com a concentracao temporal e o formato semelhante dos diferenciais entre os ativos.

## Metodo verificado

- d_t = perda_GPR - perda_VR, comparado nas mesmas origens, ordenadas cronologicamente.
- Autocovariancias: gamma_k = sum((d_t-media_d)*(d_(t-k)-media_d))/n.
- Variancia de longo prazo: S = gamma_0 + 2*sum((1-k/(L+1))*gamma_k), para k=1,...,L.
- Variancia da media: S/n. Erro-padrao: sqrt(S/n). DM = media_d/sqrt(S/n).
- A funcao sandwich::NeweyWest aplicada a lm(d ~ 1) ja entrega S/n: nao se deve dividir novamente por n.
- Sem prebranqueamento, sem ajuste n/(n-k), p bilateral normal. Seletor de banda Newey-West reproduzido independentemente.
- L = max(h-1, floor(bwNeweyWest(...))), limitado a n-2. A sobreposicao das medias e contemplada pelo minimo h-1 e pela sensibilidade a outras larguras.
- Nenhuma regra de banda assegura sozinha validade assintotica ou estabilidade estrutural dos diferenciais.

Erro relativo maximo entre as variancias HAC independentes e as do pacote: 2.04e-15. Os valores DM e p coincidem com as tabelas registradas ate o arredondamento.

## Exemplo: MSE em um pregao

| Serie | Media do diferencial | Erro-padrao HAC | Lag HAC | DM |
|---|---:|---:|---:|---:|
| Ibovespa | -8,4035 | 8,3508 | 30 | -1,0063 |
| Bens basicos | -4,5584 | 4,0322 | 29 | -1,1305 |
| Consumo | -4,6351 | 4,5697 | 31 | -1,0143 |
| Energia | -26,7527 | 26,2269 | 26 | -1,0200 |
| Financeiro | -1,0055 | 1,0405 | 33 | -0,9664 |

## Concentracao na crise de 2020

Participacao das origens de marco e abril de 2020 na soma dos valores absolutos do diferencial MSE, h=1:

| Serie | Participacao | Maior origem em sum(d_t-media)^2 |
|---|---:|---:|
| Ibovespa | 95,5% | 43,9% |
| Bens basicos | 76,5% | 44,6% |
| Consumo | 92,6% | 45,5% |
| Energia | 94,6% | 54,0% |
| Financeiro | 73,6% | 48,9% |

As maiores contribuicoes se concentram no fim de marco e inicio de abril de 2020. No Ibovespa, em 01/04/2020, o observado e 8,133, a previsao GPR e 38,322 e a VR e 94,921. A diferenca de MSE nessa observacao e aproximadamente -6.620,794.
Os diferenciais MSE em h=1 sao altamente correlacionados entre ativos: Ibovespa/consumo aproximadamente 0,995; Ibovespa/energia 0,970; Ibovespa/bens basicos 0,970.
Esta concentracao nao prova erro no DM, mas restringe a interpretacao de uma vantagem media como ganho regular em toda a amostra. Examinar a resposta do benchmark VR na virada de marco/abril e uma prioridade de diagnostico; a auditoria nao certifica a adequacao econometrica de todas as previsoes.

## Sensibilidade a largura HAC

Foram comparadas as larguras 5, 10, 22, 40, 60 e 100, alem do minimo h-1 e da banda original, usando para a sintese somente L >= max(h-1,5).
Nos 15 testes MSE, nenhum p-valor cai abaixo de 10% nessas escolhas. O menor encontrado e 0,156165.
Para QLIKE de bens basicos, as faixas de p-valores individuais sao:

| Horizonte | Menor p | Maior p |
|---|---:|---:|
| 1 | 0,010449 | 0,015341 |
| 5 | 0,001240 | 0,012627 |
| 22 | 0,001091 | 0,015290 |

A evidencia QLIKE favoravel aos bens basicos permanece individualmente significativa a 5% nas bandas examinadas. Os resultados a 1% dependem da banda. Isto nao substitui correcao de comparacoes multiplas.
Bandas zero constam apenas como referencia de independencia. Elas nao devem substituir o HAC diante da autocorrelacao observada (inclusive em h=1).
Nao foram removidas observacoes nem selecionada uma banda para obter significancia.

## Reproducao e arquivos

Execute, na raiz do projeto: `source("tmp/auditoria_dm/auditar.R", encoding="UTF-8")`.
- resumo.csv: media, bandas, autocovariancia zero, variancia de longo prazo, variancia da media e erro-padrao efetivos para cada teste.
- autocovariancias.csv: autocovariancias, autocorrelacoes, pesos Bartlett e contribuicoes a variancia.
- sensibilidade_lag.csv: estatisticas sob bandas alternativas, incluindo indicador L >= h-1.
- origens_influentes.csv: dez maiores diferenciais absolutos de cada teste.
- diferenciais.rds: series cronologicas utilizadas.
- Entrada: tmp/perdas_media_auditoria_dm.rds.

SHA256 da entrada: `259a1958ac6bc5e7df6de1573511bb01a661c6fcac19b4288d2c33248633c50e`.
SHA256 do teste_dm.R auditado: `2ae2e0e9b2d7274f77be21e10a48eeaa5e2721c625342daeb2e1de35c2647270`.

Referencia: [documentacao oficial do sandwich](https://cran.r-universe.dev/sandwich/doc/manual.html), secao Newey-West HAC Covariance Matrix Estimation.
