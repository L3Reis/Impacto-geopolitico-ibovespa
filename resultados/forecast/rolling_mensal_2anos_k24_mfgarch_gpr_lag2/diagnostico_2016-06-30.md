# Ajuste VR de 30/06/2016

O resultado padrão de `fit_mfgarch()` registrou convergência 1, persistência alpha+beta+gamma/2 aproximadamente 0,9999999 e estados finais positivos. O período efetivo de estimação estava correto. No otimizador do R, [código 1 indica limite de iterações atingido](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html).

O mesmo modelo foi reestimado, na mesma janela e com a mesma especificação, usando `control = list(par.start = ...)` com os parâmetros do ajuste válido de 31/05/2016. A nova tentativa retornou código 0, persistência 0,9839264 e passou nas verificações de parâmetros, estados e período.

A rotina passou a fazer essa tentativa apenas quando o ajuste padrão falha. Ela não usa o modelo anterior como substituto: realiza uma nova estimação com os dados da janela atual. Também não aceita automaticamente códigos de convergência diferentes de zero nem elimina a origem da avaliação.

O ajuste aceito está em `ajustes/2016-06-30.rds`, com `recuperados = "vr"`. Os objetos de diagnóstico das duas tentativas estão em `tmp/auditoria_forecast/vr_2016-06-30_padrao.rds` e `tmp/auditoria_forecast/vr_2016-06-30_anterior.rds`.

A execução de validação completou 210 origens e conferiu a preservação das previsões anteriormente salvas. Os demais meses ainda precisam ser percorridos pela execução completa.
