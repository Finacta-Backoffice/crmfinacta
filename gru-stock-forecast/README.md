# GRU — Previsão de direção de retorno diário de ações (v1)

Primeira versão funcional e reproduzível de um pipeline em **R** que utiliza uma
rede neural recorrente **GRU** para prever a **direção** (sobe / não sobe) do
retorno do próximo dia útil de ações. Projeto **isolado**, sem qualquer relação
com o restante do repositório.

> **Escopo desta v1:** 1 ano calendário, subconjunto reduzido de empresas
> (`n_empresas_teste = 5`), 1 definição de target e uma grade enxuta de
> hiperparâmetros. Escrito para escalar depois até ~100 empresas.

---

## Premissas principais

- **Divisão estritamente temporal** (sem amostragem aleatória), dentro do ano:
  - Treino: `01/01 → 31/08`
  - Validação: `01/09 → 15/10`
  - Teste final: `16/10 → 31/12`
- **Sem vazamento de informação**:
  - o *scaler* (média/desvio) é ajustado **somente no treino**;
  - o *target* usa o retorno de `t+1` (o que queremos prever) e nunca é feature;
  - o **conjunto de teste não participa da seleção de hiperparâmetros**.
- **Fonte dos dados**: Yahoo Finance via `quantmod` (coerente com a planilha,
  exportada do screener do Yahoo). OHLCV + Adjusted.
- **Target padrão**: `1` se o retorno de `t+1` for positivo, `0` caso contrário
  (fácil de trocar em `R/03_features.R → make_target()`).

## Estrutura

```
main.R                # orquestrador + BLOCO DE PARÂMETROS no topo
R/
  00_packages.R       # pacotes + instruções keras/tensorflow
  01_load_tickers.R   # lê tickers da planilha, limpa, aplica elegibilidade
  02_download_data.R  # baixa OHLCV+Adjusted (tolerante a falhas + backfill)
  03_features.R       # indicadores técnicos (janela = hiperparâmetro) + target
  04_split_data.R     # divisão temporal treino/val/teste
  05_sequences.R      # tensores 3D; scaler ajustado só no treino
  06_model_gru.R      # GRU (ou LSTM) de 1 camada + early stopping
  07_tuning.R         # busca de hiperparâmetros na validação
  08_evaluation.R     # métricas de classificação no teste
  09_backtest.R       # estratégia long-only 1 dia vs. buy&hold
  utils.R             # logging, scaler, métricas
data_raw/  data_processed/  models/  outputs/  logs/
```

## Instalação (uma vez, no ambiente local)

```r
install.packages(c("tidyverse","data.table","quantmod","TTR","readxl",
                   "keras","tensorflow","yardstick","purrr","glue","fs"))
library(keras)
keras::install_keras()      # cria ambiente Python + TensorFlow (CPU basta)
tensorflow::tf_config()     # verificação
```

## Execução

```bash
cd gru-stock-forecast
Rscript main.R
```

Os parâmetros ajustáveis estão **reunidos no topo de `main.R`**: caminho da
planilha, ano analisado, `n_empresas_teste`, tipo de target, threshold, grade de
hiperparâmetros, tipo de célula (`gru`/`lstm`) e semente.

## Hiperparâmetros otimizados na validação

- Janela dos indicadores técnicos (5–21)
- Lookback da sequência (ex.: 10, 20)
- **Arquitetura da rede** (busca empírica de topologia): nº de camadas GRU
  empilhadas × unidades por camada, gerada por `arquitetura_redes()`
  (`R/10_architectures.R`). Ex.: `arquitetura_redes(2, c(16,32))` →
  `c(16), c(32), c(16,16), c(32,32)`. Ajuste `ARQ_MAX_CAMADAS` / `ARQ_NEURONIOS`
  no topo de `main.R`.
- Dropout, learning rate, batch size, épocas (com early stopping)

> Obs.: a função `arquitetura_redes` original foi escrita para o pacote
> `neuralnet` (MLP feedforward). Como o `neuralnet` não modela sequências
> temporais, reaproveitamos a **ideia** de varredura de topologias, mapeando
> cada vetor de unidades para **GRU empilhada** (Keras).

## Saídas geradas

| Arquivo | Conteúdo |
|---|---|
| `models/<TICKER>.rds` | modelo Keras serializado + hiperparâmetros + scaler |
| `outputs/metrics_by_company.csv` | accuracy, precision, recall, F1, retornos |
| `outputs/winning_hyperparams.csv` | melhor configuração por empresa |
| `outputs/test_predictions.csv` | previsões do teste (prob, sinal, retorno) |
| `outputs/excluded_companies.csv` | empresas descartadas e motivo |
| `outputs/summary_overall.csv` | resumo consolidado |
| `logs/run_*.log` | log completo da execução |

## Recarregar um modelo salvo

```r
library(keras)
bundle <- readRDS("models/NVDA.rds")
model  <- keras::unserialize_model(bundle$model)
bundle$best_hp   # hiperparâmetros vencedores
bundle$scaler    # parâmetros de padronização (do treino)
```

## Como escalar para 100 empresas

1. Aumente `n_empresas_teste` (até ~100) no topo de `main.R`.
2. O pipeline já é por-ativo e tolerante a falhas (`tryCatch` por empresa);
   nenhuma empresa derruba a execução.
3. Amplie `HP_GRID` e `MAX_HP_COMBOS` conforme o orçamento de tempo.

**Gargalos / melhorias futuras**: o custo dominante é o *tuning* (nº empresas ×
nº combinações × treino da GRU). Caminhos: paralelizar por empresa, cache de
downloads, busca aleatória/bayesiana em vez de grade, GPU, e um modelo
*pooled/multi-ativo* (transfer learning) em lugar de um modelo por ação.
