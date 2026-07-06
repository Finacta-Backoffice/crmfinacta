# =============================================================================
# main.R  -  Previsao de DIRECAO de retorno diario de acoes com GRU
# =============================================================================
# README (fluxo e dependencias)
# -----------------------------------------------------------------------------
# OBJETIVO
#   Primeira versao funcional e reproduzivel de um pipeline que preve a direcao
#   (sobe/nao sobe) do retorno do PROXIMO dia util para um subconjunto reduzido
#   de acoes, usando uma rede recorrente GRU. Divisao estritamente TEMPORAL
#   (sem embaralhamento) e sem uso do teste na selecao de hiperparametros.
#
# FLUXO (modulos em R/)
#   00_packages     -> pacotes e ambiente keras/tensorflow
#   01_load_tickers -> le tickers da planilha, limpa e aplica elegibilidade
#   02_download     -> baixa OHLCV+Adjusted (Yahoo) com tolerancia a falhas
#   03_features     -> indicadores tecnicos (janela = hiperparametro) + target
#   04_split        -> divisao temporal treino/validacao/teste
#   05_sequences    -> tensores 3D; scaler ajustado SO no treino (anti-vazamento)
#   06_model_gru    -> GRU (ou LSTM) de 1 camada + early stopping
#   07_tuning       -> busca de hiperparametros na validacao (teste intocado)
#   08_evaluation   -> metricas de classificacao no teste
#   09_backtest     -> estrategia long-only 1 dia vs. buy&hold
#
# DEPENDENCIAS
#   Ver R/00_packages.R. Antes de rodar, no ambiente local (UMA vez):
#     install.packages(c("tidyverse","data.table","quantmod","TTR","readxl",
#                        "keras","tensorflow","yardstick","purrr","glue","fs"))
#     library(keras); keras::install_keras()   # backend Python/TensorFlow (CPU)
#   Verifique com: tensorflow::tf_config()
#
# COMO EXECUTAR
#   No diretorio do projeto:  Rscript main.R
#   (ou abra no RStudio e execute o arquivo inteiro)
#
# SAIDAS GERADAS (ver secao final)
#   models/<TICKER>.rds, outputs/*.csv, logs/run_*.log
# =============================================================================


# ####################  BLOCO DE PARAMETROS (AJUSTE AQUI)  #####################
# -----------------------------------------------------------------------------
# Estes sao os pontos que voce provavelmente vai querer alterar. Estao reunidos
# no topo justamente para facilitar a expansao futura (ex.: 100 empresas).

# Caminho da planilha Excel (universo de tickers).
EXCEL_PATH <- "data_raw/Maiores_empresas.xlsx"

# Ano calendario completo a analisar (a divisao treino/val/teste e feita dentro
# deste ano). Use um ano JA ENCERRADO para ter o periodo de teste completo.
ANO_ANALISE <- 2025L

# Numero de empresas na PRIMEIRA rodada. AMPLIE aqui para escalar (ate ~100).
n_empresas_teste <- 5L

# Definicao do TARGET (facil de trocar em 03_features.R -> make_target()).
TARGET_TYPE <- "next_day_up"   # 1 se retorno de t+1 > 0; 0 caso contrario

# Limiar de decisao do backtest/classificacao (parametrizavel).
THRESHOLD <- 0.5

# Tipo de celula recorrente: "gru" (preferencial) ou "lstm" (alternativa).
RNN_TYPE <- "gru"

# Metrica usada para SELECIONAR hiperparametros na validacao ("accuracy"/"f1").
SELECTION_METRIC <- "accuracy"

# Semente para reprodutibilidade (R + backend TensorFlow).
SEED <- 42L

# Se TRUE, ao falhar o download de um ticker elegivel, tenta o proximo da fila
# ate completar n_empresas_teste (robustez da primeira entrega).
BACKFILL_FAILED <- TRUE

# GRADE DE HIPERPARAMETROS (mantida enxuta para tempo de execucao razoavel).
# Para escalar, amplie as listas e/ou aumente MAX_HP_COMBOS.
HP_GRID <- list(
  ind_window    = c(5L, 10L, 21L),   # janela dos indicadores tecnicos (5..21)
  lookback      = c(10L, 20L),       # tamanho da sequencia (lookback da rede)
  units         = c(16L, 32L),       # unidades da GRU
  dropout       = c(0.0, 0.2),       # dropout
  learning_rate = c(1e-3),           # taxa de aprendizado
  batch_size    = c(32L),            # tamanho do batch
  epochs        = c(30L)             # epocas (com early stopping)
)
MAX_HP_COMBOS <- 12L   # teto de combinacoes avaliadas por empresa

# Diretorios de saida (estrutura do projeto).
DIRS <- list(
  data_raw       = "data_raw",
  data_processed = "data_processed",
  models         = "models",
  outputs        = "outputs",
  logs           = "logs"
)
# ############################################################################


# --------------------------------------------------------------- inicializacao
# Garante execucao a partir da pasta do projeto (paths relativos acima).
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
}

# Carrega modulos.
source("R/00_packages.R")
load_packages()                    # falha cedo se faltar pacote obrigatorio
source("R/utils.R")
source("R/01_load_tickers.R")
source("R/02_download_data.R")
source("R/03_features.R")
source("R/04_split_data.R")
source("R/05_sequences.R")
source("R/06_model_gru.R")
source("R/07_tuning.R")
source("R/08_evaluation.R")
source("R/09_backtest.R")

set.seed(SEED)
ensure_dirs(unlist(DIRS))

log_file <- file.path(DIRS$logs,
                      format(Sys.time(), "run_%Y%m%d_%H%M%S.log"))
log <- new_logger(log_file)
log(glue::glue("=== Pipeline GRU | ano={ANO_ANALISE} | ",
               "n_empresas_teste={n_empresas_teste} | rnn={RNN_TYPE} ==="))

# Janela de download: comeca em out/(ano-1) para aquecer indicadores e lookback.
DOWNLOAD_FROM <- as.Date(sprintf("%d-10-01", ANO_ANALISE - 1L))
DOWNLOAD_TO   <- as.Date(sprintf("%d-12-31", ANO_ANALISE))


# ------------------------------------------------ 1) universo e selecao inicial
universe <- load_ticker_universe(EXCEL_PATH, log)
log(glue::glue("Empresas lidas da planilha: {nrow(universe$all)} | ",
               "elegiveis: {nrow(universe$eligible)}"))

# Log de empresas excluidas e motivo (persistido ao final tambem).
excluded <- universe$all[!universe$all$eligible,
                         c("rank", "ticker", "name", "reason")]

# Seleciona as PRIMEIRAS empresas elegiveis (criterio reproduzivel: ordem de
# rank/valor de mercado). Baixa mais candidatos que o necessario para permitir
# backfill de eventuais falhas de download.
candidate_pool <- universe$eligible$ticker
dl <- download_universe(
  candidate_tickers = candidate_pool,
  from = DOWNLOAD_FROM, to = DOWNLOAD_TO, n_target = n_empresas_teste,
  data_raw_dir = DIRS$data_raw, log = log, backfill = BACKFILL_FAILED)

used_tickers <- dl$used
log(glue::glue("Empresas efetivamente usadas ({length(used_tickers)}): ",
               "{paste(used_tickers, collapse = ', ')}"))
if (length(used_tickers) == 0) {
  stop("Nenhuma empresa com dados suficientes; verifique conexao/tickers.")
}

# Acrescenta as falhas de download ao log de exclusoes.
if (nrow(dl$failures) > 0) {
  excluded <- rbind(excluded, data.frame(
    rank = NA_integer_, ticker = dl$failures$ticker, name = NA_character_,
    reason = paste0("download_falhou: ", dl$failures$reason)))
}


# ------------------------------------------------ 2) grade de hiperparametros
hp_grid <- build_hp_grid(HP_GRID, max_combos = MAX_HP_COMBOS, seed = SEED)
log(glue::glue("Grade de hiperparametros: {nrow(hp_grid)} combinacoes por empresa."))


# ---------------------------------------------------- 3) pipeline por empresa
metrics_rows  <- list()   # metricas de teste por empresa
hp_rows       <- list()   # hiperparametros vencedores por empresa
pred_rows     <- list()   # previsoes de teste por empresa
per_company_failures <- list()

for (tk in used_tickers) {
  log(glue::glue("--- Empresa: {tk} ---"))
  res <- tryCatch({
    prices <- dl$data[[tk]]

    # 3a) Tuning na validacao (teste NAO e tocado).
    tuned <- tune_company(
      prices, hp_grid, year = ANO_ANALISE, target_type = TARGET_TYPE,
      seed = SEED, rnn_type = RNN_TYPE, threshold = THRESHOLD,
      selection_metric = SELECTION_METRIC, log = log)
    best_hp <- tuned$best_hp
    log(glue::glue("  melhor config: win={best_hp$ind_window} ",
                   "lb={best_hp$lookback} u={best_hp$units} ",
                   "drop={best_hp$dropout} | val_acc=",
                   "{round(best_hp$val_accuracy, 3)}"))

    # 3b) Modelo FINAL com a config vencedora (val so como early stopping).
    fit <- fit_final_model(prices, best_hp, year = ANO_ANALISE,
                           target_type = TARGET_TYPE, seed = SEED,
                           rnn_type = RNN_TYPE)

    # 3c) Avaliacao no TESTE FINAL (uma unica vez).
    ev <- evaluate_on_test(fit$model, fit$seqs, threshold = THRESHOLD)
    log(glue::glue("  TESTE: acc={round(ev$metrics$accuracy,3)} ",
                   "prec={round(ev$metrics$precision,3)} ",
                   "rec={round(ev$metrics$recall,3)} ",
                   "f1={round(ev$metrics$f1,3)} ",
                   "hit_dir={round(ev$metrics$directional_hit,3)}"))
    log(glue::glue("  matriz de confusao (linhas=real, colunas=previsto): ",
                   "[00={ev$confusion[1,1]}, 01={ev$confusion[1,2]}, ",
                   "10={ev$confusion[2,1]}, 11={ev$confusion[2,2]}]"))

    # 3d) Backtest da estrategia vs. buy&hold no teste.
    bt <- backtest_strategy(ev$predictions, threshold = THRESHOLD)
    log(glue::glue("  BACKTEST: estrategia={round(bt$metrics$strat_cum_return,4)} ",
                   "buy&hold={round(bt$metrics$bh_cum_return,4)} ",
                   "n_trades={bt$metrics$n_trades} ",
                   "win_rate={round(bt$metrics$win_rate,3)}"))

    # 3e) Salva o MODELO OTIMIZADO em .rds (keras serializado + metadados).
    model_bundle <- list(
      ticker       = tk,
      model        = keras::serialize_model(fit$model, include_optimizer = TRUE),
      best_hp      = best_hp,
      scaler       = fit$seqs$scaler,
      feat_cols    = fit$seqs$feat_cols,
      target_type  = TARGET_TYPE,
      rnn_type     = RNN_TYPE,
      year         = ANO_ANALISE
    )
    saveRDS(model_bundle, file.path(DIRS$models, glue::glue("{tk}.rds")))

    # Coleta para as tabelas consolidadas.
    metrics_rows[[tk]] <- data.frame(
      ticker = tk,
      accuracy = ev$metrics$accuracy, precision = ev$metrics$precision,
      recall = ev$metrics$recall, f1 = ev$metrics$f1,
      directional_hit = ev$metrics$directional_hit,
      n_test = ev$metrics$n_test,
      strat_cum_return = bt$metrics$strat_cum_return,
      bh_cum_return = bt$metrics$bh_cum_return,
      n_trades = bt$metrics$n_trades, win_rate = bt$metrics$win_rate,
      avg_return_per_trade = bt$metrics$avg_return_per_trade,
      avg_return_per_day = bt$metrics$avg_return_per_day,
      stringsAsFactors = FALSE)

    hp_rows[[tk]] <- data.frame(ticker = tk, best_hp, stringsAsFactors = FALSE)

    preds <- ev$predictions
    preds$ticker <- tk
    preds$signal <- bt$daily$signal
    preds$strat_ret <- bt$daily$strat_ret
    pred_rows[[tk]] <- preds

    keras::k_clear_session()
    "ok"
  }, error = function(e) {
    log(glue::glue("Empresa {tk} FALHOU: {conditionMessage(e)}"), level = "ERROR")
    per_company_failures[[tk]] <<- data.frame(
      rank = NA_integer_, ticker = tk, name = NA_character_,
      reason = paste0("pipeline_falhou: ", conditionMessage(e)),
      stringsAsFactors = FALSE)
    "erro"
  })
}


# ---------------------------------------------------------- 4) salvar saidas
if (length(per_company_failures) > 0) {
  excluded <- rbind(excluded, do.call(rbind, per_company_failures))
}

metrics_tbl <- if (length(metrics_rows)) do.call(rbind, metrics_rows) else
  data.frame()
hp_tbl      <- if (length(hp_rows)) do.call(rbind, hp_rows) else data.frame()
pred_tbl    <- if (length(pred_rows)) do.call(rbind, pred_rows) else data.frame()

write.csv(metrics_tbl, file.path(DIRS$outputs, "metrics_by_company.csv"),
          row.names = FALSE)
write.csv(hp_tbl, file.path(DIRS$outputs, "winning_hyperparams.csv"),
          row.names = FALSE)
write.csv(pred_tbl, file.path(DIRS$outputs, "test_predictions.csv"),
          row.names = FALSE)
write.csv(excluded, file.path(DIRS$outputs, "excluded_companies.csv"),
          row.names = FALSE)

# Resumo consolidado (medias das metricas entre empresas bem-sucedidas).
if (nrow(metrics_tbl) > 0) {
  summary_tbl <- data.frame(
    n_companies = nrow(metrics_tbl),
    mean_accuracy = mean(metrics_tbl$accuracy, na.rm = TRUE),
    mean_f1 = mean(metrics_tbl$f1, na.rm = TRUE),
    mean_directional_hit = mean(metrics_tbl$directional_hit, na.rm = TRUE),
    mean_strat_cum_return = mean(metrics_tbl$strat_cum_return, na.rm = TRUE),
    mean_bh_cum_return = mean(metrics_tbl$bh_cum_return, na.rm = TRUE),
    stringsAsFactors = FALSE)
  write.csv(summary_tbl, file.path(DIRS$outputs, "summary_overall.csv"),
            row.names = FALSE)
  log(glue::glue("RESUMO: acc_medio={round(summary_tbl$mean_accuracy,3)} ",
                 "f1_medio={round(summary_tbl$mean_f1,3)} ",
                 "estrategia_media={round(summary_tbl$mean_strat_cum_return,4)} ",
                 "b&h_medio={round(summary_tbl$mean_bh_cum_return,4)}"))
}

log(glue::glue("=== Concluido. Empresas com modelo: {nrow(metrics_tbl)}. ",
               "Saidas em '{DIRS$outputs}/', modelos em '{DIRS$models}/'. ==="))
