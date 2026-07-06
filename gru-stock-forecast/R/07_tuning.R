# =============================================================================
# 07_tuning.R  -  Busca de hiperparametros na VALIDACAO (sem tocar no teste)
# =============================================================================
# Para cada empresa:
#   1) treina no periodo de TREINO;
#   2) mede desempenho na VALIDACAO para cada combinacao de hiperparametros;
#   3) escolhe a melhor combinacao pela metrica de validacao;
#   4) congela essa configuracao (o TESTE so e usado depois, na avaliacao final).
#
# O conjunto de TESTE NUNCA e consultado aqui -- esse e o ponto central para
# evitar selecao de parametros com vazamento.
#
# Hiperparametros otimizados: janela dos indicadores, lookback da rede, unidades
# da GRU, dropout, learning rate, batch size e epocas (com early stopping).
# =============================================================================

# Monta a grade de hiperparametros (produto cartesiano) e a limita a
# 'max_combos' para manter o tempo de execucao razoavel nesta primeira versao.
build_hp_grid <- function(grid, max_combos = 24L, seed = 42L) {
  g <- expand.grid(grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (nrow(g) > max_combos) {
    set.seed(seed)
    # Amostra reproduzivel de combinacoes (a busca em si nao mistura o tempo;
    # apenas escolhemos quais configuracoes avaliar).
    g <- g[sort(sample(seq_len(nrow(g)), max_combos)), , drop = FALSE]
  }
  rownames(g) <- NULL
  g
}

# Cache de features por janela: evita recalcular indicadores identicos para
# combinacoes que compartilham 'ind_window'.
.new_feature_cache <- function() new.env(parent = emptyenv())

.features_for_window <- function(prices_df, ind_window, target_type, cache) {
  key <- as.character(ind_window)
  if (!is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }
  f <- compute_features(prices_df, ind_window = ind_window,
                        target_type = target_type)
  if (!is.null(cache)) assign(key, f, envir = cache)
  f
}

# Prepara features + split + sequencias para uma dada combinacao (janela,
# lookback). Retorna NULL se nao houver dados minimos.
prepare_company_sequences <- function(prices_df, ind_window, lookback, year,
                                      target_type, feature_cache = NULL) {
  feats <- .features_for_window(prices_df, ind_window, target_type,
                                feature_cache)
  feats <- assign_temporal_split(feats, year)
  fcols <- feature_columns(feats)

  chk <- check_min_rows(feats, fcols, lookback)
  if (!chk$ok) return(list(ok = FALSE, msg = chk$msg))

  seqs <- tryCatch(build_sequences(feats, fcols, lookback),
                   error = function(e) e)
  if (inherits(seqs, "error")) {
    return(list(ok = FALSE, msg = conditionMessage(seqs)))
  }
  if (dim(seqs$train$X)[1] == 0 || dim(seqs$val$X)[1] == 0) {
    return(list(ok = FALSE, msg = "sem sequencias de treino/validacao"))
  }
  list(ok = TRUE, seqs = seqs)
}

# Avalia UMA combinacao de hiperparametros na validacao. Devolve as metricas de
# validacao (accuracy/f1) ou NULL em caso de inviabilidade.
evaluate_hp_on_val <- function(prices_df, hp, year, target_type, seed,
                               rnn_type, threshold, feature_cache = NULL) {
  prep <- prepare_company_sequences(
    prices_df, hp$ind_window, hp$lookback, year, target_type, feature_cache)
  if (!isTRUE(prep$ok)) return(list(ok = FALSE, msg = prep$msg))
  seqs <- prep$seqs

  model <- build_rnn_model(
    lookback = hp$lookback, n_features = length(seqs$feat_cols),
    units = hp$units, dropout = hp$dropout,
    learning_rate = hp$learning_rate, rnn_type = rnn_type)

  train_rnn_model(
    model, seqs$train$X, seqs$train$y, seqs$val$X, seqs$val$y,
    epochs = hp$epochs, batch_size = hp$batch_size, seed = seed)

  val_prob <- predict_prob(model, seqs$val$X)
  val_pred <- as.integer(val_prob > threshold)
  m <- classification_metrics(seqs$val$y, val_pred, positive = 1)

  # Libera memoria do backend entre combinacoes.
  keras::k_clear_session()

  list(ok = TRUE, val_accuracy = m$accuracy, val_precision = m$precision,
       val_recall = m$recall, val_f1 = m$f1,
       n_train = dim(seqs$train$X)[1], n_val = dim(seqs$val$X)[1])
}

# Executa a busca completa para uma empresa. Cada combinacao roda dentro de
# tryCatch para que uma falha isolada nao interrompa a empresa.
#
# Retorna lista com:
#   $best_hp  : linha (data.frame) da melhor combinacao
#   $results  : data.frame com metricas de validacao de todas as combinacoes
tune_company <- function(prices_df, hp_grid, year, target_type = "next_day_up",
                         seed = 42, rnn_type = "gru", threshold = 0.5,
                         selection_metric = c("accuracy", "f1"), log = cat) {
  selection_metric <- match.arg(selection_metric)
  metric_col <- if (selection_metric == "accuracy") "val_accuracy" else "val_f1"
  cache <- .new_feature_cache()

  rows <- vector("list", nrow(hp_grid))
  for (i in seq_len(nrow(hp_grid))) {
    hp <- as.list(hp_grid[i, , drop = FALSE])
    res <- tryCatch(
      evaluate_hp_on_val(prices_df, hp, year, target_type, seed, rnn_type,
                         threshold, feature_cache = cache),
      error = function(e) list(ok = FALSE, msg = conditionMessage(e)))

    if (isTRUE(res$ok)) {
      rows[[i]] <- data.frame(
        hp_grid[i, , drop = FALSE],
        val_accuracy = res$val_accuracy, val_precision = res$val_precision,
        val_recall = res$val_recall, val_f1 = res$val_f1,
        n_train = res$n_train, n_val = res$n_val,
        status = "ok", stringsAsFactors = FALSE)
      log(glue::glue("  [tuning {i}/{nrow(hp_grid)}] ",
                     "win={hp$ind_window} lb={hp$lookback} u={hp$units} ",
                     "drop={hp$dropout} -> val_acc=",
                     "{round(res$val_accuracy, 3)} val_f1={round(res$val_f1, 3)}"))
    } else {
      rows[[i]] <- data.frame(
        hp_grid[i, , drop = FALSE],
        val_accuracy = NA_real_, val_precision = NA_real_,
        val_recall = NA_real_, val_f1 = NA_real_,
        n_train = NA_integer_, n_val = NA_integer_,
        status = substr(res$msg, 1, 80), stringsAsFactors = FALSE)
      log(glue::glue("  [tuning {i}/{nrow(hp_grid)}] descartada: {res$msg}"),
          level = "WARN")
    }
  }

  results <- do.call(rbind, rows)
  ok <- results[results$status == "ok" & !is.na(results[[metric_col]]), ,
                drop = FALSE]
  if (nrow(ok) == 0) stop("nenhuma combinacao de hiperparametros viavel")

  # Melhor por metrica de validacao; desempate por val_f1 e menor complexidade.
  ord <- order(-ok[[metric_col]], -ok$val_f1, ok$units, ok$lookback)
  best_hp <- ok[ord[1], , drop = FALSE]
  rownames(best_hp) <- NULL

  list(best_hp = best_hp, results = results)
}

# Treina o MODELO FINAL da empresa com os hiperparametros vencedores e devolve
# o modelo + as sequencias (incluindo teste) para avaliacao/backtest.
# A validacao entra apenas como early stopping; o TESTE nao e tocado no treino.
fit_final_model <- function(prices_df, best_hp, year, target_type,
                            seed, rnn_type) {
  prep <- prepare_company_sequences(
    prices_df, best_hp$ind_window, best_hp$lookback, year, target_type)
  if (!isTRUE(prep$ok)) stop(glue::glue("dados finais insuficientes: {prep$msg}"))
  seqs <- prep$seqs

  model <- build_rnn_model(
    lookback = best_hp$lookback, n_features = length(seqs$feat_cols),
    units = best_hp$units, dropout = best_hp$dropout,
    learning_rate = best_hp$learning_rate, rnn_type = rnn_type)

  history <- train_rnn_model(
    model, seqs$train$X, seqs$train$y, seqs$val$X, seqs$val$y,
    epochs = best_hp$epochs, batch_size = best_hp$batch_size, seed = seed)

  list(model = model, seqs = seqs, history = history)
}
