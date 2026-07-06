# =============================================================================
# 05_sequences.R  -  Montagem de sequencias para a rede recorrente
# =============================================================================
# Transforma a matriz de features (ordenada no tempo) em tensores 3D
# [amostras, lookback, n_features] exigidos pela GRU/LSTM.
#
# DECISOES ANTI-VAZAMENTO (criticas):
#   * O SCALER (media/desvio) e ajustado SOMENTE nas linhas de TREINO e aplicado
#     a todas as demais -- validacao e teste jamais influenciam a escala.
#   * As sequencias sao construidas sobre a serie continua (o lookback de uma
#     amostra de validacao pode incluir dias de treino: isso e dado PASSADO,
#     nao vazamento). Cada sequencia e rotulada pelo split da sua DATA-ALVO.
#   * Nenhuma etapa embaralha o tempo; a ordem cronologica e mantida.
# =============================================================================

# Constroi os tensores por split.
# Retorna lista com:
#   $train,$val,$test : cada um lista(X = array 3D, y = vetor,
#                                     dates = Date, fwd_ret = numeric)
#   $scaler           : parametros de padronizacao (center/scale) do treino
#   $feat_cols        : nomes das features (ordem das colunas do tensor)
build_sequences <- function(features_df, feat_cols, lookback) {
  df <- features_df[order(features_df$Date), , drop = FALSE]
  X_mat <- as.matrix(df[, feat_cols, drop = FALSE])
  n     <- nrow(df)
  p     <- length(feat_cols)

  # --- Scaler ajustado APENAS no treino ------------------------------------
  train_rows <- which(df$split == "train" &
                        stats::complete.cases(X_mat) & !is.na(df$target))
  if (length(train_rows) < lookback + 1) {
    stop("linhas de treino insuficientes para ajustar o scaler")
  }
  scaler <- standardize_fit(X_mat[train_rows, , drop = FALSE])
  X_scaled <- standardize_apply(X_mat, scaler)

  # --- Varredura temporal para montar as janelas ---------------------------
  # A amostra i usa as linhas (i-lookback+1):i para prever target[i].
  idx_by_split <- list(train = integer(0), val = integer(0), test = integer(0))
  for (i in seq(lookback, n)) {
    win <- (i - lookback + 1):i
    sp  <- as.character(df$split[i])
    if (!sp %in% c("train", "val", "test")) next
    if (anyNA(X_scaled[win, ]) || is.na(df$target[i])) next  # descarta janela invalida
    idx_by_split[[sp]] <- c(idx_by_split[[sp]], i)
  }

  make_arrays <- function(idx) {
    if (length(idx) == 0) {
      return(list(X = array(0, dim = c(0, lookback, p)),
                  y = numeric(0), dates = as.Date(character(0)),
                  fwd_ret = numeric(0)))
    }
    X <- array(0, dim = c(length(idx), lookback, p))
    for (k in seq_along(idx)) {
      win <- (idx[k] - lookback + 1):idx[k]
      X[k, , ] <- X_scaled[win, , drop = FALSE]
    }
    list(X = X,
         y = df$target[idx],
         dates = df$Date[idx],
         fwd_ret = df$fwd_ret[idx])
  }

  list(
    train    = make_arrays(idx_by_split$train),
    val      = make_arrays(idx_by_split$val),
    test     = make_arrays(idx_by_split$test),
    scaler   = scaler,
    feat_cols = feat_cols
  )
}
