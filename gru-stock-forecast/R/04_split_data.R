# =============================================================================
# 04_split_data.R  -  Divisao TEMPORAL (sem embaralhamento)
# =============================================================================
# Divisao estritamente cronologica, para o ano calendario analisado:
#   * Treino     : 01/01 a 31/08
#   * Validacao  : 01/09 a 15/10
#   * Teste final: 16/10 a 31/12
#
# Datas anteriores a 01/01 (aquecimento de indicadores/lookback) recebem o
# rotulo "warmup" e NUNCA sao usadas como alvo -- apenas como historico.
#
# NAO ha amostragem aleatoria em nenhum ponto: a ordem temporal e preservada.
# =============================================================================

# Atribui a cada linha (por Date) um rotulo de split. Retorna o data.frame de
# entrada acrescido da coluna 'split' (factor: warmup/train/val/test/post).
assign_temporal_split <- function(features_df, year) {
  d <- features_df$Date
  train_start <- as.Date(sprintf("%d-01-01", year))
  train_end   <- as.Date(sprintf("%d-08-31", year))
  val_start   <- as.Date(sprintf("%d-09-01", year))
  val_end     <- as.Date(sprintf("%d-10-15", year))
  test_start  <- as.Date(sprintf("%d-10-16", year))
  test_end    <- as.Date(sprintf("%d-12-31", year))

  split <- rep("warmup", length(d))
  split[d >= train_start & d <= train_end] <- "train"
  split[d >= val_start   & d <= val_end]   <- "val"
  split[d >= test_start  & d <= test_end]  <- "test"
  split[d > test_end]                       <- "post"

  features_df$split <- factor(
    split, levels = c("warmup", "train", "val", "test", "post"))
  features_df
}

# Verifica se cada split tem linhas suficientes (apos remover NA) para montar
# pelo menos algumas sequencias. Retorna lista com $ok (logico) e $msg.
check_min_rows <- function(features_df, feat_cols, lookback,
                           min_seq_per_split = 10) {
  ok_rows <- stats::complete.cases(features_df[, feat_cols]) &
    !is.na(features_df$target)
  counts <- table(factor(features_df$split[ok_rows],
                         levels = c("train", "val", "test")))
  needed <- lookback + min_seq_per_split
  deficient <- names(counts)[counts < needed]
  if (length(deficient) > 0) {
    return(list(ok = FALSE, msg = glue::glue(
      "linhas insuficientes em: {paste(deficient, collapse = ', ')} ",
      "(necessario >= {needed} por split; obtido ",
      "train={counts['train']}, val={counts['val']}, test={counts['test']})")))
  }
  list(ok = TRUE, msg = "ok")
}
