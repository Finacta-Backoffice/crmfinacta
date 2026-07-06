# =============================================================================
# utils.R  -  Funcoes utilitarias transversais ao pipeline
# =============================================================================
# Contem helpers de logging, criacao de diretorios, padronizacao (scaler),
# metricas de classificacao e pequenas conveniencias usadas pelos demais
# modulos. Nenhuma logica de modelagem vive aqui.
# =============================================================================

# ------------------------------------------------------------------ diretorios
# Garante que a estrutura de pastas exista antes da escrita de artefatos.
ensure_dirs <- function(paths) {
  for (p in paths) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(paths)
}

# ------------------------------------------------------------------- logging
# Logger simples com timestamp que escreve simultaneamente no console e num
# arquivo de log. Retorna a mensagem de forma invisivel para encadeamento.
new_logger <- function(log_file) {
  con <- file(log_file, open = "wt")
  force(con)
  function(..., level = "INFO") {
    msg <- paste0(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", level, "] ",
      paste0(..., collapse = "")
    )
    cat(msg, "\n", sep = "")
    writeLines(msg, con)
    flush(con)
    invisible(msg)
  }
}

# ------------------------------------------------------- padronizacao (scaler)
# IMPORTANTE (vazamento de informacao): os parametros de escala (media e desvio)
# devem ser estimados SOMENTE no conjunto de treino. Estas duas funcoes separam
# explicitamente o "fit" (estimacao) do "apply" (aplicacao) justamente para
# tornar impossivel usar dados de validacao/teste na estimativa dos parametros.
standardize_fit <- function(mat) {
  center <- apply(mat, 2, mean, na.rm = TRUE)
  scale_ <- apply(mat, 2, stats::sd, na.rm = TRUE)
  # Colunas constantes (sd == 0) recebem escala 1 para evitar divisao por zero.
  scale_[!is.finite(scale_) | scale_ == 0] <- 1
  list(center = center, scale = scale_)
}

standardize_apply <- function(mat, scaler) {
  sweep(sweep(mat, 2, scaler$center, "-"), 2, scaler$scale, "/")
}

# --------------------------------------------------- metricas de classificacao
# Matriz de confusao e metricas derivadas calculadas em R base (sem dependencia
# rigida de yardstick) para robustez. 'positive' e a classe de interesse (1).
classification_metrics <- function(y_true, y_pred, positive = 1) {
  y_true <- as.integer(y_true)
  y_pred <- as.integer(y_pred)

  tp <- sum(y_pred == positive & y_true == positive)
  tn <- sum(y_pred != positive & y_true != positive)
  fp <- sum(y_pred == positive & y_true != positive)
  fn <- sum(y_pred != positive & y_true == positive)

  n         <- length(y_true)
  accuracy  <- ifelse(n > 0, (tp + tn) / n, NA_real_)
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), NA_real_)
  recall    <- ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_)
  f1        <- ifelse(!is.na(precision) && !is.na(recall) &&
                        (precision + recall) > 0,
                      2 * precision * recall / (precision + recall), NA_real_)

  confusion <- matrix(c(tn, fp, fn, tp), nrow = 2, byrow = TRUE,
                      dimnames = list(Real = c("0", "1"),
                                      Previsto = c("0", "1")))

  list(accuracy = accuracy, precision = precision, recall = recall,
       f1 = f1, tp = tp, tn = tn, fp = fp, fn = fn, confusion = confusion)
}

# ------------------------------------------------------------------- diversos
# Coersao segura de percentuais/volumes textuais nao usada no fluxo padrao,
# mantida como conveniencia para planilhas alternativas.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
