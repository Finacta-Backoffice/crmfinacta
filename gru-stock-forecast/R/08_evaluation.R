# =============================================================================
# 08_evaluation.R  -  Avaliacao no conjunto de TESTE FINAL
# =============================================================================
# Calcula, no periodo de teste (16/10 a 31/12), as metricas de classificacao
# exigidas: accuracy, precision, recall, F1 e matriz de confusao. As metricas
# de retorno (estrategia vs. buy&hold) ficam em 09_backtest.R.
#
# O conjunto de teste e avaliado UMA UNICA VEZ, com a configuracao ja congelada
# pela validacao -- sem qualquer reotimizacao.
# =============================================================================

# Avalia as previsoes de teste. Retorna metricas de classificacao + a tabela
# de previsoes (Date, prob, pred, y_true, fwd_ret) para persistencia.
evaluate_on_test <- function(model, seqs, threshold = 0.5) {
  test <- seqs$test
  if (dim(test$X)[1] == 0) {
    stop("sem sequencias de teste para avaliar")
  }

  prob <- predict_prob(model, test$X)
  pred <- as.integer(prob > threshold)
  y    <- as.integer(test$y)

  m <- classification_metrics(y, pred, positive = 1)

  # Taxa de acerto direcional (o sinal previsto bate com o retorno realizado).
  directional_hit <- mean((prob > threshold) == (test$fwd_ret > 0))

  predictions <- data.frame(
    Date    = test$dates,
    prob    = prob,
    pred    = pred,
    y_true  = y,
    fwd_ret = test$fwd_ret,
    stringsAsFactors = FALSE
  )

  list(
    metrics = list(
      accuracy = m$accuracy, precision = m$precision, recall = m$recall,
      f1 = m$f1, directional_hit = directional_hit, n_test = length(y)),
    confusion = m$confusion,
    predictions = predictions
  )
}
