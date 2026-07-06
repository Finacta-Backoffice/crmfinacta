# =============================================================================
# 06_model_gru.R  -  Definicao e treino da rede recorrente (GRU / LSTM)
# =============================================================================
# Arquitetura simples de 1 camada recorrente, conforme escopo da primeira
# versao. O tipo de celula e parametrizavel ('gru' por padrao; 'lstm' como
# alternativa caso haja dificuldade tecnica com GRU no seu ambiente).
# =============================================================================

# Constroi o modelo Keras (GRU por padrao).
build_rnn_model <- function(lookback, n_features, units = 32, dropout = 0.2,
                            learning_rate = 1e-3, rnn_type = c("gru", "lstm")) {
  rnn_type <- match.arg(rnn_type)

  rnn_layer <- if (rnn_type == "gru") keras::layer_gru else keras::layer_lstm

  # Construcao encadeada (captura o retorno) para robustez entre versoes do
  # keras: 1 camada recorrente -> camada densa sigmoide (saida de probabilidade).
  model <- keras::keras_model_sequential() %>%
    rnn_layer(
      units = units,
      input_shape = c(lookback, n_features),
      dropout = dropout,
      recurrent_dropout = 0
    ) %>%
    keras::layer_dense(units = 1, activation = "sigmoid")

  model %>% keras::compile(
    optimizer = keras::optimizer_adam(learning_rate = learning_rate),
    loss      = "binary_crossentropy",
    metrics   = "accuracy"
  )
  model
}

# Treina o modelo. A VALIDACAO e usada APENAS para early stopping e selecao de
# hiperparametros -- nunca para ajustar pesos diretamente (o gradiente vem so do
# treino). 'restore_best_weights' devolve o melhor estado segundo val_loss.
train_rnn_model <- function(model, X_train, y_train, X_val, y_val,
                            epochs = 30, batch_size = 32, patience = 6,
                            seed = 42, verbose = 0) {
  # Reprodutibilidade do backend (fixa sementes de R/NumPy/TensorFlow).
  if (requireNamespace("tensorflow", quietly = TRUE)) {
    tensorflow::set_random_seed(seed)
  }

  callbacks <- list(
    keras::callback_early_stopping(
      monitor = "val_loss", patience = patience, restore_best_weights = TRUE)
  )

  has_val <- !is.null(X_val) && dim(X_val)[1] > 0
  history <- model %>% keras::fit(
    x = X_train, y = y_train,
    validation_data = if (has_val) list(X_val, y_val) else NULL,
    epochs = epochs, batch_size = batch_size,
    callbacks = if (has_val) callbacks else list(),
    shuffle = FALSE,          # NUNCA embaralhar: preserva a ordem temporal
    verbose = verbose
  )
  history
}

# Retorna as probabilidades previstas (classe 1) para um tensor de entrada.
predict_prob <- function(model, X) {
  if (dim(X)[1] == 0) return(numeric(0))
  as.numeric(stats::predict(model, X, verbose = 0))
}
