# =============================================================================
# 03_features.R  -  Engenharia de atributos (indicadores tecnicos) e target
# =============================================================================
# A JANELA dos indicadores tecnicos e um HIPERPARAMETRO (5 a 21 dias): esta
# funcao recalcula todos os indicadores em funcao de 'ind_window', o que permite
# ao tuning testar diferentes janelas na validacao.
#
# NOTA SOBRE VAZAMENTO DE INFORMACAO:
#   * Todos os indicadores usam apenas janelas passadas/atuais (TTR usa janelas
#     "trailing"), portanto NAO ha look-ahead nas features do instante t.
#   * O TARGET usa o retorno de t+1 (lead), isto e, informacao do futuro -- por
#     construcao e o que queremos prever; ele nunca e usado como feature.
#   * 'fwd_ret' (retorno realizado em t+1) e guardado apenas para o backtest e
#     jamais entra na matriz de features.
# =============================================================================

# ------------------------------------------------------- definicao do target
# Alvo binario padrao: 1 se o retorno do proximo dia util for positivo.
# Para trocar a definicao depois, edite ESTA funcao (ex.: usar limiar > custo,
# retorno acumulado em h dias, etc.).
make_target <- function(fwd_ret, type = "next_day_up") {
  switch(
    type,
    next_day_up = as.integer(fwd_ret > 0),
    stop(glue::glue("Tipo de target nao suportado: {type}"))
  )
}

# ------------------------------------------------------- calculo das features
# Recebe o data.frame OHLCV e uma janela candidata; devolve um data.frame com:
#   Date, <features...>, target, fwd_ret
# Mantem as linhas de aquecimento (com NA nas features) para preservar os
# indices temporais -- a filtragem de NA e feita na montagem das sequencias.
compute_features <- function(prices_df, ind_window, target_type = "next_day_up") {
  df <- prices_df[order(prices_df$Date), , drop = FALSE]

  price <- df$Adjusted
  vol   <- df$Volume
  hlc   <- df[, c("High", "Low", "Close")]

  # Retorno diario simples (base para varias features e para o target).
  ret1 <- c(NA, price[-1] / price[-length(price)] - 1)

  # Medias moveis e distancia do preco a media.
  sma <- TTR::SMA(price, n = ind_window)
  ema <- TTR::EMA(price, n = ind_window)
  dist_sma <- (price - sma) / sma

  # Volatilidade movel (desvio-padrao dos retornos na janela).
  vol_roll <- TTR::runSD(ret1, n = ind_window)

  # RSI e momentum na mesma janela.
  rsi <- TTR::RSI(price, n = ind_window)
  mom <- TTR::momentum(price, n = ind_window)

  # MACD com parametros convencionais (12/26/9) -- independentes da janela
  # candidata; incluido como feature adicional robusta.
  macd_mat <- tryCatch(
    TTR::MACD(price, nFast = 12, nSlow = 26, nSig = 9),
    error = function(e) matrix(NA_real_, nrow = length(price), ncol = 2))
  macd_line   <- macd_mat[, 1]
  macd_signal <- macd_mat[, 2]
  macd_hist   <- macd_line - macd_signal

  # Volume relativo (volume / media movel de volume na janela).
  vol_sma <- TTR::SMA(vol, n = ind_window)
  vol_rel <- vol / vol_sma

  # Estocastico %K (usa HLC) na janela candidata.
  stoch_k <- tryCatch(
    TTR::stoch(hlc, nFastK = ind_window, nFastD = 3, nSlowD = 3)[, "fastK"],
    error = function(e) rep(NA_real_, length(price)))

  # Retorno do proximo dia (lead) -> base do target e do backtest.
  fwd_ret <- c(ret1[-1], NA)  # ret1 deslocado uma posicao para frente
  target  <- make_target(fwd_ret, type = target_type)

  out <- data.frame(
    Date        = df$Date,
    ret1        = ret1,
    sma_dist    = dist_sma,
    ema_ret     = c(NA, ema[-1] / ema[-length(ema)] - 1),
    volatility  = vol_roll,
    rsi         = rsi,
    momentum    = mom,
    macd        = macd_line,
    macd_signal = macd_signal,
    macd_hist   = macd_hist,
    vol_rel     = vol_rel,
    stoch_k     = stoch_k,
    target      = target,
    fwd_ret     = fwd_ret,
    stringsAsFactors = FALSE
  )
  out
}

# Nomes das colunas de feature (tudo exceto colunas de controle). Inclui 'split'
# na exclusao pois esse rotulo e adicionado por 04_split_data.R apos as features.
feature_columns <- function(features_df) {
  setdiff(colnames(features_df), c("Date", "target", "fwd_ret", "split"))
}
