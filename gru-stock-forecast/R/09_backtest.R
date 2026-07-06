# =============================================================================
# 09_backtest.R  -  Backtest simples e transparente (periodo de teste)
# =============================================================================
# Estrategia (long-only, 1 dia):
#   * Se probabilidade prevista > threshold (default 0.5): comprado no proximo
#     pregao por 1 dia, capturando o retorno realizado 'fwd_ret'.
#   * Caso contrario: em caixa (retorno 0 no dia).
#
# PREMISSAS / SIMPLIFICACOES (primeira versao):
#   * Sem custos de transacao, slippage ou impostos.
#   * Sem alavancagem; posicao integral (100%) quando comprado.
#   * Retorno close-to-close via 'fwd_ret' (retorno de t+1).
# Compara com buy&hold do mesmo ativo no mesmo periodo.
# =============================================================================

backtest_strategy <- function(predictions, threshold = 0.5) {
  df <- predictions[order(predictions$Date), , drop = FALSE]

  signal     <- df$prob > threshold                 # 1 = comprado; 0 = caixa
  strat_ret  <- ifelse(signal, df$fwd_ret, 0)       # retorno da estrategia/dia
  bh_ret     <- df$fwd_ret                          # buy & hold

  strat_curve <- cumprod(1 + strat_ret)
  bh_curve    <- cumprod(1 + bh_ret)

  n_trades   <- sum(signal)
  wins       <- sum(signal & df$fwd_ret > 0)
  win_rate   <- ifelse(n_trades > 0, wins / n_trades, NA_real_)
  avg_trade  <- ifelse(n_trades > 0, mean(strat_ret[signal]), NA_real_)

  metrics <- list(
    strat_cum_return = strat_curve[length(strat_curve)] - 1,
    bh_cum_return    = bh_curve[length(bh_curve)] - 1,
    n_trades         = n_trades,
    win_rate         = win_rate,
    avg_return_per_trade = avg_trade,
    avg_return_per_day   = mean(strat_ret)
  )

  daily <- data.frame(
    Date        = df$Date,
    prob        = df$prob,
    signal      = as.integer(signal),
    fwd_ret     = df$fwd_ret,
    strat_ret   = strat_ret,
    strat_equity = strat_curve,
    bh_equity    = bh_curve,
    stringsAsFactors = FALSE
  )

  list(metrics = metrics, daily = daily)
}
