# =============================================================================
# 02_download_data.R  -  Download de dados historicos diarios (Yahoo Finance)
# =============================================================================
# Baixa OHLCV + Adjusted via quantmod (fonte coerente com a planilha, que veio
# do screener do Yahoo). Baixamos alguns meses ANTES de 01/01 do ano analisado
# para dar "aquecimento" aos indicadores tecnicos e ao lookback da rede -- essas
# linhas de aquecimento (datas < 01/01) servem apenas como historico e NUNCA
# entram como alvos de treino/validacao/teste (a divisao e feita por data).
# =============================================================================

# Converte o objeto xts do quantmod em data.frame padronizado com colunas:
#   Date, Open, High, Low, Close, Adjusted, Volume
.xts_to_df <- function(x, ticker) {
  df <- data.frame(
    Date     = as.Date(zoo::index(x)),
    Open     = as.numeric(quantmod::Op(x)),
    High     = as.numeric(quantmod::Hi(x)),
    Low      = as.numeric(quantmod::Lo(x)),
    Close    = as.numeric(quantmod::Cl(x)),
    Adjusted = as.numeric(quantmod::Ad(x)),
    Volume   = as.numeric(quantmod::Vo(x)),
    stringsAsFactors = FALSE
  )
  df[order(df$Date), , drop = FALSE]
}

# Faz o download de um unico ticker. Lanca erro (capturado pelo chamador) em
# caso de falha ou historico insuficiente. 'min_rows' e uma checagem minima.
download_one <- function(ticker, from, to, min_rows = 150) {
  # Padroniza para o formato do Yahoo (classes usam '-', ex.: BRK-B).
  yf_ticker <- gsub("\\.", "-", toupper(trimws(ticker)))

  x <- quantmod::getSymbols(
    yf_ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE
  )
  if (is.null(x) || nrow(x) < min_rows) {
    stop(glue::glue("historico insuficiente ({ifelse(is.null(x), 0, nrow(x))} ",
                    "linhas < {min_rows})"))
  }
  df <- .xts_to_df(x, yf_ticker)
  df <- df[stats::complete.cases(df[, c("Open", "High", "Low", "Close",
                                        "Adjusted")]), , drop = FALSE]
  if (nrow(df) < min_rows) {
    stop(glue::glue("historico insuficiente apos limpeza ({nrow(df)} linhas)"))
  }
  df
}

# Baixa dados para uma lista de tickers COM tolerancia a falhas (tryCatch por
# empresa) e "backfill" opcional: percorre a lista de candidatos ate obter
# 'n_target' downloads bem-sucedidos.
#
# Retorna lista com:
#   $data     : named list ticker -> data.frame OHLCV
#   $used     : tickers efetivamente usados
#   $failures : data.frame (ticker, reason) das falhas de download
download_universe <- function(candidate_tickers, from, to, n_target,
                              data_raw_dir, log, min_rows = 150,
                              backfill = TRUE) {
  data     <- list()
  failures <- list()
  used     <- character(0)

  for (tk in candidate_tickers) {
    if (length(used) >= n_target) break
    res <- tryCatch(
      download_one(tk, from = from, to = to, min_rows = min_rows),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      failures[[length(failures) + 1]] <- data.frame(
        ticker = tk, reason = conditionMessage(res), stringsAsFactors = FALSE)
      log(glue::glue("Download FALHOU para {tk}: {conditionMessage(res)}"),
          level = "WARN")
      if (!backfill) next
    } else {
      data[[tk]] <- res
      used <- c(used, tk)
      # Persiste o dado bruto para reprodutibilidade / inspecao.
      utils::write.csv(res, file.path(data_raw_dir, glue::glue("{tk}.csv")),
                       row.names = FALSE)
      log(glue::glue("Download OK para {tk}: {nrow(res)} linhas ",
                     "({min(res$Date)} a {max(res$Date)})."))
    }
  }

  fail_df <- if (length(failures)) do.call(rbind, failures) else
    data.frame(ticker = character(0), reason = character(0))
  list(data = data, used = used, failures = fail_df)
}
