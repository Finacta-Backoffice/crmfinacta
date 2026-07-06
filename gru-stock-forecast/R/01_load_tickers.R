# =============================================================================
# 01_load_tickers.R  -  Leitura e limpeza do universo de tickers
# =============================================================================
# A planilha "Maiores empresas" e um export do screener de maior valor de
# mercado do Yahoo Finance. Ela NAO contem historico diario; serve apenas como
# universo inicial de tickers.
#
# PARTICULARIDADES OBSERVADAS NA PLANILHA (tratadas abaixo):
#   * Cada empresa ocupa DUAS linhas. Na 1a linha o simbolo aparece truncado
#     (ex.: "G" no lugar de "GOOGL"); na 2a linha (sem "rank") aparece o
#     ticker limpo. Usamos, portanto, o ticker da 2a linha como fonte confiavel.
#   * O universo contem acoes preferenciais (JPM-PD, BAC-PL, BML-PG...),
#     multiplas classes (BRK-A/BRK-B, GOOG/GOOGL) e ADRs/ordinarias OTC
#     estrangeiras (TSMWF, RHHBY, NSRGY, SSNLF...), sem historico diario
#     confiavel no Yahoo. Tudo isso e filtrado e registrado em log.
#
# Se voce trocar a planilha por um export "padrao" (colunas Symbol / Name em
# uma linha por empresa), a funcao tenta detectar esse layout automaticamente.
# =============================================================================

# ------------------------------------------------------------- leitura crua
# Escolhe a aba de dados (a maior) e le a grade sem cabecalho, para lidar com
# layouts deslocados. Retorna um data.frame de caracteres.
.read_raw_sheet <- function(excel_path) {
  sheets <- readxl::excel_sheets(excel_path)
  # Seleciona a aba com maior area util (ignora abas de metadados como "Fonte").
  dims <- vapply(sheets, function(s) {
    d <- suppressMessages(readxl::read_excel(excel_path, sheet = s,
                                             col_names = FALSE))
    nrow(d) * ncol(d)
  }, numeric(1))
  data_sheet <- sheets[which.max(dims)]

  raw <- suppressMessages(readxl::read_excel(
    excel_path, sheet = data_sheet, col_names = FALSE, .name_repair = "minimal"))
  as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)
}

# ----------------------------------------------------- extracao de tickers/nome
# Estrategia robusta ao layout observado:
#   1) identifica a coluna com maior densidade de valores "parecidos com ticker";
#   2) usa a primeira coluna numerica como "rank" para distinguir a linha
#      primaria (truncada) da secundaria (ticker limpo);
#   3) pareia o ticker limpo (linha secundaria) com o nome (linha primaria).
# Fallback: se detectar um cabecalho "Symbol"/"Name", le no formato padrao.
.looks_like_ticker <- function(x) {
  grepl("^[A-Z]{1,6}([.\\-][A-Z0-9]{1,4})?$", x)
}

extract_ticker_table <- function(excel_path, log) {
  raw <- .read_raw_sheet(excel_path)
  log(glue::glue("Planilha lida: {nrow(raw)} linhas x {ncol(raw)} colunas."))

  # --- Fallback: layout padrao com cabecalho "Symbol"/"Name" -----------------
  header_hit <- which(apply(raw, c(1, 2), function(v)
    !is.na(v) && tolower(trimws(v)) %in% c("symbol", "ticker")), arr.ind = TRUE)
  if (nrow(header_hit) > 0) {
    hr <- header_hit[1, "row"]; sc <- header_hit[1, "col"]
    name_col <- which(tolower(trimws(unlist(raw[hr, ]))) == "name")
    body <- raw[(hr + 1):nrow(raw), , drop = FALSE]
    tk <- toupper(trimws(body[[sc]]))
    nm <- if (length(name_col) > 0) trimws(body[[name_col[1]]]) else NA_character_
    df <- data.frame(rank = seq_along(tk), ticker = tk, name = nm,
                     stringsAsFactors = FALSE)
    df <- df[.looks_like_ticker(df$ticker) & !is.na(df$ticker), , drop = FALSE]
    if (nrow(df) > 0) {
      log(glue::glue("Layout padrao detectado: {nrow(df)} tickers extraidos."))
      return(df)
    }
  }

  # --- Layout observado (duas linhas por empresa, ticker limpo na 2a) --------
  # Coluna de ticker = aquela com mais valores no formato de ticker.
  ticker_col <- which.max(vapply(raw, function(col)
    sum(.looks_like_ticker(toupper(trimws(col))), na.rm = TRUE), numeric(1)))
  # Coluna de "rank" = primeira coluna majoritariamente numerica.
  rank_col <- which(vapply(raw, function(col)
    mean(grepl("^[0-9]+$", trimws(col)), na.rm = TRUE) > 0.3, logical(1)))[1]
  # Coluna de nome = coluna imediatamente apos a de ticker (heuristica do export).
  name_col <- min(ticker_col + 1, ncol(raw))

  records <- list()
  current_name <- NA_character_; current_rank <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    rk <- suppressWarnings(as.integer(trimws(raw[i, rank_col])))
    tk <- toupper(trimws(raw[i, ticker_col]))
    nm <- trimws(raw[i, name_col])
    if (!is.na(rk)) {
      # Linha primaria: guarda rank e nome; o simbolo aqui pode estar truncado.
      current_rank <- rk
      current_name <- if (!is.na(nm) && nzchar(nm)) nm else current_name
    } else if (.looks_like_ticker(tk) && !is.na(current_rank)) {
      # Linha secundaria: ticker limpo -> registra o par (rank, ticker, nome).
      # A checagem '!is.na(current_rank)' descarta artefatos de cabecalho (ex.:
      # a celula "Name") que aparecem antes da primeira linha com rank.
      records[[length(records) + 1]] <- data.frame(
        rank = current_rank, ticker = tk, name = current_name,
        stringsAsFactors = FALSE)
    }
  }
  df <- do.call(rbind, records)
  df <- df[!is.na(df$ticker) & nzchar(df$ticker), , drop = FALSE]
  df <- df[order(df$rank, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  log(glue::glue("Layout 'duas linhas por empresa' detectado: ",
                 "{nrow(df)} tickers limpos extraidos."))
  df
}

# --------------------------------------------- limpeza e regras de elegibilidade
# Aplica filtros reproduziveis e retorna a tabela COMPLETA com colunas
# 'eligible' (logico) e 'reason' (motivo do descarte), preservando o rastro
# para o log de exclusoes.
apply_eligibility_rules <- function(ticker_df, log) {
  df <- ticker_df
  df$ticker <- toupper(trimws(df$ticker))
  df$eligible <- TRUE
  df$reason   <- NA_character_

  flag <- function(mask, reason) {
    take <- mask & df$eligible
    df$eligible[take] <<- FALSE
    df$reason[take]   <<- reason
  }

  # (1) Acoes preferenciais: padrao "-P" (ex.: JPM-PD, BAC-PL, BML-PG).
  flag(grepl("-P[A-Z]?$", df$ticker), "acao_preferencial")

  # (2) ADR/ordinaria OTC estrangeira: heuristica de 5 letras terminadas em
  #     F ou Y (pink sheets), tipicamente sem historico diario confiavel.
  flag(grepl("^[A-Z]{5}$", df$ticker) & grepl("[FY]$", df$ticker),
       "otc_estrangeira")

  # (3) Listagens duplicadas da MESMA empresa (multiplas classes / listagens):
  #     mantem a primeira ocorrencia elegivel por nome (maior valor de mercado).
  seen <- character(0)
  for (i in which(df$eligible)) {
    key <- toupper(df$name[i])
    if (!is.na(key) && key %in% seen) {
      df$eligible[i] <- FALSE
      df$reason[i]   <- "listagem_duplicada"
    } else if (!is.na(key)) {
      seen <- c(seen, key)
    }
  }

  n_elig <- sum(df$eligible)
  log(glue::glue("Elegibilidade aplicada: {n_elig}/{nrow(df)} tickers elegiveis."))
  for (r in unique(stats::na.omit(df$reason))) {
    log(glue::glue("  descartados por '{r}': {sum(df$reason == r, na.rm = TRUE)}"))
  }
  df
}

# -------------------------------------------------- API de alto nivel do modulo
# Retorna lista com:
#   $all       : tabela completa (rank, ticker, name, eligible, reason)
#   $eligible  : subconjunto elegivel em ordem de rank
load_ticker_universe <- function(excel_path, log) {
  raw_tbl <- extract_ticker_table(excel_path, log)
  full    <- apply_eligibility_rules(raw_tbl, log)
  list(all = full, eligible = full[full$eligible, , drop = FALSE])
}
