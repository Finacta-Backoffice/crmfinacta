# =============================================================================
# 00_packages.R  -  Dependencias e configuracao de ambiente
# =============================================================================
# Carrega (e opcionalmente instala) os pacotes usados no pipeline.
#
# REQUISITO CRITICO -> keras/tensorflow em R:
#   O componente de deep learning depende de um backend Python (TensorFlow).
#   A instalacao NAO e feita automaticamente aqui para evitar efeitos colaterais.
#   Faca UMA VEZ, no seu ambiente local:
#
#     install.packages(c("tidyverse","data.table","quantmod","TTR","readxl",
#                        "keras","tensorflow","yardstick","purrr","glue","fs"))
#     library(keras)
#     keras::install_keras()      # cria um ambiente Python + TensorFlow (CPU)
#
#   Alternativa via reticulate/conda:
#     reticulate::install_miniconda()
#     keras::install_keras(method = "conda")
#
#   Verifique a instalacao com:
#     tensorflow::tf_config()
#
#   Em maquinas sem GPU o backend CPU e suficiente para esta primeira versao
#   (amostra reduzida de empresas e 1 ano de dados).
# =============================================================================

# Pacotes obrigatorios do pipeline.
.required_pkgs <- c(
  "readxl",     # leitura da planilha Excel
  "quantmod",   # download de dados de mercado (Yahoo Finance)
  "TTR",        # indicadores tecnicos (SMA, EMA, RSI, MACD, stoch, ...)
  "data.table", # manipulacao eficiente
  "dplyr",      # verbos de transformacao (subconjunto do tidyverse)
  "tidyr",      # pivot/limpeza
  "purrr",      # programacao funcional (map/safely)
  "glue",       # interpolacao de strings para logs/paths
  "keras"       # rede neural recorrente (GRU/LSTM)
)

# Pacotes opcionais: o pipeline funciona sem eles (fallbacks em R base).
.optional_pkgs <- c("yardstick", "fs", "tensorflow")

# Carrega os pacotes obrigatorios, sinalizando claramente o que faltar.
load_packages <- function(install_missing = FALSE) {
  missing <- .required_pkgs[!vapply(
    .required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    if (install_missing) {
      install.packages(missing)
    } else {
      stop(glue::glue(
        "Pacotes ausentes: {paste(missing, collapse = ', ')}.\n",
        "Instale-os (veja o cabecalho de 00_packages.R) ou chame ",
        "load_packages(install_missing = TRUE)."
      ))
    }
  }

  suppressPackageStartupMessages({
    library(readxl)
    library(quantmod)
    library(TTR)
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(glue)
    library(keras)
  })

  # Carrega opcionais silenciosamente quando presentes.
  for (p in .optional_pkgs) {
    if (requireNamespace(p, quietly = TRUE)) {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
    }
  }
  invisible(TRUE)
}
