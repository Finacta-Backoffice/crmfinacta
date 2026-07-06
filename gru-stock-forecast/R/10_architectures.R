# =============================================================================
# 10_architectures.R  -  Geracao empirica de arquiteturas (busca de topologia)
# =============================================================================
# Abordagem empirica para testar diferentes topologias de rede. A funcao
# 'arquitetura_redes' e uma adaptacao da versao original (escrita para o pacote
# neuralnet) para o nosso contexto de GRU EMPILHADA:
#
#   * Cada vetor retornado representa as UNIDADES POR CAMADA RECORRENTE.
#     Ex.: c(32)      -> 1 camada GRU de 32 unidades
#          c(32, 32)  -> 2 camadas GRU de 32 unidades (empilhadas)
#          c(16,16,16)-> 3 camadas GRU de 16 unidades
#
#   * A versao original iterava 'seq(1, max_neuronios, by = 1)', o que gera
#     larguras de 1..N (a maioria inutil para GRU). Aqui o 2o argumento aceita:
#       - um ESCALAR  -> comportamento original (1:max_neuronios); ou
#       - um VETOR    -> conjunto curado de larguras (ex.: c(16, 32, 64)).
#
# OBS.: o pacote neuralnet (MLP feedforward) NAO e usado aqui -- ele nao modela
# sequencias temporais. Reaproveitamos apenas a IDEIA de varrer topologias.
# =============================================================================

# Gera a lista de arquiteturas (vetores de unidades por camada) a serem testadas.
#   max_camadas : numero maximo de camadas recorrentes empilhadas
#   neuronios   : escalar (=> 1:neuronios) ou vetor de larguras candidatas
arquitetura_redes <- function(max_camadas, neuronios) {
  # Compatibilidade com a assinatura original (escalar => 1..max).
  larguras <- if (length(neuronios) == 1L) seq_len(neuronios) else neuronios

  lista_arquiteturas <- list()
  for (n_camadas in seq_len(max_camadas)) {
    for (u in larguras) {
      # Arquitetura "retangular": todas as camadas com a mesma largura 'u'.
      lista_arquiteturas <- c(lista_arquiteturas, list(rep(as.integer(u),
                                                           n_camadas)))
    }
  }
  lista_arquiteturas
}

# Representacao textual legivel de uma arquitetura, para logs/CSVs.
#   c(32, 32) -> "32-32"
arch_to_str <- function(arch) paste(arch, collapse = "-")
