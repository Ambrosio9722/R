# dias: 14/10 
#       15/10
#       16/10
#       20/10
#       21/10
#       26/10
#       27/10
#       03/10
#       04/10
#░░░░░░░░░░░░░░░░░░░░░░█████████░░░░░░░░░
#░░███████░░░░░░░░░░███▒▒▒▒▒▒▒▒███░░░░░░░
#░░█▒▒▒▒▒▒█░░░░░░░███▒▒▒▒▒▒▒▒▒▒▒▒▒███░░░░
#░░░█▒▒▒▒▒▒█░░░░██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██░░
#░░░░█▒▒▒▒▒█░░░██▒▒▒▒▒██▒▒▒▒▒▒██▒▒▒▒▒███░
#░░░░░█▒▒▒█░░░█▒▒▒▒▒▒████▒▒▒▒████▒▒▒▒▒▒██
#░░░█████████████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
#░░░█▒▒▒▒▒▒▒▒▒▒▒▒█▒▒▒▒▒▒▒▒▒█▒▒▒▒▒▒▒▒▒▒▒██
#░██▒▒▒▒▒▒▒▒▒▒▒▒▒█▒▒▒██▒▒▒▒▒▒▒▒▒▒██▒▒▒▒██
#██▒▒▒███████████▒▒▒▒▒██▒▒▒▒▒▒▒▒██▒▒▒▒▒██
#█▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█▒▒▒▒▒▒████████▒▒▒▒▒▒▒██
#██▒▒▒▒▒▒▒▒▒▒▒▒▒▒█▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██░
#░█▒▒▒███████████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██░░░
#░██▒▒▒▒▒▒▒▒▒▒████▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒█░░░░░
#░░████████████░░░█████████████████░░░░░░
#_______________________  COMEÇO _______________________________________________
rm(list = ls()) # Remove as variáveis da memória
cat("\014") # Limpa o console

library(tictoc)   # Medir o tempo
#library(dplyr)   # Filtrar dados
library(Rcpp)     # Pacote para código C++

#________________  CONFIGURAÇÃO DOS PARÂMETROS__________________________________

RAINHAS <- 100                         # Número de rainhas
MAX_ITERACAO <- 10000                  # Número máximo de iterações
TABU_PARADO <- 7                       # Número máximo de movimentos na lista tabu
MELHORIA_SIGNIFICATIVA <- 5            # Critério para melhoria significativa
REINICIO_LIMITE <- 500                 # Limite sem melhoria para reiniciar 
VIZINHOS_POR_ITERACAO <- 20            # Quantidade de vizinhos gerados por iteração

melhores_vizinhos <- vector("list", VIZINHOS_POR_ITERACAO)  # Pré-alocação lista de melhores vizinhos (vetor de listas)
fitness_vizinhos <- numeric(VIZINHOS_POR_ITERACAO)  # Pré-alocação vetor de fitness de vizinhos
#__________________ VETOR RAINHAS ______________________________________________

vetorRainhas <- sample(1:RAINHAS, RAINHAS, replace = FALSE)
print(vetorRainhas)

#________________  CALCULAR OS CONFLITOS  ______________________________________

# Função em C++ usando Rcpp para calcular conflitos
cppFunction('int conflitos_cpp(IntegerVector S) {
  int n = S.size(); 
  
  int conflitos_positiva = 0, conflitos_negativo = 0;
  std::unordered_map<int, int> diag_positiva, diag_negativa;
  
  for (int i = 0; i < n; ++i) {
    diag_positiva[i - S[i]]++;
    diag_negativa[i + S[i]]++;
  }

  for (auto& kv : diag_positiva) {
    if (kv.second > 1) conflitos_positiva += kv.second - 1;
  }
  
  for (auto& kv : diag_negativa) {
    if (kv.second > 1) conflitos_negativo += kv.second - 1;
  }
  
  return conflitos_positiva + conflitos_negativo;
}')

#________________ GERAR/TROCAR MOVIMENTOS  _____________________________________

# Função para trocar posições em um vetor de rainhas
trocar_posicoes <- function(S) {
  posicoes <- sample(1:length(S), 2) # Escolher 2 posições aleatórias para trocar
  S_copia <- S  # Copia do vetor original
  
  # Trocar as posições
  S_copia[posicoes[1]] <- S[posicoes[2]]
  S_copia[posicoes[2]] <- S[posicoes[1]]
  
  # Retorna o vetor com as posições trocadas e os valores trocados
  return(list(S_novo = S_copia, movimento = posicoes))
}

#_____________ Atualizar a lista TABU __________________________________________

atualizar_lista_tabu <- function(lista_tabu, movimento, tempo_tabu) {
  # Subtrair tempo dos movimentos tabu diretamente no vetor nomeado
  lista_tabu <- lista_tabu - 1
  
  # Remover movimentos expirados
  lista_tabu <- lista_tabu[lista_tabu > 0]
  
  # Adicionar novo movimento como vetor nomeado
  lista_tabu[paste(movimento, collapse = "-")] <- tempo_tabu
  return(lista_tabu)
}

#_________________ BUSCA TABU (Variaveis) __________________________________________________

lista_tabu <- numeric()  # vetor nomeado (em vez de lista)
melhor_solucao <- vetorRainhas  # Valor da melhor solução
melhor_fitness <- conflitos_cpp(melhor_solucao) # Fitness da melhor solução
ultima_melhoria <- 0  # Contador para as iterações 

#_________________ Medir o Tempo _______________________________________________
tic("Tempo gasto: ")

#__________________ LOOP PRINCIPAL _____________________________________________

for (iteracao in 1:MAX_ITERACAO) {
  
  # Gera um novo vizinho para cada iteração (múltiplos vizinhos)
  for (i in 1:VIZINHOS_POR_ITERACAO) {
    movimento <- trocar_posicoes(melhor_solucao)  # Chama trocar posição
    fitness_vizinhos[i] <- conflitos_cpp(movimento$S_novo)  # Calcula os conflitos
    melhores_vizinhos[[i]] <- movimento
  }
  
  # Escolher/pegar o melhor vizinho
  indice_melhor_vizinho <- which.min(fitness_vizinhos)
  melhor_vizinho <- melhores_vizinhos[[indice_melhor_vizinho]]
  fitness_novo <- fitness_vizinhos[indice_melhor_vizinho]
  
  # Verificação do Movimento Tabu
  movimento_proibido <- !is.null(lista_tabu[paste(melhor_vizinho$movimento, collapse = "-")])
  
  # Critério de Aspiração/ permitir fazer a troca 
  if (fitness_novo < melhor_fitness || (movimento_proibido && fitness_novo < melhor_fitness - MELHORIA_SIGNIFICATIVA)) {
    melhor_solucao <- melhor_vizinho$S_novo
    melhor_fitness <- fitness_novo
    ultima_melhoria <- iteracao
    lista_tabu <- atualizar_lista_tabu(lista_tabu, melhor_vizinho$movimento, TABU_PARADO)
  }
  
  # Critério de Parada
  if (melhor_fitness == 0) {
    cat("Solução ótima encontrada na iteração:", iteracao, "\n")
    break
  }
  
  # Reinicialização para Diversificação
  if (iteracao - ultima_melhoria > REINICIO_LIMITE) {
    melhor_solucao <- sample(1:RAINHAS, RAINHAS, replace = FALSE)  # Reinicia o vetor com rainhas aleatórias
    melhor_fitness <- conflitos_cpp(melhor_solucao)  # Recalcula o fitness
    lista_tabu <- numeric()  # Limpa a lista tabu
    ultima_melhoria <- iteracao  # Atualiza
  }
}

#-------------------- RESULTADOS -----------------------------------------------
toc()  # Para o tempo
cat("Melhor solução encontrada:", melhor_solucao, "\n")
cat("Fitness da melhor solução:", melhor_fitness, "\n")
