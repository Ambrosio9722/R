# Preparação do ambiente
rm(list = ls())
cat("\014")
library(rstudioapi)
library(readr)
library(igraph)
library(ggplot2)

# Configuração do diretório de trabalho
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_path)

#------------------------------- VARIÁVEIS -------------------------------------
feromonioInicial <- 0.01
taxaEvaporacao <- 0.03 
maximoIteracao <- 500
quantidadeFormigas <- 80
quantidadeExecucoes <- 1
melhor_caminho <- ''
custo_melhor_caminho <- -Inf
BASE_CSV <- "C"

#------------------------- FUNÇÃO CSV ------------------------------------------
carregar_base <- function(base) {
  switch(base,
         "A" = {
           arquivoCSV <- read.table('exemplo_sala_novo.csv', header = TRUE)
           vinicial <- 1
           vfinal <- 12
           nomebase <- "Grafo A - 7 vértices e 11 arestas"
         },
         
         "B" = {
           arquivoCSV <- read.table('grafo1.csv', header = TRUE)
           vinicial <- 1
           vfinal <- 12
           nomebase <- "Grafo A - 12 vértices e 25 arestas"
         },
         "C" = {
           arquivoCSV <- read.table('grafo2.csv', header = TRUE)
           vinicial <- 1
           vfinal <- 20
           nomebase <- "Grafo B - 20 vértices e 190 arestas"
         },
         "D" = {
           arquivoCSV <- read.table('grafo3.csv', header = TRUE)
           vinicial <- 1
           vfinal <- 100
           nomebase <- "Grafo C - 100 vértices e 8020 arestas"
         }
  )
  return(list(arquivoCSV = arquivoCSV, vinicial = vinicial, vfinal = vfinal, nomebase = nomebase))
}

# Carregar dados
dados <- carregar_base(BASE_CSV)
arquivoCSV <- dados$arquivoCSV
vinicial <- dados$vinicial
vfinal <- dados$vfinal
nomebase <- dados$nomebase

# Separar colunas em vetores
origens <- arquivoCSV[, 1]
destinos <- arquivoCSV[, 2]
pesos <- arquivoCSV[, 3]

# Criar grafo
grafo <- graph_from_data_frame(d = data.frame(origem = origens, destino = destinos, peso = pesos), directed = FALSE)
V(grafo)$name <- unique(c(origens, destinos))
E(grafo)$weight <- pesos

# Plot do grafo
layout <- layout_with_fr(grafo)
plot(
  grafo,
  layout = layout,
  vertex.size = 20,
  vertex.label.cex = 0.8,
  vertex.label.color = "black",
  edge.width = E(grafo)$weight / max(E(grafo)$weight) * 5,
  edge.color = "gray50",
  main = nomebase
)

#----------------- Algoritmo ACO ------------------------------------------------

todoscustos <- matrix(0, nrow = quantidadeExecucoes, ncol = quantidadeFormigas) # Armazena todos os custos
custos_expcorrente <- matrix(0, nrow = maximoIteracao, ncol = quantidadeFormigas) # Gráfico

tempo_execucao <- system.time({
  for (nexp in 1:quantidadeExecucoes) {
    Feromonio <- rep(feromonioInicial, length(origens)) # vetor de feromônios
    
    for (iter in 1:maximoIteracao) {
      caminhos <- vector("list", quantidadeFormigas) # Vetor de caminhos
      custos <- numeric(quantidadeFormigas) # Vetor de custos
      
      for (n in 1:quantidadeFormigas) {
        VerticeAtual <- vinicial # Começo da formiga
        caminho <- c(VerticeAtual)
        custo <- 0
        CaminhoValido <- TRUE
        visitados <- setNames(rep(FALSE, max(destinos)), seq_len(max(destinos)))
        visitados[vinicial] <- TRUE
        
        while (VerticeAtual != vfinal && CaminhoValido) {
          # Encontrar arestas disponíveis sem visitar vértices já percorridos
          indiceVerticeAtual <- which(origens == VerticeAtual & !visitados[destinos])
          
          if (length(indiceVerticeAtual) == 0) {
            CaminhoValido <- FALSE
            break
          }
          
          #--------- Cálculo probabilidade ----------------- 
          probabilidade <- Feromonio[indiceVerticeAtual] * pesos[indiceVerticeAtual]
          if (sum(probabilidade) == 0) {
            CaminhoValido <- FALSE
            break
          }
          
          probabilidade <- probabilidade / sum(probabilidade)
          probabilidade <- cumsum(probabilidade)
          valorSorteio <- runif(1)
          indiceEscolhido <- indiceVerticeAtual[which(valorSorteio <= probabilidade)[1]]
          
          if (is.na(indiceEscolhido)) {
            CaminhoValido <- FALSE
            break
          }
          
          # Atualizar caminho, custo e marcar vértice como visitado
          VerticeAtual <- destinos[indiceEscolhido]
          caminho <- c(caminho, VerticeAtual)
          custo <- custo + pesos[indiceEscolhido]
          visitados[VerticeAtual] <- TRUE
        }
        
        if (CaminhoValido) {
          caminhos[[n]] <- caminho
          custos[n] <- custo
        } else {
          custos[n] <- -Inf
        }
      }
      
      # Atualizar feromônio
      Feromonio <- Feromonio * (1 - taxaEvaporacao)
      for (n in 1:quantidadeFormigas) {
        caminho_atual <- caminhos[[n]]
        if (!is.null(caminho_atual)) {
          for (k in 1:(length(caminho_atual) - 1)) {
            indiceOrigens <- which(origens == caminho_atual[k])
            indicadorPeso <- which(destinos[indiceOrigens] == caminho_atual[k + 1])
            Feromonio[indiceOrigens[indicadorPeso]] <- Feromonio[indiceOrigens[indicadorPeso]] + custos[n]
          }
        }
      }
      
      # Melhor caminho da iteração
      if (max(custos) > custo_melhor_caminho) {
        melhor_caminho <- caminhos[[which.max(custos)]]
        custo_melhor_caminho <- max(custos)
      }
      
      if (nexp == 1) {
        for (z in 1:quantidadeFormigas) {
          custos_expcorrente[iter, z] <- custos[z]
        }
      }
    }
    
    todoscustos[nexp, ] <- custos
  }
})

#----------------- Resultados Finais --------------------------------------------

cat("\014")
cat("Quantidade de experimentos: ", quantidadeExecucoes, "\n",
    "Melhor caminho: ", melhor_caminho, "\n",
    "Custo do melhor caminho: ", custo_melhor_caminho, "\n",
    "Custo médio: ", mean(todoscustos[todoscustos > -Inf]), "\n",
    "Tempo de execução: ", tempo_execucao["elapsed"], " segundos\n")

# Gráfico de convergência
convergencia <- data.frame(Iteracao = 1:maximoIteracao, MelhorCusto = apply(custos_expcorrente, 1, max, na.rm = TRUE))
ggplot(convergencia, aes(x = Iteracao, y = MelhorCusto)) +
  geom_line() +
  labs(title = "Convergência do ACO", x = "Iteração", y = "Custo do Melhor Caminho")
