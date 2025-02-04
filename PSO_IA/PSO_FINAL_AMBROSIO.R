library(ggplot2)

# Parâmetros do PSO
Maxiter <- 10000
QtdParticulas <- 600
LimiteP <- 5.12
LimiteN <- -5.12
dimensoes <- 5

inercia <- 0.9    #velocidade
cognitiva <- 1.0  #melhor por particula
social <- 1.0     #melhor todas particulas

# /// Função Rastrigin
fitnessInicial <- function(x) {
  10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
 # n <- length(x)
 # sum((1 - x[1:(n-1)])^2 + 100 * (x[2:n] - x[1:(n-1)]^2)^2)
}

# /// Penalização
penalizarFitness <- function(x) {
  penalidade <- 100
  g <- sin(2 * pi * x) + 0.5
  h <- cos(2 * pi * x) + 0.5
  sum(pmax(g, 0)) + sum(abs(h)) * penalidade
}

# /// Função Objetiva
funcaoObjetiva <- function(x) {
  fitnessInicial(x) + penalizarFitness(x)
}

# /// PSO
funcaoPSO <- function(QtdParticulas, Dimensoes, Max, Min, MaxIter, inercia, cognitiva, social) {
  posicao <- matrix(runif(QtdParticulas * Dimensoes, Min, Max), QtdParticulas, Dimensoes) #matrix posição aleatoria
  velocidade <- matrix(runif(QtdParticulas * Dimensoes, -abs(Max - Min), abs(Max - Min)), QtdParticulas, Dimensoes) #velocidade inicial
  
  pbest <- posicao # Pbest inicial
  fitnessPbest <- apply(posicao, 1, funcaoObjetiva) # pega melhor posição
  gbest <- pbest[which.min(fitnessPbest), ] # melhor posição global
  fitnessGbest <- min(fitnessPbest) # melhor fitnes global
  
  historicoGbest <- numeric(MaxIter)
  historicoMedia <- numeric(MaxIter)
  historicoDesvio <- numeric(MaxIter)
      
      for (i in 1:MaxIter) {
        r1 <- matrix(runif(QtdParticulas * Dimensoes), QtdParticulas, Dimensoes) #aleatorio (0 e 1)
        r2 <- matrix(runif(QtdParticulas * Dimensoes), QtdParticulas, Dimensoes)
        
        velocidade <- inercia * velocidade + cognitiva * r1 * (pbest - posicao) + social * r2 * (gbest - posicao) #CALCULO VELOCIDADE
        velocidade <- pmax(pmin(velocidade, abs(Max - Min)), -abs(Max - Min)) #limitar velocidade a max e min
        
        posicao <- pmax(pmin(posicao + velocidade, Max), Min) # atualiza a posicao
        fitnessAtual <- apply(posicao, 1, funcaoObjetiva) # atualiza fitnes
        
        melhora <- fitnessAtual < fitnessPbest #atualiza pbest
        pbest[melhora, ] <- posicao[melhora, ]
        fitnessPbest[melhora] <- fitnessAtual[melhora]
        
        if (min(fitnessAtual) < fitnessGbest) { # atualiza gbest
          gbest <- posicao[which.min(fitnessAtual), ]
          fitnessGbest <- min(fitnessAtual)
        }
    
    historicoGbest[i] <- fitnessGbest # melhor fitnes
    historicoMedia[i] <- mean(fitnessAtual, na.rm = TRUE) #Media
    historicoDesvio[i] <- ifelse(length(fitnessAtual) > 1, sd(fitnessAtual, na.rm = TRUE), 0) #desvio
    
    if (i %% 100 == 0) cat("Iteração:", i, "- Melhor Fitness:", fitnessGbest, "\n") # MOSTRA A CADA 100
  }
  
  list(posicao = gbest, fitness = fitnessGbest, historicoGbest, historicoMedia, historicoDesvio)  # RETORNA A MELHOR SOLUÇÃO
}

resultado <- funcaoPSO(QtdParticulas, dimensoes, LimiteP, LimiteN, Maxiter, inercia, cognitiva, social) # chamar função 


#///////////////////////// GRAFICO//////////////////////////////////////////////
dados <- data.frame(Iteracao = 1:Maxiter, MelhorFitness = resultado[[3]], MediaFitness = resultado[[4]], DesvioPadraoFitness = resultado[[5]])

ggplot(dados, aes(Iteracao, MelhorFitness)) + geom_line(color = "blue") + ggtitle("Melhor Fitness por Geração") + theme_minimal()
ggplot(dados, aes(Iteracao, MediaFitness)) + geom_line(color = "red") + ggtitle("Média da Fitness por Geração") + theme_minimal()
ggplot(dados, aes(Iteracao, DesvioPadraoFitness)) + geom_line(color = "green") + ggtitle("Desvio Padrão da Fitness por Geração") + theme_minimal()

cat("Melhor solução encontrada:\n", "[", resultado$posicao, "]", "\n")
cat("Valor da fitness:\n", resultado$fitness, "\n")
