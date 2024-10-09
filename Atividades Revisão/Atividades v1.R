#Nível Iniciante

#Operações Básicas com Vetores
#Crie um vetor com 15 números inteiros aleatórios.
#Calcule a soma, média e o desvio padrão dos valores do vetor.
#Filtre os valores que são maiores que a média do vetor.

vetor1 <- c(1,2,3,4,5,6,7,8,12,34,11,12,45,65,76)
vetor2 <-c()
soma <-sum(vetor1)
media <-mean(vetor1)
desvioPadrao <- sd(vetor1)
  
paste("A soma dos valores apresentados é: " ,soma)
print(paste("A media dos valores apresentados é: ",media))
paste("O desvio padrão dos valores apresentados é: ", desvioPadrao)

for(i in 1:15){
 if(vetor1[i] > media){
      vetor2 <- c(vetor2,vetor1[i])
  }
}

vetor2 <- vetor1[media < vetor1]
 



