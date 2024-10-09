#Manipulação de Data Frames

#Crie um data frame que contenha as seguintes colunas: Nome, Idade, Altura, e Peso para cinco pessoas.
#Calcule a média da idade e do peso das pessoas.
#Adicione uma nova coluna chamada IMC que calcula o Índice de Massa Corporal de cada pessoa 
#(IMC = Peso/Altura²).


Nome <- c("Antonio","Pedro","Lucas","Ana", "Bia")
Idade <- c(20,12,30,12,18)
Altura <- c(1.83,1.70,1.78,1.66,1.59)
Peso <- c(80,75,84,45,52)
IMC <-c()
data.frame(Nome,Idade,Altura,Peso)

dataFrame <- data.frame(Nome,Idade,Altura,Peso)
mediaIdade <- c()

#medias
mediaIdade <- c(mean(Idade))
mediaPeso <- c(mean(Peso))

IMC <- c(Peso/Altura^2)

dataFrame <- data.frame(dataFrame,IMC)
#ou
dataFrame$IMC <- Peso / (Altura^2)
