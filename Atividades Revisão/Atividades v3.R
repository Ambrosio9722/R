#Estruturas de Controle

# Escreva um script que pergunte ao usuário um número e, usando uma estrutura if-else, informe se o número é 
# positivo, negativo ou zero.
# Use um loop for para imprimir os números de 1 a 10.

numero <- as.numeric(readline(prompt = "Informe um número:"))

if(numero == 0){
  print(paste("Zero"))
} else if(numero > 0){
  print (paste("Positivo"))
}else{
  print(paste("Negativo"))
}

for (i in 1:10){
  print(i)
}
