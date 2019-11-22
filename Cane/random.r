library(caTools)
library(randomForest)
library(caret)


base = read.csv("cane.csv")

base$X = NULL

#nconsistentes
base$block = factor(base$block, levels = unique(base$block), labels = c(1,2, 3, 4))

#escalonamento
base[,1:4] = scale(base[, 1:4])

set.seed(1)
divisao = sample.split(base$block, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#Com pre-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-5], y = base_treinamento$block, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
  matriz_confusao = table(base_teste[, 5], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

#sem pré-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-6], y = base_treinamento$block, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-6], type = 'class')
  matriz_confusao = table(base_teste[, 6], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

plot(c, type="l")
