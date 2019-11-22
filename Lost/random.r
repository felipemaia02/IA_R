library(caTools)
library(randomForest)
library(caret)

base = read.csv("LostLetter.csv")

base$X = NULL
base$Address = NULL

#inconsistentes
base$Location = factor(base$Location, levels = unique(base$Location), labels = c(1,2,3))

#escalonamento
base[,2:7]= scale(base[, 2:7])

base$Location = as.factor(base$Location)

set.seed(1)
divisao = sample.split(base$Location, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#Com pre-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-1], y = base_treinamento$Location, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-1], type = 'class')
  matriz_confusao = table(base_teste[, 1], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

#sem pré-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-2], y = base_treinamento$Location, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-2], type = 'class')
  matriz_confusao = table(base_teste[, 2], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

plot(c, type="l")
