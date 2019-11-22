library(caTools)
library(randomForest)
library(caret)

base = read.csv("Titanic.csv")

base$X = NULL
base$SexCode = NULL
#é necessario tirar a base name para a random forest poder funcionar
base$Name = NULL


base$PClass = as.integer(base$PClass)
base$Age = as.integer(base$Age)



#inconsistentes
base$Sex = factor(base$Sex, levels = unique(base$Sex), labels = c(1,2))

#faltantes
base$Age = ifelse(is.na(base$Age), mean(base$Age, na.rm = TRUE), base$Age)



#escalonamento
base[,1:2] = scale(base[, 1:2])
base[,4] = scale(base[, 4])


set.seed(1)
divisao = sample.split(base$Sex, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#Com pre-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-3], y = base_treinamento$Sex, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-3], type = 'class')
  matriz_confusao = table(base_teste[, 3], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

#sem pré-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-4], y = base_treinamento$Sex, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')
  matriz_confusao = table(base_teste[, 4], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

plot(c, type="l")
