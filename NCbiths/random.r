library(caTools)
library(randomForest)
library(caret)


base = read.csv("NCbirths.csv")


base$X = NULL
base$ID = NULL
base$Premie = NULL

#necessario retirar
base$MomRace = NULL

base$Low = NULL
base$BirthWeightOz = NULL
base$HispMom = NULL

#necessario retirar
base$Smoke = NULL



base$Plural= as.factor(base$Plural)

summary(base)


#faltantes
base$Weeks = ifelse(is.na(base$Weeks), mean(base$Week, na.rm = TRUE), base$Week)
base$Gained = ifelse(is.na(base$Gained), mean(base$Gained, na.rm = TRUE), base$Gained)


#escalonamento
base[,2:8] = scale(base[, 2:8])


set.seed(1)
divisao = sample.split(base$Plural, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#Com pre-processamento

c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-1], y = base_treinamento$Plural, ntree = i)
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
  classificador = randomForest(x = base_treinamento[-3], y = base_treinamento$Plural, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-3], type = 'class')
  matriz_confusao = table(base_teste[, 3], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

plot(c, type="l")
