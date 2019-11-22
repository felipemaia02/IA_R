library(caTools)
library(randomForest)
library(caret)

#pegando a base de dados
base = read.csv("HI.csv")

#Tirando valores, apenas utilizar se for fazer pré-processamento
base$X = NULL
base$hhi = NULL
base$whi = NULL
base$hhi2 = NULL
base$wght = NULL
base$education = NULL
base$region = NULL

#inconsistentes
base$whrswk = ifelse(base$whrswk <= 0, mean(base$whrswk), base$whrswk)
base$experience = ifelse(base$whrswk <= 0, mean(base$whrswk), base$whrswk)

#escalonamento
base[,1] = scale(base[,1])
base[,4:7] = scale(base[,4:7])
base$race = factor(base$race, levels = unique(base$race), labels = c(1,2,3))
base$hispanic = factor(base$hispanic, levels = unique(base$hispanic), labels = c(1,2))

#Fazendo as divisões das bases
set.seed(1)
divisao = sample.split(base$race, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#Com pre-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-2], y = base_treinamento$race, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-2], type = 'class')
  matriz_confusao = table(base_teste[, 2], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

#Sem Pre-processamento
c = rep(0,30)
for(i in 1:30){
  classificador = randomForest(x = base_treinamento[-7], y = base_treinamento$race, ntree = i)
  previsoes = predict(classificador, newdata = base_teste[-7], type = 'class')
  matriz_confusao = table(base_teste[, 7], previsoes)
  print(matriz_confusao)
  
  cm = confusionMatrix(matriz_confusao)
  
  print(confusionMatrix(matriz_confusao))
  
  
  c[i] = cm$overall['Accuracy']
  
}

plot(c, type="l")
