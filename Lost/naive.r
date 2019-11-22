library(e1071)
library(caret)
library(caTools)

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

#pre-processamento
classificador = naiveBayes(x = base_treinamento[-1], y = base_treinamento$Location)
previsoes = predict(classificador, newdata = base_teste[-1])
matriz_confusao = table(base_teste[, 1], previsoes)
confusionMatrix(matriz_confusao)

#sem pre-processamento
classificador = naiveBayes(x = base_treinamento[-2], y = base_treinamento$Location)
previsoes = predict(classificador, newdata = base_teste[-2])
matriz_confusao = table(base_teste[, 2], previsoes)
confusionMatrix(matriz_confusao)
