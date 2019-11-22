library(e1071)
library(caret)
library(caTools)

base = read.csv("cane.csv")

base$X = NULL

#nconsistentes
base$block = factor(base$block, levels = unique(base$block), labels = c(1,2, 3, 4))

#escalonamento
base[,1:4] = scale(base[, 1:4])

base$block = as.factor(base$block)

set.seed(1)
divisao = sample.split(base$block, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)



#pre-processamento
classificador = naiveBayes(x = base_treinamento[-5], y = base_treinamento$block)
previsoes = predict(classificador, newdata = base_teste[-5])
matriz_confusao = table(base_teste[, 5], previsoes)
confusionMatrix(matriz_confusao)

#sem pre-processamento
classificador = naiveBayes(x = base_treinamento[-6], y = base_treinamento$block)
previsoes = predict(classificador, newdata = base_teste[-6])
matriz_confusao = table(base_teste[, 6], previsoes)
confusionMatrix(matriz_confusao)

