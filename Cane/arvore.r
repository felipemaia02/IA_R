library(caTools)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)

base = read.csv("cane.csv")

base$X = NULL

#Não apresenta faltantes

#inconsistentes
base$block = factor(base$block, levels = unique(base$block), labels = c(1,2, 3, 4))

#escalonamento
base[,1:4] = scale(base[, 1:4])

set.seed(1)
divisao = sample.split(base$block, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#sem pre-processamento
classificador = rpart(formula = block ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-6], type = 'class')
matriz_confusao = table(base_teste[, 6], previsoes)

#com pre-processamento
classificador = rpart(formula = block ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsoes)


print(classificador)
plot(classificador)
rpart.plot(classificador)


confusionMatrix(matriz_confusao)
