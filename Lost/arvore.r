library(caTools)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)


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


#sem pre-processamento
classificador = rpart(formula = Location ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-2], type = 'class')
matriz_confusao = table(base_teste[, 2], previsoes)

#com pre-processamento
classificador = rpart(formula = Location ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-1], type = 'class')
matriz_confusao = table(base_teste[, 1], previsoes)


print(classificador)
plot(classificador)
rpart.plot(classificador)


confusionMatrix(matriz_confusao)
