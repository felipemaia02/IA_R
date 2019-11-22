library(caTools)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)

base = read.csv("Titanic.csv")

base$X = NULL
base$SexCode = NULL
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

#sem pre-processamento
classificador = rpart(formula = Sex ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-5])
previsoes = predict(classificador, newdata = base_teste[-5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsoes)

#com pre-processamento
classificador = rpart(formula = Sex ~., data = base_treinamento,control =rpart.control(minsplit = 15, minbucket = 15, cp=0))
previsoes = predict(classificador, newdata = base_teste[-3])
previsoes = predict(classificador, newdata = base_teste[-3], type = 'class')
matriz_confusao = table(base_teste[, 3], previsoes)


print(classificador)
plot(classificador)
rpart.plot(classificador)


confusionMatrix(matriz_confusao)
