library(caTools)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(e1071)

base = read.csv("NCbirths.csv")

base$X = NULL
base$ID = NULL
base$Premie = NULL
base$MomRace = NULL
base$Low = NULL
base$BirthWeightOz = NULL
base$HispMom = NULL
base$Smoke = NULL


base$Plural= as.factor(base$Plural)

#faltantes
base$Weeks = ifelse(is.na(base$Weeks), mean(base$Week, na.rm = TRUE), base$Week)
base$Gained = ifelse(is.na(base$Gained), mean(base$Gained, na.rm = TRUE), base$Gained)


#escalonamento
base[,2:8] = scale(base[, 2:8])




set.seed(1)
divisao = sample.split(base$Plural, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)


#sem pre-processamento
classificador = rpart(formula = Plural ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-3], type = 'class')
matriz_confusao = table(base_teste[, 3], previsoes)

#com pre-processamento
classificador = rpart(formula = Plural ~., data = base_treinamento)
previsoes = predict(classificador, newdata = base_teste[-1], type = 'class')
matriz_confusao = table(base_teste[, 1], previsoes)


print(classificador)
plot(classificador)
rpart.plot(classificador)


confusionMatrix(matriz_confusao)
