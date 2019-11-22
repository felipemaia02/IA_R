library(e1071)
library(caret)
library(caTools)

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

#Com pré-processamento
classificador = naiveBayes(x = base_treinamento[-1], y = base_treinamento$Plural)
previsoes = predict(classificador, newdata = base_teste[-1])
matriz_confusao = table(base_teste[, 1], previsoes)
confusionMatrix(matriz_confusao)

#sem pre-processamento
classificador = naiveBayes(x = base_treinamento[-3], y = base_treinamento$Plural)
previsoes = predict(classificador, newdata = base_teste[-3])
matriz_confusao = table(base_teste[, 3], previsoes)
confusionMatrix(matriz_confusao)
