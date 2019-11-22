library(e1071)
library(caret)
library(caTools)

#Esse dataset não apresenta dados faltantes
data = read.csv("HI.csv")

#Tirando valores, apenas utilizar se for fazer pré-processamento
data$X = NULL
data$hhi = NULL
data$whi = NULL
data$hhi2 = NULL
data$wght = NULL
data$region = NULL
data$education = NULL

summary(data)

#inconsistentes
data$whrswk = ifelse(data$whrswk <= 0, mean(data$whrswk), data$whrswk)
data$experience = ifelse(data$whrswk <= 0, mean(data$whrswk), data$whrswk)
data$race = factor(data$race, levels = unique(data$race), labels = c(1,2,3))
data$hispanic = factor(data$hispanic, levels = unique(data$hispanic), labels = c(1,2))


#escalonamento
data[,1] = scale(data[,1])
data[,4:7] = scale(data[,4:7])

#Fazendo as divisões das bases
set.seed(1)
divisao = sample.split(data$race, SplitRatio = 0.75)
base_treinamento = subset(data, divisao == TRUE)
base_teste = subset(data, divisao == FALSE)

#Sem pré-processamento
classificador = naiveBayes(x = base_treinamento[-7], y = base_treinamento$race)
previsoes = predict(classificador, newdata = base_teste[-7])
matriz_confusao = table(base_teste[, 7], previsoes)

#Com pré-processamento
classificador = naiveBayes(x = base_treinamento[-2], y = base_treinamento$race)
previsoes = predict(classificador, newdata = base_teste[-2])
matriz_confusao = table(base_teste[, 2], previsoes)

#Gerando a matriz
confusionMatrix(matriz_confusao)



