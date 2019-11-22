library(e1071)
library(caret)
library(caTools)

base = read.csv("Titanic.csv")

base$X = NULL
base$SexCode = NULL
base$Name = NULL

base$PClass = as.integer(base$PClass)
base$Age = as.integer(base$Age)



#nconsistentes
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
classificador = naiveBayes(x = base_treinamento[-5], y = base_treinamento$Sex)
previsoes = predict(classificador, newdata = base_teste[-5])
matriz_confusao = table(base_teste[,5], previsoes)
confusionMatrix(table(base_teste[,5], previsoes))


#Com pre-processamento
classificador = naiveBayes(x = base_treinamento[-3], y = base_treinamento$Sex)
previsoes = predict(classificador, newdata = base_teste[-3])
matriz_confusao = table(base_teste[, 3], previsoes)
confusionMatrix(matriz_confusao)



