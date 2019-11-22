library(caTools)
library(h2o)
library(caret)
h2o.init(nthreads = -1)

base = read.csv("NCbirths.csv")



base$X = NULL
base$ID = NULL
base$Premie = NULL

#necessario retirar
base$MomRace = NULL

base$Low = NULL
base$BirthWeightOz = NULL
base$HispMom = NULL

#necessario retirar
base$Smoke = NULL


#fatoramento
base$Plural= as.factor(base$Plural)

#faltantes
base$Weeks = ifelse(is.na(base$Weeks), mean(base$Week, na.rm = TRUE), base$Week)
base$Gained = ifelse(is.na(base$Gained), mean(base$Gained, na.rm = TRUE), base$Gained)


#escalonamento
base[,2:8] = scale(base[, 2:8])


base$Plural =as.numeric(base$Plural)

set.seed(1)
divisao = sample.split(base$Plural, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)


classificador = h2o.deeplearning(y = 'Plural',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-1]))
#sem pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-3]))

previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)

predicted = previsoes
reference = base_teste[, 1]
#sem pre-processamento
reference = base_teste[, 3]

u = union(reference, predicted)

matriz_confusao <- table(factor(predicted, u), factor(reference, u))

confusionMatrix(matriz_confusao)
