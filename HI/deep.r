library(caTools)
library(h2o)
library(caret)
h2o.init(nthreads = -1)


#pegando a base de dados
base = read.csv("HI.csv")

#Tirando valores, apenas utilizar se for fazer pré-processamento
base$X = NULL
base$hhi = NULL
base$whi = NULL
base$hhi2 = NULL
base$wght = NULL
base$education = NULL
base$region = NULL


#inconsistentes
base$whrswk = ifelse(base$whrswk <= 0, mean(base$whrswk), base$whrswk)
base$experience = ifelse(base$whrswk <= 0, mean(base$whrswk), base$whrswk)

#fatoramento
base$race = factor(base$race, levels = unique(base$race), labels = c(1,2,3))
base$hispanic = factor(base$hispanic, levels = unique(base$hispanic), labels = c(1,2))


#escalonamento
base[,1] = scale(base[,1])
base[,4:7] = scale(base[,4:7])

base$whrswk= as.numeric(base$whrswk)
base$race = as.numeric(base$race)
base$hispanic = as.numeric(base$hispanic)
base$experience = as.numeric(base$experience)
base$kidslt6 = as.numeric(base$kidslt6)
base$kids618 = as.numeric(base$kids618)
base$husby = as.numeric(base$husby)

#Fazendo as divisões das bases
set.seed(1)
divisao = sample.split(base$race, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)


summary(base)

classificador = h2o.deeplearning( y = 'race',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)

#com pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-2]))

#sem pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-7]))


previsoes = (previsoes > 0.5)
previsoes = as.vector(previsoes)

predicted = previsoes
#com pre-processamento
reference = base_teste[, 2]
#sem pre-processamento
reference = base_teste[, 7]

u = union(reference, predicted)

matriz_confusao <- table(factor(predicted, u), factor(reference, u))



confusionMatrix(matriz_confusao)


