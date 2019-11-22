library(caTools)
library(h2o)
library(caret)
h2o.init(nthreads = -1)

base = read.csv("LostLetter.csv")

base$X = NULL
base$Address = NULL

#inconsistentes
base$Location = factor(base$Location, levels = unique(base$Location), labels = c(1,2,3))

#escalonamento
base[,2:7]= scale(base[, 2:7])

base$Location = as.factor(base$Location)
base$Location = as.numeric(base$Location)

set.seed(1)
divisao = sample.split(base$Location, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = h2o.deeplearning(y = 'Location',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)


#Pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-1]))

#sem pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-2]))


previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)

predicted = previsoes
#Pre-processamento
reference = base_teste[, 1]
#sem pre-processamento
reference = base_teste[, 2]

u = union(reference, predicted)

matriz_confusao = table(c(predicted, u), c(reference, u))

confusionMatrix(matriz_confusao)
