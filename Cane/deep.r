library(caTools)
library(h2o)
library(caret)
h2o.init(nthreads = -1)

base = read.csv("cane.csv")

base$X = NULL


#nconsistentes
base$block = factor(base$block, levels = unique(base$block), labels = c(1,2, 3, 4))

#escalonamento
base[,1:4] = scale(base[, 1:4])

base$n = as.numeric(base$n)
base$r = as.numeric(base$r)
base$x = as.numeric(base$x)
base$var = as.numeric(base$var)
base$block = as.numeric(base$block)


set.seed(1)
divisao = sample.split(base$block, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)




classificador = h2o.deeplearning(y = 'block',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)


#Pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-5]))

#sem pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-6]))


previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)

predicted = previsoes
#Pre-processamento
reference = base_teste[, 5]

#sem pre-processamento
reference = base_teste[, 6]

u = union(reference, predicted)

matriz_confusao = table(c(predicted, u), c(reference, u))

confusionMatrix(matriz_confusao)


table(base_teste$block)
