library(caTools)
library(h2o)
library(caret)
h2o.init(nthreads = -1)


base = read.csv("Titanic.csv")

base$X = NULL
base$SexCode = NULL
base$Name = NULL

base$PClass = as.integer(base$PClass)
base$Age = as.integer(base$Age)
base$Survived = as.integer(base$Survived)


#inconsistentes
base$Sex = factor(base$Sex, levels = unique(base$Sex), labels = c(1,2))

#faltantes
base$Age = ifelse(is.na(base$Age), mean(base$Age, na.rm = TRUE), base$Age)




#escalonamento
base[,1:2] = scale(base[, 1:2])
base[,4] = scale(base[, 4])

base$PClass = as.numeric(base$PClass)
base$Age = as.numeric(base$Age)
base$Survived = as.numeric(base$Survived)
base$Sex = as.numeric(base$Sex)


set.seed(1)
divisao = sample.split(base$Sex, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)



classificador = h2o.deeplearning(y = 'Sex',
                                 training_frame = as.h2o(base_treinamento),
                                 activation = 'Rectifier',
                                 hidden = c(80, 160),
                                 epochs = 1000)
#com pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-3]))

#sem pre-processamento
previsoes = h2o.predict(classificador, newdata = as.h2o(base_teste[-5]))


previsoes = (previsoes > 0.5)

previsoes = as.vector(previsoes)

predicted = previsoes
#com pre-processamento
reference = base_teste[, 3]
#sem pre-processamento
reference = base_teste[, 5]

u = union(reference, predicted)

matriz_confusao <- table(c(predicted, u), c(reference, u))


confusionMatrix(matriz_confusao)
