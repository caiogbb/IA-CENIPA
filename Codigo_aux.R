# Pacotes utilizados

library(tidyverse)
library(dplyr)
library(fdth)
library(ggplot2)
library(car)
library(purrr)
library(caTools)
library(rpart)
library(rpart.plot)
library(e1071)
library(neuralnet)
library(xgboost)
library(randomForest)
library(caret)
library(visdat)
library(multiROC)
library(pROC)
library(glmnet)
library(hnp)
require(nnet)

# Conjunto de dados carregados para o teste

ocorrencia <- read.csv('ocorrencia.csv', sep =';')
aeronave <- read.csv('aeronave.csv', sep = ';')
#ocorrencia_tipo <- read.csv('ocorrencia_tipo.csv', sep=';')
#fator_contribuinte <- read.csv('fator_contribuinte.csv', sep = ';')
#recomendacao <- read.csv('recomendacao.csv', sep = ';')

# Juntando em uma única tabela (Tabelão de dados!)

ocor_aren <- left_join(ocorrencia, aeronave, by = c('codigo_ocorrencia2' = 'codigo_ocorrencia2') )

#ocor_tipo <- left_join(ocor_aren, ocorrencia_tipo, by = c('codigo_ocorrencia1' = 'codigo_ocorrencia1') )

#ocor_contribuinte <- left_join(ocor_tipo, fator_contribuinte, by = c('codigo_ocorrencia3' = 'codigo_ocorrencia3') )

#dados <- left_join(ocor_contribuinte, recomendacao, by = c('codigo_ocorrencia4' = 'codigo_ocorrencia4') )

# Análise descritivas para o conjunto de dados
colym <- c('codigo_ocorrencia','codigo_ocorrencia1', 'codigo_ocorrencia2', 'codigo_ocorrencia3', 'codigo_ocorrencia4', 'ocorrencia_latitude', 'ocorrencia_longitude', 'aeronave_pais_fabricante', 'aeronave_pais_registro', 'ocorrencia_dia', 'ocorrencia_hora', 'divulgacao_relatorio_numero', 'aeronave_pmd', 'aeronave_pais_registro', 'aeronave_assentos', 'total_recomendacoes', 'aeronave_fabricante', 'investigacao_aeronave_liberada', 'investigacao_status', 'divulgacao_relatorio_numero', 'divulgacao_dia_publicacao', 'aeronave_assentos', 'aeronave_ano_fabricacao', 'investigacao_aeronave_liberada', 'divulgacao_relatorio_publicado', 'aeronave_voo_origem', 'aeronave_voo_destino', 'aeronave_tipo_icao', 'aeronave_fase_operacao', 'aeronave_pmd_categoria', 'aeronave_matricula', 'ocorrencia_cidade', 'ocorrencia_pais', 'ocorrencia_aerodromo', 'ocorrencia_uf', 'aeronave_modelo' )
data <- ocor_aren %>% dplyr::select(-colym)

to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

names(data)

variable <- names(data)[]
variable
data <- to.factors(data, variable)

str(data)

summary(data)

# Dados desbalanceados, utilizar técnica de undersampling

Under_Sampling <- function(df){
  
  m <- table(df)  
  
  ponto <- min(m)  
  
  valores_acidente <- which(df == 'ACIDENTE')
  valores_icidente <- which(df == 'INCIDENTE')
  valores_icidente_grave <- which(df == 'INCIDENTE GRAVE')
  
  
  under_acidente <- sample(valores_acidente, ponto,  replace = F)
  
  under_icidente <- sample(valores_icidente, ponto,  replace = F)
  
  
  pos <- c(valores_icidente_grave, under_acidente, under_icidente  )
  
  df = data[pos, ]
  
  return(df)
  
}

dados <- Under_Sampling(data$ocorrencia_classificacao)

table(dados$ocorrencia_classificacao) # checando se a ténica está correta

names(dados)


str(dados)

# SPlit dos dados 

set.seed(13)
split <- sample.split(dados$ocorrencia_classificacao,SplitRatio = 0.70)
dadostreino <- subset(dados, split == "TRUE")
dadosteste <- subset(dados, split == "FALSE")


str(dadosteste)

# Multinomial 

class.Mult <- multinom(ocorrencia_classificacao~ ., data = dadostreino)

#hnp(class.Mult)

pred.Mult = predict(class.Mult, newdata = dadosteste, "class")

#round(pred.Mult, 4)

caret::confusionMatrix(as.factor(pred.Mult), as.factor(dadosteste$ocorrencia_classificacao))

pred.Mult = predict(class.Mult, newdata = dadosteste, "probs")

multiclass.roc(dadosteste$ocorrencia_classificacao, pred.Mult)


# Arvore de Decisão

#  Tuning 

hyper_grid <- expand.grid(minsplit = seq(1, 10, 1), maxdepth = seq(1, 10, 1))
models <- list()

for (i in 1:nrow(hyper_grid)) {
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  set.seed(13)
  models[[i]] <- rpart(ocorrencia_classificacao~ ., data = dadostreino,
                       control = list(cp = 0, minsplit = minsplit, 
                                      maxdepth = maxdepth)
  )
}
# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}
# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

Er <- hyper_grid %>%
  mutate(cp = purrr::map_dbl(models, get_cp),
         error = purrr::map_dbl(models, get_min_error)) %>%
  arrange(error) 

summary(Er)



set.seed(13)
Class.Arvore <- rpart(ocorrencia_classificacao~ ., data = dadostreino,
                      control = rpart.control(minsplit = 1, cp = 0,
                                              maxdepth =8))
Class.Arvore
plotcp(Class.Arvore)


Arvore.mod.pred <- predict(Class.Arvore, dadosteste, type = "class")


caret::confusionMatrix(as.factor(Arvore.mod.pred), as.factor(dadosteste$ocorrencia_classificacao))

Arvore.mod.pred = predict(Class.Arvore, newdata = dadosteste, "prob")

multiclass.roc(dadosteste$ocorrencia_classificacao, Arvore.mod.pred)


# -------------------------------------------- #

# Random Forest

####==========================
#### Slide 28 - Random Forest

set.seed(13)
mtry <- tuneRF(dadostreino %>% dplyr::select(-ocorrencia_classificacao),
               as.factor(dadostreino$ocorrencia_classificacao), ntreeTry=100,
               stepFactor = 10, improve=0.0001)



best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)

set.seed(13)
Class.RandomForest <- 
  randomForest(dadostreino %>% dplyr::select(-ocorrencia_classificacao),
               as.factor(dadostreino$ocorrencia_classificacao),
               data = dadostreino, ntree = 100, mtry = 3, 
               importance = TRUE, proximity = TRUE)
Class.RandomForest


#ggsave(tree_func(RandomForest,1),file="slide 28.png",width=12,height=8)

Class.RandomForest$importance
#RandomForest$oob.times

varImpPlot(Class.RandomForest)

Pred.Class.RF <- predict(Class.RandomForest, dadosteste)

mat = caret::confusionMatrix(as.factor(Pred.Class.RF), as.factor(dadosteste$ocorrencia_classificacao))

xtable::xtable(mat$table)

Pred.Class.RF = predict(Class.RandomForest, dadosteste, "prob")

multiclass.roc(dadosteste$ocorrencia_classificacao, Pred.Class.RF)



# =========================================================================== #
# =========================================================================== #
# ==================== KNN model ======================================== #
# =========================================================================== #
# =========================================================================== #
set.seed(13)
Class.KNN.mod <- 
  caret::train(ocorrencia_classificacao~ ., data = dadostreino,
               method="knn", metric = "Accuracy",
               trControl = caret::trainControl("cv", number = 10),
               preProcess = c("center","scale"),tuneLength = 20)
plot(Class.KNN.mod)
Class.KNN.mod$bestTune


pred.Class.KNN = predict(Class.KNN.mod, dadosteste)

caret::confusionMatrix(as.factor(pred.Class.KNN), as.factor(dadosteste$ocorrencia_classificacao))

pred.Class.KNN = predict(Class.KNN.mod, dadosteste, "prob")

multiclass.roc(dadosteste$ocorrencia_classificacao, pred.Class.KNN)


# Metricas dos modelos


caret::confusionMatrix(as.factor(Pred.Class.RF), as.factor(dadosteste$ocorrencia_classificacao))

caret::confusionMatrix(as.factor(pred.Class.KNN), as.factor(dadosteste$ocorrencia_classificacao))

#caret::confusionMatrix(as.factor(pred.Class.svm), as.factor(dadosteste$potencial))

caret::confusionMatrix(as.factor(Arvore.mod.pred), as.factor(dadosteste$ocorrencia_classificacao))

caret::confusionMatrix(as.factor(pred.Mult), as.factor(dadosteste$ocorrencia_classificacao))


# Modelo de séries temporais 


# Pacotes utilizados

library(tidyverse)
library(dplyr)
library(fdth)
library(ggplot2)
library(forecast)
library(MLmetrics)

# Conjunto de dados carregados para o teste

ocorrencia <- read.csv('ocorrencia.csv', sep =';')

as.Date("03/08/2017", format = "%d/%m/%Y")

ocorrencia$ocorrencia_dia

names(ocorrencia)

ocorrencia$ocorrencia_dia <- as.Date(ocorrencia$ocorrencia_dia, format = "%d/%m/%Y")

filther = ocorrencia %>% filter( ocorrencia_classificacao == 'INCIDENTE'  )


data_acid <- filther$ocorrencia_dia

acid_mes_ano <- format( data_acid, "%y/%m")

tb <- table(acid_mes_ano)

ts_acid <- ts( tb, start = c(2012,1), frequency = 12)

ts.plot(ts_acid, xlab = 'Tempo (ano)', ylab='Ocorrência de acidentes',
        main = 'Série temporal de ocorrências CENIPA')

mod <- auto.arima(ts_acid ,seasonal = TRUE, approximation = FALSE)


# Modelo de Holt 

ho2 <- holt(ts_acid)

res <- mod$residuals

acf(res)

hist(res)

shapiro.test(res)





