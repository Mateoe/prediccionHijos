#library('olsrr')

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

train <- train[,-c(1)]
test <- test[,-c(1)]

#train$nivel_edu <- as.factor(train$nivel_edu)
#train$estu_actual <- as.factor(train$estu_actual)
#train$tipo_trabajo <- as.factor(train$tipo_trabajo)
#train$estado_civil <- as.factor(train$estado_civil)
#train$tipo_vivienda <- as.factor(train$tipo_vivienda)
#train$sexo <- as.factor(train$sexo)
#train$conyugue_hogar <- as.factor(train$conyugue_hogar)
#train$estado_salud <- as.factor(train$estado_salud)
#train$regimen_salud <- as.factor(train$regimen_salud)

#modelo <- lm(Hijos ~ . , data=train)

#k <- ols_step_all_possible(modelo)

#install.packages("neuralnet")
#require(neuralnet)
#
#nn = neuralnet(Placed~TKS+CSS, data = df, hidden = 3, act.fct = "logistic", linear.output = FALSE)
require(randomForest)

Modelo.rf = randomForest(medv ~ . , data = Boston , subset = train)
Boston.rf
