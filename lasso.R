install.packages("caret")
install.packages("glmnet")
library("caret")
library("glmnet")
load("D:/Documentos/Inteligência Artificial Aplicada/Estatística 2/Bases de Dados Usadas nas Aulas Práticas/wage.RData")
View(wage)

dat <- wage

# Criar nossas amostras
indexes <- sample(1:nrow(dat), size=nrow(dat)*0.80)

train <- dat[indexes,]
test <- dat[-indexes,]

dim(train)
dim(test)

# Padronizar os valores de treino e de teste
columns_to_default = c("husage", "husearns", "huseduc", "hushrs", "earns", "age", "educ", "hrwage")

model_default <- preProcess(x=train[,columns_to_default], method=c("center", "scale"))
train[,columns_to_default] <- predict(model_default, train[,columns_to_default])
test[,columns_to_default] <- predict(model_default, test[,columns_to_default])

summary(train)
summary(test)

cols_reg = c('husage', 'husearns', 'huseduc', 'hushrs', 
             'earns', 'age', 'educ', 'hrwage','husblck',
             'hushisp', 'kidge6', 'black', 'hispanic',
             'union', 'kidlt6')

dummies <- dummyVars(hrwage~husage+husearns+huseduc+hushrs+
                       earns+age+educ+husblck+hushisp+
                       kidge6+black+hispanic+union+kidlt6, 
                     data = dat[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])
test_dummies = predict(dummies, newdata = test[,cols_reg])

x_train <- as.matrix(train_dummies)
y_train <- train$hrwage

x_test <- as.matrix(test_dummies)
y_test <- test$hrwage

lambdas <- 10^seq(3, -2, by=-0.1)

lambda <- cv.glmnet(
    x=x_train,
    y=y_train,
    alpha=1,
    lambda=lambdas,
    standardize=TRUE
)

best_lambda <- lambda$lambda.min

# Estimando o modelo
lasso_model <- glmnet(
    x_train,
    y_train,
    lambda=best_lambda,
    standardize = TRUE,
    alpha=1
)

# Os betas com . foram levados a 0, ou seja, não possui a menor importância para definir o valor do salario.
lasso_model[["beta"]]

# Predizendo o salario padronizado das esposas
lasso_predict <- predict(lasso_model, newx=x_train)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # As metricas de performace do modelo:
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

eval_results(y_train, lasso_predict, train)

lasso_predict_test <- predict(lasso_model, newx=x_test)

eval_results(y_test, lasso_predict_test, test)
