install.packages("caret")
library("caret")

setwd("Bases de Dados Usadas nas Aulas Práticas")
load("wage.RData")

View(wage)

dat <- wage

indexes <- sample(1:nrow(dat), as.integer(nrow(dat) * 0.80))

train <- dat[indexes,]
test <- dat[-indexes,]

cols <- c("husage", "husearns", "huseduc", "hushrs", "earns", "age", "educ", "hrwage")

preProc <- preProcess(train[,cols], method=c("center", "scale"))

train[,cols] <- predict(preProc, train[,cols])
test[,cols] <- predict(preProc, test[,cols])

summary(train)
summary(test)

cols_default <- c('husage', 'husearns', 'huseduc', 'hushrs', 
             'earns', 'age', 'educ', 'hrwage','husblck',
             'hushisp', 'kidge6', 'black', 'hispanic',
             'union', 'kidlt6')

dummies <- dummyVars(hrwage ~ husage + husearns + huseduc + hushrs +
                       earns + age + educ + husblck + hushisp +
                       kidge6 + black + hispanic + union + kidlt6, data=dat[,cols_default])

dummies_train <- predict(dummies, newdata=train[,cols_default])
dummies_test <- predict(dummies, newdata=test[,cols_default])

train_x <- as.matrix(dummies_train)
train_y <- train$hrwage

test_x <- as.matrix(dummies_test)
test_y <- test$hrwage

trainCt <- trainControl(
    method="repeatedcv",
    number=10,
    repeats=5,
    search="random"
)

elastic_reg <- train(
    hrwage ~ husage + husearns + huseduc + hushrs +
    earns + age + educ + husblck + hushisp +
    kidge6 + black + hispanic + union + kidlt6,
    data=train,
    method="glmnet",
    trControl=trainCt,
    tuneLenght=10
)

predictions_train <- predict(elastic_reg, train_x)

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

eval_results(train_y, predictions_train, train)

predictions_test <- predict(elastic_reg, test_x)

eval_results(test_y, predictions_test, test)
