# Final Project
set.seed(1)
setwd("~/Desktop/462/project/462/data")
load("split.Rdata") # Distance group should be factor
na.omit(full.data)
train = full.data[train.ind, ]
test = full.data[-train.ind, ]
Xtrain = model.matrix( ~.-1, train[, !names(train) %in% c("DEP_DEL15")])
ytrain = train$DEP_DEL15
Xtest = model.matrix( ~.-1, test[, !names(test) %in% c("DEP_DEL15")])
ytest = test$DEP_DEL15

# Exploratory Data Analysis
pairs(DEP_DEL15 ~ ., data = train, pch = 16, cex = 0.7)

# Base rate

# Logistic Regression
# Doesn't do better than guessing 0 since it's very unbalanced
log.fit = glm(DEP_DEL15 ~ ., data = train, family = 'binomial')
log.pred = predict(log.fit, newdata = test, type = 'response')
log.resp = ifelse(log.pred > 0.5, 1, 0)
log.miss = sum(log.resp != ytest) / length(ytest)

# Lasso Regression
# First do variable selection using Lasso Regression to get a simpler model
library(glmnet)
lasso.fit = cv.glmnet(Xtrain, ytrain, family = 'binomial', alpha = 1)
lasso.pred = predict(lasso.fit, newx = Xtest,
               lambda = lasso.fit$lambda.1se, type = "response")
lasso.resp = ifelse(lasso.pred > 0.5, 1, 0) 
lasso.miss = sum(lasso.resp != ytest) / length(ytest)
coef(lasso.fit)

# ROC Curve
library(pROC)
log.roc = roc(ytest, log.resp)
lasso.roc = roc(ytest, lasso.resp)
plot(log.roc, col = "black")
plot(lasso.roc, col = "red", add = TRUE)

