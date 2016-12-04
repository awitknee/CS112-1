require(foreign)
data_raw <- read.dta("~/Downloads/peace.dta")
names(data_raw)
length(data_raw$dataset)

data_raw$outcome <- factor(data_raw$pbs2s3, levels = c(0,1), labels = c("Failure", "Success"))

data <- subset(data_raw, select = c("outcome","un2int","wartype","logdead","wardur","factnum","trnsfcap","develop","decade","treaty","geo","eh","electric"))

library(Amelia)
missmap(data, main = "Missing values vs observed")

set.seed(1)


# Test and Training Set

train <- sample(1:nrow(data), 70)


# Logistic Regression Model

glm.fit <- glm(outcome ~ . , data = data[train,], family = binomial)
summary(glm.fit)
coef(glm.fit)

# Making Predictions

glm.probs <- predict(glm.fit, type="response", newdata = data)

# Assessing prediction accuracy for different classification thresholds

plot_threshold <- function(glm.probs, train) {
  plot(c(1:100/100), c(1:100/100), type = "n", xlab = "Classification Threshold", ylab = "Accuracy")
  title("Prediction Accuracy for different Classification Thresholds")
  legend("right", c("Out Of Sample", "In Sample", "Type 1 Error", "Type 2 Error"), pch = 19, col= c("red","blue","green", "orange"))
  
  for ( p in c(0:200/200)) {
    glm.pred <- rep("Failure",124)
    glm.pred[glm.probs > p ]="Success"
    points(p, mean(glm.pred[-train]==data$outcome[-train]),  col = "red", pch = 20)
    points(p, mean(glm.pred[train]==data$outcome[train]),  col = "blue", pch = 20)
    points(p, sum((glm.pred[-train] == "Failure") * (data$outcome[-train] == "Success"))/length(data$un2int), col = "green", pch = 20)
    points(p, sum((glm.pred[-train] == "Success") * (data$outcome[-train] == "Failure"))/length(data$un2int), col = "orange", pch = 20)
  }
}
plot_threshold(glm.probs, train)

library(ROCR)
pr <- prediction(glm.probs[-train], data$outcome[-train])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "red")

glm.pred <- rep("Failure",124)
glm.pred[glm.probs >.3]="Success"

# In Sample Prediction Accuracy

table(glm.pred[train], data$outcome[train])
mean(glm.pred[train]==data$outcome[train])

# Out of Sample Prediction Accuracy:

table(glm.pred[-train], data$outcome[-train])
mean(glm.pred[-train]==data$outcome[-train])

# Lets crossvalidate instead:

library(boot)
cv.error = rep(7)
for (i in 1:7){
  glm.fit <- glm(outcome ~ un2int + wartype + poly(logdead,i) + wardur + factnum + trnsfcap + develop + decade + treaty + geo + eh + electric, data = na.omit(data), family = binomial)
  cv.error[i] <- cv.glm(na.omit(data), glm.fit, K=10)$delta[1]
}
cv.error
plot(1:7,cv.error)

glm.fit <- glm(outcome ~ un2int + wartype + logdead + wardur + factnum + trnsfcap + develop + decade + treaty + geo + eh + electric, data = na.omit(data), family = binomial)
cv.fit <- cv.glm(na.omit(data), glm.fit, K=10)
cv.fit$delta[1]

# Calculate final fit with all data points
glm_final.fit <- glm(outcome ~ ., data = data, family = binomial)
summary(glm_final.fit)
glm_final.fit$coefficients
plot_threshold(predict(glm.fit, type="response", newdata = data),train)


# Let's grow some trees:

library(tree)

tree.peace <- tree(outcome~.,data)
summary(tree.peace)
plot(tree.peace)
text(tree.peace,pretty=0)

set.seed(2)
peace.test <- data[-train,]

tree.peace <- tree(outcome~.,data, subset = train)
tree.pred = predict(tree.peace, peace.test, type = "class")

table(tree.pred, peace.test$outcome)

cv.peace <- cv.tree(tree.peace, FUN= prune.misclass)

par(mfrow=c(1,2))
plot(cv.peace$size,cv.peace$dec,type = "b")
plot(cv.peace$k,cv.peace$dev,type="b")
cv.peace

# Tree with 7 nodes performed best with lowest node number
prune.peace=prune.misclass(tree.peace,best=7)
par(mfrow=c(1,1))
plot(prune.peace)
text(prune.peace,pretty=0)

tree.pred=predict(prune.peace,peace.test,type="class")
table(tree.pred, peace.test$outcome)
mean(tree.pred == peace.test$outcome)

# Let's grow a random forest

library(randomForest)
rf.peace = randomForest(outcome~., data=na.omit(data[train,]), importance =TRUE)
outcome.rf = predict(rf.peace ,newdata=data[-train ,])
table(outcome.rf, peace.test$outcome)
40/52
importance(rf.peace)
varImpPlot(rf.peace)