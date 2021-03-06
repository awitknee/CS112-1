require(foreign)

load('~/Downloads/nonimp.RData')
data <- x

##Center variables (done as according to the author).

salienzmean.c <- data$salienzmean - 3.84568
rvar.c <- data$rvar - 21.75423

##Load migration data.

mig.data <- read.csv("~/Downloads/migration.csv")

##Label migration data.

colnames(mig.data) <- c("year","migration","at","be","dk","fi","fr","deo","dew","gr","it","nl","no","pt","es","se","lu","sortcountry")

##Merge migration data with Arzheimer dataset by year and country.

new.data <- merge(data, mig.data, by=c("year","at","be","dk","fi","fr","deo","dew","gr","it","nl","no","pt","es","se","lu","sortcountry"))

##Load foreign population data.

fp.data <- read.csv("~/Downloads/foreign population.csv")

##Merge foreign population data with Arzheimer dataset by year and country.

new.data <- merge(new.data, fp.data, by=c("year", "sortcountry"))

##Create country dummies for merged dataset.

new.data$country <- factor(new.data$sortcountry, labels=c("AT", "BE","DE-E","DE-W","DK","ES","FI","FR","IT","LU","NL","NO","PT","SE"))

### Added check to verify that this works properly
sum(new.data$country[new.data$it==1]!="IT")==0
sum(new.data$country[new.data$deo==1]!="DE-E")==0


## Replicate Regression


## We will test the model used in the paper by using a testset validation approach

set.seed(1)

train <- sample(1:nrow(new.data), nrow(new.data)*.8)
data.test <- new.data[-train,]

glm.fit <- glm(rexvote ~ male + age1 + age2 + age4 + mye1 + mye2 + farmerown + worker + retired + unemployed + zlrs + euschlecht + zsatisdmo + disp + lfed1 + migration + sur + migration:sur + replacementrate + sur:replacementrate + migration:replacementrate + rmax + salienzmean + rvar + rvar:salienzmean + country, family=binomial, data=new.data[train,])

summary(glm.fit)
coef(glm.fit)

## Test Model (we have to omit "LU" as it isn't included in the training set)
data.test <- na.omit(data.test)

glm.probs <- predict(glm.fit, type="response", newdata = data.test[data.test$country!="LU",])
glm.pred <- rep(0,nrow(data.test[data.test$country!="LU",]))
glm.pred[glm.probs >.1] <- 1

table(glm.pred, data.test$rexvote[data.test$country!="LU"])
mean(glm.pred[data.test$rexvote==0 || data.test$rexvote==1] == na.omit(data.test$rexvote[data.test$country!="LU"]))


## Refit with all data
glm.fit <- glm(rexvote ~ male + age1 + age2 + age4 + mye1 + mye2 + farmerown + worker + retired + unemployed + zlrs + euschlecht + zsatisdmo + disp + lfed1 + migration + sur + migration:sur + replacementrate + sur:replacementrate + migration:replacementrate + rmax + salienzmean + rvar + rvar:salienzmean + country, family=binomial, data=new.data)


## Look at Interaction effect between Immigration and Unemployemnt


library(interplot)
par(mfrow=c(1,2))
interplot(m = glm.fit, var1 = "sur", var2 = "migration", hist = TRUE) +
  # Add labels for X and Y axes
  xlab("Migration Rate") +
  ylab("Unemployment Coef") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("Estimated Coefficient of Unemployment on Extreme Right Vote by Migration") +
  theme(plot.title = element_text(face="bold")) +
  # Add a horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed") 

interplot(m = glm.fit, var1 = "migration", var2 = "sur", hist = TRUE) +
  # Add labels for X and Y axes
  xlab("Unemployment") +
  ylab("Migration Coef") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("Estimated Coefficient of Migration on Extreme Right Vote by Unemployment") +
  theme(plot.title = element_text(face="bold"))
par(mfrow=c(1,1))


## Simulation of increased Migration

##Create matrix for all variables.

mat.mig <- cbind(new.data$rexvote, new.data$male, new.data$age1, new.data$age2, new.data$age4, new.data$mye1, new.data$mye2, new.data$farmerown, new.data$worker, new.data$retired, new.data$unemployed, new.data$zlrs, new.data$euschlecht, new.data$zsatisdmo, new.data$disp, new.data$lfed1, new.data$migration, new.data$sur, new.data$replacementrate, new.data$rmax, new.data$salienzmean, new.data$rvar, new.data$at, new.data$be, new.data$deo, new.data$dew, new.data$dk, new.data$es, new.data$fi, new.data$fr, new.data$gr, new.data$it, new.data$nl, new.data$no, new.data$pt, new.data$se, new.data$country)

##Label matrix.

colnames(mat.mig) <- c("rexvote","male","age1","age2","age4","mye1","mye2","farmerown","worker","retired","unemployed","zlrs","euschlecht","zsatisdmo","disp","lfed1","migration","sur","replacementrate","rmax","salienzmean","rvar","AT", "BE","DE-E","DE-W","DK","ES","FI","FR","GR","IT","NL","NO","PT","SE","country")

##Omit missing data and create x matrix.

mat.mig2 <- na.omit(mat.mig)
mat.mig3 <- mat.mig2[,-1]

##Create means for all variables.

means.mig <- c(0,1,0,0,1,0,0,0,0,1,-0.2823178,0.1254217,-0.09784655,4.614493,2.663593,0,8.295773,36.87453,5.586569,8.7076,16.40313,0)
names(means.mig) <- c("male","age1","age2","age4","mye1","mye2","farmerown","worker","retired","unemployed","zlrs","euschlecht","zsatisdmo","disp","lfed1","migration","sur","replacementrate","rmax","salienzmean","rvar","country")
means.mig$male = "nein"
means.mig$country = "IT"

##Simulate change in prob of ER vote due to change in net migration rate with all other variables held at mean.

mig <- seq(from = -4, to=16.3, by=.1)
ests.mig <- matrix(data=NA, ncol=length(mig) ,nrow=1)

for(j in 1:length(mig)){
  data.mig <- means.mig
  data.mig$migration <- mig[j] 
  ests.mig[j] <- predict(glm.fit, type="response", newdata = data.mig)
}

plot(NA,NA, ylim= c(0.0,.25),xlim=c(-4,16.5), xlab="Net Migration Rate", ylab="Probability of Voting for the ER", main="The Effect of Net Migration on Voting for the ER", cex.main=1)
lines(mig, ests.mig)
abline(h=0)


## Classification Tree


library(tree)
set.seed(2)

new.data2 <- na.omit(new.data)
rexfactor <- factor(new.data2$rexvote)

train <- sample(1:nrow(new.data2), nrow(new.data2)*.5)


tree.er <- tree(rexfactor ~ male + age1 + age2 + age4 + mye1 + mye2 + farmerown + worker + retired + unemployed + zlrs + euschlecht + zsatisdmo + disp + lfed1 + migration + sur + replacementrate + rmax + salienzmean + rvar + country,new.data2, subset = train, split = "gini")

plot(tree.er)
text(tree.er,pretty=0)

tree.pred = predict(tree.er, new.data2[-train,], type = "class")

table(tree.pred, rexfactor[-train])
mean(tree.pred == rexfactor[-train])

cv.er <- cv.tree(tree.er, FUN=prune.misclass, K = 20)

par(mfrow=c(1,2))
plot(cv.er$size,cv.er$dec,type = "b")
plot(cv.er$k,cv.er$dev,type="b")
cv.er

# Tree with 7 nodes performed best with lowest node number
prune.er=prune.misclass(tree.er,best=21)
par(mfrow=c(1,1))
plot(prune.er)
text(prune.er,pretty=0)

tree.pred=predict(prune.er,new.data2[-train,],type="class")
table(tree.pred, rexfactor[-train])
mean(tree.pred == rexfactor[-train])

## Random Forest Model


library(randomForest)
rf.er = randomForest(rexfactor ~ male + age1 + age2 + age4 + mye1 + mye2 + farmerown + worker + retired + unemployed + zlrs + euschlecht + zsatisdmo + disp + lfed1 + migration + sur + migration:sur + replacementrate + rmax + salienzmean + rvar + country, data=new.data2, subset = train, importance =TRUE)
outcome.rf = predict(rf.er ,newdata=new.data2[-train,])
table(outcome.rf, rexfactor[-train])
mean(outcome.rf == rexfactor[-train])
importance(rf.er)
varImpPlot(rf.er)

# Part 2: Effect of Higher Education

library(Matching)

set.seed(2)
new.data2 <- na.omit(new.data)
new.data3 <- new.data2
Y <- new.data3$rexvote
Tr <- new.data3$mye2
male <- new.data3$male == "ja"

## Eliminate Variables that covary with university education such as: low education, left/right, dissatisfied with EU or Democracy

X <- cbind(male, new.data3$age1, new.data3$age2, new.data3$age4, new.data3$farmerown, new.data3$worker, new.data3$retired, new.data3$unemployed, new.data3$disp, new.data3$lfed1, new.data3$migration, new.data3$sur, new.data3$replacementrate, new.data3$rmax, new.data3$salienzmean, new.data3$at, new.data3$be, new.data3$deo, new.data3$dew, new.data3$dk, new.data3$es, new.data3$fi, new.data3$fr, new.data3$gr, new.data3$it, new.data3$nl, new.data3$no, new.data3$pt, new.data3$se)
colnames(X) <- c("male","age1","age2","age4","farmerown","worker","retired","unemployed","disp","lfed1","migration","sur","replacementrate","rmax","salienzmean","AT", "BE","DE-E","DE-W","DK","ES","FI","FR","GR","IT","NL","NO","PT","SE")

BalanceMat <- X

##gmout <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, estimand="ATT", pop.size = 7, max.generations = 15, caliper = c(1,1,1,1,1,1,1,1,1,.5,.1,.1,.3,.04,.03,1,1,1,1,1,1,1,1,1,1,1,1,1,1), wait.generations = 2)
match <- Match(Y=Y, Tr=Tr, X=X, caliper = c(1,1,1,1,1,1,1,1,1,.5,.1,.1,.3,.01,.01,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
summary(match)
mb <- MatchBalance(Tr ~ X, match.out=match, nboots=100)

par(mfrow = c(1,2))

plot(density(new.data3$salienzmean[Tr==0]), lwd = 3, col = "red", main = "Balance: Salience before Matching")
lines(density(new.data3$salienzmean[Tr==1]), lwd = 2, col = "blue")

plot(density(new.data3$salienzmean[match$index.control]), lwd = 3, col = "red", main = "Balance: Salience after Matching")
lines(density(new.data3$salienzmean[match$index.treated]), lwd = 2, col = "blue")

plot(density(new.data3$migration[Tr==0]), lwd = 3, col = "red", main = "Balance: Migration before Matching")
lines(density(new.data3$migration[Tr==1]), lwd = 2, col = "blue")

plot(density(new.data3$migration[match$index.control]), lwd = 3, col = "red", main = "Balance: Migration after Matching")
lines(density(new.data3$migration[match$index.treated]), lwd = 2, col = "blue")
# Sensitivity Analysis on these results
require(rbounds)
psens(match, Gamma = 2, GammaInc = 0.05)