require(foreign)
require(Matching)
washington = read.dta('~/Downloads/basic.dta')
summary(washington)
set.seed(123)

washington <- washington[!(is.na(washington$anygirls)) & !(is.na(washington$nowtot)) & !(washington$party == 3) ,]
attach(washington)

Y <- nowtot
Tr <- anygirls

p_dem <- party == 1
r_prot <- rgroup == 1
r_none <- rgroup == 0
r_cath <- rgroup == 2
r_christ <- rgroup == 3
r_other <- rgroup == 4
female <- female == 1
white <- white == 1


# GenMatch 1

X <- cbind("democrat" = p_dem, "protestant" = r_prot, "nonreligious" = r_none, "catholic" = r_cath, "Other christian" = r_christ, "Other religion" = r_other, "White" = white, "Female" = female, "Democartic Vote Share" = demvote, "Length of Service" = srvlng, "Age" = age)
BalanceMat <- X

gmout <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, estimand="ATT", pop.size=1000, max.generations = 500, wait.generations = 15)
match <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gmout)
summary(match)
mb <- MatchBalance(Tr ~ p_dem + r_prot + r_none + r_cath + r_christ + r_other + white + female + demvote + srvlng + age, match.out=match, nboots=1000)

# GenMatch 2
X <- cbind("democrat" = p_dem, "protestant" = r_prot, "nonreligious" = r_none, "catholic" = r_cath, "Other christian" = r_christ, "Other religion" = r_other, "White" = white, "Female" = female, "Democartic Vote Share" = demvote, "Length of Service" = srvlng, "Age" = age, "Age^2" = I(age^2), "Length of service^2" = I(srvlng^2), I(p_dem*demvote))
BalanceMat <- cbind("democrat" = p_dem, "protestant" = r_prot, "nonreligious" = r_none, "catholic" = r_cath, "Other christian" = r_christ, "Other religion" = r_other, "White" = white, "Female" = female, "Democartic Vote Share" = demvote, "Length of Service" = srvlng, "Age" = age)
gmout2 <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, estimand="ATT", pop.size=1000, max.generations = 500, wait.generations = 15)
match2 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gmout2)
summary(match2)
mb <- MatchBalance(Tr ~ p_dem + r_prot + r_none + r_cath + r_christ + r_other + white + female + demvote + srvlng + age, match.out=match2, nboots=1000)

# GenMatch 3 (final)

gmout3 <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, estimand="ATT", pop.size=1000, max.generations = 500, wait.generations = 15, caliper = c(2,.5,2,2,2,.2,1,4,.4,.4,.3,.3,1,.6))
match3 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gmout3,caliper = c(2,.5,2,2,2,.2,1,4,.4,.4,.3,.3,1,.6))
summary(match3)
mb3 <- MatchBalance(Tr ~ p_dem + r_prot + r_none + r_cath + r_christ + r_other + white + female + demvote + srvlng + age, match.out=match3, nboots=1000)

# Balance Graphs

par(mfrow = c(1,2))

plot(density(age[anygirls==1], bw = 2), lwd = 3, col = "red")
lines(density(age[anygirls==0], bw = 2), lwd = 2, col = "blue")

plot(density(age[match3$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(age[match3$index.control], bw = 2), lwd = 2, col = "blue")

plot(density(demvote[anygirls==1], bw = 2), lwd = 3, col = "red")
lines(density(demvote[anygirls==0], bw = 2), lwd = 2, col = "blue")

plot(density(demvote[match3$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(demvote[match3$index.control], bw = 2), lwd = 2, col = "blue")

plot(density(srvlng[anygirls==1], bw = 2), lwd = 3, col = "red")
lines(density(srvlng[anygirls==0], bw = 2), lwd = 2, col = "blue")

plot(density(srvlng[match3$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(srvlng[match3$index.control], bw = 2), lwd = 2, col = "blue")

# Sensitivity

require(rbounds)
psens(match3, Gamma = 1.6, GammaInc = 0.05)
psens(x=nowtot[Tr==1], y=nowtot[Tr==0], Gamma = 1.6, GammaInc = 0.05)

# Looking at the Data

## Numgirls vs Nowtot

par(mfrow = c(1,1))
plot(jitter(nowtot), jitter(ngirls), col = p_dem+2)

## Voting Behavior Dem vs Rep

plot(density(nowtot[p_dem==0]), col="red")
lines(density(nowtot[p_dem]), col="blue")

## Numgirls for Dem vs Rep

plot(density(ngirls[p_dem==0]), col="red")
lines(density(ngirls[p_dem]), col="blue")

## Voting Behavior Girls vs Nogirls (unmatched vs matched)

par(mfrow = c(1,2))
plot(density(nowtot[anygirls]), col="purple")
lines(density(nowtot[anygirls==0]), col="black")

plot(density(nowtot[match3$index.treated]), col="purple")
lines(density(nowtot[match3$index.control]), col="black")