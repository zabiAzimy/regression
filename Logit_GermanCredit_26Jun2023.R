library(evtree)
data("GermanCredit")
str(GermanCredit)
table(GermanCredit$credit_risk)

Y <- 1- as.numeric(GermanCredit$credit_risk=="good"); table(Y)
attach(GermanCredit)

## this does not really work (see lecture notes)
lin <- lm( Y ~ age)
plot(age,Y, xlim=c(0,120), pch="+"); abline(lin,col="red")

## logistic regression = logit model

logit1 <- glm( Y ~ age, family = binomial(link = "logit") )
## see ?glm and ?family
## alternatively we could do   logit1 <- glm( Y ~ age, family = binomial())
## as "logit" is the default value for the binomial family
summary(logit1)


## prediction
predict(logit1)  ## negativ values! these are the linear predictors
## ?predict.glm
lin1 <- predict(logit1); lin1; range(lin1)
range(plogis(lin1))
pred1 <- predict(logit1, type="response"); range(pred1)

plot(age,Y, pch="+")
points(age, pred1, col="green")

o <- order(age) ## note age is not sorted in original
plot(age[o],Y[o], pch="+")
lines(age[o], pred1[o], col="green", lwd=2)

## predict for a new value x=69
pred1.69 <- predict(logit1, newdata=data.frame(age=69), type="response"); pred1.69
## probability of credit default of a 69 year old customer estimated by model logit1
points(69, pred1.69,pch=19, col="blue")

## predicting by "hand"
b.hat <- coef(logit1)  ## estimated beta
lin.pred.69 <- b.hat[1] +b.hat[2]*69; lin.pred.69
pred.69 <- plogis(lin.pred.69); pred.69
points(69, pred.69,pch=19, col="red")

pred1.69 <- predict(logit1, newdata=data.frame(age=69)); pred1.69 ## linear predictor


## quadratic (linear) predictor [ linear in coefficients ]
logit2 <- glm( Y ~ age + I(age^2), family = binomial(link = "logit") )
summary(logit2)
AIC(logit1, logit2)  ## logit2 has lower AIC (is somewhat better)

pred2 <- predict(logit2, type="response"); pred2

plot(age[o],Y[o], pch="+")
lines(age[o], pred1[o], col="green")
lines(age[o], pred2[o], col="seagreen", lwd=2)

## predictions for age=69
pred1.69 <- predict(logit1, newdata=data.frame(age=69), type="response"); pred1.69
pred2.69 <- predict(logit2, newdata=data.frame(age=69), type="response"); pred2.69
points(69, pred1.69,pch=19, col="blue")
points(69, pred2.69,pch=19, col="darkblue")


## cubic (linear) predictor [ linear in coefficients ]
logit3 <- glm( Y ~ age + I(age^2) + I(age^3), family = binomial(link = "logit") )
summary(logit3)
AIC(logit1, logit2, logit3) ## logit2 is better than logit1 and logit3

pred3 <- predict(logit3, type="response"); pred3

plot(age[o],Y[o], pch="+")
lines(age[o], pred1[o], col="green")
lines(age[o], pred2[o], col="seagreen", lwd=2)
lines(age[o], pred3[o], col="black", lwd=2)

## persons older than 68
sum(age >68)

## add some more variables
logit4 <- glm( Y ~ age + I(age^2) + amount, family = binomial(link = "logit") )
summary(logit4)
AIC(logit1, logit2, logit3, logit4)

## predict for a 69-year old and an amount of 10000
predict(logit4, type="response", newdata=data.frame(amount=c(10000,10000),age=c(18,69)))

## what amounts do we have?
range(amount)
hist(amount)
boxplot(amount, horizontal = TRUE)

## predict for an 18-year an a 69-year old and an amount of 3000
predict(logit4, type="response", newdata=data.frame(amount=c(3000,3000),age=c(18,69)))

table(purpose)
carnew <- purpose=="car (new)"; table(carnew)
GermanCredit$carnew <- carnew

logit5 <- glm( Y ~ age + I(age^2) + amount + carnew, family = binomial(link = "logit") )
summary(logit5)
coef(logit5)
