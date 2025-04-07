

install.packages("xlsx")
library(xlsx)
#table <- read.xlsx("file.xlsx", 1)

# import the data 
#flu <- read.xlsx('~/Desktop/flu.xlsx')

require(gdata)

# Problem 1 a )
flu = read.xls('~/Downloads/flu.xlsx', sheet = 1, header = TRUE)

Y <- flu$Shot
X <- flu$Age

# fitting the model 
lm = glm(Shot~Age, data=flu, family='binomial')

# summary of the fit 

summary(lm)

### Problem 1 b) 

beta_0 <- lm$coefficients[1]
beta_1 <- lm$coefficients[2]
 
# estimated probability 

pi <- exp(beta_0 + beta_1*X) / (1+ exp(beta_0 + beta_1*X))  

## 
plot(X,pi, xlab= ' Age' , ylab = ' Estimated Prob. ')

## Problem 1 c)

pi <- exp(beta_0 + beta_1*Xi) / (1+ exp(beta_0 + beta_1*Xi))  # where Xi = 60

## Summary plot: these plots are important for the diagnostic 
#par(mfrow = c(2,2))
#plot(lm)

# extract the coefficients
beta_0 <- lm$coefficients[1]
beta_1 <- lm$coefficients[2]

# problem 1 c 
# data to predict 
new.data=data.frame(Age=60)


preds <-  predict(lm, new.data,interval="confidence",  se.fit = TRUE)

preds$fit # the predicted value 
preds$se.fit # the sd of the predicted value 

critval <- 1.96 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit) # upper value 
lwr <- preds$fit - (critval * preds$se.fit)  # lower value 
c(lwr, upr) # Confidence interval of pi'(60)

#   convert this to CI of pi(60)  you know the formula

# problem 2 

# You all got this 


# problem 3 a

BottleReturn <-  read.xls('~/Desktop/BottleReturn.xlsx', sheet = 1, header = TRUE)

yes=BottleReturn$Number.Returned
no=BottleReturn$Number.Sold-BottleReturn$Number.Returned

X<- BottleReturn$Deposit.level

# sample proportion 
pi <- yes/100 # all  ni = 100 

# plot logit of pi vs X
logit <- log(pi/(1-pi))
plot(X, logit) # label the axis OK ??? =) 

### problem 3 b

### fitting the model with beta_0 abd beta_1 
fit <- glm(cbind(yes,no) ~ X, family ='binomial', data = BottleReturn)
summary(fit)

## problem 3c


plot( X, pi, type="l", col="red" ) # pi is the sample prop from part a
par(new=TRUE)
plot( X, yi, type="l", col="green" ) # yi fitted logistic probability, You need to find yi, we did this in problem 1

# problem 3 d

# residuals : pearson, deviance 

res.P <- residuals(fit, type="pearson")  
res.D <- residuals(fit, type="deviance") # or you use res.D<-  rstandard(lm)

### alternative for residuals 

resid(fit) 
resid(fit, type='pear')
# the deviance can also be calculated as follows : sum of the devaince square 
sum(res.D^2)
# and the pearson : sum of the pearson residuals square 
sum(res.P^2)

# plot of  the residuals 

par(mfrow=c(1,2))
plot(res.D)
plot(res.P)

# boxplot of  the residuals 
par(mfrow=c(1,2))
boxplot(res.D,xlab= 'Deviance Residuals')
boxplot(res.P,xlab='Pearson Residuals')

### Standardized  residuals 

Sres.P <- rstandard(fit)  # standardized pearson 
Sres.D <- rstudent(fit)               # standardized deviance



############################  residuals vs fitted plot 

plot(lm$fitted.values,res.D, pch=20, main="Cubic Smoothing Splines", xlab= 'Fitted Values', main='Residuals Vs. Fitted values')
lines(smooth.spline(lm$fitted.values,res.D,spar=1.3), col="blue"); mtext(side=1,text=paste0("spar=1.3"),line=4) 

############################# dont need this 
####   half-normal plot 

#install.packages('faraway')
#library(faraway)
#halfnorm(lm$residuals)

#halfnorm(lm$residuals, nlab = 2, labs = as.character(1:length(lm)))
###########################################################

# problem 4 a, b, c is "doable" 


# Problem 4 d 
# H0 : pi' = beta_0 + beta_1 * Xi  ie pi' lies on a straight line 
#vs H1 : pi' does not lie ona straaight line

chi <- fit$null.deviance - fit$deviance

pchisq(chi   , df=1, lower.tail=FALSE)


## Problem 5 a)


### Goodness of fit  
### Goodness of fit : this test whether the model with the slope is as better
#as the model with only the intercept (This is the null) ie H0: Model 1 is a good as Model 0
# H1: Model 1 is better where 
# Model 1 : pi' = beta_0 + beta_1 X
# Model 0: pi' = beta_0 

# 1 Likelihood ratio test 

# fit is your glm fit object 
pchisq(fit$deviance, df=fit$df.residual, lower.tail=FALSE) # p-value 
# where G^2 <- fit$deviance , and df <- fit$df.residual

# Problem 5 b 
#  Pearson Chi square test see lecture 10 page 7 on how to derive this ... 
#find estimate number of bottle returns under H_0  ie, given that the model is true, you obtain 
# the fitted probability using pi <- exp()/1+exp() as usual, then you multiplied this by ni to obtain
# the expected counts under H_0

# problem 5 c 
# again follow lecture 10 page 7 on how to compute the double sum 



 



### HW6 ###

# I(Age^2) means a squared term Age^2
fit1=glm(Shot~Age+Health.Awareness+Gender+Age*Gender+I(Age^2),family=binomial(),data=flu)
summary(fit1)

library(MASS)
# stepwised AIC or BIC - the only difference is from their penalty term
# scope: defines the range of models examined in the stepwise search. 
# so the alg. will search for the 'best' model between the lower and the upper model

# forward AIC
# you may need a Null model as a staring point when you start searching 'forward'
Null=glm(Shot~1,family=binomial(),data=flu)
stepAIC(Null,scope = list(upper=~Age+Health.Awareness+Gender+Age*Gender+I(Age^2)), direction = "forward",k=2)
# backward AIC
# it starts from model fit1 and searching backward
stepAIC(fit1,scope = list(lower=~1), direction = "backward",k=2)
# both direction AIC (forward + backward)
# each step can go either forward or backward
stepAIC(fit1,scope = list(lower=~1,upper=~Age+Health.Awareness+Gender+Age*Gender+I(Age^2)), direction = "both",k=2)

# forward and backward BIC
n=nrow(flu)
stepAIC(Null,scope = list(upper=~Age+Health.Awareness+Gender+Age*Gender+I(Age^2)), direction = "forward",k=log(n))
stepAIC(fit1,scope = list(lower=~1), direction = "backward",k=log(n))

# final result from BIC backward; choose it as the final model
final=glm(Shot ~ Age + Health.Awareness,family=binomial(),data=flu)
summary(final)


### Likelihood Ratio Test
# Null is the reduced model and fit1 is the full model
# Test: H0: beta1=beta2=beta3=beta4=beta5=0 (all slopes are 0)
anova(Null,fit1,test = "Chisq")

# fit2 is the reduced model and fit1 is the full model
# Test: H0: beta4=beta5=0 (interaction and squared terms are 0)
fit2=glm(Shot~Age+Health.Awareness+Gender,family=binomial(),data=flu)
anova(fit2,fit1,test = "Chisq")


### Problem 3
GeriatricStudy = read.xls('~/Downloads/GeriatricStudy.xlsx', sheet = 1, header = TRUE)
# Poisson Regression
fit_poisson1=glm(Y~X1+X2+X3+X4,family=poisson,data=GeriatricStudy)
summary(fit_poisson1)

# fit_poisson2 is the reduced model and fit_poisson1 is the full model
# Test: H0: beta2=beta3=beta4=0 (X2 X3 X4 are not significant)
fit_poisson2=glm(Y~X1,family=poisson,data=GeriatricStudy)
anova(fit_poisson2,fit_poisson1,test = "Chisq")




