# Q1, 2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit<-lm(y~x)
summary(fit)

# 3
library(datasets)
data(mtcars)
fit<-lm(mpg~wt,data=mtcars)
summary(fit)
predict(fit, newdata=data.frame(wt=mean(mtcars$wt)),interval="confidence")


# 4
coef(fit)

# 5
library(datasets)
data(mtcars)
fit<-lm(mpg~wt, data=mtcars)
predict(fit, newdata=data.frame(wt=c(3)), interval="prediction")

# 6
fit2<-lm(mpg~I(wt/2), data=mtcars)
sumCoef <- summary(fit2)$coefficients
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit2$df) * sumCoef[2, 2]

# 8
Y=beta0+beta1*X = beta0-bata1*c + beta1*(X+c)

# 9
# with the intercept and slope (numerator)
fit<-lm(mpg~wt, data=mtcars)
e1<-resid(fit)

# just an intercept (denominator)
fit2<-lm(mpg~1, data=mtcars)
e2<-resid(fit2)

sum(e1^2)/sum(e2^2)
or
deviance(fit)/deviance(fit2)

plot(mpg~wt, data=mtcars,xlim=range(0,mtcars$wt), ylim=range(0,mtcars$mpg))
abline(lm(mpg~wt, data=mtcars),col="blue")  # with the intercept and slope
abline(lm(mpg~1, data=mtcars),col="red")  # just an intercept
abline(lm(mpg~wt-1, data=mtcars),col="green")  # remove an intercept, just slop
>>>>>>> d8f28cec7ac38fc0459e8210625d9fc1f59aed5c
