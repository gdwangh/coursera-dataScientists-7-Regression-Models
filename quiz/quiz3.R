library(datasets)
data(mtcars)

# Q1
fit<-lm(mpg~factor(cyl)+wt, data=mtcars)
summary(fit)

# Q2
fit<-lm(mpg~factor(cyl)+wt, data=mtcars)
fit$coeff

fit2<-lm(mpg~factor(cyl), data=mtcars)
fit2$coeff

summary(fit)$cov

# Q3
fit<-lm(mpg~factor(cyl)+wt, data=mtcars)
fit2<-update(fit, mpg~factor(cyl)+wt+factor(cyl)*wt)
anova(fit, fit2)

# Q4
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
lm(mpg ~ wt + factor(cyl), data = mtcars)

# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit<-lm(y~x)
round(hatvalues(fit), 4)

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit<-lm(y~x)
round(dfbetas(fit), 4)
