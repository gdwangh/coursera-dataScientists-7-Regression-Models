library(datasets);library(car);data(mtcars)
mtcars$am<-factor(mtcars$am)

pairs(mtcars, panel = panel.smooth, main = "mtcars")
pairs(mpg~am + wt + qsec, data=mtcars,panel = panel.smooth, main = "mtcars")
fit1<-lm(mpg ~ ., data=mtcars)
summary(fit1)$adj.r.squared


fit2<-lm(mpg ~ disp + hp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + hp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + drat +qsec + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + drat + wt + vs+ am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + drat + wt + qsec + am + gear + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs+ am + carb, data=mtcars);summary(fit2)$adj.r.squared
fit2<-lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs+ am + gear, data=mtcars);summary(fit2)$adj.r.squared
# adjust R-squared: 0.8153314, delete cyl
fit2<-lm(mpg ~ disp + hp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars)


fit3<-lm(mpg ~ hp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + drat + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + wt + qsec + vs+ am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + drat + qsec + vs+ am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + drat + wt + vs+ am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + drat + wt + qsec + am + gear + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + drat + wt + qsec + vs+ am + carb, data=mtcars);summary(fit3)$adj.r.squared
fit3<-lm(mpg ~ disp + hp + drat + wt + qsec + vs+ am + gear , data=mtcars);summary(fit3)$adj.r.squared

# adjust R-squared: 0.823039
fit3<-lm(mpg ~ disp + hp + drat + wt + qsec + am + gear + carb, data=mtcars)

fit4<-lm(mpg ~ hp + drat + wt + qsec + am + gear + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + drat + wt + qsec + am + gear + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + hp + wt + qsec + am + gear + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + hp + drat + qsec + am + gear + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + hp + drat + wt + am + gear + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + hp + drat + wt + qsec + am + carb, data=mtcars);summary(fit4)$adj.r.squared
fit4<-lm(mpg ~ disp + hp + drat + wt + qsec + am + gear , data=mtcars);summary(fit4)$adj.r.squared

# adjust R-squared: 0.8296261
fit4<-lm(mpg ~ disp + hp + drat + wt + qsec + am + gear , data=mtcars)


fit5<-lm(mpg ~ hp + drat + wt + qsec + am + gear , data=mtcars);summary(fit5)$adj.r.squared
fit5<-lm(mpg ~ disp + drat + wt + qsec + am + gear , data=mtcars);summary(fit5)$adj.r.squared
fit5<-lm(mpg ~ disp + hp + wt + qsec + am + gear , data=mtcars);summary(fit5)$adj.r.squared
fit5<-lm(mpg ~ disp + hp + drat +qsec + am + gear , data=mtcars);summary(fit5)$adj.r.squared
fit5<-lm(mpg ~ disp + hp + drat + wt + am + gear , data=mtcars);summary(fit5)$adj.r.squared
fit5<-lm(mpg ~ disp + hp + drat + wt + qsec + am  , data=mtcars);summary(fit5)$adj.r.squared
# adjust R-squared: 0.8347177
fit5<-lm(mpg ~ disp + hp + drat + wt + qsec + am  , data=mtcars)

fit6<-lm(mpg ~ hp + drat + wt + qsec + am  , data=mtcars);summary(fit6)$adj.r.squared
fit6<-lm(mpg ~ disp + drat + wt + qsec + am  , data=mtcars);summary(fit6)$adj.r.squared
fit6<-lm(mpg ~ disp + hp + wt + qsec + am  , data=mtcars);summary(fit6)$adj.r.squared
fit6<-lm(mpg ~ disp + hp + drat + qsec + am  , data=mtcars);summary(fit6)$adj.r.squared
fit6<-lm(mpg ~ disp + hp + drat + wt + am  , data=mtcars);summary(fit6)$adj.r.squared
# adjust R-squared: 0.8375334
fit6<-lm(mpg ~ disp + hp + wt + qsec + am  , data=mtcars)

fit7<-lm(mpg ~ hp + wt + qsec + am  , data=mtcars);summary(fit7)$adj.r.squared
fit7<-lm(mpg ~ disp + wt + qsec + am  , data=mtcars);summary(fit7)$adj.r.squared
fit7<-lm(mpg ~ disp + hp + qsec + am  , data=mtcars);summary(fit7)$adj.r.squared
fit7<-lm(mpg ~ disp + hp + wt + am  , data=mtcars);summary(fit7)$adj.r.squared

# stop, adjust R-squared: 0.8367919 < 0.8375334
fitn <- fit6
summary(fit6)

par(mfrow = c(1, 1))
rng<-round(c(-1,1)*max(abs(range(resid(fitn)))), 0)
plot(predict(fitn), resid(fitn), main="Resid vs. fit", ylim=rng)
abline(h=0, col="red", lty = 3)
abline(h=4, col="red", lty = 3)
abline(h=-4, col="red", lty = 3)
par(mfrow = c(2, 2)); plot(fitn)


