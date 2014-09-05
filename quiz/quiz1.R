# 1
猜测μ = 加权平均值
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(x*w)/sum(w)

# 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

sum(x*y)/sum(x^2)

#3
data(mtcars)
x<-mtcars$wt
y<-mtcars$mpg
beta1<-cor(y,x)*sd(y)/sd(x)

coef(lm(mtcars$mpg~mtcars$wt))

# 4
sd(x)/sd(y)=0.5
cor(y,x)=0.5

Cor(Y, X)Sd(y)/Sd(x) = 0.5/0.5=1

# 5
If you normalized the data， the slope is cor(y,x)
y=0 + cor(y,x)*x = 0.4 * X

# 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
miu <- mean(x)
S<-sd(x)

(8.58 - miu)/S 

# 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(x) - beta1*mean(x)
coef(lm(y~x))

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# 10
beta1<- cor(y,x) * sd(y)/sd(x)
y1<-cor(x,y) * sd(x)/sd(y)

beta1/y1 = (sd(y)^2) / (sd(x)^2) = var(y) / var(x)