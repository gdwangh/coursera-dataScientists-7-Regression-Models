library(MASS)


# Q1
data(shuttle)
logAutoLand<-glm(use~wind, data=shuttle,family="binomial")
summary(logAutoLand)

# odds ratio
exp(logAutoLand$coeff)

# Q2
# adjust for magn
logAutoLand2<-glm(use~wind+magn, data=shuttle,family="binomial")
exp(logAutoLand2$coeff)

# Q3
data(shuttle)
library(data.table)
df<-data.table(shuttle[,c("use","wind")])
df[df$use=="auto",flag_1:=1]
df[df$use=="noauto",flag_1:=0]
df[df$use=="auto",flag_2:=0]
df[df$use=="noauto",flag_2:=1]

logAutoLand1<-glm(use~flag_1, data=df,family="binomial")
logAutoLand1$coeff

logAutoLand2<-glm(use~flag_2, data=df,family="binomial")
logAutoLand2$coeff


# Q4
data(InsectSprays)
glm1 <- glm(count~spray,family="poisson",data=InsectSprays)
1/exp(glm1$coeff)

# Q5
t<-0:100
t2<-log(10)+t
count<-dpois(t*1000, lambda=3)
x<-sample(c(0,1), size=101, replace=TRUE)
df<-cbind(t,t2,x,count)
g1<-glm(count ~ x + offset(t), family = poisson)
g2<-glm(count ~ x + offset(t2), family = poisson)
g1$coeff
g2$coeff

# Q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)

x1<-x[7:11]
y1<-y[7:11]
fit<-lm(y1~x1)
summary(fit)
