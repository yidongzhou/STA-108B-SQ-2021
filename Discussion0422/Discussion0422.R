setwd("/Users/easton/Google Drive/Courses/TA/STA-108B-SQ-2021/datasets")
muscleMass <- read.table('CH01PR27.txt')
colnames(muscleMass) <- c('mass', 'age')
fit <- lm(mass ~ age, data = muscleMass)
summary(fit)
age <- 60
t(c(1, age))%*%fit$coefficients# 84.9468
sqrt(sum((muscleMass$mass-fit$fitted.values)^2)/58*(1/60+(60-mean(muscleMass$age))^2/sum((muscleMass$age-mean(muscleMass$age))^2)))

x <- c(43, 39, 41, 86, 72, 76)
y <- c(106, 106, 97, 60, 70, 80)
xbar <- mean(x)
ybar <- mean(y)
sum((x-xbar)^2)
sum((y-ybar)^2)
sum((x-xbar)*(y-ybar))
summary(lm(y~x))
confint(lm(y~x), level = 0.9)
anova(lm(y~x))
yh <- 30*(-0.8919)+139.5705
yh-6.417*sqrt(1/6+(30-xbar)^2/sum((x-xbar)^2))*qt(0.975, 4)
yh+6.417*sqrt(1/6+(30-xbar)^2/sum((x-xbar)^2))*qt(0.975, 4)
# (99.38, 126.25)


x <- c(43, 41, 47, 76, 72, 76)
y <- c(106, 106, 97, 56, 70, 74)
summary(lm(y~x))
confint(lm(y~x))
yh <- 30*(-1.1781)+154.5394
yh-6.545*sqrt(1/6+(30-xbar)^2/sum((x-xbar)^2))*qt(0.95, 4)
yh+6.545*sqrt(1/6+(30-xbar)^2/sum((x-xbar)^2))*qt(0.95, 4)
# [107.15, 131.24]