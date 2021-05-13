

#===================================================
# Regression
# The Muscle Mass data set with an outlier.
#===================================================

#===================================================
# 1. Import data set
#===================================================


data_muscle<-read.table("/Users/easton/Google Drive/Courses/TA/STA-108B-SQ-2021/Midterm I/muscle2.txt")
is.data.frame(data_muscle)



#===================================================
# 2. Names and plot
#===================================================


names(data_muscle)<-c("Muscle", "Age")

plot(data_muscle, xlab="Muscle", ylab= "Age",col=2)


head(data_muscle)
attach(data_muscle)
names(data_muscle)


#==================================================
# 3. Diagnostic Plots for predictor
#==================================================


par(mfrow=c(2,2))

plot(data_muscle, xlab="Age", ylab= "Muscle",col=2)
plot(data_muscle[,2], xlab="Order", ylab= "Age",col=3,type="l")
boxplot(data_muscle[,2],main="",col=4)
hist(data_muscle[,2],main="Age",col=5,xlab="",freq=FALSE)


#===================================================
# 4. Regression using lm
#===================================================


model_muscle<-lm(Muscle~Age, data = data_muscle)

summary(model_muscle)

plot(model_muscle, xlab="Age",ylab="Muscle",col=2)
abline(model_muscle)


#==================================================
# 5. Anova and summary
#==================================================


summary(model_muscle)
anova(model_muscle)

par(mfrow=c(2,2))
plot(model_muscle)



