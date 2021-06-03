job <- read.table('http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%209%20Data%20Sets/CH09PR10.txt')
str(job)
colnames(job) <- c('Y', paste0('X', 1:4))
?step
fit <- lm(Y~., data = job)# full model Y ~ X1+X2+X3+X4
step(object = fit, trace = 0)# final model Y ~ X1+X2+X4
