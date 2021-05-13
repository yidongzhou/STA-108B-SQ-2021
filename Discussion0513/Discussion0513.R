plastic <- read.table("/Users/easton/Google Drive/Teaching/TA/STA-108B-SQ-2021/Discussion0513/CH01PR22.txt")
# plastic <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR22.txt")
str(plastic)
colnames(plastic) <- c("Y", "X")
head(plastic)
n <- nrow(plastic)

# 5.26
# (a)
# (1) (X'X)^{-1}
X <- cbind(rep(1, n), plastic$X)
solve(t(X)%*%X)

# (2) b
# b = (X'X)^{-1}X'Y
b <- solve(t(X)%*%X)%*%t(X)%*%Y
b
lm(Y~X, data = plastic)

# (3) Yhat
# Yhat = HY = X(X'X)^{-1}X'Y = Xb
Yhat <- X%*%b
Yhat

# (4) H
# H = X(X'X)^{-1}X'
H <- X%*%solve(t(X)%*%X)%*%t(X)# n by n
H
dim(H)
