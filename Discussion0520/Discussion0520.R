plastic <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR22.txt")
colnames(plastic) <- c('Y', 'X')
str(plastic)
n <- nrow(plastic)
p <- 2

# (a)
# (5) SSE
# SSE = (Y-Xb)'(Y-Xb)=Y'(I-H)Y
# H = X(X'X)^{-1}X'
X <- cbind(rep(1, n), plastic$X)# n by 2
Y <- plastic$Y# n by 1
H <- X%*%solve(t(X)%*%X)%*%t(X) # n by n
I <- diag(n)
dim(H)
SSE <- as.vector(t(Y)%*%(I-H)%*%Y)
SSE
MSE <- SSE/(n-p)

fit <- lm(Y~X, data = plastic)
summary(fit)
# sqrt(MSE)=3.234
3.234^2*(n-p)

# (6) S^2(b)
# s^2(b) = MSE(X'X)^{-1}
s2b <- MSE*solve(t(X)%*%X)
s2b
summary(fit)
sqrt(diag(s2b))

# (7) s^2(pred)
# s^2(pred) = MSE(1+X_h'(X'X)^{-1}X_h)
Xh <- c(1, 30)
s2pred <- MSE*(1+t(Xh)%*%solve(t(X)%*%X)%*%Xh)
s2pred

# (b) s^2(b_0) s(b_0, b_1), s(b_1)
s2b
diag(s2b)[1]
s2b[1, 2]
sqrt(diag(s2b)[2])

# (c) the matrix of the quadratic form for SSE
I-H
