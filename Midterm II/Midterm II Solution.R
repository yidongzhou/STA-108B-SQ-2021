# Q1
plastic <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR22.txt")
colnames(plastic) <- c('Y', 'X')
str(plastic)
n <- nrow(plastic)
p <- 2

# (a)
## (X'X)^{-1}
X <- cbind(rep(1, n), plastic$X)# n by 2
Y <- plastic$Y# n by 1
solve(t(X)%*%X)

## b
## b = (X'X)^{-1}X'Y
b <- as.vector(solve(t(X)%*%X)%*%t(X)%*%Y)

# SSE
# SSE = (Y-Xb)'(Y-Xb)=Y'(I-H)Y
# H = X(X'X)^{-1}X'
H <- X%*%solve(t(X)%*%X)%*%t(X) # n by n
I <- diag(n)
SSE <- as.vector(t(Y)%*%(I-H)%*%Y)
SSE
MSE <- SSE/(n-p)

# (b) s(b_0), s(b_1), s(b_0, b_1)
s2b <- MSE*solve(t(X)%*%X)
s2b
sqrt(diag(s2b)[1])
sqrt(diag(s2b)[2])
s2b[1, 2]# or s2b[2, 1]

# (c) the matrix of the quadratic form for SSR
J <- rep(1, n)%*%t(rep(1, n))
H-J/n

# (d) 95% CI for \beta_1
# (6) s^2(b)
# s^2(b) = MSE(X'X)^{-1}
alpha <- 1-0.95
c(L = b[2] - qt(1-alpha/2, n-p)*sqrt(diag(s2b)[2]), U = b[2] + qt(1-alpha/2, n-p)*sqrt(diag(s2b)[2]))

fit <- lm(Y~X, data = plastic)
summary(fit)
# sqrt(MSE)=3.234
3.234^2*(n-p)