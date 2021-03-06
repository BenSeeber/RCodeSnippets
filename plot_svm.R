## a simple example
library(MASS)
library(e1071)
data(cats)
m <- svm(Sex~., data = cats,kernel="radial")
help(svm)
plot(m,cats)

## more than two variables: fix 2 dimensions
data(iris)
m2 <- svm(Species~., data = iris, kernel="polynomial")
plot(m2, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))


rm(list=ls(all=TRUE))

x1s <- c(.3,1,1,1.9,3,3.5,     1,3.5,4,5,5.5,4)
x2s <- c(3.5,1,2.5,2,1,1.2,  5.8,3,4,5,4,1)
ys <- c(rep(+1,6),rep(-1,6))
my.data <- data.frame(x1=x1s, x2=x2s, type=as.factor(ys))
my.data

library('e1071')
svm.model <- svm(type ~ ., data=my.data, type='C-classification', kernel='linear',scale=FALSE)

help(svm)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,7), ylim=c(-1,7))
points(my.data[svm.model$index,c(1,2)],col="blue",cex=2) 

w <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
p <- svm.model$SV

abline(a=-b/w[1,2], b=-w[1,1]/w[1,2], col="black", lty=1)
abline(a=(-b-1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)
abline(a=(-b+1)/w[1,2], b=-w[1,1]/w[1,2], col="orange", lty=3)

# Nonlin
dat <- my.data
fit = svm(type ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

plot(my.data[,-3],col=(ys+3)/2, pch=19, xlim=c(-1,7), ylim=c(-1,7))
xgrid = expand.grid(x1=seq(-1,7,by=0.05), x2=seq(-1,7,by=0.05))
ygrid = predict(fit, xgrid)
points(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
