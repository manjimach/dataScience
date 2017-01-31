#-----------------------------------------------------
library(ISLR)
library(e1071)

attach(Auto)
summary(Auto)
#-----------------------------------------------------

medianmpg<-median(mpg)
high_low_mpg <- as.factor(ifelse(mpg > medianmpg, 1, 0))
#-----------------------------------------------------

dat=data.frame(Auto, high_low_mpg = as.factor(mpg>medianmpg))
set.seed(1)
tune.out <- tune(svm, high_low_mpg ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)
bestmod =tune.out$best.model
summary (bestmod )
#-----------------------------------------------------

set.seed(1)
tune.out <- tune(svm, high_low_mpg ~ ., data = dat, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out)
bestmod =tune.out$best.model
summary (bestmod )

set.seed(1)
tune.out <- tune(svm, high_low_mpg ~ ., data = dat, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod =tune.out$best.model
summary (bestmod )
#-----------------------------------------------------

svm.linear <- svm(high_low_mpg ~ ., data = dat, kernel = "linear", cost = 1)
svm.poly <- svm(high_low_mpg ~ ., data = dat, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(high_low_mpg ~ ., data = dat, kernel = "radial", cost = 100, gamma = 0.01)

plot(svm.linear,dat,mpg~cylinders)
plot(svm.linear,dat,mpg~displacement)
plot(svm.linear,dat,mpg~horsepower)
plot(svm.linear,dat,mpg~weight)
plot(svm.linear,dat,mpg~acceleration)
plot(svm.linear,dat,mpg~year)
plot(svm.linear,dat,mpg~origin)

plot(svm.poly,dat,mpg~cylinders)
plot(svm.poly,dat,mpg~displacement)
plot(svm.poly,dat,mpg~horsepower)
plot(svm.poly,dat,mpg~weight)
plot(svm.poly,dat,mpg~acceleration)
plot(svm.poly,dat,mpg~year)
plot(svm.poly,dat,mpg~origin)

plot(svm.radial,dat,mpg~cylinders)
plot(svm.radial,dat,mpg~displacement)
plot(svm.radial,dat,mpg~horsepower)
plot(svm.radial,dat,mpg~weight)
plot(svm.radial,dat,mpg~acceleration)
plot(svm.radial,dat,mpg~year)
plot(svm.radial,dat,mpg~origin)
