
#install.packages ("e1071", dependencies = TRUE)
library(e1071)
attach(iris)

model<-svm(iris$Species~.,data=iris)
summary(model)

x<-subset(iris,select = -Species)
y <- Species
pred <- predict(model, x)
summary(pred)

table(pred, y)
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])


## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
summary(m)
new <- predict(m, x)
summary(new)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

