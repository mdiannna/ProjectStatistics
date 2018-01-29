# install.packages("ISLR")
par(mfrow=c(3,4))
library(ISLR)
Auto
plot(Auto)

plot(Auto[["mpg"]])
#hist(Auto[["mpg"]])
plot(Auto[["cylinders"]])
plot(Auto[["displacement"]])
plot(Auto[["horsepower"]])
#boxplot(Auto)
#boxplot(Auto["horsepower"])
plot(Auto[["weight"]])
plot(Auto[["acceleration"]])
#hist(Auto[["origin"]])
plot(Auto[["year"]])
plot(Auto[["origin"]])

par(mfrow=c(2,2))
#plot(Auto[["year"]], Auto[["horsepower"]])
#plot(Auto[["cylinders"]], Auto[["acceleration"]])
#plot(Auto[["cylinders"]], Auto[["horsepower"]])
#plot(Auto[["acceleration"]], Auto[["displacement"]])


horsepower <- Auto[["horsepower"]]
acceleration <-  Auto[["acceleration"]]
displacement <-  Auto[["displacement"]]
cylinders <- Auto[["cylinders"]]


linearMod1 <- lm(acceleration~horsepower )  # build linear regression model on full data
print("Parametrii de regresie:")
print(linearMod1)

#plot(horsepower,acceleration)
plot(horsepower,acceleration, col=4:30)
abline(linearMod1, col="red")

linearMod2 <- lm(displacement~horsepower)  # build linear regression model on full data
print("Parametrii de regresie:")
print(linearMod2)

plot(horsepower, displacement, col=1:3)
abline(linearMod2, col="magenta")

distPred2 <- predict(linearMod2)
#distPred2

summary(linearMod2)

error2 <- sum((distPred2 - displacement)^2)
error2





distPred1 <- predict(linearMod1)
#distPred1

summary(linearMod1)

error1 <- sum((distPred1 - acceleration)^2)
error1

