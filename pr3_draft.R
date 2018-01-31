# install.packages("ISLR")
par(mfrow=c(2,2))
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

plot(horsepower,acceleration)
#plot(horsepower,acceleration, col=4:30)
abline(linearMod1, col="red")






distPred1 <- predict(linearMod1)
#distPred1

summary(linearMod1)

error1 <- sum((distPred1 - acceleration)^2)
error1


#Calcularea parametrilor pentru linearMod1
#xm <- 1/length(horsepower) * (sum(horsepower))
xm <-mean(horsepower)
#ym <- 1/length(acceleration) * (sum(acceleration))
ym <- mean(acceleration)
sum1 <- sum((horsepower-xm)^2)
sum2 <- sum((horsepower-xm)*(acceleration-ym))
beta <- sum2/sum1
alfa <- ym - beta*xm
print("alfa:")
print(alfa)
print("beta:")
print(beta)
linearMod1

n<-length(horsepower)
#dispersia estimata
disp_estimata <- 1.00/(n-2) * ((sum((acceleration-ym)^2))-(beta^2)*(sum((horsepower-xm)^2)))
disp_estimata
#deviatia standard estimata:
sqrt(disp_estimata)

summary(linearMod1)

#Calculam coeficientul de determinare pentru a stabili 
# acuratetea modelului
#R2 = { ( 1 / N ) * Σ [ (xi - x) * (yi - y) ] / (σx * σy ) }^2
R2 = ((1/length(horsepower)) * sum((horsepower-mean(horsepower)) * (acceleration-mean(acceleration))) / (sd(horsepower) * sd(acceleration)) )^2 
R2
#R2 =  0.4725701 
# Interpretarea rezultatelor: 47,25% din varianta acceleratiei poate fi estimata avand doar valoarea pentru horsepower



# Determinam un interval de predicție de 95% pentru valoarea 
# de 200 cai-putere a unui motor
newdata =data.frame(horsepower=200)
prediction <- predict(linearMod1, newdata, interval = "predict")
prediction
#reprezint grafic punctul estimat
points(200, 10.82228, pch = "X", col = "cyan")
# intervalul de predicție este:
# [6.851216, 14.79334]
# Interpretare: pentru o valoare de 200 cai-putere a unui motor, 
# acceleratia estimata este situata in intervalul [6.851216, 14.79334]
# si este aproximativ egala cu 10.82228





#-----------------------
#linear mod 2


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
