# install.packages("ISLR")
par(mfrow=c(4,3))
library(ISLR)
Auto
boxplot(Auto)

plot(Auto[["mpg"]])
plot(Auto[["cylinders"]])
plot(Auto[["displacement"]])
plot(Auto[["horsepower"]])
plot(Auto[["weight"]])
plot(Auto[["acceleration"]])
plot(Auto[["year"]])
plot(Auto[["origin"]])


horsepower <- Auto[["horsepower"]]
acceleration <-  Auto[["acceleration"]]

#afisam datele
plot(horsepower,acceleration)
#plot(horsepower,acceleration, col=4:30)


#cream modelul de regresie
linearMod1 <- lm(acceleration~horsepower )  
#afisam parametrii de regresie
print("Parametrii de regresie:")
print(linearMod1)
# alfa = 20.7019 , beta = -0.0494
#afisam dreapta de regresie
abline(linearMod1, col="red")
# afisam datele despre modelul de regresie
summary(linearMod1)


#Verificam parametrii pentru modelul liniar linearMod1
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



