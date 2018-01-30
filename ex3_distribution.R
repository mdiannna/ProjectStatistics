par(mfrow=c(2,3))
library(MASS)

schools <- painters[["School"]]
#schools <- painters["School"]
print(schools)



#determinam repartitia utilizand un test(estimatori ai repartitiei)
#range(schools)
schools_numeric <- c()
for(i in 1:length(schools)) {
  if(schools[i] == 'A') {
    schools_numeric[i] = 1;
  } else
    if(schools[i] == 'B') {
      schools_numeric[i] = 2;
    }else
      if(schools[i] == 'C') {
        schools_numeric[i] = 3;
      }else
        if(schools[i] == 'D') {
          schools_numeric[i] = 4;
        }else
          if(schools[i] == 'E') {
            schools_numeric[i] = 5;
          }else
            if(schools[i] == 'F') {
              schools_numeric[i] = 6;
            }else
              if(schools[i] == 'G') {
                schools_numeric[i] = 7;
              }else
                if(schools[i] == 'H') {
                  schools_numeric[i] = 8;
                }
}

range(schools_numeric)
plot(schools_numeric)
plot(schools)

mean(schools_numeric)
var(schools_numeric)
#painters[["School"]]
hist(schools_numeric, probability=TRUE)
ll<-fitdistr(schools_numeric,"Poisson")
lambda <- 4.074074 
lines(schools_numeric,dpois(schools_numeric, lambda ), col="red")

ll2<-fitdistr(schools_numeric,"geometric")
lines(schools_numeric,dgeom(schools_numeric, 0.19708029 ), col="green")


n <- c(10, 6,6,10,7,4,6,4)
p4=rep(0,each=8)
for(i in 1:7) {
  p4[i] = dpois(i-1,lambda)
}
p4[8] = 1-ppois(6,lambda)
len <- length(schools_numeric)
d4 = sum(n^2/(len*p4))-len
d4

qchisq(1-0.05,7)
#nu este rep poission deoarece d4<qchisq...

#-----------------
n <- c(10, 6,6,10,7,4,6,4)
pest<- 1/(1+mean(schools_numeric))
pest

p = integer(5)
for(i in 1:7){
  p[i] = dgeom(i-1, pest)
}
r<-1
k<-8
#k-r-1
p[8] = 1-pgeom(k-r-1, pest)

len <-length(schools_numeric) 
d = sum(n^2/(len*p))-len
d
qchisq(1-0.04, k-r-1)

#Repartitia geometrica cu p=p_estimat=0.1970803 este acceptata la nivelul 
#de semnificatie de 4%



eruptions <- faithful["eruptions"]
print( eruptions)

eruptions_val <- faithful[["eruptions"]]


#plot(eruptions_val)
#ller<-fitdistr(schools_numeric,"beta")
#lambda <-0.19708029 
#plot(density(eruptions_val))
#lines(schools_numeric,dgeom(eruptions_val,  ), col="green")

#plot(ecdf(erruptions_val))
ller<-fitdistr(eruptions_val,"gamma")

eruptions_val <- sort(eruptions_val)
hist(eruptions_val, probability = TRUE)

lines(eruptions_val, dgamma(eruptions_val, 7.9664117,  2.2840913 ), col="red")

fitdistr(eruptions_val,"normal")

lines(eruptions_val, dbeta(eruptions_val,  0.5, 0.5  ), col="green")



fitdistr(eruptions_val,"t")
fitdistr(eruptions_val,"beta")

lines(eruptions_val, dnorm(eruptions_val,  3.48778309,  1.13927121  ), col="blue")


lines(eruptions_val, dgamma(eruptions_val, 7.9664117,  2.2840913 ,lower=0.01), col="red")

#fitdistr(eruptions_val, dbeta, list(shape1 = 0.5, shape2=0.5), lower = 0.01)
lines(eruptions_val, dbeta(eruptions_val,4,3), col="green")



#distributia frecventelor
print("Distributia frecventelor scolilor de pictura din datasetul painters:")
cbind(table(painters$School))

print("Distributia frecventelor duratelor de eruptie din datasetul faithful")
#aflam valoarea minima si maxima
range(faithful$eruptions)
duration.freq = table(cut(faithful$eruptions, seq(1.6, 5.1, by = 0.2), right = FALSE))
cbind(duration.freq)



