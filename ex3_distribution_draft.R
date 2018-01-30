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
lambda <- 4
lines(schools_numeric,dpois(schools_numeric, lambda ), col="red")

ll2<-fitdistr(schools_numeric,"geometric")
lines(schools_numeric,dgeom(schools_numeric, 0.19708029 ), col="green")

#testez daca e repartitie Poisson
n = integer(4)
for(i in 1:length(schools_numeric)) {
  for(j in 1:4) {
    if((j-1<=schools_numeric[i]) && (schools_numeric[i]<j)) {
      n[j] = n[j]+1
      next
    }
  }
}
n
n = integer(3)
n[1] = 10
n[2] = 6
n[3] = 6
n
#determinam probabilitatile teoretice
p = integer(3)
p
p[1] = ppois(2, lambda) - ppois(1, lambda)
p[2] = ppois(2, lambda) 
p[3] = ppois(3, lambda) 
p

#determinam d
d = sum(n^2/(length(schools_numeric)*p))-length(schools_numeric)
d
qchisq(1-0.05, 2)



#--------------
lambda2 <- 0.19708029
p2 = integer(3)
p2
p2[1] = pgeom(2, lambda2) - pgeom(1, lambda2)
p2[2] = pgeom(2, lambda2) 
p2[3] = pgeom(3, lambda2) 
p2

#determinam d
d2 = sum(n^2/(length(schools_numeric)*p2))-length(schools_numeric)
d2
qchisq(1-0.05, 2)


#plot(density(schools_numeric))




n <- c(10, 6,6,10,7,4,6,4)
p3=rep(0,each=8)
for(i in 1:7) {
  p3[i] = dpois(i-1,lambda)
}
p3[8] = 1-ppois(6,lambda)
d3 = sum(n^2/(length(schools_numeric)*p3))-length(schools_numeric)
d3

qchisq(1-0.01,7)



n <- c(10, 6,6,10,7,4,6,4)
p4=rep(0,each=8)
for(i in 1:7) {
  p4[i] = dpois(i-1,lambda)
}
p4[8] = 1-ppois(6,lambda)
d4 = sum(n^2/(length(schools_numeric)*p4))-length(schools_numeric)
d4

qchisq(1-0.01,7)

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

#Repartitia geometrica cu p=pest=0.1970803 este acceptata la nivelul 
#de semnificatie de 4%

