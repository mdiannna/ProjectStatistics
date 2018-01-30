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
lines(schools_numeric,dpois(schools_numeric, 4.074074 ), col="red")

ll2<-fitdistr(schools_numeric,"geometric")
lines(schools_numeric,dgeom(schools_numeric, 0.19708029 ), col="green")

#plot(density(schools_numeric))

