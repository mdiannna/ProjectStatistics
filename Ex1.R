par(mfrow=c(2,3))
library(MASS)
# a
#afisam datasetul painters
print(painters)
#afisam datasetul faithful
print(faithful)

#b determinam distributia frecventelor

#distributia frecventelor
print("Distributia frecventelor scolilor de pictura din datasetul painters:")
cbind(table(painters$School))

print("Distributia frecventelor duratelor de eruptie din datasetul faithful")
#aflam valoarea minima si maxima
range(faithful$eruptions)
duration.freq = table(cut(faithful$eruptions, seq(1.6, 5.1, by = 0.2), right = FALSE))
cbind(duration.freq)



#c
#reprezint grafic datasetul faithful
plot(faithful)
#reprezentam grafic datasetul painters
boxplot(painters)

schools <- painters[["School"]]
#schools <- painters["School"]
print(schools)
#schools.frame(schools, x=sample(LETTERS[1:7], 7))
# schools <- schools[order(df$x),]
plot(schools)

plot(faithful[["eruptions"]])

#d
attach(faithful)
eruptions <- faithful["eruptions"]
print( eruptions)

eruptions_val <- faithful[["eruptions"]]
print("media:")
#print(sum(eruptions_val)/length(eruptions_val))
print(mean(eruptions_val))
print("Mediana:")
print(median(eruptions_val))
print("Varianta")
print(var(eruptions_val))

quantile(eruptions_val, 1/4)
quantile(eruptions_val, 2/4)
quantile(eruptions_val, 3/4)
quantile(eruptions_val, 4/4)

#boxplot(faithful~waiting, main="Lalal", col="blue")
boxplot(eruptions, main="Eruptions in faithful dataset", xlab="eruptions", ylab="value", col=2:5)
print(faithful)

#e
print("Codeficientul de variatie dintre eruptions si waiting")
print(cov(faithful["eruptions"],faithful["waiting"]))


hist(eruptions_val)
lines(density(eruptions_val))
#par(mfrow=c(1,1))





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
#painters[["School"]]