par(mfrow=c(3,2))
N <- 50

#Student distribution
df0 <- 1
df1 <- 10
df2 <- 5
df3 <- 8
ncp2 <-2 
ncp3 <- 4
  
x0 <- rt(N, df0)
x1 <- rt(N, df1)
x2 <- rt(N, df2, ncp2)
x3 <- rt(N, df3, ncp3)

plot(x0, dt(x0, df=df0), col="red")
points(x1, dt(x1, df=df1), col="green")
points(x2, dt(x2, df=df2,ncp2), col="blue")
points(x3, dt(x3, df=df3,ncp3), col="magenta")


plot(x0, pt(x0, df0), col="red")
points(x1, pt(x1, df1), col="green")
points(x2, pt(x2, df2,ncp2), col="blue")
points(x3, pt(x3, df3,ncp3), col="magenta")

#proprietati:
# - repartitia t este simetrica in jurul originii
# - cand n creste, tinde la o repartitie normala standard
# - utilizare in testarea ipotezelor statistice:
# --- poate fi folosita pentru a stabili intervale de incredere 
#      pentru media unei populatii


# exemplu:
#O selectie de volum n=20 a condus la rezultatele:
#34.8, 34.8, 34.9, 34.9, 34.9, 35, 35, 35, 35,35.1, 35.1, 35.1, 35.1, 35.1, 35.1, 35.3, 35.3, 35.3, 35.3, 35.3
#Sa se verifice la pragul de semnificatie alfa=0.05 ipoteza H0:m=m0=35 
# cu alternativa H1: m diferit de 35
x <- c(34.8, 34.8, 34.9, 34.9, 34.9, 35, 35, 35, 35,35.1, 35.1, 35.1, 35.1, 35.1, 35.1, 35.3, 35.3, 35.3, 35.3, 35.3)
alfa <- 0.05
n <- length(x)
media <- mean(x)
dispersia <- var(x)
t <- qt(1-alfa/2, n-1)
a <- media - (t * sqrt(dispersia))/(sqrt(n))
b <- media + (t * sqrt(dispersia))/(sqrt(n))

if(35 > a && 35 < b) {
  print("Ipoteza H0 este acceptata")
} else {
  print("Ipoteza H0 este respinsa")
}


#------------------------
#Fischer distribution

df11 <- 1
df12 <- 1
df21 <- 2 
df22 <- 2
df31 <- 3
df32 <- 1
df41 <- 10
df42 <- 8

x1 <- rf(N, df11, df12)
x2 <- rf(N, df21, df22)
x3 <- rf(N, df31, df32)
x4 <- rf(N, df41, df42)

plot(x0, df(x0, df11, df21), col="red")
points(x1, df(x1, df21, df22), col="green")
points(x2, df(x2, df32, df32), col="blue")
points(x3, df(x3, df41, df42), col="magenta")

plot(x0, pf(x0, df11, df21), col="red")
points(x1, pf(x1, df21, df22), col="green")
points(x2, pf(x2, df32, df32), col="blue")
points(x3, pf(x3, df41, df42), col="magenta")

#chi-square distribution

df1 <- 2
df2 <- 3
df3 <- 4 
df4 <- 5


x1 <- rchisq(N, df1)
x2 <- rchisq(N, df2)
x3 <- rchisq(N, df3)
x4 <- rchisq(N, df4)



plot(x0, dchisq(x0, df1), col="red")
points(x1, dchisq(x1, df2), col="green")
points(x2, dchisq(x2, df3), col="blue")
points(x3, dchisq(x3, df4), col="magenta")


plot(x0, pchisq(x0, df1), col="red")
points(x1, pchisq(x1, df2), col="green")
points(x2, pchisq(x2, df3), col="blue")
points(x3, pchisq(x3, df4), col="magenta")

#proprietati:
# --- poate fi folosita pentru a stabili intervale de incredere 
#      pentru dispersia unei populatii repartizate normal


#exemplu:


