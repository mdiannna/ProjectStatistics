par(mfrow=c(3,2))
N <- 100

#Student distribution
df0 <- 1
df1 <- 6
df2 <- 5
df3 <- 8
ncp2 <-2 
ncp3 <- 4

x0 <- rt(N, df0)
x1 <- rt(N, df1)
x2 <- rt(N, df2, ncp2)
x3 <- rt(N, df3, ncp3)


plot( density(x0), col='orange')

plot(x0, dt(x0, df=df0), col="red")

hist(x0, prob=TRUE)
curve(dt(x0, df=df0), col="red", add=TRUE)
curve(dt(x1, df=df1), col="green", add=TRUE)
curve(dt(x2, df=df2,ncp2), col="blue", add=TRUE)
curve(dt(x3, df=df3, ncp3), col="magenta", add=TRUE)

lines( density(x0), col='orange')

plot(pt(x0, df0), col="red")
points(pt(x1, df1), col="green")
points(pt(x2, df2,ncp2), col="blue")
points(pt(x3, df3,ncp3), col="magenta")

x <- rchisq(100, 5)
hist(x0, prob=TRUE)
curve( dchisq(x, df=5), col='green', add=TRUE)
curve( dchisq(x, df=10), col='red', add=TRUE )
