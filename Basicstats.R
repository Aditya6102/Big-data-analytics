x<-rnorm(5)

N <-100000
a <-rnorm(N)
b <- rnorm(N)
#Vectorized approach
c<- a * b
c


X <- rnorm(5)
X <- rnorm( 5, 10, 1)
X <- rnorm (5, 10)
X <- rnorm(5, mean=10, sd = 1)
X <- rnorm(5, sd=1, mean=10)
X <- rnorm(5, sd=1)

x <-rnorm(10)
plot(x)
hist(x)