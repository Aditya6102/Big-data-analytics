x<-0
count<-0
if( x>= -1 & x <=1)
{
  count <- count+1
}else {
  count <- count-1
}
count

#square root
for (i in 1:20)
{
  i<-sqrt(i)
  print(i)
}

#Vector creation
mySecondVector <- c( "I", "am", "Aditya")
mySecondVector
typeof(mySecondVector)

myThirdVector<- c(T,F,T)
myThirdVector
typeof(myThirdVector)

#vector with range
V <- 1:10
S <- -5:30
T <- 10:1
Mix <- c( 3:6, 24, 0:4, 31, 9:7)

#vector with seq func
A <- 1:10
A <- seq(1,10)
A
B <- seq(1,10, by=2)
B
C <- seq(5, -5, by=-4)
C

D <- seq(1,100, length=10)
D
E <- seq(0,10, length=10)
E

A <- rep("Hello", 5)
A

C <- rep(1:5,4)
C

D1 <- rep(c("A", "B", "C"), 3)
D1
D2 <- rep( c("A", "B", "C"), each=4)
D2
D3 <- rep( c("A", "B", "C"), c(2,1,3) )
D3
T <- c(3,6.8)
typeof(T)

X <- c(3,6,9)
typeof(X)
length(X)

k<-c("I", "am", "a", "rider")
k[4]
A <- c(5,51,42,76,42)
B <- sort(A)
B

X <- c( 1, 2, 3)
Y <- c( 4, 5 , 6, 7, 8, 9 )
Z <- X +Y
Z

#mathematical operations
X <- c( 1,5,7,9)
Y <- mean(X)
Y
V <- prod(X)
V

#upper limit
Tax <- c(32,56,44,76,89,24)
for (i in Tax)
  if (i< 50)
    print(i)

#classwork 2
Temp <- c(45, 77, 20, 19, 101, 120, 212)
Celsius <- (Temp - 32) * 5/9
print(Celsius)
print( {max(Celsius)})
print ({min(Celsius)})
print ({mean(Celsius)})