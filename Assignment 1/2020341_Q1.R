rm(list = ls())
set.seed(123)

#1a
data1 = rexp(1000,1)
data2 = rexp(1000,2)
data3 = rexp(1000,3)
data4 = rexp(1000,4)

#1b
mle.exp <- function(lambda,data){
  s = sum(data)
  val = 1000*(log(lambda)) - lambda*s
  return(val)
}

moment <- function(data){
  s = sum(data)
  return(1000/s)
}

m1 = moment(data1)
mleval12.exp <- optim(m1,data=data1,mle.exp,method='Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval11.exp <- optim(m1-0.4,data=data1,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval13.exp <- optim(m1+0.4,data=data1,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))

print(mleval11.exp)
print(mleval12.exp)
print(mleval13.exp)

m2 = moment(data2)
mleval22.exp <- optim(m2,data=data2,mle.exp,method='Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval21.exp <- optim(m2-0.4,data=data2,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval23.exp <- optim(m2+0.4,data=data2,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))

print(mleval21.exp)
print(mleval22.exp)
print(mleval23.exp)

m3 = moment(data3)
mleval32.exp <- optim(m3,data=data3,mle.exp,method='Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval31.exp <- optim(m3-0.4,data=data3,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval33.exp <- optim(m3+0.4,data=data3,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))

print(mleval31.exp)
print(mleval32.exp)
print(mleval33.exp)

m4 = moment(data4)
mleval42.exp <- optim(m4,data=data4,mle.exp,method='Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval41.exp <- optim(m4-0.4,data=data4,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))
mleval43.exp <- optim(m4+0.4,data=data4,mle.exp,method = 'Brent',lower = 0,upper = 1000,control = list(fnscale = -1))

print(mleval41.exp)
print(mleval42.exp)
print(mleval43.exp)

#1c
lambdavalue <- c(1:100)
mleval <- c(1:100)
i <- 0.1
j <- 1
while(i < 10){
  lambdavalue[j] <- i
  mleval[j] <- mle.exp(i,data=data1)
  i <- i + 0.1
  j <- j+1
}
plot(lambdavalue,mleval,type = 'l')

lambdavalue <- c(1:100)
mleval <- c(1:100)
i <- 0.1
j <- 1
while(i < 10){
  lambdavalue[j] <- i
  mleval[j] <- mle.exp(i,data=data2)
  i <- i + 0.1
  j <- j+1
}
plot(lambdavalue,mleval,type = 'l')

lambdavalue <- c(1:100)
mleval <- c(1:100)
i <- 0.1
j <- 1
while(i < 10){
  lambdavalue[j] <- i
  mleval[j] <- mle.exp(i,data=data3)
  i <- i + 0.1
  j <- j+1
}
plot(lambdavalue,mleval,type = 'l')

lambdavalue <- c(1:100)
mleval <- c(1:100)
i <- 0.1
j <- 1
while(i < 10){
  lambdavalue[j] <- i
  mleval[j] <- mle.exp(i,data=data4)
  i <- i + 0.1
  j <- j+1
}
plot(lambdavalue,mleval,type = 'l')
