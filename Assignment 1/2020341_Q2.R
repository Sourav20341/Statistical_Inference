rm(list=ls())
set.seed(123)


#2a
data <- read.csv("data.csv")
data <- data[,c(2)]

mle.loglikelynormal <- function(theta){
  mu <- theta[1]
  sigma <- theta[2]
  val <- -(0.5)*length(data)*log(2*pi*sigma) - (0.5/sigma)*sum((data - mu)^2)
  return(val)
}
theta <- c(mean(data),var(data))
mlevalue.loglikely <- optim(par = theta,fn = mle.loglikelynormal,control = list(fnscale=-1))
print(mlevalue.loglikely)

#2b
optimmu <- 4000.04397
var = c(1:50)
fnval = c(1:50)
for(i in 1:50){
  var[i] <- i
  fnval[i] <- mle.loglikelynormal(c(optimmu,i))
}
plot(var,fnval)

optimsigma <- 15.53298
mu = c(1:50)
fnval = c(1:50)
vl <- 200
for(i in 1:50){
  mu[i] <- vl
  vl <- vl + 200
  fnval[i] <- mle.loglikelynormal(c(vl,optimsigma))
}
plot(mu,fnval)

