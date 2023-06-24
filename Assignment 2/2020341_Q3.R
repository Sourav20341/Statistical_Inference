rm(list = ls())

# Given :- H0 : mu = 12 quintals/hectare vs H1 : mu != 12 quintals/hectare
# Variety was tested on 10 randomly selected farmer fields
# yields in quintals/hectare is given
# level of significance is 5 percent
# Assumptions are met for 1 sample 2 tailed and pop variance unknown hypothesis testing
# 1-sample test/population var unknown.
dist <- c(14.3,12.6,13.7,10.9,13.7,12.0,11.4,12.0,12.6,13.1)
n <- 10
pop_mean <- 12.0
sample_mean <- sum(dist)/n
sample_var <- var(dist)
alpha <- 0.05
test_st = (sample_mean - pop_mean)/sqrt(sample_var/n)
right_criticalval<-(qt(p=alpha/2,df=n-1,lower.tail = FALSE))
left_criticalval <- (qt(p=alpha/2,df=n-1,lower.tail = TRUE))
p_val <- 2*pt(q = test_st,df = n-1,lower.tail = FALSE)
cat("Test Statistics : ",test_st,"\n")
cat("Right Critical Value : ",right_criticalval,"\n")
cat("Left Critical Value : ",left_criticalval,"\n")
cat("P value : ",p_val,"\n")

# Conclusion : So p_val > alpha or test_st lies between left critical and right critical val
# therefore we fail to reject null (H0) which means claim is true i.e., population mean = 12 quintals/hectare

