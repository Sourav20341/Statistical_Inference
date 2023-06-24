rm(list=ls())

# Given : Baker claims that his bread height is more than 15 cm on the average i.e., population mean = 15 cm 
# so we will take null hypothesis H0 : mu <= 15 cm and alternative H1 : mu > 15 cm
# He Bakes 10 loaves of bread and he found mean = 17 cm so sample mean = 17 cm.
# Now population standard deviation also given 0.5cm and the distribution is normal.
# level of Significance = 5 percent = 0.05
# So assumptions of 1 sample test and population standard deviation is known are met.

n <- 10
sample_mean <- 17
pop_mean <- 15
pop_sd <- 0.5
alpha <- 0.05
z_test_st <- (sample_mean - pop_mean)/(pop_sd/sqrt(n))
# This is 1-tailed test and critical value will lie in right.
critical_val <- qnorm(p=alpha,0,1,lower.tail = FALSE)
p_val <- pnorm(z_test_st,0,1,lower.tail = FALSE)
cat("Test Statistic Value",z_test_st,"\n")
cat("Critical Value",critical_val,"\n")
cat("P value: ",p_val,"\n")

# Conclusion : So p_val < alpha or z_test_st value is lies in right of
# critical value so this shows that we reject null which means pop_mean <= 15 cm so
# baker's claim is true i.e., pop_mean > 15 cm.


