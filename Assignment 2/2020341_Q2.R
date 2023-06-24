rm(list=ls())

# Given :- Random Survey On 75 death row inmates
# sample mean length = 17.4 years
# sample standard deviation 6.3 years
# H0 : mu = 15 years vs H1 : mu != 15 years
# So the test will be 1 sample 2 tailed with pop var unknown
n <- 75
sample_mean <- 17.4
sample_sd <- 6.3
pop_mean <- 15
alpha = 0.05
test_statistics = (sample_mean - pop_mean)/(sample_sd/sqrt(n))
right_criticalval = (qt(p=alpha/2,df=n-1,lower.tail = FALSE))
left_criticalval = (qt(p=alpha/2,df=n-1,lower.tail = TRUE))
p_val = 2 * pt(q=test_statistics,df = n-1,lower.tail = FALSE)
cat("Test Statistics : ",test_statistics,"\n")
cat("Left Critical Value : ",left_criticalval,"\n")
cat("Right Critical Value : ",right_criticalval,"\n")
cat("P Value : ",p_val,"\n")

# Conclusion : p val < alpha or test_statistics lies after the right critical value.
# so we reject H0 which means population mean time on death row will not be equal to 15 years.

