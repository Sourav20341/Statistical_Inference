rm(list = ls())

# Given :- Data is given for girls and boys on time spend in playing sports
# The distribution is normally distributed
# H0 : mean of boys time spend  = mean of girls time spend on playing sports vs
# H1 : mean of boys time spend != mean of girls time spend on playing sports
# level of significance is 5 percent
# No information given about their population variances so we assume them they are unequal
# So assumption met for 2 sample 2 tailed with variance unequal hypothesis test
n_boys <- 16
n_girls <- 9
sample_mean_boys <- 3.2
sample_mean_girls <- 2
sample_var_boys <- 1
sample_var_girls <- 0.75
alpha <- 0.05
df <- min(n_boys-1,n_girls-1)
t_test <- (sample_mean_boys - sample_mean_girls)/sqrt((sample_var_girls/n_girls) + (sample_var_boys/n_boys))
left_criticalval <- (qt(p=alpha/2,df=df,lower.tail = TRUE))
right_criticalval <- (qt(p=alpha/2,df=df,lower.tail = FALSE))
p_val = 2*pt(q = t_test,df = df,lower.tail = FALSE)
cat("Test Statistics : ",t_test,"\n")
cat("Left Critical Value : ",left_criticalval,"\n")
cat("Right Critical Value : ",right_criticalval,"\n")
cat("P Value : ",p_val,"\n")

# Conclusion : p_val < alpha or t_test lies after the right critical value
# so we reject H0 which means claim is false i.e., population means are not equal.

