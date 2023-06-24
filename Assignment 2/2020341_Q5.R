rm(list=ls())

# Given :- Two types of baby foods A and B and result of change in weight (lbs) were observed in 8 children.
# Data is given and difference of pair follows normally distribution
# H0 : mu of difference of pairs = 0 vs H1 : mu of difference of pairs != 0
# samples are dependent
# We use t.test with paired true because we have to check for difference 0 or not
# And the test is 2-tailed.

n <- 8
FoodA <- c(49,53,51,52,47,50,52,53)
FoodB <- c(52,55,52,53,50,54,54,53)
alpha <- 0.05
test.ans <-t.test(x=FoodA, y=FoodB, paired = TRUE, alternative = "two.sided")
t.stat <-test.ans$statistic
left_critical_value <- qt(p = alpha/2,df=n-1,lower.tail = TRUE)
right_critical_value <- qt(p = alpha/2,df=n-1,lower.tail = FALSE)
p_val <- test.ans$p.value
cat("Test Statistics : ",t.stat,"\n")
cat("Left Critical Value : " ,left_critical_value,"\n")
cat("Right Critical Value : ",right_critical_value,"\n")
cat("P Value : ",p_val,"\n")

# Conclusion : p_val < alpha or t.stat lies before left critical value which is rejection region 
# So we reject H0 which means claim is false i.e., average difference of pairs are not zero

