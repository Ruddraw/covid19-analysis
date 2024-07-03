rm(list = ls()) #removes all variables stored previously (Good practice)
library(Hmisc)

data <- read.csv("~/Documents/Code/R_learning/Covid19 analysis/COVID19_line_list_data.csv")
describe(data) #Hmisc command



#cleaned up death column
data$cleaned_death <- as.integer(data$death != 0)
#death rate
(sum(data$cleaned_death) / nrow(data)) * 100


#AGE claim: people who died are older 
dead = subset(data, cleaned_death == 1)
alive = subset(data,cleaned_death == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#is this statistically significant?
t.test(alive$age, 
       dead$age, 
       alternative = "two.sided", 
       conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

#gender claim: gender has no effect
male = subset(data, gender == "male")
female = subset(data, gender == "female")
mean(male$cleaned_death, na.rm = TRUE)
mean(female$cleaned_death, na.rm = TRUE)
sss
#is this statistically significant?
t.test(male$cleaned_death, 
       female$cleaned_death, 
       alternative = "two.sided", 
       conf.level = 0.99)
# men have from 0.8% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistically significant



