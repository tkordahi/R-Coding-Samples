setwd("~/SDSU CLASSES/FALL 2021/MIS 620/ASSIGNMENT 1")

# Question 1, Load suspect heartrate data file
heartrates <- read.csv("suspectheartrates.csv", stringsAsFactors = F)

# Question 2, Columns and data types, Question 3, Number of suspects
str(heartrates)
names(heartrates)

# Question 4, average, min, max, median
summary(heartrates$heartbpm)
mean(heartrates$heartbpm)
min(heartrates$heartbpm)
max(heartrates$heartbpm)
median(heartrates$heartbpm)


#Question 5 histogram and scatterplot of heartrates
hist(heartrates$heartbpm)
plot(heartrates$heartbpm)

#Question 6 average age of suspect
mean(heartrates$age)

#Question 7 males and females
heartrates.F <- heartrates[heartrates$sex == "Female",]
heartrates.M <- heartrates[heartrates$sex == "Male",]
str(heartrates.F)
str(heartrates.M)

#Question 8 truthtellers and liars
truth <- heartrates[heartrates$veracity == "Truth",]
liar <- heartrates[heartrates$veracity == "Lie",]
str(truth)
str(liar)

#Question 10 testing hypothesis 1
gveracity.table <- table(as.factor(heartrates$veracity), as.factor(heartrates$sex))
gveracity.table
hyp1 <- chisq.test(gveracity.table)
hyp1$expected

#Question 11 testing hypothesis 2
install.packages("TeachingDemos")
library(TeachingDemos)
z.test(liar$heartbpm, 94, sd(liar$heartbpm), alternative = "greater")

#Question 12 visual for hyp 1
plot(gveracity.table, col = c("red", "blue"))

#extra
sum(with(liar, heartbpm > 94))
greater <- liar[liar$heartbpm > 94,]
str(greater)

