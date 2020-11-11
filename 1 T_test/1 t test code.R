# Set the working directory
setwd("C:\\Users\\jordi\\Documents\\_DATA ANALYTICS\\1 Advanced Business Data Analysis\\CA1\\1 T test")

# Read in data file 
salary  <- read.csv(file="salaries.csv", head=TRUE, sep=",")

# Display the data file
View(salary)

# Structure of the file
str(salary)

obama <- subset(salary, ADMIN == 'OBAMA')
trump <- subset(salary, ADMIN == 'TRUMP')

# Boxplots
boxplot(obama$SALARY, col = rgb(0,0,1,0.5), main = "Boxplot showing Obama's admin staff salaries")
boxplot(trump$SALARY, col = rgb(0,0,1,0.5), main = "Boxplot showing Trump's admin staff salaries")

# Shapiro test for normality
shapiro.test(obama$SALARY)
shapiro.test(trump$SALARY)

# Levene test for homogeneity of variances
library(car)
leveneTest(SALARY~ADMIN, data=salary)

# T test to compare populations
t.test(obama$SALARY, trump$SALARY, alternative = "two.sided", paired = FALSE)

# Mann-Whitney U test to compare non normal populations
wilcox.test(obama$SALARY, trump$SALARY)
