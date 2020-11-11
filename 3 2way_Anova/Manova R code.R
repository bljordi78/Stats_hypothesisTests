# Set the working directory
setwd("C:\\Users\\jordi\\Documents\\_DATA ANALYTICS\\1 Advanced Business Data Analysis\\CA1\\3 Two Way Anova")

# Read in data file 
people  <- read.csv(file="PeopleIQ.csv", head=TRUE, sep=",")

# Display the data file
View(people)

# Structure of the file
str(people)

Wblack<-subset(people, sex == 'female' & race =='black')
Wother<-subset(people, sex == 'female' & race =='other')
Wwhite<-subset(people, sex == 'female' & race =='white')
Mblack<-subset(people, sex == 'male' & race =='black')
Mother<-subset(people, sex == 'male' & race =='other')
Mwhite<-subset(people, sex == 'male' & race =='white')

# Shapiro test for normality
shapiro.test(Wblack$IQ)
shapiro.test(Wother$IQ)
shapiro.test(Wwhite$IQ)
shapiro.test(Mblack$IQ)
shapiro.test(Mother$IQ)
shapiro.test(Mwhite$IQ)

# Run the anova function
result <- aov( IQ ~ sex * race, data=people)
summary(result)

