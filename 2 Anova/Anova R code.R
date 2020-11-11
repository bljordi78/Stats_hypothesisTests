# Set the working directory
setwd("C:\\Users\\jordi\\Documents\\_DATA ANALYTICS\\1 Advanced Business Data Analysis\\CA1\\2 One Way Anova")

# Read in data file 
players  <- read.csv(file="baseball.csv", head=TRUE, sep=",")

# Display the data file
View(players)

# Structure of the file
str(players)

Catcher<-subset(players, Position =='Catcher')
FirstBaseman<-subset(players, Position =='First Baseman')
SecondBaseman<-subset(players, Position =='Second Baseman')
Shortstop<-subset(players, Position =='Shortstop')
ThirdBaseman<-subset(players, Position =='Third Baseman')
Outfielder<-subset(players, Position =='Outfielder')
DesignatedHitter<-subset(players, Position =='Designated Hitter')
StartingPitcher<-subset(players, Position =='Starting Pitcher')
ReliefPitcher<-subset(players, Position =='Relief Pitcher')


# Boxplots
boxplot(Catcher$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Catcher's weights")
boxplot(FirstBaseman$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing First Baseman's weights")
boxplot(SecondBaseman$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Second Baseman's weights")
boxplot(Shortstop$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Shortstop's weights")
boxplot(ThirdBaseman$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Third Baseman's weights")
boxplot(Outfielder$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Outfielder's weights")
boxplot(DesignatedHitter$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Designated Hitter's weights")
boxplot(StartingPitcher$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Starting Pitcher's weights")
boxplot(ReliefPitcher$Weight, col = rgb(0,0,1,0.5), main = "Boxplot showing Relief Pitcher's weights")

# Shapiro test for normality
shapiro.test(Catcher$Weight)
shapiro.test(FirstBaseman$Weight)
shapiro.test(SecondBaseman$Weight)
shapiro.test(Shortstop$Weight)
shapiro.test(ThirdBaseman$Weight)
shapiro.test(Outfielder$Weight)
shapiro.test(DesignatedHitter$Weight)
shapiro.test(StartingPitcher$Weight)
shapiro.test(ReliefPitcher$Weight)


# Levene test for homogeneity of variances
library(car)
players <- subset(players, Position == 'Catcher' | Position == 'First Baseman' | Position == 'Second Baseman' | Position == 'Shortstop' | Position ==  'Third Baseman'  | Position == 'Designated Hitter' | Position ==  'Starting Pitcher')
leveneTest(Weight~Position, data=players)


# Use aov function
results = aov(Weight ~ Position, data=players) 

# display ANOVA table
summary(results)

# Tukey test to compare the groups
TukeyHSD(results)
??scheffe.test
