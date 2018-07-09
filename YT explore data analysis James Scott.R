#---------------------------------------------------------------
# Exploratory data analysis 1 by James Scott (2014)
# https://www.youtube.com/watch?v=Yo7BPFSGrdw
# https://github.com/jgscott/STA371H_Spring2015
#---------------------------------------------------------------
#install.packages("effects")
#install.packages("mosaic")

rm(list = ls())
library(effects)
library(mosaic)

# load in data from "effects" package
data("TitanicSurvival", package = "effects")

names(TitanicSurvival)
head(TitanicSurvival, 20)
str(TitanicSurvival)

#make tables that show who survived stratified by sex and class
xtabs(~survived + sex, data=TitanicSurvival)
tally(~survived + sex, data=TitanicSurvival, margins=TRUE)
tally(~survived + sex:passengerClass, data=TitanicSurvival, margins=TRUE)

#Define factors for age and age:sex interaction
AgeFactor = cut(TitanicSurvival$age, breaks = c(0,17,Inf), labels=c("Child", "Adult"))
AgeFactor

AgeSexFactor = factor(AgeFactor:TitanicSurvival$sex)
AgeSexFactor

#Make a table that shows who survived by age/sex
tab1 = xtabs(~survived + AgeSexFactor, data=TitanicSurvival)
tab1



#---------------------------------------------------------------
# City temp data
#---------------------------------------------------------------
casandie <- read.table("D:/Downloads/STA371H_Spring2015-master/data/casandie.txt", header = TRUE)
sdrapcty <- read.table("D:/Downloads/STA371H_Spring2015-master/data/sdrapcty.txt", header = TRUE)

hist(casandie$Temp, main = "Avg Daily Temp in San Diego 1995-2011", xlab = "Temp")
hist(sdrapcty$Temp, main = "Avg Daily Temp in Rapid City 1995-2011", xlab = "Temp")

#Put histograms for SD and RC next to each other
par(mfrow=c(2,1))  # 2 rows 1 column
hist(casandie$Temp)
hist(sdrapcty$Temp)

# line things up to make comparisons easier

breakseq = seq(-20, 92, by=2)
par(mfrow=c(2,1))  # 2 rows 1 column
hist(casandie$Temp, breaks=breakseq, xlim=c(-20,100), ylim=c(0,760), main = "Avg Daily Temp (F) in San Diego 1995-2011",  xlab = "Temp")
hist(sdrapcty$Temp, breaks=breakseq, xlim=c(-20,100), ylim=c(0,760), main = "Avg Daily Temp (F) in Rapid City 1995-2011", xlab = "Temp")


#---------------------------------------------------------------
# mammal sleep
#---------------------------------------------------------------
#install.packages("faraway")
library(mosaic)
library(faraway)

data("mammalsleep", package = "faraway")
head(mammalsleep, 10)
#glimpse(mammalsleep)
summary(mammalsleep)

histogram(mammalsleep$dream)
histogram(mammalsleep$dream, breaks=20)

# peep danger variable.  R things it's quantitative, but it's categorical
summary(mammalsleep$danger)
mammalsleep$danger <- as.factor(mammalsleep$danger)
summary(mammalsleep$danger)

bwplot(dream~danger, data=mammalsleep)
dotplot(dream~danger, data=mammalsleep)

mean(dream~danger, data=mammalsleep, na.rm = TRUE)
sd(dream~danger, data=mammalsleep, na.rm = TRUE)

lm1 <- lm(dream~danger, data=mammalsleep)
coef(lm1)





