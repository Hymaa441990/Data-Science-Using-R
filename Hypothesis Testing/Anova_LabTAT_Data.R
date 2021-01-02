#-----------Assumptions------------------------------------------------------------
#Null Hypothesis - No difference between average TAT of reports of the laboratories
#Alternative Hypothesis - There is a difference between average TAT of reports of the laboratories
#----------------------------------------------------------------------------------

LabTAT <- read.csv("C:/Hymaa/Data Science/Hypothesis Testing/LabTAT.csv")
str(LabTAT)

# Rename variables
names(LabTAT)[1] <- "Lab1"
names(LabTAT)[2] <- "Lab2"
names(LabTAT)[3] <- "Lab3"
names(LabTAT)[4] <- "Lab4"

#Checking for NA
summary(LabTAT$Lab1)
summary(LabTAT$Lab2)
summary(LabTAT$Lab3)
summary(LabTAT$Lab4)

#Stack data
#LabTAT_Final <- melt(LabTAT, id.vars=c(),var='Group') OR
LabTAT_Final <- stack(LabTAT)

names(LabTAT_Final)[1] <- "TAT"
names(LabTAT_Final)[2] <- "Labs"
names(LabTAT_Final)

levels(LabTAT_Final$Labs)

plot(TAT ~ Labs,data=LabTAT_Final)

Anova_Results <- aov(TAT ~ Labs,data=LabTAT_Final)

summary(Anova_Results)

#----------------------------------------------------------------------------------------------
#p-Value is 2e-16 which is less than 0.05
#Hence, we can reject null hypothesis
#According to anova analysis we can conclude average TAT among given laborataries is will not be same
#----------------------------------------------------------------------------------------------
