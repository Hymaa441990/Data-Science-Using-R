#Read Data
Faltoons <- read.csv("C:/Hymaa/Data Science/Hypothesis Testing/Faltoons.csv")
str(Faltoons)

#Checking if any blank rows
summary(Faltoons)

tab1 <- table(Faltoons$Weekdays)
tab2 <- table(Faltoons$Weekend)

data_table <- matrix(c(tab1,tab2),ncol=2,byrow=FALSE)
colnames(data_table) <- c("Weekdays","Weekend")
rownames(data_table) <- c("Female","Male")
data_table


chisq.test(data_table)

#------------------------------------------------------------------------------------
#P-value is less than 0.05, hence, we reject null hypothesis that male vs. female ratio walking in to the store is independent based on day of  the week
#------------------------------------------------------------------------------------
