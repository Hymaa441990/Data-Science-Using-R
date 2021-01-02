#------------------------------------------------------------------------------------
#Null Hypo - Defective percent is significant among all centres 
#Alternative Hypo - Defective percent varies by centre 
#------------------------------------------------------------------------------------

#Read Data
OrderForm <- read.csv("C:/Hymaa/Data Science/Hypothesis Testing/Costomer+OrderForm.csv")
str(OrderForm)

#Checking if any blank rows
summary(OrderForm)

#Get Base and Defective count
base <- nrow(OrderForm)
Phil <- table(OrderForm$Phillippines)[[1]]
Indo <- table(OrderForm$Indonesia)[[1]]
Mal <- table(OrderForm$Malta)[[1]]
Ind <- table(OrderForm$India)[[1]]

#Checking Defective % Varies by centre
Phil_Indo <- prop.test(x=c(Phil,Indo),n=c(base,base))
Phil_Indo

Phil_Mal <- prop.test(x=c(Phil,Mal),n=c(base,base))
Phil_Mal

Phil_Ind <- prop.test(x=c(Phil,Ind),n=c(base,base))
Phil_Ind

Indo_Mal <- prop.test(x=c(Indo,Mal),n=c(base,base))
Indo_Mal

Indo_Ind <- prop.test(x=c(Indo,Ind),n=c(base,base))
Indo_Ind

Ind_Mal <- prop.test(x=c(Ind,Mal),n=c(base,base))
Ind_Mal

Phil_Indo$p.value
Phil_Mal$p.value
Phil_Ind$p.value
Indo_Mal$p.value
Indo_Ind$p.value
Ind_Mal$p.value

#------------------------------------------------------------------------------------
#All P-values are greater than 0.05,hence we can conclude that
#           Defective percent among all centres will be significant 
#------------------------------------------------------------------------------------