#-----------Assumptions------------------------------------------------------------
#Null Hypothesis - All proportions are equal
#Alternative Hypothesis - Not all Proportions are equal
#----------------------------------------------------------------------------------

BuyerRatio <- read.csv("C:/Hymaa/Data Science/Hypothesis Testing/BuyerRatio.csv")
str(BuyerRatio)


n <- as.ts(BuyerRatio[,-1])
n <- as.table(n)

rownames(n)[rownames(n) == "A"] = "Male"
rownames(n)[rownames(n) == "B"] = "Female"

barplot(n,beside = TRUE)

rows_cnt=length(rownames(n))
cols_cnt=length(colnames(n))

for (i in 1:cols_cnt)
  for (j in i+1:cols_cnt-1)
  {
    if (i < j & j <= cols_cnt)
    {
       print(paste("[",i,j,"] = ",prop.test(n[,c(i,j)])$p.value))
    }
  }

#As all P-values are greater than 0.05, we can accept null hypothesis
