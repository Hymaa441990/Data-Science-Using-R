#install.packages("rmarkdown",repos = "http://cran.us.r-project.org")
#install.packages("arules",repos = "http://cran.us.r-project.org")
#install.packages("arulesViz",repos = "http://cran.us.r-project.org")

library(arules)
library(arulesViz)

Groceries <- read.transactions("C:/Hymaa/Data Science/Association Rules/groceries.csv")
str(Groceries)
class(Groceries)
summary(Groceries)

inspect(head(Groceries, 5))

#Generating Rules
rules <- apriori(Groceries, parameter=list(support=0.002, confidence=0.65,minlen=2))
rules

inspect(head(sort(groery_rules, by = "lift")))
head(quality(rules))

#Finding redundent Rules
gi <- generatingItemsets(rules)
d <- which(duplicated(gi))
exlc_redundant <- rules[-d]
exlc_redundant

inspect(head(sort(exlc_redundant, by = "lift")))
head(quality(exlc_redundant))
inspect(exlc_redundant)


#Plot
windows()
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")

