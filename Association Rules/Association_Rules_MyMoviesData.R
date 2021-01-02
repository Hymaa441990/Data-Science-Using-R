library(arules)
library(arulesViz)

mymovies <- read.csv("C:/Hymaa/Data Science/Association Rules/my_movies.csv")
View(mymovies)
summary(mymovies)
str(mymovies)

rules <- apriori(as.matrix(mymovies[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=3)))
rules

inspect(head(sort(rules, by = "lift")))
head(quality(rules))

#Finding redundent Rules
gi <- generatingItemsets(rules)
d <- which(duplicated(gi))
exlc_redundant <- rules[-d]
exlc_redundant

inspect(head(sort(exlc_redundant, by = "lift")))
head(quality(exlc_redundant))
inspect(exlc_redundant)

windows()
plot(rules,method = "scatterplot")
plot(rules, method = "grouped")


# It looks ike most of them has wateched Lord of the rings movies along with Gladiator and Greenville
# Also most of them has watched Gladiator, Sixth sense along with Patrioit
# Patriot ,Braveheart and other three items along with Gladiator. 

plot(rules,method = "graph")

