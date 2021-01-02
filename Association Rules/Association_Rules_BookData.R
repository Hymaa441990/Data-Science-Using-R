library(arules)
library(arulesViz)

book <- read.csv("C:/Hymaa/Data Science/Association Rules/book.csv")
summary(book)
str(book)

rules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.7,minlen=5))
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
plot(exlc_redundant,method = "scatterplot")
plot(exlc_redundant, method = "grouped")


# The Art books are being sold at a larger extent along with other Cook, art, geo, child books
# Cook books are also being sold at a larger extent along with other chld, art, geo, Doit books)

plot(exlc_redundant,method = "graph")
